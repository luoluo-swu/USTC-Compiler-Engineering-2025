#include "cminusf_builder.hpp"

#define CONST_FP(num) ConstantFP::get((float)num, module.get())
#define CONST_INT(num) ConstantInt::get(num, module.get())

// types
Type *VOID_T;
Type *INT1_T;
Type *INT32_T;
Type *INT32PTR_T;
Type *FLOAT_T;
Type *FLOATPTR_T;

bool promote(IRBuilder *builder, Value **l_val_p, Value **r_val_p) {
    bool is_int = false;
    auto &l_val = *l_val_p;
    auto &r_val = *r_val_p;
    if (l_val->get_type() == r_val->get_type()) {
        is_int = l_val->get_type()->is_integer_type();
    } else {
        if (l_val->get_type()->is_integer_type()) {
            l_val = builder->create_sitofp(l_val, FLOAT_T);
        } else {
            r_val = builder->create_sitofp(r_val, FLOAT_T);
        }
    }
    return is_int;
}

/*
 * use CMinusfBuilder::Scope to construct scopes
 * scope.enter: enter a new scope
 * scope.exit: exit current scope
 * scope.push: add a new binding to current scope
 * scope.find: find and return the value bound to the name
 */

Value* CminusfBuilder::visit(ASTProgram &node) {
    VOID_T = module->get_void_type();
    INT1_T = module->get_int1_type();
    INT32_T = module->get_int32_type();
    INT32PTR_T = module->get_int32_ptr_type();
    FLOAT_T = module->get_float_type();
    FLOATPTR_T = module->get_float_ptr_type();

    Value *ret_val = nullptr;
    for (auto &decl : node.declarations) {
        ret_val = decl->accept(*this);
    }
    return ret_val;
}

Value* CminusfBuilder::visit(ASTNum &node) {
    if (node.type == TYPE_INT) {
        return CONST_INT(node.i_val);
    }
    return CONST_FP(node.f_val);
}

Value* CminusfBuilder::visit(ASTVarDeclaration &node) {
    // TODO: This function is empty now.
    // Add some code here.
    //return nullptr;
    Type *base_type = nullptr;
    if (node.type == TYPE_INT)
        base_type = INT32_T;
    else if (node.type == TYPE_FLOAT)
        base_type = FLOAT_T;
    else
        base_type = VOID_T;

    Value *alloca_inst = nullptr;

    if (scope.in_global()) {
        // 全局变量
        if (node.num) { // 数组声明
            int len = node.num->i_val;
            auto array_type = ArrayType::get(base_type, len);
            auto global = GlobalVariable::create(node.id, module.get(), array_type, false, ConstantZero::get(array_type, module.get()));
            scope.push(node.id, global);
            return global;
        } else { // 单个变量
            auto global = GlobalVariable::create(node.id, module.get(), base_type, false, ConstantZero::get(base_type, module.get()));
            scope.push(node.id, global);
            return global;
        }
    } else {
        // 局部变量
        if (node.num) { // 数组声明
            int len = node.num->i_val;
            auto array_type = ArrayType::get(base_type, len);
            alloca_inst = builder->create_alloca(array_type);
        } else { // 普通变量
            alloca_inst = builder->create_alloca(base_type);
        }
        scope.push(node.id, alloca_inst);
        return alloca_inst;
    }
}

Value* CminusfBuilder::visit(ASTFunDeclaration &node) {
    FunctionType *fun_type;
    Type *ret_type;
    std::vector<Type *> param_types;
    if (node.type == TYPE_INT)
        ret_type = INT32_T;
    else if (node.type == TYPE_FLOAT)
        ret_type = FLOAT_T;
    else
        ret_type = VOID_T;

    for (auto &param : node.params) {
        if (param->type == TYPE_INT) {
            if (param->isarray) {
                param_types.push_back(INT32PTR_T);
            } else {
                param_types.push_back(INT32_T);
            }
        } else {
            if (param->isarray) {
                param_types.push_back(FLOATPTR_T);
            } else {
                param_types.push_back(FLOAT_T);
            }
        }
    }

    fun_type = FunctionType::get(ret_type, param_types);
    auto func = Function::create(fun_type, node.id, module.get());
    scope.push(node.id, func);
    context.func = func;
    auto funBB = BasicBlock::create(module.get(), "entry", func);
    builder->set_insert_point(funBB);
    scope.enter();
    context.pre_enter_scope = true;
    std::vector<Value *> args;
    for (auto &arg : func->get_args()) {
        args.push_back(&arg);
    }
    for (unsigned int i = 0; i < node.params.size(); ++i) {
        auto* param_i = node.params[i]->accept(*this);
        args[i]->set_name(node.params[i]->id);
        builder->create_store(args[i], param_i);
        scope.push(args[i]->get_name(), param_i);
    }
    node.compound_stmt->accept(*this);
    if (builder->get_insert_block()->get_terminator() == nullptr) 
    {
        if (context.func->get_return_type()->is_void_type())
            builder->create_void_ret();
        else if (context.func->get_return_type()->is_float_type())
            builder->create_ret(CONST_FP(0.));
        else
            builder->create_ret(CONST_INT(0));
    }
    scope.exit();
    return nullptr;
}

Value* CminusfBuilder::visit(ASTParam &node) {
    return nullptr;
}

Value* CminusfBuilder::visit(ASTCompoundStmt &node) {
    // TODO: This function is not complete.
    // You may need to add some code here
    // to deal with complex statements. 
    
    /*for (auto &decl : node.local_declarations) {
        decl->accept(*this);
    }

    for (auto &stmt : node.statement_list) {
        stmt->accept(*this);
        if (builder->get_insert_block()->get_terminator() == nullptr)
            break;
    }
    return nullptr;*/
    // 如果不是函数体第一个复合语句，进入新作用域
    if (!context.pre_enter_scope) {
        scope.enter();
    } else {
        context.pre_enter_scope = false; // 已经在函数入口处进入过作用域
    }

    // 处理局部变量声明
    for (auto &decl : node.local_declarations) {
        if (auto *var_decl = dynamic_cast<ASTVarDeclaration*>(decl.get())) {
            // 确定变量类型
            Type* var_type = nullptr;
            if (var_decl->type == TYPE_INT) {
                var_type = INT32_T;
            } else if (var_decl->type == TYPE_FLOAT) {
                var_type = FLOAT_T;
            } else {
                assert(false && "Unsupported local variable type");
            }

            // 创建 alloca，仅传类型
            auto *alloca_inst = builder->create_alloca(var_type);
            alloca_inst->set_name(var_decl->id); // 单独设置名字

            // 初始化值
            if (var_decl->num != nullptr) {
                auto *init_val = var_decl->num->accept(*this);
                if (init_val->get_type() != var_type) {
                    if (var_type->is_integer_type())
                        init_val = builder->create_fptosi(init_val, INT32_T);
                    else
                        init_val = builder->create_sitofp(init_val, FLOAT_T);
                }
                builder->create_store(init_val, alloca_inst);
            }

            // 推入作用域
            scope.push(var_decl->id, alloca_inst);
        } else {
            // 处理其他声明类型（如函数嵌套声明）
            decl->accept(*this);
        }
    }

    // 执行语句列表
    for (auto &stmt : node.statement_list) {
        stmt->accept(*this);
        // 如果当前基本块已有终止指令，则不再生成后续语句
        if (builder->get_insert_block()->get_terminator() != nullptr)
            break;
    }

    // 退出作用域
    scope.exit();

    return nullptr;
}

Value* CminusfBuilder::visit(ASTExpressionStmt &node) {
    if (node.expression != nullptr) {
        return node.expression->accept(*this);
    }
    return nullptr;
}

Value* CminusfBuilder::visit(ASTSelectionStmt &node) {
    auto *ret_val = node.expression->accept(*this);
    auto *trueBB = BasicBlock::create(module.get(), "", context.func);
    BasicBlock *falseBB{};
    auto *contBB = BasicBlock::create(module.get(), "", context.func);
    Value *cond_val = nullptr;
    if (ret_val->get_type()->is_integer_type()) {
        cond_val = builder->create_icmp_ne(ret_val, CONST_INT(0));
    } else {
        cond_val = builder->create_fcmp_ne(ret_val, CONST_FP(0.));
    }

    if (node.else_statement == nullptr) {
        builder->create_cond_br(cond_val, trueBB, contBB);
    } else {
        falseBB = BasicBlock::create(module.get(), "", context.func);
        builder->create_cond_br(cond_val, trueBB, falseBB);
    }
    builder->set_insert_point(trueBB);
    node.if_statement->accept(*this);

    if (not builder->get_insert_block()->is_terminated()) {
        builder->create_br(contBB);
    }

    if (node.else_statement == nullptr) {
        // falseBB->erase_from_parent(); // did not clean up memory
    } else {
        builder->set_insert_point(falseBB);
        node.else_statement->accept(*this);
        if (not builder->get_insert_block()->is_terminated()) {
            builder->create_br(contBB);
        }
    }

    builder->set_insert_point(contBB);
    return nullptr;
}

Value* CminusfBuilder::visit(ASTIterationStmt &node) {
    // TODO: This function is empty now.
    // Add some code here.
    auto *func = context.func;

    // 建立基本块
    auto *condBB = BasicBlock::create(module.get(), "while.cond", func);
    auto *bodyBB = BasicBlock::create(module.get(), "while.body", func);
    auto *endBB  = BasicBlock::create(module.get(), "while.end", func);

    // 跳转进入条件检查块
    builder->create_br(condBB);

    // 条件判断
    builder->set_insert_point(condBB);
    auto *cond_val = node.expression->accept(*this);
    if (cond_val->get_type()->is_float_type())
        cond_val = builder->create_fcmp_ne(cond_val, CONST_FP(0.));
    else
        cond_val = builder->create_icmp_ne(cond_val, CONST_INT(0));

    builder->create_cond_br(cond_val, bodyBB, endBB);

    // 循环体
    builder->set_insert_point(bodyBB);

    // 保存原 context（嵌套循环时需恢复）
    auto *old_cond = context.loop_cond_bb;
    auto *old_end  = context.loop_end_bb;
    context.loop_cond_bb = condBB;
    context.loop_end_bb  = endBB;

    node.statement->accept(*this);

    // 恢复 context
    context.loop_cond_bb = old_cond;
    context.loop_end_bb  = old_end;

    // 如果循环体没终结，跳回条件
    if (!builder->get_insert_block()->is_terminated())
        builder->create_br(condBB);

    // 循环结束块
    builder->set_insert_point(endBB);
    return nullptr;
}

Value* CminusfBuilder::visit(ASTReturnStmt &node) {
    if (node.expression == nullptr) {
        builder->create_void_ret();
    } else {
        auto *fun_ret_type =
            context.func->get_function_type()->get_return_type();
        auto *ret_val = node.expression->accept(*this);
        if (fun_ret_type != ret_val->get_type()) {
            if (fun_ret_type->is_integer_type()) {
                ret_val = builder->create_fptosi(ret_val, INT32_T);
            } else {
                ret_val = builder->create_sitofp(ret_val, FLOAT_T);
            }
        }

        builder->create_ret(ret_val);
    }

    return nullptr;
}

Value* CminusfBuilder::visit(ASTVar &node) {
    Value* baseAddr = this->scope.find(node.id);
    Type* alloctype = nullptr;
    
    if(baseAddr->is<AllocaInst>()) {
        alloctype = baseAddr->as<AllocaInst>()->get_alloca_type();
    } else {
        alloctype = baseAddr->as<GlobalVariable>()->get_type()->get_pointer_element_type();
    }

    if(node.expression) {
        bool original_require_lvalue = context.require_lvalue;
        context.require_lvalue = false;
        auto idx = node.expression->accept(*this);
        context.require_lvalue = original_require_lvalue;

        if (idx->get_type()->is_float_type()) {
            idx = builder->create_fptosi(idx, INT32_T);
        } else if(idx->get_type()->is_int1_type()){
            idx = builder->create_zext(idx, INT32_T);
        }
        auto right_bb = BasicBlock::create(module.get(), "", context.func);
        auto wrong_bb = BasicBlock::create(module.get(), "", context.func);
        
        auto cond_neg = builder->create_icmp_ge(idx, CONST_INT(0));
        builder->create_cond_br(cond_neg,right_bb, wrong_bb);

        auto wrong = scope.find("neg_idx_except");
        builder->set_insert_point(wrong_bb);
        builder->create_call(wrong, {});
        builder->create_br(right_bb);
        builder->set_insert_point(right_bb);
        
        if(context.require_lvalue) {
            if(alloctype->is_pointer_type()) {
                baseAddr = builder->create_load(baseAddr);
                baseAddr = builder->create_gep(baseAddr,{idx});
            } else if(alloctype->is_array_type()){ 
                baseAddr = builder->create_gep(baseAddr,{CONST_INT(0),idx});
            }
            context.require_lvalue = false;
            return baseAddr;
        } else {
            if(alloctype->is_pointer_type()){
                baseAddr = builder->create_load(baseAddr);
                baseAddr = builder->create_gep(baseAddr,{idx});
            } else if(alloctype->is_array_type()){ 
                baseAddr = builder->create_gep(baseAddr,{CONST_INT(0),idx});
            }
            baseAddr = builder->create_load(baseAddr);
            return baseAddr;
        }
    } else {
        if (context.require_lvalue) {
            context.require_lvalue = false;
            return baseAddr;
            // return builder->create_gep(baseAddr, {CONST_INT(0)});
        } else {
            if(alloctype->is_array_type()){
                return builder->create_gep(baseAddr, {CONST_INT(0),CONST_INT(0)});
            } else {
                return builder->create_load(baseAddr);
            }
            
        }
    }
    return nullptr;
}

Value* CminusfBuilder::visit(ASTAssignExpression &node) {
    auto *expr_result = node.expression->accept(*this);
    context.require_lvalue = true;
    auto *var_addr = node.var->accept(*this);
    if (var_addr->get_type()->get_pointer_element_type() !=
        expr_result->get_type()) {
        if (expr_result->get_type() == INT32_T) {
            expr_result = builder->create_sitofp(expr_result, FLOAT_T);
        } else {
            expr_result = builder->create_fptosi(expr_result, INT32_T);
        }
    }
    builder->create_store(expr_result, var_addr);
    return expr_result;
}

Value* CminusfBuilder::visit(ASTSimpleExpression &node) {
    // TODO: This function is empty now.
    // Add some code here.
    //return nullptr;
    auto *l_val = node.additive_expression_l->accept(*this);
auto *r_val = node.additive_expression_r->accept(*this);
bool is_int = promote(&*builder, &l_val, &r_val);

Value *ret_val = nullptr; // 用 Value* 统一类型
switch (node.op) {
case OP_LE:
    ret_val = is_int ? static_cast<Value*>(builder->create_icmp_le(l_val, r_val))
                     : static_cast<Value*>(builder->create_fcmp_le(l_val, r_val));
    break;
case OP_LT:
    ret_val = is_int ? static_cast<Value*>(builder->create_icmp_lt(l_val, r_val))
                     : static_cast<Value*>(builder->create_fcmp_lt(l_val, r_val));
    break;
case OP_GT:
    ret_val = is_int ? static_cast<Value*>(builder->create_icmp_gt(l_val, r_val))
                     : static_cast<Value*>(builder->create_fcmp_gt(l_val, r_val));
    break;
case OP_GE:
    ret_val = is_int ? static_cast<Value*>(builder->create_icmp_ge(l_val, r_val))
                     : static_cast<Value*>(builder->create_fcmp_ge(l_val, r_val));
    break;
case OP_EQ:
    ret_val = is_int ? static_cast<Value*>(builder->create_icmp_eq(l_val, r_val))
                     : static_cast<Value*>(builder->create_fcmp_eq(l_val, r_val));
    break;
case OP_NEQ:
    ret_val = is_int ? static_cast<Value*>(builder->create_icmp_ne(l_val, r_val))
                     : static_cast<Value*>(builder->create_fcmp_ne(l_val, r_val));
    break;
}

return ret_val; // 返回 i1 类型 Value*，可直接用于条件分支
}

Value* CminusfBuilder::visit(ASTAdditiveExpression &node) {
    if (node.additive_expression == nullptr) {
        return node.term->accept(*this);
    }

    auto *l_val = node.additive_expression->accept(*this);
    auto *r_val = node.term->accept(*this);
    bool is_int = promote(&*builder, &l_val, &r_val);
    Value *ret_val = nullptr;
    switch (node.op) {
    case OP_PLUS:
        if (is_int) {
            ret_val = builder->create_iadd(l_val, r_val);
        } else {
            ret_val = builder->create_fadd(l_val, r_val);
        }
        break;
    case OP_MINUS:
        if (is_int) {
            ret_val = builder->create_isub(l_val, r_val);
        } else {
            ret_val = builder->create_fsub(l_val, r_val);
        }
        break;
    }
    return ret_val;
}

Value* CminusfBuilder::visit(ASTTerm &node) {
    if (node.term == nullptr) {
        return node.factor->accept(*this);
    }

    auto *l_val = node.term->accept(*this);
    auto *r_val = node.factor->accept(*this);
    bool is_int = promote(&*builder, &l_val, &r_val);

    Value *ret_val = nullptr;
    switch (node.op) {
    case OP_MUL:
        if (is_int) {
            ret_val = builder->create_imul(l_val, r_val);
        } else {
            ret_val = builder->create_fmul(l_val, r_val);
        }
        break;
    case OP_DIV:
        if (is_int) {
            ret_val = builder->create_isdiv(l_val, r_val);
        } else {
            ret_val = builder->create_fdiv(l_val, r_val);
        }
        break;
    }
    return ret_val;
}

Value* CminusfBuilder::visit(ASTCall &node) {
    auto *func = dynamic_cast<Function *>(scope.find(node.id));
    std::vector<Value *> args;
    auto param_type = func->get_function_type()->param_begin();
    for (auto &arg : node.args) {
        auto *arg_val = arg->accept(*this);
        if (!arg_val->get_type()->is_pointer_type() &&
            *param_type != arg_val->get_type()) {
            if (arg_val->get_type()->is_integer_type()) {
                arg_val = builder->create_sitofp(arg_val, FLOAT_T);
            } else {
                arg_val = builder->create_fptosi(arg_val, INT32_T);
            }
        }
        args.push_back(arg_val);
        param_type++;
    }

    return builder->create_call(static_cast<Function *>(func), args);
}
