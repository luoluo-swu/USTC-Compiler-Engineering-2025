#include "DeadCode.hpp"
#include "Instruction.hpp"
#include "logging.hpp"
#include <memory>
#include <vector>


// 处理流程：两趟处理，mark 标记有用变量，sweep 删除无用指令
void DeadCode::run() {
    bool changed{};
    func_info->run();
    do {
        changed = false;
        for (auto &F : m_->get_functions()) {
            auto func = &F;
            changed |= clear_basic_blocks(func);
            mark(func);
            changed |= sweep(func);
        }
    } while (changed);
    LOG_INFO << "dead code pass erased " << ins_count << " instructions";
}

bool DeadCode::clear_basic_blocks(Function *func) {
    bool changed = 0;
    std::vector<BasicBlock *> to_erase;
    for (auto &bb1 : func->get_basic_blocks()) {
        auto bb = &bb1;
        if(bb->get_pre_basic_blocks().empty() && bb != func->get_entry_block()) {
            to_erase.push_back(bb);
            changed = 1;
        }
    }
    for (auto &bb : to_erase) {
        bb->erase_from_parent();
        delete bb;
    }
    return changed;
}

void DeadCode::mark(Function *func) {
    work_list.clear();
    marked.clear();
    for (auto &block : func->get_basic_blocks()) {
        for (auto &instruction : block.get_instructions()) {
            if (is_critical(&instruction)) {
                marked.emplace(&instruction, true);
                work_list.push_back(&instruction);
            }
        }
    }
    while (!work_list.empty()) {
        Instruction* current = work_list.front();
        work_list.pop_front();
        mark(current);
    }
}

void DeadCode::mark(Instruction *ins) {
    for (auto &val : ins->get_operands()) {
        auto* instrDef = dynamic_cast<Instruction*>(val);
        if (!instrDef) continue;
        bool alreadyMarked = marked[instrDef];
        bool sameFunction = (instrDef->get_function() == ins->get_function());
        if (alreadyMarked || !sameFunction) continue;
        marked[instrDef] = true;
        work_list.push_back(instrDef);
    }
}

bool DeadCode::sweep(Function *func) {
    std::unordered_set<Instruction *> wait_del{};
    for (auto &block : func->get_basic_blocks()) {
        for (Instruction* instr : block.get_instructions()) {
            if (!marked[instr]) {
                wait_del.insert(instr);
            }
        }
    }
    for (Instruction* instr : wait_del) {
        instr->remove_all_operands();
    }
    for (Instruction* instr : wait_del) {
        auto parent = instr->get_parent();
        if (parent) {
            auto &instrs = parent->get_instructions();
            instrs.erase(std::remove(instrs.begin(), instrs.end(), instr), instrs.end());
        }
    }
    ins_count += wait_del.size();
    return not wait_del.empty();
}

bool DeadCode::is_critical(Instruction *ins) {
    // TODO: 判断指令是否是无用指令
    // 提示：
    // 1. 如果是函数调用，且函数是纯函数，则无用
    // 2. 如果是无用的分支指令，则无用
    // 3. 如果是无用的返回指令，则无用
    // 4. 如果是无用的存储指令，则无用
    
}

void DeadCode::sweep_globally() {
    std::vector<Function *> unused_funcs;
    std::vector<GlobalVariable *> unused_globals;
    for (auto &f_r : m_->get_functions()) {
        if (f_r.get_use_list().size() == 0 and f_r.get_name() != "main")
            unused_funcs.push_back(&f_r);
    }
    for (auto &glob_var_r : m_->get_global_variable()) {
        if (glob_var_r.get_use_list().size() == 0)
            unused_globals.push_back(&glob_var_r);
    }
    // changed |= unused_funcs.size() or unused_globals.size();
    for (auto func : unused_funcs)
        m_->get_functions().erase(func);
    for (auto glob : unused_globals)
        m_->get_global_variable().erase(glob);
}
