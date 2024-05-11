#pragma once

#include "SSAIR.h"
#include "../util/cast.h"
#include "controldep.h"
#include <queue>
#include <functional>

class DSU
{
public:
    std::unordered_map<int, int> ff;
    int f(int x)
    {
        if (!ff.count(x))
        {
            ff[x] = x;
            return x;
        }
        if (ff[x] == x)
            return x;
        return ff[x] = f(ff[x]);
    }
    void addedge(int x, int y)
    {
        auto a = f(x), b = f(y);
        ff[a] = b;
    }
    bool sameset(int x, int y) { return f(x) == f(y); }
};

enum class COND
{
    undefined = 0,
    constant = 1,
    indefinite = 2
};
struct TEMP_COND
{
    COND cond;
    int val;
};

class CP
{
private:
    SSA::SSAIR *ir;
    std::vector<GRAPH::Node *> *nodes;

    DSU dsu;

    /**
     * 标识一个块是否被处理过
     */
    std::unordered_set<int> blockCondition;
    std::queue<int> curBlock;
    std::queue<int> curTemp;

    std::unordered_map<LIR::StmLinkedList *, int> stmlBlockmap;
    std::unordered_map<int, LIR::StmLinkedList *> tempDef;
    std::unordered_map<int, std::vector<LIR::StmLinkedList *>> tempUse;

    std::unordered_map<int, TEMP_COND> tempCondition;

    // 判断是否为定值
    bool isTempConst(LIR::Exp *exp)
    {
        if (exp->exp_kind == LIR::EXP_KIND::constexp)
            return true;
        if (exp->exp_kind == LIR::EXP_KIND::temp)
        {
            auto tmp = static_cast<LIR::TempVarExp *>(exp);
            if (tempCondition.count(tmp->temp_label_id_int))
            {
                return tempCondition[tmp->temp_label_id_int].cond == COND::constant;
            }
            return false;
        }
        return false;
    }

    // 判断是否为不确定并且初始化不确定的默认信息
    bool isTempIndefinite(LIR::Exp *exp)
    {
        if (exp->exp_kind == LIR::EXP_KIND::constexp)
            return true;
        if (exp->exp_kind == LIR::EXP_KIND::temp)
        {
            auto tmp = static_cast<LIR::TempVarExp *>(exp);
            if (tempCondition.count(tmp->temp_label_id_int))
            {
                return tempCondition[tmp->temp_label_id_int].cond == COND::indefinite;
            }
            else
            {
                if (tmp->temp_label_id_int < 1000)
                {
                    return true;
                } // reg
                tempCondition[tmp->temp_label_id_int] = idf();
                return true;
            }
            return false;
        }
        return true;
    }

    // 判断两定值是否相等
    bool evalRel(LIR::RelOp op, int lv, int rv)
    {
        switch (op)
        {
        case LIR::RelOp::T_eq:
            return lv == rv;
        case LIR::RelOp::T_ge:
            return lv >= rv;
        case LIR::RelOp::T_gt:
            return lv > rv;
        case LIR::RelOp::T_le:
            return lv <= rv;
        case LIR::RelOp::T_lt:
            return lv < rv;
        case LIR::RelOp::T_ne:
            return lv != rv;
        case LIR::RelOp::F_eq:
            return decode(lv) == decode(rv);
        case LIR::RelOp::F_ge:
            return decode(lv) >= decode(rv);
        case LIR::RelOp::F_gt:
            return decode(lv) > decode(rv);
        case LIR::RelOp::F_le:
            return decode(lv) <= decode(rv);
        case LIR::RelOp::F_lt:
            return decode(lv) < decode(rv);
        case LIR::RelOp::F_ne:
            return decode(lv) != decode(rv);
        default:
            break;
        }
        assert(0);
    }

    // 计算已知为定值常量的表达式的值
    int tempEval(LIR::Exp *exp)
    {
        switch (exp->exp_kind)
        {
        case LIR::EXP_KIND::binop:
        {
            auto binopexp = static_cast<LIR::BinOpExp *>(exp);
            auto lf = tempEval(binopexp->l), rg = tempEval(binopexp->r);
            switch (binopexp->binop)
            {
            case LIR::BIN_OP::T_div:
                return lf / rg;
            case LIR::BIN_OP::T_minus:
                return lf - rg;
            case LIR::BIN_OP::T_mod:
                return lf % rg;
            case LIR::BIN_OP::T_mul:
                return lf * rg;
            case LIR::BIN_OP::T_plus:
                return lf + rg;
            case LIR::BIN_OP::F_div:
                return encode(decode(lf) / decode(rg));
            case LIR::BIN_OP::F_minus:
                return encode(decode(lf) - decode(rg));
            case LIR::BIN_OP::F_mul:
                return encode(decode(lf) * decode(rg));
            case LIR::BIN_OP::F_plus:
                return encode(decode(lf) + decode(rg));
            default:
                assert(0);
            }
        }
        case LIR::EXP_KIND::call:
        {
            auto callexp = static_cast<LIR::CallExp *>(exp);
            return tempEval(callexp->args[0]);
        }
        case LIR::EXP_KIND::constexp:
            return (static_cast<LIR::ConstExp *>(exp))->val;
        case LIR::EXP_KIND::mem:
            assert(0);
        case LIR::EXP_KIND::name:
            assert(0);
        case LIR::EXP_KIND::temp:
        {
            auto tmp = static_cast<LIR::TempVarExp *>(exp);
            assert(tempCondition.count(tmp->temp_label_id_int) && tempCondition[tmp->temp_label_id_int].cond == COND::constant);
            return tempCondition[tmp->temp_label_id_int].val;
        }
        default:
            assert(0);
        }
    }

    // 返回定值形式
    TEMP_COND cst(int y)
    {
        TEMP_COND x;
        x.cond = COND::constant;
        x.val = y;
        return x;
    }

    // 返回不定值形式
    TEMP_COND idf()
    {
        TEMP_COND x;
        x.cond = COND::indefinite;
        return x;
    }

    // 修改stml定义的变量的属性为定值/不定值
    void doDef(LIR::StmLinkedList *stml)
    {
        if (!stml)
            return;
        auto defid = static_cast<LIR::TempVarExp *>(static_cast<LIR::MoveStm *>(stml->stm)->dst)->temp_label_id_int;
        auto src = static_cast<LIR::MoveStm *>(stml->stm)->src;
        int base = 0;
        if (tempCondition.count(defid))
        {
            base = static_cast<int>(tempCondition[defid].cond);
        }
        if (base == 2)
            return;
        switch (src->exp_kind)
        {
        case LIR::EXP_KIND::binop:
        {
            auto bexp = static_cast<LIR::BinOpExp *>(src);
            if (isTempConst(bexp->l) && isTempConst(bexp->r))
            {
                tempCondition[defid] = cst(tempEval(bexp));
            }
            else
            {
                auto lf = isTempIndefinite(bexp->l);
                auto rf = isTempIndefinite(bexp->r);
                assert(lf || rf);
                tempCondition[defid] = idf();
            }
            break;
        }
        case LIR::EXP_KIND::call:
        {
            auto cal = static_cast<LIR::CallExp *>(src);
            if (cal->func_name_str[0] == '$')
            {
                int cur = stmlBlockmap[stml];
                int cnt = 0;
                int flag = 0, guess = 0, fall = 0;
                auto &v = (ir->pre_nodes[cur]);
                auto len = v.size();
                // 处理phi函数的第i个参数，代表的是第i个前驱块
                // phi 函数的结果为常量的条件：所有参数值相同且已经被视为常量
                for (int i = 0; i < len; i++)
                {
                    if (blockCondition.count(v[i]))
                    {
                        // 接受相同的已处理的视为常量的临时变量
                        if (cal->args[i]->exp_kind == LIR::EXP_KIND::temp)
                        {
                            auto temp_label_id_int =
                                static_cast<LIR::TempVarExp *>(cal->args[i])->temp_label_id_int;
                            if (!tempCondition.count(temp_label_id_int))
                            {
                                if (temp_label_id_int < 1000)
                                {
                                    fall = 1;
                                    tempCondition[defid] = idf();
                                    break;
                                }
                                continue;
                            }
                            else
                            {
                                auto b = isTempIndefinite(cal->args[i]);
                                if (b)
                                {
                                    fall = 1;
                                    tempCondition[defid] = idf();
                                    break;
                                }
                                assert(isTempConst(cal->args[i]));
                                int tval = tempEval(cal->args[i]);
                                if (flag && tval != guess)
                                {
                                    fall = 1;
                                    tempCondition[defid] = idf();
                                    break;
                                }
                                else
                                {
                                    flag = 1;
                                    guess = tval;
                                }
                            }
                        }
                        else if (cal->args[i]->exp_kind == LIR::EXP_KIND::constexp)
                        {
                            if (flag)
                            {
                                // 所有 Phi 函数的参数都相同表示phi函数的结果为定值，即定义的变量为定值常量
                                if (guess != tempEval(cal->args[i]))
                                {
                                    fall = 1;
                                    tempCondition[defid] = idf();
                                    break;
                                }
                            }
                            else
                            {
                                flag = 1;
                                guess = tempEval(cal->args[i]);
                            }
                        }
                    }
                }
                if (!fall)
                    tempCondition[defid] = cst(guess);
            }
            else // normal call
                tempCondition[defid] = idf();
            break;
        }
        case LIR::EXP_KIND::constexp:
        {
            auto cs = static_cast<LIR::ConstExp *>(src);
            tempCondition[defid] = cst(cs->val);
            break;
        }
        case LIR::EXP_KIND::mem: // fallthrough
        case LIR::EXP_KIND::name:
        {
            tempCondition[defid] = idf();
            break;
        }
        case LIR::EXP_KIND::temp:
        {
            auto ot = isTempIndefinite(src);
            if (ot)
                tempCondition[defid] = idf();
            else
                tempCondition[defid] = tempCondition[static_cast<LIR::TempVarExp *>(src)->temp_label_id_int];
            break;
        }
        default:
            assert(0);
        }
        if (static_cast<int>(tempCondition[defid].cond) > base)
            curTemp.push(defid); // 处理constant/indefinite的变量
    }

    /**
     * 接下来处理根据 jump 连接的 block
     */
    void doJump(LIR::StmLinkedList *stml)
    {
        auto stm = stml->stm;
        auto curb = stmlBlockmap[stml];
        if (stm->stm_kind == LIR::STM_KIND::jump)
        {
            auto nx = *nodes->at(curb)->succ().begin();
            if (!blockCondition.count(nx))
            {
                blockCondition.insert(nx);
                curBlock.push(nx);
            }
        }
        else
        {
            auto cjmp = static_cast<LIR::CJumpStm *>(stm);
            if (isTempConst(cjmp->l) && isTempConst(cjmp->r))
            {
                // 计算出将要跳转的block，剪枝
                auto lv = tempEval(cjmp->l), rv = tempEval(cjmp->r);
                auto b = evalRel(cjmp->op, lv, rv);
                auto tg = cjmp->falseLabel;
                if (b)
                    tg = cjmp->trueLabel;
                for (auto it : (nodes->at(curb))->succ())
                {
                    if (getNodeLabel(ir->in_nodes[it]) == tg)
                    {
                        auto nx = it;
                        if (!blockCondition.count(nx))
                        {
                            blockCondition.insert(nx);
                            curBlock.push(nx);
                        }
                    }
                }
                return;
            }
            if ((cjmp->l->exp_kind == LIR::EXP_KIND::temp && isTempIndefinite(cjmp->l)) || (cjmp->r->exp_kind == LIR::EXP_KIND::temp && isTempIndefinite(cjmp->r)))
            {
                for (auto it : ((nodes->at(curb))->succ()))
                {
                    auto nx = it;
                    if (!blockCondition.count(nx))
                    {
                        blockCondition.insert(nx);
                        curBlock.push(nx);
                    }
                }
                return;
            }
        }
    }

    /**
     * 删除路径和phi函数参数关联
     */
    void cutEdge(int from, int to)
    {
        int cnt = 0;
        auto toNode = nodes->at(to);
        for (auto jt : ir->pre_nodes[to])
        {
            if (jt == from)
            {
                break;
            }
            cnt++;
        }
        // assert(cnt != ir->prednode[to].size());
        ir->pre_nodes[to].erase(ir->pre_nodes[to].begin() + cnt);
        if (ir->Phi_mp[to].size())
        {
            // jt to modify the phi func
            for (auto jt : ir->Phi_mp[to])
            {
                auto src = (static_cast<LIR::MoveStm *>(jt.second->stm))->src;
                auto callexp = static_cast<LIR::CallExp *>(src);
                callexp->args.erase(callexp->args.begin() + cnt);
            }
        }
        ir->rm_path(nodes->at(from), toNode);
        if (toNode->pres.empty())
        {
            while (!toNode->succs.empty())
            {
                cutEdge(to, *(toNode->succs.begin()));
            }
        }
    }

    void setup()
    {
        auto root = nodes->at(0)->uid;
        blockCondition.insert(root);
        curBlock.push(root);
    }

    /**
     * 通过bfs，尝试标记变量为定值
     */
    void bfsMark()
    {
        while (!curBlock.empty() || !curTemp.empty())
        {
            while (!curBlock.empty())
            {
                auto cur = curBlock.front();
                curBlock.pop();
                auto stml = static_cast<LIR::StmLinkedList *>(nodes->at(cur)->info);
                while (stml)
                {
                    auto stm = stml->stm;
                    // 设置 变量定义->句子 的 mapping
                    auto df = getDefExp(stm);
                    if (df)
                    {
                        stmlBlockmap[stml] = cur;
                        tempDef[static_cast<LIR::TempVarExp *>(*df)->temp_label_id_int] = stml;
                        doDef(stml);
                    }
                    auto us = findTempUse(stm);
                    for (const auto it : us)
                        tempUse[static_cast<LIR::TempVarExp *>(*it)->temp_label_id_int].push_back(stml);
                    if (stm->stm_kind == LIR::STM_KIND::jump || stm->stm_kind == LIR::STM_KIND::cjump)
                    {
                        stmlBlockmap[stml] = cur;
                        doJump(stml);
                    }
                    stml = stml->next;
                }
                for (auto nx : ir->in_nodes.at(cur)->succ())
                {
                    // 处理所有后继块中的phi函数的参数定义
                    if (blockCondition.count(nx))
                    {
                        for (auto st : ir->Phi_mp[nx])
                        {
                            doDef(st.second);
                        }
                    }
                }
            }
            while (!curTemp.empty())
            {
                // 在定义变量的每一处尝试常量化
                auto cur = curTemp.front();
                curTemp.pop();
                for (auto use : tempUse[cur])
                {
                    auto df = getDefExp(use->stm);
                    if (df)
                        doDef(use);
                    if (use->stm->stm_kind == LIR::STM_KIND::cjump || use->stm->stm_kind == LIR::STM_KIND::jump)
                        doJump(use);
                }
            }
        }
    }

    // 常量替换
    void replaceTemp()
    {
        auto nodes = ir->all_nodes();
        for (auto &it : (*nodes))
        {
            auto stml = static_cast<LIR::StmLinkedList *>(it->info);
            // 删除剪去的block
            if (!blockCondition.count(it->uid))
            {
                while (!it->pre().empty())
                {
                    ir->rm_path(ir->in_nodes[*it->pres.begin()], it);
                }
                while (!it->succ().empty())
                {
                    // 删除与后继的关系，同时删除后继phi函数中的某一个参数
                    cutEdge(it->uid, *(it->succs.begin()));
                }
                stml->next = 0;
                continue;
            }
            auto head = stml, tail = stml->next;
            while (tail)
            {
                auto def = getDefExp(tail->stm);
                bool flag = false;
                int tempid = -1;
                if (def)
                {
                    tempid = static_cast<LIR::TempVarExp *>(*def)->temp_label_id_int;
                    if (tempCondition.count(tempid))
                    {
                        if (tempCondition[tempid].cond == COND::constant)
                        {
                            flag = true;
                        }
                    }
                }
                if (flag)
                {
                    // 定义的变量为常量 删除phi函数
                    auto newtail = tail->next;
                    tail->next = 0;
                    if (ir->Phi_mp[it->uid].count(tempid) && ir->Phi_mp[it->uid][tempid] == tail)
                    {
                        ir->Phi_mp[it->uid].erase(tempid);
                    }
                    delete tail;
                    tail = newtail;
                    head->next = newtail;
                }
                else
                {
                    // 将句子中所有鉴定为常量的变量化为常量
                    auto uses = findTempUse(tail->stm);
                    for (auto it : uses)
                    {
                        auto tempid = static_cast<LIR::TempVarExp *>(*it)->temp_label_id_int;
                        if (tempCondition.count(tempid))
                        {
                            if (tempCondition[tempid].cond == COND::constant)
                            {
                                delete (*it);
                                *it = new LIR::ConstExp(tempCondition[tempid].val);
                            }
                        }
                    }
                    head = head->next;
                    tail = tail->next;
                }
            }
            // 剪枝
            if (head && head->stm->stm_kind == LIR::STM_KIND::cjump)
            {
                auto cjmpstm = static_cast<LIR::CJumpStm *>(head->stm);
                bool ht = false, hf = false;
                for (auto jt : (it->succ()))
                {
                    if (!blockCondition.count(jt))
                        continue;
                    if (getNodeLabel(ir->in_nodes[jt]) == cjmpstm->trueLabel)
                    {
                        ht = true;
                    }
                    else if (getNodeLabel(ir->in_nodes[jt]) == cjmpstm->falseLabel)
                    {
                        hf = true;
                    }
                }
                assert(ht || hf);
                if (!ht)
                {
                    head->stm = new LIR::JumpStm(cjmpstm->falseLabel);
                    delete cjmpstm;
                }
                if (!hf)
                {
                    head->stm = new LIR::JumpStm(cjmpstm->trueLabel);
                    delete cjmpstm;
                }
            }
        }
    }

    // 去掉 phi 函数直接赋值
    void cleanup()
    {
        auto nodes = ir->all_nodes();
        for (const auto &it : (*nodes))
        {
            if (it->in_degree() == 1)
            {
                for (auto jt : ir->Phi_mp[it->uid])
                {
                    auto mv = static_cast<LIR::MoveStm *>(jt.second->stm);
                    auto cl = static_cast<LIR::CallExp *>(mv->src);
                    mv->src = allocate_temp_var_for_exp(cl->args[0]);
                    delete cl;
                }
                ir->Phi_mp[it->uid].clear();
            }
        }
    }

    // 合并 src dst 定义(用 src 定义)
    void checkCopy()
    {
        auto nodes = ir->all_nodes();
        for (const auto &it : (*nodes))
        {
            auto stml = static_cast<LIR::StmLinkedList *>(it->info);
            while (stml)
            {
                auto stm = stml->stm;
                if (stm->stm_kind == LIR::STM_KIND::move)
                {
                    auto mv = static_cast<LIR::MoveStm *>(stm);
                    if (mv->dst->exp_kind == LIR::EXP_KIND::temp && mv->src->exp_kind == LIR::EXP_KIND::temp)
                    {
                        auto from = static_cast<LIR::TempVarExp *>(mv->src)->temp_label_id_int,
                             to = static_cast<LIR::TempVarExp *>(mv->dst)->temp_label_id_int;
                        if (from > 1000 && to > 1000)
                        {
                            // replace the def temp by the use temp
                            dsu.addedge(to, from);
                        }
                    }
                }
                stml = stml->next;
            }
        }
    }

    // 消除 move-related 语句
    void cleanCopy()
    {
        auto nodes = ir->all_nodes();
        for (const auto &it : (*nodes))
        {
            auto stml = static_cast<LIR::StmLinkedList *>(it->info);
            while (stml)
            {
                auto stm = stml->stm;
                bool flag = false;
                if (stm->stm_kind == LIR::STM_KIND::move)
                {
                    auto mv = static_cast<LIR::MoveStm *>(stm);
                    if (mv->dst->exp_kind == LIR::EXP_KIND::temp && mv->src->exp_kind == LIR::EXP_KIND::temp)
                    {
                        auto from = static_cast<LIR::TempVarExp *>(mv->src)->temp_label_id_int,
                             to = static_cast<LIR::TempVarExp *>(mv->dst)->temp_label_id_int;
                        if (from > 1000 && to > 1000)
                        {
                            if (dsu.sameset(to, from))
                            {
                                delete stm;
                                stml->stm = NOPStm();
                                flag = true;
                            }
                        }
                    }
                }
                if (!flag)
                {
                    // 尽量合并所有使用到的临时变量
                    auto uses = findTempUse(stm);
                    for (auto it : uses)
                    {
                        auto tmp = static_cast<LIR::TempVarExp *>(*it);
                        if (dsu.f(tmp->temp_label_id_int) != tmp->temp_label_id_int)
                        {
                            tmp->temp_label_id_int = dsu.f(tmp->temp_label_id_int);
                        }
                    }
                    auto df = getDefExp(stm);
                    if (df)
                    {
                        auto def = static_cast<LIR::TempVarExp *>(*df);
                        if (dsu.f(def->temp_label_id_int) != def->temp_label_id_int)
                        {
                            def->temp_label_id_int = dsu.f(def->temp_label_id_int);
                        }
                    }
                }
                stml = stml->next;
            }
        }
    }

    void cleanSpPhi()
    {
        auto nodes = ir->all_nodes();
        std::unordered_map<int, LIR::Exp *> mv;
        for (const auto &it : (*nodes))
        {
            auto stml = static_cast<LIR::StmLinkedList *>(it->info)->next;
            auto fh = static_cast<LIR::StmLinkedList *>(it->info);
            while (stml)
            {
                auto stm = stml->stm;
                // 尝试清理 Phi 函数
                if (is_phi(stm))
                {
                    auto mvs = static_cast<LIR::MoveStm *>(stm);
                    auto tid = static_cast<LIR::TempVarExp *>(mvs->dst)->temp_label_id_int;
                    auto cl = static_cast<LIR::CallExp *>(mvs->src);
                    LIR::Exp *gs = 0;
                    int flag = 1, cnt = 0;
                    for (auto ar : cl->args)
                    {
                        if (ar->exp_kind == LIR::EXP_KIND::temp && static_cast<LIR::TempVarExp *>(ar)->temp_label_id_int == tid)
                        {
                            cnt++;
                            continue;
                        }
                        if (gs == 0)
                            gs = ar;
                        else
                        {
                            flag = 0;
                            break;
                        }
                    }
                    if (flag && gs)
                    {
                        // std::cerr << "!!!" << tid << '\n';
                        assert(cnt == cl->args.size() - 1);
                        mv[tid] = gs->dCopy();
                        ir->Phi_mp[it->uid].erase(tid);
                        stml->stm = NOPStm();
                    }
                }
                else if (!is_nop(stm))
                {
                    for (auto mvit : mv)
                    {
                        auto nstml = new LIR::StmLinkedList(
                            new LIR::MoveStm(new LIR::TempVarExp(mvit.first), mvit.second), fh->next);
                        fh->next = nstml;
                    }
                    mv.clear();
                    break;
                }
                fh = stml;
                stml = stml->next;
            }
        }
    }

public:
    CP(SSA::SSAIR *_ir)
    {
        this->ir = _ir;
        this->nodes = _ir->all_nodes();
        this->setup();
        this->bfsMark();
        this->replaceTemp();
        this->cleanup();
        checkCopy();
        cleanCopy();

        cleanSpPhi();

        checkCopy();
        cleanCopy();
    }
};