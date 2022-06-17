// This file is a part of Julia. License is MIT: https://julialang.org/license

// Function multi-versioning
#define DEBUG_TYPE "julia_multiversioning"
#undef DEBUG

// LLVM pass to clone function for different archs

#include "llvm-version.h"
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/Pass.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/CallGraph.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/Cloning.h>

#include "julia.h"
#include "julia_internal.h"
#include "processor.h"
#include "support/dtypes.h"

#include <map>
#include <memory>
#include <set>
#include <vector>

#include "codegen_shared.h"
#include "julia_assert.h"

using namespace llvm;

extern Optional<bool> always_have_fma(Function&);

namespace {
constexpr uint32_t clone_mask =
    JL_TARGET_CLONE_LOOP | JL_TARGET_CLONE_SIMD | JL_TARGET_CLONE_MATH | JL_TARGET_CLONE_CPU;

// Treat identical mapping as missing and return `def` in that case.
// We mainly need this to identify cloned function using value map after LLVM cloning
// functions fills the map with identity entries.
template<typename T>
Value *map_get(T &&vmap, Value *key, Value *def=nullptr)
{
    auto val = vmap.lookup(key);
    if (!val || key == val)
        return def;
    return val;
}

// Iterate through uses of a particular type.
// Recursively scan through `ConstantExpr` and `ConstantAggregate` use.
template<typename U>
struct ConstantUses {
    template<typename T>
    struct Info {
        Use *use;
        T *val;
        // If `samebits == true`, the offset the original value appears in the constant.
        size_t offset;
        // This specify whether the original value appears in the current value in exactly
        // the same bit pattern (with possibly an offset determined by `offset`).
        bool samebits;
        Info(Use *use, T *val, size_t offset, bool samebits) :
            use(use),
            val(val),
            offset(offset),
            samebits(samebits)
        {
        }
        Info(Use *use, size_t offset, bool samebits) :
            use(use),
            val(cast<T>(use->getUser())),
            offset(offset),
            samebits(samebits)
        {
        }
    };
    using UseInfo = Info<U>;
    struct Frame : Info<Constant> {
        template<typename... Args>
        Frame(Args &&... args) :
            Info<Constant>(std::forward<Args>(args)...),
            cur(this->val->use_empty() ? nullptr : &*this->val->use_begin()),
            _next(cur ? cur->getNext() : nullptr)
        {
        }
    private:
        void next()
        {
            cur = _next;
            if (!cur)
                return;
            _next = cur->getNext();
        }
        Use *cur;
        Use *_next;
        friend struct ConstantUses;
    };
    ConstantUses(Constant *c, Module &M)
        : stack{Frame(nullptr, c, 0u, true)},
          M(M)
    {
        forward();
    }
    UseInfo get_info() const
    {
        auto &top = stack.back();
        return UseInfo(top.cur, top.offset, top.samebits);
    }
    const SmallVector<Frame, 4> &get_stack() const
    {
        return stack;
    }
    void next()
    {
        stack.back().next();
        forward();
    }
    bool done()
    {
        return stack.empty();
    }
private:
    void forward();
    SmallVector<Frame, 4> stack;
    Module &M;
};

template<typename U>
void ConstantUses<U>::forward()
{
    assert(!stack.empty());
    auto frame = &stack.back();
    const DataLayout &DL = M.getDataLayout();
    auto pop = [&] {
        stack.pop_back();
        if (stack.empty()) {
            return false;
        }
        frame = &stack.back();
        return true;
    };
    auto push = [&] (Use *use, Constant *c, size_t offset, bool samebits) {
        stack.emplace_back(use, c, offset, samebits);
        frame = &stack.back();
    };
    auto handle_constaggr = [&] (Use *use, ConstantAggregate *aggr) {
        if (!frame->samebits) {
            push(use, aggr, 0, false);
            return;
        }
        if (auto strct = dyn_cast<ConstantStruct>(aggr)) {
            auto layout = DL.getStructLayout(strct->getType());
            push(use, strct, frame->offset + layout->getElementOffset(use->getOperandNo()), true);
        }
        else if (auto ary = dyn_cast<ConstantArray>(aggr)) {
            auto elty = ary->getType()->getElementType();
            push(use, ary, frame->offset + DL.getTypeAllocSize(elty) * use->getOperandNo(), true);
        }
        else if (auto vec = dyn_cast<ConstantVector>(aggr)) {
            auto elty = vec->getType()->getElementType();
            push(use, vec, frame->offset + DL.getTypeAllocSize(elty) * use->getOperandNo(), true);
        }
        else {
            jl_safe_printf("Unknown ConstantAggregate:\n");
            llvm_dump(aggr);
            abort();
        }
    };
    auto handle_constexpr = [&] (Use *use, ConstantExpr *expr) {
        if (!frame->samebits) {
            push(use, expr, 0, false);
            return;
        }
        auto opcode = expr->getOpcode();
        if (opcode == Instruction::PtrToInt || opcode == Instruction::IntToPtr ||
            opcode == Instruction::AddrSpaceCast || opcode == Instruction::BitCast) {
            push(use, expr, frame->offset, true);
        }
        else {
            push(use, expr, 0, false);
        }
    };
    while (true) {
        auto use = frame->cur;
        if (!use) {
            if (!pop())
                return;
            continue;
        }
        auto user = use->getUser();
        if (isa<U>(user))
            return;
        frame->next();
        if (auto aggr = dyn_cast<ConstantAggregate>(user)) {
            handle_constaggr(use, aggr);
        }
        else if (auto expr = dyn_cast<ConstantExpr>(user)) {
            handle_constexpr(use, expr);
        }
    }
}

struct CloneCtx {
    struct Target {
        int idx;
        uint32_t flags;
        std::unique_ptr<ValueToValueMapTy> vmap; // ValueToValueMapTy is not movable....
        // function ids that needs relocation to be initialized
        std::set<uint32_t> relocs{};
        Target(int idx, const jl_target_spec_t &spec) :
            idx(idx),
            flags(spec.flags),
            vmap(new ValueToValueMapTy)
        {
        }
    };
    struct Group : Target {
        std::vector<Target> clones;
        std::set<uint32_t> clone_fs;
        Group(int base, const jl_target_spec_t &spec) :
            Target(base, spec),
            clones{},
            clone_fs{}
        {}
        Function *base_func(Function *orig_f) const
        {
            if (idx == 0)
                return orig_f;
            return cast<Function>(vmap->lookup(orig_f));
        }
    };
    CloneCtx(Module &M, function_ref<LoopInfo&(Function&)> GetLI, function_ref<CallGraph&()> GetCG, bool allow_bad_fvars);
    void clone_bases();
    void collect_func_infos();
    void clone_all_partials();
    void fix_gv_uses();
    void fix_inst_uses();
    void emit_metadata();
private:
    void prepare_vmap(ValueToValueMapTy &vmap);
    bool is_vector(FunctionType *ty) const;
    void clone_function(Function *F, Function *new_f, ValueToValueMapTy &vmap);
    uint32_t collect_func_info(Function &F);
    void check_partial(Group &grp, Target &tgt);
    void clone_partial(Group &grp, Target &tgt);
    void add_features(Function *F, StringRef name, StringRef features, uint32_t flags) const;
    template<typename T>
    T *add_comdat(T *G) const;
    uint32_t get_func_id(Function *F);
    template<typename Stack>
    Constant *rewrite_gv_init(const Stack& stack);
    template<typename Stack>
    Value *rewrite_inst_use(const Stack& stack, Value *replace, Instruction *insert_before);
    std::pair<uint32_t,GlobalVariable*> get_reloc_slot(Function *F);
    Constant *get_ptrdiff32(Constant *ptr, Constant *base) const;
    template<typename T>
    Constant *emit_offset_table(const std::vector<T*> &vars, StringRef name) const;
    void rewrite_alias(GlobalAlias *alias, Function* F);

    MDNode *tbaa_const;
    std::vector<jl_target_spec_t> specs;
    std::vector<Group> groups{};
    std::vector<Function*> fvars;
    std::vector<Constant*> gvars;
    Module &M;
    function_ref<LoopInfo&(Function&)> GetLI;
    function_ref<CallGraph&()> GetCG;

    // Map from original functiton to one based index in `fvars`
    std::map<const Function*,uint32_t> func_ids{};
    std::vector<Function*> orig_funcs{};
    std::vector<uint32_t> func_infos{};
    std::set<Function*> cloned{};
    // GV addresses and their corresponding function id (i.e. 0-based index in `fvars`)
    std::vector<std::pair<Constant*,uint32_t>> gv_relocs{};
    // Mapping from function id (i.e. 0-based index in `fvars`) to GVs to be initialized.
    std::map<uint32_t,GlobalVariable*> const_relocs;
    // Functions that were referred to by a global alias, and might not have other uses.
    std::set<uint32_t> alias_relocs;
    bool has_veccall{false};
    bool has_cloneall{false};
    bool allow_bad_fvars{false};
};

template<typename T>
static inline std::vector<T*> consume_gv(Module &M, const char *name, bool allow_bad_fvars)
{
    // Get information about sysimg export functions from the two global variables.
    // Strip them from the Module so that it's easier to handle the uses.
    GlobalVariable *gv = M.getGlobalVariable(name);
    assert(gv && gv->hasInitializer());
    auto *ary = cast<ConstantArray>(gv->getInitializer());
    unsigned nele = ary->getNumOperands();
    std::vector<T*> res(nele);
    unsigned i = 0;
    while (i < nele) {
        llvm::Value *val = ary->getOperand(i)->stripPointerCasts();
        if (allow_bad_fvars && (!isa<T>(val) || (isa<Function>(val) && cast<Function>(val)->isDeclaration()))) {
            // Shouldn't happen in regular use, but can happen in bugpoint.
            nele--;
            continue;
        }
        res[i++] = cast<T>(val);
    }
    res.resize(nele);
    assert(gv->use_empty());
    gv->eraseFromParent();
    if (ary->use_empty())
        ary->destroyConstant();
    return res;
}

// Collect basic information about targets and functions.
CloneCtx::CloneCtx(Module &M, function_ref<LoopInfo&(Function&)> GetLI, function_ref<CallGraph&()> GetCG, bool allow_bad_fvars)
    : tbaa_const(tbaa_make_child_with_context(M.getContext(), "jtbaa_const", nullptr, true).first),
      specs(jl_get_llvm_clone_targets()),
      fvars(consume_gv<Function>(M, "jl_sysimg_fvars", allow_bad_fvars)),
      gvars(consume_gv<Constant>(M, "jl_sysimg_gvars", false)),
      M(M),
      GetLI(GetLI),
      GetCG(GetCG),
      allow_bad_fvars(allow_bad_fvars)
{
    groups.emplace_back(0, specs[0]);
    uint32_t ntargets = specs.size();
    for (uint32_t i = 1; i < ntargets; i++) {
        auto &spec = specs[i];
        if (spec.flags & JL_TARGET_CLONE_ALL) {
            has_cloneall = true;
            groups.emplace_back(i, spec);
        }
        else {
            auto base = spec.base;
            bool found = false;
            for (auto &grp: groups) {
                if (grp.idx == base) {
                    found = true;
                    grp.clones.emplace_back(i, spec);
                    break;
                }
            }
            (void)found;
        }
    }
    uint32_t nfvars = fvars.size();
    for (uint32_t i = 0; i < nfvars; i++)
        func_ids[fvars[i]] = i + 1;
    for (auto &F: M) {
        if (F.empty())
            continue;
        orig_funcs.push_back(&F);
    }
}

void CloneCtx::prepare_vmap(ValueToValueMapTy &vmap)
{
    // Workaround LLVM `CloneFunctionInfo` bug (?) pre-5.0
    // The `DICompileUnit`s are being cloned but are not added to the `llvm.dbg.cu` metadata
    // which triggers assertions when generating native code/in the verifier.
    // Fix this by forcing an identical mapping for all `DICompileUnit` recorded.
    // The `DISubprogram` cloning on LLVM 5.0 handles this
    // but it doesn't hurt to enforce the identity either.
    auto &MD = vmap.MD();
    for (auto cu: M.debug_compile_units()) {
        MD[cu].reset(cu);
    }
}

void CloneCtx::clone_function(Function *F, Function *new_f, ValueToValueMapTy &vmap)
{
    Function::arg_iterator DestI = new_f->arg_begin();
    for (Function::const_arg_iterator J = F->arg_begin(); J != F->arg_end(); ++J) {
        DestI->setName(J->getName());
        vmap[&*J] = &*DestI++;
    }
    SmallVector<ReturnInst*,8> Returns;
#if JL_LLVM_VERSION >= 130000
    // We are cloning into the same module
    CloneFunctionInto(new_f, F, vmap, CloneFunctionChangeType::GlobalChanges, Returns);
#else
    CloneFunctionInto(new_f, F, vmap, true, Returns);
#endif
}

// Clone all clone_all targets. Makes sure that the base targets are all available.
void CloneCtx::clone_bases()
{
    if (!has_cloneall)
        return;
    uint32_t ngrps = groups.size();
    for (uint32_t gid = 1; gid < ngrps; gid++) {
        auto &grp = groups[gid];
        auto suffix = ".clone_" + std::to_string(grp.idx);
        auto &vmap = *grp.vmap;
        // Fill in old->new mapping. We need to do this before cloning the function so that
        // the intra target calls are automatically fixed up on cloning.
        for (auto F: orig_funcs) {
            Function *new_f = Function::Create(F->getFunctionType(), F->getLinkage(),
                                               F->getName() + suffix, &M);
            new_f->copyAttributesFrom(F);
            vmap[F] = new_f;
        }
        prepare_vmap(vmap);
        for (auto F: orig_funcs) {
            clone_function(F, cast<Function>(vmap.lookup(F)), vmap);
        }
    }
}

bool CloneCtx::is_vector(FunctionType *ty) const
{
    if (ty->getReturnType()->isVectorTy())
        return true;
    for (auto arg: ty->params()) {
        if (arg->isVectorTy()) {
            return true;
        }
    }
    return false;
}

uint32_t CloneCtx::collect_func_info(Function &F)
{
    uint32_t flag = 0;
    if (!GetLI(F).empty())
        flag |= JL_TARGET_CLONE_LOOP;
    if (is_vector(F.getFunctionType())) {
        flag |= JL_TARGET_CLONE_SIMD;
        has_veccall = true;
    }
    for (auto &bb: F) {
        for (auto &I: bb) {
            if (auto call = dyn_cast<CallInst>(&I)) {
                if (is_vector(call->getFunctionType())) {
                    has_veccall = true;
                    flag |= JL_TARGET_CLONE_SIMD;
                }
                if (auto callee = call->getCalledFunction()) {
                    auto name = callee->getName();
                    if (name.startswith("llvm.muladd.") || name.startswith("llvm.fma.")) {
                        flag |= JL_TARGET_CLONE_MATH;
                    }
                    else if (name.startswith("julia.cpu.")) {
                        if (name.startswith("julia.cpu.have_fma.")) {
                            // for some platforms we know they always do (or don't) support
                            // FMA. in those cases we don't need to clone the function.
                            if (!always_have_fma(*callee).hasValue())
                                flag |= JL_TARGET_CLONE_CPU;
                        } else {
                            flag |= JL_TARGET_CLONE_CPU;
                        }
                    }
                }
            }
            else if (auto store = dyn_cast<StoreInst>(&I)) {
                if (store->getValueOperand()->getType()->isVectorTy()) {
                    flag |= JL_TARGET_CLONE_SIMD;
                }
            }
            else if (I.getType()->isVectorTy()) {
                flag |= JL_TARGET_CLONE_SIMD;
            }
            if (auto mathOp = dyn_cast<FPMathOperator>(&I)) {
                if (mathOp->getFastMathFlags().any()) {
                    flag |= JL_TARGET_CLONE_MATH;
                }
            }
            if (has_veccall && (flag & JL_TARGET_CLONE_SIMD) && (flag & JL_TARGET_CLONE_MATH)) {
                return flag;
            }
        }
    }
    return flag;
}

void CloneCtx::collect_func_infos()
{
    uint32_t nfuncs = orig_funcs.size();
    func_infos.resize(nfuncs);
    for (uint32_t i = 0; i < nfuncs; i++) {
        func_infos[i] = collect_func_info(*orig_funcs[i]);
    }
}

void CloneCtx::clone_all_partials()
{
    // First decide what to clone
    // Do this before actually cloning the functions
    // so that the call graph is easier to understand
    for (auto &grp: groups) {
        for (auto &tgt: grp.clones) {
            check_partial(grp, tgt);
        }
    }
    for (auto &grp: groups) {
        for (auto &tgt: grp.clones)
            clone_partial(grp, tgt);
        // Also set feature strings for base target functions
        // now that all the actual cloning is done.
        auto &base_spec = specs[grp.idx];
        for (auto orig_f: orig_funcs) {
            add_features(grp.base_func(orig_f), base_spec.cpu_name,
                         base_spec.cpu_features, base_spec.flags);
        }
    }
    func_infos.clear(); // We don't need this anymore
}

void CloneCtx::check_partial(Group &grp, Target &tgt)
{
    auto flag = specs[tgt.idx].flags & clone_mask;
    auto suffix = ".clone_" + std::to_string(tgt.idx);
    auto &vmap = *tgt.vmap;
    uint32_t nfuncs = func_infos.size();

    std::set<Function*> all_origs;
    // Use a simple heuristic to decide which function we need to clone.
    for (uint32_t i = 0; i < nfuncs; i++) {
        if (!(func_infos[i] & flag))
            continue;
        auto orig_f = orig_funcs[i];
        // Fill in old->new mapping. We need to do this before cloning the function so that
        // the intra target calls are automatically fixed up on cloning.
        auto F = grp.base_func(orig_f);
        Function *new_f = Function::Create(F->getFunctionType(), F->getLinkage(),
                                           F->getName() + suffix, &M);
        new_f->copyAttributesFrom(F);
        vmap[F] = new_f;
        if (!has_cloneall)
            cloned.insert(orig_f);
        grp.clone_fs.insert(i);
        all_origs.insert(orig_f);
    }
    std::set<Function*> sets[2]{all_origs, std::set<Function*>{}};
    auto *cur_set = &sets[0];
    auto *next_set = &sets[1];
    // Reduce dispatch by expand the cloning set to functions that are directly called by
    // and calling cloned functions.
    auto &graph = GetCG();
    while (!cur_set->empty()) {
        for (auto orig_f: *cur_set) {
            // Use the uncloned function since it's already in the call graph
            auto node = graph[orig_f];
            for (const auto &I: *node) {
                auto child_node = I.second;
                auto orig_child_f = child_node->getFunction();
                if (!orig_child_f)
                    continue;
                // Already cloned
                if (all_origs.count(orig_child_f))
                    continue;
                bool calling_clone = false;
                for (const auto &I2: *child_node) {
                    auto orig_child_f2 = I2.second->getFunction();
                    if (!orig_child_f2)
                        continue;
                    if (all_origs.count(orig_child_f2)) {
                        calling_clone = true;
                        break;
                    }
                }
                if (!calling_clone)
                    continue;
                next_set->insert(orig_child_f);
                all_origs.insert(orig_child_f);
                auto child_f = grp.base_func(orig_child_f);
                Function *new_f = Function::Create(child_f->getFunctionType(),
                                                   child_f->getLinkage(),
                                                   child_f->getName() + suffix, &M);
                new_f->copyAttributesFrom(child_f);
                vmap[child_f] = new_f;
            }
        }
        std::swap(cur_set, next_set);
        next_set->clear();
    }
    for (uint32_t i = 0; i < nfuncs; i++) {
        // Only need to handle expanded functions
        if (func_infos[i] & flag)
            continue;
        auto orig_f = orig_funcs[i];
        if (all_origs.count(orig_f)) {
            if (!has_cloneall)
                cloned.insert(orig_f);
            grp.clone_fs.insert(i);
        }
    }
}

void CloneCtx::clone_partial(Group &grp, Target &tgt)
{
    auto &spec = specs[tgt.idx];
    auto &vmap = *tgt.vmap;
    uint32_t nfuncs = orig_funcs.size();
    prepare_vmap(vmap);
    for (uint32_t i = 0; i < nfuncs; i++) {
        auto orig_f = orig_funcs[i];
        auto F = grp.base_func(orig_f);
        if (auto new_v = map_get(vmap, F)) {
            auto new_f = cast<Function>(new_v);
            assert(new_f != F);
            clone_function(F, new_f, vmap);
            // We can set the feature strings now since no one is going to
            // clone these functions again.
            add_features(new_f, spec.cpu_name, spec.cpu_features, spec.flags);
        }
    }
}

void CloneCtx::add_features(Function *F, StringRef name, StringRef features, uint32_t flags) const
{
    auto attr = F->getFnAttribute("target-features");
    if (attr.isStringAttribute()) {
        std::string new_features(attr.getValueAsString());
        new_features += ",";
        new_features += features;
        F->addFnAttr("target-features", new_features);
    }
    else {
        F->addFnAttr("target-features", features);
    }
    F->addFnAttr("target-cpu", name);
    if (!F->hasFnAttribute(Attribute::OptimizeNone)) {
        if (flags & JL_TARGET_OPTSIZE) {
            F->addFnAttr(Attribute::OptimizeForSize);
        }
        else if (flags & JL_TARGET_MINSIZE) {
            F->addFnAttr(Attribute::MinSize);
        }
    }
}

uint32_t CloneCtx::get_func_id(Function *F)
{
    auto &ref = func_ids[F];
    if (!ref) {
        if (allow_bad_fvars && F->isDeclaration()) {
            // This should never happen in regular use, but can happen if
            // bugpoint deletes the function. Just do something here to
            // allow bugpoint to proceed.
            return (uint32_t)-1;
        }
        fvars.push_back(F);
        ref = fvars.size();
    }
    return ref - 1;
}

template<typename Stack>
Constant *CloneCtx::rewrite_gv_init(const Stack& stack)
{
    // Null initialize so that LLVM put it in the correct section.
    SmallVector<Constant*, 8> args;
    Constant *res = ConstantPointerNull::get(cast<PointerType>(stack[0].val->getType()));
    uint32_t nlevel = stack.size();
    for (uint32_t i = 1; i < nlevel; i++) {
        auto &frame = stack[i];
        auto val = frame.val;
        Use *use = frame.use;
        unsigned idx = use->getOperandNo();
        unsigned nargs = val->getNumOperands();
        args.resize(nargs);
        for (unsigned j = 0; j < nargs; j++) {
            if (idx == j) {
                args[j] = res;
            }
            else {
                args[j] = cast<Constant>(val->getOperand(j));
            }
        }
        if (auto expr = dyn_cast<ConstantExpr>(val)) {
            res = expr->getWithOperands(args);
        }
        else if (auto ary = dyn_cast<ConstantArray>(val)) {
            res = ConstantArray::get(ary->getType(), args);
        }
        else if (auto strct = dyn_cast<ConstantStruct>(val)) {
            res = ConstantStruct::get(strct->getType(), args);
        }
        else if (isa<ConstantVector>(val)) {
            res = ConstantVector::get(args);
        }
        else {
            jl_safe_printf("Unknown const use.");
            llvm_dump(val);
            abort();
        }
    }
    return res;
}

// replace an alias to a function with a trampoline and (uninitialized) global variable slot
void CloneCtx::rewrite_alias(GlobalAlias *alias, Function *F)
{
    assert(!is_vector(F->getFunctionType()));

    Function *trampoline =
        Function::Create(F->getFunctionType(), alias->getLinkage(), "", &M);
    trampoline->copyAttributesFrom(F);
    trampoline->takeName(alias);
    alias->eraseFromParent();

    uint32_t id;
    GlobalVariable *slot;
    std::tie(id, slot) = get_reloc_slot(F);
    for (auto &grp: groups) {
        grp.relocs.insert(id);
        for (auto &tgt: grp.clones) {
            tgt.relocs.insert(id);
        }
    }
    alias_relocs.insert(id);

    auto BB = BasicBlock::Create(F->getContext(), "top", trampoline);
    IRBuilder<> irbuilder(BB);

    auto ptr = irbuilder.CreateLoad(F->getType(), slot);
    ptr->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
    ptr->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(F->getContext(), None));

    std::vector<Value *> Args;
    for (auto &arg : trampoline->args())
        Args.push_back(&arg);
    auto call = irbuilder.CreateCall(F->getFunctionType(), ptr, makeArrayRef(Args));
    if (F->isVarArg())
#if (defined(_CPU_ARM_) || defined(_CPU_PPC_) || defined(_CPU_PPC64_))
        abort();    // musttail support is very bad on ARM, PPC, PPC64 (as of LLVM 3.9)
#else
        call->setTailCallKind(CallInst::TCK_MustTail);
#endif
    else
        call->setTailCallKind(CallInst::TCK_Tail);

    if (F->getReturnType() == Type::getVoidTy(F->getContext()))
        irbuilder.CreateRetVoid();
    else
        irbuilder.CreateRet(call);
}

void CloneCtx::fix_gv_uses()
{
    auto single_pass = [&] (Function *orig_f) {
        bool changed = false;
        for (auto uses = ConstantUses<GlobalValue>(orig_f, M); !uses.done(); uses.next()) {
            changed = true;
            auto &stack = uses.get_stack();
            auto info = uses.get_info();
            // We only support absolute pointer relocation.
            assert(info.samebits);
            GlobalVariable *val;
            if (auto alias = dyn_cast<GlobalAlias>(info.val)) {
                rewrite_alias(alias, orig_f);
                continue;
            }
            else {
                val = cast<GlobalVariable>(info.val);
            }
            assert(info.use->getOperandNo() == 0);
            assert(!val->isConstant());
            auto fid = get_func_id(orig_f);
            auto addr = ConstantExpr::getPtrToInt(val, getSizeTy(val->getContext()));
            if (info.offset)
                addr = ConstantExpr::getAdd(addr, ConstantInt::get(getSizeTy(val->getContext()), info.offset));
            gv_relocs.emplace_back(addr, fid);
            val->setInitializer(rewrite_gv_init(stack));
        }
        return changed;
    };
    for (auto orig_f: orig_funcs) {
        if (!has_cloneall && !cloned.count(orig_f))
            continue;
        while (single_pass(orig_f)) {
        }
    }
}

std::pair<uint32_t,GlobalVariable*> CloneCtx::get_reloc_slot(Function *F)
{
    // Null initialize so that LLVM put it in the correct section.
    auto id = get_func_id(F);
    auto &slot = const_relocs[id];
    if (!slot)
        slot = new GlobalVariable(M, F->getType(), false, GlobalVariable::InternalLinkage,
                                  ConstantPointerNull::get(F->getType()),
                                  F->getName() + ".reloc_slot");
    return std::make_pair(id, slot);
}

template<typename Stack>
Value *CloneCtx::rewrite_inst_use(const Stack& stack, Value *replace, Instruction *insert_before)
{
    SmallVector<Constant*, 8> args;
    uint32_t nlevel = stack.size();
    for (uint32_t i = 1; i < nlevel; i++) {
        auto &frame = stack[i];
        auto val = frame.val;
        Use *use = frame.use;
        unsigned idx = use->getOperandNo();
        if (auto expr = dyn_cast<ConstantExpr>(val)) {
            auto inst = expr->getAsInstruction();
            inst->replaceUsesOfWith(val->getOperand(idx), replace);
            inst->insertBefore(insert_before);
            replace = inst;
            continue;
        }
        unsigned nargs = val->getNumOperands();
        args.resize(nargs);
        for (unsigned j = 0; j < nargs; j++) {
            auto op = val->getOperand(j);
            if (idx == j) {
                args[j] = UndefValue::get(op->getType());
            }
            else {
                args[j] = cast<Constant>(op);
            }
        }
        if (auto ary = dyn_cast<ConstantArray>(val)) {
            replace = InsertValueInst::Create(ConstantArray::get(ary->getType(), args),
                                              replace, {idx}, "", insert_before);
        }
        else if (auto strct = dyn_cast<ConstantStruct>(val)) {
            replace = InsertValueInst::Create(ConstantStruct::get(strct->getType(), args),
                                              replace, {idx}, "", insert_before);
        }
        else if (isa<ConstantVector>(val)) {
            replace = InsertElementInst::Create(ConstantVector::get(args), replace,
                                                ConstantInt::get(getSizeTy(insert_before->getContext()), idx), "",
                                                insert_before);
        }
        else {
            jl_safe_printf("Unknown const use.");
            llvm_dump(val);
            abort();
        }
    }
    return replace;
}

void CloneCtx::fix_inst_uses()
{
    uint32_t nfuncs = orig_funcs.size();
    for (auto &grp: groups) {
        auto suffix = ".clone_" + std::to_string(grp.idx);
        for (uint32_t i = 0; i < nfuncs; i++) {
            if (!grp.clone_fs.count(i))
                continue;
            auto orig_f = orig_funcs[i];
            auto F = grp.base_func(orig_f);
            bool changed;
            do {
                changed = false;
                for (auto uses = ConstantUses<Instruction>(F, M); !uses.done(); uses.next()) {
                    auto info = uses.get_info();
                    auto use_i = info.val;
                    auto use_f = use_i->getFunction();
                    if (!use_f->getName().endswith(suffix))
                        continue;
                    Instruction *insert_before = use_i;
                    if (auto phi = dyn_cast<PHINode>(use_i))
                        insert_before = phi->getIncomingBlock(*info.use)->getTerminator();
                    uint32_t id;
                    GlobalVariable *slot;
                    std::tie(id, slot) = get_reloc_slot(orig_f);
                    Instruction *ptr = new LoadInst(orig_f->getType(), slot, "", false, insert_before);
                    ptr->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
                    ptr->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(ptr->getContext(), None));
                    use_i->setOperand(info.use->getOperandNo(),
                                      rewrite_inst_use(uses.get_stack(), ptr,
                                                       insert_before));

                    grp.relocs.insert(id);
                    for (auto &tgt: grp.clones) {
                        // The enclosing function of the use is cloned,
                        // no need to deal with this use on this target.
                        if (map_get(*tgt.vmap, use_f))
                            continue;
                        tgt.relocs.insert(id);
                    }

                    changed = true;
                }
            } while (changed);
        }
    }
}

template<typename T>
inline T *CloneCtx::add_comdat(T *G) const
{
#if defined(_OS_WINDOWS_)
    // add __declspec(dllexport) to everything marked for export
    if (G->getLinkage() == GlobalValue::ExternalLinkage)
        G->setDLLStorageClass(GlobalValue::DLLExportStorageClass);
    else
        G->setDLLStorageClass(GlobalValue::DefaultStorageClass);
#endif
    return G;
}

Constant *CloneCtx::get_ptrdiff32(Constant *ptr, Constant *base) const
{
    if (ptr->getType()->isPointerTy())
        ptr = ConstantExpr::getPtrToInt(ptr, getSizeTy(ptr->getContext()));
    auto ptrdiff = ConstantExpr::getSub(ptr, base);
    return sizeof(void*) == 8 ? ConstantExpr::getTrunc(ptrdiff, Type::getInt32Ty(ptr->getContext())) : ptrdiff;
}

template<typename T>
Constant *CloneCtx::emit_offset_table(const std::vector<T*> &vars, StringRef name) const
{
    auto T_int32 = Type::getInt32Ty(M.getContext());
    auto T_size = getSizeTy(M.getContext());
    assert(!vars.empty());
    add_comdat(GlobalAlias::create(T_size, 0, GlobalVariable::ExternalLinkage,
                                   name + "_base",
                                   ConstantExpr::getBitCast(vars[0], T_size->getPointerTo()), &M));
    auto vbase = ConstantExpr::getPtrToInt(vars[0], T_size);
    uint32_t nvars = vars.size();
    std::vector<Constant*> offsets(nvars + 1);
    offsets[0] = ConstantInt::get(T_int32, nvars);
    offsets[1] = ConstantInt::get(T_int32, 0);
    for (uint32_t i = 1; i < nvars; i++)
        offsets[i + 1] = get_ptrdiff32(vars[i], vbase);
    ArrayType *vars_type = ArrayType::get(T_int32, nvars + 1);
    add_comdat(new GlobalVariable(M, vars_type, true,
                                  GlobalVariable::ExternalLinkage,
                                  ConstantArray::get(vars_type, offsets),
                                  name + "_offsets"));
    return vbase;
}

void CloneCtx::emit_metadata()
{
    uint32_t nfvars = fvars.size();
    if (allow_bad_fvars && nfvars == 0) {
        // Will result in a non-loadable sysimg, but `allow_bad_fvars` is for bugpoint only
        return;
    }

    // Store back the information about exported functions.
    auto fbase = emit_offset_table(fvars, "jl_sysimg_fvars");
    auto gbase = emit_offset_table(gvars, "jl_sysimg_gvars");

    uint32_t ntargets = specs.size();
    SmallVector<Target*, 8> targets(ntargets);
    for (auto &grp: groups) {
        targets[grp.idx] = &grp;
        for (auto &tgt: grp.clones) {
            targets[tgt.idx] = &tgt;
        }
    }

    // Generate `jl_dispatch_target_ids`
    {
        const uint32_t base_flags = has_veccall ? JL_TARGET_VEC_CALL : 0;
        std::vector<uint8_t> data;
        auto push_i32 = [&] (uint32_t v) {
            uint8_t buff[4];
            memcpy(buff, &v, 4);
            data.insert(data.end(), buff, buff + 4);
        };
        push_i32(ntargets);
        for (uint32_t i = 0; i < ntargets; i++) {
            push_i32(base_flags | (specs[i].flags & JL_TARGET_UNKNOWN_NAME));
            auto &specdata = specs[i].data;
            data.insert(data.end(), specdata.begin(), specdata.end());
        }
        auto value = ConstantDataArray::get(M.getContext(), data);
        add_comdat(new GlobalVariable(M, value->getType(), true,
                                      GlobalVariable::ExternalLinkage,
                                      value, "jl_dispatch_target_ids"));
    }

    // Generate `jl_dispatch_reloc_slots`
    std::set<uint32_t> shared_relocs;
    {
        auto T_int32 = Type::getInt32Ty(M.getContext());
        std::stable_sort(gv_relocs.begin(), gv_relocs.end(),
                         [] (const std::pair<Constant*,uint32_t> &lhs,
                             const std::pair<Constant*,uint32_t> &rhs) {
                             return lhs.second < rhs.second;
                         });
        std::vector<Constant*> values{nullptr};
        uint32_t gv_reloc_idx = 0;
        uint32_t ngv_relocs = gv_relocs.size();
        for (uint32_t id = 0; id < nfvars; id++) {
            // TODO:
            // explicitly set section? so that we are sure the relocation slots
            // are in the same section as `gbase`.
            auto id_v = ConstantInt::get(T_int32, id);
            for (; gv_reloc_idx < ngv_relocs && gv_relocs[gv_reloc_idx].second == id;
                 gv_reloc_idx++) {
                shared_relocs.insert(id);
                values.push_back(id_v);
                values.push_back(get_ptrdiff32(gv_relocs[gv_reloc_idx].first, gbase));
            }
            auto it = const_relocs.find(id);
            if (it != const_relocs.end()) {
                values.push_back(id_v);
                values.push_back(get_ptrdiff32(it->second, gbase));
            }
            if (alias_relocs.find(id) != alias_relocs.end()) {
                shared_relocs.insert(id);
            }
        }
        values[0] = ConstantInt::get(T_int32, values.size() / 2);
        ArrayType *vars_type = ArrayType::get(T_int32, values.size());
        add_comdat(new GlobalVariable(M, vars_type, true, GlobalVariable::ExternalLinkage,
                                      ConstantArray::get(vars_type, values),
                                      "jl_dispatch_reloc_slots"));
    }

    // Generate `jl_dispatch_fvars_idxs` and `jl_dispatch_fvars_offsets`
    {
        std::vector<uint32_t> idxs;
        std::vector<Constant*> offsets;
        for (uint32_t i = 0; i < ntargets; i++) {
            auto tgt = targets[i];
            auto &spec = specs[i];
            uint32_t len_idx = idxs.size();
            idxs.push_back(0); // We will fill in the real value later.
            uint32_t count = 0;
            if (i == 0 || spec.flags & JL_TARGET_CLONE_ALL) {
                auto grp = static_cast<Group*>(tgt);
                count = jl_sysimg_tag_mask;
                for (uint32_t j = 0; j < nfvars; j++) {
                    if (shared_relocs.count(j) || tgt->relocs.count(j)) {
                        count++;
                        idxs.push_back(j);
                    }
                    if (i != 0) {
                        offsets.push_back(get_ptrdiff32(grp->base_func(fvars[j]), fbase));
                    }
                }
            }
            else {
                auto baseidx = spec.base;
                auto grp = static_cast<Group*>(targets[baseidx]);
                idxs.push_back(baseidx);
                for (uint32_t j = 0; j < nfvars; j++) {
                    auto base_f = grp->base_func(fvars[j]);
                    if (shared_relocs.count(j) || tgt->relocs.count(j)) {
                        count++;
                        idxs.push_back(jl_sysimg_tag_mask | j);
                        auto f = map_get(*tgt->vmap, base_f, base_f);
                        offsets.push_back(get_ptrdiff32(cast<Function>(f), fbase));
                    }
                    else if (auto f = map_get(*tgt->vmap, base_f)) {
                        count++;
                        idxs.push_back(j);
                        offsets.push_back(get_ptrdiff32(cast<Function>(f), fbase));
                    }
                }
            }
            idxs[len_idx] = count;
        }
        auto idxval = ConstantDataArray::get(M.getContext(), idxs);
        add_comdat(new GlobalVariable(M, idxval->getType(), true,
                                      GlobalVariable::ExternalLinkage,
                                      idxval, "jl_dispatch_fvars_idxs"));
        ArrayType *offsets_type = ArrayType::get(Type::getInt32Ty(M.getContext()), offsets.size());
        add_comdat(new GlobalVariable(M, offsets_type, true,
                                      GlobalVariable::ExternalLinkage,
                                      ConstantArray::get(offsets_type, offsets),
                                      "jl_dispatch_fvars_offsets"));
    }
}

static bool runMultiVersioning(Module &M, function_ref<LoopInfo&(Function&)> GetLI, function_ref<CallGraph&()> GetCG, bool allow_bad_fvars)
{
    // Group targets and identify cloning bases.
    // Also initialize function info maps (we'll update these maps as we go)
    // Maps that we need includes,
    //
    //     * Original function -> ID (initialize from `fvars` and allocate ID lazily)
    //     * Cloned function -> Original function (add as we clone functions)
    //     * Original function -> Base function (target specific and updated by LLVM)
    //     * ID -> relocation slots (const).
    if (M.getName() == "sysimage")
        return false;

    GlobalVariable *fvars = M.getGlobalVariable("jl_sysimg_fvars");
    GlobalVariable *gvars = M.getGlobalVariable("jl_sysimg_gvars");
    if (allow_bad_fvars && (!fvars || !fvars->hasInitializer() || !isa<ConstantArray>(fvars->getInitializer()) ||
                            !gvars || !gvars->hasInitializer() || !isa<ConstantArray>(gvars->getInitializer())))
        return false;

    CloneCtx clone(M, GetLI, GetCG, allow_bad_fvars);

    // Collect a list of original functions and clone base functions
    clone.clone_bases();

    // Collect function info (type of instruction used)
    clone.collect_func_infos();

    // If any partially cloned target exist decide which functions to clone for these targets.
    // Clone functions for each group and collect a list of them.
    // We can also add feature strings for cloned functions
    // now that no additional cloning needs to be done.
    clone.clone_all_partials();

    // Scan **ALL** cloned functions (including full cloning for base target)
    // for global variables initialization use.
    // Replace them with `null` slot to be initialized at runtime and record relocation slot.
    // These relocations must be initialized for **ALL** targets.
    clone.fix_gv_uses();

    // For each group, scan all functions cloned by **PARTIALLY** cloned targets for
    // instruction use.
    // A function needs a const relocation slot if it is cloned and is called by a
    // uncloned function for at least one partially cloned target in the group.
    // This is also the condition that a use in an uncloned function needs to be replaced with
    // a slot load (i.e. if both the caller and the callee are always cloned or not cloned
    // on all targets, the caller site does not need a relocation slot).
    // A target needs a slot to be initialized iff at least one caller is not initialized.
    clone.fix_inst_uses();

    // Store back sysimg information with the correct format.
    // At this point, we should have fixed up all the uses of the cloned functions
    // and collected all the shared/target-specific relocations.
    clone.emit_metadata();

    assert(!verifyModule(M));

    return true;
}

struct MultiVersioningLegacy: public ModulePass {
    static char ID;
    MultiVersioningLegacy(bool allow_bad_fvars=false)
        : ModulePass(ID), allow_bad_fvars(allow_bad_fvars)
    {}

private:
    bool runOnModule(Module &M) override;
    void getAnalysisUsage(AnalysisUsage &AU) const override
    {
        AU.addRequired<LoopInfoWrapperPass>();
        AU.addRequired<CallGraphWrapperPass>();
        AU.addPreserved<LoopInfoWrapperPass>();
    }
    bool allow_bad_fvars;
};

bool MultiVersioningLegacy::runOnModule(Module &M)
{
    auto GetLI = [this](Function &F) -> LoopInfo & {
        return getAnalysis<LoopInfoWrapperPass>(F).getLoopInfo();
    };
    auto GetCG = [this]() -> CallGraph & {
        return getAnalysis<CallGraphWrapperPass>().getCallGraph();
    };
    return runMultiVersioning(M, GetLI, GetCG, allow_bad_fvars);
}


char MultiVersioningLegacy::ID = 0;
static RegisterPass<MultiVersioningLegacy> X("JuliaMultiVersioning", "JuliaMultiVersioning Pass",
                                       false /* Only looks at CFG */,
                                       false /* Analysis Pass */);

} // anonymous namespace

PreservedAnalyses MultiVersioning::run(Module &M, ModuleAnalysisManager &AM)
{
    auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
    auto GetLI = [&](Function &F) -> LoopInfo & {
        return FAM.getResult<LoopAnalysis>(F);
    };
    auto GetCG = [&]() -> CallGraph & {
        return AM.getResult<CallGraphAnalysis>(M);
    };
    if (runMultiVersioning(M, GetLI, GetCG, external_use)) {
        auto preserved = PreservedAnalyses::allInSet<CFGAnalyses>();
        preserved.preserve<LoopAnalysis>();
        return preserved;
    }
    return PreservedAnalyses::all();
}

Pass *createMultiVersioningPass(bool allow_bad_fvars)
{
    return new MultiVersioningLegacy(allow_bad_fvars);
}

extern "C" JL_DLLEXPORT void LLVMExtraAddMultiVersioningPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createMultiVersioningPass(false));
}
