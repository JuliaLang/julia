#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/DenseMap.h>

class GCRootsPass : public FunctionPass
{
public:
    typedef SmallVector<Value*,32> RootVec;
    typedef DenseSet<Value*> ValSet;
    typedef DenseMap<CallInst*, ValSet> LiveAt;
    typedef DenseMap<BasicBlock*, ValSet> LiveState;
    static char ID;
    GCRootsPass() : FunctionPass(ID) {}

    virtual void getAnalysisUsage(AnalysisUsage &a) const {
        a.addRequired<DominatorTreeWrapperPass>();
    }

    bool isGCptr(Type *ty)
    {
        return ty->isPointerTy() && ty->getPointerAddressSpace() == 1;
    }

    // check if inst is a direct gc value
    bool isGCval(Instruction *inst)
    {
        if (isa<CallInst>(inst) || isa<LoadInst>(inst)) {
            Type *ty = inst->getType();
            checkSupportedType(ty);
            return isGCptr(ty);
        }
        return false;
    }

    bool isStatepoint(Instruction *inst)
    {
        CallInst *ci;
        if ((ci = dyn_cast<CallInst>(inst))) {
            Function *callee = ci->getCalledFunction();
            if (callee &&
                callee->getIntrinsicID() == Intrinsic::experimental_gc_statepoint)
                return true;
        }
        return false;
    }

    bool runOnFunction(Function &F) override {
        //errs() << "gc roots pass for " << F.getName()) << '\n';
        DominatorTree& dtree = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
        ValSet gc_values;
        std::vector<BasicBlock*> exit_blocks;
        for(BasicBlock& b : F.getBasicBlockList()) {
            if (isa<ReturnInst>(b.getTerminator()) ||
                isa<UnreachableInst>(b.getTerminator()))
                exit_blocks.push_back(&b);
            for (Instruction& i : b.getInstList()) {
                if (isGCval(&i)) {
                    gc_values.insert(&i);
                }
            }
        }
        //dtree.print(errs());
        LiveState live_state;
        LiveAt live_at;
        for (BasicBlock *b : exit_blocks)
            computeLiveness(live_state, live_at, b, gc_values, ValSet());
        bool changed = false;
        for (auto it = live_at.begin(); it != live_at.end(); it++)
            changed |= rewriteStatepoint(it->getFirst(), it->getSecond());
        return changed;
    }

    const int MAX_ARGS = 200;

    bool rewriteStatepoint(CallInst *statepoint, ValSet live_roots)
    {
        int old_nargs = statepoint->getNumArgOperands();
        int new_nargs = old_nargs + live_roots.size();
        if (new_nargs > MAX_ARGS) {
            fprintf(stderr, "warning: too many live roots (%d), fixme\n", live_roots.size());
        }
        else {
            Value **new_args = (Value**)alloca(new_nargs*sizeof(Value*));
            for (int i = 0; i < old_nargs-1; i++)
                new_args[i] = statepoint->getArgOperand(i);
            new_args[old_nargs-1] = ConstantInt::get(T_uint64, live_roots.size());

            unsigned i = 0;
            for (auto it = live_roots.begin(); it != live_roots.end(); ++it) {
                new_args[old_nargs + i] = *it;
                i++;
            }

            CallInst *new_stp = CallInst::Create(statepoint->getCalledFunction(), ArrayRef<Value*>(new_args, new_nargs));

            ReplaceInstWithInst(statepoint, new_stp);
            return true;
        }
        return false;
    }

    void computeLiveness(LiveState &state, LiveAt &live_at, BasicBlock *b, ValSet &gc_values, ValSet live_values)
    {
        for (auto it = b->rbegin(); it != b->rend(); ++it) {
            Instruction *inst = &*it;
            if (!isa<PHINode>(inst)) {
                for (unsigned i = 0; i < inst->getNumOperands(); i++) {
                    Value *arg = inst->getOperand(i);
                    if (gc_values.count(arg)) {
                        live_values.insert(arg); // use (gen)
                    }
                }
            }
            if (live_values.count(inst)) {
                live_values.erase(inst); // def (kill)
            }
            if (isStatepoint(inst)) {
                CallInst *ci = dyn_cast<CallInst>(inst);
                assert(ci);
                live_at[ci].insert(live_values.begin(), live_values.end());
            }
        }
        ValSet& s = state[b];
        bool must_continue = false;
        for (Value *live : live_values) {
            if (!s.count(live)) {
                must_continue = true;
                s.insert(live);
            }
        }
        if (!must_continue)
            return;
        for (auto it = pred_begin(b); it != pred_end(b); it++) {
            computeLiveness(state, live_at, *it, gc_values, live_values);
        }
    }

    uint64_t constInt(Value *v)
    {
        ConstantInt *ci = dyn_cast<ConstantInt>(v);
        assert(ci);
        return ci->getZExtValue();
    }

    void checkSupportedType(Type *ty)
    {
        if(ty->isAggregateType()) {
            for (Type *elty : ty->subtypes()) {
                if (isGCptr(elty)) {
                    fprintf(stderr, "unsuported gc ptr inside aggregate\n");
                    ty->dump();
                    abort();
                }
                checkSupportedType(elty);
            }
        }
    }
};

char GCRootsPass::ID = 0;
