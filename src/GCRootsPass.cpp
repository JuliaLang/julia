#include <llvm/Transforms/Utils/BasicBlockUtils.h>

class GCRootsPass : public FunctionPass
{
public:
    typedef SmallVector<Value*,32> RootVec;
    
    static char ID;
    GCRootsPass() : FunctionPass(ID) {}
    
    virtual void getAnalysisUsage(AnalysisUsage &a) const {
        a.addRequired<DominatorTreeWrapperPass>();
    }
    
    bool runOnFunction(Function &F) override {
        //errs() << "gc roots pass for " << F.getName()) << '\n';
        DominatorTree& dtree = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
        //dtree.print(errs());
        return processBB(&F, dtree.getRootNode(), RootVec());
    }

    uint64_t constInt(Value *v)
    {
        ConstantInt *ci = dyn_cast<ConstantInt>(v);
        assert(ci);
        return ci->getZExtValue();
    }
    const int MAX_ARGS = 30;
    bool processBB(Function* F, DomTreeNodeBase<BasicBlock> *dnode, RootVec live_roots)
    {
        bool changed = false;
        BasicBlock *bb = dnode->getBlock();
        for (BasicBlock::iterator it = bb->begin(), end = bb->end(); it != end;) {
            Instruction *inst = &*it;
            it++;
            CallInst *ci;
            if ((ci = dyn_cast<CallInst>(inst))) {
                Function *callee = ci->getCalledFunction();
                if (callee) {
                    if (callee->getIntrinsicID() == Intrinsic::experimental_gc_statepoint) {
                        if (live_roots.size() > 0) {
                            int old_nargs = ci->getNumArgOperands();
                            int new_nargs = old_nargs + live_roots.size();
                            if (new_nargs > MAX_ARGS) {
                                fprintf(stderr, "warning: too many live roots (%d), fixme\n", live_roots.size());
                            }
                            else {
                                Value **new_args = (Value**)alloca(new_nargs*sizeof(Value*));
                                //new_args[0] = ConstantInt::get(T_uint64, 42);
                                for (int i = 0; i < old_nargs-1; i++)
                                    new_args[i] = ci->getArgOperand(i);
                                new_args[old_nargs-1] = ConstantInt::get(T_uint64, live_roots.size());

                                for (int i = 0; i < live_roots.size(); i++)
                                    new_args[old_nargs + i] = live_roots[i];
                            
                                CallInst *new_stp = CallInst::Create(callee, ArrayRef<Value*>(new_args, new_nargs));

                                ReplaceInstWithInst(ci, new_stp);
                                changed = true;
                                inst = new_stp;
                            }
                        }
                    }
                }
            }
            if (isa<CallInst>(inst) || isa<LoadInst>(inst)) {
                Type *ty = inst->getType();
                if (ty->isPointerTy() && ty->getPointerAddressSpace() == 1) {
                    //inst.dump();
                    live_roots.push_back(inst);
                }
            }
        }
        for (DomTreeNodeBase<BasicBlock> *child : dnode->getChildren()) {
            changed |= processBB(F, child, live_roots);
        }
        //errs() << "sz " << live_roots.size() << "\n";
        return changed;
    }
};

char GCRootsPass::ID = 0;
