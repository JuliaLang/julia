#define DEBUG_TYPE "annotate_array_alloc"
#undef DEBUG
#include "llvm-version.h"

#include <llvm/InitializePasses.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>

#include "llvm-pass-helpers.h"
#include "llvm-alloc-helpers.h"

#include "julia.h"

using namespace llvm;

namespace {
    struct AnnotateArrayAlloc : public FunctionPass, public JuliaPassContext {
        static char ID;
        AnnotateArrayAlloc(): FunctionPass(ID) {}
        bool runOnFunction(Function &F) {
            AttrBuilder builder;
            builder.addAttribute(Attribute::NoAlias);
            builder.addAttribute(Attribute::NonNull);
            bool changed = false;
            for (auto &bb : F) {
                for (auto &I : bb) {
                    if (auto call = dyn_cast<CallInst>(&I)) {
                        jl_alloc::AllocIdInfo info;
                        if (jl_alloc::getAllocIdInfo(info, call, nullptr)) {
                            assert(info.isarray);
                            call->setAttributes(call->getAttributes().addAttributes(F.getContext(), AttributeList::ReturnIndex, builder));
                            changed = true;
                        }
                    }
                }
            }
            return changed;
        }
    };

char AnnotateArrayAlloc::ID = 0;
static RegisterPass<AnnotateArrayAlloc> X("AnnotateArrayAlloc", "Add array allocation attributes",
                                false /* Only looks at CFG */,
                                false /* Analysis Pass */);
}


Pass *createAnnotateArrayAllocPass()
{
    return new AnnotateArrayAlloc();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddAnnotateArrayAllocPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createAnnotateArrayAllocPass());
}
