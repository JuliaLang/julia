// LLVM's BBVectorize but enabled only by explicit request
#include "llvm/Transforms/Vectorize.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/AliasSetTracker.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionAliasAnalysis.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Local.h"
#include <algorithm>
#include <iostream>

namespace llvm {
  void initializeHotspotBBVectorizePass(PassRegistry&);
  namespace {
    struct HotspotBBVectorize : public BasicBlockPass {
      static char ID; // Pass identification, replacement for typeid

      HotspotBBVectorize() : BasicBlockPass(ID) {
        initializeHotspotBBVectorizePass(*PassRegistry::getPassRegistry());
        initializeBBVectorizePass(*PassRegistry::getPassRegistry());
      }
    
      bool runOnBasicBlock(BasicBlock &BB) override {
        if (BB.getParent()->hasFnAttribute("hotspot")) {
          return vectorizeBasicBlock(this, BB);
        }
        return false;
      }

      void getAnalysisUsage(AnalysisUsage &AU) const override {
        BasicBlockPass::getAnalysisUsage(AU);
        AU.addRequired<AAResultsWrapperPass>();
        AU.addRequired<DominatorTreeWrapperPass>();
        AU.addRequired<ScalarEvolutionWrapperPass>();
        AU.addRequired<TargetLibraryInfoWrapperPass>();
        AU.addRequired<TargetTransformInfoWrapperPass>();
        AU.addPreserved<DominatorTreeWrapperPass>();
        AU.addPreserved<GlobalsAAWrapperPass>();
        AU.addPreserved<ScalarEvolutionWrapperPass>();
        AU.addPreserved<SCEVAAWrapperPass>();
        AU.setPreservesCFG();
      }
    };
  }

}
using namespace llvm;

BasicBlockPass *createHotSpotBBVectorizePass() {
  return new HotspotBBVectorize();
}

char HotspotBBVectorize::ID = 0;
static const char bb_vectorize_name[] = "Basic-Block Vectorization for hotspots";
INITIALIZE_PASS_BEGIN(HotspotBBVectorize, "hotspot-bb-vectorize", bb_vectorize_name, false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(GlobalsAAWrapperPass)
INITIALIZE_PASS_DEPENDENCY(SCEVAAWrapperPass)
INITIALIZE_PASS_END(HotspotBBVectorize, "hotspot-bb-vectorize", bb_vectorize_name, false, false)
