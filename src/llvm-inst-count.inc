//Borrowed from LLVM's InstCount.cpp

#include "llvm/Analysis/InstCount.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#ifndef DEBUG_TYPE
#error "Expected a debug type to be provided to inst counting!"
#endif

#ifndef PASS_NAME
#error "Expected a pass name to be provided to inst counting!"
#endif

STATISTIC(TotalInsts, "Number of instructions (of all types)");
STATISTIC(TotalBlocks, "Number of basic blocks");
STATISTIC(TotalFuncs, "Number of non-external functions");

#define HANDLE_INST(N, OPCODE, CLASS)                                          \
STATISTIC(Num##OPCODE##Inst, "Number of " #OPCODE " insts");

#include "llvm/IR/Instruction.def"

namespace {
class InstCount : public InstVisitor<InstCount> {
friend class InstVisitor<InstCount>;

void visitFunction(Function &F) { ++TotalFuncs; }
void visitBasicBlock(BasicBlock &BB) { ++TotalBlocks; }

#define HANDLE_INST(N, OPCODE, CLASS)                                          \
void visit##OPCODE(CLASS &) {                                                  \
    ++Num##OPCODE##Inst;                                                       \
    ++TotalInsts;                                                              \
}

#include "llvm/IR/Instruction.def"

void visitInstruction(Instruction &I) {
    errs() << "Instruction Count does not know about " << I;
    llvm_unreachable(nullptr);
}
};
} // namespace

#define expand(before, PASS_NAME, after) before##PASS_NAME##after
#define initializePass(PASS_NAME) expand(initialize, PASS_NAME, Pass)
#define createPass(PASS_NAME) expand(create, PASS_NAME,)
namespace {
class PASS_NAME : public FunctionPass {
public:
static char ID; // Pass identification, replacement for typeid
PASS_NAME() : FunctionPass(ID) {}

bool runOnFunction(Function &F) override {
    InstCount().visit(F);
    return false;
};

void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
}

void print(raw_ostream &O, const Module *M) const override {}
};
} // namespace

char PASS_NAME::ID = 0;
#define STR_PASS_NAME(PASS_NAME) #PASS_NAME
static RegisterPass<PASS_NAME> X(STR_PASS_NAME(PASS_NAME), "Count instructions in function",
                                false /* Only looks at CFG */,
                                true /* Analysis Pass */);

FunctionPass *createPass(PASS_NAME)() { return new PASS_NAME(); }
