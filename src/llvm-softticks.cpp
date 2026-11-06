// This file is a part of Julia. License is MIT: https://julialang.org/license

// Software ticks for rr: generated code that counts its own progress, so that rr can record
// and replay it without the CPU's branch counter (rr-debugger/rr#4104).
//
// rr identifies a point in an execution by the registers and a count of "ticks", normally
// the retired conditional branches counted by the PMU. A program built for software ticks
// counts them itself instead: a tick decrements a countdown at a fixed address and traps when
// it reaches zero, and rr programs the countdown like a PMU sample period. A countdown with its
// top bit set is disarmed, and a tick only reads it: outside rr it is 2^64 - 1 (on x86-64 and
// aarch64, a new page's 0 wraps to that at the first tick), so that there the ticks of all
// threads share its cache line without writing it. The compilers that build such a program
// (with a software-ticks pass for GCC or LLVM) instrument the code they compile; this pass does
// the same for the code Julia generates, so that JIT output and system and package images tick
// like the C code of an instrumented libjulia.
//
// rr needs that no instrumented instruction executes twice in the same stack frame without a
// tick in between, so ticks go
//   1. at the start of every function, after its leading allocas, which cuts every cycle
//      through calls (recursion, callbacks);
//   2. at the start of every block that is the target of a retreating edge of a depth-first
//      walk of the CFG: every cycle of the function, reducible or not, contains one;
//   3. right after every call to a returns_twice function (setjmp: a longjmp back to it
//      takes an edge the CFG does not show).
// The pipeline runs this pass last, so the ticks are a property of the code that runs. Code
// generation may duplicate or merge a tick sequence, but not change how often one executes.
//
// In imaging mode (system and package images, --output-o), every module with ticks also gets
// the ABI's initializer, which maps the countdown's page when the image is loaded outside rr,
// and the ELF note that marks the image as instrumented. Both are in one COMDAT group, so a
// linked image has one of each however many of its shards tick. JIT code needs neither: an
// instrumented runtime maps the page from its own initializers (and jl_softticks_init does it
// when -julia-softticks enabled the ticks in a runtime that is not instrumented).

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalIFunc.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Debug.h>
#include <llvm/TargetParser/Triple.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include "julia.h"
#include "jitlayers.h"

#ifdef _OS_LINUX_
#include <sys/mman.h>
#endif

#define DEBUG_TYPE "softticks"

STATISTIC(SoftTicksInserted, "Number of software ticks inserted");
STATISTIC(SoftTicksFunctions, "Number of functions instrumented with software ticks");

using namespace llvm;

// The value of -julia-softticks if it was given (see jl_softticks_init), else -1.
static int SoftTicksOption = -1;

bool jl_softticks_enabled(void) JL_NOTSAFEPOINT
{
    if (SoftTicksOption >= 0)
        return SoftTicksOption;
    return JL_SOFTTICKS_DEFAULT;
}

// Registered at run time, like jl_init_llvm's other options, rather than as a static cl::opt,
// which a second copy of this library loaded into the process would register twice.
void jl_softticks_add_option(void) JL_NOTSAFEPOINT
{
    if (!cl::getRegisteredOptions().lookup("julia-softticks"))
        new cl::opt<bool>(
            "julia-softticks", cl::Hidden,
            cl::desc("Instrument generated code with software ticks for rr (default: on if "
                     "Julia was built for software ticks, i.e. with __RR_SOFTTICKS__ defined)"));
}

// The tracee ABI, version 1, from rr's include/rr/softticks.h (rr-debugger/rr#4104), whose
// comments define it; the definitions below are copied from there unchanged, for every
// architecture. They are ABI byte for byte: rr recognizes a tick trap by the sequence's code,
// and a new ABI version comes with new sequences.
#define RR_SOFTTICKS_ABI_VERSION 1

/* The offset of the countdown in the preload thread-locals page. rr's
 * struct preload_thread_locals uses the first PRELOAD_THREAD_LOCALS_SIZE bytes
 * (144, plus 1040 on aarch64). */
#define RR_SOFTTICKS_COUNTDOWN_OFFSET 0x800

/* The priority of the page-mapping initializer: before every user constructor
 * (101 and up) and every other initializer of the implementation. */
#define RR_SOFTTICKS_INIT_PRIORITY 1

/* The symbol of the initializer. Hidden and in a COMDAT group, so each linked
 * module (executable or shared library) has one, however many of its
 * translation units are instrumented. (lld's ThinLTO ignores COMDAT groups in
 * module asm, so there it keeps one .init_array entry per backend object, all
 * calling the same initializer; running it again is harmless.) */
#define RR_SOFTTICKS_INIT_SYMBOL "__rr_softticks_init"

#define RR_SOFTTICKS_BRK_IMM 0x5354

#define RR_SOFTTICKS_NOTE_SECTION ".note.rrsoftticks"
#define RR_SOFTTICKS_NOTE_NAME "rr"
#define RR_SOFTTICKS_NOTE_TYPE 1

#define RR_SOFTTICKS_STR_(x) #x
#define RR_SOFTTICKS_STR(x) RR_SOFTTICKS_STR_(x)

/* The initializer's .init_array entry, in the initializer's COMDAT group. */
#define RR_SOFTTICKS_INIT_ARRAY_(ptr)                                          \
  "\t.section .init_array.00001,\"awG\",@init_array," RR_SOFTTICKS_INIT_SYMBOL \
  ",comdat\n"                                                                  \
  "\t.p2align " ptr "\n"

#define RR_SOFTTICKS_INIT_HEAD_(sectype)                                       \
  "\t.pushsection .text." RR_SOFTTICKS_INIT_SYMBOL ",\"axG\"," sectype ","     \
  RR_SOFTTICKS_INIT_SYMBOL ",comdat\n"                                         \
  "\t.weak " RR_SOFTTICKS_INIT_SYMBOL "\n"                                     \
  "\t.hidden " RR_SOFTTICKS_INIT_SYMBOL "\n"                                   \
  "\t.type " RR_SOFTTICKS_INIT_SYMBOL ",%function\n"                           \
  "\t.p2align 4\n" RR_SOFTTICKS_INIT_SYMBOL ":\n"

/* The note, in the initializer's COMDAT group. */
#define RR_SOFTTICKS_NOTE_(notetype)                                           \
  "\t.section " RR_SOFTTICKS_NOTE_SECTION ",\"aG\"," notetype ","             \
  RR_SOFTTICKS_INIT_SYMBOL ",comdat\n"                                         \
  "\t.p2align 2\n"                                                             \
  "\t.long 3\n"                 /* namesz: "rr\\0" */                          \
  "\t.long 4\n"                 /* descsz */                                   \
  "\t.long " RR_SOFTTICKS_STR(RR_SOFTTICKS_NOTE_TYPE) "\n"                     \
  "\t.asciz \"rr\"\n"                                                          \
  "\t.p2align 2\n"                                                             \
  "\t.long " RR_SOFTTICKS_STR(RR_SOFTTICKS_ABI_VERSION) "\n"

#define RR_SOFTTICKS_INIT_TAIL_(ptrdir, ptralign, notetype)                    \
  "\t.size " RR_SOFTTICKS_INIT_SYMBOL ",.-" RR_SOFTTICKS_INIT_SYMBOL "\n"      \
  RR_SOFTTICKS_INIT_ARRAY_(ptralign)                                           \
  "\t" ptrdir " " RR_SOFTTICKS_INIT_SYMBOL "\n"                                \
  RR_SOFTTICKS_NOTE_(notetype)                                                 \
  "\t.popsection\n"

/* mmap flags: MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE. Kernels before
 * 4.17 treat MAP_FIXED_NOREPLACE as a hint; a mapping placed elsewhere is
 * unmapped again. */
#define RR_SOFTTICKS_MMAP_FLAGS 0x100022


#define RR_SOFTTICKS_X86_PAGE_ADDR 0x70001000
#define RR_SOFTTICKS_X86_COUNTDOWN_ADDR 0x70001800

/* x86-64: 21 bytes; clobbers the flags only. A countdown with its top bit
 * set (the top bit of its last byte, 0x70001807) is disarmed: the tick only
 * tests it. */
#define RR_SOFTTICKS_X64_TICK_ASM                                              \
  "testb $0x80, 0x70001807\n\t"                                                \
  "jnz 1f\n\t"                                                                 \
  "decq 0x70001800\n\t"                                                        \
  "jnz 1f\n\t"                                                                 \
  "int3\n"                                                                     \
  "1:"

/* i386: 28 bytes; a 64-bit countdown, decremented one 32-bit word at a time:
 * the low word (0x70001800), and when that reaches 0, the high word
 * (0x70001804), trapping when that borrows. A countdown with its top bit set
 * (the top bit of the high word's last byte, 0x70001807) is disarmed: the tick
 * only tests it. Clobbers the flags only. */
#define RR_SOFTTICKS_X86_TICK_ASM                                              \
  "testb $0x80, 0x70001807\n\t"                                                \
  "jnz 1f\n\t"                                                                 \
  "subl $1, 0x70001800\n\t"                                                    \
  "jnz 1f\n\t"                                                                 \
  "subl $1, 0x70001804\n\t"                                                    \
  "jae 1f\n\t"                                                                 \
  "int3\n"                                                                     \
  "1:"

#define RR_SOFTTICKS_X64_INIT_ASM                                              \
  RR_SOFTTICKS_INIT_HEAD_("@progbits")                                         \
  "\tmovl $9, %eax\n"            /* SYS_mmap */                                \
  "\tmovl $0x70001000, %edi\n"                                                 \
  "\tmovl $4096, %esi\n"                                                       \
  "\tmovl $3, %edx\n"            /* PROT_READ | PROT_WRITE */                  \
  "\tmovl $0x100022, %r10d\n"                                                  \
  "\tmovq $-1, %r8\n"                                                          \
  "\txorl %r9d, %r9d\n"                                                        \
  "\tsyscall\n"                                                                \
  "\tcmpq $0x70001000, %rax\n"                                                 \
  "\tje 1f\n"                                                                  \
  "\tcmpq $-4095, %rax\n"                                                      \
  "\tjae 1f\n"                   /* an error: EEXIST under rr */               \
  "\tmovq %rax, %rdi\n"                                                        \
  "\tmovl $11, %eax\n"           /* SYS_munmap */                              \
  "\tmovl $4096, %esi\n"                                                       \
  "\tsyscall\n"                                                                \
  "1:\tret\n"                                                                  \
  RR_SOFTTICKS_INIT_TAIL_(".quad", "3", "@note")

#define RR_SOFTTICKS_X86_INIT_ASM                                              \
  RR_SOFTTICKS_INIT_HEAD_("@progbits")                                         \
  "\tpushl %ebx\n"                                                             \
  "\tpushl %esi\n"                                                             \
  "\tpushl %edi\n"                                                             \
  "\tpushl %ebp\n"                                                             \
  "\tmovl $192, %eax\n"          /* SYS_mmap2 */                               \
  "\tmovl $0x70001000, %ebx\n"                                                 \
  "\tmovl $4096, %ecx\n"                                                       \
  "\tmovl $3, %edx\n"                                                          \
  "\tmovl $0x100022, %esi\n"                                                   \
  "\tmovl $-1, %edi\n"                                                         \
  "\txorl %ebp, %ebp\n"                                                        \
  "\tint $0x80\n"                                                              \
  "\tcmpl $0x70001000, %eax\n"                                                 \
  "\tjne 2f\n"                                                                 \
  "\tmovl $-1, 0x70001800\n"    /* created the page: 2^64 - 1 to go */         \
  "\tmovl $-1, 0x70001804\n"                                                   \
  "\tjmp 1f\n"                                                                 \
  "2:\n"                                                                       \
  "\tcmpl $-4095, %eax\n"                                                      \
  "\tjae 1f\n"                                                                 \
  "\tmovl %eax, %ebx\n"                                                        \
  "\tmovl $91, %eax\n"           /* SYS_munmap */                              \
  "\tmovl $4096, %ecx\n"                                                       \
  "\tint $0x80\n"                                                              \
  "1:\tpopl %ebp\n"                                                            \
  "\tpopl %edi\n"                                                              \
  "\tpopl %esi\n"                                                              \
  "\tpopl %ebx\n"                                                              \
  "\tret\n"                                                                    \
  RR_SOFTTICKS_INIT_TAIL_(".long", "2", "@note")



/* PRELOAD_LIBRARY_PAGE_SIZE is 64 KiB on aarch64. */
#define RR_SOFTTICKS_ARM64_PAGE_ADDR 0x70010000
#define RR_SOFTTICKS_ARM64_COUNTDOWN_ADDR 0x70010800

/* aarch64: 32 bytes; clobbers x16 and x17 (the intra-procedure-call scratch
 * registers), not the flags. A countdown with its top bit set is disarmed: the
 * tick only loads it. x17 holds the countdown only inside the sequence: it is
 * cleared at the end, because rr compares general registers between recording and
 * replay and the countdown's values differ between the two. (On x86 only the flags
 * depend on the countdown, and rr does not compare them.) */
#define RR_SOFTTICKS_ARM64_TICK_ASM                                            \
  "movz x16, #0x7001, lsl #16\n\t"                                             \
  "ldr x17, [x16, #0x800]\n\t"                                                 \
  "tbnz x17, #63, 1f\n\t"                                                      \
  "sub x17, x17, #1\n\t"                                                       \
  "str x17, [x16, #0x800]\n\t"                                                 \
  "cbnz x17, 1f\n\t"                                                           \
  "brk #0x5354\n"                                                              \
  "1:\n\t"                                                                     \
  "mov x17, xzr"

#define RR_SOFTTICKS_ARM64_INIT_ASM                                            \
  RR_SOFTTICKS_INIT_HEAD_("%progbits")                                         \
  "\tmov x8, #222\n"             /* SYS_mmap */                                \
  "\tmovz x0, #0x0000\n"                                                       \
  "\tmovk x0, #0x7001, lsl #16\n"                                              \
  "\tmov x1, #4096\n"                                                          \
  "\tmov x2, #3\n"                                                             \
  "\tmovz x3, #0x0022\n"                                                       \
  "\tmovk x3, #0x10, lsl #16\n"                                                \
  "\tmov x4, #-1\n"                                                            \
  "\tmov x5, #0\n"                                                             \
  "\tsvc #0\n"                                                                 \
  "\tmovz x9, #0x0000\n"                                                       \
  "\tmovk x9, #0x7001, lsl #16\n"                                              \
  "\tcmp x0, x9\n"                                                             \
  "\tb.eq 1f\n"                                                                \
  "\tcmn x0, #4095\n"                                                          \
  "\tb.hs 1f\n"                                                                \
  "\tmov x8, #215\n"             /* SYS_munmap */                              \
  "\tmov x1, #4096\n"                                                          \
  "\tsvc #0\n"                                                                 \
  "1:\tret\n"                                                                  \
  RR_SOFTTICKS_INIT_TAIL_(".xword", "3", "%note")


namespace {

// The module flag that marks a module as instrumented, with the ABI version as its value: a
// marked module is not instrumented again.
constexpr const char *SoftTicksMarker = "rr.softticks";

// Functions placed in these sections belong to the ABI's runtime support and must not tick.
constexpr const char *SoftTicksReservedSection = ".text.__rr_softticks";

struct SoftTicksTarget {
    const char *TickAsm;
    // What clang emits for asm volatile(tick ::: clobbers), with the ABI's clobbers.
    const char *Constraints;
    const char *InitAsm;
};

const SoftTicksTarget X86_64Target = {RR_SOFTTICKS_X64_TICK_ASM,
                                      "~{cc},~{dirflag},~{fpsr},~{flags}",
                                      RR_SOFTTICKS_X64_INIT_ASM};
const SoftTicksTarget X86Target = {RR_SOFTTICKS_X86_TICK_ASM,
                                   "~{cc},~{dirflag},~{fpsr},~{flags}",
                                   RR_SOFTTICKS_X86_INIT_ASM};
const SoftTicksTarget AArch64Target = {RR_SOFTTICKS_ARM64_TICK_ASM, "~{x16},~{x17}",
                                       RR_SOFTTICKS_ARM64_INIT_ASM};

// The ABI is defined for Linux on x86-64 (not x32), i386 and aarch64 (not ILP32).
static const SoftTicksTarget *getSoftTicksTarget(const Triple &T) JL_NOTSAFEPOINT
{
    if (!T.isOSLinux() || !T.isOSBinFormatELF())
        return nullptr;
    switch (T.getArch()) {
    case Triple::x86_64:
        return T.isX32() ? nullptr : &X86_64Target;
    case Triple::x86:
        return &X86Target;
    case Triple::aarch64:
        return T.getEnvironment() == Triple::GNUILP32 ? nullptr : &AArch64Target;
    default:
        return nullptr;
    }
}

// In an inline asm call '$' starts an operand reference; the tick has none (but the x86
// sequences have immediates such as "$0x80").
static std::string escapeInlineAsm(StringRef S) JL_NOTSAFEPOINT
{
    std::string Out;
    for (char C : S) {
        if (C == '$')
            Out += '$';
        Out += C;
    }
    return Out;
}

static bool shouldInstrument(const Function &F,
                             const SmallPtrSetImpl<const Function *> &Resolvers) JL_NOTSAFEPOINT
{
    if (F.isDeclaration() || F.hasAvailableExternallyLinkage())
        return false;
    if (F.hasFnAttribute(Attribute::Naked) ||
        F.hasFnAttribute(Attribute::DisableSanitizerInstrumentation))
        return false;
    // IFUNC resolvers run during relocation, before the initializer maps the page.
    if (Resolvers.count(&F))
        return false;
    if (F.hasSection() && F.getSection().starts_with(SoftTicksReservedSection))
        return false;
    return true;
}

// Where a tick goes at the start of BB: its first insertion point (after the phis and the EH
// pad). A catchswitch block has none: the tick goes to the start of each of its successors
// instead (its handlers and unwind destination, the only ways out of it).
static void addBlockStartTicks(BasicBlock *BB, SetVector<BasicBlock *> &Out) JL_NOTSAFEPOINT
{
    SmallVector<BasicBlock *, 4> Work{BB};
    SmallPtrSet<BasicBlock *, 4> Seen{BB};
    while (!Work.empty()) {
        BasicBlock *B = Work.pop_back_val();
        if (B->getFirstInsertionPt() != B->end()) {
            Out.insert(B);
            continue;
        }
        for (BasicBlock *S : successors(B))
            if (Seen.insert(S).second)
                Work.push_back(S);
    }
}

static void insertTick(InlineAsm *Tick, BasicBlock *BB, BasicBlock::iterator IP) JL_NOTSAFEPOINT
{
    IRBuilder<> B(BB, IP);
    B.SetCurrentDebugLocation(IP == BB->end() ? DebugLoc() : IP->getDebugLoc());
    CallInst *CI = B.CreateCall(Tick);
    CI->setDoesNotThrow();
}

// Instruments F; returns the number of ticks inserted.
static unsigned instrumentFunction(Function &F, InlineAsm *Tick) JL_NOTSAFEPOINT
{
    // Rule 3's call sites first: splitting an invoke's normal edge changes the CFG that the
    // walk below runs on.
    SmallVector<CallInst *, 4> TwiceCalls;
    SmallVector<InvokeInst *, 4> TwiceInvokes;
    for (BasicBlock &BB : F) {
        for (Instruction &I : BB) {
            auto *CB = dyn_cast<CallBase>(&I);
            if (!CB || !CB->hasFnAttr(Attribute::ReturnsTwice))
                continue;
            if (auto *CI = dyn_cast<CallInst>(CB)) {
                // A musttail call returns to our caller; nothing may follow it.
                if (!CI->isMustTailCall())
                    TwiceCalls.push_back(CI);
            }
            else if (auto *II = dyn_cast<InvokeInst>(CB)) {
                TwiceInvokes.push_back(II);
            }
        }
    }
    SmallVector<BasicBlock *, 4> TwiceDests;
    for (InvokeInst *II : TwiceInvokes) {
        BasicBlock *From = II->getParent(), *To = II->getNormalDest();
        // The tick must run only after the invoke: give it a block of its own if the normal
        // destination can be reached otherwise.
        if (!To->getSinglePredecessor())
            To = SplitEdge(From, To);
        TwiceDests.push_back(To);
    }

    // Blocks that tick at their start (rule 2, and rule 3 for invokes), once each.
    SetVector<BasicBlock *> StartTicks;
    SmallVector<std::pair<const BasicBlock *, const BasicBlock *>, 8> Backedges;
    FindFunctionBackedges(F, Backedges);
    for (auto &E : Backedges)
        addBlockStartTicks(const_cast<BasicBlock *>(E.second), StartTicks);
    for (BasicBlock *BB : TwiceDests)
        addBlockStartTicks(BB, StartTicks);

    // Rule 1: the entry block, after its leading allocas, so that they stay static allocas.
    // The entry block has no predecessors, so it is never the target of a retreating edge.
    BasicBlock &Entry = F.getEntryBlock();
    BasicBlock::iterator IP = Entry.getFirstInsertionPt();
    while (IP != Entry.end() && isa<AllocaInst>(*IP))
        ++IP;
    insertTick(Tick, &Entry, IP);
    unsigned N = 1;
    for (BasicBlock *BB : StartTicks) {
        insertTick(Tick, BB, BB->getFirstInsertionPt());
        ++N;
    }
    for (CallInst *CI : TwiceCalls) {
        insertTick(Tick, CI->getParent(), std::next(CI->getIterator()));
        ++N;
    }
    return N;
}

static bool definesSoftTicksInit(const Module &M) JL_NOTSAFEPOINT
{
    for (const Module::GlobalAsmFragment &Fragment : M.getModuleInlineAsm())
        if (StringRef(Fragment.Asm).contains(RR_SOFTTICKS_INIT_SYMBOL ":"))
            return true;
    return false;
}

} // namespace

bool addSoftTicks(Module &M, bool imaging) JL_NOTSAFEPOINT
{
    if (M.getModuleFlag(SoftTicksMarker))
        return false;
    const SoftTicksTarget *Target = getSoftTicksTarget(M.getTargetTriple());
    if (!Target) {
        LLVM_DEBUG(dbgs() << "softticks: unsupported target " << M.getTargetTriple().str()
                          << ", module " << M.getModuleIdentifier() << " not instrumented\n");
        return false;
    }

    SmallPtrSet<const Function *, 4> Resolvers;
    for (const GlobalIFunc &GI : M.ifuncs())
        if (const Function *R = GI.getResolverFunction())
            Resolvers.insert(R);

    LLVMContext &Ctx = M.getContext();
    InlineAsm *Tick = InlineAsm::get(FunctionType::get(Type::getVoidTy(Ctx), false),
                                     escapeInlineAsm(Target->TickAsm), Target->Constraints,
                                     /*hasSideEffects=*/true);
    unsigned Ticks = 0;
    for (Function &F : M) {
        if (!shouldInstrument(F, Resolvers))
            continue;
        Ticks += instrumentFunction(F, Tick);
        ++SoftTicksFunctions;
    }
    SoftTicksInserted += Ticks;
    // A module whose asm defines the initializer already (code instrumented by hand) gets no
    // second copy, which the assembler would reject.
    if (imaging && Ticks && !definesSoftTicksInit(M))
        M.appendModuleInlineAsm(StringRef(Target->InitAsm));
    M.addModuleFlag(Module::Max, SoftTicksMarker, RR_SOFTTICKS_ABI_VERSION);
    return true;
}

PreservedAnalyses SoftTicksPass::run(Module &M, ModuleAnalysisManager &AM)
{
    if (addSoftTicks(M, imaging_mode))
        return PreservedAnalyses::none();
    return PreservedAnalyses::all();
}

// Called once the LLVM options are parsed. A runtime built for software ticks maps the
// countdown's page from its initializers before any code runs; one that is not, but runs with
// -julia-softticks, maps it here, before generating any code that ticks. Under rr the page
// exists already.
void jl_softticks_init(void) JL_NOTSAFEPOINT
{
    cl::Option *opt = cl::getRegisteredOptions().lookup("julia-softticks");
    if (opt && opt->getNumOccurrences())
        SoftTicksOption = static_cast<cl::opt<bool>*>(opt)->getValue();
#if defined(_OS_LINUX_) && (defined(_CPU_X86_64_) || defined(_CPU_X86_) || defined(_CPU_AARCH64_))
    if (!jl_softticks_enabled())
        return;
#if defined(_CPU_AARCH64_)
    uintptr_t page = RR_SOFTTICKS_ARM64_PAGE_ADDR;
#else
    uintptr_t page = RR_SOFTTICKS_X86_PAGE_ADDR;
#endif
    // MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE: an existing page (EEXIST) is kept.
    void *p = mmap((void*)page, 4096, PROT_READ | PROT_WRITE, RR_SOFTTICKS_MMAP_FLAGS, -1, 0);
    if (p == MAP_FAILED)
        return;
    if ((uintptr_t)p != page) {
        // Kernels before 4.17 take MAP_FIXED_NOREPLACE as a hint.
        munmap(p, 4096);
        jl_safe_printf("WARNING: could not map the software ticks countdown at %p\n", (void*)page);
        return;
    }
#if defined(_CPU_X86_)
    // On i386 a page that we create holds 2^64 - 1, disarmed, as the ABI's initializer leaves
    // it. (The other countdowns start at 0, which their first tick wraps to 2^64 - 1; on i386
    // that tick would only decrement the low word, leaving the countdown armed.)
    ((volatile uint32_t*)(page + RR_SOFTTICKS_COUNTDOWN_OFFSET))[0] = 0xffffffff;
    ((volatile uint32_t*)(page + RR_SOFTTICKS_COUNTDOWN_OFFSET))[1] = 0xffffffff;
#endif
#endif
}
