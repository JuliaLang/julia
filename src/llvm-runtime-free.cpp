// This file is a part of Julia. License is MIT: https://julialang.org/license

// Verification of runtime-free functions.
//
// A runtime-free callback (`@cfunction(f, rt, at, runtime_free=true)`) is a
// function that C may call from any context: on a thread the runtime does not
// know, while the GC is running, from `JL_NOTSAFEPOINT` code, from an error
// path. It is made of runtime-free functions: its entry point, and the code of
// `f` and of all that this calls. The code of such a function must not
// interact with the runtime at all. It must not
//
//   - find the current task (pgcstack), and so not push a GC frame, read or
//     set the world age, change the GC state or reach a safepoint;
//   - allocate, or run a write barrier;
//   - throw, or set up an exception handler;
//   - call anything other than another runtime-free function, an LLVM
//     intrinsic that is plain code, or a `libc` function that LLVM itself
//     calls for such code (`memcmp` and its like).
//
// `jl_verify_runtime_free` decides that for one function, from its LLVM IR:
// everything that is not known to be fine is a violation. Codegen
// (`emit_runtime_free_cfunction`) asks it about the code that it emitted, to
// refuse a function that it cannot compile that way. That verdict is about IR
// that still has to be lowered, so it relies on what the lowering passes do
// with the few `julia.*` intrinsics that are allowed below (and on nothing
// else: there is no call for which they could place a GC frame). The
// `VerifyRuntimeFree` pass therefore asks again at the end of the pipeline,
// about the code that will run, and does not let a violation pass.

#include "llvm-version.h"
#include "passes.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

using namespace llvm;

// `name` is `entry`, or starts with it at a `.` (an overloaded intrinsic's
// type suffix, or the rest of a family that `entry` names with a final `.`)
static bool matchesName(StringRef name, StringRef entry) JL_NOTSAFEPOINT
{
    if (!name.consume_front(entry))
        return false;
    return name.empty() || name.front() == '.' || entry.back() == '.';
}

// LLVM intrinsics that are plain code (or a call of a `libm`/`libc` function
// that is)
static bool isPlainIntrinsic(const Function &F) JL_NOTSAFEPOINT
{
    static const StringRef allowed[] = {
        // memory
        "llvm.memcpy", "llvm.memmove", "llvm.memset", "llvm.lifetime.", "llvm.invariant.",
        "llvm.launder.invariant.group", "llvm.strip.invariant.group", "llvm.prefetch",
        "llvm.stacksave", "llvm.stackrestore", "llvm.masked.", "llvm.ptrmask",
        // hints, and what optimizes away
        "llvm.assume", "llvm.experimental.noalias.scope.decl", "llvm.dbg.", "llvm.donothing",
        "llvm.expect", "llvm.is.constant", "llvm.objectsize", "llvm.sideeffect",
        "llvm.pseudoprobe", "llvm.fake.use", "llvm.ssa.copy", "llvm.annotation",
        "llvm.ptr.annotation", "llvm.var.annotation",
        // the end of a path that is not taken; it does not return
        "llvm.trap",
        // integers
        "llvm.ctlz", "llvm.cttz", "llvm.ctpop", "llvm.bswap", "llvm.bitreverse", "llvm.fshl",
        "llvm.fshr", "llvm.abs", "llvm.smax", "llvm.smin", "llvm.umax", "llvm.umin",
        "llvm.scmp", "llvm.ucmp", "llvm.sadd.", "llvm.uadd.", "llvm.ssub.", "llvm.usub.",
        "llvm.smul.", "llvm.umul.", "llvm.sshl.", "llvm.ushl.",
        // floating point
        "llvm.fabs", "llvm.sqrt", "llvm.fma", "llvm.fmuladd", "llvm.copysign", "llvm.floor",
        "llvm.ceil", "llvm.trunc", "llvm.rint", "llvm.nearbyint", "llvm.round",
        "llvm.roundeven", "llvm.lrint", "llvm.llrint", "llvm.lround", "llvm.llround",
        "llvm.minnum", "llvm.maxnum", "llvm.minimum", "llvm.maximum", "llvm.minimumnum",
        "llvm.maximumnum", "llvm.canonicalize", "llvm.is.fpclass", "llvm.fptosi.sat",
        "llvm.fptoui.sat", "llvm.pow", "llvm.powi", "llvm.exp", "llvm.exp2", "llvm.exp10",
        "llvm.log", "llvm.log2", "llvm.log10", "llvm.sin", "llvm.cos", "llvm.tan",
        "llvm.sincos", "llvm.asin", "llvm.acos", "llvm.atan", "llvm.atan2", "llvm.sinh",
        "llvm.cosh", "llvm.tanh", "llvm.frexp", "llvm.ldexp", "llvm.convert.to.fp16",
        "llvm.convert.from.fp16",
        // vectors
        "llvm.vector.", "llvm.vp.", "llvm.experimental.vector.", "llvm.experimental.vp.",
        "llvm.stepvector", "llvm.experimental.stepvector", "llvm.vscale",
        "llvm.get.active.lane.mask",
        // Julia's: these only keep a value alive across safepoints, and there
        // are none
        "llvm.julia.gc_preserve_begin", "llvm.julia.gc_preserve_end",
    };
    // an instruction of the target
    if (F.isTargetIntrinsic())
        return true;
    StringRef name = F.getName();
    for (StringRef entry : allowed)
        if (matchesName(name, entry))
            return true;
    return false;
}

// Julia's intrinsics that lower to plain code
static bool isPlainJuliaIntrinsic(StringRef name) JL_NOTSAFEPOINT
{
    return name == "julia.typeof" || name == "julia.pointer_from_objref" ||
           name == "julia.gc_loaded" || name.starts_with("julia.cpu.have_fma.") ||
           name.starts_with("julia.cpu.supports.");
}

// `libc` functions that LLVM calls for plain code (loop idioms, comparisons
// of memory), and that codegen calls for the same
static bool isPlainLibcall(StringRef name) JL_NOTSAFEPOINT
{
    return name == "memcmp" || name == "bcmp" || name == "memchr" || name == "strlen" ||
           name == "wcslen" || name == "memcpy" || name == "memmove" || name == "memset" ||
           name == "memset_pattern4" || name == "memset_pattern8" || name == "memset_pattern16";
}

// What a call of a function of the runtime means for the user
static const char *describeRuntimeCall(StringRef name) JL_NOTSAFEPOINT
{
    if (name.starts_with("ijl_"))
        name = name.substr(1); // `jl_` in the runtime library
    if (name.starts_with("julia.gc_alloc") || name.starts_with("jl_gc_") ||
        name.starts_with("jl_box_") || name.starts_with("jl_alloc_") ||
        name.starts_with("jl_new_") || name == "jl_copy_ast" || name.contains("genericmemory"))
        return "it allocates";
    if (name.contains("throw") || name.contains("error") || name.contains("_checked") || name == "jl_typeassert")
        return "it can throw";
    if (name.starts_with("julia.call") || name == "jl_apply_generic" || name.starts_with("jl_invoke") ||
        name.starts_with("jl_f_"))
        return "it makes a call that is not resolved statically";
    if (name == "julia.safepoint")
        return "it has a GC safepoint";
    if (name.contains("write_barrier") || name == "jl_gc_queue_root")
        return "it stores a reference into an object";
    if (name.starts_with("julia.get_pgcstack"))
        return "it uses the current task";
    return nullptr;
}

static std::string violation(const Instruction &I, const Twine &what, const char *meaning = nullptr) JL_NOTSAFEPOINT
{
    std::string msg;
    raw_string_ostream os(msg);
    if (meaning)
        os << meaning << ": ";
    os << what;
    if (const DebugLoc &DL = I.getDebugLoc()) {
        // the innermost frame, and the line in the function that was compiled
        const DILocation *Loc = DL.get();
        os << " (at " << Loc->getFilename() << ":" << Loc->getLine();
        const DILocation *Outer = Loc;
        while (const DILocation *IA = Outer->getInlinedAt())
            Outer = IA;
        if (Outer != Loc)
            os << ", inlined at " << Outer->getFilename() << ":" << Outer->getLine();
        os << ")";
    }
    return msg;
}

std::string jl_verify_runtime_free(const Function &F, StringRef calleeAttr)
{
    assert(!F.isDeclaration());
    // Most of what needs the runtime needs the task for it, so a use of the
    // task is only what is wrong if nothing more telling is. The task comes
    // from `julia.get_pgcstack` (a call, see below), or, for a function that
    // takes it as an argument, from there.
    std::string uses_task;
    for (const Argument &A : F.args()) {
        if ((A.hasSwiftSelfAttr() || F.getAttributes().getParamAttrs(A.getArgNo()).hasAttribute("gcstack")) &&
            !A.use_empty())
            uses_task = "it uses the current task";
    }
    for (const Instruction &I : instructions(F)) {
        if (isa<InvokeInst>(I) || isa<CallBrInst>(I) || isa<LandingPadInst>(I) || isa<ResumeInst>(I) ||
            isa<CatchSwitchInst>(I) || isa<CatchPadInst>(I) || isa<CleanupPadInst>(I) ||
            isa<CatchReturnInst>(I) || isa<CleanupReturnInst>(I))
            return violation(I, "it handles exceptions");
        const CallInst *CI = dyn_cast<CallInst>(&I);
        if (!CI)
            continue;
        if (CI->isInlineAsm())
            // n.b. this is also how the lowered code finds the current task
            return violation(I, "it has inline assembly, or uses the current task");
        const Function *Callee = dyn_cast<Function>(CI->getCalledOperand()->stripPointerCasts());
        if (!Callee)
            // n.b. this is also how the lowered code finds the current task
            return violation(I, "it calls a function pointer, or uses the current task");
        StringRef name = Callee->getName();
        if (name == "julia.get_pgcstack") {
            // codegen emits this for every function, and it is removed if unused
            if (!CI->use_empty() && uses_task.empty())
                uses_task = "it uses the current task";
            continue;
        }
        if (isPlainJuliaIntrinsic(name))
            continue;
        if (Callee->isIntrinsic()) {
            if (isPlainIntrinsic(*Callee))
                continue;
            return violation(I, "it calls `" + name + "`");
        }
        if (isPlainLibcall(name))
            continue;
        if (Callee->hasFnAttribute("julia.runtime_free") ||
            (!calleeAttr.empty() && Callee->hasFnAttribute(calleeAttr)))
            continue;
        StringRef shown = name;
        if (name.starts_with("julia.call") && CI->arg_size() > 0) {
            // these call their first argument
            if (const Function *Target = dyn_cast<Function>(CI->getArgOperand(0)->stripPointerCasts()))
                shown = Target->getName();
        }
        const char *meaning = describeRuntimeCall(shown);
        if (!meaning)
            meaning = describeRuntimeCall(name);
        return violation(I, "it calls `" + shown + "`", meaning);
    }
    return uses_task;
}

PreservedAnalyses VerifyRuntimeFreePass::run(Function &F, FunctionAnalysisManager &AM)
{
    if (F.isDeclaration() || !F.hasFnAttribute("julia.runtime_free"))
        return PreservedAnalyses::all();
    std::string why = jl_verify_runtime_free(F);
    if (!why.empty()) {
        // Codegen only marks a function that it verified, so this is a bug in
        // it or in a pass. Do not let the code run.
        errs() << "Internal error: the runtime-free function `" << F.getName()
               << "` needs the runtime once compiled: " << why << "\n";
        abort();
    }
    return PreservedAnalyses::all();
}
