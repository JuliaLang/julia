// This file is a part of Julia. License is MIT: https://julialang.org/license

// Emits the stubs of lazily bound calls (see src/plt.h) from the templates in
// src/arch/<arch>/plt_stubs.inc.

#include "llvm-version.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Module.h>
#include <llvm/TargetParser/Triple.h>

#include "jitlayers.h"

using namespace llvm;

#include "arch/x86_64/plt_stubs.inc"
#include "arch/i686/plt_stubs.inc"
#include "arch/aarch64/plt_stubs.inc"
#include "arch/arm/plt_stubs.inc"
#include "arch/powerpc64le/plt_stubs.inc"
#include "arch/riscv64/plt_stubs.inc"

// On PPC64, the slot thunks must not be strong definitions, which callers may
// assume to preserve the TOC pointer (r2). The trampoline intentionally does
// not preserve this register (`.localentry <thunk>, 1`, see plt_asm_ppc64le).
static void set_slot_thunk_linkage(Function *F, const Triple &TT) JL_NOTSAFEPOINT
{
    if (!TT.isPPC64()) {
        F->setLinkage(GlobalValue::PrivateLinkage);
        return;
    }
    F->setLinkage(GlobalValue::LinkOnceODRLinkage);
    F->setVisibility(GlobalValue::HiddenVisibility);
    F->addFnAttr("julia.slot_thunk");
}

bool isSlotThunk(const GlobalObject &G) JL_NOTSAFEPOINT
{
    auto F = dyn_cast<Function>(&G);
    return F && F->hasFnAttribute("julia.slot_thunk");
}

// Define `F` as the naked code `Code`, whose operands are the symbols `Ops`.
static void define_asm_function(Function *F, const char *Code,
                                ArrayRef<Value *> Ops) JL_NOTSAFEPOINT
{
    auto &Ctx = F->getContext();
    F->addFnAttr(Attribute::Naked);
    F->addFnAttr(Attribute::NoUnwind);
    F->addFnAttr("frame-pointer", "none");
    std::string Constraints;
    for (size_t i = 0; i < Ops.size(); i++)
        Constraints += i ? ",s" : "s";
    SmallVector<Type *, 3> OpTys(Ops.size(), PointerType::getUnqual(Ctx));
    auto IA = InlineAsm::get(FunctionType::get(Type::getVoidTy(Ctx), OpTys, false), Code,
                             Constraints, /*hasSideEffects=*/true, /*isAlignStack=*/false);
    IRBuilder<> B(BasicBlock::Create(Ctx, "", F));
    B.CreateCall(IA, Ops)->addFnAttr(Attribute::NoReturn);
    B.CreateUnreachable();
}

// Returns its return address in %eax, like __x86.get_pc_thunk.ax.
static Function *emit_pc_thunk_i686(Module &M) JL_NOTSAFEPOINT
{
    if (Function *F = M.getFunction("$jl$pc_thunk"))
        return F;
    auto F = Function::Create(FunctionType::get(Type::getVoidTy(M.getContext()), false),
                              GlobalValue::PrivateLinkage, "$jl$pc_thunk", &M);
    define_asm_function(F, "movl (%esp), %eax\nretl\n", {});
    return F;
}

namespace {
struct PLTAsm {
    const char *Jump;
    const char *Trampoline;
    // Whether both take the PC from emit_pc_thunk_i686, as their last operand
    bool PCThunk = false;
    // The alignment both need (see the templates)
    MaybeAlign Alignment = {};
};
} // namespace

static PLTAsm select_plt_asm(const Triple &TT, bool LargeCodeModel) JL_NOTSAFEPOINT
{
    if (TT.getArch() == Triple::x86_64)
        return LargeCodeModel ? PLTAsm{plt_asm_x86_64_large, plt_tramp_asm_x86_64_large}
                              : PLTAsm{plt_asm_x86_64, plt_tramp_asm_x86_64};
    if (TT.getArch() == Triple::x86)
        // The triple's object format is ELF during codegen on Windows, see
        // createTargetMachine, even for the COFF images.
        return TT.isOSWindows() ? PLTAsm{plt_asm_i686_coff, plt_tramp_asm_i686_coff}
                                : PLTAsm{plt_asm_i686, plt_tramp_asm_i686, /*PCThunk=*/true};
    if (TT.isAArch64()) {
        if (LargeCodeModel)
            return {plt_asm_aarch64_large, plt_tramp_asm_aarch64_large};
        return TT.isOSBinFormatMachO() ? PLTAsm{plt_asm_aarch64_macho, plt_tramp_asm_aarch64_macho}
                                      : PLTAsm{plt_asm_aarch64, plt_tramp_asm_aarch64};
    }
    if (TT.getArch() == Triple::riscv64)
        return LargeCodeModel ? PLTAsm{plt_asm_riscv64_large, plt_tramp_asm_riscv64_large,
                                       /*PCThunk=*/false, Align(8)}
                              : PLTAsm{plt_asm_riscv64, plt_tramp_asm_riscv64};
    if (TT.getArch() == Triple::arm)
        return {plt_asm_arm, plt_tramp_asm_arm};
    if (TT.getArch() == Triple::ppc64le)
        return {plt_asm_ppc64le, plt_tramp_asm_ppc64le};
    report_fatal_error("no PLT stub for target " + Twine(TT.str()) +
                       (LargeCodeModel ? " (large code model)" : ""));
}

// Define `F` as a thunk that jumps through `Slot`, leaving the arguments alone.
static void define_slot_jump_thunk(jl_codegen_output_t &out, Function *F, GlobalVariable *Slot,
                                   const PLTAsm &Asm) JL_NOTSAFEPOINT
{
    auto &TT = out.TargetTriple;
    set_slot_thunk_linkage(F, TT);
    SmallVector<Value *, 2> Ops{Slot};
    if (Asm.PCThunk)
        Ops.push_back(emit_pc_thunk_i686(out.get_module()));
    else if (TT.isPPC64())
        Ops.push_back(F);
    define_asm_function(F, Asm.Jump, Ops);
    F->addFnAttr("thunk");
    F->addFnAttr(Attribute::NoInline);
    F->setAlignment(Asm.Alignment);
}

// Define `StubFn` as a lazily bound stub (see src/plt.h), with its slot,
// trampoline and entry. The entry is {`Resolve`, slot, `Data`...}.
void emit_plt_stub(jl_codegen_output_t &out, Function *StubFn, StringRef Resolve,
                   ArrayRef<Constant *> Data, bool LargeCodeModel) JL_NOTSAFEPOINT
{
    auto &M = out.get_module();
    auto &Ctx = out.get_context();
    auto &TT = out.TargetTriple;
    auto Asm = select_plt_asm(TT, LargeCodeModel);

    PointerType *PtrTy = PointerType::getUnqual(Ctx);
    FunctionType *VoidFnTy = FunctionType::get(Type::getVoidTy(Ctx), false);

    auto ResolverFn = cast<Function>(M.getOrInsertFunction("jl_plt_resolver",
                                                           VoidFnTy).getCallee());
    auto ResolveFn = cast<Function>(M.getOrInsertFunction(Resolve,
        FunctionType::get(PtrTy, {PtrTy}, false)).getCallee());
    Value *Resolver = ResolverFn;
    if (TT.getArch() == llvm::Triple::x86 || TT.getArch() == llvm::Triple::ppc64le)
        // a local pointer to the resolver (see the trampolines)
        Resolver = M.getOrInsertGlobal("$plt$resolver", PtrTy, [&] {
            return new GlobalVariable(M, PtrTy, /*isConstant=*/true,
                                      GlobalVariable::PrivateLinkage, ResolverFn,
                                      "$plt$resolver");
        });

    std::string base = out.make_name("$plt$");

    auto Slot = new GlobalVariable(M, PtrTy, /*isConstant=*/false,
                                   GlobalVariable::PrivateLinkage,
                                   ConstantPointerNull::get(PtrTy), base + "$ptr");

    SmallVector<Constant *, 5> EntryFields{ResolveFn, Slot};
    EntryFields.append(Data.begin(), Data.end());
    auto EntryInit = ConstantStruct::getAnon(Ctx, EntryFields);
    auto EntryGV = new GlobalVariable(M, EntryInit->getType(), /*isConstant=*/true,
                                      GlobalVariable::PrivateLinkage,
                                      EntryInit, base + "$entry");

    // The trampoline passes &entry to jl_plt_resolver
    auto TrampFn = Function::Create(VoidFnTy, GlobalValue::PrivateLinkage,
                                    base + "$tramp", &M);
    SmallVector<Value *, 3> TrampOps{EntryGV, Resolver};
    if (Asm.PCThunk)
        TrampOps.push_back(emit_pc_thunk_i686(M));
    define_asm_function(TrampFn, Asm.Trampoline, TrampOps);
    TrampFn->setAlignment(Asm.Alignment);
    Slot->setInitializer(TrampFn);

    // The call sites use their own signatures, not that of `StubFn` (`void()`)
    define_slot_jump_thunk(out, StubFn, Slot, Asm);
}
