// This file is a part of Julia. License is MIT: https://julialang.org/license

// Thunks that jump through a slot, and the stubs of lazily bound calls, for the
// JIT and images, from the assembly for each architecture in
// src/arch/<arch>/plt_stubs.inc (see src/plt.h).

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

// On PPC64, the slot thunks jump to code with its own TOC (r2), so callers must
// save and restore theirs. LLVM only emits the restore after calls to functions
// that are not strong definitions, and the linker only saves the TOC for
// functions marked `.localentry <thunk>, 1` (see plt_asm_ppc64le).
void set_slot_thunk_linkage(Function *F, const Triple &TT) JL_NOTSAFEPOINT
{
    if (!TT.isPPC64()) {
        F->setLinkage(GlobalValue::PrivateLinkage);
        return;
    }
    F->setLinkage(GlobalValue::LinkOnceODRLinkage);
    F->setVisibility(GlobalValue::HiddenVisibility);
    F->addFnAttr("julia.slot_thunk");
}

// For a stub in a JIT module of its own, called from other modules
void set_jit_stub_linkage(Function *F)
{
    F->setLinkage(GlobalValue::ExternalLinkage);
    F->setVisibility(GlobalValue::DefaultVisibility);
}

bool isSlotThunk(const GlobalObject &G) JL_NOTSAFEPOINT
{
    auto F = dyn_cast<Function>(&G);
    return F && F->hasFnAttribute("julia.slot_thunk");
}

// Returns its return address in %eax, like __x86.get_pc_thunk.ax.
static Function *emit_pc_thunk_i686(Module &M) JL_NOTSAFEPOINT
{
    if (Function *F = M.getFunction("$jl$pc_thunk"))
        return F;
    auto &Ctx = M.getContext();
    FunctionType *VoidFnTy = FunctionType::get(Type::getVoidTy(Ctx), false);
    auto F = Function::Create(VoidFnTy, GlobalValue::PrivateLinkage, "$jl$pc_thunk", &M);
    F->addFnAttr(Attribute::Naked);
    F->addFnAttr(Attribute::NoUnwind);
    F->addFnAttr("frame-pointer", "none");
    IRBuilder<> B(BasicBlock::Create(Ctx, "", F));
    auto IA = InlineAsm::get(VoidFnTy, "movl (%esp), %eax\nretl\n", "",
                             /*hasSideEffects=*/true, /*isAlignStack=*/false);
    B.CreateCall(IA)->addFnAttr(Attribute::NoReturn);
    B.CreateUnreachable();
    return F;
}

// Define `F` as a thunk that jumps through `Slot`, leaving the arguments alone.
// `LargeCodeModel` selects code that can reach any address.
static void define_slot_jump_thunk(jl_codegen_output_t &out, Function *F, GlobalVariable *Slot,
                                   bool LargeCodeModel) JL_NOTSAFEPOINT
{
    auto &M = out.get_module();
    auto &Ctx = out.get_context();
    Type *PtrTy = PointerType::getUnqual(Ctx);

    const char *Code = nullptr;
    auto &TT = out.TargetTriple;
    if (TT.getArch() == Triple::x86_64)
        Code = LargeCodeModel ? plt_asm_x86_64_large : plt_asm_x86_64;
    else if (TT.getArch() == Triple::x86)
        // (the triple's object format is ELF during codegen on Windows, see
        // createTargetMachine, even for the COFF images)
        Code = TT.isOSWindows() ? plt_asm_i686_coff : plt_asm_i686;
    else if (TT.isAArch64())
        Code = LargeCodeModel ? plt_asm_aarch64_large :
               TT.isOSBinFormatMachO() ? plt_asm_aarch64_macho : plt_asm_aarch64;
    else if (TT.getArch() == Triple::riscv64)
        Code = LargeCodeModel ? plt_asm_riscv64_large : plt_asm_riscv64;
    else if (TT.getArch() == Triple::riscv32 && !LargeCodeModel)
        Code = plt_asm_riscv32;
    else if (TT.getArch() == Triple::arm)
        Code = plt_asm_arm;
    else if (TT.getArch() == Triple::ppc64le)
        Code = plt_asm_ppc64le;
    else
        report_fatal_error("no slot jump thunk for target " + Twine(TT.str()) +
                           (LargeCodeModel ? " (large code model)" : ""));

    AttrBuilder Attrs{Ctx};
    Attrs.addAttribute("frame-pointer", "none");
    Attrs.addAttribute("thunk");
    Attrs.addAttribute(Attribute::Naked);
    Attrs.addAttribute(Attribute::NoInline);
    set_slot_thunk_linkage(F, TT);
    SmallVector<Value*, 2> Ops{Slot};
    if (Code == plt_asm_i686)
        Ops.push_back(emit_pc_thunk_i686(M));
    else if (TT.isPPC64())
        Ops.push_back(F);
    SmallVector<Type*, 2> OpTys(Ops.size(), PtrTy);
    IRBuilder<> B(BasicBlock::Create(Ctx, "", F));
    auto IA = InlineAsm::get(FunctionType::get(Type::getVoidTy(Ctx), OpTys, false), Code,
                             Ops.size() == 2 ? "s,s" : "s", true, false);
    B.CreateCall(IA, Ops)->addFnAttr(Attribute::NoReturn);
    B.CreateUnreachable();
    F->addFnAttrs(Attrs);
    if (Code == plt_asm_riscv64_large)
        F->setAlignment(Align(8));
}

// Emit a thunk named `Name` that jumps through `Slot` (see define_slot_jump_thunk).
Function *emit_slot_jump_thunk(jl_codegen_output_t &out, GlobalVariable *Slot,
                               const Twine &Name, CallingConv::ID CC,
                               bool LargeCodeModel)
{
    auto F = Function::Create(FunctionType::get(Type::getVoidTy(out.get_context()), false),
                              Function::PrivateLinkage, 0, Name, &out.get_module());
    F->setCallingConv(CC);
    define_slot_jump_thunk(out, F, Slot, LargeCodeModel);
    return F;
}

// Define `StubFn` as a lazily bound stub (see src/plt.h), with its slot,
// trampoline and entry. The entry is {`Resolve`, slot, `Data`...}.
void emit_plt_stub(jl_codegen_output_t &out, Function *StubFn, StringRef Resolve,
                   ArrayRef<Constant *> Data, bool LargeCodeModel) JL_NOTSAFEPOINT
{
    auto &M = out.get_module();
    auto &Ctx = out.get_context();
    auto &TT = out.TargetTriple;

    const char *TrampAsm = nullptr;
    if (TT.getArch() == llvm::Triple::x86_64)
        TrampAsm = LargeCodeModel ? plt_tramp_asm_x86_64_large : plt_tramp_asm_x86_64;
    else if (TT.getArch() == llvm::Triple::x86)
        TrampAsm = TT.isOSWindows() ? plt_tramp_asm_i686_coff : plt_tramp_asm_i686;
    else if (TT.isAArch64())
        TrampAsm = LargeCodeModel ? plt_tramp_asm_aarch64_large :
                   TT.isOSBinFormatMachO() ? plt_tramp_asm_aarch64_macho : plt_tramp_asm_aarch64;
    else if (TT.isRISCV64())
        TrampAsm = LargeCodeModel ? plt_tramp_asm_riscv64_large : plt_tramp_asm_riscv64;
    else if (TT.getArch() == llvm::Triple::arm)
        TrampAsm = plt_tramp_asm_arm;
    else if (TT.getArch() == llvm::Triple::ppc64le)
        TrampAsm = plt_tramp_asm_ppc64le;
    else
        report_fatal_error("no PLT trampoline for target " + Twine(TT.str()) +
                           (LargeCodeModel ? " (large code model)" : ""));

    PointerType *PtrTy = PointerType::getUnqual(Ctx);
    Type *VoidTy = Type::getVoidTy(Ctx);
    FunctionType *VoidFnTy = FunctionType::get(VoidTy, false);

    auto ResolverFn = cast<Function>(M.getOrInsertFunction("jl_plt_resolver",
                                                           VoidFnTy).getCallee());
    auto ResolveFn = cast<Function>(M.getOrInsertFunction(Resolve,
        FunctionType::get(PtrTy, {PtrTy}, false)).getCallee());
    Value *Resolver = ResolverFn;
    Function *PCThunk = nullptr;
    if (TrampAsm == plt_tramp_asm_i686)
        PCThunk = emit_pc_thunk_i686(M);
    if (TT.getArch() == llvm::Triple::x86 || TT.getArch() == llvm::Triple::ppc64le) {
        // a local pointer to the resolver (see the trampolines)
        GlobalVariable *ResolverPtr = M.getNamedGlobal("$plt$resolver");
        if (!ResolverPtr)
            ResolverPtr = new GlobalVariable(M, PtrTy, /*isConstant=*/true,
                                             GlobalVariable::PrivateLinkage,
                                             ResolverFn, "$plt$resolver");
        Resolver = ResolverPtr;
    }

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
    TrampFn->addFnAttr(Attribute::Naked);
    TrampFn->addFnAttr(Attribute::NoUnwind);
    TrampFn->addFnAttr("frame-pointer", "none");
    if (TrampAsm == plt_tramp_asm_riscv64_large)
        TrampFn->setAlignment(Align(8));
    {
        IRBuilder<> B(BasicBlock::Create(Ctx, "", TrampFn));
        SmallVector<Value*, 3> Ops{EntryGV, Resolver};
        if (PCThunk)
            Ops.push_back(PCThunk);
        SmallVector<Type*, 3> OpTys(Ops.size(), PtrTy);
        auto IA = InlineAsm::get(FunctionType::get(VoidTy, OpTys, false), TrampAsm,
                                 PCThunk ? "s,s,s" : "s,s",
                                 /*hasSideEffects=*/true, /*isAlignStack=*/false);
        B.CreateCall(IA, Ops)->addFnAttr(Attribute::NoReturn);
        B.CreateUnreachable();
    }
    Slot->setInitializer(TrampFn);

    // The call sites use their own signatures, not that of `StubFn` (`void()`)
    define_slot_jump_thunk(out, StubFn, Slot, LargeCodeModel);
    StubFn->addFnAttr(Attribute::NoUnwind);
}
