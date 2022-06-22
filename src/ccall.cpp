// This file is a part of Julia. License is MIT: https://julialang.org/license

// --- the ccall, cglobal, and llvm intrinsics ---

// Mark our stats as being from ccall
#undef DEBUG_TYPE
#define DEBUG_TYPE "julia_irgen_ccall"

STATISTIC(RuntimeSymLookups, "Number of runtime symbol lookups emitted");
STATISTIC(PLTThunks, "Number of PLT Thunks emitted");
STATISTIC(PLT, "Number of direct PLT entries emitted");
STATISTIC(EmittedCGlobals, "Number of C globals emitted");
STATISTIC(EmittedLLVMCalls, "Number of llvmcall intrinsics emitted");

#define _CCALL_STAT(name) jl_transformed_ccall__##name
#define CCALL_STAT(name) _CCALL_STAT(name)
#define TRANSFORMED_CCALL_STAT(name) STATISTIC(_CCALL_STAT(name), "Number of " #name " ccalls intercepted")
TRANSFORMED_CCALL_STAT(jl_array_ptr);
TRANSFORMED_CCALL_STAT(jl_value_ptr);
TRANSFORMED_CCALL_STAT(jl_cpu_pause);
TRANSFORMED_CCALL_STAT(jl_cpu_wake);
TRANSFORMED_CCALL_STAT(jl_gc_safepoint);
TRANSFORMED_CCALL_STAT(jl_get_ptls_states);
TRANSFORMED_CCALL_STAT(jl_threadid);
TRANSFORMED_CCALL_STAT(jl_gc_enable_disable_finalizers_internal);
TRANSFORMED_CCALL_STAT(jl_get_current_task);
TRANSFORMED_CCALL_STAT(jl_set_next_task);
TRANSFORMED_CCALL_STAT(jl_sigatomic_begin);
TRANSFORMED_CCALL_STAT(jl_sigatomic_end);
TRANSFORMED_CCALL_STAT(jl_svec_len);
TRANSFORMED_CCALL_STAT(jl_svec_isassigned);
TRANSFORMED_CCALL_STAT(jl_svec_ref);
TRANSFORMED_CCALL_STAT(jl_array_isassigned);
TRANSFORMED_CCALL_STAT(jl_string_ptr);
TRANSFORMED_CCALL_STAT(jl_symbol_name);
TRANSFORMED_CCALL_STAT(memcpy);
TRANSFORMED_CCALL_STAT(memset);
TRANSFORMED_CCALL_STAT(memmove);
TRANSFORMED_CCALL_STAT(jl_object_id);
#undef TRANSFORMED_CCALL_STAT

STATISTIC(EmittedCCalls, "Number of ccalls emitted");
STATISTIC(DeferredCCallLookups, "Number of ccalls looked up at runtime");
STATISTIC(LiteralCCalls, "Number of ccalls directly emitted through a pointer");
STATISTIC(RetBoxedCCalls, "Number of ccalls that were retboxed");
STATISTIC(SRetCCalls, "Number of ccalls that were marked sret");

// somewhat unusual variable, in that aotcompile wants to get the address of this for a sanity check
GlobalVariable *jl_emit_RTLD_DEFAULT_var(Module *M)
{
    return prepare_global_in(M, jlRTLD_DEFAULT_var);
}


// Find or create the GVs for the library and symbol lookup.
// Return `runtime_lib` (whether the library name is a string)
// The `lib` and `sym` GV returned may not be in the current module.
static bool runtime_sym_gvs(jl_codectx_t &ctx, const char *f_lib, const char *f_name,
                            GlobalVariable *&lib, GlobalVariable *&sym)
{
    auto &TSM = ctx.emission_context.shared_module(*jl_Module);
    //Safe b/c emission context holds context lock
    auto M = TSM.getModuleUnlocked();
    bool runtime_lib = false;
    GlobalVariable *libptrgv;
    jl_codegen_params_t::SymMapGV *symMap;
#ifdef _OS_WINDOWS_
    if ((intptr_t)f_lib == (intptr_t)JL_EXE_LIBNAME) {
        libptrgv = prepare_global_in(M, jlexe_var);
        symMap = &ctx.emission_context.symMapExe;
    }
    else if ((intptr_t)f_lib == (intptr_t)JL_LIBJULIA_INTERNAL_DL_LIBNAME) {
        libptrgv = prepare_global_in(M, jldlli_var);
        symMap = &ctx.emission_context.symMapDlli;
    }
    else if ((intptr_t)f_lib == (intptr_t)JL_LIBJULIA_DL_LIBNAME) {
        libptrgv = prepare_global_in(M, jldll_var);
        symMap = &ctx.emission_context.symMapDll;
    }
    else
#endif
    if (f_lib == NULL) {
        libptrgv = jl_emit_RTLD_DEFAULT_var(M);
        symMap = &ctx.emission_context.symMapDefault;
    }
    else {
        std::string name = "ccalllib_";
        name += llvm::sys::path::filename(f_lib);
        name += std::to_string(globalUniqueGeneratedNames++);
        runtime_lib = true;
        auto &libgv = ctx.emission_context.libMapGV[f_lib];
        if (libgv.first == NULL) {
            libptrgv = new GlobalVariable(*M, getInt8PtrTy(M->getContext()), false,
                                          GlobalVariable::ExternalLinkage,
                                          Constant::getNullValue(getInt8PtrTy(M->getContext())), name);
            libgv.first = libptrgv;
        }
        else {
            libptrgv = libgv.first;
        }
        symMap = &libgv.second;
    }

    GlobalVariable *&llvmgv = (*symMap)[f_name];
    if (llvmgv == NULL) {
        std::string name = "ccall_";
        name += f_name;
        name += "_";
        name += std::to_string(globalUniqueGeneratedNames++);
        auto T_pvoidfunc = JuliaType::get_pvoidfunc_ty(M->getContext());
        llvmgv = new GlobalVariable(*M, T_pvoidfunc, false,
                                    GlobalVariable::ExternalLinkage,
                                    Constant::getNullValue(T_pvoidfunc), name);
    }

    lib = libptrgv;
    sym = llvmgv;
    return runtime_lib;
}

static Value *runtime_sym_lookup(
        jl_codegen_params_t &emission_context,
        IRBuilder<> &irbuilder,
        jl_codectx_t *ctx,
        PointerType *funcptype, const char *f_lib, jl_value_t *lib_expr,
        const char *f_name, Function *f,
        GlobalVariable *libptrgv,
        GlobalVariable *llvmgv, bool runtime_lib)
{
    ++RuntimeSymLookups;
    // in pseudo-code, this function emits the following:
    //   global HMODULE *libptrgv
    //   global void **llvmgv
    //   if (*llvmgv == NULL) {
    //       *llvmgv = jl_load_and_lookup(f_lib, f_name, libptrgv);
    //   }
    //   return (*llvmgv)
    auto T_pvoidfunc = JuliaType::get_pvoidfunc_ty(irbuilder.getContext());
    BasicBlock *enter_bb = irbuilder.GetInsertBlock();
    BasicBlock *dlsym_lookup = BasicBlock::Create(irbuilder.getContext(), "dlsym");
    BasicBlock *ccall_bb = BasicBlock::Create(irbuilder.getContext(), "ccall");
    Constant *initnul = ConstantPointerNull::get(T_pvoidfunc);
    LoadInst *llvmf_orig = irbuilder.CreateAlignedLoad(T_pvoidfunc, llvmgv, Align(sizeof(void*)));
    // This in principle needs a consume ordering so that load from
    // this pointer sees a valid value. However, this is not supported by
    // LLVM (or agreed on in the C/C++ standard FWIW) and should be
    // almost impossible to happen on every platform we support since this
    // ordering is enforced by the hardware and LLVM has to speculate an
    // invalid load from the `cglobal` but doesn't depend on the `cglobal`
    // value for this to happen.
    llvmf_orig->setAtomic(AtomicOrdering::Unordered);
    irbuilder.CreateCondBr(
            irbuilder.CreateICmpNE(llvmf_orig, initnul),
            ccall_bb,
            dlsym_lookup);

    assert(f->getParent() != NULL);
    f->getBasicBlockList().push_back(dlsym_lookup);
    irbuilder.SetInsertPoint(dlsym_lookup);
    Instruction *llvmf;
    Value *nameval = stringConstPtr(emission_context, irbuilder, f_name);
    if (lib_expr) {
        jl_cgval_t libval = emit_expr(*ctx, lib_expr);
        llvmf = irbuilder.CreateCall(prepare_call_in(jl_builderModule(irbuilder), jllazydlsym_func),
                    { boxed(*ctx, libval), nameval });
    }
    else {
        Value *libname;
        if (runtime_lib) {
            libname = stringConstPtr(emission_context, irbuilder, f_lib);
        }
        else {
            // f_lib is actually one of the special sentinel values
            libname = ConstantExpr::getIntToPtr(ConstantInt::get(getSizeTy(irbuilder.getContext()), (uintptr_t)f_lib), getInt8PtrTy(irbuilder.getContext()));
        }
        llvmf = irbuilder.CreateCall(prepare_call_in(jl_builderModule(irbuilder), jldlsym_func),
                    { libname, nameval, libptrgv });
    }
    StoreInst *store = irbuilder.CreateAlignedStore(llvmf, llvmgv, Align(sizeof(void*)));
    store->setAtomic(AtomicOrdering::Release);
    irbuilder.CreateBr(ccall_bb);

    f->getBasicBlockList().push_back(ccall_bb);
    irbuilder.SetInsertPoint(ccall_bb);
    PHINode *p = irbuilder.CreatePHI(T_pvoidfunc, 2);
    p->addIncoming(llvmf_orig, enter_bb);
    p->addIncoming(llvmf, llvmf->getParent());
    return irbuilder.CreateBitCast(p, funcptype);
}

static Value *runtime_sym_lookup(
        jl_codectx_t &ctx,
        PointerType *funcptype, const char *f_lib, jl_value_t *lib_expr,
        const char *f_name, Function *f,
        GlobalVariable *libptrgv,
        GlobalVariable *llvmgv, bool runtime_lib)
{
    return runtime_sym_lookup(ctx.emission_context, ctx.builder, &ctx, funcptype, f_lib, lib_expr,
                              f_name, f, libptrgv, llvmgv, runtime_lib);
}

static Value *runtime_sym_lookup(
        jl_codectx_t &ctx,
        PointerType *funcptype, const char *f_lib, jl_value_t *lib_expr,
        const char *f_name, Function *f)
{
    auto T_pvoidfunc = JuliaType::get_pvoidfunc_ty(ctx.builder.getContext());
    GlobalVariable *libptrgv;
    GlobalVariable *llvmgv;
    bool runtime_lib;
    if (lib_expr) {
        // for computed library names, generate a global variable to cache the function
        // pointer just for this call site.
        runtime_lib = true;
        libptrgv = NULL;
        std::string gvname = "libname_";
        gvname += f_name;
        gvname += "_";
        gvname += std::to_string(globalUniqueGeneratedNames++);
        llvmgv = new GlobalVariable(*jl_Module, T_pvoidfunc, false,
                                    GlobalVariable::ExternalLinkage,
                                    Constant::getNullValue(T_pvoidfunc), gvname);
    }
    else {
        runtime_lib = runtime_sym_gvs(ctx, f_lib, f_name, libptrgv, llvmgv);
        libptrgv = prepare_global_in(jl_Module, libptrgv);
    }
    llvmgv = prepare_global_in(jl_Module, llvmgv);
    return runtime_sym_lookup(ctx, funcptype, f_lib, lib_expr, f_name, f, libptrgv, llvmgv, runtime_lib);
}

// Emit a "PLT" entry that will be lazily initialized
// when being called the first time.
static GlobalVariable *emit_plt_thunk(
        jl_codectx_t &ctx,
        FunctionType *functype, const AttributeList &attrs,
        CallingConv::ID cc, const char *f_lib, const char *f_name,
        GlobalVariable *libptrgv, GlobalVariable *llvmgv,
        bool runtime_lib)
{
    ++PLTThunks;
    auto &TSM = ctx.emission_context.shared_module(*jl_Module);
    Module *M = TSM.getModuleUnlocked();
    PointerType *funcptype = PointerType::get(functype, 0);
    libptrgv = prepare_global_in(M, libptrgv);
    llvmgv = prepare_global_in(M, llvmgv);
    std::string fname;
    raw_string_ostream(fname) << "jlplt_" << f_name << "_" << globalUniqueGeneratedNames++;
    Function *plt = Function::Create(functype,
                                     GlobalVariable::ExternalLinkage,
                                     fname, M);
    plt->setAttributes(attrs);
    if (cc != CallingConv::C)
        plt->setCallingConv(cc);
    fname += "_got";
    auto T_pvoidfunc = JuliaType::get_pvoidfunc_ty(M->getContext());
    GlobalVariable *got = new GlobalVariable(*M, T_pvoidfunc, false,
                                             GlobalVariable::ExternalLinkage,
                                             ConstantExpr::getBitCast(plt, T_pvoidfunc),
                                             fname);
    BasicBlock *b0 = BasicBlock::Create(M->getContext(), "top", plt);
    IRBuilder<> irbuilder(b0);
    Value *ptr = runtime_sym_lookup(ctx.emission_context, irbuilder, NULL, funcptype, f_lib, NULL, f_name, plt, libptrgv,
                                    llvmgv, runtime_lib);
    StoreInst *store = irbuilder.CreateAlignedStore(irbuilder.CreateBitCast(ptr, T_pvoidfunc), got, Align(sizeof(void*)));
    store->setAtomic(AtomicOrdering::Release);
    SmallVector<Value*, 16> args;
    for (Function::arg_iterator arg = plt->arg_begin(), arg_e = plt->arg_end(); arg != arg_e; ++arg)
        args.push_back(&*arg);
    assert(cast<PointerType>(ptr->getType())->isOpaqueOrPointeeTypeMatches(functype));
    CallInst *ret = irbuilder.CreateCall(
        functype,
        ptr, ArrayRef<Value*>(args));
    ret->setAttributes(attrs);
    if (cc != CallingConv::C)
        ret->setCallingConv(cc);
    // NoReturn function can trigger LLVM verifier error when declared as
    // MustTail since other passes might replace the `ret` with
    // `unreachable` (LLVM should probably accept `unreachable`).
    if (hasFnAttr(attrs, Attribute::NoReturn)) {
        irbuilder.CreateUnreachable();
    }
    else {
        // musttail support is very bad on ARM, PPC, PPC64 (as of LLVM 3.9)
        // Known failures includes vararg (not needed here) and sret.

#if (defined(_CPU_X86_) || defined(_CPU_X86_64_) || (defined(_CPU_AARCH64_) && !defined(_OS_DARWIN_)))
        // Ref https://bugs.llvm.org/show_bug.cgi?id=47058
        // LLVM, as of 10.0.1 emits wrong/worse code when musttail is set
        // Apple silicon macs give an LLVM ERROR if musttail is set here #44107.
        if (!attrs.hasAttrSomewhere(Attribute::ByVal))
            ret->setTailCallKind(CallInst::TCK_MustTail);
#endif
        if (functype->getReturnType() == getVoidTy(irbuilder.getContext())) {
            irbuilder.CreateRetVoid();
        }
        else {
            irbuilder.CreateRet(ret);
        }
    }
    irbuilder.ClearInsertionPoint();

    return got;
}

static Value *emit_plt(
        jl_codectx_t &ctx,
        FunctionType *functype,
        const AttributeList &attrs,
        CallingConv::ID cc, const char *f_lib, const char *f_name)
{
    ++PLT;
    assert(ctx.emission_context.imaging);
    // Don't do this for vararg functions so that the `musttail` is only
    // an optimization and is not required to function correctly.
    assert(!functype->isVarArg());
    GlobalVariable *libptrgv;
    GlobalVariable *llvmgv;
    bool runtime_lib = runtime_sym_gvs(ctx, f_lib, f_name, libptrgv, llvmgv);
    PointerType *funcptype = PointerType::get(functype, 0);

    auto &pltMap = ctx.emission_context.allPltMap[attrs];
    auto key = std::make_tuple(llvmgv, functype, cc);
    GlobalVariable *&sharedgot = pltMap[key];
    if (!sharedgot) {
        sharedgot = emit_plt_thunk(ctx,
                functype, attrs, cc, f_lib, f_name, libptrgv, llvmgv, runtime_lib);
    }
    GlobalVariable *got = prepare_global_in(jl_Module, sharedgot);
    LoadInst *got_val = ctx.builder.CreateAlignedLoad(got->getValueType(), got, Align(sizeof(void*)));
    // See comment in `runtime_sym_lookup` above. This in principle needs a
    // consume ordering too. This is even less likely to cause issues though
    // since the only thing we do to this loaded pointer is to call it
    // immediately.
    got_val->setAtomic(AtomicOrdering::Unordered);
    return ctx.builder.CreateBitCast(got_val, funcptype);
}

// --- ABI Implementations ---
// Partially based on the LDC ABI implementations licensed under the BSD 3-clause license

class AbiLayout {
public:
    virtual ~AbiLayout() {}
    virtual bool use_sret(jl_datatype_t *ty, LLVMContext &ctx) = 0;
    virtual bool needPassByRef(jl_datatype_t *ty, AttrBuilder&, LLVMContext &ctx, Type* llvm_t) = 0;
    virtual Type *preferred_llvm_type(jl_datatype_t *ty, bool isret, LLVMContext &ctx) const = 0;
};

// Determine if object of bitstype ty maps to a native x86 SIMD type (__m128, __m256, or __m512) in C
static bool is_native_simd_type(jl_datatype_t *dt) {
    size_t size = jl_datatype_size(dt);
    if (size != 16 && size != 32 && size != 64)
        // Wrong size for xmm, ymm, or zmm register.
        return false;
    uint32_t n = jl_datatype_nfields(dt);
    if (n<2)
        // Not mapped to SIMD register.
        return false;
    jl_value_t *ft0 = jl_field_type(dt, 0);
    for (uint32_t i = 1; i < n; ++i)
        if (jl_field_type(dt, i) != ft0)
            // Not homogeneous
            return false;
    // Type is homogeneous.  Check if it maps to LLVM vector.
    return jl_special_vector_alignment(n, ft0) != 0;
}

#include "abi_llvm.cpp"

#include "abi_arm.cpp"
#include "abi_aarch64.cpp"
#include "abi_ppc64le.cpp"
#include "abi_win32.cpp"
#include "abi_win64.cpp"
#include "abi_x86_64.cpp"
#include "abi_x86.cpp"

#if defined ABI_LLVM
  typedef ABI_LLVMLayout DefaultAbiState;
#elif defined _CPU_X86_64_
#  if defined _OS_WINDOWS_
     typedef ABI_Win64Layout DefaultAbiState;
#  else
     typedef ABI_x86_64Layout DefaultAbiState;
#  endif
#elif defined _CPU_X86_
#  if defined _OS_WINDOWS_
     typedef ABI_Win32Layout DefaultAbiState;
#  else
     typedef ABI_x86Layout DefaultAbiState;
#  endif
#elif defined _CPU_ARM_
  typedef ABI_ARMLayout DefaultAbiState;
#elif defined _CPU_AARCH64_
  typedef ABI_AArch64Layout DefaultAbiState;
#elif defined _CPU_PPC64_
  typedef ABI_PPC64leLayout DefaultAbiState;
#else
#  pragma message("ccall is defaulting to llvm ABI, since no platform ABI has been defined for this CPU/OS combination")
  typedef ABI_LLVMLayout DefaultAbiState;
#endif

// basic type widening and cast conversions
static Value *llvm_type_rewrite(
        jl_codectx_t &ctx,
        Value *v, Type *target_type,
        bool issigned) /* determines whether an integer value should be zero or sign extended */
{
    Type *from_type = v->getType();
    if (target_type == from_type)
        return v;

    if (from_type == getVoidTy(ctx.builder.getContext()) || isa<UndefValue>(v))
        return UndefValue::get(target_type); // convert undef (unreachable) -> undef (target_type)

    assert(from_type->isPointerTy() == target_type->isPointerTy()); // expect that all ABIs consider all pointers to be equivalent
    if (target_type->isPointerTy())
        return emit_bitcast(ctx, v, target_type);

    // simple integer and float widening & conversion cases
    if (from_type->getPrimitiveSizeInBits() > 0 &&
            target_type->getPrimitiveSizeInBits() == from_type->getPrimitiveSizeInBits())
        return emit_bitcast(ctx, v, target_type);

    if (target_type->isFloatingPointTy() && from_type->isFloatingPointTy()) {
        if (target_type->getPrimitiveSizeInBits() > from_type->getPrimitiveSizeInBits())
            return ctx.builder.CreateFPExt(v, target_type);
        else if (target_type->getPrimitiveSizeInBits() < from_type->getPrimitiveSizeInBits())
            return ctx.builder.CreateFPTrunc(v, target_type);
        else
            return v;
    }

    if (target_type->isIntegerTy() && from_type->isIntegerTy()) {
        if (issigned)
            return ctx.builder.CreateSExtOrTrunc(v, target_type);
        else
            return ctx.builder.CreateZExtOrTrunc(v, target_type);
    }

    // one or both of from_type and target_type is a VectorType or AggregateType
    // LLVM doesn't allow us to cast these values directly, so
    // we need to use this alloca copy trick instead
    // On ARM and AArch64, the ABI requires casting through memory to different
    // sizes.
    Value *from;
    Value *to;
    const DataLayout &DL = ctx.builder.GetInsertBlock()->getModule()->getDataLayout();
    unsigned align = std::max(DL.getPrefTypeAlignment(target_type), DL.getPrefTypeAlignment(from_type));
    if (DL.getTypeAllocSize(target_type) >= DL.getTypeAllocSize(from_type)) {
        to = emit_static_alloca(ctx, target_type);
        cast<AllocaInst>(to)->setAlignment(Align(align));
        from = emit_bitcast(ctx, to, from_type->getPointerTo());
    }
    else {
        from = emit_static_alloca(ctx, from_type);
        cast<AllocaInst>(from)->setAlignment(Align(align));
        to = emit_bitcast(ctx, from, target_type->getPointerTo());
    }
    ctx.builder.CreateAlignedStore(v, from, Align(align));
    return ctx.builder.CreateAlignedLoad(target_type, to, Align(align));
}

// --- argument passing and scratch space utilities ---

// Returns ctx.types().T_prjlvalue
static Value *runtime_apply_type_env(jl_codectx_t &ctx, jl_value_t *ty)
{
    // box if concrete type was not statically known
    Value *args[] = {
        literal_pointer_val(ctx, ty),
        literal_pointer_val(ctx, (jl_value_t*)ctx.linfo->def.method->sig),
        ctx.builder.CreateInBoundsGEP(
                ctx.types().T_prjlvalue,
                ctx.spvals_ptr,
                ConstantInt::get(getSizeTy(ctx.builder.getContext()), sizeof(jl_svec_t) / sizeof(jl_value_t*)))
    };
    auto call = ctx.builder.CreateCall(prepare_call(jlapplytype_func), makeArrayRef(args));
    addRetAttr(call, Attribute::getWithAlignment(ctx.builder.getContext(), Align(16)));
    return call;
}

static const std::string make_errmsg(const char *fname, int n, const char *err)
{
    std::string _msg;
    raw_string_ostream msg(_msg);
    msg << fname;
    if (n > 0)
        msg << " argument " << n;
    else
        msg << " return";
    msg << err;
    return msg.str();
}

static void typeassert_input(jl_codectx_t &ctx, const jl_cgval_t &jvinfo, jl_value_t *jlto, jl_unionall_t *jlto_env, int argn)
{
    if (jlto != (jl_value_t*)jl_any_type && !jl_subtype(jvinfo.typ, jlto)) {
        if (jlto == (jl_value_t*)jl_voidpointer_type) {
            // allow a bit more flexibility for what can be passed to (void*) due to Ref{T} conversion behavior in input
            if (!jl_is_cpointer_type(jvinfo.typ)) {
                // emit a typecheck, if not statically known to be correct
                emit_cpointercheck(ctx, jvinfo, make_errmsg("ccall", argn + 1, ""));
            }
        }
        else {
            // emit a typecheck, if not statically known to be correct
            std::string msg = make_errmsg("ccall", argn + 1, "");
            if (!jlto_env || !jl_has_typevar_from_unionall(jlto, jlto_env)) {
                emit_typecheck(ctx, jvinfo, jlto, msg);
            }
            else {
                jl_cgval_t jlto_runtime = mark_julia_type(ctx, runtime_apply_type_env(ctx, jlto), true, jl_any_type);
                Value *vx = boxed(ctx, jvinfo);
                Value *istype = ctx.builder.CreateICmpNE(
                        ctx.builder.CreateCall(prepare_call(jlisa_func), { vx, boxed(ctx, jlto_runtime) }),
                        ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0));
                BasicBlock *failBB = BasicBlock::Create(ctx.builder.getContext(), "fail", ctx.f);
                BasicBlock *passBB = BasicBlock::Create(ctx.builder.getContext(), "pass", ctx.f);
                ctx.builder.CreateCondBr(istype, passBB, failBB);

                ctx.builder.SetInsertPoint(failBB);
                emit_type_error(ctx, mark_julia_type(ctx, vx, true, jl_any_type), boxed(ctx, jlto_runtime), msg);
                ctx.builder.CreateUnreachable();
                ctx.builder.SetInsertPoint(passBB);
            }
        }
    }
}

// Emit code to convert argument to form expected by C ABI
// to = desired LLVM type
// jlto = Julia type of formal argument
// jvinfo = value of actual argument
static Value *julia_to_native(
        jl_codectx_t &ctx,
        Type *to, bool toboxed, jl_value_t *jlto, jl_unionall_t *jlto_env,
        const jl_cgval_t &jvinfo,
        bool byRef, int argn)
{
    // We're passing Any
    if (toboxed) {
        assert(!byRef); // don't expect any ABI to pass pointers by pointer
        return boxed(ctx, jvinfo);
    }
    assert(jl_is_datatype(jlto) && jl_struct_try_layout((jl_datatype_t*)jlto));

    typeassert_input(ctx, jvinfo, jlto, jlto_env, argn);
    if (!byRef)
        return emit_unbox(ctx, to, jvinfo, jlto);

    // pass the address of an alloca'd thing, not a box
    // since those are immutable.
    Value *slot = emit_static_alloca(ctx, to);
    if (!jvinfo.ispointer()) {
        tbaa_decorate(jvinfo.tbaa, ctx.builder.CreateStore(emit_unbox(ctx, to, jvinfo, jlto), slot));
    }
    else {
        emit_memcpy(ctx, slot, jvinfo.tbaa, jvinfo, jl_datatype_size(jlto), julia_alignment(jlto));
    }
    return slot;
}

typedef struct {
    Value *jl_ptr;  // if the argument is a run-time computed pointer
    void (*fptr)(void);     // if the argument is a constant pointer
    const char *f_name;   // if the symbol name is known
    const char *f_lib;    // if a library name is specified
    jl_value_t *lib_expr; // expression to compute library path lazily
    jl_value_t *gcroot;
} native_sym_arg_t;

// --- parse :sym or (:sym, :lib) argument into address info ---
static void interpret_symbol_arg(jl_codectx_t &ctx, native_sym_arg_t &out, jl_value_t *arg, const char *fname, bool llvmcall)
{
    Value *&jl_ptr = out.jl_ptr;
    void (*&fptr)(void) = out.fptr;
    const char *&f_name = out.f_name;
    const char *&f_lib = out.f_lib;

    jl_value_t *ptr = static_eval(ctx, arg);
    if (ptr == NULL) {
        if (jl_is_expr(arg) && ((jl_expr_t*)arg)->head == jl_call_sym && jl_expr_nargs(arg) == 3 &&
            jl_is_globalref(jl_exprarg(arg,0)) && jl_globalref_mod(jl_exprarg(arg,0)) == jl_core_module &&
            jl_globalref_name(jl_exprarg(arg,0)) == jl_symbol("tuple")) {
            // attempt to interpret a non-constant 2-tuple expression as (func_name, lib_name()), where
            // `lib_name()` will be executed when first used.
            jl_value_t *name_val = static_eval(ctx, jl_exprarg(arg,1));
            if (name_val && jl_is_symbol(name_val)) {
                f_name = jl_symbol_name((jl_sym_t*)name_val);
                out.lib_expr = jl_exprarg(arg, 2);
                return;
            }
            else if (name_val && jl_is_string(name_val)) {
                f_name = jl_string_data(name_val);
                out.gcroot = name_val;
                out.lib_expr = jl_exprarg(arg, 2);
                return;
            }
        }
        jl_cgval_t arg1 = emit_expr(ctx, arg);
        jl_value_t *ptr_ty = arg1.typ;
        if (!jl_is_cpointer_type(ptr_ty)) {
            const char *errmsg = !strcmp(fname, "ccall") ?
                "ccall: first argument not a pointer or valid constant expression" :
                "cglobal: first argument not a pointer or valid constant expression";
            emit_cpointercheck(ctx, arg1, errmsg);
        }
        arg1 = update_julia_type(ctx, arg1, (jl_value_t*)jl_voidpointer_type);
        jl_ptr = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), arg1, (jl_value_t*)jl_voidpointer_type);
    }
    else {
        out.gcroot = ptr;
        if (jl_is_tuple(ptr) && jl_nfields(ptr) == 1) {
            ptr = jl_fieldref(ptr, 0);
        }

        if (jl_is_symbol(ptr))
            f_name = jl_symbol_name((jl_sym_t*)ptr);
        else if (jl_is_string(ptr))
            f_name = jl_string_data(ptr);

        if (f_name != NULL) {
            // just symbol, default to JuliaDLHandle
            // will look in process symbol table
            if (!llvmcall) {
                void *symaddr;
                std::string iname("i");
                iname += f_name;
                if (jl_dlsym(jl_libjulia_internal_handle, iname.c_str(), &symaddr, 0)) {
#ifdef _OS_WINDOWS_
                    f_lib = JL_LIBJULIA_INTERNAL_DL_LIBNAME;
#endif
                    f_name = jl_symbol_name(jl_symbol(iname.c_str()));
                }
#ifdef _OS_WINDOWS_
                else {
                    f_lib = jl_dlfind_win32(f_name);
                }
#endif
            }
        }
        else if (jl_is_cpointer_type(jl_typeof(ptr))) {
            fptr = *(void(**)(void))jl_data_ptr(ptr);
        }
        else if (jl_is_tuple(ptr) && jl_nfields(ptr) > 1) {
            jl_value_t *t0 = jl_fieldref(ptr, 0);
            if (jl_is_symbol(t0))
                f_name = jl_symbol_name((jl_sym_t*)t0);
            else if (jl_is_string(t0))
                f_name = jl_string_data(t0);
            else
                JL_TYPECHKS(fname, symbol, t0);

            jl_value_t *t1 = jl_fieldref(ptr, 1);
            if (jl_is_symbol(t1))
                f_lib = jl_symbol_name((jl_sym_t*)t1);
            else if (jl_is_string(t1))
                f_lib = jl_string_data(t1);
            else
                JL_TYPECHKS(fname, symbol, t1);
        }
        else {
            JL_TYPECHKS(fname, pointer, ptr);
        }
    }
}

// --- code generator for cglobal ---

static jl_cgval_t emit_runtime_call(jl_codectx_t &ctx, JL_I::intrinsic f, const jl_cgval_t *argv, size_t nargs);

static jl_cgval_t emit_cglobal(jl_codectx_t &ctx, jl_value_t **args, size_t nargs)
{
    ++EmittedCGlobals;
    JL_NARGS(cglobal, 1, 2);
    jl_value_t *rt = NULL;
    Value *res;
    native_sym_arg_t sym = {};
    JL_GC_PUSH2(&rt, &sym.gcroot);

    if (nargs == 2) {
        rt = static_eval(ctx, args[2]);
        if (rt == NULL) {
            JL_GC_POP();
            jl_cgval_t argv[2] = {jl_cgval_t(), jl_cgval_t()};
            argv[0] = emit_expr(ctx, args[1]);
            argv[1] = emit_expr(ctx, args[2]);
            return emit_runtime_call(ctx, JL_I::cglobal, argv, nargs);
        }

        JL_TYPECHK(cglobal, type, rt);
        rt = (jl_value_t*)jl_apply_type1((jl_value_t*)jl_pointer_type, rt);
    }
    else {
        rt = (jl_value_t*)jl_voidpointer_type;
    }
    Type *lrt = getSizeTy(ctx.builder.getContext());
    assert(lrt == julia_type_to_llvm(ctx, rt));

    interpret_symbol_arg(ctx, sym, args[1], "cglobal", false);

    if (sym.jl_ptr != NULL) {
        res = ctx.builder.CreateBitCast(sym.jl_ptr, lrt);
    }
    else if (sym.fptr != NULL) {
        res = ConstantInt::get(lrt, (uint64_t)sym.fptr);
        if (ctx.emission_context.imaging)
            jl_printf(JL_STDERR,"WARNING: literal address used in cglobal for %s; code cannot be statically compiled\n", sym.f_name);
    }
    else {
        if (sym.lib_expr) {
            res = runtime_sym_lookup(ctx, cast<PointerType>(getInt8PtrTy(ctx.builder.getContext())), NULL, sym.lib_expr, sym.f_name, ctx.f);
        }
        else if (ctx.emission_context.imaging) {
            res = runtime_sym_lookup(ctx, cast<PointerType>(getInt8PtrTy(ctx.builder.getContext())), sym.f_lib, NULL, sym.f_name, ctx.f);
            res = ctx.builder.CreatePtrToInt(res, lrt);
        }
        else {
            void *symaddr;

            void* libsym = jl_get_library_(sym.f_lib, 0);
            if (!libsym || !jl_dlsym(libsym, sym.f_name, &symaddr, 0)) {
                // Error mode, either the library or the symbol couldn't be find during compiletime.
                // Fallback to a runtime symbol lookup.
                res = runtime_sym_lookup(ctx, cast<PointerType>(getInt8PtrTy(ctx.builder.getContext())), sym.f_lib, NULL, sym.f_name, ctx.f);
                res = ctx.builder.CreatePtrToInt(res, lrt);
            } else {
                // since we aren't saving this code, there's no sense in
                // putting anything complicated here: just JIT the address of the cglobal
                res = ConstantInt::get(lrt, (uint64_t)symaddr);
            }
        }
    }

    JL_GC_POP();
    return mark_julia_type(ctx, res, false, rt);
}

// --- code generator for llvmcall ---

static jl_cgval_t emit_llvmcall(jl_codectx_t &ctx, jl_value_t **args, size_t nargs)
{
    ++EmittedLLVMCalls;
    // parse and validate arguments
    //
    // two forms of llvmcall are supported:
    // - llvmcall(ir, (rettypes...), (argtypes...), args...)
    //   where `ir` represents IR that should be pasted in a function body
    // - llvmcall((mod, fn), (rettypes...), (argtypes...), args...)
    //   where `mod` represents the assembly of an entire LLVM module,
    //   and `fn` the name of the function to call
    JL_NARGSV(llvmcall, 3);
    jl_value_t *rt = NULL, *at = NULL, *ir = NULL, *entry = NULL;
    jl_value_t *ir_arg = args[1];
    JL_GC_PUSH4(&ir, &rt, &at, &entry);
    if (jl_is_ssavalue(ir_arg))
        ir_arg = jl_arrayref((jl_array_t*)ctx.source->code, ((jl_ssavalue_t*)ir_arg)->id - 1);
    ir = static_eval(ctx, ir_arg);
    if (!ir) {
        emit_error(ctx, "error statically evaluating llvm IR argument");
        return jl_cgval_t();
    }
    if (jl_is_ssavalue(args[2]) && !jl_is_long(ctx.source->ssavaluetypes)) {
        jl_value_t *rtt = jl_arrayref((jl_array_t*)ctx.source->ssavaluetypes, ((jl_ssavalue_t*)args[2])->id - 1);
        if (jl_is_type_type(rtt))
            rt = jl_tparam0(rtt);
    }
    if (!rt) {
        rt = static_eval(ctx, args[2]);
        if (!rt) {
            emit_error(ctx, "error statically evaluating llvmcall return type");
            return jl_cgval_t();
        }
    }
    if (jl_is_ssavalue(args[3]) && !jl_is_long(ctx.source->ssavaluetypes)) {
        jl_value_t *att = jl_arrayref((jl_array_t*)ctx.source->ssavaluetypes, ((jl_ssavalue_t*)args[3])->id - 1);
        if (jl_is_type_type(att))
            at = jl_tparam0(att);
    }
    if (!at) {
        at = static_eval(ctx, args[3]);
        if (!at) {
            emit_error(ctx, "error statically evaluating llvmcall argument tuple");
            return jl_cgval_t();
        }
    }
    if (jl_is_tuple(ir)) {
        // if the IR is a tuple, we expect (mod, fn)
        if (jl_nfields(ir) != 2) {
            emit_error(ctx, "Tuple as first argument to llvmcall must have exactly two children");
            return jl_cgval_t();
        }
        entry = jl_fieldref(ir, 1);
        if (!jl_is_string(entry)) {
            emit_error(ctx, "Function name passed to llvmcall must be a string");
            return jl_cgval_t();
        }
        ir = jl_fieldref(ir, 0);

        if (!jl_is_string(ir) && !jl_typeis(ir, jl_array_uint8_type)) {
            emit_error(ctx, "Module IR passed to llvmcall must be a string or an array of bytes");
            return jl_cgval_t();
        }
    }
    else {
        if (!jl_is_string(ir)) {
            emit_error(ctx, "Function IR passed to llvmcall must be a string");
            return jl_cgval_t();
        }
    }

    JL_TYPECHK(llvmcall, type, rt);
    JL_TYPECHK(llvmcall, type, at);

    // Generate arguments
    std::string arguments;
    raw_string_ostream argstream(arguments);
    jl_svec_t *tt = ((jl_datatype_t*)at)->parameters;
    jl_value_t *rtt = rt;
    size_t nargt = jl_svec_len(tt);

    /*
     * Semantics for arguments are as follows:
     * If the argument type is immutable (including bitstype), we pass the loaded llvm value
     * type. Otherwise we pass a pointer to a jl_value_t.
     */
    std::vector<llvm::Type*> argtypes;
    Value **argvals = (Value**)alloca(nargt * sizeof(Value*));
    for (size_t i = 0; i < nargt; ++i) {
        jl_value_t *tti = jl_svecref(tt,i);
        bool toboxed;
        Type *t = julia_type_to_llvm(ctx, tti, &toboxed);
        argtypes.push_back(t);
        if (4 + i > nargs) {
            emit_error(ctx, "Missing arguments to llvmcall!");
            return jl_cgval_t();
        }
        jl_value_t *argi = args[4 + i];
        jl_cgval_t arg = emit_expr(ctx, argi);

        Value *v = julia_to_native(ctx, t, toboxed, tti, NULL, arg, false, i);
        bool issigned = jl_signed_type && jl_subtype(tti, (jl_value_t*)jl_signed_type);
        argvals[i] = llvm_type_rewrite(ctx, v, t, issigned);
    }

    bool retboxed;
    Type *rettype = julia_type_to_llvm(ctx, rtt, &retboxed);

    // Make sure to find a unique name
    std::string ir_name;
    while (true) {
        raw_string_ostream(ir_name) << (ctx.f->getName().str()) << "u" << globalUniqueGeneratedNames++;
        if (jl_Module->getFunction(ir_name) == NULL)
            break;
    }

    // generate a temporary module that contains our IR
    std::unique_ptr<Module> Mod;
    if (entry == NULL) {
        // we only have function IR, which we should put in a function

        bool first = true;
        for (std::vector<Type *>::iterator it = argtypes.begin(); it != argtypes.end(); ++it) {
            if (!first)
                argstream << ",";
            else
                first = false;
            (*it)->print(argstream);
            argstream << " ";
        }

        std::string rstring;
        raw_string_ostream rtypename(rstring);
        rettype->print(rtypename);
        std::map<uint64_t,std::string> localDecls;

        std::string ir_string;
        raw_string_ostream ir_stream(ir_string);
        ir_stream << "; Number of arguments: " << nargt << "\n"
        << "define "<<rtypename.str()<<" @\"" << ir_name << "\"("<<argstream.str()<<") {\n"
        << jl_string_data(ir) << "\n}";

        SMDiagnostic Err = SMDiagnostic();
        Mod = parseAssemblyString(ir_stream.str(), Err, ctx.builder.getContext());
        if (!Mod) {
            std::string message = "Failed to parse LLVM assembly: \n";
            raw_string_ostream stream(message);
            Err.print("", stream, true);
            emit_error(ctx, stream.str());
            return jl_cgval_t();
        }

        Function *f = Mod->getFunction(ir_name);
        f->addFnAttr(Attribute::AlwaysInline);
    }
    else {
        // we have the IR or bitcode of an entire module, which we can parse directly

        if (jl_is_string(ir)) {
            SMDiagnostic Err = SMDiagnostic();
            Mod = parseAssemblyString(jl_string_data(ir), Err, ctx.builder.getContext());
            if (!Mod) {
                std::string message = "Failed to parse LLVM assembly: \n";
                raw_string_ostream stream(message);
                Err.print("", stream, true);
                emit_error(ctx, stream.str());
                return jl_cgval_t();
            }
        }
        else {
            auto Buf = MemoryBuffer::getMemBuffer(
                StringRef((char *)jl_array_data(ir), jl_array_len(ir)), "llvmcall",
                /*RequiresNullTerminator*/ false);
            Expected<std::unique_ptr<Module>> ModuleOrErr =
                parseBitcodeFile(*Buf, ctx.builder.getContext());
            if (Error Err = ModuleOrErr.takeError()) {
                std::string Message;
                handleAllErrors(std::move(Err),
                                [&](ErrorInfoBase &EIB) { Message = EIB.message(); });
                std::string message = "Failed to parse LLVM bitcode: \n";
                raw_string_ostream stream(message);
                stream << Message;
                emit_error(ctx, stream.str());
                return jl_cgval_t();
            }
            Mod = std::move(ModuleOrErr.get());
        }

        Function *f = Mod->getFunction(jl_string_data(entry));
        if (!f) {
            emit_error(ctx, "Module IR does not contain specified entry function");
            return jl_cgval_t();
        }
        f->setName(ir_name);

        // verify the function type
        assert(!f->isDeclaration());
        assert(f->getReturnType() == rettype);
        int i = 0;
        for (std::vector<Type *>::iterator it = argtypes.begin();
            it != argtypes.end(); ++it, ++i)
            assert(*it == f->getFunctionType()->getParamType(i));
    }

    // copy module properties that should always match
    Mod->setTargetTriple(jl_Module->getTargetTriple());
    Mod->setDataLayout(jl_Module->getDataLayout());
#if JL_LLVM_VERSION >= 130000
    Mod->setStackProtectorGuard(jl_Module->getStackProtectorGuard());
    Mod->setOverrideStackAlignment(jl_Module->getOverrideStackAlignment());
#endif

    // verify the definition
    Function *def = Mod->getFunction(ir_name);
    assert(def);
    std::string message = "Malformed LLVM function: \n";
    raw_string_ostream stream(message);
    if (verifyFunction(*def, &stream)) {
        emit_error(ctx, stream.str());
        return jl_cgval_t();
    }
    def->setLinkage(GlobalVariable::LinkOnceODRLinkage);

    // generate a call
    FunctionType *decl_typ = FunctionType::get(rettype, argtypes, def->isVarArg());
    Function *decl = Function::Create(decl_typ, def->getLinkage(), def->getAddressSpace(),
                                      def->getName(), jl_Module);
    decl->setAttributes(def->getAttributes());
    CallInst *inst = ctx.builder.CreateCall(decl, ArrayRef<Value *>(&argvals[0], nargt));

    // save the module to be linked later.
    // we cannot do this right now, because linking mutates the destination module,
    // which might invalidate LLVM values cached in cgval_t's (specifically constant arrays)
    ctx.llvmcall_modules.push_back(orc::ThreadSafeModule(std::move(Mod), ctx.emission_context.tsctx));

    JL_GC_POP();

    if (inst->getType() != rettype) {
        std::string message;
        raw_string_ostream stream(message);
        stream << "llvmcall return type " << *inst->getType()
               << " does not match declared return type" << *rettype;
        emit_error(ctx, stream.str());
        return jl_cgval_t();
    }

    return mark_julia_type(ctx, inst, retboxed, rtt);
}

// --- code generator for ccall itself ---

// Returns ctx.types().T_prjlvalue
static Value *box_ccall_result(jl_codectx_t &ctx, Value *result, Value *runtime_dt, jl_value_t *rt)
{
    // XXX: need to handle parameterized zero-byte types (singleton)
    const DataLayout &DL = ctx.builder.GetInsertBlock()->getModule()->getDataLayout();
    unsigned nb = DL.getTypeStoreSize(result->getType());
    MDNode *tbaa = jl_is_mutable(rt) ? ctx.tbaa().tbaa_mutab : ctx.tbaa().tbaa_immut;
    Value *strct = emit_allocobj(ctx, nb, runtime_dt);
    init_bits_value(ctx, strct, result, tbaa);
    return strct;
}

static jl_cgval_t mark_or_box_ccall_result(jl_codectx_t &ctx, Value *result, bool isboxed, jl_value_t *rt, jl_unionall_t *unionall, bool static_rt)
{
    if (!static_rt) {
        assert(!isboxed && jl_is_datatype(rt) && ctx.spvals_ptr && unionall);
        Value *runtime_dt = runtime_apply_type_env(ctx, rt);
        // TODO: skip this check if rt is not a Tuple
        emit_concretecheck(ctx, runtime_dt, "ccall: return type must be a concrete DataType");
        Value *strct = box_ccall_result(ctx, result, runtime_dt, rt);
        return mark_julia_type(ctx, strct, true, rt); // TODO: jl_rewrap_unionall(rt, unionall)
    }
    return mark_julia_type(ctx, result, isboxed, rt);
}

class function_sig_t {
public:
    std::vector<Type*> fargt; // vector of llvm output types (julia_struct_to_llvm) for arguments
    std::vector<Type*> fargt_sig; // vector of ABI coercion types for call signature
    std::vector<bool> fargt_isboxed; // vector of whether the llvm output type is a Julia-box for each argument
    std::vector<bool> byRefList; // vector of "byref" parameters
    AttributeList attributes; // vector of function call site attributes
    Type *lrt; // input parameter of the llvm return type (from julia_struct_to_llvm)
    bool retboxed; // input parameter indicating whether lrt is jl_value_t*
    Type *prt; // out parameter of the llvm return type for the function signature
    int sret; // out parameter for indicating whether return value has been moved to the first argument position
    std::string err_msg;
    CallingConv::ID cc; // calling convention ABI
    bool llvmcall;
    jl_svec_t *at; // svec of julia argument types
    jl_value_t *rt; // julia return type
    jl_unionall_t *unionall_env; // UnionAll environment for `at` and `rt`
    size_t nccallargs; // number of actual arguments
    size_t nreqargs; // number of required arguments in ccall function definition
    jl_codegen_params_t *ctx;

    function_sig_t(const char *fname, Type *lrt, jl_value_t *rt, bool retboxed, jl_svec_t *at, jl_unionall_t *unionall_env, size_t nreqargs, CallingConv::ID cc, bool llvmcall, jl_codegen_params_t *ctx)
      : lrt(lrt), retboxed(retboxed),
        prt(NULL), sret(0), cc(cc), llvmcall(llvmcall),
        at(at), rt(rt), unionall_env(unionall_env),
        nccallargs(jl_svec_len(at)), nreqargs(nreqargs),
        ctx(ctx)
    {
        err_msg = generate_func_sig(fname);
    }

    FunctionType *functype(LLVMContext &ctxt) const {
        assert(err_msg.empty());
        if (nreqargs > 0)
            return FunctionType::get(sret ? getVoidTy(ctxt) : prt, makeArrayRef(fargt_sig).slice(0, nreqargs), true);
        else
            return FunctionType::get(sret ? getVoidTy(ctxt) : prt, fargt_sig, false);
    }

    jl_cgval_t emit_a_ccall(
            jl_codectx_t &ctx,
            const native_sym_arg_t &symarg,
            jl_cgval_t *argv,
            SmallVector<Value*, 16> &gc_uses,
            bool static_rt) const;

private:
std::string generate_func_sig(const char *fname)
{
    assert(rt && !jl_is_abstract_ref_type(rt));

    std::vector<AttrBuilder> paramattrs;
    std::unique_ptr<AbiLayout> abi;
    if (llvmcall)
        abi.reset(new ABI_LLVMLayout());
    else
        abi.reset(new DefaultAbiState());
    sret = 0;
    LLVMContext &LLVMCtx = lrt->getContext();
    if (type_is_ghost(lrt)) {
        prt = lrt = getVoidTy(LLVMCtx);
        abi->use_sret(jl_nothing_type, LLVMCtx);
    }
    else {
        if (retboxed || jl_is_cpointer_type(rt) || lrt->isPointerTy()) {
            prt = lrt; // passed as pointer
            abi->use_sret(jl_voidpointer_type, LLVMCtx);
        }
        else if (abi->use_sret((jl_datatype_t*)rt, LLVMCtx)) {
#if JL_LLVM_VERSION >= 140000
            AttrBuilder retattrs(LLVMCtx);
#else
            AttrBuilder retattrs;
#endif
#if !defined(_OS_WINDOWS_) // llvm used to use the old mingw ABI, skipping this marking works around that difference
            retattrs.addStructRetAttr(lrt);
#endif
            retattrs.addAttribute(Attribute::NoAlias);
            paramattrs.push_back(std::move(retattrs));
            fargt_sig.push_back(PointerType::get(lrt, 0));
            sret = 1;
            prt = lrt;
        }
        else {
            prt = abi->preferred_llvm_type((jl_datatype_t*)rt, true, LLVMCtx);
            if (prt == NULL)
                prt = lrt;
        }
    }

    for (size_t i = 0; i < nccallargs; ++i) {
#if JL_LLVM_VERSION >= 140000
        AttrBuilder ab(LLVMCtx);
#else
        AttrBuilder ab;
#endif
        jl_value_t *tti = jl_svecref(at, i);
        Type *t = NULL;
        bool isboxed;
        if (jl_is_abstract_ref_type(tti)) {
            tti = (jl_value_t*)jl_voidpointer_type;
            t = getInt8PtrTy(LLVMCtx);
            isboxed = false;
        }
        else if (llvmcall && jl_is_llvmpointer_type(tti)) {
            t = bitstype_to_llvm(tti, LLVMCtx, true);
            tti = (jl_value_t*)jl_voidpointer_type;
            isboxed = false;
        }
        else {
            if (jl_is_primitivetype(tti)) {
                // see pull req #978. need to annotate signext/zeroext for
                // small integer arguments.
                jl_datatype_t *bt = (jl_datatype_t*)tti;
                if (jl_datatype_size(bt) < 4 && bt != jl_float16_type) {
                    if (jl_signed_type && jl_subtype(tti, (jl_value_t*)jl_signed_type))
                        ab.addAttribute(Attribute::SExt);
                    else
                        ab.addAttribute(Attribute::ZExt);
                }
            }

            t = _julia_struct_to_llvm(ctx, LLVMCtx, tti, &isboxed, llvmcall);
            if (t == getVoidTy(LLVMCtx)) {
                return make_errmsg(fname, i + 1, " type doesn't correspond to a C type");
            }
        }

        Type *pat;
        if (!jl_is_datatype(tti) || ((jl_datatype_t*)tti)->layout == NULL || jl_is_layout_opaque(((jl_datatype_t*)tti)->layout)) {
            tti = (jl_value_t*)jl_voidpointer_type; // passed as pointer
        }

        // Whether or not LLVM wants us to emit a pointer to the data
        assert(t && "LLVM type should not be null");
        bool byRef = abi->needPassByRef((jl_datatype_t*)tti, ab, LLVMCtx, t);

        if (jl_is_cpointer_type(tti)) {
            pat = t;
        }
        else if (byRef) {
            pat = PointerType::get(t, AddressSpace::Derived);
        }
        else {
            pat = abi->preferred_llvm_type((jl_datatype_t*)tti, false, LLVMCtx);
            if (pat == NULL)
                pat = t;
        }

        if (!byRef && nreqargs > 0 && i >= nreqargs) { // TODO: handle byRef case too?
            // The C vararg ABI says that small types must get widened,
            // but we don't really want to expect the user to know that,
            // so attempt to do that coercion here
            if (!llvmcall && cc == CallingConv::C) {
                if (pat->isIntegerTy() && pat->getPrimitiveSizeInBits() < sizeof(int) * 8)
                    pat = getInt32Ty(lrt->getContext());
                if (pat->isFloatingPointTy() && pat->getPrimitiveSizeInBits() < sizeof(double) * 8)
                    pat = getDoubleTy(lrt->getContext());
                ab.removeAttribute(Attribute::SExt);
                ab.removeAttribute(Attribute::ZExt);
            }
        }

        byRefList.push_back(byRef);
        fargt.push_back(t);
        fargt_isboxed.push_back(isboxed);
        fargt_sig.push_back(pat);
#if JL_LLVM_VERSION >= 140000
        paramattrs.push_back(AttrBuilder(LLVMCtx, AttributeSet::get(LLVMCtx, ab)));
#else
        paramattrs.push_back(AttributeSet::get(LLVMCtx, ab));
#endif
    }

    for (size_t i = 0; i < nccallargs + sret; ++i) {
        const auto &as = paramattrs.at(i);
        if (!as.hasAttributes())
            continue;
        attributes = addAttributesAtIndex(attributes, LLVMCtx, i + 1, as);
    }
    // If return value is boxed it must be non-null.
    if (retboxed)
        attributes = addRetAttribute(attributes, LLVMCtx, Attribute::NonNull);
    if (rt == jl_bottom_type) {
        attributes = addFnAttribute(attributes, LLVMCtx, Attribute::NoReturn);
    }
    return "";
}
};

static std::pair<CallingConv::ID, bool> convert_cconv(jl_sym_t *lhd)
{
    // check for calling convention specifier
    if (lhd == jl_symbol("stdcall")) {
        return std::make_pair(CallingConv::X86_StdCall, false);
    }
    else if (lhd == jl_symbol("cdecl") || lhd == jl_symbol("ccall")) {
        // `ccall` calling convention is a placeholder for when there isn't one provided
        // it is not by itself a valid calling convention name to be specified in the surface
        // syntax.
        return std::make_pair(CallingConv::C, false);
    }
    else if (lhd == jl_symbol("fastcall")) {
        return std::make_pair(CallingConv::X86_FastCall, false);
    }
    else if (lhd == jl_symbol("thiscall")) {
        return std::make_pair(CallingConv::X86_ThisCall, false);
    }
    else if (lhd == jl_symbol("llvmcall")) {
        return std::make_pair(CallingConv::C, true);
    }
    jl_errorf("ccall: invalid calling convention %s", jl_symbol_name(lhd));
}

static bool verify_ref_type(jl_codectx_t &ctx, jl_value_t* ref, jl_unionall_t *unionall_env, int n, const char *fname)
{
    // emit verification that the tparam for Ref isn't Any or a TypeVar
    const char rt_err_msg_notany[] = " type Ref{Any} is invalid. Use Any or Ptr{Any} instead.";
    if (ref == (jl_value_t*)jl_any_type && n == 0) {
        emit_error(ctx, make_errmsg(fname, n, rt_err_msg_notany));
        return false;
    }
    else if (jl_is_typevar(ref)) {
        bool always_error = true;
        if (unionall_env) {
            int i;
            jl_unionall_t *ua = unionall_env;
            for (i = 0; jl_is_unionall(ua); i++) {
                if (ua->var == (jl_tvar_t*)ref) {
                    jl_cgval_t runtime_sp = emit_sparam(ctx, i);
                    if (n > 0) {
                        always_error = false;
                    }
                    else if (runtime_sp.constant) {
                        if (runtime_sp.constant != (jl_value_t*)jl_any_type)
                            always_error = false;
                    }
                    else {
                        Value *notany = ctx.builder.CreateICmpNE(
                                boxed(ctx, runtime_sp),
                                track_pjlvalue(ctx, literal_pointer_val(ctx, (jl_value_t*)jl_any_type)));
                        error_unless(ctx, notany, make_errmsg(fname, n, rt_err_msg_notany));
                        always_error = false;
                    }
                    break;
                }
                ua = (jl_unionall_t*)ua->body;
            }
        }
        if (always_error) {
            emit_error(ctx, make_errmsg(fname, n, " type Ref should have an element type, not Ref{<:T}."));
            return false;
        }
    }
    return true;
}

static const std::string verify_ccall_sig(jl_value_t *&rt, jl_value_t *at,
                                          jl_unionall_t *unionall_env, jl_svec_t *sparam_vals,
                                          jl_codegen_params_t *ctx,
                                          Type *&lrt, LLVMContext &ctxt,
                                          bool &retboxed, bool &static_rt, bool llvmcall=false)
{
    JL_TYPECHK(ccall, type, rt);
    JL_TYPECHK(ccall, simplevector, at);

    if (rt == (jl_value_t*)jl_any_type || jl_is_array_type(rt) ||
            (jl_is_datatype(rt) && ((jl_datatype_t*)rt)->layout != NULL &&
             jl_is_layout_opaque(((jl_datatype_t*)rt)->layout))) {
        // n.b. `Array` used as return type just returns a julia object reference
        lrt = JuliaType::get_prjlvalue_ty(ctxt);
        retboxed = true;
    }
    else {
        // jl_type_mappable_to_c should have already ensured that these are valid
        assert(jl_is_structtype(rt) || jl_is_primitivetype(rt) || rt == (jl_value_t*)jl_bottom_type);
        lrt = _julia_struct_to_llvm(ctx, ctxt, rt, &retboxed, llvmcall);
        assert(!retboxed);
        if (CountTrackedPointers(lrt).count != 0)
            return "return type struct fields cannot contain a reference";
    }

    // is return type fully statically known?
    if (unionall_env == NULL) {
        static_rt = true;
    }
    else {
        static_rt = retboxed || !jl_has_typevar_from_unionall(rt, unionall_env);
        if (!static_rt && sparam_vals != NULL && jl_svec_len(sparam_vals) > 0) {
            rt = jl_instantiate_type_in_env(rt, unionall_env, jl_svec_data(sparam_vals));
            // `rt` is gc-rooted by the caller
            static_rt = true;
        }
    }

    return "";
}

const int fc_args_start = 6;

// Expr(:foreigncall, pointer, rettype, (argtypes...), nreq, [cconv | (cconv, effects)], args..., roots...)
static jl_cgval_t emit_ccall(jl_codectx_t &ctx, jl_value_t **args, size_t nargs)
{
    JL_NARGSV(ccall, 5);
    args -= 1;
    jl_value_t *rt = args[2];
    jl_value_t *at = args[3];
    size_t nccallargs = jl_svec_len(at);
    size_t nreqargs = jl_unbox_long(args[4]); // if vararg
    assert(jl_is_quotenode(args[5]));
    jl_value_t *jlcc = jl_quotenode_value(args[5]);
    jl_sym_t *cc_sym = NULL;
    if (jl_is_symbol(jlcc)) {
        cc_sym = (jl_sym_t*)jlcc;
    }
    else if (jl_is_tuple(jlcc)) {
        cc_sym = (jl_sym_t*)jl_get_nth_field_noalloc(jlcc, 0);
    }
    assert(jl_is_symbol(cc_sym));
    native_sym_arg_t symarg = {};
    JL_GC_PUSH3(&rt, &at, &symarg.gcroot);

    CallingConv::ID cc = CallingConv::C;
    bool llvmcall = false;
    std::tie(cc, llvmcall) = convert_cconv(cc_sym);

    interpret_symbol_arg(ctx, symarg, args[1], "ccall", llvmcall);
    Value *&jl_ptr = symarg.jl_ptr;
    void (*&fptr)(void) = symarg.fptr;
    const char *&f_name = symarg.f_name;
    const char *&f_lib = symarg.f_lib;

    if (f_name == NULL && fptr == NULL && jl_ptr == NULL) {
        emit_error(ctx, "ccall: null function pointer");
        JL_GC_POP();
        return jl_cgval_t();
    }

    auto ccallarg = [=] (size_t i) {
        assert(i < nccallargs && i + fc_args_start <= nargs);
        return args[fc_args_start + i];
    };

    auto _is_libjulia_func = [&] (uintptr_t ptr, StringRef name) {
        if ((uintptr_t)fptr == ptr)
            return true;
        if (f_lib) {
#ifdef _OS_WINDOWS_
            if ((f_lib == JL_EXE_LIBNAME) || // preventing invalid pointer access
                (f_lib == JL_LIBJULIA_INTERNAL_DL_LIBNAME) ||
                (f_lib == JL_LIBJULIA_DL_LIBNAME) ||
                (!strcmp(f_lib, jl_crtdll_basename))) {
                // libjulia-like
            }
            else
                return false;
#else
            return false;
#endif
        }
        return f_name && f_name == name;
    };
#define is_libjulia_func(name) _is_libjulia_func((uintptr_t)&(name), StringRef(XSTR(name)))

    // emit arguments
    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nccallargs);
    for (size_t i = 0; i < nccallargs; i++) {
        // Julia (expression) value of current parameter
        jl_value_t *argi = ccallarg(i);
        argv[i] = emit_expr(ctx, argi);
    }

    // emit roots
    SmallVector<Value*, 16> gc_uses;
    for (size_t i = nccallargs + fc_args_start; i <= nargs; i++) {
        // Julia (expression) value of current parameter gcroot
        jl_value_t *argi_root = args[i];
        if (jl_is_long(argi_root))
            continue;
        jl_cgval_t arg_root = emit_expr(ctx, argi_root);
        Value *gc_root = get_gc_root_for(arg_root);
        if (gc_root)
            gc_uses.push_back(gc_root);
    }

    jl_unionall_t *unionall = (jl_is_method(ctx.linfo->def.method) && jl_is_unionall(ctx.linfo->def.method->sig))
        ? (jl_unionall_t*)ctx.linfo->def.method->sig
        : NULL;

    if (jl_is_abstract_ref_type(rt)) {
        if (!verify_ref_type(ctx, jl_tparam0(rt), unionall, 0, "ccall")) {
            JL_GC_POP();
            return jl_cgval_t();
        }
        rt = (jl_value_t*)jl_any_type; // convert return type to jl_value_t*
    }

    // some sanity checking and check whether there's a vararg
    Type *lrt;
    bool retboxed;
    bool static_rt;
    std::string err = verify_ccall_sig(
      /* inputs:  */
      rt, at, unionall,
      ctx.spvals_ptr == NULL ? ctx.linfo->sparam_vals : NULL,
      &ctx.emission_context,
      /* outputs: */
      lrt, ctx.builder.getContext(),
      retboxed, static_rt,
      /* optional arguments */
      llvmcall);
    if (err.empty()) {
        // some extra checks for ccall
        if (!retboxed && static_rt) {
            if (!jl_is_concrete_type(rt)) {
                if (jl_is_cpointer_type(rt))
                    err = "return type Ptr should have an element type (not Ptr{<:T})";
                else if (rt != jl_bottom_type)
                    err = "return type must be a concrete DataType";
            }
        }
        assert(jl_svec_len(at) >= nreqargs);
    }
    if (!err.empty()) {
        emit_error(ctx, "ccall " + err);
        JL_GC_POP();
        return jl_cgval_t();
    }
    if (rt != args[2] && rt != (jl_value_t*)jl_any_type)
        jl_add_method_root(ctx, rt);
    function_sig_t sig("ccall", lrt, rt, retboxed,
                       (jl_svec_t*)at, unionall, nreqargs,
                       cc, llvmcall, &ctx.emission_context);
    for (size_t i = 0; i < nccallargs; i++) {
        jl_value_t *tti = jl_svecref(at, i);
        if (jl_is_abstract_ref_type(tti)) {
            if (!verify_ref_type(ctx, jl_tparam0(tti), unionall, i + 1, "ccall")) {
                JL_GC_POP();
                return jl_cgval_t();
            }
        }
    }

    // some special functions
    bool isVa = nreqargs > 0;
    (void)isVa; // prevent compiler warning
    if (is_libjulia_func(jl_array_ptr)) {
        ++CCALL_STAT(jl_array_ptr);
        assert(lrt == getSizeTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 1);
        const jl_cgval_t &ary = argv[0];
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, ctx.builder.CreatePtrToInt(emit_unsafe_arrayptr(ctx, ary), lrt),
                                        retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_value_ptr)) {
        ++CCALL_STAT(jl_value_ptr);
        assert(retboxed ? lrt == ctx.types().T_prjlvalue : lrt == getSizeTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 1);
        jl_value_t *tti = jl_svecref(at, 0);
        Type *largty;
        bool isboxed;
        if (jl_is_abstract_ref_type(tti)) {
            tti = (jl_value_t*)jl_voidpointer_type;
            largty = getSizeTy(ctx.builder.getContext());
            isboxed = false;
        }
        else {
            largty = _julia_struct_to_llvm(&ctx.emission_context, ctx.builder.getContext(), tti, &isboxed, llvmcall);
        }
        Value *retval;
        if (isboxed) {
            retval = boxed(ctx, argv[0]);
            retval = emit_pointer_from_objref(ctx, emit_bitcast(ctx, retval, ctx.types().T_prjlvalue));
        }
        else {
            retval = emit_unbox(ctx, largty, argv[0], tti);
            retval = emit_inttoptr(ctx, retval, ctx.types().T_pjlvalue);
        }
        // retval is now an untracked jl_value_t*
        if (retboxed)
            // WARNING: this addrspace cast necessarily implies that the value is rooted elsewhere!
            retval = ctx.builder.CreateAddrSpaceCast(retval, ctx.types().T_prjlvalue);
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, retval, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_cpu_pause)) {
        ++CCALL_STAT(jl_cpu_pause);
        // Keep in sync with the julia_threads.h version
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
#ifdef __MIC__
        // TODO
#elif defined(_CPU_X86_64_) || defined(_CPU_X86_)  /* !__MIC__ */
        auto pauseinst = InlineAsm::get(FunctionType::get(getVoidTy(ctx.builder.getContext()), false), "pause",
                                               "~{memory}", true);
        ctx.builder.CreateCall(pauseinst);
        JL_GC_POP();
        return ghostValue(ctx, jl_nothing_type);
#elif defined(_CPU_AARCH64_) || (defined(_CPU_ARM_) && __ARM_ARCH >= 7)
        auto wfeinst = InlineAsm::get(FunctionType::get(getVoidTy(ctx.builder.getContext()), false), "wfe",
                                             "~{memory}", true);
        ctx.builder.CreateCall(wfeinst);
        JL_GC_POP();
        return ghostValue(ctx, jl_nothing_type);
#else
        JL_GC_POP();
        return ghostValue(ctx, jl_nothing_type);
#endif
    }
    else if (is_libjulia_func(jl_cpu_wake)) {
        ++CCALL_STAT(jl_cpu_wake);
        // Keep in sync with the julia_threads.h version
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
#if JL_CPU_WAKE_NOOP == 1
        JL_GC_POP();
        return ghostValue(ctx, jl_nothing_type);
#elif defined(_CPU_AARCH64_) || (defined(_CPU_ARM_) && __ARM_ARCH >= 7)
        auto sevinst = InlineAsm::get(FunctionType::get(getVoidTy(ctx.builder.getContext()), false), "sev",
                                             "~{memory}", true);
        ctx.builder.CreateCall(sevinst);
        JL_GC_POP();
        return ghostValue(ctx, jl_nothing_type);
#endif
    }
    else if (is_libjulia_func(jl_gc_safepoint)) {
        ++CCALL_STAT(jl_gc_safepoint);
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        emit_gc_safepoint(ctx);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func("jl_get_ptls_states")) {
        ++CCALL_STAT(jl_get_ptls_states);
        assert(lrt == getSizeTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx,
            ctx.builder.CreatePtrToInt(get_current_ptls(ctx), lrt),
            retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_threadid)) {
        ++CCALL_STAT(jl_threadid);
        assert(lrt == getInt16Ty(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        Value *ptask_i16 = emit_bitcast(ctx, get_current_task(ctx), getInt16PtrTy(ctx.builder.getContext()));
        const int tid_offset = offsetof(jl_task_t, tid);
        Value *ptid = ctx.builder.CreateInBoundsGEP(getInt16Ty(ctx.builder.getContext()), ptask_i16, ConstantInt::get(getSizeTy(ctx.builder.getContext()), tid_offset / sizeof(int16_t)));
        LoadInst *tid = ctx.builder.CreateAlignedLoad(getInt16Ty(ctx.builder.getContext()), ptid, Align(sizeof(int16_t)));
        tbaa_decorate(ctx.tbaa().tbaa_gcframe, tid);
        return mark_or_box_ccall_result(ctx, tid, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_gc_disable_finalizers_internal)
#ifdef NDEBUG
             || is_libjulia_func(jl_gc_enable_finalizers_internal)
#endif
             ) {
        JL_GC_POP();
        Value *ptls_i32 = emit_bitcast(ctx, get_current_ptls(ctx), getInt32PtrTy(ctx.builder.getContext()));
        const int finh_offset = offsetof(jl_tls_states_t, finalizers_inhibited);
        Value *pfinh = ctx.builder.CreateInBoundsGEP(getInt32Ty(ctx.builder.getContext()), ptls_i32, ConstantInt::get(getSizeTy(ctx.builder.getContext()), finh_offset / 4));
        LoadInst *finh = ctx.builder.CreateAlignedLoad(getInt32Ty(ctx.builder.getContext()), pfinh, Align(sizeof(int32_t)));
        Value *newval;
        if (is_libjulia_func(jl_gc_disable_finalizers_internal)) {
            newval = ctx.builder.CreateAdd(finh, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 1));
        }
        else {
            newval = ctx.builder.CreateSelect(ctx.builder.CreateICmpEQ(finh, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0)),
                                              ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0),
                                              ctx.builder.CreateSub(finh, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 1)));
        }
        ctx.builder.CreateStore(newval, pfinh);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func(jl_get_current_task)) {
        ++CCALL_STAT(jl_get_current_task);
        assert(lrt == ctx.types().T_prjlvalue);
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        auto ct = track_pjlvalue(ctx, emit_bitcast(ctx, get_current_task(ctx), ctx.types().T_pjlvalue));
        return mark_or_box_ccall_result(ctx, ct, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_set_next_task)) {
        ++CCALL_STAT(jl_set_next_task);
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 1);
        JL_GC_POP();
        Value *ptls_pv = emit_bitcast(ctx, get_current_ptls(ctx), ctx.types().T_ppjlvalue);
        const int nt_offset = offsetof(jl_tls_states_t, next_task);
        Value *pnt = ctx.builder.CreateInBoundsGEP(ctx.types().T_pjlvalue, ptls_pv, ConstantInt::get(getSizeTy(ctx.builder.getContext()), nt_offset / sizeof(void*)));
        ctx.builder.CreateStore(emit_pointer_from_objref(ctx, boxed(ctx, argv[0])), pnt);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func(jl_sigatomic_begin)) {
        ++CCALL_STAT(jl_sigatomic_begin);
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        ctx.builder.CreateCall(prepare_call(gcroot_flush_func));
        Value *pdefer_sig = emit_defer_signal(ctx);
        Value *defer_sig = ctx.builder.CreateLoad(ctx.types().T_sigatomic, pdefer_sig);
        defer_sig = ctx.builder.CreateAdd(defer_sig, ConstantInt::get(ctx.types().T_sigatomic, 1));
        ctx.builder.CreateStore(defer_sig, pdefer_sig);
        emit_signal_fence(ctx);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func(jl_sigatomic_end)) {
        ++CCALL_STAT(jl_sigatomic_end);
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        ctx.builder.CreateCall(prepare_call(gcroot_flush_func));
        Value *pdefer_sig = emit_defer_signal(ctx);
        Value *defer_sig = ctx.builder.CreateLoad(ctx.types().T_sigatomic, pdefer_sig);
        emit_signal_fence(ctx);
        error_unless(ctx,
                ctx.builder.CreateICmpNE(defer_sig, ConstantInt::get(ctx.types().T_sigatomic, 0)),
                "sigatomic_end called in non-sigatomic region");
        defer_sig = ctx.builder.CreateSub(
                defer_sig,
                ConstantInt::get(ctx.types().T_sigatomic, 1));
        ctx.builder.CreateStore(defer_sig, pdefer_sig);
        BasicBlock *checkBB = BasicBlock::Create(ctx.builder.getContext(), "check",
                                                 ctx.f);
        BasicBlock *contBB = BasicBlock::Create(ctx.builder.getContext(), "cont");
        ctx.builder.CreateCondBr(
                ctx.builder.CreateICmpEQ(defer_sig, ConstantInt::get(ctx.types().T_sigatomic, 0)),
                checkBB, contBB);
        ctx.builder.SetInsertPoint(checkBB);
        ctx.builder.CreateLoad(
                getSizeTy(ctx.builder.getContext()),
                ctx.builder.CreateConstInBoundsGEP1_32(getSizeTy(ctx.builder.getContext()), get_current_signal_page(ctx), -1),
                true);
        ctx.builder.CreateBr(contBB);
        ctx.f->getBasicBlockList().push_back(contBB);
        ctx.builder.SetInsertPoint(contBB);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func(jl_svec_len)) {
        ++CCALL_STAT(jl_svec_len);
        assert(!isVa && !llvmcall && nccallargs == 1);
        const jl_cgval_t &svecv = argv[0];
        Value *len;
        if (svecv.constant && svecv.typ == (jl_value_t*)jl_simplevector_type) {
            // Check the type as well before we call
            len = ConstantInt::get(getSizeTy(ctx.builder.getContext()), jl_svec_len(svecv.constant));
        }
        else {
            auto ptr = emit_bitcast(ctx, boxed(ctx, svecv), getSizePtrTy(ctx.builder.getContext()));
            len = ctx.builder.CreateAlignedLoad(getSizeTy(ctx.builder.getContext()), ptr, Align(sizeof(size_t)));
            // Only mark with TBAA if we are sure about the type.
            // This could otherwise be in a dead branch
            if (svecv.typ == (jl_value_t*)jl_simplevector_type)
                tbaa_decorate(ctx.tbaa().tbaa_const, cast<Instruction>(len));
            MDBuilder MDB(ctx.builder.getContext());
            auto rng = MDB.createRange(
                Constant::getNullValue(getSizeTy(ctx.builder.getContext())), ConstantInt::get(getSizeTy(ctx.builder.getContext()), INTPTR_MAX / sizeof(void*) - 1));
            cast<LoadInst>(len)->setMetadata(LLVMContext::MD_range, rng);
        }
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, len, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_svec_isassigned) &&
             argv[1].typ == (jl_value_t*)jl_long_type) {
        ++CCALL_STAT(jl_svec_isassigned);
        assert(!isVa && !llvmcall && nccallargs == 2);
        const jl_cgval_t &svecv = argv[0];
        const jl_cgval_t &idxv = argv[1];
        Value *idx = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), idxv, (jl_value_t*)jl_long_type);
        idx = ctx.builder.CreateAdd(idx, ConstantInt::get(getSizeTy(ctx.builder.getContext()), 1));
        auto ptr = emit_bitcast(ctx, boxed(ctx, svecv), ctx.types().T_pprjlvalue);
        Value *slot_addr = ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue,
                                                         decay_derived(ctx, ptr), idx);
        LoadInst *load = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, slot_addr,
                                                       Align(sizeof(void*)));
        load->setAtomic(AtomicOrdering::Unordered);
        // Only mark with TBAA if we are sure about the type.
        // This could otherwise be in a dead branch
        if (svecv.typ == (jl_value_t*)jl_simplevector_type)
            tbaa_decorate(ctx.tbaa().tbaa_const, load);
        Value *res = ctx.builder.CreateZExt(ctx.builder.CreateICmpNE(load, Constant::getNullValue(ctx.types().T_prjlvalue)), getInt8Ty(ctx.builder.getContext()));
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, res, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_svec_ref) && argv[1].typ == (jl_value_t*)jl_long_type) {
        ++CCALL_STAT(jl_svec_ref);
        assert(lrt == ctx.types().T_prjlvalue);
        assert(!isVa && !llvmcall && nccallargs == 2);
        const jl_cgval_t &svecv = argv[0];
        const jl_cgval_t &idxv = argv[1];
        Value *idx = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), idxv, (jl_value_t*)jl_long_type);
        idx = ctx.builder.CreateAdd(idx, ConstantInt::get(getSizeTy(ctx.builder.getContext()), 1));
        auto ptr = emit_bitcast(ctx, boxed(ctx, svecv), ctx.types().T_pprjlvalue);
        Value *slot_addr = ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue,
                                                         decay_derived(ctx, ptr), idx);
        LoadInst *load = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, slot_addr,
                                                       Align(sizeof(void*)));
        load->setAtomic(AtomicOrdering::Unordered);
        // Only mark with TBAA if we are sure about the type.
        // This could otherwise be in a dead branch
        if (svecv.typ == (jl_value_t*)jl_simplevector_type)
            tbaa_decorate(ctx.tbaa().tbaa_const, load);
        null_pointer_check(ctx, load);
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, load, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_array_isassigned) &&
             argv[1].typ == (jl_value_t*)jl_ulong_type) {
        ++CCALL_STAT(jl_array_isassigned);
        assert(!isVa && !llvmcall && nccallargs == 2);
        jl_value_t *aryex = ccallarg(0);
        const jl_cgval_t &aryv = argv[0];
        const jl_cgval_t &idxv = argv[1];
        jl_datatype_t *arydt = (jl_datatype_t*)jl_unwrap_unionall(aryv.typ);
        if (jl_is_array_type(arydt)) {
            jl_value_t *ety = jl_tparam0(arydt);
            bool ptrarray = !jl_stored_inline(ety);
            if (!ptrarray && !jl_type_hasptr(ety)) {
                JL_GC_POP();
                return mark_or_box_ccall_result(ctx, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 1),
                                                false, rt, unionall, static_rt);
            }
            else if (!jl_has_free_typevars(ety)) {
                Value *idx = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), idxv, (jl_value_t*)jl_ulong_type);
                Value *arrayptr = emit_bitcast(ctx, emit_arrayptr(ctx, aryv, aryex), ctx.types().T_pprjlvalue);
                if (!ptrarray) {
                    size_t elsz = jl_datatype_size(ety);
                    unsigned align = jl_datatype_align(ety);
                    size_t stride = LLT_ALIGN(elsz, align) / sizeof(jl_value_t*);
                    if (stride != 1)
                        idx = ctx.builder.CreateMul(idx, ConstantInt::get(getSizeTy(ctx.builder.getContext()), stride));
                    idx = ctx.builder.CreateAdd(idx, ConstantInt::get(getSizeTy(ctx.builder.getContext()), ((jl_datatype_t*)ety)->layout->first_ptr));
                }
                Value *slot_addr = ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, arrayptr, idx);
                LoadInst *load = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, slot_addr, Align(sizeof(void*)));
                load->setAtomic(AtomicOrdering::Unordered);
                tbaa_decorate(ctx.tbaa().tbaa_ptrarraybuf, load);
                Value *res = ctx.builder.CreateZExt(ctx.builder.CreateICmpNE(load, Constant::getNullValue(ctx.types().T_prjlvalue)), getInt32Ty(ctx.builder.getContext()));
                JL_GC_POP();
                return mark_or_box_ccall_result(ctx, res, retboxed, rt, unionall, static_rt);
            }
        }
    }
    else if (is_libjulia_func(jl_string_ptr)) {
        ++CCALL_STAT(jl_string_ptr);
        assert(lrt == getSizeTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 1);
        auto obj = emit_bitcast(ctx, emit_pointer_from_objref(ctx, boxed(ctx, argv[0])),
                                ctx.types().T_pprjlvalue);
        // The inbounds gep makes it more clear to LLVM that the resulting value is not
        // a null pointer.
        auto strp = ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_prjlvalue, obj, 1);
        strp = ctx.builder.CreatePtrToInt(strp, getSizeTy(ctx.builder.getContext()));
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, strp, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_symbol_name)) {
        ++CCALL_STAT(jl_symbol_name);
        assert(lrt == getSizeTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 1);
        auto obj = emit_bitcast(ctx, emit_pointer_from_objref(ctx, boxed(ctx, argv[0])),
                                ctx.types().T_pprjlvalue);
        // The inbounds gep makes it more clear to LLVM that the resulting value is not
        // a null pointer.
        auto strp = ctx.builder.CreateConstInBoundsGEP1_32(
            ctx.types().T_prjlvalue, obj, (sizeof(jl_sym_t) + sizeof(void*) - 1) / sizeof(void*));
        strp = ctx.builder.CreatePtrToInt(strp, getSizeTy(ctx.builder.getContext()));
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, strp, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(memcpy) && (rt == (jl_value_t*)jl_nothing_type || jl_is_cpointer_type(rt))) {
        ++CCALL_STAT(memcpy);
        const jl_cgval_t &dst = argv[0];
        const jl_cgval_t &src = argv[1];
        const jl_cgval_t &n = argv[2];
        Value *destp = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), dst, (jl_value_t*)jl_voidpointer_type);

        ctx.builder.CreateMemCpy(
                emit_inttoptr(ctx, destp, getInt8PtrTy(ctx.builder.getContext())),
                MaybeAlign(1),
                emit_inttoptr(ctx,
                    emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), src, (jl_value_t*)jl_voidpointer_type),
                    getInt8PtrTy(ctx.builder.getContext())),
                MaybeAlign(0),
                emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), n, (jl_value_t*)jl_ulong_type),
                false);
        JL_GC_POP();
        return rt == (jl_value_t*)jl_nothing_type ? ghostValue(ctx, jl_nothing_type) :
            mark_or_box_ccall_result(ctx, destp, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(memset) && (rt == (jl_value_t*)jl_nothing_type || jl_is_cpointer_type(rt))) {
        ++CCALL_STAT(memset);
        const jl_cgval_t &dst = argv[0];
        const jl_cgval_t &val = argv[1];
        const jl_cgval_t &n = argv[2];
        Value *destp = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), dst, (jl_value_t*)jl_voidpointer_type);
        Value *val32 = emit_unbox(ctx, getInt32Ty(ctx.builder.getContext()), val, (jl_value_t*)jl_uint32_type);
        Value *val8 = ctx.builder.CreateTrunc(val32, getInt8Ty(ctx.builder.getContext()), "memset_val");
        ctx.builder.CreateMemSet(
            emit_inttoptr(ctx, destp, getInt8PtrTy(ctx.builder.getContext())),
            val8,
            emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), n, (jl_value_t*)jl_ulong_type),
            MaybeAlign(1)
        );
        JL_GC_POP();
        return rt == (jl_value_t*)jl_nothing_type ? ghostValue(ctx, jl_nothing_type) :
            mark_or_box_ccall_result(ctx, destp, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(memmove) && (rt == (jl_value_t*)jl_nothing_type || jl_is_cpointer_type(rt))) {
        ++CCALL_STAT(memmove);
        const jl_cgval_t &dst = argv[0];
        const jl_cgval_t &src = argv[1];
        const jl_cgval_t &n = argv[2];
        Value *destp = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), dst, (jl_value_t*)jl_voidpointer_type);

        ctx.builder.CreateMemMove(
                emit_inttoptr(ctx, destp, getInt8PtrTy(ctx.builder.getContext())),
                MaybeAlign(0),
                emit_inttoptr(ctx,
                    emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), src, (jl_value_t*)jl_voidpointer_type),
                    getInt8PtrTy(ctx.builder.getContext())),
                MaybeAlign(0),
                emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), n, (jl_value_t*)jl_ulong_type),
                false);
        JL_GC_POP();
        return rt == (jl_value_t*)jl_nothing_type ? ghostValue(ctx, jl_nothing_type) :
            mark_or_box_ccall_result(ctx, destp, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_object_id) && nccallargs == 1 &&
            rt == (jl_value_t*)jl_ulong_type) {
        ++CCALL_STAT(jl_object_id);
        jl_cgval_t val = argv[0];
        if (val.typ == (jl_value_t*)jl_symbol_type) {
            JL_GC_POP();
            const int hash_offset = offsetof(jl_sym_t, hash);
            Value *ph1 = emit_bitcast(ctx, decay_derived(ctx, boxed(ctx, val)), getSizePtrTy(ctx.builder.getContext()));
            Value *ph2 = ctx.builder.CreateInBoundsGEP(getSizeTy(ctx.builder.getContext()), ph1, ConstantInt::get(getSizeTy(ctx.builder.getContext()), hash_offset / sizeof(size_t)));
            LoadInst *hashval = ctx.builder.CreateAlignedLoad(getSizeTy(ctx.builder.getContext()), ph2, Align(sizeof(size_t)));
            tbaa_decorate(ctx.tbaa().tbaa_const, hashval);
            return mark_or_box_ccall_result(ctx, hashval, retboxed, rt, unionall, static_rt);
        }
        else if (!val.isboxed) {
            // If the value is not boxed, try to compute the object id without
            // reboxing it.
            auto T_pint8_derived = PointerType::get(getInt8Ty(ctx.builder.getContext()), AddressSpace::Derived);
            if (!val.isghost && !val.ispointer())
                val = value_to_pointer(ctx, val);
            Value *args[] = {
                emit_typeof_boxed(ctx, val),
                val.isghost ? ConstantPointerNull::get(T_pint8_derived) :
                    ctx.builder.CreateBitCast(
                        decay_derived(ctx, data_pointer(ctx, val)),
                        T_pint8_derived)
            };
            Value *ret = ctx.builder.CreateCall(prepare_call(jl_object_id__func), makeArrayRef(args));
            JL_GC_POP();
            return mark_or_box_ccall_result(ctx, ret, retboxed, rt, unionall, static_rt);
        }
    }

    jl_cgval_t retval = sig.emit_a_ccall(
            ctx,
            symarg,
            argv,
            gc_uses,
            static_rt);
    JL_GC_POP();
    return retval;
}

jl_cgval_t function_sig_t::emit_a_ccall(
        jl_codectx_t &ctx,
        const native_sym_arg_t &symarg,
        jl_cgval_t *argv,
        SmallVector<Value*, 16> &gc_uses,
        bool static_rt) const
{
    ++EmittedCCalls;
    if (!err_msg.empty()) {
        emit_error(ctx, err_msg);
        return jl_cgval_t();
    }

    FunctionType *functype = this->functype(ctx.builder.getContext());

    Value **argvals = (Value**) alloca((nccallargs + sret) * sizeof(Value*));
    for (size_t ai = 0; ai < nccallargs; ai++) {
        // Current C function parameter
        jl_cgval_t &arg = argv[ai];
        jl_value_t *jargty = jl_svecref(at, ai); // Julia type of the current parameter
        Type *largty = fargt.at(ai); // LLVM type of the current parameter
        bool toboxed = fargt_isboxed.at(ai);
        Type *pargty = fargt_sig.at(ai + sret); // LLVM coercion type
        bool byRef = byRefList.at(ai); // Argument attributes

        // if we know the function sparams, try to fill those in now
        // so that the julia_to_native type checks are more likely to be doable (e.g. concrete types) at compile-time
        jl_value_t *jargty_in_env = jargty;
        if (ctx.spvals_ptr == NULL && !toboxed && unionall_env && jl_has_typevar_from_unionall(jargty, unionall_env) &&
                jl_svec_len(ctx.linfo->sparam_vals) > 0) {
            jargty_in_env = jl_instantiate_type_in_env(jargty_in_env, unionall_env, jl_svec_data(ctx.linfo->sparam_vals));
            if (jargty_in_env != jargty)
                jl_add_method_root(ctx, jargty_in_env);
        }

        Value *v;
        if (jl_is_abstract_ref_type(jargty)) {
            if (!jl_is_cpointer_type(arg.typ)) {
                emit_cpointercheck(ctx, arg, "ccall: argument to Ref{T} is not a pointer");
                arg.typ = (jl_value_t*)jl_voidpointer_type;
                arg.isboxed = false;
            }
            jargty_in_env = (jl_value_t*)jl_voidpointer_type;
        }

        v = julia_to_native(ctx, largty, toboxed, jargty_in_env, unionall_env, arg, byRef, ai);
        bool issigned = jl_signed_type && jl_subtype(jargty, (jl_value_t*)jl_signed_type);
        if (byRef) {
            v = decay_derived(ctx, v);
            // julia_to_native should already have done the alloca and store
            assert(v->getType() == pargty);
        }
        else {
            v = llvm_type_rewrite(ctx, v, pargty, issigned);
        }

        if (isa<UndefValue>(v)) {
            return jl_cgval_t();
        }
        assert(v->getType() == pargty);
        argvals[ai + sret] = v;
    }

    Value *result = NULL;
    //This is only needed if !retboxed && srt && !jlretboxed
    Type *sretty = nullptr;
    // First, if the ABI requires us to provide the space for the return
    // argument, allocate the box and store that as the first argument type
    bool sretboxed = false;
    if (sret) {
        assert(!retboxed && jl_is_datatype(rt) && "sret return type invalid");
        if (jl_is_pointerfree(rt)) {
            result = emit_static_alloca(ctx, lrt);
            sretty = lrt;
            argvals[0] = ctx.builder.CreateBitCast(result, fargt_sig.at(0));
        }
        else {
            // XXX: result needs to be zero'd and given a GC root here
            // and has incorrect write barriers.
            // instead this code path should behave like `unsafe_load`
            assert(jl_datatype_size(rt) > 0 && "sret shouldn't be a singleton instance");
            result = emit_allocobj(ctx, jl_datatype_size(rt),
                                   literal_pointer_val(ctx, (jl_value_t*)rt));
            sretty = ctx.types().T_jlvalue;
            sretboxed = true;
            gc_uses.push_back(result);
            argvals[0] = ctx.builder.CreateBitCast(emit_pointer_from_objref(ctx, result), fargt_sig.at(0));
        }
    }

    // make LLVM function object for the target
    // keep this close to the function call, so that the compiler can
    // optimize the global pointer load in the common case
    Value *llvmf;
    if (llvmcall) {
        ++EmittedLLVMCalls;
        if (symarg.jl_ptr != NULL) {
            emit_error(ctx, "llvmcall doesn't support dynamic pointers");
            return jl_cgval_t();
        }
        else if (symarg.fptr != NULL) {
            emit_error(ctx, "llvmcall doesn't support static pointers");
            return jl_cgval_t();
        }
        else if (symarg.f_lib != NULL) {
            emit_error(ctx, "llvmcall doesn't support dynamic libraries");
            return jl_cgval_t();
        }
        else {
            assert(symarg.f_name != NULL);
            StringRef f_name(symarg.f_name);
            bool f_extern = f_name.consume_front("extern ");
            llvmf = NULL;
            if (f_extern) {
                llvmf = jl_Module->getOrInsertFunction(f_name, functype).getCallee();
                if (!isa<Function>(llvmf) || cast<Function>(llvmf)->isIntrinsic() || cast<Function>(llvmf)->getFunctionType() != functype)
                    llvmf = NULL;
            }
            else if (f_name.startswith("llvm.")) {
                // compute and verify auto-mangling for intrinsic name
                auto ID = Function::lookupIntrinsicID(f_name);
                if (ID != Intrinsic::not_intrinsic) {
                    // Accumulate an array of overloaded types for the given intrinsic
                    // and compute the new name mangling schema
                    SmallVector<Type*, 4> overloadTys;
                    SmallVector<Intrinsic::IITDescriptor, 8> Table;
                    getIntrinsicInfoTableEntries(ID, Table);
                    ArrayRef<Intrinsic::IITDescriptor> TableRef = Table;
                    auto res = Intrinsic::matchIntrinsicSignature(functype, TableRef, overloadTys);
                    if (res == Intrinsic::MatchIntrinsicTypes_Match) {
                        bool matchvararg = !Intrinsic::matchIntrinsicVarArg(functype->isVarArg(), TableRef);
                        if (matchvararg) {
                            Function *intrinsic = Intrinsic::getDeclaration(jl_Module, ID, overloadTys);
                            assert(intrinsic->getFunctionType() == functype);
                            if (intrinsic->getName() == f_name || Intrinsic::getBaseName(ID) == f_name)
                                llvmf = intrinsic;
                        }
                    }
                }
            }
            if (llvmf == NULL) {
                emit_error(ctx, "llvmcall only supports intrinsic calls");
                return jl_cgval_t();
            }
        }
    }
    else if (symarg.jl_ptr != NULL) {
        ++LiteralCCalls;
        null_pointer_check(ctx, symarg.jl_ptr);
        Type *funcptype = PointerType::get(functype, 0);
        llvmf = emit_inttoptr(ctx, symarg.jl_ptr, funcptype);
    }
    else if (symarg.fptr != NULL) {
        ++LiteralCCalls;
        Type *funcptype = PointerType::get(functype, 0);
        llvmf = literal_static_pointer_val((void*)(uintptr_t)symarg.fptr, funcptype);
        if (ctx.emission_context.imaging)
            jl_printf(JL_STDERR,"WARNING: literal address used in ccall for %s; code cannot be statically compiled\n", symarg.f_name);
    }
    else {
        assert(symarg.f_name != NULL);
        PointerType *funcptype = PointerType::get(functype, 0);
        if (symarg.lib_expr) {
            ++DeferredCCallLookups;
            llvmf = runtime_sym_lookup(ctx, funcptype, NULL, symarg.lib_expr, symarg.f_name, ctx.f);
        }
        else if (ctx.emission_context.imaging) {
            ++DeferredCCallLookups;
            // vararg requires musttail,
            // but musttail is incompatible with noreturn.
            if (functype->isVarArg())
                llvmf = runtime_sym_lookup(ctx, funcptype, symarg.f_lib, NULL, symarg.f_name, ctx.f);
            else
                llvmf = emit_plt(ctx, functype, attributes, cc, symarg.f_lib, symarg.f_name);
        }
        else {
            void *symaddr;
            void *libsym = jl_get_library_(symarg.f_lib, 0);
            if (!libsym || !jl_dlsym(libsym, symarg.f_name, &symaddr, 0)) {
                ++DeferredCCallLookups;
                // either the library or the symbol could not be found, place a runtime
                // lookup here instead.
                llvmf = runtime_sym_lookup(ctx, funcptype, symarg.f_lib, NULL, symarg.f_name, ctx.f);
            } else {
                ++LiteralCCalls;
                // since we aren't saving this code, there's no sense in
                // putting anything complicated here: just JIT the function address
                llvmf = literal_static_pointer_val(symaddr, funcptype);
            }
        }
    }

    OperandBundleDef OpBundle("jl_roots", gc_uses);
    // the actual call
    CallInst *ret = ctx.builder.CreateCall(functype, llvmf,
            ArrayRef<Value*>(&argvals[0], nccallargs + sret),
            ArrayRef<OperandBundleDef>(&OpBundle, gc_uses.empty() ? 0 : 1));
    ((CallInst*)ret)->setAttributes(attributes);

    if (cc != CallingConv::C)
        ((CallInst*)ret)->setCallingConv(cc);
    if (!sret)
        result = ret; // no need to update sretty here because we know !sret
    if (0) { // Enable this to turn on SSPREQ (-fstack-protector) on the function containing this ccall
        ctx.f->addFnAttr(Attribute::StackProtectReq);
    }

    if (rt == jl_bottom_type) {
        CreateTrap(ctx.builder);
        return jl_cgval_t();
    }

    // Finally we need to box the result into julia type
    // However, if we have already created a box for the return
    // type because the ABI required us to pass a pointer (sret),
    // then we do not need to do this.
    bool jlretboxed;
    if (retboxed) {
        assert(!sret);
        jlretboxed = true;
        ++RetBoxedCCalls;
    }
    else if (sret) {
        jlretboxed = sretboxed;
        if (!jlretboxed) {
            // something alloca'd above is SSA
            if (static_rt)
                return mark_julia_slot(result, rt, NULL, ctx.tbaa().tbaa_stack);
            ++SRetCCalls;
            result = ctx.builder.CreateLoad(sretty, result);
        }
    }
    else {
        Type *jlrt = julia_type_to_llvm(ctx, rt, &jlretboxed); // compute the real "julian" return type and compute whether it is boxed
        if (type_is_ghost(jlrt)) {
            return ghostValue(ctx, rt);
        }
        else if (jl_is_datatype(rt) && jl_is_datatype_singleton((jl_datatype_t*)rt)) {
            return mark_julia_const(ctx, ((jl_datatype_t*)rt)->instance);
        }
        else if (jlretboxed && !retboxed) {
            assert(jl_is_datatype(rt));
            if (static_rt) {
                Value *runtime_bt = literal_pointer_val(ctx, rt);
                size_t rtsz = jl_datatype_size(rt);
                assert(rtsz > 0);
                Value *strct = emit_allocobj(ctx, rtsz, runtime_bt);
                MDNode *tbaa = jl_is_mutable(rt) ? ctx.tbaa().tbaa_mutab : ctx.tbaa().tbaa_immut;
                int boxalign = julia_alignment(rt);
                // copy the data from the return value to the new struct
                const DataLayout &DL = ctx.builder.GetInsertBlock()->getModule()->getDataLayout();
                auto resultTy = result->getType();
                if (DL.getTypeStoreSize(resultTy) > rtsz) {
                    // ARM and AArch64 can use a LLVM type larger than the julia type.
                    // When this happens, cast through memory.
                    auto slot = emit_static_alloca(ctx, resultTy);
                    slot->setAlignment(Align(boxalign));
                    ctx.builder.CreateAlignedStore(result, slot, Align(boxalign));
                    emit_memcpy(ctx, strct, tbaa, slot, tbaa, rtsz, boxalign);
                }
                else {
                    init_bits_value(ctx, strct, result, tbaa, boxalign);
                }
                return mark_julia_type(ctx, strct, true, rt);
            }
            jlretboxed = false; // trigger mark_or_box_ccall_result to build the runtime box
        }
        else if (lrt != prt) {
            assert(jlrt == lrt || !lrt->isStructTy()); // julia_type_to_llvm and julia_struct_to_llvm should be returning the same StructType
            result = llvm_type_rewrite(ctx, result, lrt, false);
        }
    }

    return mark_or_box_ccall_result(ctx, result, jlretboxed, rt, unionall_env, static_rt);
}

// Reset us back to codegen debug type
#undef DEBUG_TYPE
#define DEBUG_TYPE "julia_irgen_codegen"
