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
TRANSFORMED_CCALL_STAT(jl_get_ptls_rng);
TRANSFORMED_CCALL_STAT(jl_set_ptls_rng);
TRANSFORMED_CCALL_STAT(jl_get_tls_world_age);
TRANSFORMED_CCALL_STAT(jl_get_world_counter);
TRANSFORMED_CCALL_STAT(jl_gc_enable_disable_finalizers_internal);
TRANSFORMED_CCALL_STAT(jl_get_current_task);
TRANSFORMED_CCALL_STAT(jl_set_next_task);
TRANSFORMED_CCALL_STAT(jl_sigatomic_begin);
TRANSFORMED_CCALL_STAT(jl_sigatomic_end);
TRANSFORMED_CCALL_STAT(jl_string_ptr);
TRANSFORMED_CCALL_STAT(jl_symbol_name);
TRANSFORMED_CCALL_STAT(jl_genericmemory_owner);
TRANSFORMED_CCALL_STAT(jl_alloc_genericmemory);
TRANSFORMED_CCALL_STAT(memcpy);
TRANSFORMED_CCALL_STAT(memset);
TRANSFORMED_CCALL_STAT(memmove);
TRANSFORMED_CCALL_STAT(jl_object_id);
#undef TRANSFORMED_CCALL_STAT
extern "C" JL_DLLEXPORT jl_value_t *ijl_genericmemory_owner(jl_genericmemory_t *m JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;

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
    auto M = &ctx.emission_context.shared_module();
    bool runtime_lib = false;
    GlobalVariable *libptrgv;
    jl_codegen_params_t::SymMapGV *symMap;
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
    else if (f_lib == NULL) {
        libptrgv = jl_emit_RTLD_DEFAULT_var(M);
        symMap = &ctx.emission_context.symMapDefault;
    }
    else {
        std::string name = "ccalllib_";
        name += llvm::sys::path::filename(f_lib);
        name += std::to_string(jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1));
        runtime_lib = true;
        auto &libgv = ctx.emission_context.libMapGV[f_lib];
        if (libgv.first == NULL) {
            libptrgv = new GlobalVariable(*M, getPointerTy(M->getContext()), false,
                                          GlobalVariable::ExternalLinkage,
                                          Constant::getNullValue(getPointerTy(M->getContext())), name);
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
        name += std::to_string(jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1));
        auto T_pvoidfunc = getPointerTy(M->getContext());
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
    auto T_pvoidfunc = getPointerTy(irbuilder.getContext());
    BasicBlock *enter_bb = irbuilder.GetInsertBlock();
    BasicBlock *dlsym_lookup = BasicBlock::Create(irbuilder.getContext(), "dlsym");
    BasicBlock *ccall_bb = BasicBlock::Create(irbuilder.getContext(), "ccall");
    Constant *initnul = ConstantPointerNull::get(T_pvoidfunc);
    LoadInst *llvmf_orig = irbuilder.CreateAlignedLoad(T_pvoidfunc, llvmgv, Align(sizeof(void*)));
    setName(emission_context, llvmf_orig, f_name + StringRef(".cached"));
    // This in principle needs a consume ordering so that load from
    // this pointer sees a valid value. However, this is not supported by
    // LLVM (or agreed on in the C/C++ standard FWIW) and should be
    // almost impossible to happen on every platform we support since this
    // ordering is enforced by the hardware and LLVM has to speculate an
    // invalid load from the `cglobal` but doesn't depend on the `cglobal`
    // value for this to happen.
    llvmf_orig->setAtomic(AtomicOrdering::Unordered);
    auto nonnull = irbuilder.CreateICmpNE(llvmf_orig, initnul);
    setName(emission_context, nonnull, "is_cached");
    irbuilder.CreateCondBr(
            nonnull,
            ccall_bb,
            dlsym_lookup);

    assert(f->getParent() != NULL);
    dlsym_lookup->insertInto(f);
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
            libname = ConstantExpr::getIntToPtr(ConstantInt::get(emission_context.DL.getIntPtrType(irbuilder.getContext()), (uintptr_t)f_lib), getPointerTy(irbuilder.getContext()));
        }
        auto lookup = irbuilder.CreateCall(prepare_call_in(jl_builderModule(irbuilder), jldlsym_func),
                    { libname, nameval, libptrgv });
        llvmf = lookup;
    }
    setName(emission_context, llvmf, f_name + StringRef(".found"));
    StoreInst *store = irbuilder.CreateAlignedStore(llvmf, llvmgv, Align(sizeof(void*)));
    store->setAtomic(AtomicOrdering::Release);
    irbuilder.CreateBr(ccall_bb);

    ccall_bb->insertInto(f);
    irbuilder.SetInsertPoint(ccall_bb);
    PHINode *p = irbuilder.CreatePHI(T_pvoidfunc, 2);
    p->addIncoming(llvmf_orig, enter_bb);
    p->addIncoming(llvmf, llvmf->getParent());
    setName(emission_context, p, f_name);
    return p;
}

static Value *runtime_sym_lookup(
        jl_codectx_t &ctx,
        PointerType *funcptype, const char *f_lib, jl_value_t *lib_expr,
        const char *f_name, Function *f)
{
    auto T_pvoidfunc = getPointerTy(ctx.builder.getContext());
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
        gvname += std::to_string(jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1));
        llvmgv = new GlobalVariable(*jl_Module, T_pvoidfunc, false,
                                    GlobalVariable::ExternalLinkage,
                                    Constant::getNullValue(T_pvoidfunc), gvname);
    }
    else {
        runtime_lib = runtime_sym_gvs(ctx, f_lib, f_name, libptrgv, llvmgv);
        libptrgv = prepare_global_in(jl_Module, libptrgv);
    }
    llvmgv = prepare_global_in(jl_Module, llvmgv);
    return runtime_sym_lookup(ctx.emission_context, ctx.builder, &ctx, funcptype, f_lib, lib_expr, f_name, f, libptrgv, llvmgv, runtime_lib);
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
    auto M = &ctx.emission_context.shared_module();
    PointerType *funcptype = PointerType::get(functype, 0);
    libptrgv = prepare_global_in(M, libptrgv);
    llvmgv = prepare_global_in(M, llvmgv);
    std::string fname;
    raw_string_ostream(fname) << "jlplt_" << f_name << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
    Function *plt = Function::Create(functype,
                                     GlobalVariable::PrivateLinkage,
                                     fname, M);
    plt->setAttributes(attrs);
    if (cc != CallingConv::C)
        plt->setCallingConv(cc);
    auto T_pvoidfunc = getPointerTy(M->getContext());
    GlobalVariable *got = new GlobalVariable(*M, T_pvoidfunc, false,
                                             GlobalVariable::ExternalLinkage,
                                             plt,
                                             fname + "_got");
    if (runtime_lib) {
        got->addAttribute("julia.libname", f_lib);
    } else {
        got->addAttribute("julia.libidx", std::to_string((uintptr_t) f_lib));
    }
    got->addAttribute("julia.fname", f_name);
    BasicBlock *b0 = BasicBlock::Create(M->getContext(), "top", plt);
    IRBuilder<> irbuilder(b0);
    Value *ptr = runtime_sym_lookup(ctx.emission_context, irbuilder, NULL, funcptype, f_lib, NULL, f_name, plt, libptrgv,
                                    llvmgv, runtime_lib);
    StoreInst *store = irbuilder.CreateAlignedStore(ptr, got, Align(sizeof(void*)));
    store->setAtomic(AtomicOrdering::Release);
    SmallVector<Value*, 16> args;
    for (auto &arg : plt->args())
        args.push_back(&arg);
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
        if (ctx.emission_context.TargetTriple.isX86() || (ctx.emission_context.TargetTriple.isAArch64() && !ctx.emission_context.TargetTriple.isOSDarwin())) {
            // Ref https://bugs.llvm.org/show_bug.cgi?id=47058
            // LLVM, as of 10.0.1 emits wrong/worse code when musttail is set
            // Apple silicon macs give an LLVM ERROR if musttail is set here #44107.
            if (!attrs.hasAttrSomewhere(Attribute::ByVal))
                ret->setTailCallKind(CallInst::TCK_MustTail);
        }
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
    // Don't do this for vararg functions so that the `musttail` is only
    // an optimization and is not required to function correctly.
    assert(!functype->isVarArg());
    GlobalVariable *libptrgv;
    GlobalVariable *llvmgv;
    bool runtime_lib = runtime_sym_gvs(ctx, f_lib, f_name, libptrgv, llvmgv);

    auto &pltMap = ctx.emission_context.allPltMap[attrs];
    auto key = std::make_tuple(llvmgv, functype, cc);
    GlobalVariable *&sharedgot = pltMap[key];
    if (!sharedgot) {
        sharedgot = emit_plt_thunk(ctx,
                functype, attrs, cc, f_lib, f_name, libptrgv, llvmgv, runtime_lib);
    }
    GlobalVariable *got = prepare_global_in(jl_Module, sharedgot);
    LoadInst *got_val = ctx.builder.CreateAlignedLoad(got->getValueType(), got, Align(sizeof(void*)));
    setName(ctx.emission_context, got_val, f_name);
    // See comment in `runtime_sym_lookup` above. This in principle needs a
    // consume ordering too. This is even less likely to cause issues though
    // since the only thing we do to this loaded pointer is to call it
    // immediately.
    got_val->setAtomic(AtomicOrdering::Unordered);
    return got_val;
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
#include "abi_riscv.cpp"
#include "abi_ppc64le.cpp"
#include "abi_win32.cpp"
#include "abi_win64.cpp"
#include "abi_x86_64.cpp"
#include "abi_x86.cpp"

#if defined ABI_LLVM
  typedef ABI_LLVMLayout DefaultAbiState;
#elif defined _OS_WINDOWS_
#  if defined _CPU_X86_64_
     typedef ABI_Win64Layout DefaultAbiState;
#  elif defined _CPU_X86_
     typedef ABI_Win32Layout DefaultAbiState;
#  else
#    error Windows is currently only supported on x86 and x86_64
#  endif
#elif defined _CPU_X86_64_
  typedef ABI_x86_64Layout DefaultAbiState;
#elif defined _CPU_X86_
  typedef ABI_x86Layout DefaultAbiState;
#elif defined _CPU_ARM_
  typedef ABI_ARMLayout DefaultAbiState;
#elif defined _CPU_AARCH64_
  typedef ABI_AArch64Layout DefaultAbiState;
#elif defined _CPU_RISCV64_
  typedef ABI_RiscvLayout DefaultAbiState;
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
        return v;

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
    const DataLayout &DL = ctx.builder.GetInsertBlock()->getModule()->getDataLayout();
    Align align = std::max(DL.getPrefTypeAlign(target_type), DL.getPrefTypeAlign(from_type));
    size_t nb = std::max(DL.getTypeAllocSize(target_type), DL.getTypeAllocSize(from_type));
    AllocaInst *cast = emit_static_alloca(ctx, nb, align);
    setName(ctx.emission_context, cast, "type_rewrite_buffer");
    ctx.builder.CreateAlignedStore(v, cast, align);
    auto pun = ctx.builder.CreateAlignedLoad(target_type, cast, align);
    setName(ctx.emission_context, pun, "type_rewrite");
    return pun;
}

// --- argument passing and scratch space utilities ---

// Returns ctx.types().T_prjlvalue
static Value *runtime_apply_type_env(jl_codectx_t &ctx, jl_value_t *ty)
{
    // box if concrete type was not statically known
    Value *args[] = {
        literal_pointer_val(ctx, ty),
        literal_pointer_val(ctx, (jl_value_t*)ctx.linfo->def.method->sig),
        emit_ptrgep(ctx, maybe_decay_tracked(ctx, ctx.spvals_ptr), sizeof(jl_svec_t))
    };
    auto call = ctx.builder.CreateCall(prepare_call(jlapplytype_func), ArrayRef<Value*>(args));
    addRetAttr(call, Attribute::getWithAlignment(ctx.builder.getContext(), Align(16)));
    return call;
}

static const std::string make_errmsg(const char *fname, int n, const char *err)
{
    std::string _msg;
    raw_string_ostream msg(_msg);
    msg << fname;
    if (n > 0) {
        msg << " argument ";
        msg << n;
    } else
        msg << " return";
    msg << err;
    return msg.str();
}

// bitcast whatever Ptr kind x might be (even if it is part of a union) into Ptr{Cvoid}
// given that the caller already had emit_cpointercheck on this branch, so that
// the conversion is guaranteed to be valid on this runtiem branch
static jl_cgval_t voidpointer_update(jl_codectx_t &ctx, const jl_cgval_t &x)
{
    if (x.typ == (jl_value_t*)jl_voidpointer_type)
        return x;
    if (jl_type_intersection(x.typ, (jl_value_t*)jl_pointer_type) == jl_bottom_type)
        return jl_cgval_t();
    if (x.constant)
        return mark_julia_type(ctx, julia_const_to_llvm(ctx, x.constant), false, jl_voidpointer_type);
    if (x.V == nullptr)
        return jl_cgval_t();
    if (!x.inline_roots.empty() || x.ispointer())
        return mark_julia_slot(x.V, (jl_value_t*)jl_voidpointer_type, NULL, x.tbaa);
    return mark_julia_type(ctx, x.V, false, jl_voidpointer_type);
}

static jl_cgval_t typeassert_input(jl_codectx_t &ctx, const jl_cgval_t &jvinfo, jl_value_t *jlto, jl_unionall_t *jlto_env, int argn)
{
    if (jlto != (jl_value_t*)jl_any_type && !jl_subtype(jvinfo.typ, jlto)) {
        if (jlto == (jl_value_t*)jl_voidpointer_type) {
            // allow a bit more flexibility for what can be passed to (void*) due to Ref{T} conversion behavior in input
            if (!jl_is_cpointer_type(jvinfo.typ))
                // emit a typecheck, if not statically known to be correct
                emit_cpointercheck(ctx, jvinfo, make_errmsg("ccall", argn + 1, ""));
            return voidpointer_update(ctx, jvinfo);
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
                setName(ctx.emission_context, istype, "istype");
                BasicBlock *failBB = BasicBlock::Create(ctx.builder.getContext(), "fail", ctx.f);
                BasicBlock *passBB = BasicBlock::Create(ctx.builder.getContext(), "pass", ctx.f);
                ctx.builder.CreateCondBr(istype, passBB, failBB);

                ctx.builder.SetInsertPoint(failBB);
                just_emit_type_error(ctx, mark_julia_type(ctx, vx, true, jl_any_type), boxed(ctx, jlto_runtime), msg);
                ctx.builder.CreateUnreachable();
                ctx.builder.SetInsertPoint(passBB);
            }
            return update_julia_type(ctx, jvinfo, jlto);
        }
    }
    return jvinfo;
}

// Emit code to convert argument to form expected by C ABI
// to = desired LLVM type
// jlto = Julia type of formal argument
// jvinfo = value of actual argument
static Value *julia_to_native(
        jl_codectx_t &ctx,
        Type *to, bool toboxed, jl_value_t *jlto, jl_unionall_t *jlto_env,
        jl_cgval_t jvinfo,
        bool byRef, int argn)
{
    // We're passing Any
    if (toboxed) {
        assert(!byRef); // don't expect any ABI to pass pointers by pointer
        return boxed(ctx, jvinfo);
    }
    assert(jl_is_datatype(jlto) && jl_struct_try_layout((jl_datatype_t*)jlto));

    jvinfo = typeassert_input(ctx, jvinfo, jlto, jlto_env, argn);
    if (!byRef)
        return emit_unbox(ctx, to, jvinfo);

    // pass the address of an alloca'd thing, not a box
    // since those are immutable.
    Align align(julia_alignment(jlto));
    Value *slot = emit_static_alloca(ctx, to, align);
    setName(ctx.emission_context, slot, "native_convert_buffer");
    emit_unbox_store(ctx, jvinfo, slot, ctx.tbaa().tbaa_stack, align, align);
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

static inline const char *invalid_symbol_err_msg(bool ccall)
{
    return ccall ?
        "ccall: first argument not a pointer or valid constant expression" :
        "cglobal: first argument not a pointer or valid constant expression";
}

// --- parse :sym or (:sym, :lib) argument into address info ---
static void interpret_symbol_arg(jl_codectx_t &ctx, native_sym_arg_t &out, jl_value_t *arg, bool ccall, bool llvmcall)
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
            if (!ccall)
                return;
            const char *errmsg = invalid_symbol_err_msg(ccall);
            emit_cpointercheck(ctx, arg1, errmsg);
        }
        arg1 = voidpointer_update(ctx, arg1);
        jl_ptr = emit_unbox(ctx, ctx.types().T_ptr, arg1);
    }
    else if (jl_is_cpointer_type(jl_typeof(ptr))) {
        fptr = *(void(**)(void))jl_data_ptr(ptr);
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
                if (jl_dlsym(jl_libjulia_internal_handle, iname.c_str(), &symaddr, 0, 0)) {
                    f_lib = JL_LIBJULIA_INTERNAL_DL_LIBNAME;
                    f_name = jl_symbol_name(jl_symbol(iname.c_str()));
                }
                else {
                    f_lib = jl_dlfind(f_name);
                }
            }
        }
        else if (jl_is_tuple(ptr) && jl_nfields(ptr) > 1) {
            jl_value_t *t0 = jl_fieldref(ptr, 0);
            if (jl_is_symbol(t0))
                f_name = jl_symbol_name((jl_sym_t*)t0);
            else if (jl_is_string(t0))
                f_name = jl_string_data(t0);

            jl_value_t *t1 = jl_fieldref(ptr, 1);
            if (jl_is_symbol(t1))
                f_lib = jl_symbol_name((jl_sym_t*)t1);
            else if (jl_is_string(t1))
                f_lib = jl_string_data(t1);
            else {
                out.lib_expr = t1;
            }
        }
    }
}

// --- code generator for cglobal ---

static jl_cgval_t emit_runtime_call(jl_codectx_t &ctx, JL_I::intrinsic f, ArrayRef<jl_cgval_t> argv, size_t nargs);

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
            jl_cgval_t argv[2];
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
    Type *lrt = ctx.types().T_ptr;
    assert(lrt == julia_type_to_llvm(ctx, rt));

    interpret_symbol_arg(ctx, sym, args[1], /*ccall=*/false, false);

    if (sym.jl_ptr != NULL) {
        res = sym.jl_ptr;
    }
    else if (sym.fptr != NULL) {
        res = ConstantInt::get(lrt, (uint64_t)sym.fptr);
    }
    else if (sym.f_name != NULL) {
        if (sym.lib_expr) {
            res = runtime_sym_lookup(ctx, getPointerTy(ctx.builder.getContext()), NULL, sym.lib_expr, sym.f_name, ctx.f);
        }
        else {
            res = runtime_sym_lookup(ctx, getPointerTy(ctx.builder.getContext()), sym.f_lib, NULL, sym.f_name, ctx.f);
        }
    } else {
        // Fall back to runtime intrinsic
        JL_GC_POP();
        jl_cgval_t argv[2];
        argv[0] = emit_expr(ctx, args[1]);
        if (nargs == 2)
            argv[1] = emit_expr(ctx, args[2]);
        return emit_runtime_call(ctx, nargs == 1 ? JL_I::cglobal_auto : JL_I::cglobal, argv, nargs);
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
        ir_arg = jl_array_ptr_ref((jl_array_t*)ctx.source->code, ((jl_ssavalue_t*)ir_arg)->id - 1);
    ir = static_eval(ctx, ir_arg);
    if (!ir) {
        emit_error(ctx, "error statically evaluating llvm IR argument");
        JL_GC_POP();
        return jl_cgval_t();
    }
    if (jl_is_ssavalue(args[2]) && !jl_is_long(ctx.source->ssavaluetypes)) {
        jl_value_t *rtt = jl_array_ptr_ref((jl_array_t*)ctx.source->ssavaluetypes, ((jl_ssavalue_t*)args[2])->id - 1);
        if (jl_is_type_type(rtt))
            rt = jl_tparam0(rtt);
    }
    if (!rt) {
        rt = static_eval(ctx, args[2]);
        if (!rt) {
            emit_error(ctx, "error statically evaluating llvmcall return type");
            JL_GC_POP();
            return jl_cgval_t();
        }
    }
    if (jl_is_ssavalue(args[3]) && !jl_is_long(ctx.source->ssavaluetypes)) {
        jl_value_t *att = jl_array_ptr_ref((jl_array_t*)ctx.source->ssavaluetypes, ((jl_ssavalue_t*)args[3])->id - 1);
        if (jl_is_type_type(att))
            at = jl_tparam0(att);
    }
    if (!at) {
        at = static_eval(ctx, args[3]);
        if (!at) {
            emit_error(ctx, "error statically evaluating llvmcall argument tuple");
            JL_GC_POP();
            return jl_cgval_t();
        }
    }
    if (jl_is_tuple(ir)) {
        // if the IR is a tuple, we expect (mod, fn)
        if (jl_nfields(ir) != 2) {
            emit_error(ctx, "Tuple as first argument to llvmcall must have exactly two children");
            JL_GC_POP();
            return jl_cgval_t();
        }
        entry = jl_fieldref(ir, 1);
        if (!jl_is_string(entry)) {
            emit_error(ctx, "Function name passed to llvmcall must be a string");
            JL_GC_POP();
            return jl_cgval_t();
        }
        ir = jl_fieldref(ir, 0);

        if (!jl_is_string(ir) && !jl_typetagis(ir, jl_array_uint8_type)) {
            emit_error(ctx, "Module IR passed to llvmcall must be a string or an array of bytes");
            JL_GC_POP();
            return jl_cgval_t();
        }
    }
    else {
        if (!jl_is_string(ir)) {
            emit_error(ctx, "Function IR passed to llvmcall must be a string");
            JL_GC_POP();
            return jl_cgval_t();
        }
    }

    JL_TYPECHK(llvmcall, type, rt);
    JL_TYPECHK(llvmcall, type, at);

    // Determine argument types
    //
    // Semantics for arguments are as follows:
    // If the argument type is immutable (including bitstype), we pass the loaded llvm value
    // type. Otherwise we pass a pointer to a jl_value_t.
    jl_svec_t *tt = ((jl_datatype_t *)at)->parameters;
    size_t nargt = jl_svec_len(tt);
    SmallVector<llvm::Type*, 0> argtypes;
    SmallVector<Value *, 8> argvals(nargt);
    for (size_t i = 0; i < nargt; ++i) {
        jl_value_t *tti = jl_svecref(tt,i);
        bool toboxed;
        Type *t = julia_type_to_llvm(ctx, tti, &toboxed);
        argtypes.push_back(t);
        if (4 + i > nargs) {
            emit_error(ctx, "Missing arguments to llvmcall!");
            JL_GC_POP();
            return jl_cgval_t();
        }
        jl_value_t *argi = args[4 + i];
        jl_cgval_t arg = emit_expr(ctx, argi);

        Value *v = julia_to_native(ctx, t, toboxed, tti, NULL, arg, false, i);
        bool issigned = jl_signed_type && jl_subtype(tti, (jl_value_t*)jl_signed_type);
        argvals[i] = llvm_type_rewrite(ctx, v, t, issigned);
    }

    // Determine return type
    jl_value_t *rtt = rt;
    bool retboxed;
    Type *rettype = julia_type_to_llvm(ctx, rtt, &retboxed);

    // Make sure to find a unique name
    std::string ir_name;
    while (true) {
        raw_string_ostream(ir_name)
            << (ctx.f->getName().str()) << "u"
            << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
        if (jl_Module->getFunction(ir_name) == NULL)
            break;
    }

    // generate a temporary module that contains our IR
    std::unique_ptr<Module> Mod;
    bool shouldDiscardValueNames = ctx.builder.getContext().shouldDiscardValueNames();
    Function *f;
    if (entry == NULL) {
        // we only have function IR, which we should put in a function

        // stringify arguments
        std::string arguments;
        raw_string_ostream argstream(arguments);
        for (SmallVector<Type *, 0>::iterator it = argtypes.begin(); it != argtypes.end(); ++it) {
            if (it != argtypes.begin())
                argstream << ",";
            (*it)->print(argstream);
            argstream << " ";
        }

        // stringify return type
        std::string rstring;
        raw_string_ostream rtypename(rstring);
        rettype->print(rtypename);

        // generate IR function definition
        std::string ir_string;
        raw_string_ostream ir_stream(ir_string);
        ir_stream << "define " << rtypename.str() << " @\"" << ir_name << "\"("
                  << argstream.str() << ") {\n"
                  << jl_string_data(ir) << "\n}";

        SMDiagnostic Err = SMDiagnostic();
        ctx.builder.getContext().setDiscardValueNames(false);
        Mod = parseAssemblyString(ir_stream.str(), Err, ctx.builder.getContext());
        ctx.builder.getContext().setDiscardValueNames(shouldDiscardValueNames);

        // backwards compatibility: support for IR with integer pointers
        if (!Mod) {
            std::string compat_arguments;
            raw_string_ostream compat_argstream(compat_arguments);
            for (size_t i = 0; i < nargt; ++i) {
                if (i > 0)
                    compat_argstream << ",";
                jl_value_t *tti = jl_svecref(tt, i);
                Type *t;
                if (jl_is_cpointer_type(tti))
                    t = ctx.types().T_size;
                else
                    t = argtypes[i];
                t->print(compat_argstream);
                compat_argstream << " ";
            }

            std::string compat_rstring;
            raw_string_ostream compat_rtypename(compat_rstring);
            if (jl_is_cpointer_type(rtt))
                ctx.types().T_size->print(compat_rtypename);
            else
                rettype->print(compat_rtypename);

            std::string compat_ir_string;
            raw_string_ostream compat_ir_stream(compat_ir_string);
            compat_ir_stream << "define " << compat_rtypename.str() << " @\"" << ir_name
                             << "\"(" << compat_argstream.str() << ") {\n"
                             << jl_string_data(ir) << "\n}";

            SMDiagnostic Err = SMDiagnostic();
            ctx.builder.getContext().setDiscardValueNames(false);
            Mod = parseAssemblyString(compat_ir_stream.str(), Err, ctx.builder.getContext());
            ctx.builder.getContext().setDiscardValueNames(shouldDiscardValueNames);
        }

        if (!Mod) {
            std::string message = "Failed to parse LLVM assembly: \n";
            raw_string_ostream stream(message);
            Err.print("", stream, true);
            emit_error(ctx, stream.str());
            JL_GC_POP();
            return jl_cgval_t();
        }

        f = Mod->getFunction(ir_name);
        f->addFnAttr(Attribute::AlwaysInline);
    }
    else {
        // we have the IR or bitcode of an entire module, which we can parse directly

        if (jl_is_string(ir)) {
            SMDiagnostic Err = SMDiagnostic();
            ctx.builder.getContext().setDiscardValueNames(false);
            Mod = parseAssemblyString(jl_string_data(ir), Err, ctx.builder.getContext());
            ctx.builder.getContext().setDiscardValueNames(shouldDiscardValueNames);
            if (!Mod) {
                std::string message = "Failed to parse LLVM assembly: \n";
                raw_string_ostream stream(message);
                Err.print("", stream, true);
                emit_error(ctx, stream.str());
                JL_GC_POP();
                return jl_cgval_t();
            }
        }
        else {
            auto Buf = MemoryBuffer::getMemBuffer(
                StringRef(jl_array_data(ir, char), jl_array_nrows(ir)), "llvmcall",
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
                JL_GC_POP();
                return jl_cgval_t();
            }
            Mod = std::move(ModuleOrErr.get());
        }

        f = Mod->getFunction(jl_string_data(entry));
        if (!f) {
            emit_error(ctx, "Module IR does not contain specified entry function");
            JL_GC_POP();
            return jl_cgval_t();
        }
        assert(!f->isDeclaration());
        f->setName(ir_name);
    }

    // backwards compatibility: support for IR with integer pointers
    bool mismatched_pointers = false;
    for (size_t i = 0; i < nargt; ++i) {
        jl_value_t *tti = jl_svecref(tt, i);
        if (jl_is_cpointer_type(tti) &&
            !f->getFunctionType()->getParamType(i)->isPointerTy()) {
            mismatched_pointers = true;
            break;
        }
    }
    if (mismatched_pointers) {
        if (jl_options.depwarn) {
            if (jl_options.depwarn == JL_OPTIONS_DEPWARN_ERROR)
                jl_error("llvmcall with integer pointers is deprecated, "
                         "use an actual pointer type instead.");

            // ensure we only depwarn once per method
            // TODO: lift this into a reusable codegen-level depwarn utility
            static std::set<jl_method_t*> llvmcall_depwarns;
            jl_method_t *m = ctx.linfo->def.method;
            if (llvmcall_depwarns.find(m) == llvmcall_depwarns.end()) {
                llvmcall_depwarns.insert(m);
                jl_printf(JL_STDERR,
                        "WARNING: llvmcall with integer pointers is deprecated.\n"
                        "Use actual pointers instead, replacing i32 or i64 with i8* or ptr\n"
                        "in ");
                jl_static_show(JL_STDERR, (jl_value_t*) ctx.linfo->def.method);
                jl_printf(JL_STDERR, " at %s\n", ctx.file.str().c_str());
            }
        }

        // wrap the function, performing the necessary pointer conversion

        Function *inner = f;
        inner->setName(ir_name + ".inner");

        FunctionType *wrapper_ft = FunctionType::get(rettype, argtypes, false);
        Function *wrapper =
            Function::Create(wrapper_ft, inner->getLinkage(), ir_name, *Mod);

        wrapper->copyAttributesFrom(inner);
        inner->addFnAttr(Attribute::AlwaysInline);

        BasicBlock *entry = BasicBlock::Create(ctx.builder.getContext(), "", wrapper);
        IRBuilder<> irbuilder(entry);
        SmallVector<Value *, 0> wrapper_args;
        for (size_t i = 0; i < nargt; ++i) {
            jl_value_t *tti = jl_svecref(tt, i);
            Value *v = wrapper->getArg(i);
            if (jl_is_cpointer_type(tti))
                v = irbuilder.CreatePtrToInt(v, ctx.types().T_size);
            wrapper_args.push_back(v);
        }
        Value *call = irbuilder.CreateCall(inner, wrapper_args);
        // check if void
        if (rettype->isVoidTy())
            irbuilder.CreateRetVoid();
        else {
            if (jl_is_cpointer_type(rtt))
                call = irbuilder.CreateIntToPtr(call, ctx.types().T_ptr);
            irbuilder.CreateRet(call);
        }

        f = wrapper;
    }

    // verify the function type
    assert(f->getReturnType() == rettype);
    int i = 0;
    for (SmallVector<Type *, 0>::iterator it = argtypes.begin(); it != argtypes.end();
         ++it, ++i) {
        if (*it != f->getFunctionType()->getParamType(i)) {
            std::string message;
            raw_string_ostream stream(message);
            stream << "Malformed llvmcall: argument " << i + 1 << " type "
                   << *f->getFunctionType()->getParamType(i)
                   << " does not match expected argument type " << **it;
            emit_error(ctx, stream.str());
            return jl_cgval_t();
        }
    }

    // copy module properties that should always match
    Mod->setTargetTriple(jl_Module->getTargetTriple());
    Mod->setDataLayout(jl_Module->getDataLayout());
    Mod->setStackProtectorGuard(jl_Module->getStackProtectorGuard());
    Mod->setOverrideStackAlignment(jl_Module->getOverrideStackAlignment());

    // verify the definition
    Function *def = Mod->getFunction(ir_name);
    assert(def);
    std::string message = "Malformed LLVM function: \n";
    raw_string_ostream stream(message);
    if (verifyFunction(*def, &stream)) {
        emit_error(ctx, stream.str());
        JL_GC_POP();
        return jl_cgval_t();
    }
    def->setLinkage(GlobalVariable::LinkOnceODRLinkage);

    // generate a call
    FunctionType *decl_typ = FunctionType::get(rettype, argtypes, def->isVarArg());
    Function *decl = Function::Create(decl_typ, def->getLinkage(), def->getAddressSpace(),
                                      def->getName(), jl_Module);
    decl->setAttributes(def->getAttributes());
    CallInst *inst = ctx.builder.CreateCall(decl, argvals);

    // save the module to be linked later.
    // we cannot do this right now, because linking mutates the destination module,
    // which might invalidate LLVM values cached in cgval_t's (specifically constant arrays)
    ctx.llvmcall_modules.push_back(std::move(Mod));

    JL_GC_POP();

    if (inst->getType() != rettype) {
        std::string message;
        raw_string_ostream stream(message);
        stream << "Malformed llvmcall: return type " << *inst->getType()
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
    unsigned align = sizeof(void*); // Allocations are at least pointer aligned
    MDNode *tbaa = jl_is_mutable(rt) ? ctx.tbaa().tbaa_mutab : ctx.tbaa().tbaa_immut;
    Value *strct = emit_allocobj(ctx, nb, runtime_dt, true, align);
    setName(ctx.emission_context, strct, "ccall_result_box");
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
    SmallVector<Type*, 0> fargt; // vector of llvm output types (julia_struct_to_llvm) for arguments
    SmallVector<Type*, 0> fargt_sig; // vector of ABI coercion types for call signature
    SmallVector<bool, 0> fargt_isboxed; // vector of whether the llvm output type is a Julia-box for each argument
    SmallVector<bool, 0> byRefList; // vector of "byref" parameters
    AttributeList attributes; // vector of function call site attributes
    Type *lrt; // input parameter of the llvm return type (from julia_struct_to_llvm)
    bool retboxed; // input parameter indicating whether lrt is jl_value_t*
    bool gc_safe; // input parameter indicating whether the call is safe to execute concurrently to GC
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

    function_sig_t(const char *fname, Type *lrt, jl_value_t *rt, bool retboxed, bool gc_safe, jl_svec_t *at, jl_unionall_t *unionall_env, size_t nreqargs, CallingConv::ID cc, bool llvmcall, jl_codegen_params_t *ctx)
      : lrt(lrt), retboxed(retboxed), gc_safe(gc_safe),
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
            return FunctionType::get(sret ? getVoidTy(ctxt) : prt, ArrayRef<Type*>(fargt_sig).slice(0, nreqargs), true);
        else
            return FunctionType::get(sret ? getVoidTy(ctxt) : prt, fargt_sig, false);
    }

    jl_cgval_t emit_a_ccall(
            jl_codectx_t &ctx,
            const native_sym_arg_t &symarg,
            jl_cgval_t *argv,
            SmallVectorImpl<Value*> &gc_uses,
            bool static_rt) const;

private:
std::string generate_func_sig(const char *fname)
{
    assert(rt && !jl_is_abstract_ref_type(rt));

    SmallVector<AttributeSet, 0> paramattrs;
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
            AttrBuilder retattrs(LLVMCtx);
            if (!ctx->TargetTriple.isOSWindows()) {
                // llvm used to use the old mingw ABI, skipping this marking works around that difference
                retattrs.addStructRetAttr(lrt);
            }
            retattrs.addAttribute(Attribute::NoAlias);
            paramattrs.push_back(AttributeSet::get(LLVMCtx, retattrs));
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
        AttrBuilder ab(LLVMCtx);
        jl_value_t *tti = jl_svecref(at, i);
        Type *t = NULL;
        bool isboxed;
        if (jl_is_abstract_ref_type(tti)) {
            tti = (jl_value_t*)jl_voidpointer_type;
            t = getPointerTy(LLVMCtx);
            isboxed = false;
        }
        else if (llvmcall && jl_is_llvmpointer_type(tti)) {
            t = bitstype_to_llvm(tti, LLVMCtx, true);
            tti = (jl_value_t*)jl_voidpointer_type;
            isboxed = false;
        }
        else {
            t = _julia_struct_to_llvm(ctx, LLVMCtx, tti, &isboxed, llvmcall);
            if (t == getVoidTy(LLVMCtx)) {
                return make_errmsg(fname, i + 1, " type doesn't correspond to a C type");
            }
            if (jl_is_primitivetype(tti) && t->isIntegerTy()) {
                // see pull req #978. need to annotate signext/zeroext for
                // small integer arguments.
                jl_datatype_t *bt = (jl_datatype_t*)tti;
                if (jl_datatype_size(bt) < 4) {
                    if (jl_signed_type && jl_subtype(tti, (jl_value_t*)jl_signed_type))
                        ab.addAttribute(Attribute::SExt);
                    else
                        ab.addAttribute(Attribute::ZExt);
                }
            }
        }

        Type *pat;
        // n.b. `Array` used as argument type just passes a julia object reference
        if (!jl_is_datatype(tti) || ((jl_datatype_t*)tti)->layout == NULL || jl_is_array_type(tti) || jl_is_layout_opaque(((jl_datatype_t*)tti)->layout)) {
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
        paramattrs.push_back(AttributeSet::get(LLVMCtx, ab));
    }

    AttributeSet FnAttrs;
    AttributeSet RetAttrs;
    // If return value is boxed it must be non-null.
    if (retboxed)
        RetAttrs = RetAttrs.addAttribute(LLVMCtx, Attribute::NonNull);
    if (rt == jl_bottom_type)
        FnAttrs = FnAttrs.addAttribute(LLVMCtx, Attribute::NoReturn);

    assert(attributes.isEmpty());
    attributes = AttributeList::get(LLVMCtx, FnAttrs, RetAttrs, paramattrs);
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
                        setName(ctx.emission_context, notany, "any_type.not");
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

    if (rt == (jl_value_t*)jl_any_type || jl_is_array_type(rt) || jl_is_genericmemory_type(rt) ||
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

// Expr(:foreigncall, pointer, rettype, (argtypes...), nreq, gc_safe, [cconv | (cconv, effects)], args..., roots...)
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
    bool gc_safe = false;
    if (jl_is_symbol(jlcc)) {
        cc_sym = (jl_sym_t*)jlcc;
    }
    else if (jl_is_tuple(jlcc)) {
        cc_sym = (jl_sym_t*)jl_get_nth_field_noalloc(jlcc, 0);
        gc_safe = jl_unbox_bool(jl_get_nth_field_checked(jlcc, 2));
    }
    assert(jl_is_symbol(cc_sym));
    native_sym_arg_t symarg = {};
    JL_GC_PUSH3(&rt, &at, &symarg.gcroot);

    CallingConv::ID cc = CallingConv::C;
    bool llvmcall = false;
    std::tie(cc, llvmcall) = convert_cconv(cc_sym);

    interpret_symbol_arg(ctx, symarg, args[1], /*ccall=*/true, llvmcall);
    Value *&jl_ptr = symarg.jl_ptr;
    void (*&fptr)(void) = symarg.fptr;
    const char *&f_name = symarg.f_name;
    const char *&f_lib = symarg.f_lib;

    if (f_name == NULL && fptr == NULL && jl_ptr == NULL) {
        if (symarg.gcroot != NULL) { // static_eval(ctx, args[1]) could not be interpreted to a function pointer
            const char *errmsg = invalid_symbol_err_msg(/*ccall=*/true);
            jl_cgval_t arg1 = emit_expr(ctx, args[1]);
            emit_type_error(ctx, arg1, literal_pointer_val(ctx, (jl_value_t *)jl_pointer_type), errmsg);
        } else {
            emit_error(ctx, "ccall: null function pointer");
        }
        JL_GC_POP();
        return jl_cgval_t();
    }

    auto _is_libjulia_func = [&] (uintptr_t ptr, StringRef name) {
        if ((uintptr_t)fptr == ptr)
            return true;
        if (f_lib) {
            if ((f_lib == JL_EXE_LIBNAME) || // preventing invalid pointer access
                (f_lib == JL_LIBJULIA_INTERNAL_DL_LIBNAME) ||
                (f_lib == JL_LIBJULIA_DL_LIBNAME)) {
                // libjulia-like
            }
            else
#ifdef _OS_WINDOWS_
            if (strcmp(f_lib, jl_crtdll_basename) == 0) {
                // libjulia-like
            }
            else
#endif
            return false;
        }
        return f_name && f_name == name;
    };
#define is_libjulia_func(name) _is_libjulia_func((uintptr_t)&(name), StringRef(XSTR(name)))

    // emit arguments
    SmallVector<jl_cgval_t, 4> argv(nccallargs);
    for (size_t i = 0; i < nccallargs; i++) {
        // Julia (expression) value of current parameter
        assert(i < nccallargs && i + fc_args_start <= nargs);
        jl_value_t *argi = args[fc_args_start + i];
        argv[i] = emit_expr(ctx, argi);
        if (argv[i].typ == jl_bottom_type) {
            JL_GC_POP();
            return jl_cgval_t();
        }
    }

    // emit roots
    SmallVector<Value*> gc_uses;
    for (size_t i = nccallargs + fc_args_start; i <= nargs; i++) {
        // Julia (expression) value of current parameter gcroot
        jl_value_t *argi_root = args[i];
        if (jl_is_long(argi_root))
            continue;
        jl_cgval_t arg_root = emit_expr(ctx, argi_root);
        gc_uses.append(get_gc_roots_for(ctx, arg_root));
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
        jl_temporary_root(ctx, rt);
    function_sig_t sig("ccall", lrt, rt, retboxed, gc_safe,
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
    if (is_libjulia_func(jl_value_ptr)) {
        ++CCALL_STAT(jl_value_ptr);
        assert(retboxed ? lrt == ctx.types().T_prjlvalue : lrt == ctx.types().T_ptr);
        assert(!isVa && !llvmcall && nccallargs == 1);
        jl_value_t *tti = jl_svecref(at, 0);
        Type *largty;
        bool isboxed;
        if (jl_is_abstract_ref_type(tti)) {
            tti = (jl_value_t*)jl_voidpointer_type;
            largty = ctx.types().T_ptr;
            isboxed = false;
        }
        else {
            largty = _julia_struct_to_llvm(&ctx.emission_context, ctx.builder.getContext(), tti, &isboxed, llvmcall);
        }
        Value *retval;
        if (isboxed) {
            retval = boxed(ctx, argv[0]);
            retval = emit_pointer_from_objref(ctx, retval /*T_prjlvalue*/);
        }
        else if (tti == (jl_value_t*)jl_voidpointer_type) {
            retval = emit_unbox(ctx, largty, voidpointer_update(ctx, argv[0]));
        }
        else {
            retval = emit_unbox(ctx, largty, update_julia_type(ctx, argv[0], tti));
        }
        // retval is now an untracked jl_value_t*
        if (retboxed)
            // WARNING: this addrspace cast necessarily implies that the value is rooted elsewhere!
            retval = ctx.builder.CreateAddrSpaceCast(retval, ctx.types().T_prjlvalue);
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, retval, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_cpu_pause)||is_libjulia_func(jl_cpu_suspend)) {
        ++CCALL_STAT(jl_cpu_pause);
        // Keep in sync with the julia_threads.h version
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
#ifdef __MIC__
    //TODO
#else
        if (ctx.emission_context.TargetTriple.isX86()) {
            auto pauseinst = InlineAsm::get(FunctionType::get(getVoidTy(ctx.builder.getContext()), false), "pause",
                                                "~{memory}", true);
            ctx.builder.CreateCall(pauseinst);
            JL_GC_POP();
            return ghostValue(ctx, jl_nothing_type);
        } else if (ctx.emission_context.TargetTriple.isAArch64()
                    || (ctx.emission_context.TargetTriple.isARM()
                        && ctx.emission_context.TargetTriple.getSubArch() != Triple::SubArchType::NoSubArch
                        // ARMv7 and above is < armv6
                        && ctx.emission_context.TargetTriple.getSubArch() < Triple::SubArchType::ARMSubArch_v6)) {
            InlineAsm* wait_inst;
            if (is_libjulia_func(jl_cpu_pause))
                wait_inst = InlineAsm::get(FunctionType::get(getVoidTy(ctx.builder.getContext()), false), "isb",
                                                "~{memory}", true);
            else
                wait_inst = InlineAsm::get(FunctionType::get(getVoidTy(ctx.builder.getContext()), false), "wfe",
                                                "~{memory}", true);
            ctx.builder.CreateCall(wait_inst);
            JL_GC_POP();
            return ghostValue(ctx, jl_nothing_type);
        } else {
            JL_GC_POP();
            return ghostValue(ctx, jl_nothing_type);
        }
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
#endif
        if (ctx.emission_context.TargetTriple.isAArch64()
            || (ctx.emission_context.TargetTriple.isARM()
                && ctx.emission_context.TargetTriple.getSubArch() != Triple::SubArchType::NoSubArch
                // ARMv7 and above is < armv6
                && ctx.emission_context.TargetTriple.getSubArch() < Triple::SubArchType::ARMSubArch_v6)) {
            auto sevinst = InlineAsm::get(FunctionType::get(getVoidTy(ctx.builder.getContext()), false), "sev",
                                                "~{memory}", true);
            ctx.builder.CreateCall(sevinst);
            JL_GC_POP();
            return ghostValue(ctx, jl_nothing_type);
        }
    }
    else if (is_libjulia_func(jl_gc_safepoint)) {
        ++CCALL_STAT(jl_gc_safepoint);
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        ctx.builder.CreateCall(prepare_call(gcroot_flush_func));
        emit_gc_safepoint(ctx.builder, ctx.types().T_size, get_current_ptls(ctx), ctx.tbaa().tbaa_const);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func(jl_get_ptls_states)) {
        ++CCALL_STAT(jl_get_ptls_states);
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, get_current_ptls(ctx), retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_threadid)) {
        ++CCALL_STAT(jl_threadid);
        assert(lrt == getInt16Ty(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        Value *ptask = get_current_task(ctx);
        const int tid_offset = offsetof(jl_task_t, tid);
        Value *ptid = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), ptask, ConstantInt::get(ctx.types().T_size, tid_offset / sizeof(int8_t)));
        setName(ctx.emission_context, ptid, "thread_id_ptr");
        LoadInst *tid = ctx.builder.CreateAlignedLoad(getInt16Ty(ctx.builder.getContext()), ptid, Align(sizeof(int16_t)));
        setName(ctx.emission_context, tid, "thread_id");
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
        ai.decorateInst(tid);
        return mark_or_box_ccall_result(ctx, tid, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_get_ptls_rng)) {
        ++CCALL_STAT(jl_get_ptls_rng);
        assert(lrt == getInt64Ty(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        Value *ptls_p = get_current_ptls(ctx);
        const int rng_offset = offsetof(jl_tls_states_t, rngseed);
        Value *rng_ptr = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), ptls_p, ConstantInt::get(ctx.types().T_size, rng_offset / sizeof(int8_t)));
        setName(ctx.emission_context, rng_ptr, "rngseed_ptr");
        LoadInst *rng_value = ctx.builder.CreateAlignedLoad(getInt64Ty(ctx.builder.getContext()), rng_ptr, Align(sizeof(void*)));
        setName(ctx.emission_context, rng_value, "rngseed");
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
        ai.decorateInst(rng_value);
        return mark_or_box_ccall_result(ctx, rng_value, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_set_ptls_rng)) {
        ++CCALL_STAT(jl_set_ptls_rng);
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 1);
        JL_GC_POP();
        Value *ptls_p = get_current_ptls(ctx);
        const int rng_offset = offsetof(jl_tls_states_t, rngseed);
        Value *rng_ptr = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), ptls_p, ConstantInt::get(ctx.types().T_size, rng_offset / sizeof(int8_t)));
        setName(ctx.emission_context, rng_ptr, "rngseed_ptr");
        Value *val64 = emit_unbox(ctx, getInt64Ty(ctx.builder.getContext()), update_julia_type(ctx, argv[0], (jl_value_t*)jl_uint64_type));
        auto store = ctx.builder.CreateAlignedStore(val64, rng_ptr, Align(sizeof(void*)));
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
        ai.decorateInst(store);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func(jl_get_tls_world_age)) {
        ++CCALL_STAT(jl_get_tls_world_age);
        assert(lrt == ctx.types().T_size);
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        Value *world_age = get_tls_world_age(ctx);
        return mark_or_box_ccall_result(ctx, world_age, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_get_world_counter)) {
        ++CCALL_STAT(jl_get_world_counter);
        assert(lrt == ctx.types().T_size);
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);

        // jl_task_t *ct = jl_current_task;
        // if (ct->ptls->in_pure_callback)
        //     return ~(size_t)0;
        // return jl_atomic_load_acquire(&jl_world_counter);
        Type *T_int16 = getInt16Ty(ctx.builder.getContext());
        Value *offset = ConstantInt::get(ctx.types().T_size, offsetof(jl_tls_states_t, in_pure_callback) / sizeof(int16_t));
        Value *field_ptr = ctx.builder.CreateInBoundsGEP(T_int16, get_current_ptls(ctx), offset);
        Instruction *in_pure_callback = ai.decorateInst(ctx.builder.CreateAlignedLoad(T_int16,
            field_ptr, Align(sizeof(int16_t)), "in_pure_callback"));
        Value *cond = ctx.builder.CreateICmpEQ(in_pure_callback, ConstantInt::get(T_int16, 0));

        Value *world_counter = ctx.builder.CreateAlignedLoad(ctx.types().T_size,
            prepare_global_in(jl_Module, jlgetworld_global), ctx.types().alignof_ptr);
        cast<LoadInst>(world_counter)->setOrdering(AtomicOrdering::Acquire);
        Value *ret = ctx.builder.CreateSelect(cond, world_counter, ConstantInt::get(ctx.types().T_size, ~(size_t)0));
        return mark_or_box_ccall_result(ctx, ret, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_gc_disable_finalizers_internal)
#ifdef NDEBUG
             || is_libjulia_func(jl_gc_enable_finalizers_internal)
#endif
             ) {
        JL_GC_POP();
        Value *ptls_p = get_current_ptls(ctx);
        const int finh_offset = offsetof(jl_tls_states_t, finalizers_inhibited);
        Value *pfinh = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), ptls_p, ConstantInt::get(ctx.types().T_size, finh_offset / sizeof(int8_t)));
        setName(ctx.emission_context, pfinh, "finalizers_inhibited_ptr");
        LoadInst *finh = ctx.builder.CreateAlignedLoad(getInt32Ty(ctx.builder.getContext()), pfinh, Align(sizeof(int32_t)));
        setName(ctx.emission_context, finh, "finalizers_inhibited");
        Value *newval;
        if (is_libjulia_func(jl_gc_disable_finalizers_internal)) {
            newval = ctx.builder.CreateAdd(finh, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 1));
            setName(ctx.emission_context, newval, "finalizers_inhibited_inc");
        }
        else {
            newval = ctx.builder.CreateSelect(ctx.builder.CreateICmpEQ(finh, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0)),
                                              ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0),
                                              ctx.builder.CreateSub(finh, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 1)));
            setName(ctx.emission_context, newval, "finalizers_inhibited_dec");
        }
        ctx.builder.CreateStore(newval, pfinh);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func(jl_get_current_task)) {
        ++CCALL_STAT(jl_get_current_task);
        assert(lrt == ctx.types().T_prjlvalue);
        assert(!isVa && !llvmcall && nccallargs == 0);
        JL_GC_POP();
        auto ct = track_pjlvalue(ctx, get_current_task(ctx));
        return mark_or_box_ccall_result(ctx, ct, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_set_next_task)) {
        ++CCALL_STAT(jl_set_next_task);
        assert(lrt == getVoidTy(ctx.builder.getContext()));
        assert(!isVa && !llvmcall && nccallargs == 1);
        JL_GC_POP();
        Value *ptls_pv = get_current_ptls(ctx);
        const int nt_offset = offsetof(jl_tls_states_t, next_task);
        Value *pnt = ctx.builder.CreateInBoundsGEP(ctx.types().T_pjlvalue, ptls_pv, ConstantInt::get(ctx.types().T_size, nt_offset / sizeof(void*)));
        setName(ctx.emission_context, pnt, "next_task_ptr");
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
        setName(ctx.emission_context, pdefer_sig, "defer_signal_ptr");
        Value *defer_sig = ctx.builder.CreateLoad(ctx.types().T_sigatomic, pdefer_sig);
        setName(ctx.emission_context, defer_sig, "defer_signal");
        defer_sig = ctx.builder.CreateAdd(defer_sig, ConstantInt::get(ctx.types().T_sigatomic, 1));
        setName(ctx.emission_context, defer_sig, "defer_signal_inc");
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
        setName(ctx.emission_context, pdefer_sig, "defer_signal_ptr");
        Value *defer_sig = ctx.builder.CreateLoad(ctx.types().T_sigatomic, pdefer_sig);
        setName(ctx.emission_context, defer_sig, "defer_signal");
        emit_signal_fence(ctx);
        error_unless(ctx,
                ctx.builder.CreateICmpNE(defer_sig, ConstantInt::get(ctx.types().T_sigatomic, 0)),
                "sigatomic_end called in non-sigatomic region");
        defer_sig = ctx.builder.CreateSub(
                defer_sig,
                ConstantInt::get(ctx.types().T_sigatomic, 1));
        setName(ctx.emission_context, defer_sig, "defer_signal_dec");
        ctx.builder.CreateStore(defer_sig, pdefer_sig);
        BasicBlock *checkBB = BasicBlock::Create(ctx.builder.getContext(), "check",
                                                 ctx.f);
        BasicBlock *contBB = BasicBlock::Create(ctx.builder.getContext(), "cont");
        auto not_deferred = ctx.builder.CreateICmpEQ(defer_sig, ConstantInt::get(ctx.types().T_sigatomic, 0));
        setName(ctx.emission_context, not_deferred, "deferred.not");
        ctx.builder.CreateCondBr(
                not_deferred,
                checkBB, contBB);
        ctx.builder.SetInsertPoint(checkBB);
        auto signal_page_load = ctx.builder.CreateLoad(
                ctx.types().T_size,
                emit_ptrgep(ctx, get_current_signal_page_from_ptls(ctx.builder, get_current_ptls(ctx), ctx.tbaa().tbaa_const),
                    -sizeof(size_t)),
                true);
        setName(ctx.emission_context, signal_page_load, "signal_page_load");
        ctx.builder.CreateBr(contBB);
        contBB->insertInto(ctx.f);
        ctx.builder.SetInsertPoint(contBB);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (is_libjulia_func(jl_string_ptr)) {
        ++CCALL_STAT(jl_string_ptr);
        assert(lrt == ctx.types().T_ptr);
        assert(!isVa && !llvmcall && nccallargs == 1);
        auto obj = emit_pointer_from_objref(ctx, boxed(ctx, argv[0])); // T_pprjlvalue
        // The inbounds gep makes it more clear to LLVM that the resulting value is not
        // a null pointer.
        auto strp = emit_ptrgep(ctx, obj, ctx.types().sizeof_ptr, "string_ptr");
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, strp, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_symbol_name)) {
        ++CCALL_STAT(jl_symbol_name);
        assert(lrt == ctx.types().T_ptr);
        assert(!isVa && !llvmcall && nccallargs == 1);
        auto obj = emit_pointer_from_objref(ctx, boxed(ctx, argv[0])); // T_pprjlvalue
        // The inbounds gep makes it more clear to LLVM that the resulting value is not
        // a null pointer.
        auto strp = emit_ptrgep(ctx, obj, sizeof(jl_sym_t), "symbol_name");
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, strp, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_genericmemory_owner) || is_libjulia_func(ijl_genericmemory_owner)) {
        ++CCALL_STAT(jl_genericmemory_owner);
        assert(lrt == ctx.types().T_prjlvalue);
        assert(!isVa && !llvmcall && nccallargs == 1);
        Value *obj = emit_genericmemoryowner(ctx, boxed(ctx, argv[0]));
        JL_GC_POP();
        return mark_julia_type(ctx, obj, true, jl_any_type);
    }
    else if (is_libjulia_func(memcpy) && (rt == (jl_value_t*)jl_nothing_type || jl_is_cpointer_type(rt))) {
        ++CCALL_STAT(memcpy);
        const jl_cgval_t &dst = argv[0];
        const jl_cgval_t &src = argv[1];
        const jl_cgval_t &n = argv[2];
        Value *destp = emit_unbox(ctx, ctx.types().T_ptr, update_julia_type(ctx, dst, (jl_value_t*)jl_voidpointer_type));

        ctx.builder.CreateMemCpy(
                destp,
                MaybeAlign(1),
                emit_unbox(ctx, ctx.types().T_ptr, update_julia_type(ctx, src, (jl_value_t*)jl_voidpointer_type)),
                MaybeAlign(1),
                emit_unbox(ctx, ctx.types().T_size, update_julia_type(ctx, n, (jl_value_t*)jl_ulong_type)),
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
        Value *destp = emit_unbox(ctx, ctx.types().T_ptr, update_julia_type(ctx, dst, (jl_value_t*)jl_voidpointer_type));
        Value *val32 = emit_unbox(ctx, getInt32Ty(ctx.builder.getContext()), update_julia_type(ctx, val, (jl_value_t*)jl_int32_type));
        Value *val8 = ctx.builder.CreateTrunc(val32, getInt8Ty(ctx.builder.getContext()), "memset_val");
        ctx.builder.CreateMemSet(
            destp,
            val8,
            emit_unbox(ctx, ctx.types().T_size, update_julia_type(ctx, n, (jl_value_t*)jl_ulong_type)),
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
        Value *destp = emit_unbox(ctx, ctx.types().T_ptr, update_julia_type(ctx, dst, (jl_value_t*)jl_voidpointer_type));

        ctx.builder.CreateMemMove(
                destp,
                MaybeAlign(0),
                emit_unbox(ctx, ctx.types().T_ptr, update_julia_type(ctx, src, (jl_value_t*)jl_voidpointer_type)),
                MaybeAlign(0),
                emit_unbox(ctx, ctx.types().T_size, update_julia_type(ctx, n, (jl_value_t*)jl_ulong_type)),
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
            Value *ph1 = decay_derived(ctx, boxed(ctx, val));
            Value *ph2 = ctx.builder.CreateInBoundsGEP(ctx.types().T_size, ph1, ConstantInt::get(ctx.types().T_size, hash_offset / ctx.types().sizeof_ptr));
            setName(ctx.emission_context, ph2, "object_id_ptr");
            LoadInst *hashval = ctx.builder.CreateAlignedLoad(ctx.types().T_size, ph2, ctx.types().alignof_ptr);
            setName(ctx.emission_context, hashval, "object_id");
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
            ai.decorateInst(hashval);
            return mark_or_box_ccall_result(ctx, hashval, retboxed, rt, unionall, static_rt);
        }
        else if (!val.isboxed) {
            // If the value is not boxed, try to compute the object id without
            // reboxing it.
            auto T_p_derived = PointerType::get(ctx.builder.getContext(), AddressSpace::Derived);
            if (!val.isghost)
                val = value_to_pointer(ctx, val);
            Value *args[] = {
                emit_typeof(ctx, val, false, true),
                val.isghost ? ConstantPointerNull::get(T_p_derived) :
                        decay_derived(ctx, data_pointer(ctx, val))
            };
            Value *ret = ctx.builder.CreateCall(prepare_call(jl_object_id__func), ArrayRef<Value*>(args));
            setName(ctx.emission_context, ret, "object_id");
            JL_GC_POP();
            return mark_or_box_ccall_result(ctx, ret, retboxed, rt, unionall, static_rt);
        }
    }

    jl_cgval_t retval = sig.emit_a_ccall(
            ctx,
            symarg,
            argv.data(),
            gc_uses,
            static_rt);
    JL_GC_POP();
    return retval;
}

static inline Constant *literal_static_pointer_val(const void *p, Type *T);

jl_cgval_t function_sig_t::emit_a_ccall(
        jl_codectx_t &ctx,
        const native_sym_arg_t &symarg,
        jl_cgval_t *argv,
        SmallVectorImpl<Value*> &gc_uses,
        bool static_rt) const
{
    ++EmittedCCalls;
    if (!err_msg.empty()) {
        emit_error(ctx, err_msg);
        return jl_cgval_t();
    }

    FunctionType *functype = this->functype(ctx.builder.getContext());

    SmallVector<Value *, 8> argvals(nccallargs + sret);
    for (size_t ai = 0; ai < nccallargs; ai++) {
        // Current C function parameter
        jl_cgval_t &arg = argv[ai];
        jl_value_t *jargty = jl_svecref(at, ai); // Julia type of the current parameter
        Type *largty = fargt[ai]; // LLVM type of the current parameter
        bool toboxed = fargt_isboxed[ai];
        Type *pargty = fargt_sig[ai + sret]; // LLVM coercion type
        bool byRef = byRefList[ai]; // Argument attributes

        // if we know the function sparams, try to fill those in now
        // so that the julia_to_native type checks are more likely to be doable (e.g. concrete types) at compile-time
        jl_value_t *jargty_in_env = jargty;
        if (ctx.spvals_ptr == NULL && !toboxed && unionall_env && jl_has_typevar_from_unionall(jargty, unionall_env) &&
                jl_svec_len(ctx.linfo->sparam_vals) > 0) {
            jargty_in_env = jl_instantiate_type_in_env(jargty_in_env, unionall_env, jl_svec_data(ctx.linfo->sparam_vals));
            if (jargty_in_env != jargty) {
                JL_GC_PUSH1(&jargty_in_env);
                jl_temporary_root(ctx, jargty_in_env);
                JL_GC_POP();
            }
        }

        Value *v;
        if (jl_is_abstract_ref_type(jargty)) {
            if (!jl_is_cpointer_type(arg.typ)) {
                emit_cpointercheck(ctx, arg, "ccall: argument to Ref{T} is not a pointer");
                arg = voidpointer_update(ctx, arg);
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
            result = emit_static_alloca(ctx, lrt, Align(julia_alignment(rt)));
            setName(ctx.emission_context, result, "ccall_sret");
            sretty = lrt;
            argvals[0] = result;
        }
        else {
            // XXX: result needs to be zero'd and given a GC root here
            // and has incorrect write barriers.
            // instead this code path should behave like `unsafe_load`
            result = emit_allocobj(ctx, (jl_datatype_t*)rt, true);
            setName(ctx.emission_context, result, "ccall_sret_box");
            sretty = ctx.types().T_jlvalue;
            sretboxed = true;
            gc_uses.push_back(result);
            argvals[0] = emit_pointer_from_objref(ctx, result);
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
            else if (f_name.starts_with("llvm.")) {
                // compute and verify auto-mangling for intrinsic name
#if JL_LLVM_VERSION >= 200000
                auto ID = Intrinsic::lookupIntrinsicID(f_name);
#else
                auto ID = Function::lookupIntrinsicID(f_name);
#endif
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
#if JL_LLVM_VERSION >= 200000
                            Function *intrinsic = Intrinsic::getOrInsertDeclaration(jl_Module, ID, overloadTys);
#else
                            Function *intrinsic = Intrinsic::getDeclaration(jl_Module, ID, overloadTys);
#endif
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
        null_pointer_check(ctx, symarg.jl_ptr, nullptr);
        llvmf = symarg.jl_ptr;
    }
    else if (symarg.fptr != NULL) {
        ++LiteralCCalls;
        Type *funcptype = PointerType::getUnqual(functype->getContext());
        llvmf = literal_static_pointer_val((void*)(uintptr_t)symarg.fptr, funcptype);
        setName(ctx.emission_context, llvmf, "ccall_fptr");
    }
    else if (!ctx.params->use_jlplt) {
        if ((symarg.f_lib && !((symarg.f_lib == JL_EXE_LIBNAME) ||
              (symarg.f_lib == JL_LIBJULIA_INTERNAL_DL_LIBNAME) ||
              (symarg.f_lib == JL_LIBJULIA_DL_LIBNAME))) || symarg.lib_expr) {
            emit_error(ctx, "ccall: Had library expression, but symbol lookup was disabled");
        }
        llvmf = jl_Module->getOrInsertFunction(symarg.f_name, functype).getCallee();
    }
    else {
        assert(symarg.f_name != NULL);
        PointerType *funcptype = PointerType::get(functype, 0);
        if (symarg.lib_expr) {
            ++DeferredCCallLookups;
            llvmf = runtime_sym_lookup(ctx, funcptype, NULL, symarg.lib_expr, symarg.f_name, ctx.f);
        }
        else {
            ++DeferredCCallLookups;
            // vararg requires musttail,
            // but musttail is incompatible with noreturn.
            if (functype->isVarArg())
                llvmf = runtime_sym_lookup(ctx, funcptype, symarg.f_lib, NULL, symarg.f_name, ctx.f);
            else
                llvmf = emit_plt(ctx, functype, attributes, cc, symarg.f_lib, symarg.f_name);
        }
    }

    // Potentially we could add gc_uses to `gc-transition`, instead of emitting them separately as jl_roots
    SmallVector<OperandBundleDef, 2> bundles;
    if (!gc_uses.empty())
        bundles.push_back(OperandBundleDef("jl_roots", gc_uses));
    if (gc_safe)
        bundles.push_back(OperandBundleDef("gc-transition", get_current_ptls(ctx)));
    // the actual call
    CallInst *ret = ctx.builder.CreateCall(functype, llvmf,
            argvals,
            bundles);
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
            setName(ctx.emission_context, result, "returned");
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
                Value *strct = emit_allocobj(ctx, (jl_datatype_t*)rt, true);
                setName(ctx.emission_context, strct, "ccall_ret_box");
                MDNode *tbaa = jl_is_mutable(rt) ? ctx.tbaa().tbaa_mutab : ctx.tbaa().tbaa_immut;
                Align boxalign(julia_alignment(rt));
                // copy the data from the return value to the new struct
                const DataLayout &DL = ctx.builder.GetInsertBlock()->getModule()->getDataLayout();
                auto resultTy = result->getType();
                size_t rtsz = jl_datatype_size(rt);
                if (DL.getTypeStoreSize(resultTy) > rtsz) {
                    // ARM and AArch64 can use a LLVM type larger than the julia type.
                    // When this happens, cast through memory.
                    auto slot = emit_static_alloca(ctx, resultTy, boxalign);
                    setName(ctx.emission_context, slot, "type_pun_slot");
                    ctx.builder.CreateAlignedStore(result, slot, boxalign);
                    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
                    emit_memcpy(ctx, strct, ai, slot, ai, rtsz, boxalign, boxalign);
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
