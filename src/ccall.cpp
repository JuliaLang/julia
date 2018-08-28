// This file is a part of Julia. License is MIT: https://julialang.org/license

// --- the ccall, cglobal, and llvm intrinsics ---

// Map from symbol name (in a certain library) to its GV in sysimg and the
// DL handle address in the current session.
typedef StringMap<std::pair<GlobalVariable*,void*>> SymMapGV;
static StringMap<std::pair<GlobalVariable*,SymMapGV>> libMapGV;
#ifdef _OS_WINDOWS_
static SymMapGV symMapExe;
static SymMapGV symMapDl;
#endif
static SymMapGV symMapDefault;

template<typename Func>
struct LazyModule {
    Func func;
    Module *m;
    template<typename Func2>
    LazyModule(Func2 &&func)
        : func(std::forward<Func2>(func)),
          m(nullptr)
    {}
    Module *get()
    {
        if (!m)
            m = func();
        return m;
    }
    Module &operator*()
    {
        return *get();
    }
};

template<typename Func>
static LazyModule<typename std::remove_reference<Func>::type>
lazyModule(Func &&func)
{
    return LazyModule<typename std::remove_reference<Func>::type>(
        std::forward<Func>(func));
}

// Find or create the GVs for the library and symbol lookup.
// Return `runtime_lib` (whether the library name is a string)
// Optionally return the symbol address in the current session
// when `symaddr != nullptr`.
// The `lib` and `sym` GV returned may not be in the current module.
template<typename MT>
static bool runtime_sym_gvs(const char *f_lib, const char *f_name, MT &&M,
                            GlobalVariable *&lib, GlobalVariable *&sym,
                            void **symaddr=nullptr)
{
    void *libsym = NULL;
    bool runtime_lib = false;
    GlobalVariable *libptrgv;
    SymMapGV *symMap;
#ifdef _OS_WINDOWS_
    if ((intptr_t)f_lib == 1) {
        libptrgv = jlexe_var;
        libsym = jl_exe_handle;
        symMap = &symMapExe;
    }
    else if ((intptr_t)f_lib == 2) {
        libptrgv = jldll_var;
        libsym = jl_dl_handle;
        symMap = &symMapDl;
    }
    else
#endif
    if (f_lib == NULL) {
        libptrgv = jlRTLD_DEFAULT_var;
        libsym = jl_RTLD_DEFAULT_handle;
        symMap = &symMapDefault;
    }
    else {
        std::string name = "ccalllib_";
        name += f_lib;
        runtime_lib = true;
        auto iter = libMapGV.find(f_lib);
        if (iter == libMapGV.end()) {
            libptrgv = new GlobalVariable(*M, T_pint8, false,
                                          GlobalVariable::ExternalLinkage,
                                          NULL, name);
            auto &libgv = libMapGV[f_lib];
            libgv = std::make_pair(global_proto(libptrgv), SymMapGV());
            symMap = &libgv.second;
            libsym = jl_get_library(f_lib);
            assert(libsym != NULL);
            *(void**)jl_emit_and_add_to_shadow(libptrgv) = libsym;
        }
        else {
            libptrgv = iter->second.first;
            symMap = &iter->second.second;
        }
    }
    if (libsym == NULL) {
        libsym = *(void**)jl_get_globalvar(libptrgv);
    }
    assert(libsym != NULL);

    GlobalVariable *llvmgv;
    auto sym_iter = symMap->find(f_name);
    if (sym_iter == symMap->end()) {
        // MCJIT forces this to have external linkage eventually, so we would clobber
        // the symbol of the actual function.
        std::string name = "ccall_";
        name += f_name;
        name += "_";
        name += std::to_string(globalUnique++);
        llvmgv = new GlobalVariable(*M, T_pvoidfunc, false,
                                    GlobalVariable::ExternalLinkage, NULL, name);
        llvmgv = global_proto(llvmgv);
        void *addr;
        jl_dlsym(libsym, f_name, &addr, 0);
        (*symMap)[f_name] = std::make_pair(llvmgv, addr);
        if (symaddr)
            *symaddr = addr;
        *(void**)jl_emit_and_add_to_shadow(llvmgv) = addr;
    }
    else {
        if (symaddr)
            *symaddr = sym_iter->second.second;
        llvmgv = sym_iter->second.first;
    }

    lib = libptrgv;
    sym = llvmgv;
    return runtime_lib;
}

static Value *runtime_sym_lookup(
        IRBuilder<> &irbuilder,
        PointerType *funcptype, const char *f_lib,
        const char *f_name, Function *f,
        GlobalVariable *libptrgv,
        GlobalVariable *llvmgv, bool runtime_lib)
{
    // in pseudo-code, this function emits the following:
    //   global HMODULE *libptrgv
    //   global void **llvmgv
    //   if (*llvmgv == NULL) {
    //       *llvmgv = jl_load_and_lookup(f_lib, f_name, libptrgv);
    //   }
    //   return (*llvmgv)
    BasicBlock *enter_bb = irbuilder.GetInsertBlock();
    BasicBlock *dlsym_lookup = BasicBlock::Create(jl_LLVMContext, "dlsym");
    BasicBlock *ccall_bb = BasicBlock::Create(jl_LLVMContext, "ccall");
    Constant *initnul = ConstantPointerNull::get((PointerType*)T_pvoidfunc);
    LoadInst *llvmf_orig = irbuilder.CreateAlignedLoad(llvmgv, sizeof(void*));
    // This in principle needs a consume ordering so that load from
    // this pointer sees a valid value. However, this is not supported by
    // LLVM (or agreed on in the C/C++ standard FWIW) and should be
    // almost impossible to happen on every platform we support since this
    // ordering is enforced by the hardware and LLVM has to speculate an
    // invalid load from the `cglobal` but doesn't depend on the `cglobal`
    // value for this to happen.
    // llvmf_orig->setAtomic(AtomicOrdering::Consume);
    irbuilder.CreateCondBr(
            irbuilder.CreateICmpNE(llvmf_orig, initnul),
            ccall_bb,
            dlsym_lookup);

    assert(f->getParent() != NULL);
    f->getBasicBlockList().push_back(dlsym_lookup);
    irbuilder.SetInsertPoint(dlsym_lookup);
    Value *libname;
    if (runtime_lib) {
        libname = stringConstPtr(irbuilder, f_lib);
    }
    else {
        // f_lib is actually one of the special sentinel values
        libname = ConstantExpr::getIntToPtr(ConstantInt::get(T_size, (uintptr_t)f_lib), T_pint8);
    }
    Value *llvmf = irbuilder.CreateCall(prepare_call_in(jl_builderModule(irbuilder), jldlsym_func),
            { libname, stringConstPtr(irbuilder, f_name), libptrgv });
    auto store = irbuilder.CreateAlignedStore(llvmf, llvmgv, sizeof(void*));
    store->setAtomic(AtomicOrdering::Release);
    irbuilder.CreateBr(ccall_bb);

    f->getBasicBlockList().push_back(ccall_bb);
    irbuilder.SetInsertPoint(ccall_bb);
    PHINode *p = irbuilder.CreatePHI(T_pvoidfunc, 2);
    p->addIncoming(llvmf_orig, enter_bb);
    p->addIncoming(llvmf, dlsym_lookup);
    return irbuilder.CreateBitCast(p, funcptype);
}

static Value *runtime_sym_lookup(
        jl_codectx_t &ctx,
        PointerType *funcptype, const char *f_lib,
        const char *f_name, Function *f)
{
    GlobalVariable *libptrgv;
    GlobalVariable *llvmgv;
    bool runtime_lib = runtime_sym_gvs(f_lib, f_name, f->getParent(),
                                       libptrgv, llvmgv);
    libptrgv = prepare_global(libptrgv);
    llvmgv = prepare_global(llvmgv);
    return runtime_sym_lookup(ctx.builder, funcptype, f_lib, f_name, f, libptrgv, llvmgv,
                              runtime_lib);
}

// Map from distinct callee's to its GOT entry.
// In principle the attribute, function type and calling convention
// don't need to be part of the key but it seems impossible to forward
// all the arguments without writing assembly directly.
// This doesn't matter too much in reality since a single function is usually
// not called with multiple signatures.
static DenseMap<AttributeList,
                std::map<std::tuple<GlobalVariable*,FunctionType*,
                                    CallingConv::ID>,GlobalVariable*>> allPltMap;

// Emit a "PLT" entry that will be lazily initialized
// when being called the first time.
static GlobalVariable *emit_plt_thunk(
        Module *M, FunctionType *functype,
        const AttributeList &attrs,
        CallingConv::ID cc, const char *f_lib, const char *f_name,
        GlobalVariable *libptrgv, GlobalVariable *llvmgv,
        void *symaddr, bool runtime_lib)
{
    PointerType *funcptype = PointerType::get(functype, 0);
    libptrgv = prepare_global_in(M, libptrgv);
    llvmgv = prepare_global_in(M, llvmgv);
    std::stringstream funcName;
    funcName << "jlplt_" << f_name << "_" << globalUnique++;
    auto fname = funcName.str();
    Function *plt = Function::Create(functype,
                                     GlobalVariable::ExternalLinkage,
                                     fname, M);
    jl_init_function(plt);
    plt->setAttributes(attrs);
    if (cc != CallingConv::C)
        plt->setCallingConv(cc);
    funcName << "_got";
    auto gname = funcName.str();
    GlobalVariable *got = new GlobalVariable(*M, T_pvoidfunc, false,
                                             GlobalVariable::ExternalLinkage,
                                             nullptr, gname);
    *(void**)jl_emit_and_add_to_shadow(got) = symaddr;
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", plt);
    IRBuilder<> irbuilder(b0);
    Value *ptr = runtime_sym_lookup(irbuilder, funcptype, f_lib, f_name, plt, libptrgv,
                                    llvmgv, runtime_lib);
    auto store = irbuilder.CreateAlignedStore(irbuilder.CreateBitCast(ptr, T_pvoidfunc), got, sizeof(void*));
    store->setAtomic(AtomicOrdering::Release);
    SmallVector<Value*, 16> args;
    for (Function::arg_iterator arg = plt->arg_begin(), arg_e = plt->arg_end(); arg != arg_e; ++arg)
        args.push_back(&*arg);
    CallInst *ret = irbuilder.CreateCall(ptr, ArrayRef<Value*>(args));
    ret->setAttributes(attrs);
    if (cc != CallingConv::C)
        ret->setCallingConv(cc);
    // NoReturn function can trigger LLVM verifier error when declared as
    // MustTail since other passes might replace the `ret` with
    // `unreachable` (LLVM should probably accept `unreachable`).
    if (attrs.hasAttribute(AttributeList::FunctionIndex,
                           Attribute::NoReturn)) {
        irbuilder.CreateUnreachable();
    }
    else {
        // musttail support is very bad on ARM, PPC, PPC64 (as of LLVM 3.9)
        // Known failures includes vararg (not needed here) and sret.
#if (defined(_CPU_X86_) || defined(_CPU_X86_64_) || \
                        defined(_CPU_AARCH64_))
        ret->setTailCallKind(CallInst::TCK_MustTail);
#endif
        if (functype->getReturnType() == T_void) {
            irbuilder.CreateRetVoid();
        }
        else {
            irbuilder.CreateRet(ret);
        }
    }
    irbuilder.ClearInsertionPoint();
    got = global_proto(got); // exchange got for the permanent global before jl_finalize_module destroys it
    jl_finalize_module(M, true);

    auto shadowgot =
        cast<GlobalVariable>(shadow_output->getNamedValue(gname));
    auto shadowplt = cast<Function>(shadow_output->getNamedValue(fname));
    shadowgot->setInitializer(ConstantExpr::getBitCast(shadowplt,
                                                       T_pvoidfunc));
    return got;
}

static Value *emit_plt(
        jl_codectx_t &ctx,
        FunctionType *functype,
       const AttributeList &attrs,
       CallingConv::ID cc, const char *f_lib, const char *f_name)
{
    assert(imaging_mode);
    // Don't do this for vararg functions so that the `musttail` is only
    // an optimization and is not required to function correctly.
    assert(!functype->isVarArg());
    GlobalVariable *libptrgv;
    GlobalVariable *llvmgv;
    void *symaddr;
    auto LM = lazyModule([&] {
            Module *m = new Module(f_name, jl_LLVMContext);
            jl_setup_module(m);
            return m;
        });
    bool runtime_lib = runtime_sym_gvs(f_lib, f_name, LM,
                                       libptrgv, llvmgv, &symaddr);
    PointerType *funcptype = PointerType::get(functype, 0);

    auto &pltMap = allPltMap[attrs];
    auto key = std::make_tuple(llvmgv, functype, cc);
    GlobalVariable *&shadowgot = pltMap[key];
    if (!shadowgot) {
        shadowgot = emit_plt_thunk(LM.get(), functype, attrs, cc, f_lib, f_name, libptrgv, llvmgv, symaddr, runtime_lib);
    }
    else {
        // `runtime_sym_gvs` shouldn't have created anything in a new module
        // if it returns a GV that already exists.
        assert(!LM.m);
    }
    GlobalVariable *got = prepare_global(shadowgot);
    LoadInst *got_val = ctx.builder.CreateAlignedLoad(got, sizeof(void*));
    // See comment in `runtime_sym_lookup` above. This in principle needs a
    // consume ordering too. This is even less likely to cause issues though
    // since the only thing we do to this loaded pointer is to call it
    // immediately.
    // got_val->setAtomic(AtomicOrdering::Consume);
    return ctx.builder.CreateBitCast(got_val, funcptype);
}

// --- ABI Implementations ---
// Partially based on the LDC ABI implementations licensed under the BSD 3-clause license

class AbiLayout {
public:
    virtual ~AbiLayout() {}
    virtual bool use_sret(jl_datatype_t *ty) = 0;
    virtual bool needPassByRef(jl_datatype_t *ty, AttrBuilder&) = 0;
    virtual Type *preferred_llvm_type(jl_datatype_t *ty, bool isret) const = 0;
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
#  warning "ccall is defaulting to llvm ABI, since no platform ABI has been defined for this CPU/OS combination"
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

    if (from_type == T_void || isa<UndefValue>(v))
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
    const DataLayout &DL = jl_data_layout;
    if (DL.getTypeAllocSize(target_type) >= DL.getTypeAllocSize(from_type)) {
        to = emit_static_alloca(ctx, target_type);
        from = emit_bitcast(ctx, to, from_type->getPointerTo());
    }
    else {
        from = emit_static_alloca(ctx, from_type);
        to = emit_bitcast(ctx, from, target_type->getPointerTo());
    }
    ctx.builder.CreateStore(v, from);
    return ctx.builder.CreateLoad(to);
}

// --- argument passing and scratch space utilities ---

static Value *runtime_apply_type_env(jl_codectx_t &ctx, jl_value_t *ty)
{
    // box if concrete type was not statically known
    Value *args[] = {
        literal_pointer_val(ctx, ty),
        literal_pointer_val(ctx, (jl_value_t*)ctx.linfo->def.method->sig),
        ctx.builder.CreateInBoundsGEP(
                T_prjlvalue,
                ctx.spvals_ptr,
                ConstantInt::get(T_size, sizeof(jl_svec_t) / sizeof(jl_value_t*)))
    };
    return ctx.builder.CreateCall(prepare_call(jlapplytype_func), makeArrayRef(args));
}

static const std::string make_errmsg(const char *fname, int n, const char *err)
{
    std::stringstream msg;
    msg << fname;
    if (n > 0)
        msg << " argument " << n;
    else
        msg << " return";
    msg << err;
    return msg.str();
}

static void typeassert_input(jl_codectx_t &ctx, const jl_cgval_t &jvinfo, jl_value_t *jlto, jl_unionall_t *jlto_env, int argn, bool addressOf)
{
    if (jlto != (jl_value_t*)jl_any_type && !jl_subtype(jvinfo.typ, jlto)) {
        if (!addressOf && jlto == (jl_value_t*)jl_voidpointer_type) {
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
                        ConstantInt::get(T_int32, 0));
                BasicBlock *failBB = BasicBlock::Create(jl_LLVMContext, "fail", ctx.f);
                BasicBlock *passBB = BasicBlock::Create(jl_LLVMContext, "pass", ctx.f);
                ctx.builder.CreateCondBr(istype, passBB, failBB);

                ctx.builder.SetInsertPoint(failBB);
                emit_type_error(ctx, mark_julia_type(ctx, vx, true, jl_any_type), boxed(ctx, jlto_runtime), msg);
                ctx.builder.CreateUnreachable();
                ctx.builder.SetInsertPoint(passBB);
            }
        }
    }
}

static Value *julia_to_address(
        jl_codectx_t &ctx,
        Type *to, jl_value_t *jlto, jl_unionall_t *jlto_env,
        const jl_cgval_t &jvinfo,
        int argn, bool *needStackRestore)
{
    assert(jl_is_datatype(jlto) && julia_struct_has_layout((jl_datatype_t*)jlto, jlto_env));

    if (!jl_is_cpointer_type(jlto) || to != T_size) {
        emit_error(ctx, "ccall: & on argument was not matched by Ptr{T} argument type");
        return UndefValue::get(to);
    }

    jl_value_t *ety;
    if (jlto == (jl_value_t*)jl_voidpointer_type) {
        ety = jvinfo.typ; // skip the type-check
    }
    else {
        ety = jl_tparam0(jlto);
        typeassert_input(ctx, jvinfo, ety, jlto_env, argn, true);
    }

    if (jvinfo.isboxed) {
        if (!jl_is_abstracttype(ety)) {
            if (jl_is_mutable_datatype(ety)) {
                // no copy, just reference the data field
                return ctx.builder.CreateBitCast(emit_pointer_from_objref(ctx, data_pointer(ctx, jvinfo)), to);
            }
            else if (jl_is_immutable_datatype(ety) && jlto != (jl_value_t*)jl_voidpointer_type) { // anything declared `struct`, except Ptr{Cvoid}
                // yes copy
                Value *nbytes;
                AllocaInst *ai;
                if (((jl_datatype_t*)ety)->layout) {
                    int nb = jl_datatype_size(ety);
                    nbytes = ConstantInt::get(T_int32, nb);
                    ai = emit_static_alloca(ctx, T_int8, nb);
                }
                else {
                    nbytes = emit_datatype_size(ctx, emit_typeof_boxed(ctx, jvinfo));
                    ai = ctx.builder.CreateAlloca(T_int8, nbytes);
                    *needStackRestore = true;
                }
                ai->setAlignment(16);
                // minimum gc-alignment in julia is pointer size
                emit_memcpy(ctx, ai, jvinfo.tbaa, jvinfo, nbytes, sizeof(void*));
                return ctx.builder.CreatePtrToInt(ai, to);
            }
        }
        // emit maybe copy
        *needStackRestore = true;
        Value *jvt = emit_typeof_boxed(ctx, jvinfo);
        BasicBlock *mutableBB = BasicBlock::Create(jl_LLVMContext, "is-mutable", ctx.f);
        BasicBlock *immutableBB = BasicBlock::Create(jl_LLVMContext, "is-immutable", ctx.f);
        BasicBlock *afterBB = BasicBlock::Create(jl_LLVMContext, "after", ctx.f);
        Value *ismutable = emit_datatype_mutabl(ctx, jvt);
        ctx.builder.CreateCondBr(ismutable, mutableBB, immutableBB);
        ctx.builder.SetInsertPoint(mutableBB);
        Value *p1 = ctx.builder.CreateBitCast(emit_pointer_from_objref(ctx, data_pointer(ctx, jvinfo)), to);
        ctx.builder.CreateBr(afterBB);
        ctx.builder.SetInsertPoint(immutableBB);
        Value *nbytes = emit_datatype_size(ctx, jvt);
        AllocaInst *ai = ctx.builder.CreateAlloca(T_int8, nbytes);
        ai->setAlignment(16);
        emit_memcpy(ctx, ai, jvinfo.tbaa, jvinfo, nbytes, sizeof(void*)); // minimum gc-alignment in julia is pointer size
        Value *p2 = ctx.builder.CreatePtrToInt(ai, to);
        ctx.builder.CreateBr(afterBB);
        ctx.builder.SetInsertPoint(afterBB);
        PHINode *p = ctx.builder.CreatePHI(to, 2);
        p->addIncoming(p1, mutableBB);
        p->addIncoming(p2, immutableBB);
        return p;
    }

    Type *slottype = julia_struct_to_llvm(jvinfo.typ, NULL, NULL);
    // pass the address of an alloca'd thing, not a box
    // since those are immutable.
    Value *slot = emit_static_alloca(ctx, slottype);
    if (!jvinfo.ispointer()) {
        tbaa_decorate(jvinfo.tbaa, ctx.builder.CreateStore(emit_unbox(ctx, slottype, jvinfo, ety), slot));
    }
    else {
        emit_memcpy(ctx, slot, jvinfo.tbaa, jvinfo, jl_datatype_size(ety), jl_datatype_align(ety));
    }
    return ctx.builder.CreatePtrToInt(slot, to);
}


// Emit code to convert argument to form expected by C ABI
// to = desired LLVM type
// jlto = Julia type of formal argument
// jvinfo = value of actual argument
static Value *julia_to_native(
        jl_codectx_t &ctx,
        Type *to, bool toboxed, jl_value_t *jlto, jl_unionall_t *jlto_env,
        const jl_cgval_t &jvinfo,
        bool byRef, int argn,
        bool *needStackRestore)
{
    // We're passing Any
    if (toboxed) {
        assert(!byRef); // don't expect any ABI to pass pointers by pointer
        return maybe_decay_untracked(boxed(ctx, jvinfo));
    }
    assert(jl_is_datatype(jlto) && julia_struct_has_layout((jl_datatype_t*)jlto, jlto_env));

    typeassert_input(ctx, jvinfo, jlto, jlto_env, argn, false);
    if (!byRef)
        return emit_unbox(ctx, to, jvinfo, jlto);

    // pass the address of an alloca'd thing, not a box
    // since those are immutable.
    Value *slot = emit_static_alloca(ctx, to);
    if (!jvinfo.ispointer()) {
        tbaa_decorate(jvinfo.tbaa, ctx.builder.CreateStore(emit_unbox(ctx, to, jvinfo, jlto), slot));
    }
    else {
        emit_memcpy(ctx, slot, jvinfo.tbaa, jvinfo, jl_datatype_size(jlto), jl_datatype_align(jlto));
    }
    return slot;
}

typedef struct {
    Value *jl_ptr;  // if the argument is a run-time computed pointer
    void (*fptr)(void);     // if the argument is a constant pointer
    const char *f_name;   // if the symbol name is known
    const char *f_lib;    // if a library name is specified
    jl_value_t *gcroot;
} native_sym_arg_t;

// --- parse :sym or (:sym, :lib) argument into address info ---
static void interpret_symbol_arg(jl_codectx_t &ctx, native_sym_arg_t &out, jl_value_t *arg, const char *fname, bool llvmcall)
{
    Value *&jl_ptr = out.jl_ptr;
    void (*&fptr)(void) = out.fptr;
    const char *&f_name = out.f_name;
    const char *&f_lib = out.f_lib;

    jl_value_t *ptr = static_eval(ctx, arg, true);
    if (ptr == NULL) {
        jl_cgval_t arg1 = emit_expr(ctx, arg);
        jl_value_t *ptr_ty = arg1.typ;
        if (!jl_is_cpointer_type(ptr_ty)) {
           const char *errmsg = !strcmp(fname, "ccall") ?
               "ccall: first argument not a pointer or valid constant expression" :
               "cglobal: first argument not a pointer or valid constant expression";
            emit_cpointercheck(ctx, arg1, errmsg);
        }
        arg1 = update_julia_type(ctx, arg1, (jl_value_t*)jl_voidpointer_type);
        jl_ptr = emit_unbox(ctx, T_size, arg1, (jl_value_t*)jl_voidpointer_type);
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
#ifdef _OS_WINDOWS_
            if (!llvmcall)
                f_lib = jl_dlfind_win32(f_name);
#endif
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


static jl_value_t* try_eval(jl_codectx_t &ctx, jl_value_t *ex, const char *failure)
{
    jl_value_t *constant = static_eval(ctx, ex, true, true);
    if (jl_is_ssavalue(ex) && !constant)
        jl_error(failure);
    else if (constant)
        return constant;

    JL_TRY {
        size_t last_age = jl_get_ptls_states()->world_age;
        jl_get_ptls_states()->world_age = ctx.world;
        constant = jl_interpret_toplevel_expr_in(ctx.module, ex, ctx.source, ctx.linfo->sparam_vals);
        jl_get_ptls_states()->world_age = last_age;
    }
    JL_CATCH {
        jl_rethrow_with_add(failure);
    }

    return constant;
}

// --- code generator for cglobal ---

static jl_cgval_t emit_runtime_call(jl_codectx_t &ctx, JL_I::intrinsic f, const jl_cgval_t *argv, size_t nargs);

static jl_cgval_t emit_cglobal(jl_codectx_t &ctx, jl_value_t **args, size_t nargs)
{
    JL_NARGS(cglobal, 1, 2);
    jl_value_t *rt = NULL;
    Value *res;
    native_sym_arg_t sym = {};
    JL_GC_PUSH2(&rt, &sym.gcroot);

    if (nargs == 2) {
        rt = static_eval(ctx, args[2], true, true);
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
    Type *lrt = julia_type_to_llvm(rt);

    interpret_symbol_arg(ctx, sym, args[1], "cglobal", false);

    if (sym.jl_ptr != NULL) {
        res = ctx.builder.CreateBitCast(sym.jl_ptr, lrt);
    }
    else if (sym.fptr != NULL) {
        res = ConstantInt::get(lrt, (uint64_t)sym.fptr);
        if (imaging_mode)
            jl_printf(JL_STDERR,"WARNING: literal address used in cglobal for %s; code cannot be statically compiled\n", sym.f_name);
    }
    else {
        if (imaging_mode) {
            res = runtime_sym_lookup(ctx, cast<PointerType>(T_pint8), sym.f_lib, sym.f_name, ctx.f);
            res = ctx.builder.CreatePtrToInt(res, lrt);
        }
        else {
            void *symaddr;
            if (!jl_dlsym(jl_get_library(sym.f_lib), sym.f_name, &symaddr, 0)) {
                std::stringstream msg;
                msg << "cglobal: could not find symbol ";
                msg << sym.f_name;
                if (sym.f_lib != NULL) {
#ifdef _OS_WINDOWS_
                    assert(sym.f_lib != JL_EXE_LIBNAME && sym.f_lib != JL_DL_LIBNAME);
#endif
                    msg << " in library ";
                    msg << sym.f_lib;
                }
                emit_error(ctx, msg.str());
            }
            // since we aren't saving this code, there's no sense in
            // putting anything complicated here: just JIT the address of the cglobal
            res = ConstantInt::get(lrt, (uint64_t)symaddr);
        }
    }

    JL_GC_POP();
    return mark_julia_type(ctx, res, false, rt);
}

class FunctionMover final : public ValueMaterializer
{
public:
    FunctionMover(llvm::Module *dest,llvm::Module *src) :
        ValueMaterializer(), VMap(), destModule(dest), srcModule(src),
        LazyFunctions(0)
    {
    }
    ValueToValueMapTy VMap;
    llvm::Module *destModule;
    llvm::Module *srcModule;
    std::vector<Function *> LazyFunctions;

    Function *CloneFunctionProto(Function *F)
    {
        assert(!F->isDeclaration());
        Function *NewF = Function::Create(F->getFunctionType(),
                                          Function::ExternalLinkage,
                                          F->getName(),
                                          destModule);
        LazyFunctions.push_back(F);
        VMap[F] = NewF;
        return NewF;
    }

    void CloneFunctionBody(Function *F)
    {
        Function *NewF = (Function*)(Value*)VMap[F];
        assert(NewF != NULL);

        Function::arg_iterator DestI = NewF->arg_begin();
        for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
            DestI->setName(I->getName());    // Copy the name over...
            VMap[&*I] = &*(DestI++);        // Add mapping to VMap
        }

        SmallVector<ReturnInst*, 8> Returns;
        llvm::CloneFunctionInto(NewF,F,VMap,true,Returns,"",NULL,NULL,this);
        NewF->setComdat(nullptr);
        NewF->setSection("");
    }

    Function *CloneFunction(Function *F)
    {
        Function *NewF = (llvm::Function*)MapValue(F,VMap,RF_None,NULL,this);
        ResolveLazyFunctions();
        return NewF;
    }

    void ResolveLazyFunctions()
    {
        while (!LazyFunctions.empty()) {
            Function *F = LazyFunctions.back();
            LazyFunctions.pop_back();

            CloneFunctionBody(F);
        }
    }

    Value *InjectFunctionProto(Function *F)
    {
        Function *NewF = destModule->getFunction(F->getName());
        if (!NewF) {
            NewF = function_proto(F);
            NewF->setComdat(nullptr);
            destModule->getFunctionList().push_back(NewF);
        }
        return NewF;
    }

    Value *materialize(Value *V) override
    {
        Function *F = dyn_cast<Function>(V);
        if (F) {
            if (isIntrinsicFunction(F)) {
                return destModule->getOrInsertFunction(F->getName(),F->getFunctionType());
            }
            if (F->isDeclaration() || F->getParent() != destModule) {
                if (F->getName().empty())
                    return CloneFunctionProto(F);
                Function *shadow = srcModule->getFunction(F->getName());
                if (shadow != NULL && !shadow->isDeclaration()) {
                    Function *oldF = destModule->getFunction(F->getName());
                    if (oldF)
                        return oldF;
                    if (jl_ExecutionEngine->findSymbol(F->getName(), false))
                        return InjectFunctionProto(F);
                    return CloneFunctionProto(shadow);
                }
                else if (!F->isDeclaration()) {
                    return CloneFunctionProto(F);
                }
            }
            // Still a declaration and still in a different module
            if (F->isDeclaration() && F->getParent() != destModule) {
                // Create forward declaration in current module
                return InjectFunctionProto(F);
            }
        }
        else if (isa<GlobalVariable>(V)) {
            GlobalVariable *GV = cast<GlobalVariable>(V);
            assert(GV != NULL);
            GlobalVariable *oldGV = destModule->getGlobalVariable(GV->getName());
            if (oldGV != NULL)
                return oldGV;
            GlobalVariable *newGV = new GlobalVariable(*destModule,
                GV->getType()->getElementType(),
                GV->isConstant(),
                GlobalVariable::ExternalLinkage,
                NULL,
                GV->getName(),
                NULL,
                GV->getThreadLocalMode(),
                GV->getType()->getPointerAddressSpace());
            newGV->copyAttributesFrom(GV);
            newGV->setComdat(nullptr);
            if (GV->isDeclaration())
                return newGV;
            if (!GV->getName().empty()) {
                uint64_t addr = jl_ExecutionEngine->getGlobalValueAddress(GV->getName());
                if (addr != 0) {
                    newGV->setExternallyInitialized(true);
                    return newGV;
                }
            }
            if (GV->hasInitializer()) {
                Value *C = MapValue(GV->getInitializer(),VMap,RF_None,NULL,this);
                newGV->setInitializer(cast<Constant>(C));
            }
            return newGV;
        }
        return NULL;
    };
};

// llvmcall(ir, (rettypes...), (argtypes...), args...)
static jl_cgval_t emit_llvmcall(jl_codectx_t &ctx, jl_value_t **args, size_t nargs)
{
    JL_NARGSV(llvmcall, 3);
    jl_value_t *rt = NULL, *at = NULL, *ir = NULL, *decl = NULL;
    jl_value_t *ir_arg = args[1];
    JL_GC_PUSH4(&ir, &rt, &at, &decl);
    if (jl_is_ssavalue(ir_arg))
        ir_arg = jl_arrayref((jl_array_t*)ctx.source->code, ((jl_ssavalue_t*)ir_arg)->id - 1);
    ir = try_eval(ctx, ir_arg, "error statically evaluating llvm IR argument");
    if (jl_is_ssavalue(args[2])) {
        jl_value_t *rtt = jl_arrayref((jl_array_t*)ctx.source->ssavaluetypes, ((jl_ssavalue_t*)args[2])->id - 1);
        if (jl_is_type_type(rtt))
            rt = jl_tparam0(rtt);
    }
    if (rt == NULL)
        rt = try_eval(ctx, args[2], "error statically evaluating llvmcall return type");
    if (jl_is_ssavalue(args[3])) {
        jl_value_t *att = jl_arrayref((jl_array_t*)ctx.source->ssavaluetypes, ((jl_ssavalue_t*)args[3])->id - 1);
        if (jl_is_type_type(att))
            at = jl_tparam0(att);
    }
    if (at == NULL)
        at = try_eval(ctx, args[3], "error statically evaluating llvmcall argument tuple");
    int i = 1;
    if (jl_is_tuple(ir)) {
        // if the IR is a tuple, we expect (declarations, ir)
        if (jl_nfields(ir) != 2)
            jl_error("Tuple as first argument to llvmcall must have exactly two children");
        decl = jl_fieldref(ir,0);
        ir = jl_fieldref(ir,1);
        if (!jl_is_string(decl))
            jl_error("Declarations passed to llvmcall must be a string");
    }
    bool isString = jl_is_string(ir);
    bool isPtr = jl_is_cpointer(ir);
    if (!isString && !isPtr) {
        jl_error("IR passed to llvmcall must be a string or pointer to an LLVM Function");
    }

    JL_TYPECHK(llvmcall, type, rt);
    JL_TYPECHK(llvmcall, type, at);

    std::stringstream ir_stream;

    // Generate arguments
    std::string arguments;
    llvm::raw_string_ostream argstream(arguments);
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
        Type *t = julia_type_to_llvm(tti, &toboxed);
        argtypes.push_back(t);
        if (4 + i > nargs) {
            jl_error("Missing arguments to llvmcall!");
        }
        jl_value_t *argi = args[4 + i];
        jl_cgval_t arg = emit_expr(ctx, argi);

        Value *v = julia_to_native(ctx, t, toboxed, tti, NULL, arg, false, i, NULL);
        bool issigned = jl_signed_type && jl_subtype(tti, (jl_value_t*)jl_signed_type);
        argvals[i] = llvm_type_rewrite(ctx, v, t, issigned);
    }

    Function *f;
    bool retboxed;
    Type *rettype = julia_type_to_llvm(rtt, &retboxed);
    if (isString) {
        // Make sure to find a unique name
        std::string ir_name;
        while(true) {
            std::stringstream name;
            name << (ctx.f->getName().str()) << "u" << i++;
            ir_name = name.str();
            if (jl_Module->getFunction(ir_name) == NULL)
                break;
        }

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
        llvm::raw_string_ostream rtypename(rstring);
        rettype->print(rtypename);
        std::map<uint64_t,std::string> localDecls;

        if (decl != NULL) {
            std::stringstream declarations(jl_string_data(decl));

            // parse string line by line
            std::string declstr;
            while (std::getline(declarations, declstr, '\n')) {
                // Find name of declaration by searching for '@'
                std::string::size_type atpos = declstr.find('@') + 1;
                // Find end of declaration by searching for '('
                std::string::size_type bracepos = declstr.find('(', atpos);
                // Declaration name is the string between @ and (
                std::string declname = declstr.substr(atpos, bracepos - atpos);

                // Check if declaration already present in module
                if(jl_Module->getNamedValue(declname) == NULL) {
                    ir_stream << "; Declarations\n" << declstr << "\n";
                }
            }
        }
        ir_stream << "; Number of arguments: " << nargt << "\n"
        << "define "<<rtypename.str()<<" @\"" << ir_name << "\"("<<argstream.str()<<") {\n"
        << jl_string_data(ir) << "\n}";
        SMDiagnostic Err = SMDiagnostic();
        std::string ir_string = ir_stream.str();
        // Do not enable update debug info since it runs the verifier on the whole module
        // and will error on the function we are currently emitting.
        bool failed = parseAssemblyInto(llvm::MemoryBufferRef(ir_string, "llvmcall"),
                                        *jl_Module, Err, nullptr, /* UpdateDebugInfo */ false);
        if (failed) {
            std::string message = "Failed to parse LLVM Assembly: \n";
            llvm::raw_string_ostream stream(message);
            Err.print("julia",stream,true);
            jl_error(stream.str().c_str());
        }
        f = jl_Module->getFunction(ir_name);
    }
    else {
        assert(isPtr);
        // Create Function skeleton
        f = (llvm::Function*)jl_unbox_voidpointer(ir);
        assert(!f->isDeclaration());
        assert(f->getReturnType() == rettype);
        int i = 0;
        for (std::vector<Type *>::iterator it = argtypes.begin();
            it != argtypes.end(); ++it, ++i)
            assert(*it == f->getFunctionType()->getParamType(i));

        if (f->getParent() != jl_Module) {
            FunctionMover mover(jl_Module, f->getParent());
            f = mover.CloneFunction(f);
        }

        //f->dump();
        llvm::raw_fd_ostream out(1,false);
        if (verifyFunction(*f,&out)) {
            llvm_dump(f);
            jl_error("Malformed LLVM Function");
        }
    }

    // Since we dumped all of f's dependencies into the active module,
    // we cannot reasonably inline it, so leave it there and just emit
    // a regular call
    if (!isString) {
        static int llvmcallnumbering = 0;
        std::stringstream name;
        name << "jl_llvmcall" << llvmcallnumbering++;
        f->setName(name.str());
        jl_init_function(f);
        f = cast<Function>(prepare_call(function_proto(f)));
    }
    else {
        jl_init_function(f);
        f->setLinkage(GlobalValue::LinkOnceODRLinkage);
    }

    CallInst *inst = ctx.builder.CreateCall(f, ArrayRef<Value*>(&argvals[0], nargt));
    if (isString) {
        f->addFnAttr(Attribute::AlwaysInline);
        inst->setAttributes(f->getAttributes());
    }

    JL_GC_POP();

    if (inst->getType() != rettype) {
        jl_error("Return type of llvmcall'ed function does not match declared return type");
    }

    return mark_julia_type(ctx, inst, retboxed, rtt);
}

// --- code generator for ccall itself ---

static Value *box_ccall_result(jl_codectx_t &ctx, Value *result, Value *runtime_dt, jl_value_t *rt)
{
    // XXX: need to handle parameterized zero-byte types (singleton)
    const DataLayout &DL = jl_data_layout;
    unsigned nb = DL.getTypeStoreSize(result->getType());
    MDNode *tbaa = jl_is_mutable(rt) ? tbaa_mutab : tbaa_immut;
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
    std::vector<Type*> fargt; // vector of llvm output types (julia_struct_to_llvm) for arguments (vararg is the last item, if applicable)
    std::vector<Type*> fargt_sig; // vector of ABI coercion types for call signature
    std::vector<bool> fargt_isboxed; // vector of whether the llvm output type is a Julia-box for each argument (vararg is the last item, if applicable)
    Type *fargt_vasig = NULL; // ABI coercion type for vararg list
    std::vector<bool> byRefList; // vector of "byref" parameters (vararg is the last item, if applicable)
    AttributeList attributes; // vector of function call site attributes (vararg is the last item, if applicable)
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
    size_t nargs; // number of actual arguments (can be different from the size of at when varargs)
    size_t isVa;

    function_sig_t(const char *fname, Type *lrt, jl_value_t *rt, bool retboxed, jl_svec_t *at, jl_unionall_t *unionall_env, size_t nargs, size_t isVa, CallingConv::ID cc, bool llvmcall)
      : fargt_vasig(NULL), lrt(lrt), retboxed(retboxed),
        prt(NULL), sret(0), cc(cc), llvmcall(llvmcall),
        at(at), rt(rt), unionall_env(unionall_env),
        nargs(nargs), isVa(isVa)
    {
        err_msg = generate_func_sig(fname);
    }

    FunctionType *functype() const {
        assert(err_msg.empty());
        return FunctionType::get(sret ? T_void : prt, fargt_sig, isVa);
    }

    jl_cgval_t emit_a_ccall(
            jl_codectx_t &ctx,
            const native_sym_arg_t &symarg,
            size_t nargt,
            std::vector<bool> &addressOf,
            jl_cgval_t *argv,
            SmallVector<Value*, 16> &gc_uses,
            bool static_rt) const;

private:
std::string generate_func_sig(const char *fname)
{
    size_t nargt = jl_svec_len(at);
    assert(rt && !jl_is_abstract_ref_type(rt));

    std::vector<AttrBuilder> paramattrs;
    std::unique_ptr<AbiLayout> abi;
    if (llvmcall)
        abi.reset(new ABI_LLVMLayout());
    else
        abi.reset(new DefaultAbiState());
    sret = 0;

    if (type_is_ghost(lrt)) {
        prt = lrt = T_void;
        abi->use_sret(jl_void_type);
    }
    else {
        if (!jl_is_datatype(rt) || ((jl_datatype_t*)rt)->layout == NULL || jl_is_cpointer_type(rt) || jl_is_array_type(rt) || retboxed) {
            prt = lrt; // passed as pointer
            abi->use_sret(jl_voidpointer_type);
        }
        else if (abi->use_sret((jl_datatype_t*)rt)) {
            AttrBuilder retattrs = AttrBuilder();
#if !defined(_OS_WINDOWS_) // llvm used to use the old mingw ABI, skipping this marking works around that difference
            retattrs.addAttribute(Attribute::StructRet);
#endif
            retattrs.addAttribute(Attribute::NoAlias);
            paramattrs.push_back(std::move(retattrs));
            fargt_sig.push_back(PointerType::get(lrt, 0));
            sret = 1;
            prt = lrt;
        }
        else {
            prt = abi->preferred_llvm_type((jl_datatype_t*)rt, true);
            if (prt == NULL)
                prt = lrt;
        }
    }

    size_t i;
    bool current_isVa = false;
    for (i = 0; i < nargt; ) {
        AttrBuilder ab;
        jl_value_t *tti = jl_svecref(at, i);
        if (jl_is_vararg_type(tti)) {
            current_isVa = true;
            tti = jl_unwrap_vararg(tti);
        }
        Type *t = NULL;
        bool isboxed;
        if (jl_is_abstract_ref_type(tti)) {
            tti = (jl_value_t*)jl_voidpointer_type;
            t = T_pint8;
            isboxed = false;
        }
        else {
            if (jl_is_primitivetype(tti)) {
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

            t = julia_struct_to_llvm(tti, unionall_env, &isboxed);
            if (isboxed)
                t = T_prjlvalue;
            if (t == NULL || t == T_void) {
                return make_errmsg(fname, i + 1, " doesn't correspond to a C type");
            }
        }

        Type *pat;
        if (!jl_is_datatype(tti) || ((jl_datatype_t*)tti)->layout == NULL || jl_is_array_type(tti))
            tti = (jl_value_t*)jl_voidpointer_type; // passed as pointer

        // Whether or not LLVM wants us to emit a pointer to the data
        bool byRef = abi->needPassByRef((jl_datatype_t*)tti, ab);

        if (jl_is_cpointer_type(tti)) {
            pat = t;
        }
        else if (byRef) {
            pat = PointerType::get(t, AddressSpace::Derived);
        }
        else {
            pat = abi->preferred_llvm_type((jl_datatype_t*)tti, false);
            if (pat == NULL)
                pat = t;
        }

        byRefList.push_back(byRef);
        fargt.push_back(t);
        fargt_isboxed.push_back(isboxed);
        if (!current_isVa)
            fargt_sig.push_back(pat);
        else
            fargt_vasig = pat;

        do { // for each arg for which this type applies, add the appropriate LLVM parameter attributes
            if (i < nargs) { // if vararg, the last declared arg type may not have a corresponding arg value
                AttributeSet params = AttributeSet::get(jl_LLVMContext, ab);
                paramattrs.push_back(params);
            }
            i++;
        } while (current_isVa && i < nargs); // if is this is the vararg, loop to the end
    }

    for (i = 0; i < nargs + sret; ++i) {
        const auto &as = paramattrs.at(i);
        if (!as.hasAttributes())
            continue;
        attributes = attributes.addAttributes(jl_LLVMContext, i + 1, as);
    }
    if (rt == jl_bottom_type) {
        attributes = attributes.addAttribute(jl_LLVMContext,
                                             AttributeList::FunctionIndex,
                                             Attribute::NoReturn);
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
                                maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)jl_any_type)));
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

static const std::string verify_ccall_sig(size_t nccallargs, jl_value_t *&rt, jl_value_t *at,
                                          jl_unionall_t *unionall_env, jl_svec_t *sparam_vals, const char *fname,
                                          size_t &nargt, bool &isVa, Type *&lrt, bool &retboxed, bool &static_rt)
{
    JL_TYPECHK(ccall, type, rt);
    JL_TYPECHK(ccall, simplevector, at);

    if (jl_is_array_type(rt)) {
        // `Array` used as return type just returns a julia object reference
        rt = (jl_value_t*)jl_any_type;
    }

    lrt = julia_struct_to_llvm(rt, unionall_env, &retboxed);
    if (lrt == NULL)
        return "return type doesn't correspond to a C type";
    else if (retboxed)
        lrt = T_prjlvalue;

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

    nargt = jl_svec_len(at);
    isVa = (nargt > 0 && jl_is_vararg_type(jl_svecref(at, nargt - 1)));
    if (!strcmp(fname, "ccall")) {
        if (!retboxed && static_rt) {
            if (!jl_is_concrete_type(rt)) {
                if (jl_is_cpointer_type(rt))
                    return "return type Ptr should have an element type (not Ptr{<:T})";
                else if (rt != jl_bottom_type)
                    return "return type must be a concrete DataType";
            }
        }

        if (isVa ? (nccallargs < nargt - 1) :
                   (nccallargs != nargt)) {
            return "wrong number of arguments to ccall function";
        }
    }

    return "";
}

// Expr(:foreigncall, pointer, rettype, (argtypes...), args...)
static jl_cgval_t emit_ccall(jl_codectx_t &ctx, jl_value_t **args, size_t nargs)
{
    JL_NARGSV(ccall, 5);
    args -= 1;
    jl_value_t *rt = args[2];
    jl_value_t *at = args[3];
    assert(jl_is_quotenode(args[4]));
    jl_sym_t *cc_sym = *(jl_sym_t**)args[4];
    size_t nccallargs = jl_unbox_long(args[5]);
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
        assert(i < nccallargs);
        return args[6 + i];
    };

    auto _is_libjulia_func = [&] (uintptr_t ptr, const char *name) {
        if ((uintptr_t)fptr == ptr)
            return true;
        return (!f_lib || f_lib == JL_DL_LIBNAME) && f_name && !strcmp(f_name, name);
    };
#define is_libjulia_func(name) _is_libjulia_func((uintptr_t)&(name), #name)

    static auto ptls_getter = &jl_get_ptls_states;
#ifdef _OS_LINUX_
    // directly accessing the address of an ifunc can cause linker issue on
    // some configurations (e.g. AArch64 + -Bsymbolic-functions).
    jl_dlsym(jl_dlopen(nullptr, 0), "jl_get_ptls_states", (void **)&ptls_getter, 0);
#endif

    // emit arguments
    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nccallargs);
    std::vector<bool> addressOf(0);
    for (size_t i = 0; i < nccallargs; i++) {
        // Julia (expression) value of current parameter
        jl_value_t *argi = ccallarg(i);

        addressOf.push_back(false);

        argv[i] = emit_expr(ctx, argi);
    }

    // emit roots
    SmallVector<Value*, 16> gc_uses;
    for (size_t i = nccallargs + 6; i <= nargs; i++) {
        // Julia (expression) value of current parameter gcroot
        jl_value_t *argi_root = args[i];
        if (jl_is_long(argi_root))
            continue;
        jl_cgval_t arg_root = emit_expr(ctx, argi_root);
        if (arg_root.Vboxed || arg_root.V) {
            gc_uses.push_back(arg_root.Vboxed ? arg_root.Vboxed : arg_root.V);
        }
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
    bool isVa;
    size_t nargt;
    Type *lrt;
    bool retboxed;
    bool static_rt;
    const std::string err = verify_ccall_sig(
            /* inputs:  */
            nccallargs, rt, at, unionall,
            ctx.spvals_ptr == NULL ? ctx.linfo->sparam_vals : NULL,
            "ccall",
            /* outputs: */
            nargt, isVa, lrt, retboxed, static_rt);
    if (!err.empty()) {
        emit_error(ctx, "ccall " + err);
        JL_GC_POP();
        return jl_cgval_t();
    }
    if (rt != args[2] && rt != (jl_value_t*)jl_any_type)
        jl_add_method_root(ctx, rt);
    function_sig_t sig("ccall", lrt, rt, retboxed,
                       (jl_svec_t*)at, unionall, nccallargs,
                       isVa, cc, llvmcall);
    for (size_t i = 0; i < nargt; i++) {
        jl_value_t *tti = jl_svecref(at, i);
        if (jl_is_vararg_type(tti))
            tti = jl_unwrap_vararg(tti);
        if (jl_is_abstract_ref_type(tti)) {
            if (!verify_ref_type(ctx, jl_tparam0(tti), unionall, i + 1, "ccall")) {
                JL_GC_POP();
                return jl_cgval_t();
            }
        }
    }

    // some special functions
    if (is_libjulia_func(jl_array_ptr)) {
        assert(lrt == T_size);
        assert(!isVa && !llvmcall && nargt == 1);
        assert(!addressOf.at(0));
        const jl_cgval_t &ary = argv[0];
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, ctx.builder.CreatePtrToInt(emit_unsafe_arrayptr(ctx, ary), lrt),
                                        retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_value_ptr)) {
        assert(retboxed ? lrt == T_prjlvalue : lrt == T_size);
        assert(!isVa && !llvmcall && nargt == 1);
        jl_value_t *tti = jl_svecref(at, 0);
        Value *ary;
        Type *largty;
        bool isboxed;
        if (addressOf.at(0)) {
            largty = T_pjlvalue;
            isboxed = true;
        }
        else if (jl_is_abstract_ref_type(tti)) {
            tti = (jl_value_t*)jl_voidpointer_type;
            largty = T_size;
            isboxed = false;
        }
        else {
            largty = julia_struct_to_llvm(tti, unionall, &isboxed);
        }
        if (isboxed) {
            ary = boxed(ctx, argv[0]);
        }
        else {
            ary = emit_unbox(ctx, largty, argv[0], tti);
        }
        JL_GC_POP();
        if (!retboxed) {
            return mark_or_box_ccall_result(
                    ctx,
                    emit_pointer_from_objref(ctx,
                        emit_bitcast(ctx, ary, T_prjlvalue)),
                    retboxed, rt, unionall, static_rt);
        }
        else {
            return mark_or_box_ccall_result(
                    ctx,
                    ctx.builder.CreateAddrSpaceCast(
                        ctx.builder.CreateIntToPtr(ary, T_pjlvalue),
                        T_prjlvalue), // TODO: this addrspace cast is invalid (implies that the value is rooted elsewhere)
                    retboxed, rt, unionall, static_rt);
        }
    }
    else if (is_libjulia_func(jl_cpu_pause)) {
        // Keep in sync with the julia_threads.h version
        assert(lrt == T_void);
        assert(!isVa && !llvmcall && nargt == 0);
#ifdef __MIC__
        // TODO
#elif defined(_CPU_X86_64_) || defined(_CPU_X86_)  /* !__MIC__ */
        static auto pauseinst = InlineAsm::get(FunctionType::get(T_void, false), "pause",
                                               "~{memory}", true);
        ctx.builder.CreateCall(pauseinst);
        JL_GC_POP();
        return ghostValue(jl_void_type);
#elif defined(_CPU_AARCH64_) || (defined(_CPU_ARM_) && __ARM_ARCH >= 7)
        static auto wfeinst = InlineAsm::get(FunctionType::get(T_void, false), "wfe",
                                             "~{memory}", true);
        ctx.builder.CreateCall(wfeinst);
        JL_GC_POP();
        return ghostValue(jl_void_type);
#else
        JL_GC_POP();
        return ghostValue(jl_void_type);
#endif
    }
    else if (is_libjulia_func(jl_cpu_wake)) {
        // Keep in sync with the julia_threads.h version
        assert(lrt == T_void);
        assert(!isVa && !llvmcall && nargt == 0);
#if JL_CPU_WAKE_NOOP == 1
        JL_GC_POP();
        return ghostValue(jl_void_type);
#elif defined(_CPU_AARCH64_) || (defined(_CPU_ARM_) && __ARM_ARCH >= 7)
        static auto sevinst = InlineAsm::get(FunctionType::get(T_void, false), "sev",
                                             "~{memory}", true);
        ctx.builder.CreateCall(sevinst);
        JL_GC_POP();
        return ghostValue(jl_void_type);
#endif
    }
    else if (is_libjulia_func(jl_gc_safepoint)) {
        assert(lrt == T_void);
        assert(!isVa && !llvmcall && nargt == 0);
        JL_GC_POP();
        ctx.builder.CreateCall(prepare_call(gcroot_flush_func));
        emit_signal_fence(ctx);
        ctx.builder.CreateLoad(ctx.signalPage, true);
        emit_signal_fence(ctx);
        return ghostValue(jl_void_type);
    }
    else if (_is_libjulia_func((uintptr_t)ptls_getter, "jl_get_ptls_states")) {
        assert(lrt == T_size);
        assert(!isVa && !llvmcall && nargt == 0);
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx,
            ctx.builder.CreatePtrToInt(ctx.ptlsStates, lrt),
            retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_threadid)) {
        assert(lrt == T_int16);
        assert(!isVa && !llvmcall && nargt == 0);
        JL_GC_POP();
        Value *ptls_i16 = emit_bitcast(ctx, ctx.ptlsStates, T_pint16);
        const int tid_offset = offsetof(jl_tls_states_t, tid);
        Value *ptid = ctx.builder.CreateGEP(ptls_i16, ConstantInt::get(T_size, tid_offset / 2));
        return mark_or_box_ccall_result(ctx,
            tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(ptid)),
            retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(jl_sigatomic_begin)) {
        assert(lrt == T_void);
        assert(!isVa && !llvmcall && nargt == 0);
        JL_GC_POP();
        ctx.builder.CreateCall(prepare_call(gcroot_flush_func));
        Value *pdefer_sig = emit_defer_signal(ctx);
        Value *defer_sig = ctx.builder.CreateLoad(pdefer_sig);
        defer_sig = ctx.builder.CreateAdd(defer_sig,
                                      ConstantInt::get(T_sigatomic, 1));
        ctx.builder.CreateStore(defer_sig, pdefer_sig);
        emit_signal_fence(ctx);
        return ghostValue(jl_void_type);
    }
    else if (is_libjulia_func(jl_sigatomic_end)) {
        assert(lrt == T_void);
        assert(!isVa && !llvmcall && nargt == 0);
        JL_GC_POP();
        ctx.builder.CreateCall(prepare_call(gcroot_flush_func));
        Value *pdefer_sig = emit_defer_signal(ctx);
        Value *defer_sig = ctx.builder.CreateLoad(pdefer_sig);
        emit_signal_fence(ctx);
        error_unless(ctx,
                ctx.builder.CreateICmpNE(defer_sig, ConstantInt::get(T_sigatomic, 0)),
                "sigatomic_end called in non-sigatomic region");
        defer_sig = ctx.builder.CreateSub(
                defer_sig,
                ConstantInt::get(T_sigatomic, 1));
        ctx.builder.CreateStore(defer_sig, pdefer_sig);
        BasicBlock *checkBB = BasicBlock::Create(jl_LLVMContext, "check",
                                                 ctx.f);
        BasicBlock *contBB = BasicBlock::Create(jl_LLVMContext, "cont");
        ctx.builder.CreateCondBr(
                ctx.builder.CreateICmpEQ(defer_sig, ConstantInt::get(T_sigatomic, 0)),
                checkBB, contBB);
        ctx.builder.SetInsertPoint(checkBB);
        ctx.builder.CreateLoad(
                ctx.builder.CreateConstGEP1_32(ctx.signalPage, -1),
                true);
        ctx.builder.CreateBr(contBB);
        ctx.f->getBasicBlockList().push_back(contBB);
        ctx.builder.SetInsertPoint(contBB);
        return ghostValue(jl_void_type);
    }
    else if (is_libjulia_func(jl_array_isassigned) &&
             argv[1].typ == (jl_value_t*)jl_ulong_type) {
        assert(!isVa && !llvmcall && nargt == 2 && !addressOf.at(0) && !addressOf.at(1));
        jl_value_t *aryex = ccallarg(0);
        const jl_cgval_t &aryv = argv[0];
        const jl_cgval_t &idxv = argv[1];
        jl_datatype_t *arydt = (jl_datatype_t*)jl_unwrap_unionall(aryv.typ);
        if (jl_is_array_type(arydt)) {
            jl_value_t *ety = jl_tparam0(arydt);
            if (jl_array_store_unboxed(ety)) {
                JL_GC_POP();
                return mark_or_box_ccall_result(ctx, ConstantInt::get(T_int32, 1),
                                                false, rt, unionall, static_rt);
            }
            else if (!jl_has_free_typevars(ety)) {
                Value *idx = emit_unbox(ctx, T_size, idxv, (jl_value_t*)jl_ulong_type);
                Value *arrayptr = emit_bitcast(ctx, emit_arrayptr(ctx, aryv, aryex), T_ppjlvalue);
                Value *slot_addr = ctx.builder.CreateGEP(arrayptr, idx);
                Value *load = tbaa_decorate(tbaa_arraybuf, ctx.builder.CreateLoad(slot_addr));
                Value *res = ctx.builder.CreateZExt(ctx.builder.CreateICmpNE(load, V_null), T_int32);
                JL_GC_POP();
                return mark_or_box_ccall_result(ctx, res, retboxed, rt, unionall, static_rt);
            }
        }
    }
    else if (is_libjulia_func(jl_string_ptr)) {
        assert(lrt == T_size);
        assert(!isVa && !llvmcall && nargt == 1 && !addressOf.at(0));
        Value *obj = emit_pointer_from_objref(ctx, boxed(ctx, argv[0]));
        Value *strp = ctx.builder.CreateAdd(obj, ConstantInt::get(T_size, sizeof(void*)));
        JL_GC_POP();
        return mark_or_box_ccall_result(ctx, strp, retboxed, rt, unionall, static_rt);
    }
    else if (is_libjulia_func(memcpy)) {
        const jl_cgval_t &dst = argv[0];
        const jl_cgval_t &src = argv[1];
        const jl_cgval_t &n = argv[2];
        ctx.builder.CreateMemCpy(
                ctx.builder.CreateIntToPtr(
                    emit_unbox(ctx, T_size, dst, (jl_value_t*)jl_voidpointer_type), T_pint8),
                ctx.builder.CreateIntToPtr(
                    emit_unbox(ctx, T_size, src, (jl_value_t*)jl_voidpointer_type), T_pint8),
                emit_unbox(ctx, T_size, n, (jl_value_t*)jl_ulong_type), 1,
                false);
        JL_GC_POP();
        return ghostValue(jl_void_type);
    }

    jl_cgval_t retval = sig.emit_a_ccall(
            ctx,
            symarg,
            nargt,
            addressOf,
            argv,
            gc_uses,
            static_rt);
    JL_GC_POP();
    return retval;
}

jl_cgval_t function_sig_t::emit_a_ccall(
        jl_codectx_t &ctx,
        const native_sym_arg_t &symarg,
        size_t nargt,
        std::vector<bool> &addressOf,
        jl_cgval_t *argv,
        SmallVector<Value*, 16> &gc_uses,
        bool static_rt) const
{
    if (!err_msg.empty()) {
        emit_error(ctx, err_msg);
        return jl_cgval_t();
    }

    FunctionType *functype = this->functype();

    // save place before arguments, for possible insertion of temp arg area saving code.
    BasicBlock::InstListType &instList = ctx.builder.GetInsertBlock()->getInstList();
    Instruction *savespot = instList.empty() ? NULL : &instList.back();

    bool needStackRestore = false;
    Value **argvals = (Value**) alloca((nargs + sret) * sizeof(Value*));
    for (size_t ai = 0; ai < nargs; ai++) {
        // Current C function parameter
        Type *largty; // LLVM type of the current parameter
        bool toboxed;
        jl_value_t *jargty; // Julia type of the current parameter
        bool byRef; // Argument attributes
        if (isVa && ai >= nargt - 1) {
            largty = fargt.at(nargt - 1);
            toboxed = fargt_isboxed.at(nargt - 1);
            jargty = jl_unwrap_vararg(jl_svecref(at, nargt - 1));
            byRef = byRefList.at(nargt - 1);
        }
        else {
            largty = fargt.at(ai);
            toboxed = fargt_isboxed.at(ai);
            jargty = jl_svecref(at, ai);
            byRef = byRefList.at(ai);
        }
        Type *pargty = ai + sret < fargt_sig.size() ? fargt_sig.at(ai + sret) : fargt_vasig;
        jl_cgval_t &arg = argv[ai];

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
        if (!addressOf.at(ai)) {
            if (jl_is_abstract_ref_type(jargty)) {
                if (!jl_is_cpointer_type(arg.typ)) {
                    emit_cpointercheck(ctx, arg, "ccall: argument to Ref{T} is not a pointer");
                    arg.typ = (jl_value_t*)jl_voidpointer_type;
                    arg.isboxed = false;
                }
                jargty_in_env = (jl_value_t*)jl_voidpointer_type;
            }

            v = julia_to_native(ctx, largty, toboxed, jargty_in_env, unionall_env, arg, byRef,
                                ai, &needStackRestore);
            bool issigned = jl_signed_type && jl_subtype(jargty, (jl_value_t*)jl_signed_type);
            if (byRef) {
                v = decay_derived(v);
                // julia_to_native should already have done the alloca and store
                assert(v->getType() == pargty);
            }
            else {
                v = llvm_type_rewrite(ctx, v, pargty, issigned);
            }
        }
        else {
            if (jl_is_abstract_ref_type(jargty)) {
                emit_error(ctx, "ccall: & on a Ref{T} argument is invalid");
                return jl_cgval_t();
            }
            v = julia_to_address(ctx, largty, jargty_in_env, unionall_env, arg,
                                 ai, &needStackRestore);
            if (isa<UndefValue>(v)) {
                return jl_cgval_t();
            }
            assert((!toboxed && !byRef) || isa<UndefValue>(v));
        }

        if (isa<UndefValue>(v)) {
            return jl_cgval_t();
        }
        assert(v->getType() == pargty);
        argvals[ai + sret] = v;
    }

    Value *result = NULL;
    // First, if the ABI requires us to provide the space for the return
    // argument, allocate the box and store that as the first argument type
    bool sretboxed = false;
    if (sret) {
        assert(!retboxed && jl_is_datatype(rt) && "sret return type invalid");
        if (jl_justbits(rt)) {
            result = emit_static_alloca(ctx, lrt);
            argvals[0] = ctx.builder.CreateBitCast(result, fargt_sig.at(0));
        }
        else {
            // XXX: result needs to be zero'd and given a GC root here
            assert(jl_datatype_size(rt) > 0 && "sret shouldn't be a singleton instance");
            result = emit_allocobj(ctx, jl_datatype_size(rt),
                                   literal_pointer_val(ctx, (jl_value_t*)rt));
            sretboxed = true;
            gc_uses.push_back(result);
            argvals[0] = ctx.builder.CreateIntToPtr(emit_pointer_from_objref(ctx, result), fargt_sig.at(0));
        }
    }

    Instruction *stacksave = NULL;
    if (needStackRestore) {
        stacksave = CallInst::Create(Intrinsic::getDeclaration(jl_Module,
                                                               Intrinsic::stacksave));
        if (savespot) {
            instList.insertAfter(savespot->getIterator(), stacksave);
        }
        else {
            instList.push_front(stacksave);
        }
    }

    // make LLVM function object for the target
    // keep this close to the function call, so that the compiler can
    // optimize the global pointer load in the common case
    Value *llvmf;
    if (llvmcall) {
        if (symarg.jl_ptr != NULL) {
            jl_error("llvmcall doesn't support dynamic pointers");
        }
        else if (symarg.fptr != NULL) {
            jl_error("llvmcall doesn't support static pointers");
        }
        else if (symarg.f_lib != NULL) {
            jl_error("llvmcall doesn't support dynamic libraries");
        }
        else {
            assert(symarg.f_name != NULL);
            llvmf = jl_Module->getOrInsertFunction(symarg.f_name, functype);
            if (!isa<Function>(llvmf) || cast<Function>(llvmf)->getIntrinsicID() == Intrinsic::not_intrinsic)
                jl_error("llvmcall only supports intrinsic calls");
        }
    }
    else if (symarg.jl_ptr != NULL) {
        null_pointer_check(ctx, symarg.jl_ptr);
        Type *funcptype = PointerType::get(functype, 0);
        llvmf = ctx.builder.CreateIntToPtr(symarg.jl_ptr, funcptype);
    }
    else if (symarg.fptr != NULL) {
        Type *funcptype = PointerType::get(functype, 0);
        llvmf = literal_static_pointer_val(ctx, (void*)(uintptr_t)symarg.fptr, funcptype);
        if (imaging_mode)
            jl_printf(JL_STDERR,"WARNING: literal address used in ccall for %s; code cannot be statically compiled\n", symarg.f_name);
    }
    else {
        assert(symarg.f_name != NULL);

        PointerType *funcptype = PointerType::get(functype, 0);
        if (imaging_mode) {
            // vararg requires musttail,
            // but musttail is incompatible with noreturn.
            if (functype->isVarArg())
                llvmf = runtime_sym_lookup(ctx, funcptype, symarg.f_lib, symarg.f_name, ctx.f);
            else
                llvmf = emit_plt(ctx, functype, attributes, cc, symarg.f_lib, symarg.f_name);
        }
        else {
            void *symaddr;
            if (!jl_dlsym(jl_get_library(symarg.f_lib), symarg.f_name, &symaddr, 0)) {
                std::stringstream msg;
                msg << "ccall: could not find function ";
                msg << symarg.f_name;
                if (symarg.f_lib != NULL) {
#ifdef _OS_WINDOWS_
                    assert(symarg.f_lib != JL_EXE_LIBNAME && symarg.f_lib != JL_DL_LIBNAME);
#endif
                    msg << " in library ";
                    msg << symarg.f_lib;
                }
                emit_error(ctx, msg.str());
                return jl_cgval_t();
            }
            // since we aren't saving this code, there's no sense in
            // putting anything complicated here: just JIT the function address
            llvmf = literal_static_pointer_val(ctx, symaddr, funcptype);
        }
    }

    OperandBundleDef OpBundle("jl_roots", gc_uses);
    // the actual call
    Value *ret = ctx.builder.CreateCall(prepare_call(llvmf),
                                    ArrayRef<Value*>(&argvals[0], nargs + sret),
                                    ArrayRef<OperandBundleDef>(&OpBundle, gc_uses.empty() ? 0 : 1));
    ((CallInst*)ret)->setAttributes(attributes);

    if (cc != CallingConv::C)
        ((CallInst*)ret)->setCallingConv(cc);
    if (!sret)
        result = ret;
    if (needStackRestore) {
        assert(stacksave != NULL);
        ctx.builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::stackrestore), stacksave);
    }
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
    }
    else if (sret) {
        jlretboxed = sretboxed;
        if (!jlretboxed) {
            // something alloca'd above is SSA
            if (static_rt)
                return mark_julia_slot(result, rt, NULL, tbaa_stack);
            result = ctx.builder.CreateLoad(result);
        }
    }
    else {
        Type *jlrt = julia_type_to_llvm(rt, &jlretboxed); // compute the real "julian" return type and compute whether it is boxed
        if (jlretboxed) {
            jlrt = T_prjlvalue;
        }
        if (type_is_ghost(jlrt)) {
            return ghostValue(rt);
        }
        else if (jl_is_datatype(rt) && jl_is_datatype_singleton((jl_datatype_t*)rt)) {
            return mark_julia_const(((jl_datatype_t*)rt)->instance);
        }
        else if (jlretboxed && !retboxed) {
            assert(jl_is_datatype(rt));
            if (static_rt) {
                Value *runtime_bt = literal_pointer_val(ctx, rt);
                size_t rtsz = jl_datatype_size(rt);
                assert(rtsz > 0);
                Value *strct = emit_allocobj(ctx, rtsz, runtime_bt);
                MDNode *tbaa = jl_is_mutable(rt) ? tbaa_mutab : tbaa_immut;
                int boxalign = jl_datatype_align(rt);
                // copy the data from the return value to the new struct
                const DataLayout &DL = jl_data_layout;
                auto resultTy = result->getType();
                if (DL.getTypeStoreSize(resultTy) > rtsz) {
                    // ARM and AArch64 can use a LLVM type larger than the julia type.
                    // When this happens, cast through memory.
                    auto slot = emit_static_alloca(ctx, resultTy);
                    slot->setAlignment(boxalign);
                    ctx.builder.CreateAlignedStore(result, slot, boxalign);
                    emit_memcpy(ctx, strct, tbaa, slot, tbaa, rtsz, boxalign, tbaa);
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
