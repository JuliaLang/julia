// This file is a part of Julia. License is MIT: http://julialang.org/license

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
LazyModule<typename std::remove_reference<Func>::type>
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
bool runtime_sym_gvs(const char *f_lib, const char *f_name, MT &&M,
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
        libsym = *(void**)jl_get_global(libptrgv);
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
        void *addr = jl_dlsym_e(libsym, f_name);
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

static Value *runtime_sym_lookup(PointerType *funcptype, const char *f_lib,
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
    BasicBlock *enter_bb = builder.GetInsertBlock();
    BasicBlock *dlsym_lookup = BasicBlock::Create(jl_LLVMContext, "dlsym");
    BasicBlock *ccall_bb = BasicBlock::Create(jl_LLVMContext, "ccall");
    Constant *initnul = ConstantPointerNull::get((PointerType*)T_pvoidfunc);
    LoadInst *llvmf_orig = builder.CreateAlignedLoad(llvmgv, sizeof(void*));
    // This in principle needs a consume ordering so that load from
    // this pointer sees valid value. However, this is not supported by
    // LLVM (or agreed on in the C/C++ standard FWIW) and should be
    // almost impossible to happen on every platform we support since this
    // ordering is enforced by the hardware and LLVM has to speculate a
    // invalid load from the `cglobal` but doesn't depend on the `cglobal`
    // value for this to happen.
    // llvmf_orig->setAtomic(AtomicOrdering::Consume);
    builder.CreateCondBr(builder.CreateICmpNE(llvmf_orig, initnul),
                         ccall_bb, dlsym_lookup);

    assert(f->getParent() != NULL);
    f->getBasicBlockList().push_back(dlsym_lookup);
    builder.SetInsertPoint(dlsym_lookup);
    Value *libname;
    if (runtime_lib) {
        libname = jl_codectx_t::stringConstPtr(builder, f_lib);
    }
    else {
        libname = jl_codectx_t::literal_static_pointer_val(f_lib, T_pint8);
    }
#ifdef LLVM37
    Value *llvmf = builder.CreateCall(prepare_call(builder, jldlsym_func), { libname, stringConstPtr(builder, f_name), libptrgv });
#else
    Value *llvmf = builder.CreateCall3(prepare_call(builder, jldlsym_func), libname, stringConstPtr(builder, f_name), libptrgv);
#endif
    auto store = builder.CreateAlignedStore(llvmf, llvmgv, sizeof(void*));
#  ifdef LLVM39
    store->setAtomic(AtomicOrdering::Release);
#  else
    store->setAtomic(Release);
#  endif
    builder.CreateBr(ccall_bb);

    f->getBasicBlockList().push_back(ccall_bb);
    builder.SetInsertPoint(ccall_bb);
    PHINode *p = builder.CreatePHI(T_pvoidfunc, 2);
    p->addIncoming(llvmf_orig, enter_bb);
    p->addIncoming(llvmf, dlsym_lookup);
    return builder.CreatePointerCast(p, funcptype);
}

Value *runtime_sym_lookup(PointerType *funcptype, const char *f_lib,
                          const char *f_name, Function *f)
{
    GlobalVariable *libptrgv;
    GlobalVariable *llvmgv;
    bool runtime_lib = runtime_sym_gvs(f_lib, f_name, f->getParent(),
                                       libptrgv, llvmgv);
    libptrgv = prepare_global(libptrgv);
    llvmgv = prepare_global(llvmgv);
    return runtime_sym_lookup(funcptype, f_lib, f_name, f, libptrgv, llvmgv,
                              runtime_lib);
}

// Map from distinct callee's to its GOT entry.
// In principle the attribute, function type and calling convention
// don't need to be part of the key but it seems impossible to forward
// all the arguments without writing assembly directly.
// This doesn't matter too much in reality since a single function is usually
// not called with multiple signatures.
static DenseMap<AttributeSet,
                std::map<std::tuple<GlobalVariable*,FunctionType*,
                                    CallingConv::ID>,GlobalVariable*>> allPltMap;

// Emit a "PLT" entry that will be lazily initialized
// when being called the first time.
static GlobalVariable *emit_plt_thunk(Module *M, FunctionType *functype, const AttributeSet &attrs,
                                      CallingConv::ID cc, const char *f_lib, const char *f_name,
                                      GlobalVariable *libptrgv, GlobalVariable *llvmgv,
                                      void *symaddr, bool runtime_lib)
{
    PointerType *funcptype = PointerType::get(functype, 0);
    libptrgv = ::prepare_global(libptrgv, M);
    llvmgv = ::prepare_global(llvmgv, M);
    BasicBlock *old = builder.GetInsertBlock();
    DebugLoc olddl = builder.getCurrentDebugLocation();
    DebugLoc noDbg;
    builder.SetCurrentDebugLocation(noDbg);
    std::stringstream funcName;
    funcName << "jlplt_" << f_name << "_" << globalUnique++;
    auto fname = funcName.str();
    Function *plt = Function::Create(functype,
                                     GlobalVariable::ExternalLinkage,
                                     fname, M);
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
    builder.SetInsertPoint(b0);
    Value *ptr = runtime_sym_lookup(funcptype, f_lib, f_name, plt, libptrgv,
                                    llvmgv, runtime_lib);
    auto store = builder.CreateAlignedStore(builder.CreateBitCast(ptr, T_pvoidfunc), got, sizeof(void*));
#  ifdef LLVM39
    store->setAtomic(AtomicOrdering::Release);
#  else
    store->setAtomic(Release);
#  endif
    SmallVector<Value*, 16> args;
    for (Function::arg_iterator arg = plt->arg_begin(), arg_e = plt->arg_end(); arg != arg_e; ++arg)
        args.push_back(&*arg);
    CallInst *ret = builder.CreateCall(ptr, ArrayRef<Value*>(args));
    ret->setAttributes(attrs);
    if (cc != CallingConv::C)
        ret->setCallingConv(cc);
    // NoReturn function can trigger LLVM verifier error when declared as
    // MustTail since other passes might replace the `ret` with
    // `unreachable` (LLVM should probably accept `unreachable`).
    if (attrs.hasAttribute(AttributeSet::FunctionIndex,
                           Attribute::NoReturn)) {
        builder.CreateUnreachable();
    }
    else {
        // musttail support is very bad on ARM, PPC, PPC64 (as of LLVM 3.9)
        // Known failures includes vararg (not needed here) and sret.
#if defined(LLVM37) && (defined(_CPU_X86_) || defined(_CPU_X86_64_) || \
                    defined(_CPU_AARCH64_))
        ret->setTailCallKind(CallInst::TCK_MustTail);
#endif
        if (functype->getReturnType() == T_void) {
            builder.CreateRetVoid();
        }
        else {
            builder.CreateRet(ret);
        }
    }
    builder.SetInsertPoint(old);
    builder.SetCurrentDebugLocation(olddl);
    got = global_proto(got); // exchange got for the permanent global before jl_finalize_module destroys it
    jl_finalize_module(M, true);

    auto shadowgot =
        cast<GlobalVariable>(shadow_output->getNamedValue(gname));
    auto shadowplt = cast<Function>(shadow_output->getNamedValue(fname));
    shadowgot->setInitializer(ConstantExpr::getBitCast(shadowplt,
                                                       T_pvoidfunc));
    return got;
}

Value *emit_plt(FunctionType *functype, const AttributeSet &attrs,
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
    LoadInst *got_val = builder.CreateAlignedLoad(got, sizeof(void*));
    // See comment in `runtime_sym_lookup` above. This in principle needs a
    // consume ordering too. This is even less likely to cause issue though
    // since the only thing we do to this loaded pointer is to call it
    // immediately.
    // got_val->setAtomic(AtomicOrdering::Consume);
    return builder.CreateBitCast(got_val, funcptype);
}

// --- ABI Implementations ---
// Partially based on the LDC ABI implementations licensed under the BSD 3-clause license

class AbiLayout {
public:
    virtual bool use_sret(jl_datatype_t *ty) = 0;
    virtual void needPassByRef(jl_datatype_t *ty, bool *byRef, bool *inReg) = 0;
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
  typedef ABI_LLVMLayout AbiState;
#elif defined _CPU_X86_64_
#  if defined _OS_WINDOWS_
     typedef ABI_Win64Layout AbiState;
#  else
     typedef ABI_x86_64Layout AbiState;
#  endif
#elif defined _CPU_X86_
#  if defined _OS_WINDOWS_
     typedef ABI_Win32Layout AbiState;
#  else
     typedef ABI_x86Layout AbiState;
#  endif
#elif defined _CPU_ARM_
  typedef ABI_ArmLayout AbiState;
#elif defined _CPU_AARCH64_
  typedef ABI_AArch64Layout AbiState;
#elif defined _CPU_PPC64_
  typedef ABI_PPC64leLayout AbiState;
#else
#  warning "ccall is defaulting to llvm ABI, since no platform ABI has been defined for this CPU/OS combination"
  typedef ABI_LLVMLayout AbiState;
#endif


Value *llvm_type_rewrite(Value *v, Type *from_type, Type *target_type,
        bool tojulia, /* only matters if byref is set (declares the direction of the byref attribute) */
        bool byref, /* only applies to arguments, set false for return values -- effectively the same as jl_cgval_t.ispointer() */
        bool issigned /* determines whether an integer value should be zero or sign extended */
        )
{
    if (v->getType() == T_void)
        return UndefValue::get(target_type); // convert undef (unreachable) -> undef (target_type)

    if (byref) {
        if (tojulia) {
            Type *ptarget_type = PointerType::get(target_type, 0);
            if (v->getType() != ptarget_type)
                v = builder.CreatePointerCast(v, ptarget_type);
            return builder.CreateAlignedLoad(v, 1); // unknown alignment from C
        }
        else {
            // julia_to_native should already have done the alloca and store
            if (v->getType() != target_type)
                v = builder.CreatePointerCast(v, target_type);
            return v;
        }
    }
    assert(v->getType() == from_type);

    if (target_type == from_type) {
        return v;
    }

    assert(from_type->isPointerTy() == target_type->isPointerTy()); // expect that all ABIs consider all pointers to be equivalent
    if (target_type->isPointerTy()) {
        return builder.CreatePointerCast(v, target_type);
    }

    // simple integer and float widening & conversion cases
    if (from_type->getPrimitiveSizeInBits() > 0 && target_type->getPrimitiveSizeInBits() == from_type->getPrimitiveSizeInBits()) {
        return emit_bitcast(v, target_type);
    }
    if (target_type->isFloatingPointTy() && from_type->isFloatingPointTy()) {
        if (target_type->getPrimitiveSizeInBits() > from_type->getPrimitiveSizeInBits())
            return builder.CreateFPExt(v, target_type);
        else if (target_type->getPrimitiveSizeInBits() < from_type->getPrimitiveSizeInBits())
            return builder.CreateFPTrunc(v, target_type);
        else
            return v;
    }
    if (target_type->isIntegerTy() && from_type->isIntegerTy()) {
        if (issigned)
            return builder.CreateSExtOrTrunc(v, target_type);
        else
            return builder.CreateZExtOrTrunc(v, target_type);
    }

    // one or both of from_type and target_type is a VectorType or AggregateType
    // LLVM doesn't allow us to cast these values directly, so
    // we need to use this alloca copy trick instead
    // NOTE: it is assumed that the ABI has ensured that sizeof(from_type) == sizeof(target_type)
    Value *mem = emit_static_alloca(target_type);
    builder.CreateStore(v, builder.CreatePointerCast(mem, from_type->getPointerTo()));
    return builder.CreateLoad(mem);
}

// --- argument passing and scratch space utilities ---

// Emit code to convert argument to form expected by C ABI
// to = desired LLVM type
// jlto = Julia type of formal argument
// jvinfo = value of actual argument
Value *julia_to_native(Type *to, bool toboxed, jl_value_t *jlto, const jl_cgval_t &jvinfo,
                       bool addressOf, bool byRef, bool inReg,
                       bool tojulia, int argn,
                       bool *needStackRestore)
{
    // We're passing Any
    if (toboxed) {
        assert(!addressOf && !byRef); // don't expect any ABI to pass pointers by pointer
        return boxed(jvinfo);
    }
    assert(jl_is_leaf_type(jlto));

    // TODO: Tuple arguments are currently undefined behavior, for defining the calling convention that they match to.
    // XXX: However, they are used in the llvmcall test, so I guess it'll have to stay.
    //if (jl_is_tuple(jlto) || jl_is_tuple_type(jlto)) {
    //    emit_error("ccall: unimplemented: unboxed tuple argument type");
    //    return UndefValue::get(to);
    //}

    jl_value_t *ety = jlto;
    if (addressOf) {
        if (!jl_is_cpointer_type(jlto)) {
            emit_error("ccall: & on argument was not matched by Ptr{T} argument type");
            return UndefValue::get(T_void);
        }
        ety = jl_tparam0(jlto);
        if (jlto == (jl_value_t*)jl_voidpointer_type)
            ety = jvinfo.typ; // skip the type-check
        assert(to->isPointerTy());
    }
    if (jvinfo.typ != ety && ety != (jl_value_t*)jl_any_type) {
        if (!addressOf && ety == (jl_value_t*)jl_voidpointer_type) {
            // allow a bit more flexibility for what can be passed to (void*) due to Ref{T} conversion behavior below
            if (!jl_is_cpointer_type(jvinfo.typ)) {
                // emit a typecheck, if not statically known to be correct
                std::stringstream msg;
                msg << "ccall argument ";
                msg << argn;
                emit_cpointercheck(jvinfo, msg.str());
            }
        }
        else {
            // emit a typecheck, if not statically known to be correct
            std::stringstream msg;
            msg << "ccall argument ";
            msg << argn;
            emit_typecheck(jvinfo, ety, msg.str());
        }
    }

    if (!addressOf && !byRef)
        return emit_unbox(to, jvinfo, ety);

    if (addressOf && jvinfo.isboxed) {
        if (!jl_is_abstracttype(ety)) {
            if (jl_is_mutable_datatype(ety)) {
                // no copy, just reference the data field
                return data_pointer(jvinfo, to);
            }
            else if (jl_is_immutable_datatype(ety) && jlto != (jl_value_t*)jl_voidpointer_type) {
                // yes copy
                Value *nbytes;
                AllocaInst *ai;
                if (jl_is_leaf_type(ety)) {
                    int nb = jl_datatype_size(ety);
                    nbytes = ConstantInt::get(T_int32, nb);
                    ai = emit_static_alloca(T_int8, nb);
                }
                else {
                    nbytes = emit_datatype_size(emit_typeof_boxed(jvinfo));
                    ai = builder.CreateAlloca(T_int8, nbytes);
                    *needStackRestore = true;
                }
                ai->setAlignment(16);
                prepare_call(
                    builder.CreateMemCpy(ai, data_pointer(jvinfo, T_pint8), nbytes, sizeof(void*))->getCalledValue()); // minimum gc-alignment in julia is pointer size
                return emit_bitcast(ai, to);
            }
        }
        // emit maybe copy
        *needStackRestore = true;
        Value *jvt = emit_typeof_boxed(jvinfo);
        BasicBlock *mutableBB = BasicBlock::Create(jl_LLVMContext, "is-mutable", this->f);
        BasicBlock *immutableBB = BasicBlock::Create(jl_LLVMContext, "is-immutable", this->f);
        BasicBlock *afterBB = BasicBlock::Create(jl_LLVMContext, "after", this->f);
        Value *ismutable = emit_datatype_mutabl(jvt);
        builder.CreateCondBr(ismutable, mutableBB, immutableBB);
        builder.SetInsertPoint(mutableBB);
        Value *p1 = data_pointer(jvinfo, to);
        builder.CreateBr(afterBB);
        builder.SetInsertPoint(immutableBB);
        Value *nbytes = emit_datatype_size(jvt);
        AllocaInst *ai = builder.CreateAlloca(T_int8, nbytes);
        ai->setAlignment(16);
        prepare_call(builder.CreateMemCpy(ai, data_pointer(jvinfo, T_pint8), nbytes, sizeof(void*))->getCalledValue()); // minimum gc-alignment in julia is pointer size
        Value *p2 = emit_bitcast(ai, to);
        builder.CreateBr(afterBB);
        builder.SetInsertPoint(afterBB);
        PHINode *p = builder.CreatePHI(to, 2);
        p->addIncoming(p1, mutableBB);
        p->addIncoming(p2, immutableBB);
        return p;
    }

    // pass the address of an alloca'd thing, not a box
    // since those are immutable.
    if (addressOf)
        to = to->getContainedType(0);
    Value *slot = emit_static_alloca(to);
    if (!jvinfo.ispointer()) {
        builder.CreateStore(emit_unbox(to, jvinfo, ety), slot);
    }
    else {
        prepare_call(builder.CreateMemCpy(slot, data_pointer(jvinfo, slot->getType()),
                    (uint64_t)jl_datatype_size(ety),
                    (uint64_t)((jl_datatype_t*)ety)->layout->alignment)->getCalledValue());
        mark_gc_use(jvinfo);
    }
    return slot;
}

typedef struct {
    Value *jl_ptr;  // if the argument is a run-time computed pointer
    void (*fptr)(void);     // if the argument is a constant pointer
    const char *f_name;   // if the symbol name is known
    const char *f_lib;    // if a library name is specified
} native_sym_arg_t;

// --- parse :sym or (:sym, :lib) argument into address info ---
native_sym_arg_t interpret_symbol_arg(jl_value_t *arg, const char *fname)
{
    jl_value_t *ptr = NULL;
    Value *jl_ptr=NULL;

    ptr = static_eval(arg, true);
    if (ptr == NULL) {
        jl_value_t *ptr_ty = expr_type(arg);
        jl_cgval_t arg1 = emit_expr(arg);
        if (!jl_is_cpointer_type(ptr_ty)) {
            emit_cpointercheck(arg1,
                               !strcmp(fname,"ccall") ?
                               "ccall: first argument not a pointer or valid constant expression" :
                               "cglobal: first argument not a pointer or valid constant expression");
        }
        arg1 = remark_julia_type(arg1, (jl_value_t*)jl_voidpointer_type);
        jl_ptr = emit_unbox(T_size, arg1, (jl_value_t*)jl_voidpointer_type);
    }

    void (*fptr)(void) = NULL;
    const char *f_name=NULL, *f_lib=NULL;
    jl_value_t *t0 = NULL, *t1 = NULL;
    JL_GC_PUSH3(&ptr, &t0, &t1);
    if (ptr != NULL) {
        if (jl_is_tuple(ptr) && jl_nfields(ptr)==1) {
            ptr = jl_fieldref(ptr,0);
        }
        if (jl_is_symbol(ptr))
            f_name = jl_symbol_name((jl_sym_t*)ptr);
        else if (jl_is_string(ptr))
            f_name = jl_string_data(ptr);
        if (f_name != NULL) {
            // just symbol, default to JuliaDLHandle
            // will look in process symbol table
#ifdef _OS_WINDOWS_
            f_lib = jl_dlfind_win32(f_name);
#endif
        }
        else if (jl_is_cpointer_type(jl_typeof(ptr))) {
            fptr = *(void(**)(void))jl_data_ptr(ptr);
        }
        else if (jl_is_tuple(ptr) && jl_nfields(ptr)>1) {
            jl_value_t *t0 = jl_fieldref(ptr,0);
            jl_value_t *t1 = jl_fieldref(ptr,1);
            if (jl_is_symbol(t0))
                f_name = jl_symbol_name((jl_sym_t*)t0);
            else if (jl_is_string(t0))
                f_name = jl_string_data(t0);
            else
                JL_TYPECHKS(fname, symbol, t0);
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
    JL_GC_POP();
    native_sym_arg_t r;
    r.jl_ptr = jl_ptr;
    r.fptr = fptr;
    r.f_name = f_name;
    r.f_lib = f_lib;
    return r;
}


jl_value_t* try_eval(jl_value_t *ex, const char *failure, bool compiletime=false)
{
    jl_value_t *constant = NULL;
    constant = static_eval(ex, true, true);
    if (constant || jl_is_ssavalue(ex))
        return constant;
    JL_TRY {
        constant = jl_interpret_toplevel_expr_in(this->module, ex, this->linfo);
    }
    JL_CATCH {
        if (compiletime)
            jl_rethrow_with_add(failure);
        if (failure)
            emit_error(failure);
        constant = NULL;
    }
    return constant;
}

// --- code generator for cglobal ---

jl_cgval_t emit_cglobal(jl_value_t **args, size_t nargs)
{
    JL_NARGS(cglobal, 1, 2);
    jl_value_t *rt=NULL;
    Value *res;
    JL_GC_PUSH1(&rt);

    if (nargs == 2) {
        rt = try_eval(args[2], "error interpreting cglobal pointer type");
        if (rt == NULL) {
            JL_GC_POP();
            return jl_cgval_t();
        }

        JL_TYPECHK(cglobal, type, rt);
        rt = (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type, jl_svec1(rt));
    }
    else {
        rt = (jl_value_t*)jl_voidpointer_type;
    }
    Type *lrt = julia_type_to_llvm(rt);
    if (lrt == NULL) lrt = T_pint8;

    native_sym_arg_t sym = interpret_symbol_arg(args[1], "cglobal");

    if (sym.jl_ptr != NULL) {
        res = builder.CreateIntToPtr(sym.jl_ptr, lrt);
    }
    else if (sym.fptr != NULL) {
        res = literal_static_pointer_val((void*)(uintptr_t)sym.fptr, lrt);
        if (imaging_mode)
            jl_printf(JL_STDERR,"WARNING: literal address used in cglobal for %s; code cannot be statically compiled\n", sym.f_name);
    }
    else {
        if (imaging_mode) {
            res = runtime_sym_lookup((PointerType*)lrt, sym.f_lib, sym.f_name, this->f);
        }
        else {
            void *symaddr = jl_dlsym_e(jl_get_library(sym.f_lib), sym.f_name);
            if (symaddr == NULL) {
                std::stringstream msg;
                msg << "cglobal: could not find symbol ";
                msg << sym.f_name;
                if (sym.f_lib != NULL) {
#ifdef _OS_WINDOWS_
                    assert((intptr_t)sym.f_lib != 1 && (intptr_t)sym.f_lib != 2);
#endif
                    msg << " in library ";
                    msg << sym.f_lib;
                }
                emit_error(msg.str());
            }
            // since we aren't saving this code, there's no sense in
            // putting anything complicated here: just JIT the address of the cglobal
            res = literal_static_pointer_val(symaddr, lrt);
        }
    }

    JL_GC_POP();
    return mark_julia_type(res, false, rt);
}

#ifdef USE_MCJIT
class FunctionMover : public ValueMaterializer
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

    #ifdef LLVM36
        // Clone debug info - Not yet public API
        // llvm::CloneDebugInfoMetadata(NewF,F,VMap);
    #endif

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

#if defined(LLVM39)
    virtual Value *materialize(Value *V) override
#elif defined(LLVM38)
    virtual Value *materializeDeclFor(Value *V) override
#else
    virtual Value *materializeValueFor (Value *V) override
#endif
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

                    #ifdef USE_ORCJIT
                    if (jl_ExecutionEngine->findSymbol(F->getName(), false))
                        return InjectFunctionProto(F);
                    #endif

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
#endif

// llvmcall(ir, (rettypes...), (argtypes...), args...)
jl_cgval_t emit_llvmcall(jl_value_t **args, size_t nargs)
{
    JL_NARGSV(llvmcall, 3);
    jl_value_t *rt = NULL, *at = NULL, *ir = NULL, *decl = NULL;
    jl_svec_t *stt = NULL;
    JL_GC_PUSH5(&ir, &rt, &at, &stt, &decl);
    at = try_eval(args[3], "error statically evaluating llvmcall argument tuple", true);
    rt = try_eval(args[2], "error statically evaluating llvmcall return type", true);
    ir = try_eval(args[1], "error statically evaluating llvm IR argument", true);
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

    stt = jl_alloc_svec(nargs - 3);

    for (size_t i = 0; i < nargs-3; ++i) {
        jl_svecset(stt, i, expr_type(args[4+i]));
    }

    // Generate arguments
    std::string arguments;
    llvm::raw_string_ostream argstream(arguments);
    jl_svec_t *tt = ((jl_datatype_t*)at)->parameters;
    jl_value_t *rtt = rt;

    size_t nargt = jl_svec_len(tt);
    Value **argvals = (Value**) alloca(nargt*sizeof(Value*));
    std::vector<llvm::Type*> argtypes;
    /*
     * Semantics for arguments are as follows:
     * If the argument type is immutable (including bitstype), we pass the loaded llvm value
     * type. Otherwise we pass a pointer to a jl_value_t.
     */
    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargt);
    for (size_t i = 0; i < nargt; ++i) {
        jl_value_t *tti = jl_svecref(tt,i);
        bool toboxed;
        Type *t = julia_type_to_llvm(tti, &toboxed);
        argtypes.push_back(t);
        if (4+i > nargs) {
            jl_error("Missing arguments to llvmcall!");
        }
        jl_value_t *argi = args[4 + i];
        jl_cgval_t &arg = argv[i];
        arg = emit_expr(argi);

        Value *v = julia_to_native(t, toboxed, tti, arg, false, false, false, false, i, NULL);
        // make sure args are rooted
        bool issigned = jl_signed_type && jl_subtype(tti, (jl_value_t*)jl_signed_type, 0);
        argvals[i] = llvm_type_rewrite(v, t, t, false, false, issigned);
    }

    Function *f;
    bool retboxed;
    Type *rettype = julia_type_to_llvm(rtt, &retboxed);
    if (isString) {
        // Make sure to find a unique name
        std::string ir_name;
        while(true) {
            std::stringstream name;
            name << (this->f->getName().str()) << "u" << i++;
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
#ifdef LLVM36
        Module *m = NULL;
        bool failed = parseAssemblyInto(llvm::MemoryBufferRef(ir_string,"llvmcall"),*jl_Module,Err);
        if (!failed)
            m = jl_Module;
#else
        Module *m = ParseAssemblyString(ir_string.c_str(),jl_Module,Err,jl_LLVMContext);
#endif
        if (m == NULL) {
            std::string message = "Failed to parse LLVM Assembly: \n";
            llvm::raw_string_ostream stream(message);
            Err.print("julia",stream,true);
            jl_error(stream.str().c_str());
        }
        f = m->getFunction(ir_name);
    }
    else {
        assert(isPtr);
        // Create Function skeleton
        f = (llvm::Function*)jl_unbox_voidpointer(ir);
        assert(f->getReturnType() == rettype);
        int i = 0;
        for (std::vector<Type *>::iterator it = argtypes.begin();
            it != argtypes.end(); ++it, ++i)
            assert(*it == f->getFunctionType()->getParamType(i));

#ifdef USE_MCJIT
        if (f->getParent() != jl_Module) {
            FunctionMover mover(jl_Module, f->getParent());
            f = mover.CloneFunction(f);
        }
#endif

        //f->dump();
        #ifndef LLVM35
        if (verifyFunction(*f,PrintMessageAction)) {
        #else
        llvm::raw_fd_ostream out(1,false);
        if (verifyFunction(*f,&out)) {
        #endif
            f->dump();
            jl_error("Malformed LLVM Function");
        }
    }

    /*
     * It might be tempting to just try to set the Always inline attribute on the function
     * and hope for the best. However, this doesn't work since that would require an inlining
     * pass (which is a Call Graph pass and cannot be managed by a FunctionPassManager). Instead
     * We are sneaky and call the inliner directly. This however doesn't work until we've actually
     * generated the entire function, so we need to store it in the context until the end of the
     * function. This also has the benefit of looking exactly like we cut/pasted it in in `code_llvm`.
     */

    // Since we dumped all of f's dependencies into the active module,
    // we cannot reasonably inline it, so leave it there and just emit
    // a regular call
    if (!isString) {
        static int llvmcallnumbering = 0;
        std::stringstream name;
        name << "jl_llvmcall" << llvmcallnumbering++;
        f->setName(name.str());
        f = cast<Function>(prepare_call(function_proto(f)));
    }
    else
        f->setLinkage(GlobalValue::LinkOnceODRLinkage);

    // the actual call
    builder.CreateCall(prepare_call(gcroot_flush_func));
    SmallVector<Value*, 16> gc_uses;
    for (size_t i = 0; i < nargt; ++i) {
        const jl_cgval_t &arg = argv[i];
        push_gc_use(gc_uses, arg);
    }
    // Mark GC use before **and** after the llvmcall to make sure the arguments
    // are alive during the llvmcall even if the llvmcall has `unreachable`.
    // If the llvmcall generates GC safepoint, it might need to emit its own
    // gckill.
    mark_gc_uses(gc_uses);
    CallInst *inst = builder.CreateCall(f, ArrayRef<Value*>(&argvals[0], nargt));
    if (isString)
        this->to_inline.push_back(inst);
    mark_gc_uses(gc_uses);

    JL_GC_POP();

    if (inst->getType() != rettype) {
        jl_error("Return type of llvmcall'ed function does not match declared return type");
    }

    return mark_julia_type(inst, retboxed, rtt);
}

// --- code generator for ccall itself ---

jl_cgval_t mark_or_box_ccall_result(Value *result, bool isboxed, jl_value_t *rt_expr, jl_value_t *rt, bool static_rt)
{
    if (!static_rt) {
        // box if concrete type was not statically known
        assert(rt == (jl_value_t*)jl_voidpointer_type);
        Value *runtime_bt = boxed(emit_expr(rt_expr));
        int nb = sizeof(void*);
        // TODO: can this be tighter than tbaa_value?
        return mark_julia_type(
            init_bits_value(emit_allocobj(nb, runtime_bt), result, tbaa_value),
            true, (jl_value_t*)jl_pointer_type);
    }
    return mark_julia_type(result, isboxed, rt);
}

static std::string generate_func_sig(
        Type **lrt, // input parameter of the llvm return type (from julia_struct_to_llvm)
        Type **prt, // out parameter of the llvm return type for the function signature
        int &sret, // out parameter for indicating whether return value has been moved to the first argument position
        std::vector<Type *> &fargt, // vector of llvm output types (julia_struct_to_llvm) for arguments (vararg is the last item, if applicable)
        std::vector<bool> &fargt_isboxed, // vector of whether the llvm output types is boxed for each argument (vararg is the last item, if applicable)
        std::vector<Type *> &fargt_sig, // vector of ABI coercion types for call signature
        Type *&fargt_vasig, // ABI coercion type for vararg list
        std::vector<bool> &inRegList, // vector of "inreg" parameters (vararg is the last item, if applicable)
        std::vector<bool> &byRefList, // vector of "byref" parameters (vararg is the last item, if applicable)
        AttributeSet &attributes, // vector of function call site attributes (vararg is the last item, if applicable)
        jl_value_t *rt, // julia return type
        jl_svec_t *tt, // tuple of julia argument types
        size_t nargs) // number of actual arguments (can be different from the size of tt when varargs)
{
    size_t nargt = jl_svec_len(tt);
    assert(rt && !jl_is_abstract_ref_type(rt));

    std::vector<AttrBuilder> paramattrs;
    std::unique_ptr<AbiLayout> abi(new AbiState());
    sret = 0;

    if (type_is_ghost(*lrt)) {
        *prt = *lrt = T_void;
    }
    else {
        if (!jl_is_datatype(rt) || ((jl_datatype_t*)rt)->layout == NULL || jl_is_cpointer_type(rt) || jl_is_array_type(rt)) {
            *prt = *lrt; // passed as pointer
        }
        else if (abi->use_sret((jl_datatype_t*)rt)) {
            paramattrs.push_back(AttrBuilder());
            paramattrs[0].clear();
#if !defined(_OS_WINDOWS_) || defined(LLVM35) // llvm used to use the old mingw ABI, skipping this marking works around that difference
            paramattrs[0].addAttribute(Attribute::StructRet);
#endif
            paramattrs[0].addAttribute(Attribute::NoAlias);
            fargt_sig.push_back(PointerType::get(*lrt, 0));
            sret = 1;
            *prt = *lrt;
        }
        else {
            *prt = abi->preferred_llvm_type((jl_datatype_t*)rt, true);
            if (*prt == NULL)
                *prt = *lrt;
        }
    }

    size_t i;
    bool current_isVa = false;
    for(i = 0; i < nargt;) {
        jl_value_t *tti = jl_svecref(tt,i);
        if (jl_is_vararg_type(tti)) {
            current_isVa = true;
            tti = jl_tparam0(tti);
        }
        Type *t = NULL;
        bool isboxed;
        Attribute::AttrKind av = Attribute::None;
        if (jl_is_abstract_ref_type(tti)) {
            if (jl_is_typevar(jl_tparam0(tti)))
                jl_error("ccall: argument type Ref should have an element type, not Ref{T}");
            tti = (jl_value_t*)jl_voidpointer_type;
            t = T_pint8;
            isboxed = false;
        }
        else {
            if (jl_is_cpointer_type(tti) && jl_is_typevar(jl_tparam0(tti)))
                jl_error("ccall: argument type Ptr should have an element type, not Ptr{T}");
            if (jl_is_bitstype(tti)) {
                // see pull req #978. need to annotate signext/zeroext for
                // small integer arguments.
                jl_datatype_t *bt = (jl_datatype_t*)tti;
                if (bt->size < 4) {
                    if (jl_signed_type && jl_subtype(tti, (jl_value_t*)jl_signed_type, 0))
                        av = Attribute::SExt;
                    else
                        av = Attribute::ZExt;
                }
            }

            t = julia_struct_to_llvm(tti, &isboxed);
            if (t == NULL || t == T_void) {
                std::stringstream msg;
                msg << "ccall: the type of argument ";
                msg << i+1;
                msg << " doesn't correspond to a C type";
                return msg.str();
            }
        }

        // Whether or not LLVM wants us to emit a pointer to the data
        bool byRef = false;

        // Whether or not to pass this in registers
        bool inReg = false;

        Type *pat;
        if (!jl_is_datatype(tti) || ((jl_datatype_t*)tti)->layout == NULL || jl_is_array_type(tti))
            tti = (jl_value_t*)jl_voidpointer_type; // passed as pointer

        abi->needPassByRef((jl_datatype_t*)tti, &byRef, &inReg);
        if (jl_is_cpointer_type(tti)) {
            pat = t;
        }
        else if (byRef) {
            pat = PointerType::get(t, 0);
        }
        else {
            pat = abi->preferred_llvm_type((jl_datatype_t*)tti, false);
            if (pat == NULL)
                pat = t;
        }

        byRefList.push_back(byRef);
        inRegList.push_back(inReg);
        fargt.push_back(t);
        fargt_isboxed.push_back(isboxed);
        if (!current_isVa)
            fargt_sig.push_back(pat);
        else
            fargt_vasig = pat;

        do { // for each arg for which this type applies, add the appropriate LLVM parameter attributes
            if (i < nargs) { // if vararg, the last declared arg type may not have a corresponding arg value
                paramattrs.push_back(AttrBuilder());
                // Note that even though the LLVM argument is called ByVal
                // this really means that the thing we're passing is pointing to
                // the thing we want to pass by value
#ifndef _CPU_AARCH64_
                // the aarch64 backend seems to interpret ByVal as
                // implicitly passed on stack.
                if (byRef)
                    paramattrs[i + sret].addAttribute(Attribute::ByVal);
#endif
                if (inReg)
                    paramattrs[i + sret].addAttribute(Attribute::InReg);
                if (av != Attribute::None)
                    paramattrs[i + sret].addAttribute(av);
            }
            i++;
        } while (current_isVa && i < nargs); // if is this is the vararg, loop to the end
    }

    for (i = 0; i < nargs + sret; ++i) {
        if (paramattrs[i].hasAttributes()) {
            attributes = attributes.addAttributes(jl_LLVMContext, i + 1,
                                                  AttributeSet::get(jl_LLVMContext, i + 1, paramattrs[i]));
        }
    }
    if (rt == jl_bottom_type)
        attributes = attributes.addAttribute(jl_LLVMContext,
                                             AttributeSet::FunctionIndex,
                                             Attribute::NoReturn);
    return "";
}


// ccall(pointer, rettype, (argtypes...), args...)
jl_cgval_t emit_ccall(jl_value_t **args, size_t nargs)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_NARGSV(ccall, 3);
    jl_value_t *rt=NULL, *at=NULL;
    JL_GC_PUSH2(&rt, &at);

    native_sym_arg_t symarg = interpret_symbol_arg(args[1], "ccall");
    Value *jl_ptr=NULL;
    void (*fptr)(void) = NULL;
    const char *f_name = NULL, *f_lib = NULL;
    jl_ptr = symarg.jl_ptr;
    fptr = symarg.fptr;
    f_name = symarg.f_name;
    f_lib = symarg.f_lib;
    bool isVa = false;

    if (f_name == NULL && fptr == NULL && jl_ptr == NULL) {
        emit_error("ccall: null function pointer");
        JL_GC_POP();
        return jl_cgval_t();
    }

    jl_value_t *rtt_ = expr_type(args[2]);
    bool static_rt = true;  // is return type fully statically known?
    if (jl_is_type_type(rtt_) && jl_is_leaf_type(jl_tparam0(rtt_))) {
        rt = jl_tparam0(rtt_);
    }
    else {
        rt = try_eval(args[2], NULL);
        if (rt == NULL) {
            static_rt = false;
            if (jl_is_type_type(rtt_)) {
                if (jl_subtype(jl_tparam0(rtt_), (jl_value_t*)jl_pointer_type, 0)) {
                    // substitute Ptr{Void} for statically-unknown pointer type
                    rt = (jl_value_t*)jl_voidpointer_type;
                }
                else if (jl_subtype(jl_tparam0(rtt_), (jl_value_t*)jl_array_type, 0)) {
                    // `Array` used as return type just returns a julia object reference
                    rt = (jl_value_t*)jl_any_type;
                    static_rt = true;
                }
                else if (jl_is_typevar(jl_tparam0(rtt_)) && jl_is_abstract_ref_type(((jl_tvar_t*)jl_tparam0(rtt_))->ub)) {
                    // `Ref{T}` used as return type just returns T (from a jl_value_t*)
                    rt = (jl_value_t*)jl_any_type;
                    static_rt = true;
                }
            }
            if (rt == NULL) {
                if (jl_is_expr(args[2])) {
                    jl_expr_t *rtexpr = (jl_expr_t*)args[2];
                    if (rtexpr->head == call_sym && jl_expr_nargs(rtexpr) == 4 &&
                        static_eval(jl_exprarg(rtexpr, 0), true, false) == jl_builtin_apply_type &&
                        static_eval(jl_exprarg(rtexpr, 1), true, false) == (jl_value_t*)jl_array_type) {
                        // `Array` used as return type just returns a julia object reference
                        rt = (jl_value_t*)jl_any_type;
                        static_rt = true;
                    }
                    else if (rtexpr->head == call_sym && jl_expr_nargs(rtexpr) == 3 &&
                             static_eval(jl_exprarg(rtexpr, 0), true, false) == jl_builtin_apply_type &&
                             static_eval(jl_exprarg(rtexpr, 1), true, false) == (jl_value_t*)jl_pointer_type) {
                        // substitute Ptr{Void} for statically-unknown pointer type
                        rt = (jl_value_t*)jl_voidpointer_type;
                    }
                    else if (rtexpr->head == call_sym && jl_expr_nargs(rtexpr) == 3 &&
                             static_eval(jl_exprarg(rtexpr, 0), true, false) == jl_builtin_apply_type &&
                             static_eval(jl_exprarg(rtexpr, 1), true, false) == (jl_value_t*)jl_ref_type) {
                        // `Ref{T}` used as return type just returns T (from a jl_value_t*)
                        rt = (jl_value_t*)jl_any_type;
                        static_rt = true;
                    }
                }
            }
            if (rt == NULL) {
                if (ptls->exception_in_transit &&
                    jl_typeis(ptls->exception_in_transit,
                              jl_undefvarerror_type) &&
                    jl_is_symbol(args[2])) {
                    std::string msg = "ccall return type undefined: " +
                                      std::string(jl_symbol_name((jl_sym_t*)args[2]));
                    emit_error(msg.c_str());
                    JL_GC_POP();
                    return jl_cgval_t();
                }
                emit_error("error interpreting ccall return type");
                JL_GC_POP();
                return jl_cgval_t();
            }
        }
    }

    if (jl_is_svec(rt)) {
        std::string msg = "in " + this->funcName +
            ": ccall: missing return type";
        jl_error(msg.c_str());
    }
    if (jl_is_cpointer_type(rt) && jl_is_typevar(jl_tparam0(rt)))
        jl_error("ccall: return type Ptr should have an element type, not Ptr{_<:T}");

    if (jl_is_abstract_ref_type(rt)) {
        if (jl_tparam0(rt) == (jl_value_t*)jl_any_type)
            jl_error("ccall: return type Ref{Any} is invalid. use Ptr{Any} instead.");
        rt = (jl_value_t*)jl_any_type; // convert return type to jl_value_t*
    }

    if (jl_is_array_type(rt)) {
        // `Array` used as return type just returns a julia object reference
        rt = (jl_value_t*)jl_any_type;
    }

    JL_TYPECHK(ccall, type, rt);
    bool retboxed;
    Type *lrt = julia_struct_to_llvm(rt, &retboxed);
    if (lrt == NULL) {
        emit_error("ccall: return type doesn't correspond to a C type");
        JL_GC_POP();
        return jl_cgval_t();
    }

    at = try_eval(args[3], "error interpreting ccall argument tuple");
    if (at == NULL) {
        JL_GC_POP();
        return jl_cgval_t();
    }

    JL_TYPECHK(ccall, simplevector, at);
    //JL_TYPECHK(ccall, type, at);
    jl_svec_t *tt = (jl_svec_t*)at;

    // check for calling convention specifier
    CallingConv::ID cc = CallingConv::C;
    jl_value_t *last = args[nargs];
    if (jl_is_expr(last)) {
        jl_sym_t *lhd = ((jl_expr_t*)last)->head;
        if (lhd == jl_symbol("stdcall")) {
            cc = CallingConv::X86_StdCall;
            nargs--;
        }
        else if (lhd == jl_symbol("cdecl")) {
            cc = CallingConv::C;
            nargs--;
        }
        else if (lhd == jl_symbol("fastcall")) {
            cc = CallingConv::X86_FastCall;
            nargs--;
        }
        else if (lhd == jl_symbol("thiscall")) {
            cc = CallingConv::X86_ThisCall;
            nargs--;
        }
    }

    // some sanity checking and check whether there's a vararg
    size_t i;
    size_t nargt = jl_svec_len(tt);
    for(i=0; i < nargt; i++) {
        jl_value_t *tti = jl_svecref(tt,i);
        if (jl_is_cpointer_type(tti) && jl_is_typevar(jl_tparam0(tti))) {
            JL_GC_POP();
            emit_error("ccall: argument type Ptr should have an element type, Ptr{T}");
            return jl_cgval_t();
        }
        if (jl_is_vararg_type(tti))
            isVa = true;
    }

    if ((!isVa && nargt  != (nargs - 2)/2) ||
        ( isVa && nargt-1 > (nargs - 2)/2))
        jl_error("ccall: wrong number of arguments to C function");

    // some special functions
    if (fptr == (void(*)(void))&jl_array_ptr ||
        ((f_lib==NULL || (intptr_t)f_lib==2)
         && f_name && !strcmp(f_name,"jl_array_ptr"))) {
        assert(lrt->isPointerTy());
        assert(!isVa);
        assert(nargt==1);
        jl_value_t *argi = args[4];
        assert(!(jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym));
        jl_cgval_t ary = emit_expr(argi);
        JL_GC_POP();
        return mark_or_box_ccall_result(emit_bitcast(emit_arrayptr(ary), lrt),
                                        retboxed, args[2], rt, static_rt);
    }
    if (fptr == (void(*)(void))&jl_value_ptr ||
        ((f_lib==NULL || (intptr_t)f_lib==2)
         && f_name && !strcmp(f_name,"jl_value_ptr"))) {
        assert(lrt->isPointerTy());
        assert(!isVa);
        assert(nargt==1);
        jl_value_t *argi = args[4];
        bool addressOf = false;
        jl_value_t *tti = jl_svecref(tt,0);
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }
        else if (jl_is_abstract_ref_type(tti)) {
            tti = (jl_value_t*)jl_voidpointer_type;
        }
        Value *ary;
        Type *largty;
        bool isboxed;
        if (addressOf) {
            largty = T_pjlvalue;
            isboxed = true;
        }
        else {
            largty = julia_struct_to_llvm(tti, &isboxed);
        }
        if (isboxed) {
            ary = boxed(emit_expr(argi));
        }
        else {
            assert(!addressOf);
            ary = emit_unbox(largty, emit_expr(argi), tti);
        }
        JL_GC_POP();
        return mark_or_box_ccall_result(emit_bitcast(ary, lrt),
                                        retboxed, args[2], rt, static_rt);
    }
    if (JL_CPU_WAKE_NOOP &&
        (fptr == &jl_cpu_wake || ((!f_lib || (intptr_t)f_lib == 2) &&
                                  f_name && !strcmp(f_name, "jl_cpu_wake")))) {
        assert(lrt == T_void);
        assert(!isVa);
        assert(nargt == 0);
        JL_GC_POP();
        return ghostValue(jl_void_type);
    }
    if (fptr == &jl_gc_safepoint ||
        ((!f_lib || (intptr_t)f_lib == 2) && f_name &&
         strcmp(f_name, "jl_gc_safepoint") == 0)) {
        assert(lrt == T_void);
        assert(!isVa);
        assert(nargt == 0);
        JL_GC_POP();
        builder.CreateCall(prepare_call(gcroot_flush_func));
        emit_signal_fence();
        builder.CreateLoad(this->signalPage, true);
        emit_signal_fence();
        return ghostValue(jl_void_type);
    }
#ifdef _OS_LINUX_
    // directly access the address of a ifunc can cause linker issue on
    // some configurations (e.g. AArch64 + -Bsymbolic-functions).
    static const auto ptls_getter = jl_dlsym_e(jl_dlopen(nullptr, 0),
                                               "jl_get_ptls_states");
#else
    static const auto ptls_getter = &jl_get_ptls_states;
#endif
    if (fptr == (void(*)(void))(uintptr_t)ptls_getter ||
        ((!f_lib || (intptr_t)f_lib == 2) && f_name &&
         strcmp(f_name, "jl_get_ptls_states") == 0)) {
        assert(lrt == T_pint8);
        assert(!isVa);
        assert(nargt == 0);
        JL_GC_POP();
        return mark_or_box_ccall_result(
            emit_bitcast(this->ptlsStates, lrt),
            retboxed, args[2], rt, static_rt);
    }
    if (fptr == &jl_sigatomic_begin ||
        ((!f_lib || (intptr_t)f_lib == 2) && f_name &&
         strcmp(f_name, "jl_sigatomic_begin") == 0)) {
        assert(lrt == T_void);
        assert(!isVa);
        assert(nargt == 0);
        JL_GC_POP();
        builder.CreateCall(prepare_call(gcroot_flush_func));
        Value *pdefer_sig = emit_defer_signal();
        Value *defer_sig = builder.CreateLoad(pdefer_sig);
        defer_sig = builder.CreateAdd(defer_sig,
                                      ConstantInt::get(T_sigatomic, 1));
        builder.CreateStore(defer_sig, pdefer_sig);
        emit_signal_fence();
        return ghostValue(jl_void_type);
    }
    if (fptr == &jl_sigatomic_end ||
        ((!f_lib || (intptr_t)f_lib == 2) && f_name &&
         strcmp(f_name, "jl_sigatomic_end") == 0)) {
        assert(lrt == T_void);
        assert(!isVa);
        assert(nargt == 0);
        JL_GC_POP();
        builder.CreateCall(prepare_call(gcroot_flush_func));
        Value *pdefer_sig = emit_defer_signal();
        Value *defer_sig = builder.CreateLoad(pdefer_sig);
        emit_signal_fence();
        error_unless(builder.CreateICmpNE(defer_sig,
                                          ConstantInt::get(T_sigatomic, 0)),
                     "sigatomic_end called in non-sigatomic region");
        defer_sig = builder.CreateSub(defer_sig,
                                      ConstantInt::get(T_sigatomic, 1));
        builder.CreateStore(defer_sig, pdefer_sig);
        BasicBlock *checkBB = BasicBlock::Create(jl_LLVMContext, "check",
                                                 this->f);
        BasicBlock *contBB = BasicBlock::Create(jl_LLVMContext, "cont");
        builder.CreateCondBr(
            builder.CreateICmpEQ(defer_sig, ConstantInt::get(T_sigatomic, 0)),
            checkBB, contBB);
        builder.SetInsertPoint(checkBB);
        builder.CreateLoad(builder.CreateConstGEP1_32(this->signalPage, -1),
                           true);
        builder.CreateBr(contBB);
        this->f->getBasicBlockList().push_back(contBB);
        builder.SetInsertPoint(contBB);
        return ghostValue(jl_void_type);
    }
    if (fptr == (void(*)(void))&jl_is_leaf_type ||
        ((f_lib==NULL || (intptr_t)f_lib==2)
         && f_name && !strcmp(f_name, "jl_is_leaf_type"))) {
        assert(nargt == 1);
        jl_value_t *arg = args[4];
        jl_value_t *ty = expr_type(arg);
        if (jl_is_type_type(ty) && !jl_is_typevar(jl_tparam0(ty))) {
            int isleaf = jl_is_leaf_type(jl_tparam0(ty));
            JL_GC_POP();
            return mark_or_box_ccall_result(ConstantInt::get(T_int32, isleaf),
                    false, args[2], rt, static_rt);
        }
    }
    if (fptr == (void(*)(void))&jl_function_ptr ||
        ((f_lib==NULL || (intptr_t)f_lib==2)
         && f_name && !strcmp(f_name, "jl_function_ptr"))) {
        assert(nargt == 3);
        jl_value_t *f = static_eval(args[4], false, false);
        jl_value_t *frt = expr_type(args[6]);
        if (f && (jl_is_type_type((jl_value_t*)frt) && !jl_has_typevars(jl_tparam0(frt)))) {
            jl_value_t *fargt = static_eval(args[8], true, true);
            if (fargt) {
                if (jl_is_tuple(fargt)) {
                    // TODO: maybe deprecation warning, better checking
                    fargt = (jl_value_t*)jl_apply_tuple_type_v((jl_value_t**)jl_data_ptr(fargt), jl_nfields(fargt));
                }
            }
            else {
                fargt = expr_type(args[8]);
                if (jl_is_type_type((jl_value_t*)fargt))
                    fargt = jl_tparam0(fargt);
            }
            if (jl_is_tuple_type(fargt) && jl_is_leaf_type(fargt)) {
                frt = jl_tparam0(frt);
                JL_TRY {
                    Value *llvmf = prepare_call(jl_cfunction_object((jl_function_t*)f, frt, (jl_tupletype_t*)fargt));
                    // make sure to emit any side-effects that may have been part of the original expression
                    emit_expr(args[4]);
                    emit_expr(args[6]);
                    emit_expr(args[8]);
                    JL_GC_POP();
                    return mark_or_box_ccall_result(emit_bitcast(llvmf, lrt),
                                                    retboxed, args[2], rt, static_rt);
                }
                JL_CATCH {
                }
            }
        }
    }

    // save place before arguments, for possible insertion of temp arg
    // area saving code.
    Value *stacksave=NULL;
    BasicBlock::InstListType &instList = builder.GetInsertBlock()->getInstList();
    Instruction *savespot;
    if (instList.empty()) {
        savespot = NULL;
    }
    else {
        // hey C++, there's this thing called pointers...
        Instruction &_savespot = builder.GetInsertBlock()->back();
        savespot = &_savespot;
    }

    std::vector<Type*> fargt(0);
    std::vector<Type*> fargt_sig(0);
    std::vector<bool> fargt_isboxed(0);
    Type *fargt_vasig = NULL;
    std::vector<bool> inRegList(0);
    std::vector<bool> byRefList(0);
    AttributeSet attrs;
    Type *prt = NULL;
    int sret = 0;
    std::string err_msg = generate_func_sig(&lrt, &prt, sret, fargt, fargt_isboxed, fargt_sig, fargt_vasig,
                                            inRegList, byRefList, attrs, rt, tt, (nargs - 3)/2);
    if (!err_msg.empty()) {
        JL_GC_POP();
        emit_error(err_msg);
        return jl_cgval_t();
    }

    // emit arguments
    Value **argvals = (Value**) alloca(((nargs - 3) / 2 + sret) * sizeof(Value*));
    Value *result = NULL;
    bool needStackRestore = false;

    // First, if the ABI requires us to provide the space for the return
    // argument, allocate the box and store that as the first argument type
    bool sretboxed = false;
    if (sret) {
        jl_cgval_t sret_val = emit_new_struct(rt, 1, NULL); // TODO: is it valid to be creating an incomplete type this way?
        assert(sret_val.typ != NULL && "Type was not concrete");
        if (!sret_val.ispointer()) {
            Value *mem = emit_static_alloca(lrt);
            builder.CreateStore(sret_val.V, mem);
            result = mem;
        }
        else {
            // XXX: result needs a GC root here if result->getType() == T_pjlvalue
            result = sret_val.V;
        }
        argvals[0] = emit_bitcast(result, fargt_sig.at(0));
        sretboxed = sret_val.isboxed;
    }

    // number of parameters to the c function
    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * (nargs - 3)/2);
    for(i = 4; i < nargs + 1; i += 2) {
        // Current C function parameter
        size_t ai = (i - 4) / 2;

        // Julia (expression) value of current parameter
        jl_value_t *argi = args[i];

        // pass the address of the argument rather than the argument itself
        bool addressOf = false;
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }

        Type *largty; // LLVM type of the current parameter
        bool toboxed;
        jl_value_t *jargty; // Julia type of the current parameter
        bool byRef, inReg; // Argument attributes
        if (isVa && ai >= nargt - 1) {
            largty = fargt.at(nargt - 1);
            toboxed = fargt_isboxed.at(nargt - 1);
            jargty = jl_tparam0(jl_svecref(tt, nargt - 1));
            byRef = byRefList.at(nargt - 1);
            inReg = inRegList.at(nargt - 1);
        }
        else {
            largty = fargt.at(ai);
            toboxed = fargt_isboxed.at(ai);
            jargty = jl_svecref(tt, ai);
            byRef = byRefList.at(ai);
            inReg = inRegList.at(ai);
        }

        jl_cgval_t &arg = argv[ai];
        arg = emit_expr((jl_value_t*)argi);
        if (jl_is_abstract_ref_type(jargty)) {
            if (addressOf) {
                JL_GC_POP();
                emit_error("ccall: & on a Ref{T} argument is invalid");
                return jl_cgval_t();
            }
            if (!jl_is_cpointer_type(arg.typ)) {
                emit_cpointercheck(arg, "ccall: argument to Ref{T} is not a pointer");
                arg.typ = (jl_value_t*)jl_voidpointer_type;
                arg.isboxed = false;
            }
            jargty = (jl_value_t*)jl_voidpointer_type;
        }

        Value *v = julia_to_native(largty, toboxed, jargty, arg, addressOf, byRef, inReg,
                                   false, ai + 1, &needStackRestore);
        bool issigned = jl_signed_type && jl_subtype(jargty, (jl_value_t*)jl_signed_type, 0);
        argvals[ai + sret] = llvm_type_rewrite(v, largty,
                ai + sret < fargt_sig.size() ? fargt_sig.at(ai + sret) : fargt_vasig,
                false, byRef, issigned);
    }


    // make LLVM function object for the target
    // keep this close to the function call, so that the compiler can
    // optimize the global pointer load in the common case
    Value *llvmf;
    FunctionType *functype = FunctionType::get(sret ? T_void : prt, fargt_sig, isVa);

    if (jl_ptr != NULL) {
        null_pointer_check(jl_ptr);
        Type *funcptype = PointerType::get(functype,0);
        llvmf = builder.CreateIntToPtr(jl_ptr, funcptype);
    }
    else if (fptr != NULL) {
        Type *funcptype = PointerType::get(functype,0);
        llvmf = literal_static_pointer_val((void*)(uintptr_t)fptr, funcptype);
        if (imaging_mode)
            jl_printf(JL_STDERR,"WARNING: literal address used in ccall for %s; code cannot be statically compiled\n", f_name);
    }
    else {
        assert(f_name != NULL);

        PointerType *funcptype = PointerType::get(functype,0);
        if (imaging_mode) {
            // vararg requires musttail,
            // but musttail is incompatible with noreturn.
            if (functype->isVarArg())
                llvmf = runtime_sym_lookup(funcptype, f_lib, f_name, this->f);
            else
                llvmf = emit_plt(functype, attrs, cc, f_lib, f_name);
        }
        else {
            void *symaddr = jl_dlsym_e(jl_get_library(f_lib), f_name);
            if (symaddr == NULL) {
                JL_GC_POP();
                std::stringstream msg;
                msg << "ccall: could not find function ";
                msg << f_name;
                if (f_lib != NULL) {
#ifdef _OS_WINDOWS_
                    assert((intptr_t)f_lib != 1 && (intptr_t)f_lib != 2);
#endif
                    msg << " in library ";
                    msg << f_lib;
                }
                emit_error(msg.str());
                return jl_cgval_t();
            }
            // since we aren't saving this code, there's no sense in
            // putting anything complicated here: just JIT the function address
            llvmf = literal_static_pointer_val(symaddr, funcptype);
        }
    }

    if (needStackRestore) {
        stacksave = CallInst::Create(Intrinsic::getDeclaration(jl_Module,
                                                               Intrinsic::stacksave));
        if (savespot) {
#ifdef LLVM38
                instList.insertAfter(savespot->getIterator(), (Instruction*)stacksave);
#else
                instList.insertAfter((Instruction*)savespot, (Instruction*)stacksave);
#endif
        }
        else
            instList.push_front((Instruction*)stacksave);
    }

    //llvmf->dump();
    //for (int i = 0; i < (nargs - 3) / 2 + sret; ++i)
    //    argvals[i]->dump();

    // Mark GC use before **and** after the ccall to make sure the arguments
    // are alive during the ccall even if the function called is `noreturn`.
    SmallVector<Value*, 16> gc_uses;
    for(i = 4; i < nargs + 1; i += 2) {
        // Current C function parameter
        size_t ai = (i - 4) / 2;
        push_gc_use(gc_uses, argv[ai]);

        // Julia (expression) value of current parameter gcroot
        jl_value_t *argi = args[i + 1];
        if (jl_is_long(argi)) continue;
        jl_cgval_t arg = emit_expr(argi);
        push_gc_use(gc_uses, arg);
    }
    mark_gc_uses(gc_uses);
    // the actual call
    Value *ret = builder.CreateCall(prepare_call(llvmf),
                                    ArrayRef<Value*>(&argvals[0], (nargs - 3) / 2 + sret));
    ((CallInst*)ret)->setAttributes(attrs);

    if (cc != CallingConv::C)
        ((CallInst*)ret)->setCallingConv(cc);
    if (!sret)
        result = ret;
    if (needStackRestore) {
        assert(stacksave != NULL);
        builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::stackrestore), stacksave);
    }
    if (0) { // Enable this to turn on SSPREQ (-fstack-protector) on the function containing this ccall
        this->f->addFnAttr(Attribute::StackProtectReq);
    }

    mark_gc_uses(gc_uses);
    JL_GC_POP();
    if (rt == jl_bottom_type) {
        // Do this after we marked all the GC uses.
        CreateTrap(builder);
    }
    // Finally we need to box the result into julia type
    // However, if we have already created a box for the return
    // type because the ABI required us to pass a pointer (sret),
    // then we do not need to do this.
    if (!sret) {
        Type *jlrt = julia_type_to_llvm(rt, &retboxed); // compute the real "julian" return type and update retboxed
        if (type_is_ghost(jlrt)) {
            return ghostValue(rt);
        }
        else if (lrt->isStructTy() && retboxed) {
            assert(jl_is_structtype(rt));
            jl_cgval_t newst = emit_new_struct(rt, 1, NULL); // emit a new, empty struct
            assert(newst.typ != NULL && "Type was not concrete");
            assert(newst.isboxed);
            // copy the data from the return value to the new struct
            tbaa_decorate(newst.tbaa, builder.CreateAlignedStore(result, emit_bitcast(newst.V, prt->getPointerTo()), 16)); // julia gc is aligned 16
            return newst;
        }
        else if (jlrt != prt) {
            assert(lrt == jlrt); // jl_struct_to_llvm and julia_type_to_llvm should only differ for concrete types, per the case above
            result = llvm_type_rewrite(result, prt, jlrt, true, false, false);
        }
    }
    else {
        retboxed = sretboxed;
        if (!retboxed)
            result = builder.CreateLoad(result); // something alloca'd above
    }

    return mark_or_box_ccall_result(result, retboxed, args[2], rt, static_rt);
}
