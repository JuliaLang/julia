// This file is a part of Julia. License is MIT: https://julialang.org/license

// utility procedures used in code generation

static Instruction *tbaa_decorate(MDNode *md, Instruction *load_or_store)
{
    load_or_store->setMetadata(llvm::LLVMContext::MD_tbaa, md);
    if (isa<LoadInst>(load_or_store) && md == tbaa_const)
        load_or_store->setMetadata(LLVMContext::MD_invariant_load, MDNode::get(md->getContext(), None));
    return load_or_store;
}

static Function *function_proto(Function *F, Module *M = nullptr)
{
    // Copy the declaration characteristics of the Function (not the body)
    Function *NewF = Function::Create(F->getFunctionType(),
                                      Function::ExternalLinkage,
                                      F->getName(),
                                      M);

    // Declarations are not allowed to have personality routines, but
    // copyAttributesFrom sets them anyway. Temporarily unset the personality
    // routine from `F`, since copying it and then resetting is more expensive
    // as well as introducing an extra use from this unowned function, which
    // can cause crashes in the LLVMContext's global destructor.
    llvm::Constant *OldPersonalityFn = nullptr;
    if (F->hasPersonalityFn()) {
        OldPersonalityFn = F->getPersonalityFn();
        F->setPersonalityFn(nullptr);
    }

    // FunctionType does not include any attributes. Copy them over manually
    // as codegen may make decisions based on the presence of certain attributes
    NewF->copyAttributesFrom(F);

    if (OldPersonalityFn)
        F->setPersonalityFn(OldPersonalityFn);

    // DLLImport only needs to be set for the shadow module
    // it just gets annoying in the JIT
    NewF->setDLLStorageClass(GlobalValue::DefaultStorageClass);

    return NewF;
}

#define prepare_call(Callee) prepare_call_in(jl_Module, (Callee))
static Value *prepare_call_in(Module *M, Value *Callee)
{
    if (Function *F = dyn_cast<Function>(Callee)) {
        GlobalValue *local = M->getNamedValue(Callee->getName());
        if (!local) {
            local = function_proto(F, M);
        }
        return local;
    }
    return Callee;
}

// Take an arbitrary untracked value and make it gc-tracked
static Value *maybe_decay_untracked(IRBuilder<> &irbuilder, Value *V)
{
    if (V->getType() == T_pjlvalue)
        return irbuilder.CreateAddrSpaceCast(V, T_prjlvalue);
    else if (V->getType() == T_ppjlvalue)
        return irbuilder.CreateBitCast(V, T_pprjlvalue);
    return V;
}

// Take an untracked value and make it tracked (specialized on Constant)
static Constant *maybe_decay_untracked(IRBuilder<> &irbuilder, Constant *C)
{
    if (C->getType() == T_pjlvalue)
        return ConstantExpr::getAddrSpaceCast(C, T_prjlvalue);
    else if (C->getType() == T_ppjlvalue)
        return ConstantExpr::getBitCast(C, T_pprjlvalue);
    return C;
}

// Take any value and mark that it may be derived from a rooted value
static Value *decay_derived(IRBuilder<> &irbuilder, Value *V)
{
    Type *T = V->getType();
    if (cast<PointerType>(T)->getAddressSpace() == AddressSpace::Derived)
        return V;
    // Once llvm deletes pointer element types, we won't need it here any more either.
    Type *NewT = PointerType::get(cast<PointerType>(T)->getElementType(), AddressSpace::Derived);
    return irbuilder.CreateAddrSpaceCast(V, NewT);
}

// Take any value and make it safe to pass to GEP
static Value *maybe_decay_tracked(IRBuilder<> &irbuilder, Value *V)
{
    Type *T = V->getType();
    if (cast<PointerType>(T)->getAddressSpace() != AddressSpace::Tracked)
        return V;
    Type *NewT = PointerType::get(cast<PointerType>(T)->getElementType(), AddressSpace::Derived);
    return irbuilder.CreateAddrSpaceCast(V, NewT);
}

static Value *mark_callee_rooted(IRBuilder<> &irbuilder, Value *V)
{
    assert(V->getType() == T_pjlvalue || V->getType() == T_prjlvalue);
    return irbuilder.CreateAddrSpaceCast(V,
        PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
}

#define maybe_decay_untracked(V)  maybe_decay_untracked(ctx.builder, (V))
#define maybe_decay_untracked(V)  maybe_decay_untracked(ctx.builder, (V))
#define decay_derived(V)          decay_derived(ctx.builder, (V))
#define maybe_decay_tracked(V)    maybe_decay_tracked(ctx.builder, (V))
#define mark_callee_rooted(V)     mark_callee_rooted(ctx.builder, (V))


// --- language feature checks ---

#define JL_FEAT_TEST(ctx, feature) ((ctx).params->feature)


// --- hook checks ---

#define JL_HOOK_TEST(params,hook) ((params)->hook != jl_nothing)

#define JL_HOOK_CALL(params,hook,argc,...) \
    _hook_call<argc>((params)->hook, {{__VA_ARGS__}});
template<int N>
static inline void _hook_call(jl_value_t *hook, std::array<jl_value_t*,N> args) {
    jl_value_t **argv;
    JL_GC_PUSHARGS(argv, N+1);
    argv[0] = hook;
    for (int i = 0; i < N; i++)
        argv[i+1] = args[i];
    jl_apply(argv, N+1);
    JL_GC_POP();
}


// --- string constants ---
static StringMap<GlobalVariable*> stringConstants;
static Value *stringConstPtr(IRBuilder<> &irbuilder, const std::string &txt)
{
    StringRef ctxt(txt.c_str(), strlen(txt.c_str()) + 1);
    StringMap<GlobalVariable*>::iterator pooledval =
        stringConstants.insert(std::pair<StringRef, GlobalVariable*>(ctxt, NULL)).first;
    StringRef pooledtxt = pooledval->getKey();
    if (imaging_mode) {
        if (pooledval->second == NULL) {
            static int strno = 0;
            std::stringstream ssno;
            ssno << "_j_str" << strno++;
            GlobalVariable *gv = get_pointer_to_constant(
                                    ConstantDataArray::get(jl_LLVMContext,
                                                           ArrayRef<unsigned char>(
                                                           (const unsigned char*)pooledtxt.data(),
                                                           pooledtxt.size())),
                                    ssno.str(),
                                    *shadow_output);
            pooledval->second = gv;
            jl_ExecutionEngine->addGlobalMapping(gv, (void*)(uintptr_t)pooledtxt.data());
        }

        GlobalVariable *v = prepare_global_in(jl_builderModule(irbuilder), pooledval->second);
        Value *zero = ConstantInt::get(Type::getInt32Ty(jl_LLVMContext), 0);
        Value *Args[] = { zero, zero };
        return irbuilder.CreateInBoundsGEP(v->getValueType(), v, Args);
    }
    else {
        Value *v = ConstantExpr::getIntToPtr(
                ConstantInt::get(T_size, (uintptr_t)pooledtxt.data()),
                T_pint8);
        return v;
    }
}

// --- MDNode ---
Metadata *to_md_tree(jl_value_t *val) {
    if (val == jl_nothing)
        return nullptr;
    Metadata *MD = nullptr;
    if (jl_is_symbol(val)) {
        MD = MDString::get(jl_LLVMContext, jl_symbol_name((jl_sym_t*)val));
    } else if (jl_is_bool(val)) {
        MD = ConstantAsMetadata::get(ConstantInt::get(T_int1, jl_unbox_bool(val)));
    } else if (jl_is_long(val)) {
        MD = ConstantAsMetadata::get(ConstantInt::get(T_int64, jl_unbox_long(val)));
    } else if (jl_is_tuple(val)) {
        SmallVector<Metadata *, 8> MDs;
        for (int f = 0, nf = jl_nfields(val); f < nf; ++f) {
            MD = to_md_tree(jl_fieldref(val, f));
            if (MD)
                MDs.push_back(MD);
        }
        MD = MDNode::get(jl_LLVMContext, MDs);
    } else {
        jl_error("LLVM metadata needs to Symbol/Bool/Int or Tuple thereof");
    }
    return MD;
}

// --- Debug info ---

static DIType *julia_type_to_di(jl_value_t *jt, DIBuilder *dbuilder, bool isboxed = false)
{
    if (isboxed || !jl_is_datatype(jt))
        return jl_pvalue_dillvmt;
    jl_datatype_t *jdt = (jl_datatype_t*)jt;
    // always return the boxed representation for types with hidden content
    if (jdt->ditype != NULL)
        return (DIType*)jdt->ditype;
    if (jl_is_primitivetype(jt)) {
        uint64_t SizeInBits = jl_datatype_nbits(jdt);
        llvm::DIType *t = dbuilder->createBasicType(
                jl_symbol_name(jdt->name->name),
                SizeInBits,
                llvm::dwarf::DW_ATE_unsigned);
        jdt->ditype = t;
        return t;
    }
    if (jl_is_structtype(jt) && jdt->uid && jdt->layout && !jl_is_layout_opaque(jdt->layout)) {
        size_t ntypes = jl_datatype_nfields(jdt);
        const char *tname = jl_symbol_name(jdt->name->name);
        std::stringstream unique_name;
        unique_name << jdt->uid;
        llvm::DICompositeType *ct = dbuilder->createStructType(
                NULL,                       // Scope
                tname,                      // Name
                NULL,                       // File
                0,                          // LineNumber
                jl_datatype_nbits(jdt),     // SizeInBits
                8 * jl_datatype_align(jdt), // AlignInBits
                DIFlagZero,                 // Flags
                NULL,                       // DerivedFrom
                DINodeArray(),              // Elements
                dwarf::DW_LANG_Julia,       // RuntimeLanguage
                nullptr,                    // VTableHolder
                unique_name.str()           // UniqueIdentifier
                );
        jdt->ditype = ct;
        std::vector<llvm::Metadata*> Elements;
        for (unsigned i = 0; i < ntypes; i++) {
            jl_value_t *el = jl_svecref(jdt->types, i);
            DIType *di;
            if (jl_field_isptr(jdt, i))
                di = jl_pvalue_dillvmt;
            else
                di = julia_type_to_di(el, dbuilder, false);
            Elements.push_back(di);
        }
        dbuilder->replaceArrays(ct, dbuilder->getOrCreateArray(ArrayRef<Metadata*>(Elements)));
        return ct;
    }
    jdt->ditype = dbuilder->createTypedef(jl_pvalue_dillvmt,
            jl_symbol_name(jdt->name->name), NULL, 0, NULL);
    return (llvm::DIType*)jdt->ditype;
}

static Value *emit_pointer_from_objref(jl_codectx_t &ctx, Value *V)
{
    unsigned AS = cast<PointerType>(V->getType())->getAddressSpace();
    if (AS != AddressSpace::Tracked && AS != AddressSpace::Derived)
        return V;
    V = decay_derived(V);
    Type *T = PointerType::get(T_jlvalue, AddressSpace::Derived);
    if (V->getType() != T)
        V = ctx.builder.CreateBitCast(V, T);
    CallInst *Call = ctx.builder.CreateCall(prepare_call(pointer_from_objref_func), V);
    Call->setAttributes(pointer_from_objref_func->getAttributes());
    return Call;
}

static Value *get_gc_root_for(const jl_cgval_t &x)
{
    if (x.Vboxed)
        return x.Vboxed;
    if (x.ispointer() && !x.constant) {
        assert(x.V);
        if (PointerType *T = dyn_cast<PointerType>(x.V->getType())) {
            if (T->getAddressSpace() == AddressSpace::Tracked ||
                T->getAddressSpace() == AddressSpace::Derived) {
                return x.V;
            }
        }
    }
    return nullptr;
}

// --- emitting pointers directly into code ---

static Constant *literal_static_pointer_val(jl_codectx_t &ctx, const void *p, Type *T = T_pjlvalue)
{
    // this function will emit a static pointer into the generated code
    // the generated code will only be valid during the current session,
    // and thus, this should typically be avoided in new API's
#if defined(_P64)
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int64, (uint64_t)p), T);
#else
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int32, (uint32_t)p), T);
#endif
}


static Value *julia_pgv(jl_codectx_t &ctx, const char *cname, void *addr)
{
    // emit a GlobalVariable for a jl_value_t named "cname"
    return jl_get_global_for(cname, addr, jl_Module);
}

static Value *julia_pgv(jl_codectx_t &ctx, const char *prefix, jl_sym_t *name, jl_module_t *mod, void *addr)
{
    // emit a GlobalVariable for a jl_value_t, using the prefix, name, and module to
    // to create a readable name of the form prefixModA.ModB.name
    size_t len = strlen(jl_symbol_name(name))+strlen(prefix)+1;
    jl_module_t *parent = mod, *prev = NULL;
    while (parent != NULL && parent != prev) {
        len += strlen(jl_symbol_name(parent->name))+1;
        prev = parent;
        parent = parent->parent;
    }
    char *fullname = (char*)alloca(len);
    strcpy(fullname, prefix);
    int skipfirst = jl_symbol_name(name)[0] == '@';
    len -= strlen(jl_symbol_name(name)) + 1 - skipfirst;
    strcpy(fullname + len, jl_symbol_name(name) + skipfirst);
    parent = mod;
    prev = NULL;
    while (parent != NULL && parent != prev) {
        size_t part = strlen(jl_symbol_name(parent->name))+1-skipfirst;
        strcpy(fullname+len-part,jl_symbol_name(parent->name)+skipfirst);
        fullname[len-1] = '.';
        len -= part;
        prev = parent;
        parent = parent->parent;
    }
    return julia_pgv(ctx, fullname, addr);
}

static GlobalVariable *julia_const_gv(jl_value_t *val);
static Value *literal_pointer_val_slot(jl_codectx_t &ctx, jl_value_t *p)
{
    // emit a pointer to a jl_value_t* which will allow it to be valid across reloading code
    // also, try to give it a nice name for gdb, for easy identification
    if (!imaging_mode) {
        Module *M = jl_Module;
        GlobalVariable *gv = new GlobalVariable(
                *M, T_pjlvalue, true, GlobalVariable::PrivateLinkage,
                literal_static_pointer_val(ctx, p));
        gv->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
        return gv;
    }
    if (GlobalVariable *gv = julia_const_gv(p)) {
        // if this is a known object, use the existing GlobalValue
        return prepare_global(gv);
    }
    if (jl_is_datatype(p)) {
        jl_datatype_t *addr = (jl_datatype_t*)p;
        // DataTypes are prefixed with a +
        return julia_pgv(ctx, "+", addr->name->name, addr->name->module, p);
    }
    if (jl_is_method(p)) {
        jl_method_t *m = (jl_method_t*)p;
        // functions are prefixed with a -
        return julia_pgv(ctx, "-", m->name, m->module, p);
    }
    if (jl_is_method_instance(p)) {
        jl_method_instance_t *linfo = (jl_method_instance_t*)p;
        // Type-inferred functions are also prefixed with a -
        if (jl_is_method(linfo->def.method))
            return julia_pgv(ctx, "-", linfo->def.method->name, linfo->def.method->module, p);
    }
    if (jl_is_symbol(p)) {
        jl_sym_t *addr = (jl_sym_t*)p;
        // Symbols are prefixed with jl_sym#
        return julia_pgv(ctx, "jl_sym#", addr, NULL, p);
    }
    // something else gets just a generic name
    return julia_pgv(ctx, "jl_global#", p);
}

static size_t dereferenceable_size(jl_value_t *jt)
{
    if (jl_is_array_type(jt)) {
        // Array has at least this much data
        return sizeof(jl_array_t);
    }
    else if (jl_is_datatype(jt) && ((jl_datatype_t*)jt)->layout) {
        return jl_datatype_size(jt);
    }
    return 0;
}

// Return the min required / expected alignment of jltype (on the stack or heap)
static unsigned julia_alignment(jl_value_t *jt)
{
    if (jl_is_array_type(jt)) {
        // Array always has this alignment
        return JL_SMALL_BYTE_ALIGNMENT;
    }
    assert(jl_is_datatype(jt) && ((jl_datatype_t*)jt)->layout);
    unsigned alignment = jl_datatype_align(jt);
    if (alignment > JL_HEAP_ALIGNMENT)
        return JL_HEAP_ALIGNMENT;
    return alignment;
}

static inline void maybe_mark_argument_dereferenceable(Argument *A, jl_value_t *jt)
{
    AttrBuilder B;
    B.addAttribute(Attribute::NonNull);
    // The `dereferencable` below does not imply `nonnull` for non addrspace(0) pointers.
    size_t size = dereferenceable_size(jt);
    if (size) {
        B.addDereferenceableAttr(size);
        if (!A->getType()->getPointerElementType()->isSized()) // mimic LLVM Loads.cpp isAligned
            B.addAlignmentAttr(julia_alignment(jt));
    }
    A->addAttrs(B);
}

static inline Instruction *maybe_mark_load_dereferenceable(Instruction *LI, bool can_be_null,
                                                           size_t size, size_t align)
{
    if (isa<PointerType>(LI->getType())) {
        if (!can_be_null)
            // The `dereferencable` below does not imply `nonnull` for non addrspace(0) pointers.
            LI->setMetadata(LLVMContext::MD_nonnull, MDNode::get(jl_LLVMContext, None));
        if (size) {
            Metadata *OP = ConstantAsMetadata::get(ConstantInt::get(T_int64, size));
            LI->setMetadata(can_be_null ? LLVMContext::MD_dereferenceable_or_null : LLVMContext::MD_dereferenceable,
                            MDNode::get(jl_LLVMContext, { OP }));
            if (align > 1 && !LI->getType()->getPointerElementType()->isSized()) { // mimic LLVM Loads.cpp isAligned
                Metadata *OP = ConstantAsMetadata::get(ConstantInt::get(T_int64, align));
                LI->setMetadata(LLVMContext::MD_align, MDNode::get(jl_LLVMContext, { OP }));
            }
        }
    }
    return LI;
}

static inline Instruction *maybe_mark_load_dereferenceable(Instruction *LI, bool can_be_null, jl_value_t *jt)
{
    size_t size = dereferenceable_size(jt);
    unsigned alignment = 1;
    if (size > 0)
        alignment = julia_alignment(jt);
    return maybe_mark_load_dereferenceable(LI, can_be_null, size, alignment);
}

static Value *literal_pointer_val(jl_codectx_t &ctx, jl_value_t *p)
{
    if (p == NULL)
        return V_null;
    if (!imaging_mode)
        return literal_static_pointer_val(ctx, p);
    Value *pgv = literal_pointer_val_slot(ctx, p);
    return tbaa_decorate(tbaa_const, maybe_mark_load_dereferenceable(
            ctx.builder.CreateLoad(T_pjlvalue, pgv), false, jl_typeof(p)));
}

static Value *literal_pointer_val(jl_codectx_t &ctx, jl_binding_t *p)
{
    // emit a pointer to any jl_value_t which will be valid across reloading code
    if (p == NULL)
        return V_null;
    if (!imaging_mode)
        return literal_static_pointer_val(ctx, p);
    // bindings are prefixed with jl_bnd#
    Value *pgv = julia_pgv(ctx, "jl_bnd#", p->name, p->owner, p);
    return tbaa_decorate(tbaa_const, maybe_mark_load_dereferenceable(
            ctx.builder.CreateLoad(T_pjlvalue, pgv), false,
            sizeof(jl_binding_t), alignof(jl_binding_t)));
}

// bitcast a value, but preserve its address space when dealing with pointer types
static Value *emit_bitcast(jl_codectx_t &ctx, Value *v, Type *jl_value)
{
    if (isa<PointerType>(jl_value) &&
        v->getType()->getPointerAddressSpace() != jl_value->getPointerAddressSpace()) {
        // Cast to the proper address space
        Type *jl_value_addr =
                PointerType::get(cast<PointerType>(jl_value)->getElementType(),
                                 v->getType()->getPointerAddressSpace());
        return ctx.builder.CreateBitCast(v, jl_value_addr);
    }
    else {
        return ctx.builder.CreateBitCast(v, jl_value);
    }
}

static Value *maybe_bitcast(jl_codectx_t &ctx, Value *V, Type *to) {
    if (to != V->getType())
        return emit_bitcast(ctx, V, to);
    return V;
}

static Value *julia_binding_gv(jl_codectx_t &ctx, Value *bv)
{
    Value *offset = ConstantInt::get(T_size, offsetof(jl_binding_t, value) / sizeof(size_t));
    return ctx.builder.CreateInBoundsGEP(bv, offset);
}

static Value *julia_binding_gv(jl_codectx_t &ctx, jl_binding_t *b)
{
    // emit a literal_pointer_val to the value field of a jl_binding_t
    // binding->value are prefixed with *
    Value *bv;
    if (imaging_mode)
        bv = emit_bitcast(ctx,
                tbaa_decorate(tbaa_const,
                              ctx.builder.CreateLoad(T_pjlvalue, julia_pgv(ctx, "*", b->name, b->owner, b))),
                T_pprjlvalue);
    else
        bv = ConstantExpr::getBitCast(literal_static_pointer_val(ctx, b), T_pprjlvalue);
    return julia_binding_gv(ctx, bv);
}

// --- mapping between julia and llvm types ---

static Type *julia_struct_to_llvm(jl_value_t *jt, jl_unionall_t *ua_env, bool *isboxed, bool llvmcall=false);

static unsigned convert_struct_offset(Type *lty, unsigned byte_offset)
{
    const DataLayout &DL = jl_data_layout;
    const StructLayout *SL = DL.getStructLayout(cast<StructType>(lty));
    unsigned idx = SL->getElementContainingOffset(byte_offset);
    assert(SL->getElementOffset(idx) == byte_offset);
    return idx;
}

static unsigned convert_struct_offset(jl_codectx_t &ctx, Type *lty, unsigned byte_offset)
{
    return convert_struct_offset(lty, byte_offset);
}

static Value *emit_struct_gep(jl_codectx_t &ctx, Type *lty, Value *base, unsigned byte_offset)
{
    unsigned idx = convert_struct_offset(ctx, lty, byte_offset);
    return ctx.builder.CreateConstInBoundsGEP2_32(lty, base, 0, idx);
}

extern "C" {
JL_DLLEXPORT Type *julia_type_to_llvm(jl_value_t *jt, bool *isboxed)
{
    // this function converts a Julia Type into the equivalent LLVM type
    if (isboxed) *isboxed = false;
    if (jt == (jl_value_t*)jl_bottom_type)
        return T_void;
    if (jl_is_concrete_immutable(jt)) {
        if (jl_datatype_nbits(jt) == 0)
            return T_void;
        Type *t = julia_struct_to_llvm(jt, NULL, isboxed);
        assert(t != NULL);
        return t;
    }
    if (isboxed) *isboxed = true;
    return T_prjlvalue;
}
}

// converts a julia bitstype into the equivalent LLVM bitstype
static Type *bitstype_to_llvm(jl_value_t *bt, bool llvmcall = false)
{
    assert(jl_is_primitivetype(bt));
    if (bt == (jl_value_t*)jl_bool_type)
        return T_int8;
    if (bt == (jl_value_t*)jl_int32_type)
        return T_int32;
    if (bt == (jl_value_t*)jl_int64_type)
        return T_int64;
    if (llvmcall && (bt == (jl_value_t*)jl_float16_type))
        return T_float16;
    if (bt == (jl_value_t*)jl_float32_type)
        return T_float32;
    if (bt == (jl_value_t*)jl_float64_type)
        return T_float64;
    int nb = jl_datatype_size(bt);
    return Type::getIntNTy(jl_LLVMContext, nb * 8);
}

static bool jl_type_hasptr(jl_value_t* typ)
{ // assumes that jl_stored_inline(typ) is true
    return jl_is_datatype(typ) && ((jl_datatype_t*)typ)->layout->npointers > 0;
}

// compute whether all concrete subtypes of this type have the same layout
// (which is conservatively approximated here by asking whether the types of any of the
// fields depend on any of the parameters of the containing type)
static bool julia_struct_has_layout(jl_datatype_t *dt, jl_unionall_t *ua)
{
    if (dt->layout || dt->struct_decl)
        return true;
    if (ua) {
        jl_svec_t *types = jl_get_fieldtypes(dt);
        size_t i, ntypes = jl_svec_len(types);
        for (i = 0; i < ntypes; i++) {
            jl_value_t *ty = jl_svecref(types, i);
            if (jl_has_typevar_from_unionall(ty, ua))
                return false;
        }
    }
    return true;
}

static unsigned jl_field_align(jl_datatype_t *dt, size_t i)
{
    unsigned al = jl_field_offset(dt, i);
    al |= 16;
    al &= -al;
    return std::min({al, (unsigned)jl_datatype_align(dt), (unsigned)JL_HEAP_ALIGNMENT});
}

static Type *julia_struct_to_llvm(jl_value_t *jt, jl_unionall_t *ua, bool *isboxed, bool llvmcall)
{
    // this function converts a Julia Type into the equivalent LLVM struct
    // use this where C-compatible (unboxed) structs are desired
    // use julia_type_to_llvm directly when you want to preserve Julia's type semantics
    if (isboxed) *isboxed = false;
    if (jt == (jl_value_t*)jl_bottom_type)
        return T_void;
    if (jl_is_primitivetype(jt))
        return bitstype_to_llvm(jt, llvmcall);
    if (jl_is_structtype(jt)) {
        jl_datatype_t *jst = (jl_datatype_t*)jt;
        bool isTuple = jl_is_tuple_type(jt);
        // don't use pre-filled struct_decl for llvmcall
        if (!llvmcall && (jst->struct_decl != NULL))
            return (Type*)jst->struct_decl;
        if (jl_is_structtype(jt) && !(jst->layout && jl_is_layout_opaque(jst->layout))) {
            jl_svec_t *ftypes = jl_get_fieldtypes(jst);
            size_t i, ntypes = jl_svec_len(ftypes);
            if (ntypes == 0 || (jst->layout && jl_datatype_nbits(jst) == 0))
                return T_void;
            if (!julia_struct_has_layout(jst, ua))
                return NULL;
            std::vector<Type*> latypes(0);
            bool isarray = true;
            bool isvector = true;
            jl_value_t *jlasttype = NULL;
            Type *lasttype = NULL;
            bool allghost = true;
            for (i = 0; i < ntypes; i++) {
                jl_value_t *ty = jl_svecref(ftypes, i);
                if (jlasttype != NULL && ty != jlasttype)
                    isvector = false;
                jlasttype = ty;
                size_t fsz = 0, al = 0;
                bool isptr = !jl_islayout_inline(ty, &fsz, &al);
                if (jst->layout) {
                    assert(isptr == jl_field_isptr(jst, i));
                    assert((isptr ? sizeof(void*) : fsz + jl_is_uniontype(ty)) == jl_field_size(jst, i));
                }
                Type *lty;
                if (isptr) {
                    lty = T_prjlvalue;
                    isvector = false;
                }
                else if (ty == (jl_value_t*)jl_bool_type) {
                    lty = T_int8;
                }
                else if (jl_is_uniontype(ty)) {
                    // pick an Integer type size such that alignment will generally be correct,
                    // and always end with an Int8 (selector byte).
                    // We may need to insert padding first to get to the right offset
                    if (al > MAX_ALIGN) {
                        Type *AlignmentType = ArrayType::get(VectorType::get(T_int8, al), 0);
                        latypes.push_back(AlignmentType);
                        al = MAX_ALIGN;
                    }
                    assert(al <= jl_field_align(jst, i));
                    Type *AlignmentType = IntegerType::get(jl_LLVMContext, 8 * al);
                    unsigned NumATy = fsz / al;
                    unsigned remainder = fsz % al;
                    while (NumATy--)
                        latypes.push_back(AlignmentType);
                    while (remainder--)
                        latypes.push_back(T_int8);
                    latypes.push_back(T_int8);
                    isarray = false;
                    allghost = false;
                    continue;
                }
                else {
                    lty = julia_struct_to_llvm(ty, NULL, &isptr, llvmcall);
                    assert(!isptr);
                }
                if (lasttype != NULL && lasttype != lty)
                    isarray = false;
                lasttype = lty;
                if (!type_is_ghost(lty)) {
                    allghost = false;
                    latypes.push_back(lty);
                }
            }
            Type *decl;
            if (allghost) {
                assert(jst->layout == NULL); // otherwise should have been caught above
                decl = T_void;
            }
            else if (jl_is_vecelement_type(jt)) {
                // VecElement type is unwrapped in LLVM
                decl = latypes[0];
            }
            else if (isarray && !type_is_ghost(lasttype)) {
                if (isTuple && isvector && jl_special_vector_alignment(ntypes, jlasttype) != 0)
                    decl = VectorType::get(lasttype, ntypes);
                else
                    decl = ArrayType::get(lasttype, ntypes);
            }
            else {
#if 0 // stress-test code that tries to assume julia-index == llvm-index
          // (also requires change to emit_new_struct to not assume 0 == 0)
                if (!isTuple && latypes.size() > 1) {
                    Type *NoopType = ArrayType::get(T_int1, 0);
                    latypes.insert(latypes.begin(), NoopType);
                }
#endif
                decl = StructType::get(jl_LLVMContext, latypes);
            }
            // don't use struct_decl cache for llvmcall
            if (!llvmcall)
                jst->struct_decl = decl;
            return decl;
        }
    }
    // TODO: enable this (with tests):
    // if (jl_is_uniontype(jt)) {
    //  // pick an Integer type size such that alignment will be correct
    //  // and always end with an Int8 (selector byte)
    //  lty = ArrayType::get(IntegerType::get(jl_LLVMContext, 8 * al), fsz / al);
    //  std::vector<Type*> Elements(2);
    //  Elements[0] = lty;
    //  Elements[1] = T_int8;
    //  unsigned remainder = fsz % al;
    //  while (remainder--)
    //      Elements.push_back(T_int8);
    //  lty = StructType::get(jl_LLVMContext, makeArrayRef(Elements));
    // }
    if (isboxed) *isboxed = true;
    return T_prjlvalue;
}

static bool is_datatype_all_pointers(jl_datatype_t *dt)
{
    size_t i, l = jl_datatype_nfields(dt);
    for (i = 0; i < l; i++) {
        if (!jl_field_isptr(dt, i)) {
            return false;
        }
    }
    return true;
}

static bool is_tupletype_homogeneous(jl_svec_t *t, bool allow_va = false)
{
    size_t i, l = jl_svec_len(t);
    if (l > 0) {
        jl_value_t *t0 = jl_svecref(t, 0);
        if (!jl_is_concrete_type(t0)) {
            if (allow_va && jl_is_vararg_type(t0) &&
                  jl_is_concrete_type(jl_unwrap_vararg(t0)))
                return true;
            return false;
        }
        for (i = 1; i < l; i++) {
            if (allow_va && i == l - 1 && jl_is_vararg_type(jl_svecref(t, i))) {
                if (t0 != jl_unwrap_vararg(jl_svecref(t, i)))
                    return false;
                continue;
            }
            if (t0 != jl_svecref(t, i))
                return false;
        }
    }
    return true;
}

static bool for_each_uniontype_small(
        std::function<void(unsigned, jl_datatype_t*)> f,
        jl_value_t *ty,
        unsigned &counter)
{
    if (counter > 127)
        return false;
    if (jl_is_uniontype(ty)) {
        bool allunbox = for_each_uniontype_small(f, ((jl_uniontype_t*)ty)->a, counter);
        allunbox &= for_each_uniontype_small(f, ((jl_uniontype_t*)ty)->b, counter);
        return allunbox;
    }
    else if (jl_is_pointerfree(ty)) {
        f(++counter, (jl_datatype_t*)ty);
        return true;
    }
    return false;
}

static Value *emit_typeof_boxed(jl_codectx_t &ctx, const jl_cgval_t &p);

static unsigned get_box_tindex(jl_datatype_t *jt, jl_value_t *ut)
{
    unsigned new_idx = 0;
    unsigned new_counter = 0;
    for_each_uniontype_small(
            // find the corresponding index in the new union-type
            [&](unsigned new_idx_, jl_datatype_t *new_jt) {
                if (jt == new_jt)
                    new_idx = new_idx_;
            },
            ut,
            new_counter);
    return new_idx;
}


// --- generating various field accessors ---

static Value *emit_nthptr_addr(jl_codectx_t &ctx, Value *v, ssize_t n, bool gctracked = true)
{
    return ctx.builder.CreateInBoundsGEP(
            T_prjlvalue,
            emit_bitcast(ctx, maybe_decay_tracked(v), T_pprjlvalue),
            ConstantInt::get(T_size, n));
}

static Value *emit_nthptr_addr(jl_codectx_t &ctx, Value *v, Value *idx)
{
    return ctx.builder.CreateInBoundsGEP(
            T_prjlvalue,
            emit_bitcast(ctx, maybe_decay_tracked(v), T_pprjlvalue),
            idx);
}

static Value *emit_nthptr(jl_codectx_t &ctx, Value *v, ssize_t n, MDNode *tbaa)
{
    // p = (jl_value_t**)v; p[n]
    Value *vptr = emit_nthptr_addr(ctx, v, n);
    return tbaa_decorate(tbaa, ctx.builder.CreateLoad(T_prjlvalue, vptr));
}

static Value *emit_nthptr_recast(jl_codectx_t &ctx, Value *v, Value *idx, MDNode *tbaa, Type *ptype)
{
    // p = (jl_value_t**)v; *(ptype)&p[n]
    Value *vptr = emit_nthptr_addr(ctx, v, idx);
    return tbaa_decorate(tbaa, ctx.builder.CreateLoad(emit_bitcast(ctx, vptr, ptype)));
}

static Value *emit_nthptr_recast(jl_codectx_t &ctx, Value *v, ssize_t n, MDNode *tbaa, Type *ptype)
{
    // p = (jl_value_t**)v; *(ptype)&p[n]
    Value *vptr = emit_nthptr_addr(ctx, v, n);
    return tbaa_decorate(tbaa, ctx.builder.CreateLoad(emit_bitcast(ctx, vptr, ptype)));
}

static Value *boxed(jl_codectx_t &ctx, const jl_cgval_t &v);

static Value *emit_typeof(jl_codectx_t &ctx, Value *tt)
{
    assert(tt != NULL && !isa<AllocaInst>(tt) && "expected a conditionally boxed value");
    return ctx.builder.CreateCall(prepare_call(jl_typeof_func), {tt});
}

static jl_cgval_t emit_typeof(jl_codectx_t &ctx, const jl_cgval_t &p)
{
    // given p, compute its type
    if (p.constant)
        return mark_julia_const(jl_typeof(p.constant));
    if (p.isboxed && !jl_is_concrete_type(p.typ)) {
        if (jl_is_type_type(p.typ)) {
            jl_value_t *tp = jl_tparam0(p.typ);
            if (!jl_is_type(tp) || jl_is_concrete_type(tp)) {
                // convert 1::Type{1} ==> typeof(1) ==> Int
                return mark_julia_const(jl_typeof(tp));
            }
        }
        return mark_julia_type(ctx, emit_typeof(ctx, p.V), true, jl_datatype_type);
    }
    if (p.TIndex) {
        Value *tindex = ctx.builder.CreateAnd(p.TIndex, ConstantInt::get(T_int8, 0x7f));
        unsigned counter = 0;
        bool allunboxed = for_each_uniontype_small(
                [&](unsigned idx, jl_datatype_t *jt) { },
                p.typ,
                counter);
        Value *datatype_or_p = (imaging_mode ? Constant::getNullValue(T_ppjlvalue) :
                                Constant::getNullValue(T_prjlvalue));
        counter = 0;
        for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                Value *cmp = ctx.builder.CreateICmpEQ(tindex, ConstantInt::get(T_int8, idx));
                Value *ptr;
                if (imaging_mode) {
                    ptr = literal_pointer_val_slot(ctx, (jl_value_t*)jt);
                }
                else {
                    ptr = maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)jt));
                }
                datatype_or_p = ctx.builder.CreateSelect(cmp, ptr, datatype_or_p);
            },
            p.typ,
            counter);
        auto emit_unboxty = [&] () -> Value* {
            if (imaging_mode)
                return maybe_decay_untracked(
                    tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(T_pjlvalue, datatype_or_p)));
            return datatype_or_p;
        };
        Value *res;
        if (!allunboxed) {
            Value *isnull = ctx.builder.CreateIsNull(datatype_or_p);
            BasicBlock *boxBB = BasicBlock::Create(jl_LLVMContext, "boxed", ctx.f);
            BasicBlock *unboxBB = BasicBlock::Create(jl_LLVMContext, "unboxed", ctx.f);
            BasicBlock *mergeBB = BasicBlock::Create(jl_LLVMContext, "merge", ctx.f);
            ctx.builder.CreateCondBr(isnull, boxBB, unboxBB);
            ctx.builder.SetInsertPoint(boxBB);
            auto boxTy = emit_typeof(ctx, p.Vboxed);
            ctx.builder.CreateBr(mergeBB);
            ctx.builder.SetInsertPoint(unboxBB);
            auto unboxTy = emit_unboxty();
            ctx.builder.CreateBr(mergeBB);
            ctx.builder.SetInsertPoint(mergeBB);
            auto phi = ctx.builder.CreatePHI(T_prjlvalue, 2);
            phi->addIncoming(boxTy, boxBB);
            phi->addIncoming(unboxTy, unboxBB);
            res = phi;
        }
        else {
            res = emit_unboxty();
        }
        return mark_julia_type(ctx, res, true, jl_datatype_type);
    }
    return mark_julia_const(p.typ);
}

static Value *emit_typeof_boxed(jl_codectx_t &ctx, const jl_cgval_t &p)
{
    return boxed(ctx, emit_typeof(ctx, p));
}

static Value *emit_datatype_types(jl_codectx_t &ctx, Value *dt)
{
    Value *Ptr = emit_bitcast(ctx, decay_derived(dt), T_ppjlvalue);
    Value *Idx = ConstantInt::get(T_size, offsetof(jl_datatype_t, types) / sizeof(void*));
    return tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(T_pjlvalue, ctx.builder.CreateInBoundsGEP(T_pjlvalue, Ptr, Idx)));
}

static Value *emit_datatype_nfields(jl_codectx_t &ctx, Value *dt)
{
    Value *type_svec = emit_bitcast(ctx, emit_datatype_types(ctx, dt), T_psize);
    return tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(T_size, type_svec));
}

static Value *emit_datatype_size(jl_codectx_t &ctx, Value *dt)
{
    Value *Ptr = emit_bitcast(ctx, decay_derived(dt), T_pint32);
    Value *Idx = ConstantInt::get(T_size, offsetof(jl_datatype_t, size) / sizeof(int));
    return tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(T_int32, ctx.builder.CreateInBoundsGEP(T_int32, Ptr, Idx)));
}

/* this is valid code, it's simply unused
static Value *emit_sizeof(jl_codectx_t &ctx, const jl_cgval_t &p)
{
    if (p.TIndex) {
        Value *tindex = ctx.builder.CreateAnd(p.TIndex, ConstantInt::get(T_int8, 0x7f));
        Value *size = ConstantInt::get(T_int32, -1);
        unsigned counter = 0;
        bool allunboxed = for_each_uniontype_small(
                [&](unsigned idx, jl_datatype_t *jt) {
                    Value *cmp = ctx.builder.CreateICmpEQ(tindex, ConstantInt::get(T_int8, idx));
                    size = ctx.builder.CreateSelect(cmp, ConstantInt::get(T_int32, jl_datatype_size(jt)), size);
                },
                p.typ,
                counter);
        if (!allunboxed && p.ispointer() && p.V && !isa<AllocaInst>(p.V)) {
            BasicBlock *currBB = ctx.builder.GetInsertBlock();
            BasicBlock *dynloadBB = BasicBlock::Create(jl_LLVMContext, "dyn_sizeof", ctx.f);
            BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_sizeof", ctx.f);
            Value *isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(p.TIndex, ConstantInt::get(T_int8, 0x80)),
                    ConstantInt::get(T_int8, 0));
            ctx.builder.CreateCondBr(isboxed, dynloadBB, postBB);
            ctx.builder.SetInsertPoint(dynloadBB);
            Value *datatype = emit_typeof(p.V);
            Value *dyn_size = emit_datatype_size(ctx, datatype);
            ctx.builder.CreateBr(postBB);
            ctx.builder.SetInsertPoint(postBB);
            PHINode *sizeof_merge = ctx.builder.CreatePHI(T_int32, 2);
            sizeof_merge->addIncoming(dyn_size, dynloadBB);
            sizeof_merge->addIncoming(size, currBB);
            size = sizeof_merge;
        }
#ifndef NDEBUG
        // try to catch codegen errors early, before it uses this to memcpy over the entire stack
        CreateConditionalAbort(ctx.builder, ctx.builder.CreateICmpEQ(size, ConstantInt::get(T_int32, -1)));
#endif
        return size;
    }
    else if (jl_is_concrete_type(p.typ)) {
        return ConstantInt::get(T_int32, jl_datatype_size(p.typ));
    }
    else {
        Value *datatype = emit_typeof_boxed(ctx, p);
        Value *dyn_size = emit_datatype_size(ctx, datatype);
        return dyn_size;
    }
}
*/

static Value *emit_datatype_mutabl(jl_codectx_t &ctx, Value *dt)
{
    Value *Ptr = emit_bitcast(ctx, decay_derived(dt), T_pint8);
    Value *Idx = ConstantInt::get(T_size, offsetof(jl_datatype_t, mutabl));
    Value *mutabl = tbaa_decorate(tbaa_const,
            ctx.builder.CreateLoad(T_int8, ctx.builder.CreateInBoundsGEP(T_int8, Ptr, Idx)));
    return ctx.builder.CreateTrunc(mutabl, T_int1);
}

/* this is valid code, it's simply unused
static Value *emit_datatype_abstract(jl_codectx_t &ctx, Value *dt)
{
    Value *Ptr = emit_bitcast(ctx, decay_derived(dt), T_pint8);
    Value *Idx = ConstantInt::get(T_size, offsetof(jl_datatype_t, abstract));

    Value *abstract = tbaa_decorate(tbaa_const,
            ctx.builder.CreateLoad(T_int8, ctx.builder.CreateInBoundsGEP(T_int8, Ptr, Idx)));
    return ctx.builder.CreateTrunc(abstract, T_int1);
}
*/

static Value *emit_datatype_isprimitivetype(jl_codectx_t &ctx, Value *dt)
{
    Value *immut = ctx.builder.CreateNot(emit_datatype_mutabl(ctx, dt));
    Value *nofields = ctx.builder.CreateICmpEQ(emit_datatype_nfields(ctx, dt), ConstantInt::get(T_size, 0));
    Value *sized = ctx.builder.CreateICmpSGT(emit_datatype_size(ctx, dt), ConstantInt::get(T_int32, 0));
    return ctx.builder.CreateAnd(immut, ctx.builder.CreateAnd(nofields, sized));
}

static Value *emit_datatype_name(jl_codectx_t &ctx, Value *dt)
{
    return emit_nthptr(
            ctx, dt,
            (ssize_t)(offsetof(jl_datatype_t, name) / sizeof(char*)),
            tbaa_const);
}

// --- generating various error checks ---
// Do not use conditional throw for cases that type inference can know
// the error is always thrown. This may cause non dominated use
// of SSA value error in the verifier.

static void just_emit_error(jl_codectx_t &ctx, const std::string &txt)
{
    ctx.builder.CreateCall(prepare_call(jlerror_func), stringConstPtr(ctx.builder, txt));
}

static void emit_error(jl_codectx_t &ctx, const std::string &txt)
{
    just_emit_error(ctx, txt);
    ctx.builder.CreateUnreachable();
    BasicBlock *cont = BasicBlock::Create(jl_LLVMContext,"after_error",ctx.f);
    ctx.builder.SetInsertPoint(cont);
}

// DO NOT PASS IN A CONST CONDITION!
static void error_unless(jl_codectx_t &ctx, Value *cond, const std::string &msg)
{
    BasicBlock *failBB = BasicBlock::Create(jl_LLVMContext,"fail",ctx.f);
    BasicBlock *passBB = BasicBlock::Create(jl_LLVMContext,"pass");
    ctx.builder.CreateCondBr(cond, passBB, failBB);
    ctx.builder.SetInsertPoint(failBB);
    just_emit_error(ctx, msg);
    ctx.builder.CreateUnreachable();
    ctx.f->getBasicBlockList().push_back(passBB);
    ctx.builder.SetInsertPoint(passBB);
}

static void raise_exception(jl_codectx_t &ctx, Value *exc,
                            BasicBlock *contBB=nullptr)
{
    if (JL_HOOK_TEST(ctx.params, raise_exception)) {
        JL_HOOK_CALL(ctx.params, raise_exception, 2,
                     jl_box_voidpointer(wrap(ctx.builder.GetInsertBlock())),
                     jl_box_voidpointer(wrap(exc)));
    } else {
        ctx.builder.CreateCall(prepare_call(jlthrow_func), { mark_callee_rooted(exc) });
    }
    ctx.builder.CreateUnreachable();
    if (!contBB) {
        contBB = BasicBlock::Create(jl_LLVMContext, "after_throw", ctx.f);
    }
    else {
        ctx.f->getBasicBlockList().push_back(contBB);
    }
    ctx.builder.SetInsertPoint(contBB);
}

// DO NOT PASS IN A CONST CONDITION!
static void raise_exception_unless(jl_codectx_t &ctx, Value *cond, Value *exc)
{
    BasicBlock *failBB = BasicBlock::Create(jl_LLVMContext,"fail",ctx.f);
    BasicBlock *passBB = BasicBlock::Create(jl_LLVMContext,"pass");
    ctx.builder.CreateCondBr(cond, passBB, failBB);
    ctx.builder.SetInsertPoint(failBB);
    raise_exception(ctx, exc, passBB);
}

static void null_pointer_check(jl_codectx_t &ctx, Value *v)
{
    raise_exception_unless(ctx,
            ctx.builder.CreateICmpNE(v, Constant::getNullValue(v->getType())),
            literal_pointer_val(ctx, jl_undefref_exception));
}

static void emit_type_error(jl_codectx_t &ctx, const jl_cgval_t &x, Value *type, const std::string &msg)
{
    Value *msg_val = stringConstPtr(ctx.builder, msg);
    ctx.builder.CreateCall(prepare_call(jltypeerror_func),
                       { msg_val, maybe_decay_untracked(type), mark_callee_rooted(boxed(ctx, x))});
}

static std::pair<Value*, bool> emit_isa(jl_codectx_t &ctx, const jl_cgval_t &x, jl_value_t *type, const std::string *msg)
{
    // TODO: The subtype check below suffers from incorrectness issues due to broken
    // subtyping for kind types (see https://github.com/JuliaLang/julia/issues/27078). For
    // actual `isa` calls, this optimization should already have been performed upstream
    // anyway, but having this optimization in codegen might still be beneficial for
    // `typeassert`s if we can make it correct.
    Optional<bool> known_isa;
    jl_value_t *intersected_type = type;
    if (x.constant)
        known_isa = jl_isa(x.constant, type);
    else if (jl_is_not_broken_subtype(x.typ, type) && jl_subtype(x.typ, type)) {
        known_isa = true;
    } else {
        intersected_type = jl_type_intersection(x.typ, type);
        if (intersected_type == (jl_value_t*)jl_bottom_type)
            known_isa = false;
    }
    if (known_isa) {
        if (!*known_isa && msg) {
            emit_type_error(ctx, x, literal_pointer_val(ctx, type), *msg);
            ctx.builder.CreateUnreachable();
            BasicBlock *failBB = BasicBlock::Create(jl_LLVMContext, "fail", ctx.f);
            ctx.builder.SetInsertPoint(failBB);
        }
        return std::make_pair(ConstantInt::get(T_int1, *known_isa), true);
    }

    // intersection with Type needs to be handled specially
    if (jl_has_intersect_type_not_kind(type) || jl_has_intersect_type_not_kind(intersected_type)) {
        Value *vx = maybe_decay_untracked(boxed(ctx, x));
        Value *vtyp = maybe_decay_untracked(literal_pointer_val(ctx, type));
        if (msg && *msg == "typeassert") {
            ctx.builder.CreateCall(prepare_call(jltypeassert_func), { vx, vtyp });
            return std::make_pair(ConstantInt::get(T_int1, 1), true);
        }
        return std::make_pair(ctx.builder.CreateICmpNE(
                ctx.builder.CreateCall(prepare_call(jlisa_func), { vx, vtyp }),
                ConstantInt::get(T_int32, 0)), false);
    }
    // tests for isa concretetype can be handled with pointer comparisons
    if (jl_is_concrete_type(intersected_type)) {
        if (x.TIndex) {
            unsigned tindex = get_box_tindex((jl_datatype_t*)intersected_type, x.typ);
            if (tindex > 0) {
                // optimize more when we know that this is a split union-type where tindex = 0 is invalid
                Value *xtindex = ctx.builder.CreateAnd(x.TIndex, ConstantInt::get(T_int8, 0x7f));
                return std::make_pair(ctx.builder.CreateICmpEQ(xtindex, ConstantInt::get(T_int8, tindex)), false);
            }
            else if (x.Vboxed) {
                // test for (x.TIndex == 0x80 && typeof(x.V) == type)
                Value *isboxed = ctx.builder.CreateICmpEQ(x.TIndex, ConstantInt::get(T_int8, 0x80));
                BasicBlock *currBB = ctx.builder.GetInsertBlock();
                BasicBlock *isaBB = BasicBlock::Create(jl_LLVMContext, "isa", ctx.f);
                BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_isa", ctx.f);
                ctx.builder.CreateCondBr(isboxed, isaBB, postBB);
                ctx.builder.SetInsertPoint(isaBB);
                Value *istype_boxed = ctx.builder.CreateICmpEQ(emit_typeof(ctx, x.Vboxed),
                    maybe_decay_untracked(literal_pointer_val(ctx, intersected_type)));
                ctx.builder.CreateBr(postBB);
                ctx.builder.SetInsertPoint(postBB);
                PHINode *istype = ctx.builder.CreatePHI(T_int1, 2);
                istype->addIncoming(ConstantInt::get(T_int1, 0), currBB);
                istype->addIncoming(istype_boxed, isaBB);
                return std::make_pair(istype, false);
            } else {
                // handle the case where we know that `x` is unboxed (but of unknown type), but that concrete type `type` cannot be unboxed
                return std::make_pair(ConstantInt::get(T_int1, 0), false);
            }
        }
        return std::make_pair(ctx.builder.CreateICmpEQ(emit_typeof_boxed(ctx, x),
            maybe_decay_untracked(literal_pointer_val(ctx, intersected_type))), false);
    }
    jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(intersected_type);
    if (jl_is_datatype(dt) && !dt->abstract && jl_subtype(dt->name->wrapper, type)) {
        // intersection is a supertype of all instances of its constructor,
        // so the isa test reduces to a comparison of the typename by pointer
        return std::make_pair(
                ctx.builder.CreateICmpEQ(
                    mark_callee_rooted(emit_datatype_name(ctx, emit_typeof_boxed(ctx, x))),
                    mark_callee_rooted(literal_pointer_val(ctx, (jl_value_t*)dt->name))),
                false);
    }
    // everything else can be handled via subtype tests
    return std::make_pair(ctx.builder.CreateICmpNE(
            ctx.builder.CreateCall(prepare_call(jlsubtype_func),
              { maybe_decay_untracked(emit_typeof_boxed(ctx, x)),
                maybe_decay_untracked(literal_pointer_val(ctx, type)) }),
            ConstantInt::get(T_int32, 0)), false);
}

static void emit_typecheck(jl_codectx_t &ctx, const jl_cgval_t &x, jl_value_t *type, const std::string &msg)
{
    Value *istype;
    bool handled_msg;
    std::tie(istype, handled_msg) = emit_isa(ctx, x, type, &msg);
    if (!handled_msg) {
        BasicBlock *failBB = BasicBlock::Create(jl_LLVMContext, "fail", ctx.f);
        BasicBlock *passBB = BasicBlock::Create(jl_LLVMContext, "pass");
        ctx.builder.CreateCondBr(istype, passBB, failBB);
        ctx.builder.SetInsertPoint(failBB);

        emit_type_error(ctx, x, literal_pointer_val(ctx, type), msg);
        ctx.builder.CreateUnreachable();

        ctx.f->getBasicBlockList().push_back(passBB);
        ctx.builder.SetInsertPoint(passBB);
    }
}

static Value *emit_isconcrete(jl_codectx_t &ctx, Value *typ)
{
    Value *isconcrete;
    isconcrete = ctx.builder.CreateConstInBoundsGEP1_32(T_int8, emit_bitcast(ctx, decay_derived(typ), T_pint8), offsetof(jl_datatype_t, isconcretetype));
    isconcrete = ctx.builder.CreateLoad(T_int8, isconcrete, tbaa_const);
    isconcrete = ctx.builder.CreateTrunc(isconcrete, T_int1);
    return isconcrete;
}

static void emit_concretecheck(jl_codectx_t &ctx, Value *typ, const std::string &msg)
{
    assert(typ->getType() == T_prjlvalue);
    emit_typecheck(ctx, mark_julia_type(ctx, typ, true, jl_any_type), (jl_value_t*)jl_datatype_type, msg);
    error_unless(ctx, emit_isconcrete(ctx, typ), msg);
}

#define CHECK_BOUNDS 1
static bool bounds_check_enabled(jl_codectx_t &ctx, jl_value_t *inbounds) {
#if CHECK_BOUNDS==1
    if (jl_options.check_bounds == JL_OPTIONS_CHECK_BOUNDS_ON)
        return 1;
    if (jl_options.check_bounds == JL_OPTIONS_CHECK_BOUNDS_OFF)
        return 0;
    if (inbounds == jl_false)
        return 0;
    return 1;
#else
    return 0;
#endif
}

static Value *emit_bounds_check(jl_codectx_t &ctx, const jl_cgval_t &ainfo, jl_value_t *ty, Value *i, Value *len, jl_value_t *boundscheck)
{
    Value *im1 = ctx.builder.CreateSub(i, ConstantInt::get(T_size, 1));
#if CHECK_BOUNDS==1
    if (bounds_check_enabled(ctx, boundscheck)) {
        Value *ok = ctx.builder.CreateICmpULT(im1, len);
        BasicBlock *failBB = BasicBlock::Create(jl_LLVMContext, "fail", ctx.f);
        BasicBlock *passBB = BasicBlock::Create(jl_LLVMContext, "pass");
        ctx.builder.CreateCondBr(ok, passBB, failBB);
        ctx.builder.SetInsertPoint(failBB);
        if (!ty) { // jl_value_t** tuple (e.g. the vararg)
            ctx.builder.CreateCall(prepare_call(jlvboundserror_func), { ainfo.V, len, i });
        }
        else if (ainfo.isboxed) { // jl_datatype_t or boxed jl_value_t
            ctx.builder.CreateCall(prepare_call(jlboundserror_func), { mark_callee_rooted(boxed(ctx, ainfo)), i });
        }
        else { // unboxed jl_value_t*
            Value *a = ainfo.V;
            if (ainfo.isghost) {
                a = Constant::getNullValue(T_pint8);
            }
            else if (!ainfo.ispointer()) {
                // CreateAlloca is OK here since we are on an error branch
                Value *tempSpace = ctx.builder.CreateAlloca(a->getType());
                ctx.builder.CreateStore(a, tempSpace);
                a = tempSpace;
            }
            ctx.builder.CreateCall(prepare_call(jluboundserror_func), {
                    emit_bitcast(ctx, decay_derived(a), T_pint8),
                    literal_pointer_val(ctx, ty),
                    i });
        }
        ctx.builder.CreateUnreachable();
        ctx.f->getBasicBlockList().push_back(passBB);
        ctx.builder.SetInsertPoint(passBB);
    }
#endif
    return im1;
}

static Value *emit_unbox(jl_codectx_t &ctx, Type *to, const jl_cgval_t &x, jl_value_t *jt, Value* dest = NULL, MDNode *tbaa_dest = nullptr, bool isVolatile = false);
static void emit_write_barrier(jl_codectx_t&, Value*, ArrayRef<Value*>);
static void emit_write_barrier(jl_codectx_t&, Value*, Value*);
static void emit_write_multibarrier(jl_codectx_t&, Value*, Value*);

std::vector<unsigned> first_ptr(Type *T)
{
    if (isa<CompositeType>(T)) {
        if (!isa<StructType>(T) && cast<SequentialType>(T)->getNumElements() == 0)
            return {};
        unsigned i = 0;
        for (Type *ElTy : T->subtypes()) {
            if (isa<PointerType>(ElTy) && ElTy->getPointerAddressSpace() == AddressSpace::Tracked) {
                return std::vector<unsigned>{i};
            }
            auto path = first_ptr(ElTy);
            if (!path.empty()) {
                path.push_back(i);
                return path;
            }
            i++;
        }
    }
    return {};
}
Value *extract_first_ptr(jl_codectx_t &ctx, Value *V)
{
    auto path = first_ptr(V->getType());
    if (path.empty())
        return NULL;
    std::reverse(std::begin(path), std::end(path));
    return ctx.builder.CreateExtractValue(V, path);
}

static jl_cgval_t typed_load(jl_codectx_t &ctx, Value *ptr, Value *idx_0based, jl_value_t *jltype,
                             MDNode *tbaa, MDNode *aliasscope,
                             bool maybe_null_if_boxed = true, unsigned alignment = 0)
{
    bool isboxed;
    Type *elty = julia_type_to_llvm(jltype, &isboxed);
    if (type_is_ghost(elty))
        return ghostValue(jltype);
    Type *ptrty = PointerType::get(elty, ptr->getType()->getPointerAddressSpace());
    Value *data;
    if (ptr->getType() != ptrty)
        data = emit_bitcast(ctx, ptr, ptrty);
    else
        data = ptr;
    if (idx_0based)
        data = ctx.builder.CreateInBoundsGEP(elty, data, idx_0based);
    Instruction *load;
    // TODO: can only lazy load if we can create a gc root for ptr for the lifetime of elt
    //if (elty->isAggregateType() && tbaa == tbaa_immut && !alignment) { // can lazy load on demand, no copy needed
    //    elt = data;
    //}
    //else {
        load = ctx.builder.CreateAlignedLoad(data,
            isboxed || alignment ? alignment : julia_alignment(jltype),
            false);
        if (aliasscope)
            load->setMetadata("alias.scope", aliasscope);
        if (isboxed)
            load = maybe_mark_load_dereferenceable(load, true, jltype);
        if (tbaa)
            load = tbaa_decorate(tbaa, load);
        if (maybe_null_if_boxed) {
            Value *first_ptr = isboxed ? load : extract_first_ptr(ctx, load);
            if (first_ptr)
                null_pointer_check(ctx, first_ptr);
        }
    //}
    if (jltype == (jl_value_t*)jl_bool_type) { // "freeze" undef memory to a valid value
        // NOTE: if we zero-initialize arrays, this optimization should become valid
        //load->setMetadata(LLVMContext::MD_range, MDNode::get(jl_LLVMContext, {
        //    ConstantAsMetadata::get(ConstantInt::get(T_int8, 0)),
        //    ConstantAsMetadata::get(ConstantInt::get(T_int8, 2)) }));
        load = ctx.builder.Insert(CastInst::Create(Instruction::Trunc, load, T_int1));
    }
    return mark_julia_type(ctx, load, isboxed, jltype);
}

static void typed_store(jl_codectx_t &ctx,
        Value *ptr, Value *idx_0based, const jl_cgval_t &rhs,
        jl_value_t *jltype, MDNode *tbaa, MDNode *aliasscope,
        Value *parent,  // for the write barrier, NULL if no barrier needed
        unsigned alignment = 0)
{
    bool isboxed;
    Type *elty = julia_type_to_llvm(jltype, &isboxed);
    if (type_is_ghost(elty))
        return;
    Value *r;
    if (!isboxed) {
        r = emit_unbox(ctx, elty, rhs, jltype);
        if (parent != NULL)
            emit_write_multibarrier(ctx, parent, r);
    }
    else {
        r = maybe_decay_untracked(boxed(ctx, rhs));
        if (parent != NULL)
            emit_write_barrier(ctx, parent, r);
    }
    Type *ptrty = PointerType::get(elty, ptr->getType()->getPointerAddressSpace());
    if (ptr->getType() != ptrty)
        ptr = ctx.builder.CreateBitCast(ptr, ptrty);
    if (idx_0based)
        ptr = ctx.builder.CreateInBoundsGEP(r->getType(), ptr, idx_0based);
    Instruction *store = ctx.builder.CreateAlignedStore(r, ptr, isboxed || alignment ? alignment : julia_alignment(jltype));
    if (aliasscope)
        store->setMetadata("noalias", aliasscope);
    if (tbaa)
        tbaa_decorate(tbaa, store);
}

// --- convert boolean value to julia ---

static Value *julia_bool(jl_codectx_t &ctx, Value *cond)
{
    return ctx.builder.CreateSelect(cond, literal_pointer_val(ctx, jl_true),
                                          literal_pointer_val(ctx, jl_false));
}

// --- accessing the representations of built-in data types ---

static Constant *julia_const_to_llvm(jl_value_t *e);
static Value *data_pointer(jl_codectx_t &ctx, const jl_cgval_t &x)
{
    assert(x.ispointer());
    Value *data = x.V;
    if (x.constant) {
        Constant *val = julia_const_to_llvm(x.constant);
        if (val) {
            data = get_pointer_to_constant(val, "", *jl_Module);
        }
        else {
            data = literal_pointer_val(ctx, x.constant);
        }
    }
    return data;
}

static void emit_memcpy_llvm(jl_codectx_t &ctx, Value *dst, MDNode *tbaa_dst, Value *src, MDNode *tbaa_src,
                             uint64_t sz, unsigned align, bool is_volatile)
{
    if (sz == 0)
        return;
    assert(align && "align must be specified");
    // If the types are small and simple, use load and store directly.
    // Going through memcpy can cause LLVM (e.g. SROA) to create bitcasts between float and int
    // that interferes with other optimizations.
    if (sz <= 64) {
        // The size limit is arbitrary but since we mainly care about floating points and
        // machine size vectors this should be enough.
        const DataLayout &DL = jl_data_layout;
        auto srcty = cast<PointerType>(src->getType());
        auto srcel = srcty->getElementType();
        auto dstty = cast<PointerType>(dst->getType());
        auto dstel = dstty->getElementType();
        if (srcel->isArrayTy() && srcel->getArrayNumElements() == 1) {
            src = ctx.builder.CreateConstInBoundsGEP2_32(srcel, src, 0, 0);
            srcel = srcel->getArrayElementType();
            srcty = srcel->getPointerTo();
        }
        if (dstel->isArrayTy() && dstel->getArrayNumElements() == 1) {
            dst = ctx.builder.CreateConstInBoundsGEP2_32(dstel, dst, 0, 0);
            dstel = dstel->getArrayElementType();
            dstty = dstel->getPointerTo();
        }

        bool direct = false;
        if (srcel->isSized() && srcel->isSingleValueType() && DL.getTypeStoreSize(srcel) == sz) {
            direct = true;
            dst = emit_bitcast(ctx, dst, srcty);
        }
        else if (dstel->isSized() && dstel->isSingleValueType() &&
                 DL.getTypeStoreSize(dstel) == sz) {
            direct = true;
            src = emit_bitcast(ctx, src, dstty);
        }
        if (direct) {
            auto val = tbaa_decorate(tbaa_src, ctx.builder.CreateAlignedLoad(src, align, is_volatile));
            tbaa_decorate(tbaa_dst, ctx.builder.CreateAlignedStore(val, dst, align, is_volatile));
            return;
        }
    }
    // the memcpy intrinsic does not allow to specify different alias tags
    // for the load part (x.tbaa) and the store part (tbaa_stack).
    // since the tbaa lattice has to be a tree we have unfortunately
    // x.tbaa ∪ tbaa_stack = tbaa_root if x.tbaa != tbaa_stack
    ctx.builder.CreateMemCpy(dst, align, src, 0, sz, is_volatile, MDNode::getMostGenericTBAA(tbaa_dst, tbaa_src));
}

static void emit_memcpy_llvm(jl_codectx_t &ctx, Value *dst, MDNode *tbaa_dst, Value *src, MDNode *tbaa_src,
                             Value *sz, unsigned align, bool is_volatile)
{
    if (auto const_sz = dyn_cast<ConstantInt>(sz)) {
        emit_memcpy_llvm(ctx, dst, tbaa_dst, src, tbaa_src, const_sz->getZExtValue(), align, is_volatile);
        return;
    }
    ctx.builder.CreateMemCpy(dst, align, src, 0, sz, is_volatile, MDNode::getMostGenericTBAA(tbaa_dst, tbaa_src));
}

template<typename T1>
static void emit_memcpy(jl_codectx_t &ctx, Value *dst, MDNode *tbaa_dst, Value *src, MDNode *tbaa_src,
                        T1 &&sz, unsigned align, bool is_volatile=false)
{
    emit_memcpy_llvm(ctx, dst, tbaa_dst, src, tbaa_src, sz, align, is_volatile);
}

template<typename T1>
static void emit_memcpy(jl_codectx_t &ctx, Value *dst, MDNode *tbaa_dst, const jl_cgval_t &src,
                        T1 &&sz, unsigned align, bool is_volatile=false)
{
    emit_memcpy_llvm(ctx, dst, tbaa_dst, data_pointer(ctx, src), src.tbaa, sz, align, is_volatile);
}



static jl_cgval_t emit_getfield_knownidx(jl_codectx_t &ctx, const jl_cgval_t &strct, unsigned idx, jl_datatype_t *jt);

static bool emit_getfield_unknownidx(jl_codectx_t &ctx,
        jl_cgval_t *ret, jl_cgval_t strct,
        Value *idx, jl_datatype_t *stt, jl_value_t *inbounds)
{
    size_t nfields = jl_datatype_nfields(stt);
    bool maybe_null = (unsigned)stt->ninitialized != nfields;
    auto idx0 = [&]() {
        return emit_bounds_check(ctx, strct, (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), inbounds);
    };
    if (nfields == 0) {
        (void)idx0();
        *ret = jl_cgval_t();
        return true;
    }
    if (nfields == 1) {
        (void)idx0();
        *ret = emit_getfield_knownidx(ctx, strct, 0, stt);
        return true;
    }
    assert(!jl_is_vecelement_type((jl_value_t*)stt));

    if (!strct.ispointer()) { // unboxed
        assert(jl_is_concrete_immutable((jl_value_t*)stt));
        bool isboxed = is_datatype_all_pointers(stt);
        bool issame = is_tupletype_homogeneous(stt->types);
        if (issame) {
            jl_value_t *jft = jl_svecref(stt->types, 0);
            if (strct.isghost) {
                (void)idx0();
                *ret = ghostValue(jft);
                return true;
            }
            if (isa<VectorType>(strct.V->getType())) {
                assert(stt->layout->npointers == 0); // we could, but don't emit this
                idx = idx0();
                if (sizeof(void*) != sizeof(int))
                    idx = ctx.builder.CreateTrunc(idx, T_int32); // llvm3.3 requires this, harmless elsewhere
                Value *fld = ctx.builder.CreateExtractElement(strct.V, idx);
                *ret = mark_julia_type(ctx, fld, isboxed, jft);
                return true;
            }
            else if (isa<ArrayType>(strct.V->getType())) {
                if (!isboxed && nfields > 3) {
                    // For small objects and tracked pointers, emit a set of Select statements,
                    // otherwise emit as a stack load. This keeps LLVM from becoming unhappy
                    // about seeing loads of tracked pointers.
                    strct = value_to_pointer(ctx, strct);
                    assert(strct.ispointer());
                }
                // fall-through to next branch, where we'll handle it
            }
            else {
                llvm_unreachable("homogeneous struct should have had a homogeneous type");
            }
        }
        if (isboxed || (issame && isa<ArrayType>(strct.V->getType()))) {
            assert((cast<ArrayType>(strct.V->getType())->getElementType() == T_prjlvalue) == isboxed);
            Value *idx = idx0();
            unsigned i = 0;
            Value *fld = ctx.builder.CreateExtractValue(strct.V, makeArrayRef(i));
            for (i = 1; i < nfields; i++) {
                fld = ctx.builder.CreateSelect(
                        ctx.builder.CreateICmpEQ(idx, ConstantInt::get(idx->getType(), i)),
                        ctx.builder.CreateExtractValue(strct.V, makeArrayRef(i)),
                        fld);
            }
            jl_value_t *jft = issame ? jl_svecref(stt->types, 0) : (jl_value_t*)jl_any_type;
            if (isboxed && maybe_null)
                null_pointer_check(ctx, fld);
            *ret = mark_julia_type(ctx, fld, isboxed, jft);
            return true;
        }
    }

    if (strct.ispointer()) { // boxed or stack
        if (is_datatype_all_pointers(stt)) {
            size_t minimum_field_size = std::numeric_limits<size_t>::max();
            size_t minimum_align = JL_HEAP_ALIGNMENT;
            for (size_t i = 0; i < nfields; ++i) {
                jl_value_t *ft = jl_field_type(stt, i);
                minimum_field_size = std::min(minimum_field_size,
                    dereferenceable_size(ft));
                if (minimum_field_size == 0) {
                    minimum_align = 1;
                    break;
                }
                minimum_align = std::min(minimum_align,
                    (size_t)julia_alignment(ft));
            }
            Value *fldptr = ctx.builder.CreateInBoundsGEP(
                    T_prjlvalue,
                    maybe_decay_tracked(emit_bitcast(ctx, data_pointer(ctx, strct), T_pprjlvalue)),
                    idx0());
            Value *fld = tbaa_decorate(strct.tbaa,
                maybe_mark_load_dereferenceable(
                    ctx.builder.CreateLoad(T_prjlvalue, fldptr),
                    maybe_null, minimum_field_size, minimum_align));
            if (maybe_null)
                null_pointer_check(ctx, fld);
            *ret = mark_julia_type(ctx, fld, true, jl_any_type);
            return true;
        }
        else if (is_tupletype_homogeneous(stt->types)) {
            assert(nfields > 0); // nf == 0 trapped by all_pointers case
            jl_value_t *jft = jl_svecref(stt->types, 0);
            idx = idx0();
            Value *ptr = maybe_decay_tracked(data_pointer(ctx, strct));
            if (!stt->mutabl && !(maybe_null && jft == (jl_value_t*)jl_bool_type)) {
                // just compute the pointer and let user load it when necessary
                Type *fty = julia_type_to_llvm(jft);
                Value *addr = ctx.builder.CreateInBoundsGEP(fty, emit_bitcast(ctx, ptr, PointerType::get(fty, 0)), idx);
                *ret = mark_julia_slot(addr, jft, NULL, strct.tbaa);
                return true;
            }
            *ret = typed_load(ctx, ptr, idx, jft, strct.tbaa, nullptr, false);
            return true;
        }
        else if (strct.isboxed) {
            idx = ctx.builder.CreateSub(idx, ConstantInt::get(T_size, 1));
            Value *fld = ctx.builder.CreateCall(prepare_call(jlgetnthfieldchecked_func), { boxed(ctx, strct), idx });
            *ret = mark_julia_type(ctx, fld, true, jl_any_type);
            return true;
        }
    }
    return false;
}

static jl_cgval_t emit_getfield_knownidx(jl_codectx_t &ctx, const jl_cgval_t &strct, unsigned idx, jl_datatype_t *jt)
{
    jl_value_t *jfty = jl_field_type(jt, idx);
    if (jfty == jl_bottom_type) {
        raise_exception(ctx, literal_pointer_val(ctx, jl_undefref_exception));
        return jl_cgval_t(); // unreachable
    }
    if (type_is_ghost(julia_type_to_llvm(jfty)))
        return ghostValue(jfty);
    bool maybe_null = idx >= (unsigned)jt->ninitialized;
    size_t byte_offset = jl_field_offset(jt, idx);
    if (strct.ispointer()) {
        Value *staddr = maybe_decay_tracked(data_pointer(ctx, strct));
        bool isboxed;
        Type *lt = julia_type_to_llvm((jl_value_t*)jt, &isboxed);
        Value *addr;
        if (isboxed) {
            // byte_offset == 0 is an important special case here, e.g.
            // for single field wrapper types. Introducing the bitcast
            // can pessimize mem2reg
            if (byte_offset > 0) {
                addr = ctx.builder.CreateInBoundsGEP(
                        T_int8,
                        emit_bitcast(ctx, staddr, T_pint8),
                        ConstantInt::get(T_size, byte_offset));
            }
            else {
                addr = staddr;
            }
        }
        else {
            staddr = maybe_bitcast(ctx, staddr, lt->getPointerTo());
            if (jl_is_vecelement_type((jl_value_t*)jt))
                addr = staddr; // VecElement types are unwrapped in LLVM.
            else if (isa<StructType>(lt))
                addr = emit_struct_gep(ctx, lt, staddr, byte_offset);
            else
                addr = ctx.builder.CreateConstInBoundsGEP2_32(lt, staddr, 0, idx);
        }
        if (jl_field_isptr(jt, idx)) {
            Instruction *Load = maybe_mark_load_dereferenceable(
                    ctx.builder.CreateLoad(T_prjlvalue, maybe_bitcast(ctx, addr, T_pprjlvalue)),
                    maybe_null, jl_field_type(jt, idx));
            Value *fldv = tbaa_decorate(strct.tbaa, Load);
            if (maybe_null)
                null_pointer_check(ctx, fldv);
            return mark_julia_type(ctx, fldv, true, jfty);
        }
        else if (jl_is_uniontype(jfty)) {
            size_t fsz = 0, al = 0;
            bool isptr = !jl_islayout_inline(jfty, &fsz, &al);
            assert(!isptr && fsz == jl_field_size(jt, idx) - 1); (void)isptr;
            Value *ptindex;
            if (isboxed) {
                ptindex = ctx.builder.CreateConstInBoundsGEP1_32(
                    T_int8, emit_bitcast(ctx, staddr, T_pint8), byte_offset + fsz);
            }
            else {
                ptindex = emit_struct_gep(ctx, cast<StructType>(lt), staddr, byte_offset + fsz);
            }
            Instruction *tindex0 = tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateLoad(T_int8, ptindex));
            //tindex0->setMetadata(LLVMContext::MD_range, MDNode::get(jl_LLVMContext, {
            //    ConstantAsMetadata::get(ConstantInt::get(T_int8, 0)),
            //    ConstantAsMetadata::get(ConstantInt::get(T_int8, union_max)) }));
            Value *tindex = ctx.builder.CreateNUWAdd(ConstantInt::get(T_int8, 1), tindex0);
            if (jt->mutabl) {
                // move value to an immutable stack slot (excluding tindex)
                Type *ET = IntegerType::get(jl_LLVMContext, 8 * al);
                AllocaInst *lv = emit_static_alloca(ctx, ET);
                lv->setOperand(0, ConstantInt::get(T_int32, (fsz + al - 1) / al));
                emit_memcpy(ctx, lv, strct.tbaa, addr, strct.tbaa, fsz, al);
                addr = lv;
            }
            return mark_julia_slot(addr, jfty, tindex, strct.tbaa);
        }
        else if (!jt->mutabl && !(maybe_null && jfty == (jl_value_t*)jl_bool_type)) {
            // just compute the pointer and let user load it when necessary
            return mark_julia_slot(addr, jfty, NULL, strct.tbaa);
        }
        unsigned align = jl_field_align(jt, idx);
        return typed_load(ctx, addr, NULL, jfty, strct.tbaa, nullptr, true, align);
    }
    else if (isa<UndefValue>(strct.V)) {
        return jl_cgval_t();
    }
    else {
        Value *obj = strct.V; // aka emit_unbox
        Type *T = obj->getType();
        Value *fldv;
        if (jl_is_vecelement_type((jl_value_t*)jt)) {
            // VecElement types are unwrapped in LLVM.
            fldv = obj;
        }
        else if (isa<VectorType>(T)) {
            fldv = ctx.builder.CreateExtractElement(obj, ConstantInt::get(T_int32, idx));
        }
        else if (!jl_field_isptr(jt, idx) && jl_is_uniontype(jfty)) {
            int fsz = jl_field_size(jt, idx) - 1;
            unsigned ptindex = convert_struct_offset(ctx, T, byte_offset + fsz);
            AllocaInst *lv = NULL;
            if (fsz > 0) {
                unsigned st_idx = convert_struct_offset(ctx, T, byte_offset);
                IntegerType *ET = cast<IntegerType>(T->getStructElementType(st_idx));
                unsigned align = (ET->getBitWidth() + 7) / 8;
                lv = emit_static_alloca(ctx, ET);
                lv->setOperand(0, ConstantInt::get(T_int32, (fsz + align - 1) / align));
                // emit all of the align-sized words
                unsigned i = 0;
                for (; i < fsz / align; i++) {
                    unsigned fld = st_idx + i;
                    Value *fldv = ctx.builder.CreateExtractValue(obj, makeArrayRef(fld));
                    Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                    ctx.builder.CreateStore(fldv, fldp);
                }
                // emit remaining bytes up to tindex
                if (i < ptindex - st_idx) {
                    Value *staddr = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                    staddr = ctx.builder.CreateBitCast(staddr, T_pint8);
                    for (; i < ptindex - st_idx; i++) {
                        Value *fldv = ctx.builder.CreateExtractValue(obj, makeArrayRef(st_idx + i));
                        Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(T_int8, staddr, i);
                        ctx.builder.CreateStore(fldv, fldp);
                    }
                }
            }
            Value *tindex0 = ctx.builder.CreateExtractValue(obj, makeArrayRef(ptindex));
            Value *tindex = ctx.builder.CreateNUWAdd(ConstantInt::get(T_int8, 1), tindex0);
            return mark_julia_slot(lv, jfty, tindex, tbaa_stack);
        }
        else {
            unsigned st_idx;
            if (isa<ArrayType>(T))
                st_idx = idx;
            else if (isa<StructType>(T))
                st_idx = convert_struct_offset(ctx, T, byte_offset);
            else
                llvm_unreachable("encountered incompatible type for a struct");
            fldv = ctx.builder.CreateExtractValue(obj, makeArrayRef(st_idx));
        }
        if (maybe_null) {
            Value *first_ptr = jl_field_isptr(jt, idx) ? fldv : extract_first_ptr(ctx, fldv);
            if (first_ptr)
                null_pointer_check(ctx, first_ptr);
        }
        return mark_julia_type(ctx, fldv, jl_field_isptr(jt, idx), jfty);
    }
}

// emit length of vararg tuple
static Value *emit_n_varargs(jl_codectx_t &ctx)
{
    Value *valen = NULL;
    if (ctx.nvargs != -1) {
        valen = ConstantInt::get(T_int32, ctx.nvargs);
    } else {
        assert(ctx.argCount);
        int nreq = ctx.nReqArgs;
        valen = ctx.builder.CreateSub((Value*)ctx.argCount,
                                        ConstantInt::get(T_int32, nreq));
    }
#ifdef _P64
    return ctx.builder.CreateSExt(valen, T_int64);
#else
    return valen;
#endif
}

static bool arraytype_constshape(jl_value_t *ty)
{
    return (jl_is_array_type(ty) && jl_is_concrete_type(ty) &&
            jl_is_long(jl_tparam1(ty)) && jl_unbox_long(jl_tparam1(ty)) != 1);
}

static Value *emit_arraysize(jl_codectx_t &ctx, const jl_cgval_t &tinfo, Value *dim)
{
    Value *t = boxed(ctx, tinfo);
    int o = offsetof(jl_array_t, nrows) / sizeof(void*) - 1;
    MDNode *tbaa = arraytype_constshape(tinfo.typ) ? tbaa_const : tbaa_arraysize;
    return emit_nthptr_recast(ctx,
            t,
            ctx.builder.CreateAdd(dim, ConstantInt::get(dim->getType(), o)),
            tbaa, T_psize);
}

static Value *emit_arraysize(jl_codectx_t &ctx, const jl_cgval_t &tinfo, int dim)
{
    return emit_arraysize(ctx, tinfo, ConstantInt::get(T_int32, dim));
}

static Value *emit_vectormaxsize(jl_codectx_t &ctx, const jl_cgval_t &ary)
{
    return emit_arraysize(ctx, ary, 2); // maxsize aliases ncols in memory layout for vector
}

static Value *emit_arraylen_prim(jl_codectx_t &ctx, const jl_cgval_t &tinfo)
{
    Value *t = boxed(ctx, tinfo);
    jl_value_t *ty = tinfo.typ;
#ifdef STORE_ARRAY_LEN
    Value *addr = ctx.builder.CreateStructGEP(jl_array_llvmt,
                                          emit_bitcast(ctx, decay_derived(t), jl_parray_llvmt),
                                          1); //index (not offset) of length field in jl_parray_llvmt

    MDNode *tbaa = arraytype_constshape(ty) ? tbaa_const : tbaa_arraylen;
    return tbaa_decorate(tbaa, ctx.builder.CreateLoad(addr, false));
#else
    jl_value_t *p1 = jl_tparam1(ty); // FIXME: check that ty is an array type
    if (jl_is_long(p1)) {
        size_t nd = jl_unbox_long(p1);
        Value *l = ConstantInt::get(T_size, 1);
        for(size_t i=0; i < nd; i++) {
            l = ctx.builder.CreateMul(l, emit_arraysize(ctx, t, (int)(i + 1)));
        }
        return l;
    }
    else {
        std::vector<Type *> fargt(0);
        fargt.push_back(T_pjlvalue);
        FunctionType *ft = FunctionType::get(T_size, fargt, false);
        Value *alen = jl_Module->getOrInsertFunction("jl_array_len_", ft); // TODO: move to codegen init block
        return ctx.builder.CreateCall(prepare_call(alen), t);
    }
#endif
}

static Value *emit_arraylen(jl_codectx_t &ctx, const jl_cgval_t &tinfo)
{
    return emit_arraylen_prim(ctx, tinfo);
}

static Value *emit_arrayptr_internal(jl_codectx_t &ctx, const jl_cgval_t &tinfo, Value *t, unsigned AS, bool isboxed)
{
    Value *addr =
        ctx.builder.CreateStructGEP(jl_array_llvmt,
            emit_bitcast(ctx, t, jl_parray_llvmt),
            0); // index (not offset) of data field in jl_parray_llvmt
    MDNode *tbaa = arraytype_constshape(tinfo.typ) ? tbaa_const : tbaa_arrayptr;
    PointerType *PT = cast<PointerType>(addr->getType());
    PointerType *PPT = cast<PointerType>(PT->getElementType());
    if (isboxed) {
        addr = ctx.builder.CreateBitCast(addr,
            PointerType::get(PointerType::get(T_prjlvalue, AS),
            PT->getAddressSpace()));
    } else if (AS != PPT->getAddressSpace()) {
        addr = ctx.builder.CreateBitCast(addr,
            PointerType::get(
                PointerType::get(PPT->getElementType(), AS),
                PT->getAddressSpace()));
    }
    auto LI = ctx.builder.CreateLoad(addr);
    LI->setMetadata(LLVMContext::MD_nonnull, MDNode::get(jl_LLVMContext, None));
    tbaa_decorate(tbaa, LI);
    return LI;
}

static Value *emit_arrayptr(jl_codectx_t &ctx, const jl_cgval_t &tinfo, bool isboxed = false)
{
    Value *t = boxed(ctx, tinfo);
    return emit_arrayptr_internal(ctx, tinfo, decay_derived(t), AddressSpace::Loaded, isboxed);
}

static Value *emit_unsafe_arrayptr(jl_codectx_t &ctx, const jl_cgval_t &tinfo, bool isboxed = false)
{
    Value *t = boxed(ctx, tinfo);
    t = emit_pointer_from_objref(ctx, decay_derived(t));
    return emit_arrayptr_internal(ctx, tinfo, t, 0, isboxed);
}

static Value *emit_arrayptr(jl_codectx_t &ctx, const jl_cgval_t &tinfo, jl_value_t *ex, bool isboxed = false)
{
    return emit_arrayptr(ctx, tinfo, isboxed);
}

static Value *emit_arraysize(jl_codectx_t &ctx, const jl_cgval_t &tinfo, jl_value_t *ex, int dim)
{
    return emit_arraysize(ctx, tinfo, dim);
}

static Value *emit_arrayflags(jl_codectx_t &ctx, const jl_cgval_t &tinfo)
{
    Value *t = boxed(ctx, tinfo);
#ifdef STORE_ARRAY_LEN
    int arrayflag_field = 2;
#else
    int arrayflag_field = 1;
#endif
    Value *addr = ctx.builder.CreateStructGEP(
            jl_array_llvmt,
            emit_bitcast(ctx, decay_derived(t), jl_parray_llvmt),
            arrayflag_field);
    return tbaa_decorate(tbaa_arrayflags, ctx.builder.CreateLoad(addr));
}

static Value *emit_arrayndims(jl_codectx_t &ctx, const jl_cgval_t &ary)
{
    Value *flags = emit_arrayflags(ctx, ary);
    cast<LoadInst>(flags)->setMetadata(LLVMContext::MD_invariant_load, MDNode::get(jl_LLVMContext, None));
    flags = ctx.builder.CreateLShr(flags, 2);
    flags = ctx.builder.CreateAnd(flags, 0x1FF); // (1<<9) - 1
    return flags;
}

static Value *emit_arrayelsize(jl_codectx_t &ctx, const jl_cgval_t &tinfo)
{
    Value *t = boxed(ctx, tinfo);
#ifdef STORE_ARRAY_LEN
    int elsize_field = 3;
#else
    int elsize_field = 2;
#endif
    Value *addr = ctx.builder.CreateStructGEP(jl_array_llvmt,
                                          emit_bitcast(ctx, decay_derived(t), jl_parray_llvmt),
                                          elsize_field);
    return tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(addr));
}

static Value *emit_arrayoffset(jl_codectx_t &ctx, const jl_cgval_t &tinfo, int nd)
{
    if (nd != -1 && nd != 1) // only Vector can have an offset
        return ConstantInt::get(T_int32, 0);
    Value *t = boxed(ctx, tinfo);
#ifdef STORE_ARRAY_LEN
    int offset_field = 4;
#else
    int offset_field = 3;
#endif

    Value *addr = ctx.builder.CreateStructGEP(jl_array_llvmt,
                                              emit_bitcast(ctx, decay_derived(t), jl_parray_llvmt),
                                              offset_field);
    return tbaa_decorate(tbaa_arrayoffset, ctx.builder.CreateLoad(addr));
}

// Returns the size of the array represented by `tinfo` for the given dimension `dim` if
// `dim` is a valid dimension, otherwise returns constant one.
static Value *emit_arraysize_for_unsafe_dim(jl_codectx_t &ctx,
        const jl_cgval_t &tinfo, jl_value_t *ex, size_t dim, size_t nd)
{
    return dim > nd ? ConstantInt::get(T_size, 1) : emit_arraysize(ctx, tinfo, ex, dim);
}

// `nd == -1` means the dimension is unknown.
static Value *emit_array_nd_index(
        jl_codectx_t &ctx, const jl_cgval_t &ainfo, jl_value_t *ex, ssize_t nd,
        const jl_cgval_t *argv, size_t nidxs, jl_value_t *inbounds)
{
    Value *a = boxed(ctx, ainfo);
    Value *i = ConstantInt::get(T_size, 0);
    Value *stride = ConstantInt::get(T_size, 1);
#if CHECK_BOUNDS==1
    bool bc = bounds_check_enabled(ctx, inbounds);
    BasicBlock *failBB = NULL, *endBB = NULL;
    if (bc) {
        failBB = BasicBlock::Create(jl_LLVMContext, "oob");
        endBB = BasicBlock::Create(jl_LLVMContext, "idxend");
    }
#endif
    Value **idxs = (Value**)alloca(sizeof(Value*) * nidxs);
    for (size_t k = 0; k < nidxs; k++) {
        idxs[k] = emit_unbox(ctx, T_size, argv[k], (jl_value_t*)jl_long_type); // type asserted by caller
    }
    Value *ii;
    for (size_t k = 0; k < nidxs; k++) {
        ii = ctx.builder.CreateSub(idxs[k], ConstantInt::get(T_size, 1));
        i = ctx.builder.CreateAdd(i, ctx.builder.CreateMul(ii, stride));
        if (k < nidxs - 1) {
            assert(nd >= 0);
            Value *d = emit_arraysize_for_unsafe_dim(ctx, ainfo, ex, k + 1, nd);
#if CHECK_BOUNDS==1
            if (bc) {
                BasicBlock *okBB = BasicBlock::Create(jl_LLVMContext, "ib");
                // if !(i < d) goto error
                ctx.builder.CreateCondBr(ctx.builder.CreateICmpULT(ii, d), okBB, failBB);
                ctx.f->getBasicBlockList().push_back(okBB);
                ctx.builder.SetInsertPoint(okBB);
            }
#endif
            stride = ctx.builder.CreateMul(stride, d);
        }
    }
#if CHECK_BOUNDS==1
    if (bc) {
        // We have already emitted a bounds check for each index except for
        // the last one which we therefore have to do here.
        if (nidxs == 1) {
            // Linear indexing: Check against the entire linear span of the array
            Value *alen = emit_arraylen(ctx, ainfo);
            ctx.builder.CreateCondBr(ctx.builder.CreateICmpULT(i, alen), endBB, failBB);
        } else if (nidxs >= (size_t)nd){
            // No dimensions were omitted; just check the last remaining index
            assert(nd >= 0);
            Value *last_index = ii;
            Value *last_dimension = emit_arraysize_for_unsafe_dim(ctx, ainfo, ex, nidxs, nd);
            ctx.builder.CreateCondBr(ctx.builder.CreateICmpULT(last_index, last_dimension), endBB, failBB);
        } else {
            // There were fewer indices than dimensions; check the last remaining index
            BasicBlock *checktrailingdimsBB = BasicBlock::Create(jl_LLVMContext, "dimsib");
            assert(nd >= 0);
            Value *last_index = ii;
            Value *last_dimension = emit_arraysize_for_unsafe_dim(ctx, ainfo, ex, nidxs, nd);
            ctx.builder.CreateCondBr(ctx.builder.CreateICmpULT(last_index, last_dimension), checktrailingdimsBB, failBB);
            ctx.f->getBasicBlockList().push_back(checktrailingdimsBB);
            ctx.builder.SetInsertPoint(checktrailingdimsBB);
            // And then also make sure that all dimensions that weren't explicitly
            // indexed into have size 1
            for (size_t k = nidxs+1; k < (size_t)nd; k++) {
                BasicBlock *dimsokBB = BasicBlock::Create(jl_LLVMContext, "dimsok");
                Value *dim = emit_arraysize_for_unsafe_dim(ctx, ainfo, ex, k, nd);
                ctx.builder.CreateCondBr(ctx.builder.CreateICmpEQ(dim, ConstantInt::get(T_size, 1)), dimsokBB, failBB);
                ctx.f->getBasicBlockList().push_back(dimsokBB);
                ctx.builder.SetInsertPoint(dimsokBB);
            }
            Value *dim = emit_arraysize_for_unsafe_dim(ctx, ainfo, ex, nd, nd);
            ctx.builder.CreateCondBr(ctx.builder.CreateICmpEQ(dim, ConstantInt::get(T_size, 1)), endBB, failBB);
        }

        ctx.f->getBasicBlockList().push_back(failBB);
        ctx.builder.SetInsertPoint(failBB);
        // CreateAlloca is OK here since we are on an error branch
        Value *tmp = ctx.builder.CreateAlloca(T_size, ConstantInt::get(T_size, nidxs));
        for (size_t k = 0; k < nidxs; k++) {
            ctx.builder.CreateStore(idxs[k], ctx.builder.CreateInBoundsGEP(T_size, tmp, ConstantInt::get(T_size, k)));
        }
        ctx.builder.CreateCall(prepare_call(jlboundserrorv_func),
            { mark_callee_rooted(a), tmp, ConstantInt::get(T_size, nidxs) });
        ctx.builder.CreateUnreachable();

        ctx.f->getBasicBlockList().push_back(endBB);
        ctx.builder.SetInsertPoint(endBB);
    }
#endif

    return i;
}

// --- boxing ---

static Value *emit_allocobj(jl_codectx_t &ctx, size_t static_size, Value *jt);

static void init_bits_value(jl_codectx_t &ctx, Value *newv, Value *v, MDNode *tbaa,
                            unsigned alignment = sizeof(void*)) // min alignment in julia's gc is pointer-aligned
{
    // newv should already be tagged
    tbaa_decorate(tbaa, ctx.builder.CreateAlignedStore(v, emit_bitcast(ctx, newv,
        PointerType::get(v->getType(), 0)), alignment));
}

static void init_bits_cgval(jl_codectx_t &ctx, Value *newv, const jl_cgval_t& v, MDNode *tbaa)
{
    // newv should already be tagged
    if (v.ispointer()) {
        emit_memcpy(ctx, newv, tbaa, v, jl_datatype_size(v.typ), sizeof(void*));
    }
    else {
        init_bits_value(ctx, newv, v.V, tbaa);
    }
}

static jl_value_t *static_constant_instance(Constant *constant, jl_value_t *jt)
{
    assert(constant != NULL && jl_is_concrete_type(jt));
    jl_datatype_t *jst = (jl_datatype_t*)jt;

    if (isa<UndefValue>(constant))
        return NULL;

    if (ConstantInt *cint = dyn_cast<ConstantInt>(constant)) {
        if (jst == jl_bool_type)
            return cint->isZero() ? jl_false : jl_true;
        return jl_new_bits(jt,
            const_cast<uint64_t *>(cint->getValue().getRawData()));
    }

    if (ConstantFP *cfp = dyn_cast<ConstantFP>(constant)) {
        return jl_new_bits(jt,
            const_cast<uint64_t *>(cfp->getValueAPF().bitcastToAPInt().getRawData()));
    }

    if (isa<ConstantPointerNull>(constant)) {
        uint64_t val = 0;
        return jl_new_bits(jt, &val);
    }

    // issue #8464
    if (ConstantExpr *ce = dyn_cast<ConstantExpr>(constant)) {
        unsigned OpCode = ce->getOpcode();
        if (OpCode == Instruction::BitCast || OpCode == Instruction::PtrToInt || OpCode == Instruction::IntToPtr) {
            return static_constant_instance(ce->getOperand(0), jt);
        }
        return NULL;
    }

    if (isa<GlobalValue>(constant))
        return NULL;

    size_t nargs;
    if (const auto *CC = dyn_cast<ConstantAggregate>(constant))
        nargs = CC->getNumOperands();
    else if (const auto *CAZ = dyn_cast<ConstantAggregateZero>(constant))
        nargs = CAZ->getNumElements();
    else if (const auto *CDS = dyn_cast<ConstantDataSequential>(constant))
        nargs = CDS->getNumElements();
    else
        return NULL;
    assert(nargs > 0 && jst->instance == NULL);
    if (nargs != jl_datatype_nfields(jst))
        return NULL;

    jl_value_t **flds;
    JL_GC_PUSHARGS(flds, nargs);
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *ft = jl_field_type(jst, i);
        if (jl_field_isptr(jst, i) || jl_is_uniontype(ft)) {
            JL_GC_POP();
            return NULL; // TODO: handle this?
        }
        unsigned llvm_idx = i;
        if (i > 0 && isa<StructType>(constant->getType()))
            llvm_idx = convert_struct_offset(constant->getType(), jl_field_offset(jst, i));
        Constant *fld = constant->getAggregateElement(llvm_idx);
        flds[i] = static_constant_instance(fld, ft);
        if (flds[i] == NULL) {
            JL_GC_POP();
            return NULL; // must have been unreachable
        }
    }
    jl_value_t *obj = jl_new_structv(jst, flds, nargs);
    JL_GC_POP();
    return obj;
}

static Value *call_with_attrs(jl_codectx_t &ctx, Function *func, Value *v)
{
    CallInst *Call = ctx.builder.CreateCall(prepare_call(func), v);
    Call->setAttributes(func->getAttributes());
    return Call;
}

static void jl_add_method_root(jl_codectx_t &ctx, jl_value_t *val);

static Value *as_value(jl_codectx_t &ctx, Type *to, const jl_cgval_t &v)
{
    assert(!v.isboxed);
    return emit_unbox(ctx, to, v, v.typ);
}

// some types have special boxing functions with small-value caches
static Value *_boxed_special(jl_codectx_t &ctx, const jl_cgval_t &vinfo, Type *t)
{
    jl_value_t *jt = vinfo.typ;
    if (jt == (jl_value_t*)jl_bool_type)
        return julia_bool(ctx, ctx.builder.CreateTrunc(as_value(ctx, t, vinfo), T_int1));
    if (t == T_int1)
        return julia_bool(ctx, as_value(ctx, t, vinfo));

    if (ctx.linfo && jl_is_method(ctx.linfo->def.method) && !vinfo.ispointer()) { // don't bother codegen pre-boxing for toplevel
        if (Constant *c = dyn_cast<Constant>(vinfo.V)) {
            jl_value_t *s = static_constant_instance(c, jt);
            if (s) {
                jl_add_method_root(ctx, s);
                return literal_pointer_val(ctx, s);
            }
        }
    }

    jl_datatype_t *jb = (jl_datatype_t*)jt;
    assert(jl_is_datatype(jb));
    Value *box = NULL;
    if (jb == jl_int8_type)
        box = call_with_attrs(ctx, box_int8_func, as_value(ctx, t, vinfo));
    else if (jb == jl_int16_type)
        box = call_with_attrs(ctx, box_int16_func, as_value(ctx, t, vinfo));
    else if (jb == jl_int32_type)
        box = call_with_attrs(ctx, box_int32_func, as_value(ctx, t, vinfo));
    else if (jb == jl_int64_type)
        box = call_with_attrs(ctx, box_int64_func, as_value(ctx, t, vinfo));
    else if (jb == jl_float32_type)
        box = ctx.builder.CreateCall(prepare_call(box_float32_func), as_value(ctx, t, vinfo));
    //if (jb == jl_float64_type)
    //  box = ctx.builder.CreateCall(box_float64_func, as_value(ctx, t, vinfo);
    // for Float64, fall through to generic case below, to inline alloc & init of Float64 box. cheap, I know.
    else if (jb == jl_uint8_type)
        box = call_with_attrs(ctx, box_uint8_func, as_value(ctx, t, vinfo));
    else if (jb == jl_uint16_type)
        box = call_with_attrs(ctx, box_uint16_func, as_value(ctx, t, vinfo));
    else if (jb == jl_uint32_type)
        box = call_with_attrs(ctx, box_uint32_func, as_value(ctx, t, vinfo));
    else if (jb == jl_uint64_type)
        box = call_with_attrs(ctx, box_uint64_func, as_value(ctx, t, vinfo));
    else if (jb == jl_char_type)
        box = call_with_attrs(ctx, box_char_func, as_value(ctx, t, vinfo));
    else if (jb == jl_ssavalue_type) {
        unsigned zero = 0;
        Value *v = as_value(ctx, t, vinfo);
        assert(v->getType() == jl_ssavalue_type->struct_decl);
        v = ctx.builder.CreateExtractValue(v, makeArrayRef(&zero, 1));
        box = call_with_attrs(ctx, box_ssavalue_func, v);
    }
    else if (!jb->abstract && jl_datatype_nbits(jb) == 0) {
        // singleton
        assert(jb->instance != NULL);
        return literal_pointer_val(ctx, jb->instance);
    }
    return box;
}

static Value *compute_box_tindex(jl_codectx_t &ctx, Value *datatype, jl_value_t *supertype, jl_value_t *ut)
{
    Value *tindex = ConstantInt::get(T_int8, 0);
    unsigned counter = 0;
    for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                if (jl_subtype((jl_value_t*)jt, supertype)) {
                    Value *cmp = ctx.builder.CreateICmpEQ(maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)jt)), datatype);
                    tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(T_int8, idx), tindex);
                }
            },
            ut,
            counter);
    return tindex;
}

// get the runtime tindex value, assuming val is already converted to type typ if it has a TIndex
static Value *compute_tindex_unboxed(jl_codectx_t &ctx, const jl_cgval_t &val, jl_value_t *typ)
{
    if (val.typ == jl_bottom_type)
        return UndefValue::get(T_int8);
    if (val.constant)
        return ConstantInt::get(T_int8, get_box_tindex((jl_datatype_t*)jl_typeof(val.constant), typ));

    if (val.TIndex)
        return ctx.builder.CreateAnd(val.TIndex, ConstantInt::get(T_int8, 0x7f));
    if (val.isboxed)
        return compute_box_tindex(ctx, emit_typeof_boxed(ctx, val), val.typ, typ);
    return compute_box_tindex(ctx, emit_typeof_boxed(ctx, val), val.typ, typ);
}

static void union_alloca_type(jl_uniontype_t *ut,
        bool &allunbox, size_t &nbytes, size_t &align, size_t &min_align)
{
    nbytes = 0;
    align = 0;
    min_align = MAX_ALIGN;
    // compute the size of the union alloca that could hold this type
    unsigned counter = 0;
    allunbox = for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                if (!jl_is_datatype_singleton(jt)) {
                    size_t nb1 = jl_datatype_size(jt);
                    size_t align1 = jl_datatype_align(jt);
                    if (nb1 > nbytes)
                        nbytes = nb1;
                    if (align1 > align)
                        align = align1;
                    if (align1 < min_align)
                        min_align = align1;
                }
            },
            (jl_value_t*)ut,
            counter);
}

static AllocaInst *try_emit_union_alloca(jl_codectx_t &ctx, jl_uniontype_t *ut, bool &allunbox, size_t &min_align, size_t &nbytes)
{
    size_t align;
    union_alloca_type(ut, allunbox, nbytes, align, min_align);
    if (nbytes > 0) {
        // at least some of the values can live on the stack
        // try to pick an Integer type size such that SROA will emit reasonable code
        Type *AT = ArrayType::get(IntegerType::get(jl_LLVMContext, 8 * min_align), (nbytes + min_align - 1) / min_align);
        AllocaInst *lv = emit_static_alloca(ctx, AT);
        if (align > 1)
            lv->setAlignment(Align(align));
        return lv;
    }
    return NULL;
}

/*
 * Box unboxed values in a union. Optionally, skip certain unboxed values,
 * returning `V_null` in one of the skipped cases. If `skip` is not empty,
 * skip[0] (corresponding to unknown boxed) must always be set. In that
 * case, the calling code must separately deal with the case where
 * `vinfo` is already an unknown boxed union (union tag 0x80).
 */
static Value *box_union(jl_codectx_t &ctx, const jl_cgval_t &vinfo, const SmallBitVector &skip)
{
    // given vinfo::Union{T, S}, emit IR of the form:
    //   ...
    //   switch <tindex>, label <box_union_isboxed> [ 1, label <box_union_1>
    //                                                2, label <box_union_2> ]
    // box_union_1:
    //   box1 = create_box(T)
    //   br post_box_union
    // box_union_2:
    //   box2 = create_box(S)
    //   br post_box_union
    // box_union_isboxed:
    //   br post_box_union
    // post_box_union:
    //   box = phi [ box1, box_union_1 ], [ box2, box_union_2 ], [ vinfo, box_union_isboxed ]
    //   ...
    Value *tindex = vinfo.TIndex;
    BasicBlock *defaultBB = BasicBlock::Create(jl_LLVMContext, "box_union_isboxed", ctx.f);
    SwitchInst *switchInst = ctx.builder.CreateSwitch(tindex, defaultBB);
    BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_box_union", ctx.f);
    ctx.builder.SetInsertPoint(postBB);
    PHINode *box_merge = ctx.builder.CreatePHI(T_prjlvalue, 2);
    unsigned counter = 0;
    for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                if (idx < skip.size() && skip[idx])
                    return;
                Type *t = julia_type_to_llvm((jl_value_t*)jt);
                BasicBlock *tempBB = BasicBlock::Create(jl_LLVMContext, "box_union", ctx.f);
                ctx.builder.SetInsertPoint(tempBB);
                switchInst->addCase(ConstantInt::get(T_int8, idx), tempBB);
                Value *box;
                if (type_is_ghost(t)) {
                    box = literal_pointer_val(ctx, jt->instance);
                }
                else {
                    jl_cgval_t vinfo_r = jl_cgval_t(vinfo, (jl_value_t*)jt, NULL);
                    box = _boxed_special(ctx, vinfo_r, t);
                    if (!box) {
                        box = emit_allocobj(ctx, jl_datatype_size(jt), literal_pointer_val(ctx, (jl_value_t*)jt));
                        init_bits_cgval(ctx, box, vinfo_r, jl_is_mutable(jt) ? tbaa_mutab : tbaa_immut);
                    }
                }
                box_merge->addIncoming(maybe_decay_untracked(box), tempBB);
                ctx.builder.CreateBr(postBB);
            },
            vinfo.typ,
            counter);
    ctx.builder.SetInsertPoint(defaultBB);
    if (skip.size() > 0) {
        assert(skip[0]);
        box_merge->addIncoming(maybe_decay_untracked(V_null), defaultBB);
        ctx.builder.CreateBr(postBB);
    }
    else if (!vinfo.Vboxed) {
        Function *trap_func = Intrinsic::getDeclaration(
                ctx.f->getParent(),
                Intrinsic::trap);
        ctx.builder.CreateCall(trap_func);
        ctx.builder.CreateUnreachable();
    }
    else {
        box_merge->addIncoming(vinfo.Vboxed, defaultBB);
        ctx.builder.CreateBr(postBB);
    }
    ctx.builder.SetInsertPoint(postBB);
    return box_merge;
}

// this is used to wrap values for generic contexts, where a
// dynamically-typed value is required (e.g. argument to unknown function).
// if it's already a pointer it's left alone.
static Value *boxed(jl_codectx_t &ctx, const jl_cgval_t &vinfo)
{
    jl_value_t *jt = vinfo.typ;
    if (jt == jl_bottom_type || jt == NULL)
        // We have an undef value on a (hopefully) dead branch
        return UndefValue::get(T_prjlvalue);
    if (vinfo.constant)
        return maybe_decay_untracked(literal_pointer_val(ctx, vinfo.constant));
    // This can happen in early bootstrap for `gc_preserve_begin` return value.
    if (jt == (jl_value_t*)jl_nothing_type)
        return maybe_decay_untracked(literal_pointer_val(ctx, jl_nothing));
    if (vinfo.isboxed) {
        assert(vinfo.V == vinfo.Vboxed);
        return vinfo.V;
    }

    Value *box;
    if (vinfo.TIndex) {
        SmallBitVector skip_none;
        box = box_union(ctx, vinfo, skip_none);
    }
    else {
        assert(vinfo.V && "Missing data for unboxed value.");
        assert(jl_is_concrete_immutable(jt) && "This type shouldn't have been unboxed.");
        Type *t = julia_type_to_llvm(jt);
        assert(!type_is_ghost(t)); // ghost values should have been handled by vinfo.constant above!
        box = _boxed_special(ctx, vinfo, t);
        if (!box) {
            box = emit_allocobj(ctx, jl_datatype_size(jt), literal_pointer_val(ctx, (jl_value_t*)jt));
            init_bits_cgval(ctx, box, vinfo, jl_is_mutable(jt) ? tbaa_mutab : tbaa_immut);
        }
        else {
            box = maybe_decay_untracked(box);
        }
    }
    return box;
}

// copy src to dest, if src is justbits. if skip is true, the value of dest is undefined
static void emit_unionmove(jl_codectx_t &ctx, Value *dest, MDNode *tbaa_dst, const jl_cgval_t &src, Value *skip, bool isVolatile=false)
{
    if (AllocaInst *ai = dyn_cast<AllocaInst>(dest))
        // TODO: make this a lifetime_end & dereferencable annotation?
        ctx.builder.CreateAlignedStore(UndefValue::get(ai->getAllocatedType()), ai, ai->getAlignment());
    if (jl_is_concrete_type(src.typ) || src.constant) {
        jl_value_t *typ = src.constant ? jl_typeof(src.constant) : src.typ;
        Type *store_ty = julia_type_to_llvm(typ);
        assert(skip || jl_is_pointerfree(typ));
        if (jl_is_pointerfree(typ)) {
            if (!src.ispointer() || src.constant) {
                emit_unbox(ctx, store_ty, src, typ, dest, tbaa_dst, isVolatile);
            }
            else {
                Value *src_ptr = data_pointer(ctx, src);
                unsigned nb = jl_datatype_size(typ);
                unsigned alignment = julia_alignment(typ);
                Value *nbytes = ConstantInt::get(T_size, nb);
                if (skip) {
                    // TODO: this Select is very bad for performance, but is necessary to work around LLVM bugs with the undef option that we want to use:
                    //   select copy dest -> dest to simulate an undef value / conditional copy
                    // src_ptr = ctx.builder.CreateSelect(skip, dest, src_ptr);
                    nbytes = ctx.builder.CreateSelect(skip,
                        ConstantInt::get(T_size, 0),
                        nbytes);
                }
                emit_memcpy(ctx, dest, tbaa_dst, src_ptr, src.tbaa, nbytes, alignment, isVolatile);
            }
        }
    }
    else if (src.TIndex) {
        Value *tindex = ctx.builder.CreateAnd(src.TIndex, ConstantInt::get(T_int8, 0x7f));
        if (skip)
            tindex = ctx.builder.CreateSelect(skip, ConstantInt::get(T_int8, 0), tindex);
        Value *src_ptr = data_pointer(ctx, src);
        src_ptr = src_ptr ? maybe_bitcast(ctx, src_ptr, T_pint8) : src_ptr;
        dest = maybe_bitcast(ctx, dest, T_pint8);
        BasicBlock *defaultBB = BasicBlock::Create(jl_LLVMContext, "union_move_skip", ctx.f);
        SwitchInst *switchInst = ctx.builder.CreateSwitch(tindex, defaultBB);
        BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_union_move", ctx.f);
        unsigned counter = 0;
        bool allunboxed = for_each_uniontype_small(
                [&](unsigned idx, jl_datatype_t *jt) {
                    unsigned nb = jl_datatype_size(jt);
                    unsigned alignment = julia_alignment((jl_value_t*)jt);
                    BasicBlock *tempBB = BasicBlock::Create(jl_LLVMContext, "union_move", ctx.f);
                    ctx.builder.SetInsertPoint(tempBB);
                    switchInst->addCase(ConstantInt::get(T_int8, idx), tempBB);
                    if (nb > 0) {
                        if (!src_ptr) {
                            Function *trap_func =
                                Intrinsic::getDeclaration(ctx.f->getParent(), Intrinsic::trap);
                            ctx.builder.CreateCall(trap_func);
                            ctx.builder.CreateUnreachable();
                            return;
                        } else {
                            emit_memcpy(ctx, dest, tbaa_dst, src_ptr,
                                        src.tbaa, nb, alignment, isVolatile);
                        }
                    }
                    ctx.builder.CreateBr(postBB);
                },
                src.typ,
                counter);
        ctx.builder.SetInsertPoint(defaultBB);
        if (!skip && allunboxed && (src.V == NULL || isa<AllocaInst>(src.V))) {
            Function *trap_func = Intrinsic::getDeclaration(
                    ctx.f->getParent(),
                    Intrinsic::trap);
            ctx.builder.CreateCall(trap_func);
            ctx.builder.CreateUnreachable();
        }
        else {
            ctx.builder.CreateBr(postBB);
        }
        ctx.builder.SetInsertPoint(postBB);
    }
    else {
        assert(src.isboxed && "expected boxed value for sizeof/alignment computation");
        Value *datatype = emit_typeof_boxed(ctx, src);
        Value *copy_bytes = emit_datatype_size(ctx, datatype);
        if (skip) {
            copy_bytes = ctx.builder.CreateSelect(skip, ConstantInt::get(copy_bytes->getType(), 0), copy_bytes);
        }
        emit_memcpy(ctx, dest, tbaa_dst, src, copy_bytes, /*TODO: min-align*/1, isVolatile);
    }
}


static void emit_cpointercheck(jl_codectx_t &ctx, const jl_cgval_t &x, const std::string &msg)
{
    Value *t = emit_typeof_boxed(ctx, x);
    emit_typecheck(ctx, mark_julia_type(ctx, t, true, jl_any_type), (jl_value_t*)jl_datatype_type, msg);

    Value *istype =
        ctx.builder.CreateICmpEQ(mark_callee_rooted(emit_datatype_name(ctx, t)),
                             mark_callee_rooted(literal_pointer_val(ctx, (jl_value_t*)jl_pointer_typename)));
    BasicBlock *failBB = BasicBlock::Create(jl_LLVMContext,"fail",ctx.f);
    BasicBlock *passBB = BasicBlock::Create(jl_LLVMContext,"pass");
    ctx.builder.CreateCondBr(istype, passBB, failBB);
    ctx.builder.SetInsertPoint(failBB);

    emit_type_error(ctx, x, literal_pointer_val(ctx, (jl_value_t*)jl_pointer_type), msg);
    ctx.builder.CreateUnreachable();

    ctx.f->getBasicBlockList().push_back(passBB);
    ctx.builder.SetInsertPoint(passBB);
}

// allocation for known size object
static Value *emit_allocobj(jl_codectx_t &ctx, size_t static_size, Value *jt)
{
    Value *ptls_ptr = emit_bitcast(ctx, ctx.ptlsStates, T_pint8);
    auto call = ctx.builder.CreateCall(prepare_call(jl_alloc_obj_func),
                                       {ptls_ptr, ConstantInt::get(T_size, static_size),
                                               maybe_decay_untracked(jt)});
    call->setAttributes(jl_alloc_obj_func->getAttributes());
    return call;
}

// allocation for unknown object from an untracked pointer
static Value *emit_new_bits(jl_codectx_t &ctx, Value *jt, Value *pval)
{
    pval = ctx.builder.CreateBitCast(pval, T_pint8);
    auto call = ctx.builder.CreateCall(prepare_call(jl_newbits_func), { jt, pval });
    call->setAttributes(jl_newbits_func->getAttributes());
    return call;
}

// if ptr is NULL this emits a write barrier _back_
static void emit_write_barrier(jl_codectx_t &ctx, Value *parent, Value *ptr)
{
    emit_write_barrier(ctx, parent, makeArrayRef(ptr));
}

static void emit_write_barrier(jl_codectx_t &ctx, Value *parent, ArrayRef<Value*> ptrs)
{
    SmallVector<Value*, 8> decay_ptrs;
    decay_ptrs.push_back(maybe_decay_untracked(emit_bitcast(ctx, parent, T_prjlvalue)));
    for (auto ptr : ptrs) {
        decay_ptrs.push_back(maybe_decay_untracked(emit_bitcast(ctx, ptr, T_prjlvalue)));
    }
    ctx.builder.CreateCall(prepare_call(jl_write_barrier_func), decay_ptrs);
}

static void emit_write_multibarrier(jl_codectx_t &ctx, Value *parent, Value *agg)
{
    auto ptrs = ExtractTrackedValues(agg, agg->getType(), false, ctx.builder);
    emit_write_barrier(ctx, parent, ptrs);
}


static void emit_setfield(jl_codectx_t &ctx,
        jl_datatype_t *sty, const jl_cgval_t &strct, size_t idx0,
        const jl_cgval_t &rhs, bool checked, bool wb)
{
    if (sty->mutabl || !checked) {
        assert(strct.ispointer());
        size_t byte_offset = jl_field_offset(sty, idx0);
        Value *addr = data_pointer(ctx, strct);
        if (byte_offset > 0) {
            addr = ctx.builder.CreateInBoundsGEP(
                    T_int8,
                    emit_bitcast(ctx, maybe_decay_tracked(addr), T_pint8),
                    ConstantInt::get(T_size, byte_offset)); // TODO: use emit_struct_gep
        }
        jl_value_t *jfty = jl_svecref(sty->types, idx0);
        if (jl_field_isptr(sty, idx0)) {
            Value *r = maybe_decay_untracked(boxed(ctx, rhs)); // don't need a temporary gcroot since it'll be rooted by strct
            tbaa_decorate(strct.tbaa, ctx.builder.CreateStore(r,
                emit_bitcast(ctx, addr, T_pprjlvalue)));
            if (wb && strct.isboxed)
                emit_write_barrier(ctx, boxed(ctx, strct), r);
        }
        else if (jl_is_uniontype(jfty)) {
            int fsz = jl_field_size(sty, idx0) - 1;
            // compute tindex from rhs
            jl_cgval_t rhs_union = convert_julia_type(ctx, rhs, jfty);
            if (rhs_union.typ == jl_bottom_type)
                return;
            Value *tindex = compute_tindex_unboxed(ctx, rhs_union, jfty);
            tindex = ctx.builder.CreateNUWSub(tindex, ConstantInt::get(T_int8, 1));
            Value *ptindex = ctx.builder.CreateInBoundsGEP(T_int8, emit_bitcast(ctx, maybe_decay_tracked(addr), T_pint8), ConstantInt::get(T_size, fsz));
            tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateStore(tindex, ptindex));
            // copy data
            if (!rhs.isghost) {
                emit_unionmove(ctx, addr, strct.tbaa, rhs, nullptr);
            }
        }
        else {
            unsigned align = jl_field_align(sty, idx0);
            typed_store(ctx, addr, NULL, rhs, jfty,
                strct.tbaa, nullptr, maybe_bitcast(ctx,
                data_pointer(ctx, strct), T_pjlvalue), align);
        }
    }
    else {
        std::string msg = "setfield! immutable struct of type "
            + std::string(jl_symbol_name(sty->name->name))
            + " cannot be changed";
        emit_error(ctx, msg);
    }
}

static jl_cgval_t emit_new_struct(jl_codectx_t &ctx, jl_value_t *ty, size_t nargs, const jl_cgval_t *argv)
{
    assert(jl_is_datatype(ty));
    assert(jl_is_concrete_type(ty));
    jl_datatype_t *sty = (jl_datatype_t*)ty;
    size_t nf = jl_datatype_nfields(sty);
    if (nf > 0 || sty->mutabl) {
        if (deserves_stack(ty)) {
            Type *lt = julia_type_to_llvm(ty);
            unsigned na = nargs < nf ? nargs : nf;

            // whether we should perform the initialization with the struct as a IR value
            // or instead initialize the stack buffer with stores
            auto tracked = CountTrackedPointers(lt);
            bool init_as_value = false;
            if (lt->isVectorTy() || jl_is_vecelement_type(ty)) { // maybe also check the size ?
                init_as_value = true;
            }
            else if (tracked.count) {
                init_as_value = true;
            }

            Value *strct;
            if (type_is_ghost(lt)) {
                strct = NULL;
            }
            else if (init_as_value) {
                if (tracked.count)
                    strct = Constant::getNullValue(lt);
                else
                    strct = UndefValue::get(lt);
            }
            else {
                strct = emit_static_alloca(ctx, lt);
                if (tracked.count)
                    undef_derived_strct(ctx.builder, strct, sty, tbaa_stack);
            }

            for (unsigned i = 0; i < na; i++) {
                jl_value_t *jtype = jl_svecref(sty->types, i);
                jl_cgval_t fval_info = argv[i];
                emit_typecheck(ctx, fval_info, jtype, "new");
                fval_info = update_julia_type(ctx, fval_info, jtype);
                if (type_is_ghost(lt))
                    continue;
                Type *fty = julia_type_to_llvm(jtype);
                if (type_is_ghost(fty))
                    continue;
                Value *dest = NULL;
                unsigned offs = jl_field_offset(sty, i);
                unsigned llvm_idx = (i > 0 && isa<StructType>(lt)) ? convert_struct_offset(ctx, lt, offs) : i;
                if (!init_as_value) {
                    // avoid unboxing the argument explicitly
                    // and use memcpy instead
                    dest = ctx.builder.CreateConstInBoundsGEP2_32(lt, strct, 0, llvm_idx);
                }
                Value *fval = NULL;
                if (jl_field_isptr(sty, i)) {
                    fval = boxed(ctx, fval_info);
                    if (!init_as_value)
                        tbaa_decorate(tbaa_stack, ctx.builder.CreateAlignedStore(fval, dest, jl_field_align(sty, i)));
                }
                else if (jl_is_uniontype(jtype)) {
                    // compute tindex from rhs
                    jl_cgval_t rhs_union = convert_julia_type(ctx, fval_info, jtype);
                    if (rhs_union.typ == jl_bottom_type)
                        return jl_cgval_t();
                    Value *tindex = compute_tindex_unboxed(ctx, rhs_union, jtype);
                    tindex = ctx.builder.CreateNUWSub(tindex, ConstantInt::get(T_int8, 1));
                    size_t fsz = 0, al = 0;
                    bool isptr = !jl_islayout_inline(jtype, &fsz, &al);
                    assert(!isptr && fsz == jl_field_size(sty, i) - 1); (void)isptr;
                    if (init_as_value) {
                        // If you wanted to implement init_as_value,
                        // would need to emit the union-move into temporary memory,
                        // then load it and combine with the tindex.
                        // But more efficient to just store it directly.
                        unsigned ptindex = convert_struct_offset(ctx, lt, offs + fsz);
                        if (fsz > 0 && !fval_info.isghost) {
                            Type *ET = IntegerType::get(jl_LLVMContext, 8 * al);
                            assert(lt->getStructElementType(llvm_idx) == ET);
                            AllocaInst *lv = emit_static_alloca(ctx, ET);
                            lv->setOperand(0, ConstantInt::get(T_int32, (fsz + al - 1) / al));
                            emit_unionmove(ctx, lv, tbaa_stack, fval_info, nullptr);
                            // emit all of the align-sized words
                            unsigned i = 0;
                            for (; i < fsz / al; i++) {
                                Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                                Value *fldv = tbaa_decorate(tbaa_stack, ctx.builder.CreateLoad(ET, fldp));
                                strct = ctx.builder.CreateInsertValue(strct, fldv, makeArrayRef(llvm_idx + i));
                            }
                            // emit remaining bytes up to tindex
                            if (i < ptindex - llvm_idx) {
                                Value *staddr = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                                staddr = ctx.builder.CreateBitCast(staddr, T_pint8);
                                for (; i < ptindex - llvm_idx; i++) {
                                    Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(T_int8, staddr, i);
                                    Value *fldv = tbaa_decorate(tbaa_stack, ctx.builder.CreateLoad(T_int8, fldp));
                                    strct = ctx.builder.CreateInsertValue(strct, fldv, makeArrayRef(llvm_idx + i));
                                }
                            }
                        }
                        llvm_idx = ptindex;
                        fval = tindex;
                    }
                    else {
                        Value *ptindex = emit_struct_gep(ctx, lt, strct, offs + fsz);
                        tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateStore(tindex, ptindex));
                        if (!rhs_union.isghost)
                            emit_unionmove(ctx, dest, tbaa_stack, fval_info, nullptr);
                    }
                }
                else {
                    fval = emit_unbox(ctx, fty, fval_info, jtype, dest, tbaa_stack);
                }
                if (init_as_value) {
                    assert(fval);
                    if (jl_is_vecelement_type(ty))
                        strct = fval;  // VecElement type comes unwrapped in LLVM.
                    else if (lt->isVectorTy())
                        strct = ctx.builder.CreateInsertElement(strct, fval, ConstantInt::get(T_int32, llvm_idx));
                    else if (lt->isAggregateType())
                        strct = ctx.builder.CreateInsertValue(strct, fval, makeArrayRef(llvm_idx));
                    else
                        assert(false);
                }
            }
            for (size_t i = nargs; i < nf; i++) {
                if (!jl_field_isptr(sty, i) && jl_is_uniontype(jl_field_type(sty, i))) {
                    unsigned offs = jl_field_offset(sty, i);
                    int fsz = jl_field_size(sty, i) - 1;
                    unsigned llvm_idx = convert_struct_offset(ctx, cast<StructType>(lt), offs + fsz);
                    if (init_as_value)
                        strct = ctx.builder.CreateInsertValue(strct, ConstantInt::get(T_int8, 0), makeArrayRef(llvm_idx));
                    else
                        tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateStore(
                                ConstantInt::get(T_int8, 0),
                                ctx.builder.CreateConstInBoundsGEP2_32(lt, strct, 0, llvm_idx)));
                }
            }
            if (type_is_ghost(lt))
                return mark_julia_const(sty->instance);
            else if (init_as_value)
                return mark_julia_type(ctx, strct, false, ty);
            else
                return mark_julia_slot(strct, ty, NULL, tbaa_stack);
        }
        Value *strct = emit_allocobj(ctx, jl_datatype_size(sty),
                                     literal_pointer_val(ctx, (jl_value_t*)ty));
        jl_cgval_t strctinfo = mark_julia_type(ctx, strct, true, ty);
        strct = decay_derived(strct);
        undef_derived_strct(ctx.builder, strct, sty, strctinfo.tbaa);
        for (size_t i = nargs; i < nf; i++) {
            if (!jl_field_isptr(sty, i) && jl_is_uniontype(jl_field_type(sty, i))) {
                tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateStore(
                        ConstantInt::get(T_int8, 0),
                        ctx.builder.CreateInBoundsGEP(emit_bitcast(ctx, strct, T_pint8),
                                ConstantInt::get(T_size, jl_field_offset(sty, i) + jl_field_size(sty, i) - 1))));
            }
        }
        // TODO: verify that nargs <= nf (currently handled by front-end)
        for (size_t i = 0; i < nargs; i++) {
            const jl_cgval_t &rhs = argv[i];
            bool need_wb; // set to true if the store might cause the allocation of a box newer than the struct
            if (jl_field_isptr(sty, i))
                need_wb = !rhs.isboxed;
            else
                need_wb = false;
            emit_typecheck(ctx, rhs, jl_svecref(sty->types, i), "new");
            emit_setfield(ctx, sty, strctinfo, i, rhs, false, need_wb);
        }
        return strctinfo;
    }
    else {
        // 0 fields, ghost or bitstype
        if (jl_datatype_nbits(sty) == 0)
            return ghostValue(sty);
        bool isboxed;
        Type *lt = julia_type_to_llvm(ty, &isboxed);
        assert(!isboxed);
        return mark_julia_type(ctx, UndefValue::get(lt), false, ty);
    }
}

static void emit_signal_fence(jl_codectx_t &ctx)
{
#if defined(_CPU_ARM_) || defined(_CPU_AARCH64_)
    // LLVM generates very inefficient code (and might include function call)
    // for signal fence. Fallback to the poor man signal fence with
    // inline asm instead.
    // https://llvm.org/bugs/show_bug.cgi?id=27545
    ctx.builder.CreateCall(InlineAsm::get(FunctionType::get(T_void, false), "",
                                      "~{memory}", true));
#else
    ctx.builder.CreateFence(AtomicOrdering::SequentiallyConsistent, SyncScope::SingleThread);
#endif
}

static Value *emit_defer_signal(jl_codectx_t &ctx)
{
    Value *ptls = emit_bitcast(ctx, ctx.ptlsStates,
                                        PointerType::get(T_sigatomic, 0));
    Constant *offset = ConstantInt::getSigned(T_int32,
        offsetof(jl_tls_states_t, defer_signal) / sizeof(sig_atomic_t));
    return ctx.builder.CreateInBoundsGEP(ptls, ArrayRef<Value*>(offset), "jl_defer_signal");
}

static int compare_cgparams(const jl_cgparams_t *a, const jl_cgparams_t *b)
{
    return (a->cached == b->cached) &&
           // language features
           (a->track_allocations == b->track_allocations) &&
           (a->code_coverage == b->code_coverage) &&
           (a->static_alloc == b->static_alloc) &&
           (a->prefer_specsig == b->prefer_specsig) &&
           // hooks
           (a->module_setup == b->module_setup) &&
           (a->module_activation == b->module_activation) &&
           (a->raise_exception == b->raise_exception);
}
