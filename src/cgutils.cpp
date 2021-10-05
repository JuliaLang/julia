// This file is a part of Julia. License is MIT: https://julialang.org/license

// utility procedures used in code generation

Instruction *tbaa_decorate(MDNode *md, Instruction *inst)
{
    inst->setMetadata(llvm::LLVMContext::MD_tbaa, md);
    if (isa<LoadInst>(inst) && md == tbaa_const)
        inst->setMetadata(LLVMContext::MD_invariant_load, MDNode::get(md->getContext(), None));
    return inst;
}

static Value *track_pjlvalue(jl_codectx_t &ctx, Value *V)
{
    assert(V->getType() == T_pjlvalue);
    return ctx.builder.CreateAddrSpaceCast(V, T_prjlvalue);
}

// Take an arbitrary untracked value and make it gc-tracked
static Value *maybe_decay_untracked(jl_codectx_t &ctx, Value *V)
{
    if (V->getType() == T_pjlvalue)
        return ctx.builder.CreateAddrSpaceCast(V, T_prjlvalue);
    assert(V->getType() == T_prjlvalue);
    return V;
}

// Take any value and mark that it may be derived from a rooted value
static Value *decay_derived(jl_codectx_t &ctx, Value *V)
{
    Type *T = V->getType();
    if (cast<PointerType>(T)->getAddressSpace() == AddressSpace::Derived)
        return V;
    // Once llvm deletes pointer element types, we won't need it here any more either.
    Type *NewT = PointerType::get(cast<PointerType>(T)->getElementType(), AddressSpace::Derived);
    return ctx.builder.CreateAddrSpaceCast(V, NewT);
}

// Take any value and make it safe to pass to GEP
static Value *maybe_decay_tracked(jl_codectx_t &ctx, Value *V)
{
    Type *T = V->getType();
    if (cast<PointerType>(T)->getAddressSpace() != AddressSpace::Tracked)
        return V;
    Type *NewT = PointerType::get(cast<PointerType>(T)->getElementType(), AddressSpace::Derived);
    return ctx.builder.CreateAddrSpaceCast(V, NewT);
}

static Value *mark_callee_rooted(jl_codectx_t &ctx, Value *V)
{
    assert(V->getType() == T_pjlvalue || V->getType() == T_prjlvalue);
    return ctx.builder.CreateAddrSpaceCast(V,
        PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
}

AtomicOrdering get_llvm_atomic_order(enum jl_memory_order order)
{
    switch (order) {
    case jl_memory_order_notatomic: return AtomicOrdering::NotAtomic;
    case jl_memory_order_unordered: return AtomicOrdering::Unordered;
    case jl_memory_order_monotonic: return AtomicOrdering::Monotonic;
    case jl_memory_order_acquire:   return AtomicOrdering::Acquire;
    case jl_memory_order_release:   return AtomicOrdering::Release;
    case jl_memory_order_acq_rel:   return AtomicOrdering::AcquireRelease;
    case jl_memory_order_seq_cst:   return AtomicOrdering::SequentiallyConsistent;
    default:
        assert("invalid atomic ordering");
        abort();
    }
}

// --- language feature checks ---

#define JL_FEAT_TEST(ctx, feature) ((ctx).params->feature)


// --- string constants ---
static Value *stringConstPtr(
        jl_codegen_params_t &emission_context,
        IRBuilder<> &irbuilder,
        const std::string &txt)
{
    Module *M = jl_builderModule(irbuilder);
    StringRef ctxt(txt.c_str(), txt.size() + 1);
    Constant *Data = ConstantDataArray::get(jl_LLVMContext, arrayRefFromStringRef(ctxt));
    GlobalVariable *gv = get_pointer_to_constant(emission_context, Data, "_j_str", *M);
    Value *zero = ConstantInt::get(Type::getInt32Ty(jl_LLVMContext), 0);
    Value *Args[] = { zero, zero };
    return irbuilder.CreateInBoundsGEP(gv->getValueType(), gv, Args);
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

static DIType *_julia_type_to_di(jl_codegen_params_t *ctx, jl_value_t *jt, DIBuilder *dbuilder, bool isboxed)
{
    jl_datatype_t *jdt = (jl_datatype_t*)jt;
    if (isboxed || !jl_is_datatype(jt) || !jdt->isconcretetype)
        return jl_pvalue_dillvmt;
    assert(jdt->layout);
    DIType* _ditype = NULL;
    DIType* &ditype = (ctx ? ctx->ditypes[jdt] : _ditype);
    if (ditype)
        return ditype;
    const char *tname = jl_symbol_name(jdt->name->name);
    if (jl_is_primitivetype(jt)) {
        uint64_t SizeInBits = jl_datatype_nbits(jdt);
        ditype = dbuilder->createBasicType(tname, SizeInBits, llvm::dwarf::DW_ATE_unsigned);
    }
    else if (jl_is_structtype(jt) && !jl_is_layout_opaque(jdt->layout)) {
        size_t ntypes = jl_datatype_nfields(jdt);
        std::vector<llvm::Metadata*> Elements(ntypes);
        for (unsigned i = 0; i < ntypes; i++) {
            jl_value_t *el = jl_field_type_concrete(jdt, i);
            DIType *di;
            if (jl_field_isptr(jdt, i))
                di = jl_pvalue_dillvmt;
            // TODO: elseif jl_islayout_inline
            else
                di = _julia_type_to_di(ctx, el, dbuilder, false);
            Elements[i] = di;
        }
        DINodeArray ElemArray = dbuilder->getOrCreateArray(Elements);
        std::string unique_name;
        raw_string_ostream(unique_name) << (uintptr_t)jdt;
        ditype = dbuilder->createStructType(
                NULL,                       // Scope
                tname,                      // Name
                NULL,                       // File
                0,                          // LineNumber
                jl_datatype_nbits(jdt),     // SizeInBits
                8 * jl_datatype_align(jdt), // AlignInBits
                DINode::FlagZero,           // Flags
                NULL,                       // DerivedFrom
                ElemArray,                  // Elements
                dwarf::DW_LANG_Julia,       // RuntimeLanguage
                nullptr,                    // VTableHolder
                unique_name                 // UniqueIdentifier
                );
    }
    else {
        // return a typealias for types with hidden content
        ditype = dbuilder->createTypedef(jl_pvalue_dillvmt, tname, NULL, 0, NULL);
    }
    return ditype;
}

static DIType *julia_type_to_di(jl_codectx_t &ctx, jl_value_t *jt, DIBuilder *dbuilder, bool isboxed)
{
    return _julia_type_to_di(&ctx.emission_context, jt, dbuilder, isboxed);
}

static Value *emit_pointer_from_objref(jl_codectx_t &ctx, Value *V)
{
    unsigned AS = cast<PointerType>(V->getType())->getAddressSpace();
    if (AS != AddressSpace::Tracked && AS != AddressSpace::Derived)
        return V;
    V = decay_derived(ctx, V);
    Type *T = PointerType::get(T_jlvalue, AddressSpace::Derived);
    if (V->getType() != T)
        V = ctx.builder.CreateBitCast(V, T);
    Function *F = prepare_call(pointer_from_objref_func);
    CallInst *Call = ctx.builder.CreateCall(F, V);
    Call->setAttributes(F->getAttributes());
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


static inline Constant *literal_static_pointer_val(const void *p, Type *T = T_pjlvalue);

static Value *julia_pgv(jl_codectx_t &ctx, const char *cname, void *addr)
{
    // emit a GlobalVariable for a jl_value_t named "cname"
    // store the name given so we can reuse it (facilitating merging later)
    // so first see if there already is a GlobalVariable for this address
    GlobalVariable* &gv = ctx.global_targets[addr];
    Module *M = jl_Module;
    StringRef localname;
    std::string gvname;
    if (!gv) {
        raw_string_ostream(gvname) << cname << ctx.global_targets.size();
        localname = StringRef(gvname);
    }
    else {
        localname = gv->getName();
        if (gv->getParent() != M)
            gv = cast_or_null<GlobalVariable>(M->getNamedValue(localname));
    }
    if (gv == nullptr)
        gv = new GlobalVariable(*M, T_pjlvalue,
                                false, GlobalVariable::PrivateLinkage,
                                NULL, localname);
    // LLVM passes sometimes strip metadata when moving load around
    // since the load at the new location satisfy the same condition as the original one.
    // Mark the global as constant to LLVM code using our own metadata
    // which is much less likely to be striped.
    gv->setMetadata("julia.constgv", MDNode::get(gv->getContext(), None));
    assert(localname == gv->getName());
    assert(!gv->hasInitializer());
    return gv;
}

static Value *julia_pgv(jl_codectx_t &ctx, const char *prefix, jl_sym_t *name, jl_module_t *mod, void *addr)
{
    // emit a GlobalVariable for a jl_value_t, using the prefix, name, and module to
    // to create a readable name of the form prefixModA.ModB.name
    size_t len = strlen(jl_symbol_name(name)) + strlen(prefix) + 1;
    jl_module_t *parent = mod, *prev = NULL;
    while (parent != NULL && parent != prev) {
        len += strlen(jl_symbol_name(parent->name))+1;
        prev = parent;
        parent = parent->parent;
    }
    char *fullname = (char*)alloca(len);
    strcpy(fullname, prefix);
    len -= strlen(jl_symbol_name(name)) + 1;
    strcpy(fullname + len, jl_symbol_name(name));
    parent = mod;
    prev = NULL;
    while (parent != NULL && parent != prev) {
        size_t part = strlen(jl_symbol_name(parent->name)) + 1;
        strcpy(fullname + len - part, jl_symbol_name(parent->name));
        fullname[len - 1] = '.';
        len -= part;
        prev = parent;
        parent = parent->parent;
    }
    return julia_pgv(ctx, fullname, addr);
}

static JuliaVariable *julia_const_gv(jl_value_t *val);
static Value *literal_pointer_val_slot(jl_codectx_t &ctx, jl_value_t *p)
{
    // emit a pointer to a jl_value_t* which will allow it to be valid across reloading code
    // also, try to give it a nice name for gdb, for easy identification
    if (!imaging_mode) {
        // TODO: this is an optimization, but is it useful or premature
        // (it'll block any attempt to cache these, but can be simply deleted)
        Module *M = jl_Module;
        GlobalVariable *gv = new GlobalVariable(
                *M, T_pjlvalue, true, GlobalVariable::PrivateLinkage,
                literal_static_pointer_val(p));
        gv->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
        return gv;
    }
    if (JuliaVariable *gv = julia_const_gv(p)) {
        // if this is a known special object, use the existing GlobalValue
        return prepare_global_in(jl_Module, gv);
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
    else if (jl_is_datatype(jt) && jl_struct_try_layout((jl_datatype_t*)jt)) {
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
    if (jt == (jl_value_t*)jl_datatype_type) {
        // types are never allocated in julia code/on the stack
        // and this is the guarantee we have for the GC bits
        return 16;
    }
    assert(jl_is_datatype(jt) && jl_struct_try_layout((jl_datatype_t*)jt));
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
            if (align >= 1) {
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

// Returns T_pjlvalue
static Value *literal_pointer_val(jl_codectx_t &ctx, jl_value_t *p)
{
    if (p == NULL)
        return V_null;
    if (!imaging_mode)
        return literal_static_pointer_val(p);
    Value *pgv = literal_pointer_val_slot(ctx, p);
    return tbaa_decorate(tbaa_const, maybe_mark_load_dereferenceable(
            ctx.builder.CreateAlignedLoad(T_pjlvalue, pgv, Align(sizeof(void*))),
            false, jl_typeof(p)));
}

// Returns T_pjlvalue
static Value *literal_pointer_val(jl_codectx_t &ctx, jl_binding_t *p)
{
    // emit a pointer to any jl_value_t which will be valid across reloading code
    if (p == NULL)
        return V_null;
    if (!imaging_mode)
        return literal_static_pointer_val(p);
    // bindings are prefixed with jl_bnd#
    Value *pgv = julia_pgv(ctx, "jl_bnd#", p->name, p->owner, p);
    return tbaa_decorate(tbaa_const, maybe_mark_load_dereferenceable(
            ctx.builder.CreateAlignedLoad(T_pjlvalue, pgv, Align(sizeof(void*))),
            false, sizeof(jl_binding_t), alignof(jl_binding_t)));
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
    return ctx.builder.CreateInBoundsGEP(T_prjlvalue, bv, offset);
}

static Value *julia_binding_gv(jl_codectx_t &ctx, jl_binding_t *b)
{
    // emit a literal_pointer_val to the value field of a jl_binding_t
    // binding->value are prefixed with *
    Value *bv;
    if (imaging_mode)
        bv = emit_bitcast(ctx,
                tbaa_decorate(tbaa_const,
                              ctx.builder.CreateAlignedLoad(T_pjlvalue, julia_pgv(ctx, "*", b->name, b->owner, b), Align(sizeof(void*)))),
                T_pprjlvalue);
    else
        bv = ConstantExpr::getBitCast(literal_static_pointer_val(b), T_pprjlvalue);
    return julia_binding_gv(ctx, bv);
}

// --- mapping between julia and llvm types ---

static bool type_is_permalloc(jl_value_t *typ)
{
    // Singleton should almost always be handled by the later optimization passes.
    // Also do it here since it is cheap and save some effort in LLVM passes.
    if (jl_is_datatype(typ) && jl_is_datatype_singleton((jl_datatype_t*)typ))
        return true;
    return typ == (jl_value_t*)jl_symbol_type ||
        typ == (jl_value_t*)jl_int8_type ||
        typ == (jl_value_t*)jl_uint8_type;
}

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

static Type *_julia_struct_to_llvm(jl_codegen_params_t *ctx, jl_value_t *jt, bool *isboxed, bool llvmcall=false);

static Type *_julia_type_to_llvm(jl_codegen_params_t *ctx, jl_value_t *jt, bool *isboxed)
{
    // this function converts a Julia Type into the equivalent LLVM type
    if (isboxed) *isboxed = false;
    if (jt == (jl_value_t*)jl_bottom_type)
        return T_void;
    if (jl_is_concrete_immutable(jt)) {
        if (jl_datatype_nbits(jt) == 0)
            return T_void;
        Type *t = _julia_struct_to_llvm(ctx, jt, isboxed);
        assert(t != NULL);
        return t;
    }
    if (isboxed) *isboxed = true;
    return T_prjlvalue;
}

static Type *julia_type_to_llvm(jl_codectx_t &ctx, jl_value_t *jt, bool *isboxed)
{
    return _julia_type_to_llvm(&ctx.emission_context, jt, isboxed);
}

extern "C" JL_DLLEXPORT
Type *jl_type_to_llvm_impl(jl_value_t *jt, bool *isboxed)
{
    return _julia_type_to_llvm(NULL, jt, isboxed);
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
    if (bt == (jl_value_t*)jl_float16_type)
        return T_float16;
    if (bt == (jl_value_t*)jl_float32_type)
        return T_float32;
    if (bt == (jl_value_t*)jl_float64_type)
        return T_float64;
    if (jl_is_llvmpointer_type(bt)) {
        jl_value_t *as_param = jl_tparam1(bt);
        int as;
        if (jl_is_int32(as_param))
            as = jl_unbox_int32(as_param);
        else if (jl_is_int64(as_param))
            as = jl_unbox_int64(as_param);
        else
            jl_error("invalid pointer address space");
        return PointerType::get(T_int8, as);
    }
    int nb = jl_datatype_size(bt);
    return Type::getIntNTy(jl_LLVMContext, nb * 8);
}

static bool jl_type_hasptr(jl_value_t* typ)
{ // assumes that jl_stored_inline(typ) is true (and therefore that layout is defined)
    return jl_is_datatype(typ) && ((jl_datatype_t*)typ)->layout->npointers > 0;
}

static unsigned jl_field_align(jl_datatype_t *dt, size_t i)
{
    unsigned al = jl_field_offset(dt, i);
    al |= 16;
    al &= -al;
    return std::min({al, (unsigned)jl_datatype_align(dt), (unsigned)JL_HEAP_ALIGNMENT});
}

static Type *_julia_struct_to_llvm(jl_codegen_params_t *ctx, jl_value_t *jt, bool *isboxed, bool llvmcall)
{
    // this function converts a Julia Type into the equivalent LLVM struct
    // use this where C-compatible (unboxed) structs are desired
    // use julia_type_to_llvm directly when you want to preserve Julia's type semantics
    if (isboxed) *isboxed = false;
    if (jt == (jl_value_t*)jl_bottom_type)
        return T_void;
    if (jl_is_primitivetype(jt))
        return bitstype_to_llvm(jt, llvmcall);
    jl_datatype_t *jst = (jl_datatype_t*)jt;
    if (jl_is_structtype(jt) && !(jst->layout && jl_is_layout_opaque(jst->layout))) {
        bool isTuple = jl_is_tuple_type(jt);
        jl_svec_t *ftypes = jl_get_fieldtypes(jst);
        size_t i, ntypes = jl_svec_len(ftypes);
        if (!jl_struct_try_layout(jst))
            return NULL; // caller should have checked jl_type_mappable_to_c already, but we'll be nice
        if (ntypes == 0 || jl_datatype_nbits(jst) == 0)
            return T_void;
        Type *_struct_decl = NULL;
        // TODO: we should probably make a temporary root for `jst` somewhere
        // don't use pre-filled struct_decl for llvmcall (f16, etc. may be different)
        Type *&struct_decl = (ctx && !llvmcall ? ctx->llvmtypes[jst] : _struct_decl);
        if (struct_decl)
            return struct_decl;
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
            if (jl_field_isatomic(jst, i)) {
                // TODO: eventually support this?
                // though it's a bit unclear how the implicit load should be interpreted
                return NULL;
            }
            Type *lty;
            if (jl_field_isptr(jst, i)) {
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
                size_t fsz = 0, al = 0;
                bool isptr = !jl_islayout_inline(ty, &fsz, &al);
                assert(!isptr && fsz == jl_field_size(jst, i) - 1); (void)isptr;
                if (fsz > 0) {
                    if (al > MAX_ALIGN) {
                        Type *AlignmentType;
                        AlignmentType = ArrayType::get(FixedVectorType::get(T_int8, al), 0);
                        latypes.push_back(AlignmentType);
                        al = MAX_ALIGN;
                    }
                    Type *AlignmentType = IntegerType::get(jl_LLVMContext, 8 * al);
                    unsigned NumATy = fsz / al;
                    unsigned remainder = fsz % al;
                    assert(al == 1 || NumATy > 0);
                    while (NumATy--)
                        latypes.push_back(AlignmentType);
                    while (remainder--)
                        latypes.push_back(T_int8);
                }
                latypes.push_back(T_int8);
                isarray = false;
                allghost = false;
                continue;
            }
            else {
                bool isptr;
                lty = _julia_struct_to_llvm(ctx, ty, &isptr, llvmcall);
                assert(lty && !isptr);
            }
            if (lasttype != NULL && lasttype != lty)
                isarray = false;
            lasttype = lty;
            if (!type_is_ghost(lty)) {
                allghost = false;
                latypes.push_back(lty);
            }
        }
        if (allghost) {
            assert(jst->layout == NULL); // otherwise should have been caught above
            struct_decl = T_void;
        }
        else if (jl_is_vecelement_type(jt) && !jl_is_uniontype(jl_svecref(ftypes, 0))) {
            // VecElement type is unwrapped in LLVM (when possible)
            struct_decl = latypes[0];
        }
        else if (isarray && !type_is_ghost(lasttype)) {
            if (isTuple && isvector && jl_special_vector_alignment(ntypes, jlasttype) != 0)
                struct_decl = FixedVectorType::get(lasttype, ntypes);
            else if (isTuple || !llvmcall)
                struct_decl = ArrayType::get(lasttype, ntypes);
            else
                struct_decl = StructType::get(jl_LLVMContext, latypes);
        }
        else {
#if 0 // stress-test code that tries to assume julia-index == llvm-index
      // (also requires change to emit_new_struct to not assume 0 == 0)
            if (!isTuple && latypes.size() > 1) {
                Type *NoopType = ArrayType::get(T_int1, 0);
                latypes.insert(latypes.begin(), NoopType);
            }
#endif
            struct_decl = StructType::get(jl_LLVMContext, latypes);
        }
        return struct_decl;
    }
    // TODO: enable this (with tests) to change ccall calling convention for Union:
    // if (jl_is_uniontype(ty)) {
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

static Type *julia_struct_to_llvm(jl_codectx_t &ctx, jl_value_t *jt, bool *isboxed)
{
    return _julia_struct_to_llvm(&ctx.emission_context, jt, isboxed);
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
            if (allow_va && jl_is_vararg(t0) &&
                  jl_is_concrete_type(jl_unwrap_vararg(t0)))
                return true;
            return false;
        }
        for (i = 1; i < l; i++) {
            if (allow_va && i == l - 1 && jl_is_vararg(jl_svecref(t, i))) {
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

static bool is_uniontype_allunboxed(jl_value_t *typ)
{
    unsigned counter = 0;
    return for_each_uniontype_small([&](unsigned, jl_datatype_t*) {}, typ, counter);
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
            emit_bitcast(ctx, maybe_decay_tracked(ctx, v), T_pprjlvalue),
            ConstantInt::get(T_size, n));
}

static Value *emit_nthptr_addr(jl_codectx_t &ctx, Value *v, Value *idx)
{
    return ctx.builder.CreateInBoundsGEP(
            T_prjlvalue,
            emit_bitcast(ctx, maybe_decay_tracked(ctx, v), T_pprjlvalue),
            idx);
}

static LoadInst *emit_nthptr_recast(jl_codectx_t &ctx, Value *v, Value *idx, MDNode *tbaa, Type *ptype)
{
    // p = (jl_value_t**)v; *(ptype)&p[n]
    Value *vptr = emit_nthptr_addr(ctx, v, idx);
    return cast<LoadInst>(tbaa_decorate(tbaa, ctx.builder.CreateLoad(emit_bitcast(ctx, vptr, ptype))));
}

static LoadInst *emit_nthptr_recast(jl_codectx_t &ctx, Value *v, ssize_t n, MDNode *tbaa, Type *ptype)
{
    // p = (jl_value_t**)v; *(ptype)&p[n]
    Value *vptr = emit_nthptr_addr(ctx, v, n);
    return cast<LoadInst>(tbaa_decorate(tbaa, ctx.builder.CreateLoad(emit_bitcast(ctx, vptr, ptype))));
}

static Value *boxed(jl_codectx_t &ctx, const jl_cgval_t &v);

// Returns T_prjlvalue
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
        bool allunboxed = is_uniontype_allunboxed(p.typ);
        Value *datatype_or_p = imaging_mode ? Constant::getNullValue(T_ppjlvalue) : V_rnull;
        unsigned counter = 0;
        for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                Value *cmp = ctx.builder.CreateICmpEQ(tindex, ConstantInt::get(T_int8, idx));
                Value *ptr;
                if (imaging_mode) {
                    ptr = literal_pointer_val_slot(ctx, (jl_value_t*)jt);
                }
                else {
                    ptr = track_pjlvalue(ctx, literal_pointer_val(ctx, (jl_value_t*)jt));
                }
                datatype_or_p = ctx.builder.CreateSelect(cmp, ptr, datatype_or_p);
            },
            p.typ,
            counter);
        auto emit_unboxty = [&] () -> Value* {
            if (imaging_mode)
                return track_pjlvalue(
                    ctx, tbaa_decorate(tbaa_const, ctx.builder.CreateAlignedLoad(T_pjlvalue, datatype_or_p, Align(sizeof(void*)))));
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
            boxBB = ctx.builder.GetInsertBlock(); // could have changed
            ctx.builder.SetInsertPoint(unboxBB);
            auto unboxTy = emit_unboxty();
            ctx.builder.CreateBr(mergeBB);
            unboxBB = ctx.builder.GetInsertBlock(); // could have changed
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

// Returns T_prjlvalue
static Value *emit_typeof_boxed(jl_codectx_t &ctx, const jl_cgval_t &p)
{
    return boxed(ctx, emit_typeof(ctx, p));
}

static Value *emit_datatype_types(jl_codectx_t &ctx, Value *dt)
{
    Value *Ptr = emit_bitcast(ctx, decay_derived(ctx, dt), T_ppjlvalue);
    Value *Idx = ConstantInt::get(T_size, offsetof(jl_datatype_t, types) / sizeof(void*));
    return tbaa_decorate(tbaa_const, ctx.builder.CreateAlignedLoad(
                T_pjlvalue, ctx.builder.CreateInBoundsGEP(T_pjlvalue, Ptr, Idx), Align(sizeof(void*))));
}

static Value *emit_datatype_nfields(jl_codectx_t &ctx, Value *dt)
{
    Value *type_svec = emit_bitcast(ctx, emit_datatype_types(ctx, dt), T_psize);
    return tbaa_decorate(tbaa_const, ctx.builder.CreateAlignedLoad(T_size, type_svec, Align(sizeof(void*))));
}

static Value *emit_datatype_size(jl_codectx_t &ctx, Value *dt)
{
    Value *Ptr = emit_bitcast(ctx, decay_derived(ctx, dt), T_pint32);
    Value *Idx = ConstantInt::get(T_size, offsetof(jl_datatype_t, size) / sizeof(int));
    return tbaa_decorate(tbaa_const, ctx.builder.CreateAlignedLoad(T_int32, ctx.builder.CreateInBoundsGEP(T_int32, Ptr, Idx), Align(sizeof(int32_t))));
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
            dynloadBB = ctx.builder.GetInsertBlock(); // could have changed
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
    Value *Ptr = emit_bitcast(ctx, decay_derived(ctx, dt), T_ppint8);
    Value *Idx = ConstantInt::get(T_size, offsetof(jl_datatype_t, name));
    Value *Nam = tbaa_decorate(tbaa_const,
            ctx.builder.CreateAlignedLoad(T_pint8, ctx.builder.CreateInBoundsGEP(T_pint8, Ptr, Idx), Align(sizeof(int8_t*))));
    Value *Idx2 = ConstantInt::get(T_size, offsetof(jl_typename_t, n_uninitialized) + sizeof(((jl_typename_t*)nullptr)->n_uninitialized));
    Value *mutabl = tbaa_decorate(tbaa_const,
            ctx.builder.CreateAlignedLoad(T_int8, ctx.builder.CreateInBoundsGEP(T_int8, Nam, Idx2), Align(1)));
    mutabl = ctx.builder.CreateLShr(mutabl, 1);
    return ctx.builder.CreateTrunc(mutabl, T_int1);
}

static Value *emit_datatype_isprimitivetype(jl_codectx_t &ctx, Value *dt)
{
    Value *immut = ctx.builder.CreateNot(emit_datatype_mutabl(ctx, dt));
    Value *nofields = ctx.builder.CreateICmpEQ(emit_datatype_nfields(ctx, dt), V_size0);
    Value *sized = ctx.builder.CreateICmpSGT(emit_datatype_size(ctx, dt), ConstantInt::get(T_int32, 0));
    return ctx.builder.CreateAnd(immut, ctx.builder.CreateAnd(nofields, sized));
}

static Value *emit_datatype_name(jl_codectx_t &ctx, Value *dt)
{
    Value *vptr = emit_nthptr_addr(ctx, dt, (ssize_t)(offsetof(jl_datatype_t, name) / sizeof(char*)));
    return tbaa_decorate(tbaa_const, ctx.builder.CreateAlignedLoad(T_prjlvalue, vptr, Align(sizeof(void*))));
}

// --- generating various error checks ---
// Do not use conditional throw for cases that type inference can know
// the error is always thrown. This may cause non dominated use
// of SSA value error in the verifier.

static void just_emit_error(jl_codectx_t &ctx, Function *F, const std::string &txt)
{
    ctx.builder.CreateCall(F, stringConstPtr(ctx.emission_context, ctx.builder, txt));
}

static void emit_error(jl_codectx_t &ctx, Function *F, const std::string &txt)
{
    just_emit_error(ctx, F, txt);
    ctx.builder.CreateUnreachable();
    BasicBlock *cont = BasicBlock::Create(jl_LLVMContext, "after_error", ctx.f);
    ctx.builder.SetInsertPoint(cont);
}

static void emit_error(jl_codectx_t &ctx, const std::string &txt)
{
    emit_error(ctx, prepare_call(jlerror_func), txt);
}

// DO NOT PASS IN A CONST CONDITION!
static void error_unless(jl_codectx_t &ctx, Value *cond, const std::string &msg)
{
    BasicBlock *failBB = BasicBlock::Create(jl_LLVMContext, "fail", ctx.f);
    BasicBlock *passBB = BasicBlock::Create(jl_LLVMContext, "pass");
    ctx.builder.CreateCondBr(cond, passBB, failBB);
    ctx.builder.SetInsertPoint(failBB);
    just_emit_error(ctx, prepare_call(jlerror_func), msg);
    ctx.builder.CreateUnreachable();
    ctx.f->getBasicBlockList().push_back(passBB);
    ctx.builder.SetInsertPoint(passBB);
}

static void raise_exception(jl_codectx_t &ctx, Value *exc,
                            BasicBlock *contBB=nullptr)
{
    ctx.builder.CreateCall(prepare_call(jlthrow_func), { mark_callee_rooted(ctx, exc) });
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

static Value *null_pointer_cmp(jl_codectx_t &ctx, Value *v)
{
    return ctx.builder.CreateICmpNE(v, Constant::getNullValue(v->getType()));
}


// If `nullcheck` is not NULL and a pointer NULL check is necessary
// store the pointer to be checked in `*nullcheck` instead of checking it
static void null_pointer_check(jl_codectx_t &ctx, Value *v, Value **nullcheck = nullptr)
{
    if (nullcheck) {
        *nullcheck = v;
        return;
    }
    raise_exception_unless(ctx, null_pointer_cmp(ctx, v),
            literal_pointer_val(ctx, jl_undefref_exception));
}

template<typename Func>
static Value *emit_guarded_test(jl_codectx_t &ctx, Value *ifnot, Value *defval, Func &&func)
{
    if (auto Cond = dyn_cast<ConstantInt>(ifnot)) {
        if (Cond->isZero())
            return defval;
        return func();
    }
    BasicBlock *currBB = ctx.builder.GetInsertBlock();
    BasicBlock *passBB = BasicBlock::Create(jl_LLVMContext, "guard_pass", ctx.f);
    BasicBlock *exitBB = BasicBlock::Create(jl_LLVMContext, "guard_exit", ctx.f);
    ctx.builder.CreateCondBr(ifnot, passBB, exitBB);
    ctx.builder.SetInsertPoint(passBB);
    auto res = func();
    passBB = ctx.builder.GetInsertBlock();
    ctx.builder.CreateBr(exitBB);
    ctx.builder.SetInsertPoint(exitBB);
    if (defval == nullptr)
        return nullptr;
    PHINode *phi = ctx.builder.CreatePHI(defval->getType(), 2);
    phi->addIncoming(defval, currBB);
    phi->addIncoming(res, passBB);
    return phi;
}

template<typename Func>
static Value *emit_guarded_test(jl_codectx_t &ctx, Value *ifnot, bool defval, Func &&func)
{
    return emit_guarded_test(ctx, ifnot, ConstantInt::get(T_int1, defval), func);
}

template<typename Func>
static Value *emit_nullcheck_guard(jl_codectx_t &ctx, Value *nullcheck, Func &&func)
{
    if (!nullcheck)
        return func();
    return emit_guarded_test(ctx, null_pointer_cmp(ctx, nullcheck), false, func);
}

template<typename Func>
static Value *emit_nullcheck_guard2(jl_codectx_t &ctx, Value *nullcheck1,
                                    Value *nullcheck2, Func &&func)
{
    if (!nullcheck1)
        return emit_nullcheck_guard(ctx, nullcheck2, func);
    if (!nullcheck2)
        return emit_nullcheck_guard(ctx, nullcheck1, func);
    nullcheck1 = null_pointer_cmp(ctx, nullcheck1);
    nullcheck2 = null_pointer_cmp(ctx, nullcheck2);
    // If both are NULL, return true.
    return emit_guarded_test(ctx, ctx.builder.CreateOr(nullcheck1, nullcheck2), true, [&] {
        return emit_guarded_test(ctx, ctx.builder.CreateAnd(nullcheck1, nullcheck2),
                                 false, func);
    });
}

static void emit_type_error(jl_codectx_t &ctx, const jl_cgval_t &x, Value *type, const std::string &msg)
{
    Value *msg_val = stringConstPtr(ctx.emission_context, ctx.builder, msg);
    ctx.builder.CreateCall(prepare_call(jltypeerror_func),
                       { msg_val, maybe_decay_untracked(ctx, type), mark_callee_rooted(ctx, boxed(ctx, x))});
}

// Should agree with `emit_isa` below
static bool _can_optimize_isa(jl_value_t *type, int &counter)
{
    if (counter > 127)
        return false;
    if (jl_is_uniontype(type)) {
        counter++;
        return (_can_optimize_isa(((jl_uniontype_t*)type)->a, counter) &&
                _can_optimize_isa(((jl_uniontype_t*)type)->b, counter));
    }
    if (jl_is_type_type(type) && jl_pointer_egal(type))
        return true;
    if (jl_has_intersect_type_not_kind(type))
        return false;
    if (jl_is_concrete_type(type))
        return true;
    jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(type);
    if (jl_is_datatype(dt) && !dt->name->abstract && jl_subtype(dt->name->wrapper, type))
        return true;
    return false;
}

static bool can_optimize_isa_union(jl_uniontype_t *type)
{
    int counter = 1;
    return (_can_optimize_isa(type->a, counter) && _can_optimize_isa(type->b, counter));
}

// a simple case of emit_isa that is obvious not to include a safe-point
static Value *emit_exactly_isa(jl_codectx_t &ctx, const jl_cgval_t &arg, jl_value_t *dt)
{
    assert(jl_is_concrete_type(dt));
    return ctx.builder.CreateICmpEQ(
            emit_typeof_boxed(ctx, arg),
            track_pjlvalue(ctx, literal_pointer_val(ctx, dt)));
}

static std::pair<Value*, bool> emit_isa(jl_codectx_t &ctx, const jl_cgval_t &x,
                                        jl_value_t *type, const std::string *msg);

static void emit_isa_union(jl_codectx_t &ctx, const jl_cgval_t &x, jl_value_t *type,
                           SmallVectorImpl<std::pair<std::pair<BasicBlock*,BasicBlock*>,Value*>> &bbs)
{
    if (jl_is_uniontype(type)) {
        emit_isa_union(ctx, x, ((jl_uniontype_t*)type)->a, bbs);
        emit_isa_union(ctx, x, ((jl_uniontype_t*)type)->b, bbs);
        return;
    }
    BasicBlock *enter = ctx.builder.GetInsertBlock();
    Value *v = emit_isa(ctx, x, type, nullptr).first;
    BasicBlock *exit = ctx.builder.GetInsertBlock();
    bbs.emplace_back(std::make_pair(enter, exit), v);
    BasicBlock *isaBB = BasicBlock::Create(jl_LLVMContext, "isa", ctx.f);
    ctx.builder.SetInsertPoint(isaBB);
}

// Should agree with `_can_optimize_isa` above
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

    if (jl_is_type_type(intersected_type) && jl_pointer_egal(intersected_type)) {
        // Use the check in `jl_pointer_egal` to see if the type enclosed
        // has unique pointer value.
        auto ptr = track_pjlvalue(ctx, literal_pointer_val(ctx, jl_tparam0(intersected_type)));
        return {ctx.builder.CreateICmpEQ(boxed(ctx, x), ptr), false};
    }
    // intersection with Type needs to be handled specially
    if (jl_has_intersect_type_not_kind(type) || jl_has_intersect_type_not_kind(intersected_type)) {
        Value *vx = boxed(ctx, x);
        Value *vtyp = track_pjlvalue(ctx, literal_pointer_val(ctx, type));
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
                    track_pjlvalue(ctx, literal_pointer_val(ctx, intersected_type)));
                ctx.builder.CreateBr(postBB);
                isaBB = ctx.builder.GetInsertBlock(); // could have changed
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
        return std::make_pair(emit_exactly_isa(ctx, x, intersected_type), false);
    }
    jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(intersected_type);
    if (jl_is_datatype(dt) && !dt->name->abstract && jl_subtype(dt->name->wrapper, type)) {
        // intersection is a supertype of all instances of its constructor,
        // so the isa test reduces to a comparison of the typename by pointer
        return std::make_pair(
                ctx.builder.CreateICmpEQ(
                    mark_callee_rooted(ctx, emit_datatype_name(ctx, emit_typeof_boxed(ctx, x))),
                    mark_callee_rooted(ctx, literal_pointer_val(ctx, (jl_value_t*)dt->name))),
                false);
    }
    if (jl_is_uniontype(intersected_type) &&
        can_optimize_isa_union((jl_uniontype_t*)intersected_type)) {
        SmallVector<std::pair<std::pair<BasicBlock*,BasicBlock*>,Value*>,4> bbs;
        emit_isa_union(ctx, x, intersected_type, bbs);
        int nbbs = bbs.size();
        BasicBlock *currBB = ctx.builder.GetInsertBlock();
        PHINode *res = ctx.builder.CreatePHI(T_int1, nbbs);
        for (int i = 0; i < nbbs; i++) {
            auto bb = bbs[i].first.second;
            ctx.builder.SetInsertPoint(bb);
            if (i + 1 < nbbs) {
                ctx.builder.CreateCondBr(bbs[i].second, currBB, bbs[i + 1].first.first);
                res->addIncoming(ConstantInt::get(T_int1, 1), bb);
            }
            else {
                ctx.builder.CreateBr(currBB);
                res->addIncoming(bbs[i].second, bb);
            }
        }
        ctx.builder.SetInsertPoint(currBB);
        return {res, false};
    }
    // everything else can be handled via subtype tests
    return std::make_pair(ctx.builder.CreateICmpNE(
            ctx.builder.CreateCall(prepare_call(jlsubtype_func),
              { emit_typeof_boxed(ctx, x),
                track_pjlvalue(ctx, literal_pointer_val(ctx, type)) }),
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
    isconcrete = ctx.builder.CreateConstInBoundsGEP1_32(T_int8, emit_bitcast(ctx, decay_derived(ctx, typ), T_pint8), offsetof(jl_datatype_t, hash) + sizeof(((jl_datatype_t*)nullptr)->hash));
    isconcrete = tbaa_decorate(tbaa_const, ctx.builder.CreateAlignedLoad(T_int8, isconcrete, Align(1)));
    isconcrete = ctx.builder.CreateLShr(isconcrete, 1);
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
            ctx.builder.CreateCall(prepare_call(jlboundserror_func), { mark_callee_rooted(ctx, boxed(ctx, ainfo)), i });
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
                    emit_bitcast(ctx, decay_derived(ctx, a), T_pint8),
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

static Value *emit_unbox(jl_codectx_t &ctx, Type *to, const jl_cgval_t &x, jl_value_t *jt, Value* dest, MDNode *tbaa_dest, bool isVolatile = false);
static Value *emit_unbox(jl_codectx_t &ctx, Type *to, const jl_cgval_t &x, jl_value_t *jt)
{
    return emit_unbox(ctx, to, x, jt, nullptr, nullptr, false);
}
static void emit_write_barrier(jl_codectx_t&, Value*, ArrayRef<Value*>);
static void emit_write_barrier(jl_codectx_t&, Value*, Value*);
static void emit_write_multibarrier(jl_codectx_t&, Value*, Value*, jl_value_t*);

std::vector<unsigned> first_ptr(Type *T)
{
    if (isa<StructType>(T) || isa<ArrayType>(T) || isa<VectorType>(T)) {
        if (!isa<StructType>(T)) {
            uint64_t num_elements;
            if (auto *AT = dyn_cast<ArrayType>(T))
                num_elements = AT->getNumElements();
            else {
                VectorType *VT = cast<VectorType>(T);
#if JL_LLVM_VERSION >= 120000
                ElementCount EC = VT->getElementCount();
                num_elements = EC.getKnownMinValue();
#else
                num_elements = VT->getNumElements();
#endif
            }
            if (num_elements == 0)
                return {};
        }
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


static void emit_lockstate_value(jl_codectx_t &ctx, Value *strct, bool newstate)
{
    Value *v = mark_callee_rooted(ctx, strct);
    ctx.builder.CreateCall(prepare_call(newstate ? jllockvalue_func : jlunlockvalue_func), v);
}
static void emit_lockstate_value(jl_codectx_t &ctx, const jl_cgval_t &strct, bool newstate)
{
    assert(strct.isboxed);
    emit_lockstate_value(ctx, boxed(ctx, strct), newstate);
}


// If `nullcheck` is not NULL and a pointer NULL check is necessary
// store the pointer to be checked in `*nullcheck` instead of checking it
static jl_cgval_t typed_load(jl_codectx_t &ctx, Value *ptr, Value *idx_0based, jl_value_t *jltype,
                             MDNode *tbaa, MDNode *aliasscope, bool isboxed, AtomicOrdering Order,
                             bool maybe_null_if_boxed = true, unsigned alignment = 0,
                             Value **nullcheck = nullptr)
{
    Type *elty = isboxed ? T_prjlvalue : julia_type_to_llvm(ctx, jltype);
    if (type_is_ghost(elty))
        return ghostValue(jltype);
    AllocaInst *intcast = NULL;
    if (!isboxed && Order != AtomicOrdering::NotAtomic && !elty->isIntOrPtrTy()) {
        const DataLayout &DL = jl_data_layout;
        unsigned nb = DL.getTypeSizeInBits(elty);
        intcast = ctx.builder.CreateAlloca(elty);
        elty = Type::getIntNTy(jl_LLVMContext, nb);
    }
    Type *realelty = elty;
    if (Order != AtomicOrdering::NotAtomic && isa<IntegerType>(elty)) {
        unsigned nb = cast<IntegerType>(elty)->getBitWidth();
        unsigned nb2 = PowerOf2Ceil(nb);
        if (nb != nb2)
            elty = Type::getIntNTy(jl_LLVMContext, nb2);
    }
    Type *ptrty = PointerType::get(elty, ptr->getType()->getPointerAddressSpace());
    Value *data;
    if (ptr->getType() != ptrty)
        data = emit_bitcast(ctx, ptr, ptrty);
    else
        data = ptr;
    if (idx_0based)
        data = ctx.builder.CreateInBoundsGEP(elty, data, idx_0based);
    Value *instr;
    // TODO: can only lazy load if we can create a gc root for ptr for the lifetime of elt
    //if (elty->isAggregateType() && tbaa == tbaa_immut && !alignment) { // can lazy load on demand, no copy needed
    //    elt = data;
    //}
    //else {
        if (isboxed)
            alignment = sizeof(void*);
        else if (!alignment)
            alignment = julia_alignment(jltype);
        LoadInst *load = ctx.builder.CreateAlignedLoad(data, Align(alignment), false);
        load->setOrdering(Order);
        if (aliasscope)
            load->setMetadata("alias.scope", aliasscope);
        if (isboxed)
            maybe_mark_load_dereferenceable(load, true, jltype);
        if (tbaa)
            tbaa_decorate(tbaa, load);
        instr = load;
        if (elty != realelty)
            instr = ctx.builder.CreateTrunc(instr, realelty);
        if (intcast) {
            ctx.builder.CreateStore(instr, ctx.builder.CreateBitCast(intcast, instr->getType()->getPointerTo()));
            instr = ctx.builder.CreateLoad(intcast);
        }
        if (maybe_null_if_boxed) {
            Value *first_ptr = isboxed ? instr : extract_first_ptr(ctx, instr);
            if (first_ptr)
                null_pointer_check(ctx, first_ptr, nullcheck);
        }
    //}
    if (jltype == (jl_value_t*)jl_bool_type) { // "freeze" undef memory to a valid value
        // NOTE: if we zero-initialize arrays, this optimization should become valid
        //load->setMetadata(LLVMContext::MD_range, MDNode::get(jl_LLVMContext, {
        //    ConstantAsMetadata::get(ConstantInt::get(T_int8, 0)),
        //    ConstantAsMetadata::get(ConstantInt::get(T_int8, 2)) }));
        instr = ctx.builder.CreateTrunc(instr, T_int1);
    }
    return mark_julia_type(ctx, instr, isboxed, jltype);
}

static jl_cgval_t typed_store(jl_codectx_t &ctx,
        Value *ptr, Value *idx_0based, jl_cgval_t rhs, jl_cgval_t cmp,
        jl_value_t *jltype, MDNode *tbaa, MDNode *aliasscope,
        Value *parent,  // for the write barrier, NULL if no barrier needed
        bool isboxed, AtomicOrdering Order, AtomicOrdering FailOrder, unsigned alignment,
        bool needlock, bool issetfield, bool isreplacefield, bool isswapfield, bool ismodifyfield,
        bool maybe_null_if_boxed, const jl_cgval_t *modifyop, const std::string &fname)
{
    auto newval = [&](const jl_cgval_t &lhs) {
        const jl_cgval_t argv[3] = { cmp, lhs, rhs };
        jl_cgval_t ret;
        if (modifyop) {
            ret = emit_invoke(ctx, *modifyop, argv, 3, (jl_value_t*)jl_any_type);
        }
        else {
            Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, 3, JLCALL_F_CC);
            ret = mark_julia_type(ctx, callval, true, jl_any_type);
        }
        if (!jl_subtype(ret.typ, jltype)) {
            emit_typecheck(ctx, ret, jltype, fname);
            ret = update_julia_type(ctx, ret, jltype);
        }
        return ret;
    };
    assert(!needlock || parent != nullptr);
    Type *elty = isboxed ? T_prjlvalue : julia_type_to_llvm(ctx, jltype);
    if (type_is_ghost(elty)) {
        if (isStrongerThanMonotonic(Order))
            ctx.builder.CreateFence(Order);
        if (issetfield) {
            return rhs;
        }
        else if (isreplacefield) {
            Value *Success = emit_f_is(ctx, cmp, ghostValue(jltype));
            Success = ctx.builder.CreateZExt(Success, T_int8);
            const jl_cgval_t argv[2] = {ghostValue(jltype), mark_julia_type(ctx, Success, false, jl_bool_type)};
            jl_datatype_t *rettyp = jl_apply_cmpswap_type(jltype);
            return emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
        }
        else if (isswapfield) {
            return ghostValue(jltype);
        }
        else { // modifyfield
            jl_cgval_t oldval = ghostValue(jltype);
            const jl_cgval_t argv[2] = { oldval, newval(oldval) };
            jl_datatype_t *rettyp = jl_apply_modify_type(jltype);
            return emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
        }
    }
    Value *intcast = nullptr;
    if (!isboxed && Order != AtomicOrdering::NotAtomic && !elty->isIntOrPtrTy()) {
        const DataLayout &DL = jl_data_layout;
        unsigned nb = DL.getTypeSizeInBits(elty);
        if (!issetfield)
            intcast = ctx.builder.CreateAlloca(elty);
        elty = Type::getIntNTy(jl_LLVMContext, nb);
    }
    Type *realelty = elty;
    if (Order != AtomicOrdering::NotAtomic && isa<IntegerType>(elty)) {
        unsigned nb = cast<IntegerType>(elty)->getBitWidth();
        unsigned nb2 = PowerOf2Ceil(nb);
        if (nb != nb2)
            elty = Type::getIntNTy(jl_LLVMContext, nb2);
    }
    Value *r = nullptr;
    if (issetfield || isswapfield || isreplacefield)  {
        if (!isboxed)
            r = emit_unbox(ctx, realelty, rhs, jltype);
        else
            r = boxed(ctx, rhs);
        if (realelty != elty)
            r = ctx.builder.CreateZExt(r, elty);
    }
    Type *ptrty = PointerType::get(elty, ptr->getType()->getPointerAddressSpace());
    if (ptr->getType() != ptrty)
        ptr = ctx.builder.CreateBitCast(ptr, ptrty);
    if (idx_0based)
        ptr = ctx.builder.CreateInBoundsGEP(r->getType(), ptr, idx_0based);
    if (isboxed)
        alignment = sizeof(void*);
    else if (!alignment)
        alignment = julia_alignment(jltype);
    Value *instr = nullptr;
    Value *Compare = nullptr;
    Value *Success = nullptr;
    BasicBlock *DoneBB = nullptr;
    if (needlock)
        emit_lockstate_value(ctx, parent, true);
    jl_cgval_t oldval = rhs;
    if (issetfield || (Order == AtomicOrdering::NotAtomic && isswapfield)) {
        if (isswapfield) {
            auto *load = ctx.builder.CreateAlignedLoad(elty, ptr, Align(alignment));
            if (aliasscope)
                load->setMetadata("noalias", aliasscope);
            if (tbaa)
                tbaa_decorate(tbaa, load);
            assert(realelty == elty);
            instr = load;
        }
        StoreInst *store = ctx.builder.CreateAlignedStore(r, ptr, Align(alignment));
        store->setOrdering(Order);
        if (aliasscope)
            store->setMetadata("noalias", aliasscope);
        if (tbaa)
            tbaa_decorate(tbaa, store);
    }
    else if (isswapfield && !isboxed) {
        // we can't handle isboxed here as a workaround for really bad LLVM
        // design issue: plain Xchg only works with integers
#if JL_LLVM_VERSION >= 130000
        auto *store = ctx.builder.CreateAtomicRMW(AtomicRMWInst::Xchg, ptr, r, Align(alignment), Order);
#else
        auto *store = ctx.builder.CreateAtomicRMW(AtomicRMWInst::Xchg, ptr, r, Order);
        store->setAlignment(Align(alignment));
#endif
        if (aliasscope)
            store->setMetadata("noalias", aliasscope);
        if (tbaa)
            tbaa_decorate(tbaa, store);
        instr = store;
    }
    else {
        // replacefield, modifyfield, or swapfield (isboxed && atomic)
        DoneBB = BasicBlock::Create(jl_LLVMContext, "done_xchg", ctx.f);
        bool needloop;
        PHINode *Succ = nullptr, *Current = nullptr;
        if (isreplacefield) {
            if (Order == AtomicOrdering::NotAtomic) {
                needloop = false;
            }
            else if (!isboxed) {
                needloop = ((jl_datatype_t*)jltype)->layout->haspadding;
                Value *SameType = emit_isa(ctx, cmp, jltype, nullptr).first;
                if (SameType != ConstantInt::getTrue(jl_LLVMContext)) {
                    BasicBlock *SkipBB = BasicBlock::Create(jl_LLVMContext, "skip_xchg", ctx.f);
                    BasicBlock *BB = BasicBlock::Create(jl_LLVMContext, "ok_xchg", ctx.f);
                    ctx.builder.CreateCondBr(SameType, BB, SkipBB);
                    ctx.builder.SetInsertPoint(SkipBB);
                    LoadInst *load = ctx.builder.CreateAlignedLoad(elty, ptr, Align(alignment));
                    load->setOrdering(FailOrder);
                    if (aliasscope)
                        load->setMetadata("noalias", aliasscope);
                    if (tbaa)
                        tbaa_decorate(tbaa, load);
                    instr = load;
                    ctx.builder.CreateBr(DoneBB);
                    ctx.builder.SetInsertPoint(DoneBB);
                    Succ = ctx.builder.CreatePHI(T_int1, 2);
                    Succ->addIncoming(ConstantInt::get(T_int1, false), SkipBB);
                    Current = ctx.builder.CreatePHI(instr->getType(), 2);
                    Current->addIncoming(instr, SkipBB);
                    ctx.builder.SetInsertPoint(BB);
                }
                Compare = emit_unbox(ctx, realelty, cmp, jltype);
                if (realelty != elty)
                    Compare = ctx.builder.CreateZExt(Compare, elty);
            }
            else if (cmp.isboxed) {
                Compare = boxed(ctx, cmp);
                needloop = !jl_is_mutable_datatype(jltype);
            }
            else {
                Compare = V_rnull;
                needloop = true;
            }
        }
        else { // swap or modify
            LoadInst *Current = ctx.builder.CreateAlignedLoad(elty, ptr, Align(alignment));
            Current->setOrdering(Order == AtomicOrdering::NotAtomic ? Order : AtomicOrdering::Monotonic);
            if (aliasscope)
                Current->setMetadata("noalias", aliasscope);
            if (tbaa)
                tbaa_decorate(tbaa, Current);
            Compare = Current;
            needloop = !isswapfield || Order != AtomicOrdering::NotAtomic;
        }
        BasicBlock *BB = NULL;
        PHINode *CmpPhi = NULL;
        if (needloop) {
            BasicBlock *From = ctx.builder.GetInsertBlock();
            BB = BasicBlock::Create(jl_LLVMContext, "xchg", ctx.f);
            ctx.builder.CreateBr(BB);
            ctx.builder.SetInsertPoint(BB);
            CmpPhi = ctx.builder.CreatePHI(elty, 2);
            CmpPhi->addIncoming(Compare, From);
            Compare = CmpPhi;
        }
        if (ismodifyfield) {
            if (needlock)
                emit_lockstate_value(ctx, parent, false);
            Value *realCompare = Compare;
            if (realelty != elty)
                realCompare = ctx.builder.CreateTrunc(realCompare, realelty);
            if (intcast) {
                ctx.builder.CreateStore(realCompare, ctx.builder.CreateBitCast(intcast, realCompare->getType()->getPointerTo()));
                if (maybe_null_if_boxed)
                    realCompare = ctx.builder.CreateLoad(intcast);
            }
            if (maybe_null_if_boxed) {
                Value *first_ptr = isboxed ? Compare : extract_first_ptr(ctx, Compare);
                if (first_ptr)
                    null_pointer_check(ctx, first_ptr, nullptr);
            }
            if (intcast)
                oldval = mark_julia_slot(intcast, jltype, NULL, tbaa_stack);
            else
                oldval = mark_julia_type(ctx, realCompare, isboxed, jltype);
            rhs = newval(oldval);
            if (!isboxed)
                r = emit_unbox(ctx, realelty, rhs, jltype);
            else
                r = boxed(ctx, rhs);
            if (realelty != elty)
                r = ctx.builder.CreateZExt(r, elty);
            if (needlock)
                emit_lockstate_value(ctx, parent, true);
            cmp = oldval;
        }
        Value *Done;
        if (Order == AtomicOrdering::NotAtomic) {
            // modifyfield or replacefield
            assert(elty == realelty && !intcast);
            auto *load = ctx.builder.CreateAlignedLoad(elty, ptr, Align(alignment));
            if (aliasscope)
                load->setMetadata("noalias", aliasscope);
            if (tbaa)
                tbaa_decorate(tbaa, load);
            Value *first_ptr = nullptr;
            if (maybe_null_if_boxed && !ismodifyfield)
                first_ptr = isboxed ? load : extract_first_ptr(ctx, load);
            oldval = mark_julia_type(ctx, load, isboxed, jltype);
            Success = emit_nullcheck_guard(ctx, first_ptr, [&] {
                return emit_f_is(ctx, oldval, cmp);
            });
            if (needloop && ismodifyfield)
                CmpPhi->addIncoming(load, ctx.builder.GetInsertBlock());
            assert(Succ == nullptr);
            BasicBlock *XchgBB = BasicBlock::Create(jl_LLVMContext, "xchg", ctx.f);
            ctx.builder.CreateCondBr(Success, XchgBB, needloop && ismodifyfield ? BB : DoneBB);
            ctx.builder.SetInsertPoint(XchgBB);
            auto *store = ctx.builder.CreateAlignedStore(r, ptr, Align(alignment));
            if (aliasscope)
                store->setMetadata("noalias", aliasscope);
            if (tbaa)
                tbaa_decorate(tbaa, store);
            ctx.builder.CreateBr(DoneBB);
            instr = load;
        }
        else {
            if (Order == AtomicOrdering::Unordered)
                Order = AtomicOrdering::Monotonic;
            if (!isreplacefield)
                FailOrder = AtomicOrdering::Monotonic;
            else if (FailOrder == AtomicOrdering::Unordered)
                FailOrder = AtomicOrdering::Monotonic;
#if JL_LLVM_VERSION >= 130000
            auto *store = ctx.builder.CreateAtomicCmpXchg(ptr, Compare, r, Align(alignment), Order, FailOrder);
#else
            auto *store = ctx.builder.CreateAtomicCmpXchg(ptr, Compare, r, Order, FailOrder);
            store->setAlignment(Align(alignment));
#endif
            if (aliasscope)
                store->setMetadata("noalias", aliasscope);
            if (tbaa)
                tbaa_decorate(tbaa, store);
            instr = ctx.builder.Insert(ExtractValueInst::Create(store, 0));
            Success = ctx.builder.Insert(ExtractValueInst::Create(store, 1));
            Done = Success;
            if (isreplacefield && needloop) {
                Value *realinstr = instr;
                if (realelty != elty)
                    realinstr = ctx.builder.CreateTrunc(realinstr, realelty);
                if (intcast) {
                    ctx.builder.CreateStore(realinstr, ctx.builder.CreateBitCast(intcast, realinstr->getType()->getPointerTo()));
                    oldval = mark_julia_slot(intcast, jltype, NULL, tbaa_stack);
                    if (maybe_null_if_boxed)
                        realinstr = ctx.builder.CreateLoad(intcast);
                }
                else {
                    oldval = mark_julia_type(ctx, realinstr, isboxed, jltype);
                }
                Done = emit_guarded_test(ctx, ctx.builder.CreateNot(Success), false, [&] {
                    Value *first_ptr = nullptr;
                    if (maybe_null_if_boxed)
                        first_ptr = isboxed ? realinstr : extract_first_ptr(ctx, realinstr);
                    return emit_nullcheck_guard(ctx, first_ptr, [&] {
                        return emit_f_is(ctx, oldval, cmp);
                    });
                });
                Done = ctx.builder.CreateNot(Done);
            }
            if (needloop)
                ctx.builder.CreateCondBr(Done, DoneBB, BB);
            else
                ctx.builder.CreateBr(DoneBB);
            if (needloop)
                CmpPhi->addIncoming(instr, ctx.builder.GetInsertBlock());
        }
        if (Succ != nullptr) {
            Current->addIncoming(instr, ctx.builder.GetInsertBlock());
            instr = Current;
            Succ->addIncoming(Success, ctx.builder.GetInsertBlock());
            Success = Succ;
        }
    }
    if (DoneBB)
        ctx.builder.SetInsertPoint(DoneBB);
    if (needlock)
        emit_lockstate_value(ctx, parent, false);
    if (parent != NULL) {
        if (isreplacefield) {
            // TOOD: avoid this branch if we aren't making a write barrier
            BasicBlock *BB = BasicBlock::Create(jl_LLVMContext, "xchg_wb", ctx.f);
            DoneBB = BasicBlock::Create(jl_LLVMContext, "done_xchg_wb", ctx.f);
            ctx.builder.CreateCondBr(Success, BB, DoneBB);
            ctx.builder.SetInsertPoint(BB);
        }
        if (!isboxed)
            emit_write_multibarrier(ctx, parent, r, rhs.typ);
        else if (!type_is_permalloc(rhs.typ))
            emit_write_barrier(ctx, parent, r);
        if (isreplacefield) {
            ctx.builder.CreateBr(DoneBB);
            ctx.builder.SetInsertPoint(DoneBB);
        }
    }
    if (ismodifyfield) {
        const jl_cgval_t argv[2] = { oldval, rhs };
        jl_datatype_t *rettyp = jl_apply_modify_type(jltype);
        oldval = emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
    }
    else if (!issetfield) { // swapfield or replacefield
        if (realelty != elty)
            instr = ctx.builder.Insert(CastInst::Create(Instruction::Trunc, instr, realelty));
        if (intcast) {
            ctx.builder.CreateStore(instr, ctx.builder.CreateBitCast(intcast, instr->getType()->getPointerTo()));
            instr = ctx.builder.CreateLoad(intcast);
        }
        if (maybe_null_if_boxed) {
            Value *first_ptr = isboxed ? instr : extract_first_ptr(ctx, instr);
            if (first_ptr)
                null_pointer_check(ctx, first_ptr, nullptr);
        }
        oldval = mark_julia_type(ctx, instr, isboxed, jltype);
        if (isreplacefield) {
            Success = ctx.builder.CreateZExt(Success, T_int8);
            const jl_cgval_t argv[2] = {oldval, mark_julia_type(ctx, Success, false, jl_bool_type)};
            jl_datatype_t *rettyp = jl_apply_cmpswap_type(jltype);
            oldval = emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
        }
    }
    return oldval;
}

// --- convert boolean value to julia ---

// Returns T_pjlvalue
static Value *julia_bool(jl_codectx_t &ctx, Value *cond)
{
    return ctx.builder.CreateSelect(cond, literal_pointer_val(ctx, jl_true),
                                          literal_pointer_val(ctx, jl_false));
}

// --- accessing the representations of built-in data types ---

static Constant *julia_const_to_llvm(jl_codectx_t &ctx, jl_value_t *e);

static Value *data_pointer(jl_codectx_t &ctx, const jl_cgval_t &x)
{
    assert(x.ispointer());
    Value *data = x.V;
    if (x.constant) {
        Constant *val = julia_const_to_llvm(ctx, x.constant);
        if (val)
            data = get_pointer_to_constant(ctx.emission_context, val, "_j_const", *jl_Module);
        else
            data = literal_pointer_val(ctx, x.constant);
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
            auto val = tbaa_decorate(tbaa_src, ctx.builder.CreateAlignedLoad(src, Align(align), is_volatile));
            tbaa_decorate(tbaa_dst, ctx.builder.CreateAlignedStore(val, dst, Align(align), is_volatile));
            return;
        }
    }
    // the memcpy intrinsic does not allow to specify different alias tags
    // for the load part (x.tbaa) and the store part (tbaa_stack).
    // since the tbaa lattice has to be a tree we have unfortunately
    // x.tbaa ∪ tbaa_stack = tbaa_root if x.tbaa != tbaa_stack
    ctx.builder.CreateMemCpy(dst, MaybeAlign(align), src, MaybeAlign(0), sz, is_volatile, MDNode::getMostGenericTBAA(tbaa_dst, tbaa_src));
}

static void emit_memcpy_llvm(jl_codectx_t &ctx, Value *dst, MDNode *tbaa_dst, Value *src, MDNode *tbaa_src,
                             Value *sz, unsigned align, bool is_volatile)
{
    if (auto const_sz = dyn_cast<ConstantInt>(sz)) {
        emit_memcpy_llvm(ctx, dst, tbaa_dst, src, tbaa_src, const_sz->getZExtValue(), align, is_volatile);
        return;
    }
    ctx.builder.CreateMemCpy(dst, MaybeAlign(align), src, MaybeAlign(0), sz, is_volatile, MDNode::getMostGenericTBAA(tbaa_dst, tbaa_src));
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


static void emit_atomic_error(jl_codectx_t &ctx, const std::string &msg)
{
    emit_error(ctx, prepare_call(jlatomicerror_func), msg);
}

static jl_cgval_t emit_getfield_knownidx(jl_codectx_t &ctx, const jl_cgval_t &strct,
                                         unsigned idx, jl_datatype_t *jt,
                                         enum jl_memory_order order, Value **nullcheck=nullptr);

static bool emit_getfield_unknownidx(jl_codectx_t &ctx,
        jl_cgval_t *ret, jl_cgval_t strct,
        Value *idx, jl_datatype_t *stt, jl_value_t *inbounds,
        enum jl_memory_order order)
{
    size_t nfields = jl_datatype_nfields(stt);
    bool maybe_null = (unsigned)stt->name->n_uninitialized != 0;
    auto idx0 = [&]() {
        return emit_bounds_check(ctx, strct, (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), inbounds);
    };
    if (nfields == 0) {
        (void)idx0();
        *ret = jl_cgval_t();
        return true;
    }
    if (nfields == 1) {
        if (jl_has_free_typevars(jl_field_type(stt, 0))) {
            return false;
        }
        (void)idx0();
        *ret = emit_getfield_knownidx(ctx, strct, 0, stt, order);
        return true;
    }
    assert(!jl_is_vecelement_type((jl_value_t*)stt));

    if (!strct.ispointer()) { // unboxed
        assert(jl_is_concrete_immutable((jl_value_t*)stt));
        bool isboxed = is_datatype_all_pointers(stt);
        jl_svec_t *types = stt->types;
        bool issame = is_tupletype_homogeneous(types);
        if (issame) {
            jl_value_t *jft = jl_svecref(types, 0);
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
            jl_value_t *jft = issame ? jl_svecref(types, 0) : (jl_value_t*)jl_any_type;
            if (isboxed && maybe_null)
                null_pointer_check(ctx, fld);
            *ret = mark_julia_type(ctx, fld, isboxed, jft);
            return true;
        }
    }

    bool maybeatomic = stt->name->atomicfields != NULL;
    if (strct.ispointer() && !maybeatomic) { // boxed or stack
        if (order != jl_memory_order_notatomic && order != jl_memory_order_unspecified) {
            emit_atomic_error(ctx, "getfield: non-atomic field cannot be accessed atomically");
            *ret = jl_cgval_t(); // unreachable
            return true;
        }
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
                    maybe_decay_tracked(ctx, emit_bitcast(ctx, data_pointer(ctx, strct), T_pprjlvalue)),
                    idx0());
            LoadInst *fld = ctx.builder.CreateAlignedLoad(T_prjlvalue, fldptr, Align(sizeof(void*)));
            fld->setOrdering(AtomicOrdering::Unordered);
            tbaa_decorate(strct.tbaa, fld);
            maybe_mark_load_dereferenceable(fld, maybe_null, minimum_field_size, minimum_align);
            if (maybe_null)
                null_pointer_check(ctx, fld);
            *ret = mark_julia_type(ctx, fld, true, jl_any_type);
            return true;
        }
        else if (is_tupletype_homogeneous(jl_get_fieldtypes(stt))) {
            assert(nfields > 0); // nf == 0 trapped by all_pointers case
            jl_value_t *jft = jl_svecref(stt->types, 0); // n.b. jl_get_fieldtypes assigned stt->types for here
            assert(jl_is_concrete_type(jft));
            idx = idx0();
            Value *ptr = maybe_decay_tracked(ctx, data_pointer(ctx, strct));
            if (!stt->name->mutabl && !(maybe_null && (jft == (jl_value_t*)jl_bool_type ||
                                                 ((jl_datatype_t*)jft)->layout->npointers))) {
                // just compute the pointer and let user load it when necessary
                Type *fty = julia_type_to_llvm(ctx, jft);
                Value *addr = ctx.builder.CreateInBoundsGEP(fty, emit_bitcast(ctx, ptr, PointerType::get(fty, 0)), idx);
                *ret = mark_julia_slot(addr, jft, NULL, strct.tbaa);
                return true;
            }
            *ret = typed_load(ctx, ptr, idx, jft, strct.tbaa, nullptr, false, AtomicOrdering::NotAtomic, maybe_null);
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

static jl_cgval_t emit_unionload(jl_codectx_t &ctx, Value *addr, Value *ptindex, jl_value_t *jfty, size_t fsz, size_t al, MDNode *tbaa, bool mutabl)
{
    Instruction *tindex0 = tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateAlignedLoad(T_int8, ptindex, Align(1)));
    //tindex0->setMetadata(LLVMContext::MD_range, MDNode::get(jl_LLVMContext, {
    //    ConstantAsMetadata::get(ConstantInt::get(T_int8, 0)),
    //    ConstantAsMetadata::get(ConstantInt::get(T_int8, union_max)) }));
    Value *tindex = ctx.builder.CreateNUWAdd(ConstantInt::get(T_int8, 1), tindex0);
    if (mutabl) {
        // move value to an immutable stack slot (excluding tindex)
        Type *ET = IntegerType::get(jl_LLVMContext, 8 * al);
        AllocaInst *lv = emit_static_alloca(ctx, ET);
        lv->setOperand(0, ConstantInt::get(T_int32, (fsz + al - 1) / al));
        emit_memcpy(ctx, lv, tbaa, addr, tbaa, fsz, al);
        addr = lv;
    }
    return mark_julia_slot(addr, jfty, tindex, tbaa);
}

// If `nullcheck` is not NULL and a pointer NULL check is necessary
// store the pointer to be checked in `*nullcheck` instead of checking it
static jl_cgval_t emit_getfield_knownidx(jl_codectx_t &ctx, const jl_cgval_t &strct,
                                         unsigned idx, jl_datatype_t *jt,
                                         enum jl_memory_order order, Value **nullcheck)
{
    jl_value_t *jfty = jl_field_type(jt, idx);
    bool isatomic = jl_field_isatomic(jt, idx);
    bool needlock = isatomic && !jl_field_isptr(jt, idx) && jl_datatype_size(jfty) > MAX_ATOMIC_SIZE;
    if (!isatomic && order != jl_memory_order_notatomic && order != jl_memory_order_unspecified) {
        emit_atomic_error(ctx, "getfield: non-atomic field cannot be accessed atomically");
        return jl_cgval_t(); // unreachable
    }
    if (isatomic && order == jl_memory_order_notatomic) {
        emit_atomic_error(ctx, "getfield: atomic field cannot be accessed non-atomically");
        return jl_cgval_t(); // unreachable
    }
    if (order == jl_memory_order_unspecified) {
        order = isatomic ? jl_memory_order_unordered : jl_memory_order_notatomic;
    }
    if (jfty == jl_bottom_type) {
        raise_exception(ctx, literal_pointer_val(ctx, jl_undefref_exception));
        return jl_cgval_t(); // unreachable
    }
    if (type_is_ghost(julia_type_to_llvm(ctx, jfty)))
        return ghostValue(jfty);
    size_t nfields = jl_datatype_nfields(jt);
    bool maybe_null = idx >= nfields - (unsigned)jt->name->n_uninitialized;
    size_t byte_offset = jl_field_offset(jt, idx);
    auto tbaa = strct.tbaa;
    if (tbaa == tbaa_datatype && byte_offset != offsetof(jl_datatype_t, types))
        tbaa = tbaa_const;
    if (strct.ispointer()) {
        Value *staddr = maybe_decay_tracked(ctx, data_pointer(ctx, strct));
        bool isboxed;
        Type *lt = julia_type_to_llvm(ctx, (jl_value_t*)jt, &isboxed);
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
            LoadInst *Load = ctx.builder.CreateAlignedLoad(T_prjlvalue, maybe_bitcast(ctx, addr, T_pprjlvalue), Align(sizeof(void*)));
            Load->setOrdering(order <= jl_memory_order_notatomic ? AtomicOrdering::Unordered : get_llvm_atomic_order(order));
            maybe_mark_load_dereferenceable(Load, maybe_null, jl_field_type(jt, idx));
            Value *fldv = tbaa_decorate(tbaa, Load);
            if (maybe_null)
                null_pointer_check(ctx, fldv, nullcheck);
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
            return emit_unionload(ctx, addr, ptindex, jfty, fsz, al, tbaa, jt->name->mutabl);
        }
        assert(jl_is_concrete_type(jfty));
        if (!jt->name->mutabl && !(maybe_null && (jfty == (jl_value_t*)jl_bool_type ||
                                            ((jl_datatype_t*)jfty)->layout->npointers))) {
            // just compute the pointer and let user load it when necessary
            return mark_julia_slot(addr, jfty, NULL, tbaa);
        }
        unsigned align = jl_field_align(jt, idx);
        if (needlock)
            emit_lockstate_value(ctx, strct, true);
        jl_cgval_t ret = typed_load(ctx, addr, NULL, jfty, tbaa, nullptr, false,
                needlock ? AtomicOrdering::NotAtomic : get_llvm_atomic_order(order), // TODO: we should use unordered for anything with CountTrackedPointers(elty).count > 0
                maybe_null, align, nullcheck);
        if (needlock)
            emit_lockstate_value(ctx, strct, false);
        return ret;
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
                    ctx.builder.CreateAlignedStore(fldv, fldp, Align(align));
                }
                // emit remaining bytes up to tindex
                if (i < ptindex - st_idx) {
                    Value *staddr = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                    staddr = ctx.builder.CreateBitCast(staddr, T_pint8);
                    for (; i < ptindex - st_idx; i++) {
                        Value *fldv = ctx.builder.CreateExtractValue(obj, makeArrayRef(st_idx + i));
                        Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(T_int8, staddr, i);
                        ctx.builder.CreateAlignedStore(fldv, fldp, Align(1));
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
                null_pointer_check(ctx, first_ptr, nullcheck);
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

static bool arraytype_constdim(jl_value_t *ty, size_t *dim)
{
    if (jl_is_array_type(ty) && jl_is_long(jl_tparam1(ty))) {
        *dim = jl_unbox_long(jl_tparam1(ty));
        return true;
    }
    return false;
}

static bool arraytype_constshape(jl_value_t *ty)
{
    size_t dim;
    if (!arraytype_constdim(ty, &dim))
        return false;
    return dim != 1;
}

static bool arraytype_constelsize(jl_datatype_t *ty, size_t *elsz)
{
    assert(jl_is_array_type(ty));
    jl_value_t *ety = jl_tparam0(ty);
    if (jl_has_free_typevars(ety))
        return false;
    // `jl_islayout_inline` requires `*elsz` and `al` to be initialized.
    size_t al = 0;
    *elsz = 0;
    int union_max = jl_islayout_inline(ety, elsz, &al);
    bool isboxed = (union_max == 0);
    if (isboxed) {
        *elsz = sizeof(void*);
    }
    else if (jl_is_primitivetype(ety)) {
        // Primitive types should use the array element size, but
        // this can be different from the type's size
        *elsz = LLT_ALIGN(*elsz, al);
    }
    return true;
}

static intptr_t arraytype_maxsize(jl_value_t *ty)
{
    if (!jl_is_array_type(ty))
        return INTPTR_MAX;
    size_t elsz;
    if (arraytype_constelsize((jl_datatype_t*)ty, &elsz) || elsz == 0)
        return INTPTR_MAX;
    return INTPTR_MAX / elsz;
}

static Value *emit_arraysize(jl_codectx_t &ctx, const jl_cgval_t &tinfo, Value *dim)
{
    size_t ndim;
    MDNode *tbaa = tbaa_arraysize;
    if (arraytype_constdim(tinfo.typ, &ndim)) {
        if (ndim == 0)
            return ConstantInt::get(T_size, 1);
        if (ndim > 1) {
            if (tinfo.constant && isa<ConstantInt>(dim)) {
                auto n = cast<ConstantInt>(dim)->getZExtValue() - 1;
                return ConstantInt::get(T_size, jl_array_dim(tinfo.constant, n));
            }
            tbaa = tbaa_const;
        }
    }
    Value *t = boxed(ctx, tinfo);
    int o = offsetof(jl_array_t, nrows) / sizeof(void*) - 1;
    auto load = emit_nthptr_recast(ctx,
            t,
            ctx.builder.CreateAdd(dim, ConstantInt::get(dim->getType(), o)),
            tbaa, T_psize);
    MDBuilder MDB(jl_LLVMContext);
    auto rng = MDB.createRange(V_size0, ConstantInt::get(T_size, arraytype_maxsize(tinfo.typ)));
    load->setMetadata(LLVMContext::MD_range, rng);
    return load;
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
    size_t ndim;
    jl_value_t *ty = tinfo.typ;
    MDNode *tbaa = tbaa_arraylen;
    if (arraytype_constdim(ty, &ndim)) {
        if (ndim == 0)
            return ConstantInt::get(T_size, 1);
        if (ndim != 1) {
            if (tinfo.constant)
                return ConstantInt::get(T_size, jl_array_len(tinfo.constant));
            tbaa = tbaa_const;
        }
    }
    Value *t = boxed(ctx, tinfo);
#ifdef STORE_ARRAY_LEN
    Value *addr = ctx.builder.CreateStructGEP(jl_array_llvmt,
            emit_bitcast(ctx, decay_derived(ctx, t), jl_parray_llvmt),
            1); //index (not offset) of length field in jl_parray_llvmt
    LoadInst *len = ctx.builder.CreateAlignedLoad(addr, Align(sizeof(size_t)));
    len->setOrdering(AtomicOrdering::NotAtomic);
    MDBuilder MDB(jl_LLVMContext);
    auto rng = MDB.createRange(V_size0, ConstantInt::get(T_size, arraytype_maxsize(tinfo.typ)));
    len->setMetadata(LLVMContext::MD_range, rng);
    return tbaa_decorate(tbaa, len);
#else
    (void)tbaa;
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
    // Normally allocated array of 0 dimension always have a inline pointer.
    // However, we can't rely on that here since arrays can also be constructed from C pointers.
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
    LoadInst *LI = ctx.builder.CreateAlignedLoad(addr, Align(sizeof(char*)));
    LI->setOrdering(AtomicOrdering::NotAtomic);
    LI->setMetadata(LLVMContext::MD_nonnull, MDNode::get(jl_LLVMContext, None));
    tbaa_decorate(tbaa, LI);
    return LI;
}

static Value *emit_arrayptr(jl_codectx_t &ctx, const jl_cgval_t &tinfo, bool isboxed = false)
{
    Value *t = boxed(ctx, tinfo);
    return emit_arrayptr_internal(ctx, tinfo, decay_derived(ctx, t), AddressSpace::Loaded, isboxed);
}

static Value *emit_unsafe_arrayptr(jl_codectx_t &ctx, const jl_cgval_t &tinfo, bool isboxed = false)
{
    Value *t = boxed(ctx, tinfo);
    t = emit_pointer_from_objref(ctx, decay_derived(ctx, t));
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
            emit_bitcast(ctx, decay_derived(ctx, t), jl_parray_llvmt),
            arrayflag_field);
    return tbaa_decorate(tbaa_arrayflags, ctx.builder.CreateAlignedLoad(T_int16, addr, Align(sizeof(int16_t))));
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
            emit_bitcast(ctx, decay_derived(ctx, t), jl_parray_llvmt),
            elsize_field);
    return tbaa_decorate(tbaa_const, ctx.builder.CreateAlignedLoad(T_int16, addr, Align(sizeof(int16_t))));
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

    Value *addr = ctx.builder.CreateStructGEP(
            jl_array_llvmt,
            emit_bitcast(ctx, decay_derived(ctx, t), jl_parray_llvmt),
            offset_field);
    return tbaa_decorate(tbaa_arrayoffset, ctx.builder.CreateAlignedLoad(T_int32, addr, Align(sizeof(int32_t))));
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
    Value *i = V_size0;
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
    Value *ii = NULL;
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
            ctx.builder.CreateAlignedStore(idxs[k], ctx.builder.CreateInBoundsGEP(T_size, tmp, ConstantInt::get(T_size, k)), Align(sizeof(size_t)));
        }
        ctx.builder.CreateCall(prepare_call(jlboundserrorv_func),
            { mark_callee_rooted(ctx, a), tmp, ConstantInt::get(T_size, nidxs) });
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
        PointerType::get(v->getType(), 0)), Align(alignment)));
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
    else if (const auto *CAZ = dyn_cast<ConstantAggregateZero>(constant)) {
#if JL_LLVM_VERSION >= 130000
        // SVE: Elsewhere we use `getMinKownValue`
        nargs = CAZ->getElementCount().getFixedValue();
#else
        nargs = CAZ->getNumElements();
#endif
    }
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

static Value *call_with_attrs(jl_codectx_t &ctx, JuliaFunction *intr, Value *v)
{
    Function *F = prepare_call(intr);
    CallInst *Call = ctx.builder.CreateCall(F, v);
    Call->setAttributes(F->getAttributes());
    return Call;
}

static void jl_add_method_root(jl_codectx_t &ctx, jl_value_t *val);

static Value *as_value(jl_codectx_t &ctx, Type *to, const jl_cgval_t &v)
{
    assert(!v.isboxed);
    return emit_unbox(ctx, to, v, v.typ);
}

static Value *load_i8box(jl_codectx_t &ctx, Value *v, jl_datatype_t *ty)
{
    auto jvar = ty == jl_int8_type ? jlboxed_int8_cache : jlboxed_uint8_cache;
    Constant *gv = prepare_global_in(jl_Module, jvar);
    Value *idx[] = {ConstantInt::get(T_int32, 0), ctx.builder.CreateZExt(v, T_int32)};
    auto slot = ctx.builder.CreateInBoundsGEP(gv, idx);
    return tbaa_decorate(tbaa_const, maybe_mark_load_dereferenceable(
            ctx.builder.CreateAlignedLoad(T_pjlvalue, slot, Align(sizeof(void*))), false,
            (jl_value_t*)ty));
}

// some types have special boxing functions with small-value caches
// Returns T_prjlvalue
static Value *_boxed_special(jl_codectx_t &ctx, const jl_cgval_t &vinfo, Type *t)
{
    jl_value_t *jt = vinfo.typ;
    if (jt == (jl_value_t*)jl_bool_type)
        return track_pjlvalue(ctx, julia_bool(ctx, ctx.builder.CreateTrunc(as_value(ctx, t, vinfo), T_int1)));
    if (t == T_int1)
        return track_pjlvalue(ctx, julia_bool(ctx, as_value(ctx, t, vinfo)));

    if (ctx.linfo && jl_is_method(ctx.linfo->def.method) && !vinfo.ispointer()) { // don't bother codegen pre-boxing for toplevel
        if (Constant *c = dyn_cast<Constant>(vinfo.V)) {
            jl_value_t *s = static_constant_instance(c, jt);
            if (s) {
                jl_add_method_root(ctx, s);
                return track_pjlvalue(ctx, literal_pointer_val(ctx, s));
            }
        }
    }

    jl_datatype_t *jb = (jl_datatype_t*)jt;
    assert(jl_is_datatype(jb));
    Value *box = NULL;
    if (jb == jl_int8_type)
        box = track_pjlvalue(ctx, load_i8box(ctx, as_value(ctx, t, vinfo), jb));
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
        box = track_pjlvalue(ctx, load_i8box(ctx, as_value(ctx, t, vinfo), jb));
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
        assert(v->getType() == ctx.emission_context.llvmtypes[jl_ssavalue_type]);
        v = ctx.builder.CreateExtractValue(v, makeArrayRef(&zero, 1));
        box = call_with_attrs(ctx, box_ssavalue_func, v);
    }
    else if (!jb->name->abstract && jl_datatype_nbits(jb) == 0) {
        // singleton
        assert(jb->instance != NULL);
        return track_pjlvalue(ctx, literal_pointer_val(ctx, jb->instance));
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
                    Value *cmp = ctx.builder.CreateICmpEQ(track_pjlvalue(ctx, literal_pointer_val(ctx, (jl_value_t*)jt)), datatype);
                    tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(T_int8, idx), tindex);
                }
            },
            ut,
            counter);
    return tindex;
}

// Returns typeof(v), or null if v is a null pointer at run time.
// This is used when the value might have come from an undefined variable,
// yet we try to read its type to compute a union index when moving the value.
static Value *emit_typeof_or_null(jl_codectx_t &ctx, Value *v)
{
    BasicBlock *nonnull = BasicBlock::Create(jl_LLVMContext, "nonnull", ctx.f);
    BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "postnull", ctx.f);
    Value *isnull = ctx.builder.CreateICmpEQ(v, Constant::getNullValue(v->getType()));
    ctx.builder.CreateCondBr(isnull, postBB, nonnull);
    BasicBlock *entry = ctx.builder.GetInsertBlock();
    ctx.builder.SetInsertPoint(nonnull);
    Value *typof = emit_typeof(ctx, v);
    ctx.builder.CreateBr(postBB);
    nonnull = ctx.builder.GetInsertBlock(); // could have changed
    ctx.builder.SetInsertPoint(postBB);
    PHINode *ti = ctx.builder.CreatePHI(typof->getType(), 2);
    ti->addIncoming(Constant::getNullValue(typof->getType()), entry);
    ti->addIncoming(typof, nonnull);
    return ti;
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
    Value *typof;
    if (val.isboxed && !jl_is_concrete_type(val.typ) && !jl_is_type_type(val.typ))
        typof = emit_typeof_or_null(ctx, val.V);
    else
        typof = emit_typeof_boxed(ctx, val);
    return compute_box_tindex(ctx, typof, val.typ, typ);
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
// Returns T_prjlvalue
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
                Type *t = julia_type_to_llvm(ctx, (jl_value_t*)jt);
                BasicBlock *tempBB = BasicBlock::Create(jl_LLVMContext, "box_union", ctx.f);
                ctx.builder.SetInsertPoint(tempBB);
                switchInst->addCase(ConstantInt::get(T_int8, idx), tempBB);
                Value *box;
                if (type_is_ghost(t)) {
                    box = track_pjlvalue(ctx, literal_pointer_val(ctx, jt->instance));
                }
                else {
                    jl_cgval_t vinfo_r = jl_cgval_t(vinfo, (jl_value_t*)jt, NULL);
                    box = _boxed_special(ctx, vinfo_r, t);
                    if (!box) {
                        box = emit_allocobj(ctx, jl_datatype_size(jt), literal_pointer_val(ctx, (jl_value_t*)jt));
                        init_bits_cgval(ctx, box, vinfo_r, jl_is_mutable(jt) ? tbaa_mutab : tbaa_immut);
                    }
                }
                tempBB = ctx.builder.GetInsertBlock(); // could have changed
                box_merge->addIncoming(box, tempBB);
                ctx.builder.CreateBr(postBB);
            },
            vinfo.typ,
            counter);
    ctx.builder.SetInsertPoint(defaultBB);
    if (skip.size() > 0) {
        assert(skip[0]);
        box_merge->addIncoming(V_rnull, defaultBB);
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
// Returns T_prjlvalue
static Value *boxed(jl_codectx_t &ctx, const jl_cgval_t &vinfo)
{
    jl_value_t *jt = vinfo.typ;
    if (jt == jl_bottom_type || jt == NULL)
        // We have an undef value on a (hopefully) dead branch
        return UndefValue::get(T_prjlvalue);
    if (vinfo.constant)
        return track_pjlvalue(ctx, literal_pointer_val(ctx, vinfo.constant));
    // This can happen in early bootstrap for `gc_preserve_begin` return value.
    if (jt == (jl_value_t*)jl_nothing_type)
        return track_pjlvalue(ctx, literal_pointer_val(ctx, jl_nothing));
    if (vinfo.isboxed) {
        assert(vinfo.V == vinfo.Vboxed && vinfo.V != nullptr);
        assert(vinfo.V->getType() == T_prjlvalue);
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
        Type *t = julia_type_to_llvm(ctx, jt);
        assert(!type_is_ghost(t)); // ghost values should have been handled by vinfo.constant above!
        box = _boxed_special(ctx, vinfo, t);
        if (!box) {
            box = emit_allocobj(ctx, jl_datatype_size(jt), literal_pointer_val(ctx, (jl_value_t*)jt));
            init_bits_cgval(ctx, box, vinfo, jl_is_mutable(jt) ? tbaa_mutab : tbaa_immut);
        }
    }
    return box;
}

// copy src to dest, if src is justbits. if skip is true, the value of dest is undefined
static void emit_unionmove(jl_codectx_t &ctx, Value *dest, MDNode *tbaa_dst, const jl_cgval_t &src, Value *skip, bool isVolatile=false)
{
    if (AllocaInst *ai = dyn_cast<AllocaInst>(dest))
        // TODO: make this a lifetime_end & dereferencable annotation?
        ctx.builder.CreateAlignedStore(UndefValue::get(ai->getAllocatedType()), ai, ai->getAlign());
    if (jl_is_concrete_type(src.typ) || src.constant) {
        jl_value_t *typ = src.constant ? jl_typeof(src.constant) : src.typ;
        Type *store_ty = julia_type_to_llvm(ctx, typ);
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
                    nbytes = ctx.builder.CreateSelect(skip, V_size0, nbytes);
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
        ctx.builder.CreateICmpEQ(mark_callee_rooted(ctx, emit_datatype_name(ctx, t)),
                                 mark_callee_rooted(ctx, literal_pointer_val(ctx, (jl_value_t*)jl_pointer_typename)));
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
    // Value *ptls_ptr = emit_bitcast(ctx, get_current_ptls(ctx), T_pint8);
    Value *current_task = get_current_task(ctx);
    Function *F = prepare_call(jl_alloc_obj_func);
    auto call = ctx.builder.CreateCall(F, {current_task, ConstantInt::get(T_size, static_size), maybe_decay_untracked(ctx, jt)});
    call->setAttributes(F->getAttributes());
    return call;
}

// allocation for unknown object from an untracked pointer
static Value *emit_new_bits(jl_codectx_t &ctx, Value *jt, Value *pval)
{
    pval = ctx.builder.CreateBitCast(pval, T_pint8);
    Function *F = prepare_call(jl_newbits_func);
    auto call = ctx.builder.CreateCall(F, { jt, pval });
    call->setAttributes(F->getAttributes());
    return call;
}

// if ptr is NULL this emits a write barrier _back_
static void emit_write_barrier(jl_codectx_t &ctx, Value *parent, Value *ptr)
{
    emit_write_barrier(ctx, parent, makeArrayRef(ptr));
}

static void emit_write_barrier(jl_codectx_t &ctx, Value *parent, ArrayRef<Value*> ptrs)
{
    // if there are no child objects we can skip emission
    if (ptrs.empty())
        return;
    SmallVector<Value*, 8> decay_ptrs;
    decay_ptrs.push_back(maybe_decay_untracked(ctx, emit_bitcast(ctx, parent, T_prjlvalue)));
    for (auto ptr : ptrs) {
        decay_ptrs.push_back(maybe_decay_untracked(ctx, emit_bitcast(ctx, ptr, T_prjlvalue)));
    }
    ctx.builder.CreateCall(prepare_call(jl_write_barrier_func), decay_ptrs);
}

static void find_perm_offsets(jl_datatype_t *typ, SmallVector<unsigned,4> &res, unsigned offset)
{
    // This is a inlined field at `offset`.
    if (!typ->layout || typ->layout->npointers == 0)
        return;
    jl_svec_t *types = jl_get_fieldtypes(typ);
    size_t nf = jl_svec_len(types);
    for (size_t i = 0; i < nf; i++) {
        jl_value_t *_fld = jl_svecref(types, i);
        if (!jl_is_datatype(_fld))
            continue;
        jl_datatype_t *fld = (jl_datatype_t*)_fld;
        if (jl_field_isptr(typ, i)) {
            // pointer field, check if field is perm-alloc
            if (type_is_permalloc((jl_value_t*)fld))
                res.push_back(offset + jl_field_offset(typ, i));
            continue;
        }
        // inline field
        find_perm_offsets(fld, res, offset + jl_field_offset(typ, i));
    }
}

static void emit_write_multibarrier(jl_codectx_t &ctx, Value *parent, Value *agg,
                                    jl_value_t *jltype)
{
    SmallVector<unsigned,4> perm_offsets;
    if (jltype && jl_is_datatype(jltype) && ((jl_datatype_t*)jltype)->layout)
        find_perm_offsets((jl_datatype_t*)jltype, perm_offsets, 0);
    auto ptrs = ExtractTrackedValues(agg, agg->getType(), false, ctx.builder, perm_offsets);
    emit_write_barrier(ctx, parent, ptrs);
}


static jl_cgval_t emit_setfield(jl_codectx_t &ctx,
        jl_datatype_t *sty, const jl_cgval_t &strct, size_t idx0,
        jl_cgval_t rhs, jl_cgval_t cmp,
        bool checked, bool wb, AtomicOrdering Order, AtomicOrdering FailOrder,
        bool needlock, bool issetfield, bool isreplacefield, bool isswapfield, bool ismodifyfield,
        const jl_cgval_t *modifyop, const std::string &fname)
{
    if (!sty->name->mutabl && checked) {
        std::string msg = fname + ": immutable struct of type "
            + std::string(jl_symbol_name(sty->name->name))
            + " cannot be changed";
        emit_error(ctx, msg);
        return jl_cgval_t();
    }
    assert(strct.ispointer());
    size_t byte_offset = jl_field_offset(sty, idx0);
    Value *addr = data_pointer(ctx, strct);
    if (byte_offset > 0) {
        addr = ctx.builder.CreateInBoundsGEP(
                T_int8,
                emit_bitcast(ctx, maybe_decay_tracked(ctx, addr), T_pint8),
                ConstantInt::get(T_size, byte_offset)); // TODO: use emit_struct_gep
    }
    jl_value_t *jfty = jl_field_type(sty, idx0);
    if (!jl_field_isptr(sty, idx0) && jl_is_uniontype(jfty)) {
        size_t fsz = 0, al = 0;
        bool isptr = !jl_islayout_inline(jfty, &fsz, &al);
        assert(!isptr && fsz == jl_field_size(sty, idx0) - 1); (void)isptr;
        // compute tindex from rhs
        jl_cgval_t rhs_union = convert_julia_type(ctx, rhs, jfty);
        if (rhs_union.typ == jl_bottom_type)
            return jl_cgval_t();
        Value *ptindex = ctx.builder.CreateInBoundsGEP(T_int8, emit_bitcast(ctx, maybe_decay_tracked(ctx, addr), T_pint8), ConstantInt::get(T_size, fsz));
        if (needlock)
            emit_lockstate_value(ctx, strct, true);
        BasicBlock *BB = ctx.builder.GetInsertBlock();
        jl_cgval_t oldval = rhs;
        if (!issetfield)
            oldval = emit_unionload(ctx, addr, ptindex, jfty, fsz, al, strct.tbaa, true);
        Value *Success = NULL;
        BasicBlock *DoneBB = NULL;
        if (isreplacefield || ismodifyfield) {
            if (ismodifyfield) {
                if (needlock)
                    emit_lockstate_value(ctx, strct, false);
                const jl_cgval_t argv[3] = { cmp, oldval, rhs };
                if (modifyop) {
                    rhs = emit_invoke(ctx, *modifyop, argv, 3, (jl_value_t*)jl_any_type);
                }
                else {
                    Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, 3, JLCALL_F_CC);
                    rhs = mark_julia_type(ctx, callval, true, jl_any_type);
                }
                if (!jl_subtype(rhs.typ, jfty)) {
                    emit_typecheck(ctx, rhs, jfty, fname);
                    rhs = update_julia_type(ctx, rhs, jfty);
                }
               rhs_union = convert_julia_type(ctx, rhs, jfty);
                if (rhs_union.typ == jl_bottom_type)
                    return jl_cgval_t();
                if (needlock)
                    emit_lockstate_value(ctx, strct, true);
                cmp = oldval;
                oldval = emit_unionload(ctx, addr, ptindex, jfty, fsz, al, strct.tbaa, true);
            }
            BasicBlock *XchgBB = BasicBlock::Create(jl_LLVMContext, "xchg", ctx.f);
            DoneBB = BasicBlock::Create(jl_LLVMContext, "done_xchg", ctx.f);
            Success = emit_f_is(ctx, oldval, cmp);
            ctx.builder.CreateCondBr(Success, XchgBB, ismodifyfield ? BB : DoneBB);
            ctx.builder.SetInsertPoint(XchgBB);
        }
        Value *tindex = compute_tindex_unboxed(ctx, rhs_union, jfty);
        tindex = ctx.builder.CreateNUWSub(tindex, ConstantInt::get(T_int8, 1));
        tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateAlignedStore(tindex, ptindex, Align(1)));
        // copy data
        if (!rhs.isghost) {
            emit_unionmove(ctx, addr, strct.tbaa, rhs, nullptr);
        }
        if (isreplacefield || ismodifyfield) {
            ctx.builder.CreateBr(DoneBB);
            ctx.builder.SetInsertPoint(DoneBB);
        }
        if (needlock)
            emit_lockstate_value(ctx, strct, false);
        if (isreplacefield) {
            Success = ctx.builder.CreateZExt(Success, T_int8);
            jl_cgval_t argv[2] = {oldval, mark_julia_type(ctx, Success, false, jl_bool_type)};
            jl_datatype_t *rettyp = jl_apply_cmpswap_type(jfty);
            oldval = emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
        }
        else if (ismodifyfield) {
            jl_cgval_t argv[2] = {oldval, rhs};
            jl_datatype_t *rettyp = jl_apply_modify_type(jfty);
            oldval = emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
        }
        return oldval;
    }
    else {
        unsigned align = jl_field_align(sty, idx0);
        bool isboxed = jl_field_isptr(sty, idx0);
        size_t nfields = jl_datatype_nfields(sty);
        bool maybe_null = idx0 >= nfields - (unsigned)sty->name->n_uninitialized;
        return typed_store(ctx, addr, NULL, rhs, cmp, jfty, strct.tbaa, nullptr,
            wb ? maybe_bitcast(ctx, data_pointer(ctx, strct), T_pjlvalue) : nullptr,
            isboxed, Order, FailOrder, align,
            needlock, issetfield, isreplacefield, isswapfield, ismodifyfield, maybe_null, modifyop, fname);
    }
}

static jl_cgval_t emit_new_struct(jl_codectx_t &ctx, jl_value_t *ty, size_t nargs, const jl_cgval_t *argv)
{
    assert(jl_is_datatype(ty));
    assert(jl_is_concrete_type(ty));
    jl_datatype_t *sty = (jl_datatype_t*)ty;
    size_t nf = jl_datatype_nfields(sty);
    if (nf > 0 || sty->name->mutabl) {
        if (deserves_stack(ty)) {
            Type *lt = julia_type_to_llvm(ctx, ty);
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
                jl_value_t *jtype = jl_svecref(sty->types, i); // n.b. ty argument must be concrete
                jl_cgval_t fval_info = argv[i];
                emit_typecheck(ctx, fval_info, jtype, "new");
                fval_info = update_julia_type(ctx, fval_info, jtype);
                if (type_is_ghost(lt))
                    continue;
                Type *fty = julia_type_to_llvm(ctx, jtype);
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
                        cast<StoreInst>(tbaa_decorate(tbaa_stack,
                                    ctx.builder.CreateAlignedStore(fval, dest, Align(jl_field_align(sty, i)))))
                                ->setOrdering(AtomicOrdering::Unordered);
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
                                Value *fldv = tbaa_decorate(tbaa_stack, ctx.builder.CreateAlignedLoad(ET, fldp, Align(al)));
                                strct = ctx.builder.CreateInsertValue(strct, fldv, makeArrayRef(llvm_idx + i));
                            }
                            // emit remaining bytes up to tindex
                            if (i < ptindex - llvm_idx) {
                                Value *staddr = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                                staddr = ctx.builder.CreateBitCast(staddr, T_pint8);
                                for (; i < ptindex - llvm_idx; i++) {
                                    Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(T_int8, staddr, i);
                                    Value *fldv = tbaa_decorate(tbaa_stack, ctx.builder.CreateAlignedLoad(T_int8, fldp, Align(1)));
                                    strct = ctx.builder.CreateInsertValue(strct, fldv, makeArrayRef(llvm_idx + i));
                                }
                            }
                        }
                        llvm_idx = ptindex;
                        fval = tindex;
                        if (jl_is_vecelement_type(ty))
                            fval = ctx.builder.CreateInsertValue(strct, fval, makeArrayRef(llvm_idx));
                    }
                    else {
                        Value *ptindex = emit_struct_gep(ctx, lt, strct, offs + fsz);
                        tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateAlignedStore(tindex, ptindex, Align(1)));
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
                        tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateAlignedStore(
                                ConstantInt::get(T_int8, 0),
                                ctx.builder.CreateConstInBoundsGEP2_32(lt, strct, 0, llvm_idx),
                                Align(1)));
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
        strct = decay_derived(ctx, strct);
        undef_derived_strct(ctx.builder, strct, sty, strctinfo.tbaa);
        for (size_t i = nargs; i < nf; i++) {
            if (!jl_field_isptr(sty, i) && jl_is_uniontype(jl_field_type(sty, i))) {
                tbaa_decorate(tbaa_unionselbyte, ctx.builder.CreateAlignedStore(
                        ConstantInt::get(T_int8, 0),
                        ctx.builder.CreateInBoundsGEP(T_int8, emit_bitcast(ctx, strct, T_pint8),
                                ConstantInt::get(T_size, jl_field_offset(sty, i) + jl_field_size(sty, i) - 1)),
                        Align(1)));
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
            emit_typecheck(ctx, rhs, jl_svecref(sty->types, i), "new"); // n.b. ty argument must be concrete
            emit_setfield(ctx, sty, strctinfo, i, rhs, jl_cgval_t(), false, need_wb, AtomicOrdering::NotAtomic, AtomicOrdering::NotAtomic, false, true, false, false, false, nullptr, "");
        }
        return strctinfo;
    }
    else {
        // 0 fields, ghost or bitstype
        if (jl_datatype_nbits(sty) == 0)
            return ghostValue(sty);
        bool isboxed;
        Type *lt = julia_type_to_llvm(ctx, ty, &isboxed);
        assert(!isboxed);
        return mark_julia_type(ctx, UndefValue::get(lt), false, ty);
    }
}

static void emit_signal_fence(jl_codectx_t &ctx)
{
    ctx.builder.CreateFence(AtomicOrdering::SequentiallyConsistent, SyncScope::SingleThread);
}

static Value *emit_defer_signal(jl_codectx_t &ctx)
{
    Value *ptls = emit_bitcast(ctx, get_current_ptls(ctx),
                                        PointerType::get(T_sigatomic, 0));
    Constant *offset = ConstantInt::getSigned(T_int32,
        offsetof(jl_tls_states_t, defer_signal) / sizeof(sig_atomic_t));
    return ctx.builder.CreateInBoundsGEP(T_sigatomic, ptls, ArrayRef<Value*>(offset), "jl_defer_signal");
}

#ifndef JL_NDEBUG
static int compare_cgparams(const jl_cgparams_t *a, const jl_cgparams_t *b)
{
    return
           (a->track_allocations == b->track_allocations) &&
           (a->code_coverage == b->code_coverage) &&
           (a->prefer_specsig == b->prefer_specsig) &&
           (a->gnu_pubnames == b->gnu_pubnames) &&
           (a->debug_info_kind == b->debug_info_kind) &&
           (a->lookup == b->lookup) &&
           (a->generic_context == b->generic_context);
}
#endif
