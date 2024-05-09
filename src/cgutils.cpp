// This file is a part of Julia. License is MIT: https://julialang.org/license

// utility procedures used in code generation

// Mark our stats as being from cgutils
#undef DEBUG_TYPE
#define DEBUG_TYPE "julia_irgen_cgutils"

STATISTIC(EmittedPointerFromObjref, "Number of emitted pointer_from_objref calls");
STATISTIC(EmittedPointerBitcast, "Number of emitted pointer bitcasts");
STATISTIC(EmittedTypeof, "Number of emitted typeof instructions");
STATISTIC(EmittedErrors, "Number of emitted errors");
STATISTIC(EmittedConditionalErrors, "Number of emitted conditional errors");
STATISTIC(EmittedExceptions, "Number of emitted exceptions");
STATISTIC(EmittedConditionalExceptions, "Number of emitted conditional exceptions");
STATISTIC(EmittedNullchecks, "Number of emitted nullchecks");
STATISTIC(EmittedGuards, "Number of emitted guards");
STATISTIC(EmittedIsaUnions, "Number of emitted isa-union checks");
STATISTIC(EmittedIsa, "Number of emitted isa checks");
STATISTIC(EmittedTypechecks, "Number of emitted typechecks");
STATISTIC(EmittedConcretechecks, "Number of emitted concrete checks");
STATISTIC(EmittedBoundschecks, "Number of emitted boundschecks");
STATISTIC(EmittedLockstates, "Number of emitted lockstate value calls");
STATISTIC(EmittedMemcpys, "Number of emitted memcpy instructions");
STATISTIC(SkippedMemcpys, "Number of skipped memcpy instructions");
STATISTIC(EmittedGetfieldUnknowns, "Number of unknown getfield calls emitted");
STATISTIC(EmittedGetfieldKnowns, "Number of known getfield calls emitted");
STATISTIC(EmittedSetfield, "Number of setfield calls emitted");
STATISTIC(EmittedUnionLoads, "Number of union loads emitted");
STATISTIC(EmittedVarargsLength, "Number of varargs length calls emitted");
STATISTIC(EmittedArrayptr, "Number of array ptr calls emitted");
STATISTIC(EmittedArrayElsize, "Number of array elsize calls emitted");
STATISTIC(EmittedArrayNdIndex, "Number of array nd index calls emitted");
STATISTIC(EmittedBoxes, "Number of box operations emitted");
STATISTIC(EmittedCPointerChecks, "Number of C pointer checks emitted");
STATISTIC(EmittedAllocObjs, "Number of object allocations emitted");
STATISTIC(EmittedWriteBarriers, "Number of write barriers emitted");
STATISTIC(EmittedNewStructs, "Number of new structs emitted");
STATISTIC(EmittedDeferSignal, "Number of deferred signals emitted");

static Value *track_pjlvalue(jl_codectx_t &ctx, Value *V)
{
    assert(V->getType() == ctx.types().T_pjlvalue);
    return ctx.builder.CreateAddrSpaceCast(V, ctx.types().T_prjlvalue);
}

// Take an arbitrary untracked value and make it gc-tracked
static Value *maybe_decay_untracked(jl_codectx_t &ctx, Value *V)
{
    if (V->getType() == ctx.types().T_pjlvalue)
        return ctx.builder.CreateAddrSpaceCast(V, ctx.types().T_prjlvalue);
    assert(V->getType() == ctx.types().T_prjlvalue);
    return V;
}

// Take any value and mark that it may be derived from a rooted value
static Value *decay_derived(jl_codectx_t &ctx, Value *V)
{
    Type *T = V->getType();
    if (T->getPointerAddressSpace() == AddressSpace::Derived)
        return V;
    // Once llvm deletes pointer element types, we won't need it here any more either.
    #if JL_LLVM_VERSION >= 170000
    Type *NewT = PointerType::get(T, AddressSpace::Derived);
    #else
    Type *NewT = PointerType::getWithSamePointeeType(cast<PointerType>(T), AddressSpace::Derived);
    #endif
    return ctx.builder.CreateAddrSpaceCast(V, NewT);
}

// Take any value and make it safe to pass to GEP
static Value *maybe_decay_tracked(jl_codectx_t &ctx, Value *V)
{
    Type *T = V->getType();
    if (T->getPointerAddressSpace() != AddressSpace::Tracked)
        return V;
    #if JL_LLVM_VERSION >= 170000
    Type *NewT = PointerType::get(T, AddressSpace::Derived);
    #else
    Type *NewT = PointerType::getWithSamePointeeType(cast<PointerType>(T), AddressSpace::Derived);
    #endif
    return ctx.builder.CreateAddrSpaceCast(V, NewT);
}

static Value *mark_callee_rooted(jl_codectx_t &ctx, Value *V)
{
    assert(V->getType() == ctx.types().T_pjlvalue || V->getType() == ctx.types().T_prjlvalue);
    return ctx.builder.CreateAddrSpaceCast(V,
        PointerType::get(ctx.types().T_jlvalue, AddressSpace::CalleeRooted));
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
        const Twine &txt)
{
    Module *M = jl_builderModule(irbuilder);
    SmallVector<char, 128> ctxt;
    txt.toVector(ctxt);
    // null-terminate the string
    ctxt.push_back(0);
    Constant *Data = ConstantDataArray::get(irbuilder.getContext(), ctxt);
    ctxt.pop_back();
    // We use this for the name of the gv, so cap its size to avoid memory blowout
    if (ctxt.size() > 28) {
        ctxt.resize(28);
        ctxt[25] = ctxt[26] = ctxt[27] = '.';
    }
    // Doesn't need to be aligned, we shouldn't operate on these like julia objects
    GlobalVariable *gv = get_pointer_to_constant(emission_context, Data, Align(1), "_j_str_" + StringRef(ctxt.data(), ctxt.size()), *M);
    Value *zero = ConstantInt::get(Type::getInt32Ty(irbuilder.getContext()), 0);
    Value *Args[] = { zero, zero };
    auto gep = irbuilder.CreateInBoundsGEP(gv->getValueType(),
                                       // AddrSpaceCast in case globals are in non-0 AS
                                       irbuilder.CreateAddrSpaceCast(gv, gv->getValueType()->getPointerTo(0)),
                                       Args);
    setName(emission_context, gep, "string_const_ptr");
    return gep;
}


// --- MDNode ---
Metadata *to_md_tree(jl_value_t *val, LLVMContext &ctxt) {
    if (val == jl_nothing)
        return nullptr;
    Metadata *MD = nullptr;
    if (jl_is_symbol(val)) {
        MD = MDString::get(ctxt, jl_symbol_name((jl_sym_t*)val));
    } else if (jl_is_bool(val)) {
        MD = ConstantAsMetadata::get(ConstantInt::get(getInt1Ty(ctxt), jl_unbox_bool(val)));
    } else if (jl_is_long(val)) {
        MD = ConstantAsMetadata::get(ConstantInt::get(getInt64Ty(ctxt), jl_unbox_long(val)));
    } else if (jl_is_tuple(val)) {
        SmallVector<Metadata *, 8> MDs;
        for (int f = 0, nf = jl_nfields(val); f < nf; ++f) {
            MD = to_md_tree(jl_fieldref(val, f), ctxt);
            if (MD)
                MDs.push_back(MD);
        }
        MD = MDNode::get(ctxt, MDs);
    } else {
        jl_error("LLVM metadata needs to Symbol/Bool/Int or Tuple thereof");
    }
    return MD;
}

// --- Debug info ---

static DICompileUnit *getOrCreateJuliaCU(Module &M,
    DICompileUnit::DebugEmissionKind emissionKind,
    DICompileUnit::DebugNameTableKind tableKind)
{
    // TODO: share debug objects globally in the context, instead of allocating a new one every time
    // or figure out how to delete them afterwards?
    // But at least share them a little bit here
    auto CUs = M.debug_compile_units();
    for (DICompileUnit *CU : CUs) {
        if (CU->getEmissionKind() == emissionKind &&
            CU->getNameTableKind() == tableKind)
        return CU;
    }
    DIFile *topfile = DIFile::get(M.getContext(), "julia", ".");
    DIBuilder dbuilder(M);
    DICompileUnit *CU =
        dbuilder.createCompileUnit(llvm::dwarf::DW_LANG_Julia
                                   ,topfile      // File
                                   ,"julia"      // Producer
                                   ,true         // isOptimized
                                   ,""           // Flags
                                   ,0            // RuntimeVersion
                                   ,""           // SplitName
                                   ,emissionKind // Kind
                                   ,0            // DWOId
                                   ,true         // SplitDebugInlining
                                   ,false        // DebugInfoForProfiling
                                   ,tableKind    // NameTableKind
                                   );
    dbuilder.finalize();
    return CU;
}

static DIType *_julia_type_to_di(jl_codegen_params_t *ctx, jl_debugcache_t &debuginfo, jl_value_t *jt, DIBuilder *dbuilder, bool isboxed)
{
    jl_datatype_t *jdt = (jl_datatype_t*)jt;
    if (isboxed || !jl_is_datatype(jt) || !jdt->isconcretetype)
        return debuginfo.jl_pvalue_dillvmt;
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
    else if (jl_is_structtype(jt) && !jl_is_layout_opaque(jdt->layout) && !jl_is_array_type(jdt)) {
        size_t ntypes = jl_datatype_nfields(jdt);
        SmallVector<llvm::Metadata*, 0> Elements(ntypes);
        for (unsigned i = 0; i < ntypes; i++) {
            jl_value_t *el = jl_field_type_concrete(jdt, i);
            DIType *di;
            if (jl_field_isptr(jdt, i))
                di = debuginfo.jl_pvalue_dillvmt;
            // TODO: elseif jl_islayout_inline
            else
                di = _julia_type_to_di(ctx, debuginfo, el, dbuilder, false);
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
        ditype = dbuilder->createTypedef(debuginfo.jl_pvalue_dillvmt, tname, NULL, 0, NULL);
    }
    return ditype;
}

static DIType *julia_type_to_di(jl_codectx_t &ctx, jl_debugcache_t &debuginfo, jl_value_t *jt, DIBuilder *dbuilder, bool isboxed)
{
    return _julia_type_to_di(&ctx.emission_context, debuginfo, jt, dbuilder, isboxed);
}

void jl_debugcache_t::initialize(Module *m) {
    if (initialized) {
        return;
    }
    initialized = true;
    // add needed base debugging definitions to our LLVM environment
    DIBuilder dbuilder(*m);
    DIFile *julia_h = dbuilder.createFile("julia.h", "");
    DICompositeType *jl_value_dillvmt = dbuilder.createStructType(nullptr,
        "jl_value_t",
        julia_h,
        71, // At the time of this writing. Not sure if it's worth it to keep this in sync
        0 * 8, // sizeof(jl_value_t) * 8,
        __alignof__(void*) * 8, // __alignof__(jl_value_t) * 8,
        DINode::FlagZero, // Flags
        nullptr,    // Derived from
        nullptr);  // Elements - will be corrected later

    jl_pvalue_dillvmt = dbuilder.createPointerType(jl_value_dillvmt, sizeof(jl_value_t*) * 8,
                                                __alignof__(jl_value_t*) * 8);

    SmallVector<llvm::Metadata *, 1> Elts;
    SmallVector<Metadata*, 0> diargs(0);
    Elts.push_back(jl_pvalue_dillvmt);
    dbuilder.replaceArrays(jl_value_dillvmt,
    dbuilder.getOrCreateArray(Elts));

    jl_ppvalue_dillvmt = dbuilder.createPointerType(jl_pvalue_dillvmt, sizeof(jl_value_t**) * 8,
                                                    __alignof__(jl_value_t**) * 8);

    diargs.push_back(jl_pvalue_dillvmt);    // Return Type (ret value)
    diargs.push_back(jl_pvalue_dillvmt);    // First Argument (function)
    diargs.push_back(jl_ppvalue_dillvmt);   // Second Argument (argv)
    // Third argument (length(argv))
    diargs.push_back(_julia_type_to_di(NULL, *this, (jl_value_t*)jl_int32_type, &dbuilder, false));

    jl_di_func_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(diargs));
    jl_di_func_null_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(None));
}

static Value *emit_pointer_from_objref(jl_codectx_t &ctx, Value *V)
{
    unsigned AS = V->getType()->getPointerAddressSpace();
    if (AS != AddressSpace::Tracked && AS != AddressSpace::Derived)
        return V;
    V = decay_derived(ctx, V);
    Type *T = PointerType::get(ctx.types().T_jlvalue, AddressSpace::Derived);
    if (V->getType() != T)
        V = ctx.builder.CreateBitCast(V, T);
    Function *F = prepare_call(pointer_from_objref_func);
    CallInst *Call = ctx.builder.CreateCall(F, V);
    Call->setAttributes(F->getAttributes());
    ++EmittedPointerFromObjref;
    return Call;
}

static Value *emit_unbox(jl_codectx_t &ctx, Type *to, const jl_cgval_t &x, jl_value_t *jt);
static void emit_unbox_store(jl_codectx_t &ctx, const jl_cgval_t &x, Value* dest, MDNode *tbaa_dest, unsigned alignment, bool isVolatile=false);

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


static void find_perm_offsets(jl_datatype_t *typ, SmallVectorImpl<unsigned> &res, unsigned offset)
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

static llvm::SmallVector<llvm::Value*, 0> get_gc_roots_for(jl_codectx_t &ctx, const jl_cgval_t &x)
{
    if (x.constant || x.typ == jl_bottom_type)
        return {};
    if (x.Vboxed) // superset of x.isboxed
        return {x.Vboxed};
    assert(!x.isboxed);
    if (x.ispointer()) {
        assert(x.V);
        assert(x.V->getType()->getPointerAddressSpace() != AddressSpace::Tracked);
        return {x.V};
    }
    else if (jl_is_concrete_immutable(x.typ) && !jl_is_pointerfree(x.typ)) {
        jl_value_t *jltype = x.typ;
        Type *T = julia_type_to_llvm(ctx, jltype);
        Value *agg = emit_unbox(ctx, T, x, jltype);
        SmallVector<unsigned,4> perm_offsets;
        if (jltype && jl_is_datatype(jltype) && ((jl_datatype_t*)jltype)->layout)
            find_perm_offsets((jl_datatype_t*)jltype, perm_offsets, 0);
        return ExtractTrackedValues(agg, agg->getType(), false, ctx.builder, perm_offsets);
    }
    // nothing here to root, move along
    return {};
}

// --- emitting pointers directly into code ---


static inline Constant *literal_static_pointer_val(const void *p, Type *T);

static Constant *julia_pgv(jl_codectx_t &ctx, const char *cname, void *addr)
{
    // emit a GlobalVariable for a jl_value_t named "cname"
    // store the name given so we can reuse it (facilitating merging later)
    // so first see if there already is a GlobalVariable for this address
    GlobalVariable* &gv = ctx.emission_context.global_targets[addr];
    Module *M = jl_Module;
    StringRef localname;
    std::string gvname;
    if (!gv) {
        uint64_t id = jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1); // TODO: use ctx.emission_context.global_targets.size()
        raw_string_ostream(gvname) << cname << id;
        localname = StringRef(gvname);
    }
    else {
        localname = gv->getName();
        if (gv->getParent() != M)
            gv = cast_or_null<GlobalVariable>(M->getNamedValue(localname));
    }
    if (gv == nullptr)
        gv = new GlobalVariable(*M, ctx.types().T_pjlvalue,
                                false, GlobalVariable::ExternalLinkage,
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

static Constant *julia_pgv(jl_codectx_t &ctx, const char *prefix, jl_sym_t *name, jl_module_t *mod, void *addr)
{
    // emit a GlobalVariable for a jl_value_t, using the prefix, name, and module to
    // to create a readable name of the form prefixModA.ModB.name#
    // reverse-of-reverse algorithm
    std::string finalname;
    StringRef name_str(jl_symbol_name(name));
    finalname.resize(name_str.size() + 1);
    finalname[0] = '#';
    std::reverse_copy(name_str.begin(), name_str.end(), finalname.begin() + 1);
    jl_module_t *parent = mod, *prev = NULL;
    while (parent && parent != prev) {
        size_t orig_end = finalname.size() + 1;
        StringRef parent_name(jl_symbol_name(parent->name));
        finalname.resize(orig_end + parent_name.size());
        finalname[orig_end - 1] = '.';
        std::reverse_copy(parent_name.begin(), parent_name.end(), finalname.begin() + orig_end);
        prev = parent;
        parent = parent->parent;
    }
    size_t orig_end = finalname.size();
    StringRef prefix_name(prefix);
    finalname.resize(orig_end + prefix_name.size());
    std::reverse_copy(prefix_name.begin(), prefix_name.end(), finalname.begin() + orig_end);
    std::reverse(finalname.begin(), finalname.end());
    return julia_pgv(ctx, finalname.c_str(), addr);
}

static JuliaVariable *julia_const_gv(jl_value_t *val);
static Constant *literal_pointer_val_slot(jl_codectx_t &ctx, jl_value_t *p)
{
    // emit a pointer to a jl_value_t* which will allow it to be valid across reloading code
    // also, try to give it a nice name for gdb, for easy identification
    if (JuliaVariable *gv = julia_const_gv(p)) {
        // if this is a known special object, use the existing GlobalValue
        return prepare_global_in(jl_Module, gv);
    }
    if (jl_is_datatype(p)) {
        jl_datatype_t *addr = (jl_datatype_t*)p;
        if (addr->smalltag) {
            // some common builtin datatypes have a special pool for accessing them by smalltag id
            Constant *tag = ConstantInt::get(getInt32Ty(ctx.builder.getContext()), addr->smalltag << 4);
            Constant *smallp = ConstantExpr::getInBoundsGetElementPtr(getInt8Ty(ctx.builder.getContext()), prepare_global_in(jl_Module, jl_small_typeof_var), tag);
            auto ty = ctx.types().T_ppjlvalue;
            if (ty->getPointerAddressSpace() == smallp->getType()->getPointerAddressSpace())
                return ConstantExpr::getBitCast(smallp, ty);
            else {
                Constant *newsmallp = ConstantExpr::getAddrSpaceCast(smallp, ty);
                return ConstantExpr::getBitCast(newsmallp, ty);
            }
        }
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
    if (jl_is_datatype(jt) && jl_struct_try_layout((jl_datatype_t*)jt)) {
        return jl_datatype_size(jt);
    }
    return 0;
}

// Return the min required / expected alignment of jltype (on the stack or heap)
static unsigned julia_alignment(jl_value_t *jt)
{
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

static inline void maybe_mark_argument_dereferenceable(AttrBuilder &B, jl_value_t *jt)
{
    B.addAttribute(Attribute::NonNull);
    B.addAttribute(Attribute::NoUndef);
    // The `dereferenceable` below does not imply `nonnull` for non addrspace(0) pointers.
    size_t size = dereferenceable_size(jt);
    if (size) {
        B.addDereferenceableAttr(size);
        B.addAlignmentAttr(julia_alignment(jt));
    }
}

static inline Instruction *maybe_mark_load_dereferenceable(Instruction *LI, bool can_be_null,
                                                           size_t size, size_t align)
{
    if (isa<PointerType>(LI->getType())) {
        if (!can_be_null)
            // The `dereferenceable` below does not imply `nonnull` for non addrspace(0) pointers.
            LI->setMetadata(LLVMContext::MD_nonnull, MDNode::get(LI->getContext(), None));
        if (size) {
            Metadata *OP = ConstantAsMetadata::get(ConstantInt::get(getInt64Ty(LI->getContext()), size));
            LI->setMetadata(can_be_null ? LLVMContext::MD_dereferenceable_or_null : LLVMContext::MD_dereferenceable,
                            MDNode::get(LI->getContext(), { OP }));
            if (align >= 1) {
                Metadata *OP = ConstantAsMetadata::get(ConstantInt::get(getInt64Ty(LI->getContext()), align));
                LI->setMetadata(LLVMContext::MD_align, MDNode::get(LI->getContext(), { OP }));
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

// Returns ctx.types().T_pjlvalue
static Value *literal_pointer_val(jl_codectx_t &ctx, jl_value_t *p)
{
    if (p == NULL)
        return Constant::getNullValue(ctx.types().T_pjlvalue);
    Value *pgv = literal_pointer_val_slot(ctx, p);
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    auto load = ai.decorateInst(maybe_mark_load_dereferenceable(
            ctx.builder.CreateAlignedLoad(ctx.types().T_pjlvalue, pgv, Align(sizeof(void*))),
            false, jl_typeof(p)));
    setName(ctx.emission_context, load, pgv->getName());
    return load;
}

// Returns ctx.types().T_pjlvalue
static Value *literal_pointer_val(jl_codectx_t &ctx, jl_binding_t *p)
{
    // emit a pointer to any jl_value_t which will be valid across reloading code
    if (p == NULL)
        return Constant::getNullValue(ctx.types().T_pjlvalue);
    // bindings are prefixed with jl_bnd#
    jl_globalref_t *gr = p->globalref;
    Value *pgv = gr ? julia_pgv(ctx, "jl_bnd#", gr->name, gr->mod, p) : julia_pgv(ctx, "jl_bnd#", p);
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    auto load = ai.decorateInst(maybe_mark_load_dereferenceable(
            ctx.builder.CreateAlignedLoad(ctx.types().T_pjlvalue, pgv, Align(sizeof(void*))),
            false, sizeof(jl_binding_t), alignof(jl_binding_t)));
    setName(ctx.emission_context, load, pgv->getName());
    return load;
}

// bitcast a value, but preserve its address space when dealing with pointer types
static Value *emit_bitcast(jl_codectx_t &ctx, Value *v, Type *jl_value)
{
    if (isa<PointerType>(jl_value) &&
        v->getType()->getPointerAddressSpace() != jl_value->getPointerAddressSpace()) {
        // Cast to the proper address space
        #if JL_LLVM_VERSION >= 170000
        Type *jl_value_addr = PointerType::get(jl_value, v->getType()->getPointerAddressSpace());
        #else
        Type *jl_value_addr = PointerType::getWithSamePointeeType(cast<PointerType>(jl_value), v->getType()->getPointerAddressSpace());
        #endif
        ++EmittedPointerBitcast;
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

static Value *julia_binding_pvalue(jl_codectx_t &ctx, Value *bv)
{
    bv = emit_bitcast(ctx, bv, ctx.types().T_pprjlvalue);
    Value *offset = ConstantInt::get(ctx.types().T_size, offsetof(jl_binding_t, value) / ctx.types().sizeof_ptr);
    return ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, bv, offset);
}

static Value *julia_binding_gv(jl_codectx_t &ctx, jl_binding_t *b)
{
    // emit a literal_pointer_val to a jl_binding_t
    // binding->value are prefixed with *
    jl_globalref_t *gr = b->globalref;
    Value *pgv = gr ? julia_pgv(ctx, "*", gr->name, gr->mod, b) : julia_pgv(ctx, "*jl_bnd#", b);
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    auto load = ai.decorateInst(ctx.builder.CreateAlignedLoad(ctx.types().T_pjlvalue, pgv, Align(sizeof(void*))));
    setName(ctx.emission_context, load, pgv->getName());
    return load;
}

// --- mapping between julia and llvm types ---

static unsigned convert_struct_offset(const llvm::DataLayout &DL, Type *lty, unsigned byte_offset)
{
    const StructLayout *SL = DL.getStructLayout(cast<StructType>(lty));
    unsigned idx = SL->getElementContainingOffset(byte_offset);
    assert(SL->getElementOffset(idx) == byte_offset);
    return idx;
}

static unsigned convert_struct_offset(jl_codectx_t &ctx, Type *lty, unsigned byte_offset)
{
    return convert_struct_offset(ctx.builder.GetInsertBlock()->getModule()->getDataLayout(), lty, byte_offset);
}

static Value *emit_struct_gep(jl_codectx_t &ctx, Type *lty, Value *base, unsigned byte_offset)
{
    unsigned idx = convert_struct_offset(ctx, lty, byte_offset);
    return ctx.builder.CreateConstInBoundsGEP2_32(lty, base, 0, idx);
}

static Type *_julia_struct_to_llvm(jl_codegen_params_t *ctx, LLVMContext &ctxt, jl_value_t *jt, bool *isboxed, bool llvmcall=false);

static Type *_julia_type_to_llvm(jl_codegen_params_t *ctx, LLVMContext &ctxt, jl_value_t *jt, bool *isboxed)
{
    // this function converts a Julia Type into the equivalent LLVM type
    if (isboxed) *isboxed = false;
    if (jt == (jl_value_t*)jl_bottom_type)
        return getVoidTy(ctxt);
    if (jl_is_concrete_immutable(jt)) {
        if (jl_datatype_nbits(jt) == 0)
            return getVoidTy(ctxt);
        Type *t = _julia_struct_to_llvm(ctx, ctxt, jt, isboxed);
        assert(t != NULL);
        return t;
    }
    if (isboxed) *isboxed = true;
    return JuliaType::get_prjlvalue_ty(ctxt);
}

static Type *julia_type_to_llvm(jl_codectx_t &ctx, jl_value_t *jt, bool *isboxed)
{
    return _julia_type_to_llvm(&ctx.emission_context, ctx.builder.getContext(), jt, isboxed);
}

extern "C" JL_DLLEXPORT_CODEGEN
Type *jl_type_to_llvm_impl(jl_value_t *jt, LLVMContextRef ctxt, bool *isboxed)
{
    return _julia_type_to_llvm(NULL, *unwrap(ctxt), jt, isboxed);
}


// converts a julia bitstype into the equivalent LLVM bitstype
static Type *bitstype_to_llvm(jl_value_t *bt, LLVMContext &ctxt, bool llvmcall = false)
{
    assert(jl_is_primitivetype(bt));
    if (bt == (jl_value_t*)jl_bool_type)
        return llvmcall ? getInt1Ty(ctxt) : getInt8Ty(ctxt);
    if (bt == (jl_value_t*)jl_int32_type)
        return getInt32Ty(ctxt);
    if (bt == (jl_value_t*)jl_int64_type)
        return getInt64Ty(ctxt);
    if (bt == (jl_value_t*)jl_float16_type)
        return getHalfTy(ctxt);
    if (bt == (jl_value_t*)jl_float32_type)
        return getFloatTy(ctxt);
    if (bt == (jl_value_t*)jl_float64_type)
        return getDoubleTy(ctxt);
    if (bt == (jl_value_t*)jl_bfloat16_type)
        return getBFloatTy(ctxt);
    if (jl_is_cpointer_type(bt))
        return PointerType::get(getInt8Ty(ctxt), 0);
    if (jl_is_llvmpointer_type(bt)) {
        jl_value_t *as_param = jl_tparam1(bt);
        int as;
        if (jl_is_int32(as_param))
            as = jl_unbox_int32(as_param);
        else if (jl_is_int64(as_param))
            as = jl_unbox_int64(as_param);
        else
            jl_error("invalid pointer address space");
        return PointerType::get(getInt8Ty(ctxt), as);
    }
    int nb = jl_datatype_size(bt);
    return Type::getIntNTy(ctxt, nb * 8);
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

static llvm::StructType* get_jlmemoryref(llvm::LLVMContext &C, unsigned AS) {
    return llvm::StructType::get(C, {
            llvm::PointerType::get(llvm::Type::getInt8Ty(C), AS),
            JuliaType::get_prjlvalue_ty(C),
            });
}
static llvm::StructType* get_jlmemoryboxedref(llvm::LLVMContext &C, unsigned AS) {
    return llvm::StructType::get(C, {
            llvm::PointerType::get(JuliaType::get_prjlvalue_ty(C), AS),
            JuliaType::get_prjlvalue_ty(C),
            });
}
static llvm::StructType* get_jlmemoryunionref(llvm::LLVMContext &C, llvm::Type *T_size) {
    return llvm::StructType::get(C, {
            T_size, // offset
            JuliaType::get_prjlvalue_ty(C),
            });
}
static StructType *get_memoryref_type(LLVMContext &ctxt, Type *T_size, const jl_datatype_layout_t *layout, unsigned AS)
{
    // TODO: try to remove this slightly odd special case
    bool isboxed = layout->flags.arrayelem_isboxed;
    bool isunion = layout->flags.arrayelem_isunion;
    bool isghost = layout->size == 0;
    if (isboxed)
        return get_jlmemoryboxedref(ctxt, AS);
    if (isunion || isghost)
        return get_jlmemoryunionref(ctxt, T_size);
    return get_jlmemoryref(ctxt, AS);
}

static Type *_julia_struct_to_llvm(jl_codegen_params_t *ctx, LLVMContext &ctxt, jl_value_t *jt, bool *isboxed, bool llvmcall)
{
    // this function converts a Julia Type into the equivalent LLVM struct
    // use this where C-compatible (unboxed) structs are desired
    // use julia_type_to_llvm directly when you want to preserve Julia's type semantics
    if (isboxed) *isboxed = false;
    if (jt == (jl_value_t*)jl_bottom_type)
        return getVoidTy(ctxt);
    if (jl_is_primitivetype(jt))
        return bitstype_to_llvm(jt, ctxt, llvmcall);
    jl_datatype_t *jst = (jl_datatype_t*)jt;
    if (jl_is_structtype(jt) && !(jst->layout && jl_is_layout_opaque(jst->layout)) && !jl_is_array_type(jst) && !jl_is_genericmemory_type(jst)) {
        if (jl_is_genericmemoryref_type(jst)) {
            jl_value_t *mty_dt = jl_field_type_concrete(jst, 1);
            const jl_datatype_layout_t *layout = ((jl_datatype_t*)mty_dt)->layout;
            Type *T_size = bitstype_to_llvm((jl_value_t*)jl_long_type, ctxt);
            return get_memoryref_type(ctxt, T_size, layout, 0);
        }
        bool isTuple = jl_is_tuple_type(jt);
        jl_svec_t *ftypes = jl_get_fieldtypes(jst);
        size_t i, ntypes = jl_svec_len(ftypes);
        if (!jl_struct_try_layout(jst)) {
            assert(0 && "caller should have checked jl_type_mappable_to_c already");
            abort();
        }
        if (ntypes == 0 || jl_datatype_nbits(jst) == 0)
            return getVoidTy(ctxt);
        Type *_struct_decl = NULL;
        // TODO: we should probably make a temporary root for `jst` somewhere
        // don't use pre-filled struct_decl for llvmcall (f16, etc. may be different)
        Type *&struct_decl = (ctx && !llvmcall ? ctx->llvmtypes[jst] : _struct_decl);
        if (struct_decl)
            return struct_decl;
        SmallVector<Type*, 0> latypes(0);
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
                assert(0 && "caller should have checked jl_type_mappable_to_c already");
                abort();
            }
            Type *lty;
            if (jl_field_isptr(jst, i)) {
                lty = JuliaType::get_prjlvalue_ty(ctxt);
                isvector = false;
            }
            else if (jl_is_uniontype(ty)) {
                // pick an Integer type size such that alignment will generally be correct,
                // and always end with an Int8 (selector byte).
                // We may need to insert padding first to get to the right offset
                size_t fsz = 0, al = 0;
                bool isptr = !jl_islayout_inline(ty, &fsz, &al);
                assert(!isptr && fsz < jl_field_size(jst, i)); (void)isptr;
                size_t fsz1 = jl_field_size(jst, i) - 1;
                if (fsz1 > 0) {
                    if (al > MAX_ALIGN) {
                        Type *AlignmentType;
                        AlignmentType = ArrayType::get(FixedVectorType::get(getInt8Ty(ctxt), al), 0);
                        latypes.push_back(AlignmentType);
                        al = MAX_ALIGN;
                    }
                    Type *AlignmentType = IntegerType::get(ctxt, 8 * al);
                    unsigned NumATy = fsz1 / al;
                    unsigned remainder = fsz1 % al;
                    assert(al == 1 || NumATy > 0);
                    while (NumATy--)
                        latypes.push_back(AlignmentType);
                    while (remainder--)
                        latypes.push_back(getInt8Ty(ctxt));
                }
                latypes.push_back(getInt8Ty(ctxt));
                isarray = false;
                allghost = false;
                continue;
            }
            else {
                bool isptr;
                lty = _julia_struct_to_llvm(ctx, ctxt, ty, &isptr, llvmcall);
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
            struct_decl = getVoidTy(ctxt);
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
                struct_decl = StructType::get(ctxt, latypes);
        }
        else {
#if 0 // stress-test code that tries to assume julia-index == llvm-index
      // (also requires change to emit_new_struct to not assume 0 == 0)
            if (!isTuple && latypes.size() > 1) {
                Type *NoopType = ArrayType::get(getInt1Ty(ctxt), 0);
                latypes.insert(latypes.begin(), NoopType);
            }
#endif
            struct_decl = StructType::get(ctxt, latypes);
        }
        return struct_decl;
    }
    // TODO: enable this (with tests) to change ccall calling convention for Union:
    // if (jl_is_uniontype(ty)) {
    //  // pick an Integer type size such that alignment will be correct
    //  // and always end with an Int8 (selector byte)
    //  lty = ArrayType::get(IntegerType::get(lty->getContext(), 8 * al), fsz / al);
    //  SmallVector<Type*, 0> Elements(2);
    //  Elements[0] = lty;
    //  Elements[1] = getInt8Ty(ctxt);
    //  unsigned remainder = fsz % al;
    //  while (remainder--)
    //      Elements.push_back(getInt8Ty(ctxt));
    //  lty = StructType::get(lty->getContext(),ArrayRef<Type*>(Elements));
    // }
    if (isboxed) *isboxed = true;
    return JuliaType::get_prjlvalue_ty(ctxt);
}

static Type *julia_struct_to_llvm(jl_codectx_t &ctx, jl_value_t *jt, bool *isboxed)
{
    return _julia_struct_to_llvm(&ctx.emission_context, ctx.builder.getContext(), jt, isboxed);
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
        llvm::function_ref<void(unsigned, jl_datatype_t*)> f,
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

static Value *emit_typeof(jl_codectx_t &ctx, Value *v, bool maybenull, bool justtag, bool notag=false);
static Value *emit_typeof(jl_codectx_t &ctx, const jl_cgval_t &p, bool maybenull=false, bool justtag=false);

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

static Constant *julia_const_to_llvm(jl_codectx_t &ctx, jl_value_t *e);

static Value *data_pointer(jl_codectx_t &ctx, const jl_cgval_t &x)
{
    assert(x.ispointer());
    Value *data;
    if (x.constant) {
        Constant *val = julia_const_to_llvm(ctx, x.constant);
        if (val)
            data = get_pointer_to_constant(ctx.emission_context, val, Align(julia_alignment(jl_typeof(x.constant))), "_j_const", *jl_Module);
        else
            data = literal_pointer_val(ctx, x.constant);
    }
    else if (x.V == NULL) {
        // might be a ghost union with tindex but no actual pointer
        data = NULL;
    }
    else {
        data = maybe_decay_tracked(ctx, x.V);
    }
    return data;
}

static void emit_memcpy_llvm(jl_codectx_t &ctx, Value *dst, jl_aliasinfo_t const &dst_ai, Value *src,
                             jl_aliasinfo_t const &src_ai, uint64_t sz, unsigned align_dst, unsigned align_src, bool is_volatile)
{
    if (sz == 0)
        return;
    assert(align_dst && "align must be specified");
    #if JL_LLVM_VERSION < 170000
    // If the types are small and simple, use load and store directly.
    // Going through memcpy can cause LLVM (e.g. SROA) to create bitcasts between float and int
    // that interferes with other optimizations.
    // TODO: Restore this for opaque pointers? Needs extra type information from the caller.
    if (ctx.builder.getContext().supportsTypedPointers() && sz <= 64) {
        // The size limit is arbitrary but since we mainly care about floating points and
        // machine size vectors this should be enough.
        const DataLayout &DL = jl_Module->getDataLayout();
        auto srcty = cast<PointerType>(src->getType());
        //TODO unsafe nonopaque pointer
        auto srcel = srcty->getNonOpaquePointerElementType();
        auto dstty = cast<PointerType>(dst->getType());
        //TODO unsafe nonopaque pointer
        auto dstel = dstty->getNonOpaquePointerElementType();
        while (srcel->isArrayTy() && srcel->getArrayNumElements() == 1) {
            src = ctx.builder.CreateConstInBoundsGEP2_32(srcel, src, 0, 0);
            srcel = srcel->getArrayElementType();
            srcty = srcel->getPointerTo();
        }
        while (dstel->isArrayTy() && dstel->getArrayNumElements() == 1) {
            dst = ctx.builder.CreateConstInBoundsGEP2_32(dstel, dst, 0, 0);
            dstel = dstel->getArrayElementType();
            dstty = dstel->getPointerTo();
        }

        llvm::Type *directel = nullptr;
        if (srcel->isSized() && srcel->isSingleValueType() && DL.getTypeStoreSize(srcel) == sz) {
            directel = srcel;
            dst = emit_bitcast(ctx, dst, srcty);
        }
        else if (dstel->isSized() && dstel->isSingleValueType() &&
                DL.getTypeStoreSize(dstel) == sz) {
            directel = dstel;
            src = emit_bitcast(ctx, src, dstty);
        }
        if (directel) {
            if (isa<Instruction>(src) && !src->hasName())
                setName(ctx.emission_context, src, "memcpy_refined_src");
            if (isa<Instruction>(dst) && !dst->hasName())
                setName(ctx.emission_context, dst, "memcpy_refined_dst");
            auto val = src_ai.decorateInst(ctx.builder.CreateAlignedLoad(directel, src, MaybeAlign(align_src), is_volatile));
            dst_ai.decorateInst(ctx.builder.CreateAlignedStore(val, dst, Align(align_dst), is_volatile));
            ++SkippedMemcpys;
            return;
        }
    }
    #endif
    ++EmittedMemcpys;

    // the memcpy intrinsic does not allow to specify different alias tags
    // for the load part (x.tbaa) and the store part (ctx.tbaa().tbaa_stack).
    // since the tbaa lattice has to be a tree we have unfortunately
    // x.tbaa ∪ ctx.tbaa().tbaa_stack = tbaa_root if x.tbaa != ctx.tbaa().tbaa_stack

    // Now that we use scoped aliases to label disparate regions of memory, the TBAA
    // metadata should be revisited so that it only represents memory layouts. Once
    // that's done, we can expect that in most cases tbaa(src) == tbaa(dst) and the
    // above problem won't be as serious.

    auto merged_ai = dst_ai.merge(src_ai);
    ctx.builder.CreateMemCpy(dst, Align(align_dst), src, Align(align_src), sz, is_volatile,
                             merged_ai.tbaa, merged_ai.tbaa_struct, merged_ai.scope, merged_ai.noalias);
}

static void emit_memcpy_llvm(jl_codectx_t &ctx, Value *dst, jl_aliasinfo_t const &dst_ai, Value *src,
                             jl_aliasinfo_t const &src_ai, Value *sz, unsigned align_dst, unsigned align_src, bool is_volatile)
{
    if (auto const_sz = dyn_cast<ConstantInt>(sz)) {
        emit_memcpy_llvm(ctx, dst, dst_ai, src, src_ai, const_sz->getZExtValue(), align_dst, align_src, is_volatile);
        return;
    }
    ++EmittedMemcpys;

    auto merged_ai = dst_ai.merge(src_ai);
    ctx.builder.CreateMemCpy(dst, MaybeAlign(align_dst), src, MaybeAlign(align_src), sz, is_volatile,
                             merged_ai.tbaa, merged_ai.tbaa_struct, merged_ai.scope, merged_ai.noalias);
}

template<typename T1>
static void emit_memcpy(jl_codectx_t &ctx, Value *dst, jl_aliasinfo_t const &dst_ai, Value *src,
                        jl_aliasinfo_t const &src_ai, T1 &&sz, unsigned align_dst, unsigned align_src, bool is_volatile=false)
{
    emit_memcpy_llvm(ctx, dst, dst_ai, src, src_ai, sz, align_dst, align_src, is_volatile);
}

template<typename T1>
static void emit_memcpy(jl_codectx_t &ctx, Value *dst, jl_aliasinfo_t const &dst_ai, const jl_cgval_t &src,
                        T1 &&sz, unsigned align_dst, unsigned align_src, bool is_volatile=false)
{
    auto src_ai = jl_aliasinfo_t::fromTBAA(ctx, src.tbaa);
    emit_memcpy_llvm(ctx, dst, dst_ai, data_pointer(ctx, src), src_ai, sz, align_dst, align_src, is_volatile);
}

static Value *emit_tagfrom(jl_codectx_t &ctx, jl_datatype_t *dt)
{
    if (dt->smalltag)
        return ConstantInt::get(ctx.types().T_size, dt->smalltag << 4);
    auto tag = ctx.builder.CreatePtrToInt(literal_pointer_val(ctx, (jl_value_t*)dt), ctx.types().T_size);
    setName(ctx.emission_context, tag, jl_symbol_name(dt->name->name));
    return tag;
}

// Returns justtag ? ctx.types.T_size : ctx.types().T_prjlvalue
static Value *emit_typeof(jl_codectx_t &ctx, const jl_cgval_t &p, bool maybenull, bool justtag)
{
    // given p, compute its type
    jl_datatype_t *dt = NULL;
    if (p.constant)
        dt = (jl_datatype_t*)jl_typeof(p.constant);
    else if (jl_is_concrete_type(p.typ))
        dt = (jl_datatype_t*)p.typ;
    if (dt) {
        if (justtag)
            return emit_tagfrom(ctx, dt);
        return track_pjlvalue(ctx, literal_pointer_val(ctx, (jl_value_t*)dt));
    }
    auto notag = [justtag] (jl_value_t *typ) {
        // compute if the tag is always a type (not a builtin tag)
        // based on having no intersection with one of the special types
        // this doesn't matter if the user just wants the tag value
        if (justtag)
            return false;
        jl_value_t *uw = jl_unwrap_unionall(typ);
        if (jl_is_datatype(uw)) { // quick path to catch common cases
            jl_datatype_t *dt = (jl_datatype_t*)uw;
            assert(!dt->smalltag);
            if (!dt->name->abstract)
                return true;
            if (dt == jl_any_type)
                return false;
        }
        if (jl_has_intersect_type_not_kind(typ))
            return false;
        for (size_t i = 0; i < jl_tags_count; i++) {
            jl_datatype_t *dt = jl_small_typeof[(i << 4) / sizeof(*jl_small_typeof)];
            if (dt && !jl_has_empty_intersection((jl_value_t*)dt, typ))
                return false;
        }
        return true;
    };
    if (p.isboxed)
        return emit_typeof(ctx, p.V, maybenull, justtag, notag(p.typ));
    if (p.TIndex) {
        Value *tindex = ctx.builder.CreateAnd(p.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x7f));
        bool allunboxed = is_uniontype_allunboxed(p.typ);
        Type *expr_type = justtag ? ctx.types().T_size : ctx.types().T_pjlvalue;
        Value *datatype_or_p = Constant::getNullValue(expr_type->getPointerTo());
        unsigned counter = 0;
        for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                Value *cmp = ctx.builder.CreateICmpEQ(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), idx));
                Constant *ptr;
                if (justtag && jt->smalltag) {
                    ptr = get_pointer_to_constant(ctx.emission_context, ConstantInt::get(expr_type, jt->smalltag << 4), Align(sizeof(jl_value_t*)), StringRef("_j_smalltag_") + jl_symbol_name(jt->name->name), *jl_Module);
                }
                else {
                    ptr = ConstantExpr::getBitCast(literal_pointer_val_slot(ctx, (jl_value_t*)jt), datatype_or_p->getType());
                }
                datatype_or_p = ctx.builder.CreateSelect(cmp, ptr, datatype_or_p);
                setName(ctx.emission_context, datatype_or_p, "typetag_ptr");
            },
            p.typ,
            counter);
        auto emit_unboxty = [&] () -> Value* {
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
            Value *datatype = ai.decorateInst(ctx.builder.CreateAlignedLoad(expr_type, datatype_or_p, Align(sizeof(void*))));
            setName(ctx.emission_context, datatype, "typetag");
            return justtag ? datatype : track_pjlvalue(ctx, datatype);
        };
        Value *res;
        if (!allunboxed) {
            Value *isnull = ctx.builder.CreateIsNull(datatype_or_p);
            setName(ctx.emission_context, isnull, "typetag_isnull");
            BasicBlock *boxBB = BasicBlock::Create(ctx.builder.getContext(), "boxed", ctx.f);
            BasicBlock *unboxBB = BasicBlock::Create(ctx.builder.getContext(), "unboxed", ctx.f);
            BasicBlock *mergeBB = BasicBlock::Create(ctx.builder.getContext(), "merge", ctx.f);
            ctx.builder.CreateCondBr(isnull, boxBB, unboxBB);
            ctx.builder.SetInsertPoint(boxBB);
            auto boxTy = emit_typeof(ctx, p.Vboxed, maybenull, justtag, notag(p.typ));
            ctx.builder.CreateBr(mergeBB);
            boxBB = ctx.builder.GetInsertBlock(); // could have changed
            ctx.builder.SetInsertPoint(unboxBB);
            auto unboxTy = emit_unboxty();
            ctx.builder.CreateBr(mergeBB);
            unboxBB = ctx.builder.GetInsertBlock(); // could have changed
            ctx.builder.SetInsertPoint(mergeBB);
            auto phi = ctx.builder.CreatePHI(boxTy->getType(), 2);
            phi->addIncoming(boxTy, boxBB);
            phi->addIncoming(unboxTy, unboxBB);
            res = phi;
            setName(ctx.emission_context, res, "typetag");
        }
        else {
            res = emit_unboxty();
        }
        return res;
    }
    assert(0 && "what is this struct"); abort();
}

static Value *emit_datatype_types(jl_codectx_t &ctx, Value *dt)
{
    Value *Ptr = emit_bitcast(ctx, decay_derived(ctx, dt), ctx.types().T_ppjlvalue);
    Value *Idx = ConstantInt::get(ctx.types().T_size, offsetof(jl_datatype_t, types) / sizeof(void*));
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    auto types = ai.decorateInst(ctx.builder.CreateAlignedLoad(
                ctx.types().T_pjlvalue, ctx.builder.CreateInBoundsGEP(ctx.types().T_pjlvalue, Ptr, Idx), Align(sizeof(void*))));
    setName(ctx.emission_context, types, "datatype_types");
    return types;
}

static Value *emit_datatype_nfields(jl_codectx_t &ctx, Value *dt)
{
    Value *type_svec = emit_bitcast(ctx, emit_datatype_types(ctx, dt), ctx.types().T_size->getPointerTo());
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    auto nfields = ai.decorateInst(ctx.builder.CreateAlignedLoad(ctx.types().T_size, type_svec, Align(sizeof(void*))));
    setName(ctx.emission_context, nfields, "datatype_nfields");
    return nfields;
}

// emit the size field from the layout of a dt
static Value *emit_datatype_size(jl_codectx_t &ctx, Value *dt, bool add_isunion=false)
{
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    Value *Ptr = emit_bitcast(ctx, decay_derived(ctx, dt), getInt32PtrTy(ctx.builder.getContext())->getPointerTo());
    Value *Idx = ConstantInt::get(ctx.types().T_size, offsetof(jl_datatype_t, layout) / sizeof(int32_t*));
    Ptr = ctx.builder.CreateInBoundsGEP(getInt32PtrTy(ctx.builder.getContext()), Ptr, Idx);
    Ptr = ai.decorateInst(ctx.builder.CreateAlignedLoad(getInt32PtrTy(ctx.builder.getContext()), Ptr, Align(sizeof(int32_t*))));
    Idx = ConstantInt::get(ctx.types().T_size, offsetof(jl_datatype_layout_t, size) / sizeof(int32_t));
    Value *SizePtr = ctx.builder.CreateInBoundsGEP(getInt32Ty(ctx.builder.getContext()), Ptr, Idx);
    Value *Size = ai.decorateInst(ctx.builder.CreateAlignedLoad(getInt32Ty(ctx.builder.getContext()), SizePtr, Align(sizeof(int32_t))));
    setName(ctx.emission_context, Size, "datatype_size");
    if (add_isunion) {
        Idx = ConstantInt::get(ctx.types().T_size, offsetof(jl_datatype_layout_t, flags) / sizeof(int16_t));
        Value *FlagPtr = ctx.builder.CreateInBoundsGEP(getInt16Ty(ctx.builder.getContext()), emit_bitcast(ctx, Ptr, getInt16PtrTy(ctx.builder.getContext())), Idx);
        Value *Flag = ai.decorateInst(ctx.builder.CreateAlignedLoad(getInt16Ty(ctx.builder.getContext()), FlagPtr, Align(sizeof(int16_t))));
        Flag = ctx.builder.CreateLShr(Flag, 4);
        Flag = ctx.builder.CreateAnd(Flag, ConstantInt::get(Flag->getType(), 1));
        Flag = ctx.builder.CreateZExt(Flag, Size->getType());
        Size = ctx.builder.CreateAdd(Size, Flag);
    }
    return Size;
}

/* this is valid code, it's simply unused
static Value *emit_sizeof(jl_codectx_t &ctx, const jl_cgval_t &p)
{
    if (p.TIndex) {
        Value *tindex = ctx.builder.CreateAnd(p.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x7f));
        Value *size = ConstantInt::get(getInt32Ty(ctx.builder.getContext()), -1);
        unsigned counter = 0;
        bool allunboxed = for_each_uniontype_small(
                [&](unsigned idx, jl_datatype_t *jt) {
                    Value *cmp = ctx.builder.CreateICmpEQ(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), idx));
                    size = ctx.builder.CreateSelect(cmp, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), jl_datatype_size(jt)), size);
                },
                p.typ,
                counter);
        if (!allunboxed && p.ispointer() && p.V && !isa<AllocaInst>(p.V)) {
            BasicBlock *currBB = ctx.builder.GetInsertBlock();
            BasicBlock *dynloadBB = BasicBlock::Create(ctx.builder.getContext(), "dyn_sizeof", ctx.f);
            BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_sizeof", ctx.f);
            Value *isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(p.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
                    ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
            ctx.builder.CreateCondBr(isboxed, dynloadBB, postBB);
            ctx.builder.SetInsertPoint(dynloadBB);
            Value *datatype = emit_typeof(ctx, p.V, false, false);
            Value *dyn_size = emit_datatype_size(ctx, datatype);
            ctx.builder.CreateBr(postBB);
            dynloadBB = ctx.builder.GetInsertBlock(); // could have changed
            ctx.builder.SetInsertPoint(postBB);
            PHINode *sizeof_merge = ctx.builder.CreatePHI(getInt32Ty(ctx.builder.getContext()), 2);
            sizeof_merge->addIncoming(dyn_size, dynloadBB);
            sizeof_merge->addIncoming(size, currBB);
            size = sizeof_merge;
        }
#ifndef NDEBUG
        // try to catch codegen errors early, before it uses this to memcpy over the entire stack
        CreateConditionalAbort(ctx.builder, ctx.builder.CreateICmpEQ(size, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), -1)));
#endif
        return size;
    }
    else if (jl_is_concrete_type(p.typ)) {
        return ConstantInt::get(getInt32Ty(ctx.builder.getContext()), jl_datatype_size(p.typ));
    }
    else {
        Value *datatype = emit_typeof(ctx, p, false, false);
        Value *dyn_size = emit_datatype_size(ctx, datatype);
        return dyn_size;
    }
}

static Value *emit_datatype_mutabl(jl_codectx_t &ctx, Value *dt)
{
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    Value *Ptr = emit_bitcast(ctx, decay_derived(ctx, dt), ctx.types().T_ppint8);
    Value *Idx = ConstantInt::get(ctx.types().T_size, offsetof(jl_datatype_t, name));
    Value *Nam = ai.decorateInst(
            ctx.builder.CreateAlignedLoad(getInt8PtrTy(ctx.builder.getContext()), ctx.builder.CreateInBoundsGEP(getInt8PtrTy(ctx.builder.getContext()), Ptr, Idx), Align(sizeof(int8_t*))));
    Value *Idx2 = ConstantInt::get(ctx.types().T_size, offsetof(jl_typename_t, n_uninitialized) + sizeof(((jl_typename_t*)nullptr)->n_uninitialized));
    Value *mutabl = ai.decorateInst(
            ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), Nam, Idx2), Align(1)));
    mutabl = ctx.builder.CreateLShr(mutabl, 1);
    return ctx.builder.CreateTrunc(mutabl, getInt1Ty(ctx.builder.getContext()));
}
*/

static Value *emit_datatype_isprimitivetype(jl_codectx_t &ctx, Value *typ)
{
    Value *isprimitive;
    isprimitive = ctx.builder.CreateConstInBoundsGEP1_32(getInt8Ty(ctx.builder.getContext()), emit_bitcast(ctx, decay_derived(ctx, typ), getInt8PtrTy(ctx.builder.getContext())), offsetof(jl_datatype_t, hash) + sizeof(((jl_datatype_t*)nullptr)->hash));
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    isprimitive = ai.decorateInst(ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), isprimitive, Align(1)));
    isprimitive = ctx.builder.CreateLShr(isprimitive, 7);
    isprimitive = ctx.builder.CreateTrunc(isprimitive, getInt1Ty(ctx.builder.getContext()));
    setName(ctx.emission_context, isprimitive, "datatype_isprimitive");
    return isprimitive;
}

static Value *emit_datatype_name(jl_codectx_t &ctx, Value *dt)
{
    unsigned n = offsetof(jl_datatype_t, name) / sizeof(char*);
    Value *vptr = ctx.builder.CreateInBoundsGEP(
            ctx.types().T_pjlvalue,
            emit_bitcast(ctx, maybe_decay_tracked(ctx, dt), ctx.types().T_ppjlvalue),
            ConstantInt::get(ctx.types().T_size, n));
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    auto name = ai.decorateInst(ctx.builder.CreateAlignedLoad(ctx.types().T_pjlvalue, vptr, Align(sizeof(void*))));
    setName(ctx.emission_context, name, "datatype_name");
    return name;
}

// --- generating various error checks ---
// Do not use conditional throw for cases that type inference can know
// the error is always thrown. This may cause non dominated use
// of SSA value error in the verifier.

static void just_emit_error(jl_codectx_t &ctx, Function *F, const Twine &txt)
{
    ++EmittedErrors;
    ctx.builder.CreateCall(F, stringConstPtr(ctx.emission_context, ctx.builder, txt));
}

static void emit_error(jl_codectx_t &ctx, Function *F, const Twine &txt)
{
    just_emit_error(ctx, F, txt);
    ctx.builder.CreateUnreachable();
    BasicBlock *cont = BasicBlock::Create(ctx.builder.getContext(), "after_error", ctx.f);
    ctx.builder.SetInsertPoint(cont);
}

static void emit_error(jl_codectx_t &ctx, const Twine &txt)
{
    emit_error(ctx, prepare_call(jlerror_func), txt);
}

// DO NOT PASS IN A CONST CONDITION!
static void error_unless(jl_codectx_t &ctx, Value *cond, const Twine &msg)
{
    ++EmittedConditionalErrors;
    BasicBlock *failBB = BasicBlock::Create(ctx.builder.getContext(), "fail", ctx.f);
    BasicBlock *passBB = BasicBlock::Create(ctx.builder.getContext(), "pass");
    ctx.builder.CreateCondBr(cond, passBB, failBB);
    ctx.builder.SetInsertPoint(failBB);
    just_emit_error(ctx, prepare_call(jlerror_func), msg);
    ctx.builder.CreateUnreachable();
    passBB->insertInto(ctx.f);
    ctx.builder.SetInsertPoint(passBB);
}

static void raise_exception(jl_codectx_t &ctx, Value *exc,
                            BasicBlock *contBB=nullptr)
{
    ++EmittedExceptions;
    ctx.builder.CreateCall(prepare_call(jlthrow_func), { mark_callee_rooted(ctx, exc) });
    ctx.builder.CreateUnreachable();
    if (!contBB) {
        contBB = BasicBlock::Create(ctx.builder.getContext(), "after_throw", ctx.f);
    }
    else {
        contBB->insertInto(ctx.f);
    }
    ctx.builder.SetInsertPoint(contBB);
}

// DO NOT PASS IN A CONST CONDITION!
static void raise_exception_unless(jl_codectx_t &ctx, Value *cond, Value *exc)
{
    ++EmittedConditionalExceptions;
    BasicBlock *failBB = BasicBlock::Create(ctx.builder.getContext(),"fail",ctx.f);
    BasicBlock *passBB = BasicBlock::Create(ctx.builder.getContext(),"pass");
    ctx.builder.CreateCondBr(cond, passBB, failBB);
    ctx.builder.SetInsertPoint(failBB);
    raise_exception(ctx, exc, passBB);
}

static void undef_var_error_ifnot(jl_codectx_t &ctx, Value *ok, jl_sym_t *name, jl_value_t *scope)
{
    ++EmittedUndefVarErrors;
    BasicBlock *err = BasicBlock::Create(ctx.builder.getContext(), "err", ctx.f);
    BasicBlock *ifok = BasicBlock::Create(ctx.builder.getContext(), "ok");
    ctx.builder.CreateCondBr(ok, ifok, err);
    ctx.builder.SetInsertPoint(err);
    ctx.builder.CreateCall(prepare_call(jlundefvarerror_func), {
            mark_callee_rooted(ctx, literal_pointer_val(ctx, (jl_value_t*)name)),
            mark_callee_rooted(ctx, literal_pointer_val(ctx, scope))});
    ctx.builder.CreateUnreachable();
    ifok->insertInto(ctx.f);
    ctx.builder.SetInsertPoint(ifok);
}

static Value *null_pointer_cmp(jl_codectx_t &ctx, Value *v)
{
    ++EmittedNullchecks;
    return ctx.builder.CreateIsNotNull(v);
}


// If `nullcheck` is not NULL and a pointer NULL check is necessary
// store the pointer to be checked in `*nullcheck` instead of checking it
static void null_pointer_check(jl_codectx_t &ctx, Value *v, Value **nullcheck)
{
    if (nullcheck) {
        *nullcheck = v;
        return;
    }
    raise_exception_unless(ctx, null_pointer_cmp(ctx, v),
            literal_pointer_val(ctx, jl_undefref_exception));
}


static void null_load_check(jl_codectx_t &ctx, Value *v, jl_module_t *scope, jl_sym_t *name)
{
    Value *notnull = null_pointer_cmp(ctx, v);
    if (name && scope)
        undef_var_error_ifnot(ctx, notnull, name, (jl_value_t*)scope);
    else
        raise_exception_unless(ctx, notnull, literal_pointer_val(ctx, jl_undefref_exception));
}

template<typename Func>
static Value *emit_guarded_test(jl_codectx_t &ctx, Value *ifnot, Value *defval, Func &&func)
{
    if (!ifnot) {
        return func();
    }
    if (auto Cond = dyn_cast<ConstantInt>(ifnot)) {
        if (Cond->isZero())
            return defval;
        return func();
    }
    ++EmittedGuards;
    BasicBlock *currBB = ctx.builder.GetInsertBlock();
    BasicBlock *passBB = BasicBlock::Create(ctx.builder.getContext(), "guard_pass", ctx.f);
    BasicBlock *exitBB = BasicBlock::Create(ctx.builder.getContext(), "guard_exit", ctx.f);
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
    setName(ctx.emission_context, phi, "guard_res");
    return phi;
}

template<typename Func>
static Value *emit_guarded_test(jl_codectx_t &ctx, Value *ifnot, bool defval, Func &&func)
{
    return emit_guarded_test(ctx, ifnot, ConstantInt::get(getInt1Ty(ctx.builder.getContext()), defval), func);
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

// Returns typeof(v), or null if v is a null pointer at run time and maybenull is true.
// This is used when the value might have come from an undefined value (a PhiNode),
// yet jl_max_tags try to read its type to compute a union index when moving the value (a PiNode).
// Returns a ctx.types().T_prjlvalue typed Value
static Value *emit_typeof(jl_codectx_t &ctx, Value *v, bool maybenull, bool justtag, bool notag)
{
    ++EmittedTypeof;
    assert(v != NULL && !isa<AllocaInst>(v) && "expected a conditionally boxed value");
    Value *nonnull = maybenull ? null_pointer_cmp(ctx, v) : ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
    Function *typeof = prepare_call(jl_typeof_func);
    auto val = emit_guarded_test(ctx, nonnull, Constant::getNullValue(justtag ? ctx.types().T_size : typeof->getReturnType()), [&] {
        // e.g. emit_typeof(ctx, v)
        Value *typetag = ctx.builder.CreateCall(typeof, {v});
        if (notag)
            return typetag;
        Value *tag = ctx.builder.CreatePtrToInt(emit_pointer_from_objref(ctx, typetag), ctx.types().T_size);
        if (justtag)
            return tag;
        auto issmall = ctx.builder.CreateICmpULT(tag, ConstantInt::get(tag->getType(), (uintptr_t)jl_max_tags << 4));
        return emit_guarded_test(ctx, issmall, typetag, [&] {
            // we lied a bit: this wasn't really an object (though it was valid for GC rooting)
            // and we need to use it as an index to get the real object now
            Module *M = jl_Module;
            Value *smallp = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), prepare_global_in(M, jl_small_typeof_var), tag);
            smallp = ctx.builder.CreateBitCast(smallp, typetag->getType()->getPointerTo(0));
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
            auto small = ctx.builder.CreateAlignedLoad(typetag->getType(), smallp, M->getDataLayout().getPointerABIAlignment(0));
            small->setMetadata(LLVMContext::MD_nonnull, MDNode::get(M->getContext(), None));
            return ai.decorateInst(small);
        });
    });
    setName(ctx.emission_context, val, "typeof");
    return val;
}

static Value *boxed(jl_codectx_t &ctx, const jl_cgval_t &v,  bool is_promotable=false);

static void just_emit_type_error(jl_codectx_t &ctx, const jl_cgval_t &x, Value *type, const Twine &msg)
{
    Value *msg_val = stringConstPtr(ctx.emission_context, ctx.builder, msg);
    ctx.builder.CreateCall(prepare_call(jltypeerror_func),
                       { msg_val, maybe_decay_untracked(ctx, type), mark_callee_rooted(ctx, boxed(ctx, x))});
}

static void emit_type_error(jl_codectx_t &ctx, const jl_cgval_t &x, Value *type, const Twine &msg)
{
    just_emit_type_error(ctx, x, type, msg);
    ctx.builder.CreateUnreachable();
    BasicBlock *cont = BasicBlock::Create(ctx.builder.getContext(), "after_type_error", ctx.f);
    ctx.builder.SetInsertPoint(cont);
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
    if (type == (jl_value_t*)jl_type_type)
        return true;
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
static Value *emit_exactly_isa(jl_codectx_t &ctx, const jl_cgval_t &arg, jl_datatype_t *dt, bool could_be_null=false)
{
    assert(jl_is_concrete_type((jl_value_t*)dt));
    if (arg.TIndex) {
        unsigned tindex = get_box_tindex(dt, arg.typ);
        if (tindex > 0) {
            // optimize more when we know that this is a split union-type where tindex = 0 is invalid
            Value *xtindex = ctx.builder.CreateAnd(arg.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), ~UNION_BOX_MARKER));
            auto isa = ctx.builder.CreateICmpEQ(xtindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), tindex));
            setName(ctx.emission_context, isa, "exactly_isa");
            return isa;
        }
        else if (arg.Vboxed) {
            // test for (arg.TIndex == UNION_BOX_MARKER && typeof(arg.V) == type)
            Value *isboxed = ctx.builder.CreateICmpEQ(arg.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER));
            if (could_be_null) {
                isboxed = ctx.builder.CreateAnd(isboxed,
                    ctx.builder.CreateNot(null_pointer_cmp(ctx, arg.Vboxed)));
            }
            setName(ctx.emission_context, isboxed, "isboxed");
            BasicBlock *currBB = ctx.builder.GetInsertBlock();
            BasicBlock *isaBB = BasicBlock::Create(ctx.builder.getContext(), "isa", ctx.f);
            BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_isa", ctx.f);
            ctx.builder.CreateCondBr(isboxed, isaBB, postBB);
            ctx.builder.SetInsertPoint(isaBB);
            Value *istype_boxed = ctx.builder.CreateICmpEQ(emit_typeof(ctx, arg.Vboxed, false, true), emit_tagfrom(ctx, dt));
            ctx.builder.CreateBr(postBB);
            isaBB = ctx.builder.GetInsertBlock(); // could have changed
            ctx.builder.SetInsertPoint(postBB);
            PHINode *istype = ctx.builder.CreatePHI(getInt1Ty(ctx.builder.getContext()), 2);
            istype->addIncoming(ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0), currBB);
            istype->addIncoming(istype_boxed, isaBB);
            setName(ctx.emission_context, istype, "exactly_isa");
            return istype;
        } else {
            // handle the case where we know that `arg` is unboxed (but of unknown type), but that concrete type `type` cannot be unboxed
            return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);
        }
    }
    Value *isnull = NULL;
    if (could_be_null && arg.isboxed) {
        isnull = null_pointer_cmp(ctx, arg.Vboxed);
    }
    Constant *Vfalse = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);
    return emit_guarded_test(ctx, isnull, Vfalse, [&]{
        auto isa = ctx.builder.CreateICmpEQ(emit_typeof(ctx, arg, false, true), emit_tagfrom(ctx, dt));
        setName(ctx.emission_context, isa, "exactly_isa");
        return isa;
    });
}

static std::pair<Value*, bool> emit_isa(jl_codectx_t &ctx, const jl_cgval_t &x,
                                        jl_value_t *type, const Twine &msg);

static void emit_isa_union(jl_codectx_t &ctx, const jl_cgval_t &x, jl_value_t *type,
                           SmallVectorImpl<std::pair<std::pair<BasicBlock*,BasicBlock*>,Value*>> &bbs)
{
    ++EmittedIsaUnions;
    if (jl_is_uniontype(type)) {
        emit_isa_union(ctx, x, ((jl_uniontype_t*)type)->a, bbs);
        emit_isa_union(ctx, x, ((jl_uniontype_t*)type)->b, bbs);
        return;
    }
    BasicBlock *enter = ctx.builder.GetInsertBlock();
    Value *v = emit_isa(ctx, x, type, Twine()).first;
    BasicBlock *exit = ctx.builder.GetInsertBlock();
    bbs.emplace_back(std::make_pair(enter, exit), v);
    BasicBlock *isaBB = BasicBlock::Create(ctx.builder.getContext(), "isa", ctx.f);
    ctx.builder.SetInsertPoint(isaBB);
}

// Should agree with `_can_optimize_isa` above
static std::pair<Value*, bool> emit_isa(jl_codectx_t &ctx, const jl_cgval_t &x, jl_value_t *type, const Twine &msg)
{
    ++EmittedIsa;
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
        if (!*known_isa && !msg.isTriviallyEmpty()) {
            emit_type_error(ctx, x, literal_pointer_val(ctx, type), msg);
        }
        return std::make_pair(ConstantInt::get(getInt1Ty(ctx.builder.getContext()), *known_isa), true);
    }

    if (jl_is_type_type(intersected_type) && jl_pointer_egal(intersected_type)) {
        // Use the check in `jl_pointer_egal` to see if the type enclosed
        // has unique pointer value.
        auto ptr = track_pjlvalue(ctx, literal_pointer_val(ctx, jl_tparam0(intersected_type)));
        return {ctx.builder.CreateICmpEQ(boxed(ctx, x), ptr), false};
    }
    if (intersected_type == (jl_value_t*)jl_type_type) {
        // Inline jl_is_kind(jl_typeof(x))
        // N.B. We do the comparison with untracked pointers, because that gives
        // LLVM more optimization opportunities. That means it is possible for
        // `typ` to get GC'ed, but we don't actually care, because we don't ever
        // dereference it.
        Value *typ = emit_typeof(ctx, x, false, true);
        auto val = ctx.builder.CreateOr(
            ctx.builder.CreateOr(
                ctx.builder.CreateICmpEQ(typ, emit_tagfrom(ctx, jl_uniontype_type)),
                ctx.builder.CreateICmpEQ(typ, emit_tagfrom(ctx, jl_datatype_type))),
            ctx.builder.CreateOr(
                ctx.builder.CreateICmpEQ(typ, emit_tagfrom(ctx, jl_unionall_type)),
                ctx.builder.CreateICmpEQ(typ, emit_tagfrom(ctx, jl_typeofbottom_type))));
        setName(ctx.emission_context, val, "is_kind");
        return std::make_pair(val, false);
    }
    // intersection with Type needs to be handled specially
    if (jl_has_intersect_type_not_kind(type) || jl_has_intersect_type_not_kind(intersected_type)) {
        Value *vx = boxed(ctx, x);
        Value *vtyp = track_pjlvalue(ctx, literal_pointer_val(ctx, type));
        if (msg.isSingleStringRef() && msg.getSingleStringRef() == "typeassert") {
            ctx.builder.CreateCall(prepare_call(jltypeassert_func), { vx, vtyp });
            return std::make_pair(ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1), true);
        }
        return std::make_pair(ctx.builder.CreateICmpNE(
                ctx.builder.CreateCall(prepare_call(jlisa_func), { vx, vtyp }),
                ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0)), false);
    }
    // tests for isa concretetype can be handled with pointer comparisons
    if (jl_is_concrete_type(intersected_type)) {
        return std::make_pair(emit_exactly_isa(ctx, x, (jl_datatype_t*)intersected_type), false);
    }
    jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(intersected_type);
    if (jl_is_datatype(dt) && !dt->name->abstract && jl_subtype(dt->name->wrapper, type)) {
        // intersection is a supertype of all instances of its constructor,
        // so the isa test reduces to a comparison of the typename by pointer
        return std::make_pair(
                ctx.builder.CreateICmpEQ(
                    emit_datatype_name(ctx, emit_typeof(ctx, x, false, false)),
                    literal_pointer_val(ctx, (jl_value_t*)dt->name)),
                false);
    }
    if (jl_is_uniontype(intersected_type) &&
        can_optimize_isa_union((jl_uniontype_t*)intersected_type)) {
        SmallVector<std::pair<std::pair<BasicBlock*,BasicBlock*>,Value*>,4> bbs;
        emit_isa_union(ctx, x, intersected_type, bbs);
        int nbbs = bbs.size();
        BasicBlock *currBB = ctx.builder.GetInsertBlock();
        PHINode *res = ctx.builder.CreatePHI(getInt1Ty(ctx.builder.getContext()), nbbs);
        for (int i = 0; i < nbbs; i++) {
            auto bb = bbs[i].first.second;
            ctx.builder.SetInsertPoint(bb);
            if (i + 1 < nbbs) {
                ctx.builder.CreateCondBr(bbs[i].second, currBB, bbs[i + 1].first.first);
                res->addIncoming(ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1), bb);
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
              { emit_typeof(ctx, x, false, false),
                track_pjlvalue(ctx, literal_pointer_val(ctx, type)) }),
            ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0)), false);
}

// If this might have been sourced from a PhiNode object, it is possible our
// Vboxed pointer itself is null (undef) at runtime even if we thought we should
// know exactly the type of the bytes that should have been inside.
//
// n.b. It is also possible the value is a ghost of some sort, and we will
// declare that the pointer is legal (for zero bytes) even though it might be undef.
static Value *emit_isa_and_defined(jl_codectx_t &ctx, const jl_cgval_t &val, jl_value_t *typ)
{
    return emit_nullcheck_guard(ctx, val.ispointer() ? val.V : nullptr, [&] {
        return emit_isa(ctx, val, typ, Twine()).first;
    });
}


static void emit_typecheck(jl_codectx_t &ctx, const jl_cgval_t &x, jl_value_t *type, const Twine &msg)
{
    Value *istype;
    bool handled_msg;
    std::tie(istype, handled_msg) = emit_isa(ctx, x, type, msg);
    if (!handled_msg) {
        ++EmittedTypechecks;
        BasicBlock *failBB = BasicBlock::Create(ctx.builder.getContext(), "fail", ctx.f);
        BasicBlock *passBB = BasicBlock::Create(ctx.builder.getContext(), "pass");
        ctx.builder.CreateCondBr(istype, passBB, failBB);
        ctx.builder.SetInsertPoint(failBB);

        just_emit_type_error(ctx, x, literal_pointer_val(ctx, type), msg);
        ctx.builder.CreateUnreachable();

        passBB->insertInto(ctx.f);
        ctx.builder.SetInsertPoint(passBB);
    }
}

static Value *emit_isconcrete(jl_codectx_t &ctx, Value *typ)
{
    Value *isconcrete;
    isconcrete = ctx.builder.CreateConstInBoundsGEP1_32(getInt8Ty(ctx.builder.getContext()), emit_bitcast(ctx, decay_derived(ctx, typ), getInt8PtrTy(ctx.builder.getContext())), offsetof(jl_datatype_t, hash) + sizeof(((jl_datatype_t*)nullptr)->hash));
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    isconcrete = ai.decorateInst(ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), isconcrete, Align(1)));
    isconcrete = ctx.builder.CreateLShr(isconcrete, 1);
    isconcrete = ctx.builder.CreateTrunc(isconcrete, getInt1Ty(ctx.builder.getContext()));
    setName(ctx.emission_context, isconcrete, "isconcrete");
    return isconcrete;
}

static void emit_concretecheck(jl_codectx_t &ctx, Value *typ, const Twine &msg)
{
    ++EmittedConcretechecks;
    assert(typ->getType() == ctx.types().T_prjlvalue);
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
    Value *im1 = ctx.builder.CreateSub(i, ConstantInt::get(ctx.types().T_size, 1));
    if (bounds_check_enabled(ctx, boundscheck)) {
        ++EmittedBoundschecks;
        Value *ok = ctx.builder.CreateICmpULT(im1, len);
        setName(ctx.emission_context, ok, "boundscheck");
        BasicBlock *failBB = BasicBlock::Create(ctx.builder.getContext(), "fail", ctx.f);
        BasicBlock *passBB = BasicBlock::Create(ctx.builder.getContext(), "pass");
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
                a = Constant::getNullValue(getInt8PtrTy(ctx.builder.getContext()));
            }
            else if (!ainfo.ispointer()) {
                // CreateAlloca is OK here since we are on an error branch
                Value *tempSpace = ctx.builder.CreateAlloca(a->getType());
                setName(ctx.emission_context, tempSpace, "errorbox");
                ctx.builder.CreateStore(a, tempSpace);
                a = tempSpace;
            }
            ctx.builder.CreateCall(prepare_call(jluboundserror_func), {
                    emit_bitcast(ctx, decay_derived(ctx, a), getInt8PtrTy(ctx.builder.getContext())),
                    literal_pointer_val(ctx, ty),
                    i });
        }
        ctx.builder.CreateUnreachable();
        passBB->insertInto(ctx.f);
        ctx.builder.SetInsertPoint(passBB);
    }
    return im1;
}

static Value *CreateSimplifiedExtractValue(jl_codectx_t &ctx, Value *Agg, ArrayRef<unsigned> Idxs)
{
    // aka IRBuilder<InstSimplifyFolder>
    SimplifyQuery SQ(jl_Module->getDataLayout()); // not actually used, but required by API
    if (Value *Inst = simplifyExtractValueInst(Agg, Idxs, SQ))
        return Inst;
    return ctx.builder.CreateExtractValue(Agg, Idxs);
}

static void emit_write_barrier(jl_codectx_t&, Value*, ArrayRef<Value*>);
static void emit_write_barrier(jl_codectx_t&, Value*, Value*);
static void emit_write_multibarrier(jl_codectx_t&, Value*, Value*, jl_value_t*);

SmallVector<unsigned, 0> first_ptr(Type *T)
{
    if (isa<StructType>(T) || isa<ArrayType>(T) || isa<VectorType>(T)) {
        if (!isa<StructType>(T)) {
            uint64_t num_elements;
            if (auto *AT = dyn_cast<ArrayType>(T))
                num_elements = AT->getNumElements();
            else {
                VectorType *VT = cast<VectorType>(T);
                ElementCount EC = VT->getElementCount();
                num_elements = EC.getKnownMinValue();
            }
            if (num_elements == 0)
                return {};
        }
        unsigned i = 0;
        for (Type *ElTy : T->subtypes()) {
            if (isa<PointerType>(ElTy) && ElTy->getPointerAddressSpace() == AddressSpace::Tracked) {
                return SmallVector<unsigned, 0>{i};
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
    return CreateSimplifiedExtractValue(ctx, V, path);
}


static void emit_lockstate_value(jl_codectx_t &ctx, Value *strct, bool newstate)
{
    ++EmittedLockstates;
    if (strct->getType()->getPointerAddressSpace() == AddressSpace::Loaded) {
        Value *v = emit_bitcast(ctx, strct, PointerType::get(ctx.types().T_jlvalue, AddressSpace::Loaded));
        ctx.builder.CreateCall(prepare_call(newstate ? jllockfield_func : jlunlockfield_func), v);
    }
    else {
        Value *v = mark_callee_rooted(ctx, strct);
        ctx.builder.CreateCall(prepare_call(newstate ? jllockvalue_func : jlunlockvalue_func), v);
    }
}

// If `nullcheck` is not NULL and a pointer NULL check is necessary
// store the pointer to be checked in `*nullcheck` instead of checking it
static jl_cgval_t typed_load(jl_codectx_t &ctx, Value *ptr, Value *idx_0based, jl_value_t *jltype,
                             MDNode *tbaa, MDNode *aliasscope, bool isboxed, AtomicOrdering Order,
                             bool maybe_null_if_boxed = true, unsigned alignment = 0,
                             Value **nullcheck = nullptr)
{
    // TODO: we should use unordered loads for anything with CountTrackedPointers(elty).count > 0 (if not otherwise locked)
    Type *elty = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, jltype);
    if (type_is_ghost(elty)) {
        if (isStrongerThanMonotonic(Order))
            ctx.builder.CreateFence(Order);
        return ghostValue(ctx, jltype);
    }
    unsigned nb = isboxed ? sizeof(void*) : jl_datatype_size(jltype);
    // note that nb == jl_Module->getDataLayout().getTypeAllocSize(elty) or getTypeStoreSize, depending on whether it is a struct or primitive type
    AllocaInst *intcast = NULL;
    if (Order == AtomicOrdering::NotAtomic) {
        if (!isboxed && !aliasscope && elty->isAggregateType() && !CountTrackedPointers(elty).count) {
            intcast = emit_static_alloca(ctx, elty);
            setName(ctx.emission_context, intcast, "aggregate_load_box");
        }
    }
    else {
        if (!isboxed && !elty->isIntOrPtrTy()) {
            intcast = emit_static_alloca(ctx, elty);
            setName(ctx.emission_context, intcast, "atomic_load_box");
            elty = Type::getIntNTy(ctx.builder.getContext(), 8 * nb);
        }
    }
    Type *realelty = elty;
    if (Order != AtomicOrdering::NotAtomic && isa<IntegerType>(elty)) {
        unsigned nb2 = PowerOf2Ceil(nb);
        if (nb != nb2)
            elty = Type::getIntNTy(ctx.builder.getContext(), 8 * nb2);
    }
    Type *ptrty = PointerType::get(elty, ptr->getType()->getPointerAddressSpace());
    Value *data;
    if (ptr->getType() != ptrty)
        data = emit_bitcast(ctx, ptr, ptrty);
    else
        data = ptr;
    if (idx_0based)
        data = ctx.builder.CreateInBoundsGEP(elty, data, idx_0based);
    Value *instr = nullptr;
    if (isboxed)
        alignment = sizeof(void*);
    else if (!alignment)
        alignment = julia_alignment(jltype);
    if (intcast && Order == AtomicOrdering::NotAtomic) {
        emit_memcpy(ctx, intcast, jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack), data, jl_aliasinfo_t::fromTBAA(ctx, tbaa), nb, alignment, intcast->getAlign().value());
    }
    else {
        if (!isboxed && jl_is_genericmemoryref_type(jltype)) {
            // load these FCA as individual fields, so LLVM does not need to split them later
            Value *fld0 = ctx.builder.CreateStructGEP(elty, data, 0);
            LoadInst *load0 = ctx.builder.CreateAlignedLoad(elty->getStructElementType(0), fld0, Align(alignment), false);
            load0->setOrdering(Order);
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            ai.scope = MDNode::concatenate(aliasscope, ai.scope);
            ai.decorateInst(load0);
            Value *fld1 = ctx.builder.CreateStructGEP(elty, data, 1);
            LoadInst *load1 = ctx.builder.CreateAlignedLoad(elty->getStructElementType(1), fld1, Align(alignment), false);
            static_assert(offsetof(jl_genericmemoryref_t, ptr_or_offset) == 0, "wrong field order");
            maybe_mark_load_dereferenceable(load1, true, sizeof(void*)*2, alignof(void*));
            load1->setOrdering(Order);
            ai.decorateInst(load1);
            instr = Constant::getNullValue(elty);
            instr = ctx.builder.CreateInsertValue(instr, load0, 0);
            instr = ctx.builder.CreateInsertValue(instr, load1, 1);
        }
        else {
            LoadInst *load = ctx.builder.CreateAlignedLoad(elty, data, Align(alignment), false);
            load->setOrdering(Order);
            if (isboxed)
                maybe_mark_load_dereferenceable(load, true, jltype);
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            ai.scope = MDNode::concatenate(aliasscope, ai.scope);
            ai.decorateInst(load);
            instr = load;
        }
        if (elty != realelty)
            instr = ctx.builder.CreateTrunc(instr, realelty);
        if (intcast) {
            ctx.builder.CreateStore(instr, ctx.builder.CreateBitCast(intcast, instr->getType()->getPointerTo()));
            instr = nullptr;
        }
    }
    if (maybe_null_if_boxed) {
        if (intcast)
            instr = ctx.builder.CreateLoad(intcast->getAllocatedType(), intcast);
        Value *first_ptr = isboxed ? instr : extract_first_ptr(ctx, instr);
        if (first_ptr)
            null_pointer_check(ctx, first_ptr, nullcheck);
        if (intcast && !first_ptr)
            instr = nullptr;
    }
    if (jltype == (jl_value_t*)jl_bool_type) { // "freeze" undef memory to a valid value
        // NOTE: if we zero-initialize arrays, this optimization should become valid
        //load->setMetadata(LLVMContext::MD_range, MDNode::get(ctx.builder.getContext(), {
        //    ConstantAsMetadata::get(ConstantInt::get(T_int8, 0)),
        //    ConstantAsMetadata::get(ConstantInt::get(T_int8, 2)) }));
        if (intcast)
            instr = ctx.builder.CreateLoad(intcast->getAllocatedType(), intcast);
        instr = ctx.builder.CreateTrunc(instr, getInt1Ty(ctx.builder.getContext()));
    }
    if (instr)
        return mark_julia_type(ctx, instr, isboxed, jltype);
    else
        return mark_julia_slot(intcast, jltype, NULL, ctx.tbaa().tbaa_stack);
}

static jl_cgval_t typed_store(jl_codectx_t &ctx,
        Value *ptr, jl_cgval_t rhs, jl_cgval_t cmp,
        jl_value_t *jltype, MDNode *tbaa, MDNode *aliasscope,
        Value *parent,  // for the write barrier, NULL if no barrier needed
        bool isboxed, AtomicOrdering Order, AtomicOrdering FailOrder, unsigned alignment,
        Value *needlock, bool issetfield, bool isreplacefield, bool isswapfield, bool ismodifyfield, bool issetfieldonce,
        bool maybe_null_if_boxed, const jl_cgval_t *modifyop, const Twine &fname,
        jl_module_t *mod, jl_sym_t *var)
{
    auto newval = [&](const jl_cgval_t &lhs) {
        const jl_cgval_t argv[3] = { cmp, lhs, rhs };
        jl_cgval_t ret;
        if (modifyop) {
            ret = emit_invoke(ctx, *modifyop, argv, 3, (jl_value_t*)jl_any_type);
        }
        else {
            Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, 3, julia_call);
            ret = mark_julia_type(ctx, callval, true, jl_any_type);
        }
        emit_typecheck(ctx, ret, jltype, fname);
        ret = update_julia_type(ctx, ret, jltype);
        return ret;
    };
    Type *elty = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, jltype);
    if (type_is_ghost(elty) ||
            (issetfieldonce && !maybe_null_if_boxed) ||
            (issetfieldonce && !isboxed && !jl_type_hasptr(jltype))) {
        if (isStrongerThanMonotonic(Order))
            ctx.builder.CreateFence(Order);
        if (issetfield) {
            return rhs;
        }
        else if (isreplacefield) {
            Value *Success = emit_f_is(ctx, cmp, ghostValue(ctx, jltype));
            Success = ctx.builder.CreateZExt(Success, getInt8Ty(ctx.builder.getContext()));
            const jl_cgval_t argv[2] = {ghostValue(ctx, jltype), mark_julia_type(ctx, Success, false, jl_bool_type)};
            jl_datatype_t *rettyp = jl_apply_cmpswap_type(jltype);
            return emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
        }
        else if (isswapfield) {
            return ghostValue(ctx, jltype);
        }
        else if (ismodifyfield) {
            jl_cgval_t oldval = ghostValue(ctx, jltype);
            const jl_cgval_t argv[2] = { oldval, newval(oldval) };
            jl_datatype_t *rettyp = jl_apply_modify_type(jltype);
            return emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
        }
        else { // issetfieldonce
            return mark_julia_const(ctx, jl_false);
        }
    }
    // if FailOrder was inherited from Order, may need to remove Load-only effects now
    if (FailOrder == AtomicOrdering::AcquireRelease)
        FailOrder = AtomicOrdering::Acquire;
    if (FailOrder == AtomicOrdering::Release)
        FailOrder = AtomicOrdering::Monotonic;
    unsigned nb = isboxed ? sizeof(void*) : jl_datatype_size(jltype);
    AllocaInst *intcast = nullptr;
    if (!isboxed && Order != AtomicOrdering::NotAtomic && !elty->isIntOrPtrTy()) {
        if (!issetfield) {
            intcast = emit_static_alloca(ctx, elty);
            setName(ctx.emission_context, intcast, "atomic_store_box");
        }
        elty = Type::getIntNTy(ctx.builder.getContext(), 8 * nb);
    }
    Type *realelty = elty;
    if (Order != AtomicOrdering::NotAtomic && isa<IntegerType>(elty)) {
        unsigned nb2 = PowerOf2Ceil(nb);
        if (nb != nb2)
            elty = Type::getIntNTy(ctx.builder.getContext(), 8 * nb2);
    }
    Value *r = nullptr;
    if (issetfield || isswapfield || isreplacefield || issetfieldonce)  {
        if (isboxed)
            r = boxed(ctx, rhs);
        else if (aliasscope || Order != AtomicOrdering::NotAtomic || CountTrackedPointers(realelty).count) {
            r = emit_unbox(ctx, realelty, rhs, jltype);
            if (realelty != elty)
                r = ctx.builder.CreateZExt(r, elty);
        }
    }
    Type *ptrty = PointerType::get(elty, ptr->getType()->getPointerAddressSpace());
    if (ptr->getType() != ptrty)
        ptr = ctx.builder.CreateBitCast(ptr, ptrty);
    if (isboxed)
        alignment = sizeof(void*);
    else if (!alignment)
        alignment = julia_alignment(jltype);
    Value *instr = nullptr;
    Value *Compare = nullptr;
    Value *Success = nullptr;
    BasicBlock *DoneBB = nullptr;
    if (needlock)
        emit_lockstate_value(ctx, needlock, true);
    jl_cgval_t oldval = rhs;
    // TODO: we should do Release ordering for anything with CountTrackedPointers(elty).count > 0, instead of just isboxed
    if (issetfield || (Order == AtomicOrdering::NotAtomic && isswapfield)) {
        if (isswapfield) {
            auto *load = ctx.builder.CreateAlignedLoad(elty, ptr, Align(alignment));
            setName(ctx.emission_context, load, "swap_load");
            if (isboxed)
                load->setOrdering(AtomicOrdering::Unordered);
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            ai.noalias = MDNode::concatenate(aliasscope, ai.noalias);
            ai.decorateInst(load);
            assert(realelty == elty);
            instr = load;
        }
        if (r) {
            StoreInst *store = ctx.builder.CreateAlignedStore(r, ptr, Align(alignment));
            store->setOrdering(Order == AtomicOrdering::NotAtomic && isboxed ? AtomicOrdering::Release : Order);
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            ai.noalias = MDNode::concatenate(aliasscope, ai.noalias);
            ai.decorateInst(store);
        }
        else {
            assert(Order == AtomicOrdering::NotAtomic && !isboxed && rhs.typ == jltype);
            emit_unbox_store(ctx, rhs, ptr, tbaa, alignment);
        }
    }
    else if (isswapfield) {
        if (Order == AtomicOrdering::Unordered)
            Order = AtomicOrdering::Monotonic;
        assert(Order != AtomicOrdering::NotAtomic && r);
        auto *store = ctx.builder.CreateAtomicRMW(AtomicRMWInst::Xchg, ptr, r, Align(alignment), Order);
        setName(ctx.emission_context, store, "swap_atomicrmw");
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
        ai.noalias = MDNode::concatenate(aliasscope, ai.noalias);
        ai.decorateInst(store);
        instr = store;
    }
    else {
        // replacefield, modifyfield, swapfield, setfieldonce (isboxed && atomic)
        DoneBB = BasicBlock::Create(ctx.builder.getContext(), "done_xchg", ctx.f);
        bool needloop;
        PHINode *Succ = nullptr, *Current = nullptr;
        if (isreplacefield) {
            if (Order == AtomicOrdering::NotAtomic) {
                needloop = false;
            }
            else if (!isboxed) {
                assert(jl_is_concrete_type(jltype));
                needloop = ((jl_datatype_t*)jltype)->layout->flags.haspadding ||
                          !((jl_datatype_t*)jltype)->layout->flags.isbitsegal;
                Value *SameType = emit_isa(ctx, cmp, jltype, Twine()).first;
                if (SameType != ConstantInt::getTrue(ctx.builder.getContext())) {
                    BasicBlock *SkipBB = BasicBlock::Create(ctx.builder.getContext(), "skip_xchg", ctx.f);
                    BasicBlock *BB = BasicBlock::Create(ctx.builder.getContext(), "ok_xchg", ctx.f);
                    ctx.builder.CreateCondBr(SameType, BB, SkipBB);
                    ctx.builder.SetInsertPoint(SkipBB);
                    LoadInst *load = ctx.builder.CreateAlignedLoad(elty, ptr, Align(alignment));
                    setName(ctx.emission_context, load, "atomic_replace_initial");
                    load->setOrdering(FailOrder == AtomicOrdering::NotAtomic && isboxed ? AtomicOrdering::Monotonic : FailOrder);
                    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
                    ai.noalias = MDNode::concatenate(aliasscope, ai.noalias);
                    instr = ai.decorateInst(load);
                    ctx.builder.CreateBr(DoneBB);
                    ctx.builder.SetInsertPoint(DoneBB);
                    Succ = ctx.builder.CreatePHI(getInt1Ty(ctx.builder.getContext()), 2);
                    Succ->addIncoming(ConstantInt::get(getInt1Ty(ctx.builder.getContext()), false), SkipBB);
                    Current = ctx.builder.CreatePHI(instr->getType(), 2);
                    Current->addIncoming(instr, SkipBB);
                    ctx.builder.SetInsertPoint(BB);
                }
                Compare = emit_unbox(ctx, realelty, cmp, jltype);
                if (realelty != elty)
                    Compare = ctx.builder.CreateZExt(Compare, elty);
            }
            else if (cmp.isboxed || cmp.constant || jl_pointer_egal(jltype)) {
                Compare = boxed(ctx, cmp);
                needloop = !jl_pointer_egal(jltype) && !jl_pointer_egal(cmp.typ);
                if (needloop && !cmp.isboxed) // try to use the same box in the compare now and later
                    cmp = mark_julia_type(ctx, Compare, true, cmp.typ);
            }
            else {
                Compare = Constant::getNullValue(ctx.types().T_prjlvalue); // TODO: does this need to be an invalid bit pattern?
                needloop = true;
            }
        }
        else if (issetfieldonce) {
            needloop = !isboxed && Order != AtomicOrdering::NotAtomic && nb > sizeof(void*);
            if (Order != AtomicOrdering::NotAtomic)
                Compare = Constant::getNullValue(elty);
        }
        else { // swap or modify
            LoadInst *Current = ctx.builder.CreateAlignedLoad(elty, ptr, Align(alignment));
            Current->setOrdering(Order == AtomicOrdering::NotAtomic && !isboxed ? Order : AtomicOrdering::Monotonic);
            setName(ctx.emission_context, Current, "atomic_initial");
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            ai.noalias = MDNode::concatenate(aliasscope, ai.noalias);
            Compare = ai.decorateInst(Current);
            needloop = !isswapfield || Order != AtomicOrdering::NotAtomic;
        }
        BasicBlock *BB = NULL;
        PHINode *CmpPhi = NULL;
        if (needloop) {
            BasicBlock *From = ctx.builder.GetInsertBlock();
            BB = BasicBlock::Create(ctx.builder.getContext(), "xchg", ctx.f);
            ctx.builder.CreateBr(BB);
            ctx.builder.SetInsertPoint(BB);
            CmpPhi = ctx.builder.CreatePHI(elty, 2);
            CmpPhi->addIncoming(Compare, From);
            Compare = CmpPhi;
        }
        if (ismodifyfield) {
            if (needlock)
                emit_lockstate_value(ctx, needlock, false);
            Value *realCompare = Compare;
            if (realelty != elty)
                realCompare = ctx.builder.CreateTrunc(realCompare, realelty);
            if (intcast) {
                ctx.builder.CreateStore(realCompare, ctx.builder.CreateBitCast(intcast, realCompare->getType()->getPointerTo()));
                if (maybe_null_if_boxed)
                    realCompare = ctx.builder.CreateLoad(intcast->getAllocatedType(), intcast);
            }
            if (maybe_null_if_boxed) {
                Value *first_ptr = isboxed ? Compare : extract_first_ptr(ctx, Compare);
                if (first_ptr)
                    null_load_check(ctx, first_ptr, mod, var);
            }
            if (intcast)
                oldval = mark_julia_slot(intcast, jltype, NULL, ctx.tbaa().tbaa_stack);
            else
                oldval = mark_julia_type(ctx, realCompare, isboxed, jltype);
            rhs = newval(oldval);
            if (isboxed) {
                r = boxed(ctx, rhs);
            }
            else if (Order != AtomicOrdering::NotAtomic || CountTrackedPointers(realelty).count) {
                r = emit_unbox(ctx, realelty, rhs, jltype);
                if (realelty != elty)
                    r = ctx.builder.CreateZExt(r, elty);
            }
            if (needlock)
                emit_lockstate_value(ctx, needlock, true);
            cmp = oldval;
        }
        Value *Done;
        if (Order == AtomicOrdering::NotAtomic) {
            // modifyfield or replacefield or setfieldonce
            assert(elty == realelty && !intcast);
            auto *load = ctx.builder.CreateAlignedLoad(elty, ptr, Align(alignment));
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            ai.noalias = MDNode::concatenate(aliasscope, ai.noalias);
            ai.decorateInst(load);
            if (isboxed)
                load->setOrdering(AtomicOrdering::Monotonic);
            Value *first_ptr = nullptr;
            if (maybe_null_if_boxed && !ismodifyfield)
                first_ptr = isboxed ? load : extract_first_ptr(ctx, load);
            oldval = mark_julia_type(ctx, load, isboxed, jltype);
            assert(!issetfieldonce || first_ptr != nullptr);
            if (issetfieldonce)
                Success = ctx.builder.CreateIsNull(first_ptr);
            else
                Success = emit_f_is(ctx, oldval, cmp, first_ptr, nullptr);
            if (needloop && ismodifyfield)
                CmpPhi->addIncoming(load, ctx.builder.GetInsertBlock());
            assert(Succ == nullptr);
            BasicBlock *XchgBB = BasicBlock::Create(ctx.builder.getContext(), "xchg", ctx.f);
            ctx.builder.CreateCondBr(Success, XchgBB, needloop && ismodifyfield ? BB : DoneBB);
            ctx.builder.SetInsertPoint(XchgBB);
            if (r) {
                auto *store = ctx.builder.CreateAlignedStore(r, ptr, Align(alignment));
                jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
                ai.noalias = MDNode::concatenate(aliasscope, ai.noalias);
                ai.decorateInst(store);
            }
            else {
                assert(!isboxed && rhs.typ == jltype);
                emit_unbox_store(ctx, rhs, ptr, tbaa, alignment);
            }
            ctx.builder.CreateBr(DoneBB);
            instr = load;
        }
        else { // something atomic
            assert(r);
            if (Order == AtomicOrdering::Unordered)
                Order = AtomicOrdering::Monotonic;
            if (Order == AtomicOrdering::Monotonic && isboxed)
                Order = AtomicOrdering::Release;
            if (!isreplacefield && !issetfieldonce)
                FailOrder = AtomicOrdering::Monotonic;
            else if (FailOrder == AtomicOrdering::Unordered)
                FailOrder = AtomicOrdering::Monotonic;
            auto *store = ctx.builder.CreateAtomicCmpXchg(ptr, Compare, r, Align(alignment), Order, FailOrder);
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            ai.noalias = MDNode::concatenate(aliasscope, ai.noalias);
            ai.decorateInst(store);
            instr = ctx.builder.Insert(ExtractValueInst::Create(store, 0));
            Success = ctx.builder.Insert(ExtractValueInst::Create(store, 1));
            Done = Success;
            if ((isreplacefield || issetfieldonce) && needloop) {
                Value *realinstr = instr;
                if (realelty != elty)
                    realinstr = ctx.builder.CreateTrunc(realinstr, realelty);
                if (intcast) {
                    ctx.builder.CreateStore(realinstr, ctx.builder.CreateBitCast(intcast, realinstr->getType()->getPointerTo()));
                    oldval = mark_julia_slot(intcast, jltype, NULL, ctx.tbaa().tbaa_stack);
                    if (maybe_null_if_boxed)
                        realinstr = ctx.builder.CreateLoad(intcast->getAllocatedType(), intcast);
                }
                else {
                    oldval = mark_julia_type(ctx, realinstr, isboxed, jltype);
                }
                if (issetfieldonce) {
                    assert(!isboxed && maybe_null_if_boxed);
                    Value *first_ptr = extract_first_ptr(ctx, realinstr);
                    assert(first_ptr != nullptr);
                    Done = ctx.builder.CreateIsNotNull(first_ptr);
                }
                else {
                    // Done = !(!Success && (first_ptr != NULL && oldval == cmp))
                    Done = emit_guarded_test(ctx, ctx.builder.CreateNot(Success), false, [&] {
                        Value *first_ptr = nullptr;
                        if (maybe_null_if_boxed)
                            first_ptr = isboxed ? realinstr : extract_first_ptr(ctx, realinstr);
                        return emit_f_is(ctx, oldval, cmp, first_ptr, nullptr);
                    });
                    Done = ctx.builder.CreateNot(Done);
                }
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
        emit_lockstate_value(ctx, needlock, false);
    if (parent != NULL) {
        if (isreplacefield || issetfieldonce) {
            // TODO: avoid this branch if we aren't making a write barrier
            BasicBlock *BB = BasicBlock::Create(ctx.builder.getContext(), "xchg_wb", ctx.f);
            DoneBB = BasicBlock::Create(ctx.builder.getContext(), "done_xchg_wb", ctx.f);
            ctx.builder.CreateCondBr(Success, BB, DoneBB);
            ctx.builder.SetInsertPoint(BB);
        }
        if (r) {
            if (!isboxed)
                emit_write_multibarrier(ctx, parent, r, rhs.typ);
            else if (!type_is_permalloc(rhs.typ))
                emit_write_barrier(ctx, parent, r);
        }
        if (isreplacefield || issetfieldonce) {
            ctx.builder.CreateBr(DoneBB);
            ctx.builder.SetInsertPoint(DoneBB);
        }
    }
    if (ismodifyfield) {
        const jl_cgval_t argv[2] = { oldval, rhs };
        jl_datatype_t *rettyp = jl_apply_modify_type(jltype);
        oldval = emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
    }
    else if (issetfieldonce) {
        oldval = mark_julia_type(ctx, Success, false, jl_bool_type);
    }
    else if (!issetfield) { // swapfield or replacefield
        if (realelty != elty)
            instr = ctx.builder.Insert(CastInst::Create(Instruction::Trunc, instr, realelty));
        if (intcast) {
            ctx.builder.CreateStore(instr, ctx.builder.CreateBitCast(intcast, instr->getType()->getPointerTo()));
            instr = nullptr;
        }
        if (maybe_null_if_boxed) {
            if (intcast)
                instr = ctx.builder.CreateLoad(intcast->getAllocatedType(), intcast);
            Value *first_ptr = isboxed ? instr : extract_first_ptr(ctx, instr);
            if (first_ptr)
                null_load_check(ctx, first_ptr, mod, var);
            if (intcast && !first_ptr)
                instr = nullptr;
        }
        if (instr)
            oldval = mark_julia_type(ctx, instr, isboxed, jltype);
        else
            oldval = mark_julia_slot(intcast, jltype, NULL, ctx.tbaa().tbaa_stack);
        if (isreplacefield) {
            Success = ctx.builder.CreateZExt(Success, getInt8Ty(ctx.builder.getContext()));
            const jl_cgval_t argv[2] = {oldval, mark_julia_type(ctx, Success, false, jl_bool_type)};
            jl_datatype_t *rettyp = jl_apply_cmpswap_type(jltype);
            oldval = emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
        }
    }
    return oldval;
}

// --- convert boolean value to julia ---

// Returns ctx.types().T_pjlvalue
static Value *julia_bool(jl_codectx_t &ctx, Value *cond)
{
    auto boolean = ctx.builder.CreateSelect(cond, literal_pointer_val(ctx, jl_true),
                                          literal_pointer_val(ctx, jl_false));
    setName(ctx.emission_context, boolean, "bool");
    return boolean;
}

// --- accessing the representations of built-in data types ---

static void emit_atomic_error(jl_codectx_t &ctx, const Twine &msg)
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
    ++EmittedGetfieldUnknowns;
    size_t nfields = jl_datatype_nfields(stt);
    bool maybe_null = (unsigned)stt->name->n_uninitialized != 0;
    auto idx0 = [&]() {
        return emit_bounds_check(ctx, strct, (jl_value_t*)stt, idx, ConstantInt::get(ctx.types().T_size, nfields), inbounds);
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
                *ret = ghostValue(ctx, jft);
                return true;
            }
            if (isa<VectorType>(strct.V->getType())) {
                assert(stt->layout->npointers == 0); // we could, but don't emit this
                idx = idx0();
                if (sizeof(void*) != sizeof(int))
                    idx = ctx.builder.CreateTrunc(idx, getInt32Ty(ctx.builder.getContext())); // llvm3.3 requires this, harmless elsewhere
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
            assert((cast<ArrayType>(strct.V->getType())->getElementType() == ctx.types().T_prjlvalue) == isboxed);
            Value *idx = idx0();
            unsigned i = 0;
            Value *fld = ctx.builder.CreateExtractValue(strct.V, ArrayRef<unsigned>(i));
            for (i = 1; i < nfields; i++) {
                fld = ctx.builder.CreateSelect(
                        ctx.builder.CreateICmpEQ(idx, ConstantInt::get(idx->getType(), i)),
                        ctx.builder.CreateExtractValue(strct.V, ArrayRef<unsigned>(i)),
                        fld);
            }
            setName(ctx.emission_context, fld, "getfield");
            jl_value_t *jft = issame ? jl_svecref(types, 0) : (jl_value_t*)jl_any_type;
            if (isboxed && maybe_null)
                null_pointer_check(ctx, fld, nullptr);
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
                    ctx.types().T_prjlvalue,
                    emit_bitcast(ctx, data_pointer(ctx, strct), ctx.types().T_pprjlvalue),
                    idx0());
            setName(ctx.emission_context, fldptr, "getfield_ptr");
            LoadInst *fld = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, fldptr, Align(sizeof(void*)));
            setName(ctx.emission_context, fld, "getfield");
            fld->setOrdering(AtomicOrdering::Unordered);
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, strct.tbaa);
            ai.decorateInst(fld);
            maybe_mark_load_dereferenceable(fld, maybe_null, minimum_field_size, minimum_align);
            if (maybe_null)
                null_pointer_check(ctx, fld, nullptr);
            *ret = mark_julia_type(ctx, fld, true, jl_any_type);
            return true;
        }
        else if (is_tupletype_homogeneous(jl_get_fieldtypes(stt))) {
            assert(nfields > 0); // nf == 0 trapped by all_pointers case
            jl_value_t *jft = jl_svecref(stt->types, 0); // n.b. jl_get_fieldtypes assigned stt->types for here
            assert(jl_is_concrete_type(jft));
            idx = idx0();
            Value *ptr = data_pointer(ctx, strct);
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
            idx = ctx.builder.CreateSub(idx, ConstantInt::get(ctx.types().T_size, 1));
            Value *fld = ctx.builder.CreateCall(prepare_call(jlgetnthfieldchecked_func), { boxed(ctx, strct), idx });
            *ret = mark_julia_type(ctx, fld, true, jl_any_type);
            return true;
        }
    }
    return false;
}

static jl_cgval_t emit_unionload(jl_codectx_t &ctx, Value *addr, Value *ptindex,
        jl_value_t *jfty, size_t fsz, size_t al, MDNode *tbaa, bool mutabl,
        unsigned union_max, MDNode *tbaa_ptindex)
{
    ++EmittedUnionLoads;
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa_ptindex);
    Instruction *tindex0 = ai.decorateInst(ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), ptindex, Align(1)));
    tindex0->setMetadata(LLVMContext::MD_range, MDNode::get(ctx.builder.getContext(), {
        ConstantAsMetadata::get(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0)),
        ConstantAsMetadata::get(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), union_max)) }));
    Value *tindex = ctx.builder.CreateNUWAdd(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 1), tindex0);
    if (fsz > 0 && mutabl) {
        // move value to an immutable stack slot (excluding tindex)
        Type *AT = ArrayType::get(IntegerType::get(ctx.builder.getContext(), 8 * al), (fsz + al - 1) / al);
        AllocaInst *lv = emit_static_alloca(ctx, AT);
        setName(ctx.emission_context, lv, "immutable_union");
        if (al > 1)
            lv->setAlignment(Align(al));
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
        emit_memcpy(ctx, lv, ai, addr, ai, fsz, al, al);
        addr = lv;
    }
    return mark_julia_slot(fsz > 0 ? addr : nullptr, jfty, tindex, tbaa);
}

static bool isTBAA(MDNode *TBAA, std::initializer_list<const char*> const strset)
{
    if (!TBAA)
        return false;
    while (TBAA->getNumOperands() > 1) {
        TBAA = cast<MDNode>(TBAA->getOperand(1).get());
        auto str = cast<MDString>(TBAA->getOperand(0))->getString();
        for (auto str2 : strset) {
            if (str == str2) {
                return true;
            }
        }
    }
    return false;
}

// Check if this is a load from an immutable value. The easiest
// way to do so is to look at the tbaa and see if it derives from
// jtbaa_immut.
static bool isLoadFromImmut(LoadInst *LI)
{
    if (LI->getMetadata(LLVMContext::MD_invariant_load))
        return true;
    MDNode *TBAA = LI->getMetadata(LLVMContext::MD_tbaa);
    if (isTBAA(TBAA, {"jtbaa_immut", "jtbaa_const", "jtbaa_datatype", "jtbaa_memoryptr", "jtbaa_memorylen", "jtbaa_memoryown"}))
        return true;
    return false;
}

static bool isConstGV(GlobalVariable *gv)
{
    return gv->isConstant() || gv->getMetadata("julia.constgv");
}

// Check if this is can be traced through constant loads to an constant global
// or otherwise globally rooted value.
// Almost all `tbaa_const` loads satisfies this with the exception of
// task local constants which are constant as far as the code is concerned but aren't
// global constants. For task local constant `task_local` will be true when this function
// returns.
// Unlike this function in llvm-late-gc-lowering, we do not examine PhiNode, as those are not emitted yet
static bool isLoadFromConstGV(LoadInst *LI);
static bool isLoadFromConstGV(Value *v)
{
    v = v->stripInBoundsOffsets();
    if (auto LI = dyn_cast<LoadInst>(v))
        return isLoadFromConstGV(LI);
    if (auto gv = dyn_cast<GlobalVariable>(v))
        return isConstGV(gv);
    // null pointer
    if (isa<ConstantData>(v))
        return true;
    // literal pointers
    if (auto CE = dyn_cast<ConstantExpr>(v))
        return (CE->getOpcode() == Instruction::IntToPtr &&
                isa<ConstantData>(CE->getOperand(0)));
    if (auto SL = dyn_cast<SelectInst>(v))
        return (isLoadFromConstGV(SL->getTrueValue()) &&
                isLoadFromConstGV(SL->getFalseValue()));
    if (auto call = dyn_cast<CallInst>(v)) {
        auto callee = call->getCalledFunction();
        if (callee && callee->getName() == "julia.typeof") {
            return true;
        }
        if (callee && callee->getName() == "julia.get_pgcstack") {
            return true;
        }
        if (callee && callee->getName() == "julia.gc_loaded") {
            return isLoadFromConstGV(call->getArgOperand(0)) &&
                   isLoadFromConstGV(call->getArgOperand(1));
        }
    }
    if (isa<Argument>(v)) {
        return true;
    }
    return false;
}

// The white list implemented here and above in `isLoadFromConstGV(Value*)` should
// cover all the cases we and LLVM generates.
static bool isLoadFromConstGV(LoadInst *LI)
{
    // We only emit single slot GV in codegen
    // but LLVM global merging can change the pointer operands to GEPs/bitcasts
    auto load_base = LI->getPointerOperand()->stripInBoundsOffsets();
    assert(load_base); // Static analyzer
    auto gv = dyn_cast<GlobalVariable>(load_base);
    if (isLoadFromImmut(LI)) {
        if (gv)
            return true;
        return isLoadFromConstGV(load_base);
    }
    if (gv)
        return isConstGV(gv);
    return false;
}


static MDNode *best_field_tbaa(jl_codectx_t &ctx, const jl_cgval_t &strct, jl_datatype_t *jt, unsigned idx, size_t byte_offset)
{
    auto tbaa = strct.tbaa;
    if (tbaa == ctx.tbaa().tbaa_datatype)
        if (byte_offset != offsetof(jl_datatype_t, types))
            return ctx.tbaa().tbaa_const;
    if (tbaa == ctx.tbaa().tbaa_array) {
        if (jl_is_genericmemory_type(jt)) {
            if (idx == 0)
                return ctx.tbaa().tbaa_memorylen;
            if (idx == 1)
                return ctx.tbaa().tbaa_memoryptr;
        }
        else if (jl_is_array_type(jt)) {
            if (idx == 0)
                return ctx.tbaa().tbaa_arrayptr;
            if (idx == 1)
                return ctx.tbaa().tbaa_arraysize;
        }
    }
    if (strct.V && jl_field_isconst(jt, idx) && isLoadFromConstGV(strct.V))
        return ctx.tbaa().tbaa_const;
    return tbaa;
}

// If `nullcheck` is not NULL and a pointer NULL check is necessary
// store the pointer to be checked in `*nullcheck` instead of checking it
static jl_cgval_t emit_getfield_knownidx(jl_codectx_t &ctx, const jl_cgval_t &strct,
                                         unsigned idx, jl_datatype_t *jt,
                                         enum jl_memory_order order, Value **nullcheck)
{
    auto get_objname = [&]() {
        return strct.V ? strct.V->getName() : StringRef("");
    };
    jl_value_t *jfty = jl_field_type(jt, idx);
    bool isatomic = jl_field_isatomic(jt, idx);
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
        return ghostValue(ctx, jfty);
    Value *needlock = nullptr;
    if (isatomic && !jl_field_isptr(jt, idx) && jl_datatype_size(jfty) > MAX_ATOMIC_SIZE) {
        assert(strct.isboxed);
        needlock = boxed(ctx, strct);
    }
    size_t nfields = jl_datatype_nfields(jt);
    bool maybe_null = idx >= nfields - (unsigned)jt->name->n_uninitialized;
    size_t byte_offset = jl_field_offset(jt, idx);
    if (strct.ispointer()) {
        auto tbaa = best_field_tbaa(ctx, strct, jt, idx, byte_offset);
        Value *staddr = data_pointer(ctx, strct);
        bool isboxed;
        Type *lt = julia_type_to_llvm(ctx, (jl_value_t*)jt, &isboxed);
        Value *addr;
        if (isboxed) {
            // byte_offset == 0 is an important special case here, e.g.
            // for single field wrapper types. Introducing the bitcast
            // can pessimize mem2reg
            if (byte_offset > 0) {
                addr = ctx.builder.CreateInBoundsGEP(
                        getInt8Ty(ctx.builder.getContext()),
                        emit_bitcast(ctx, staddr, getInt8PtrTy(ctx.builder.getContext())),
                        ConstantInt::get(ctx.types().T_size, byte_offset));
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
            if (addr != staddr) {
                setNameWithField(ctx.emission_context, addr, get_objname, jt, idx, Twine("_ptr"));
            }
        }
        if (jl_field_isptr(jt, idx)) {
            setNameWithField(ctx.emission_context, addr, get_objname, jt, idx, Twine("_ptr"));
            LoadInst *Load = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, maybe_bitcast(ctx, addr, ctx.types().T_pprjlvalue), Align(sizeof(void*)));
            setNameWithField(ctx.emission_context, Load, get_objname, jt, idx, Twine());
            Load->setOrdering(order <= jl_memory_order_notatomic ? AtomicOrdering::Unordered : get_llvm_atomic_order(order));
            maybe_mark_load_dereferenceable(Load, maybe_null, jl_field_type(jt, idx));
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            Value *fldv = ai.decorateInst(Load);
            if (maybe_null)
                null_pointer_check(ctx, fldv, nullcheck);
            return mark_julia_type(ctx, fldv, true, jfty);
        }
        else if (jl_is_uniontype(jfty)) {
            size_t fsz = 0, al = 0;
            int union_max = jl_islayout_inline(jfty, &fsz, &al);
            bool isptr = (union_max == 0);
            assert(!isptr && fsz < jl_field_size(jt, idx)); (void)isptr;
            size_t fsz1 = jl_field_size(jt, idx) - 1;
            Value *ptindex;
            if (isboxed) {
                ptindex = ctx.builder.CreateConstInBoundsGEP1_32(
                    getInt8Ty(ctx.builder.getContext()), emit_bitcast(ctx, staddr, getInt8PtrTy(ctx.builder.getContext())), byte_offset + fsz1);
            }
            else {
                ptindex = emit_struct_gep(ctx, cast<StructType>(lt), staddr, byte_offset + fsz1);
            }
            auto val = emit_unionload(ctx, addr, ptindex, jfty, fsz, al, tbaa, !jl_field_isconst(jt, idx), union_max, ctx.tbaa().tbaa_unionselbyte);
            if (val.V && val.V != addr) {
                setNameWithField(ctx.emission_context, val.V, get_objname, jt, idx, Twine());
            }
            return val;
        }
        assert(jl_is_concrete_type(jfty));
        if (jl_field_isconst(jt, idx) && !(maybe_null && (jfty == (jl_value_t*)jl_bool_type ||
                                            ((jl_datatype_t*)jfty)->layout->npointers))) {
            // just compute the pointer and let user load it when necessary
            return mark_julia_slot(addr, jfty, NULL, tbaa);
        }
        unsigned align = jl_field_align(jt, idx);
        if (needlock)
            emit_lockstate_value(ctx, needlock, true);
        jl_cgval_t ret = typed_load(ctx, addr, NULL, jfty, tbaa, nullptr, false,
                needlock ? AtomicOrdering::NotAtomic : get_llvm_atomic_order(order),
                maybe_null, align, nullcheck);
        if (ret.V) {
            setNameWithField(ctx.emission_context, ret.V, get_objname, jt, idx, Twine());
        }
        if (needlock)
            emit_lockstate_value(ctx, needlock, false);
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
            fldv = ctx.builder.CreateExtractElement(obj, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), idx));
            setNameWithField(ctx.emission_context, fldv, get_objname, jt, idx, Twine());
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
                lv->setOperand(0, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), (fsz + align - 1) / align));
                // emit all of the align-sized words
                unsigned i = 0;
                for (; i < fsz / align; i++) {
                    unsigned fld = st_idx + i;
                    Value *fldv = ctx.builder.CreateExtractValue(obj, ArrayRef<unsigned>(fld));
                    Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                    ctx.builder.CreateAlignedStore(fldv, fldp, Align(align));
                }
                // emit remaining bytes up to tindex
                if (i < ptindex - st_idx) {
                    Value *staddr = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                    staddr = ctx.builder.CreateBitCast(staddr, getInt8PtrTy(ctx.builder.getContext()));
                    for (; i < ptindex - st_idx; i++) {
                        Value *fldv = ctx.builder.CreateExtractValue(obj, ArrayRef<unsigned>(st_idx + i));
                        Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(getInt8Ty(ctx.builder.getContext()), staddr, i);
                        ctx.builder.CreateAlignedStore(fldv, fldp, Align(1));
                    }
                }
                setNameWithField(ctx.emission_context, lv, get_objname, jt, idx, Twine());
            }
            Value *tindex0 = ctx.builder.CreateExtractValue(obj, ArrayRef<unsigned>(ptindex));
            Value *tindex = ctx.builder.CreateNUWAdd(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 1), tindex0);
            setNameWithField(ctx.emission_context, tindex, get_objname, jt, idx, Twine(".tindex"));
            return mark_julia_slot(lv, jfty, tindex, ctx.tbaa().tbaa_stack);
        }
        else {
            unsigned st_idx;
            if (isa<ArrayType>(T))
                st_idx = idx;
            else if (isa<StructType>(T))
                st_idx = convert_struct_offset(ctx, T, byte_offset);
            else
                llvm_unreachable("encountered incompatible type for a struct");
            fldv = ctx.builder.CreateExtractValue(obj, ArrayRef<unsigned>(st_idx));
            setNameWithField(ctx.emission_context, fldv, get_objname, jt, idx, Twine());
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
    ++EmittedVarargsLength;
    Value *valen = NULL;
    if (ctx.nvargs != -1) {
        valen = ConstantInt::get(getInt32Ty(ctx.builder.getContext()), ctx.nvargs);
    } else {
        assert(ctx.argCount);
        int nreq = ctx.nReqArgs;
        valen = ctx.builder.CreateSub((Value*)ctx.argCount,
                                        ConstantInt::get(getInt32Ty(ctx.builder.getContext()), nreq));
    }
#ifdef _P64
    return ctx.builder.CreateSExt(valen, getInt64Ty(ctx.builder.getContext()));
#else
    return valen;
#endif
}

static Value *emit_genericmemoryelsize(jl_codectx_t &ctx, Value *v, jl_value_t *typ, bool add_isunion)
{
    ++EmittedArrayElsize;
    jl_datatype_t *sty = (jl_datatype_t*)jl_unwrap_unionall(typ);
    if (jl_is_datatype(sty) && !jl_has_free_typevars((jl_value_t*)sty) && sty->layout) {
        if (jl_is_genericmemoryref_type(sty))
            sty = (jl_datatype_t*)jl_field_type_concrete(sty, 1);
        size_t sz = sty->layout->size;
        if (sty->layout->flags.arrayelem_isunion)
            sz++;
        return ConstantInt::get(ctx.types().T_size, sz);
    }
    else {
        v = emit_bitcast(ctx, v, ctx.types().T_prjlvalue);
        Value *t = emit_typeof(ctx, v, false, false, true);
        Value *elsize = emit_datatype_size(ctx, t, add_isunion);
        return ctx.builder.CreateZExt(elsize, ctx.types().T_size);
    }
}

static ssize_t genericmemoryype_constelsize(jl_value_t *typ)
{
    jl_datatype_t *sty = (jl_datatype_t*)jl_unwrap_unionall(typ);
    if (jl_is_datatype(sty) && !jl_has_free_typevars((jl_value_t*)sty) && sty->layout) {
        if (jl_is_array_type(sty))
            sty = (jl_datatype_t*)jl_field_type_concrete(sty, 0);
        if (jl_is_genericmemoryref_type(sty))
            sty = (jl_datatype_t*)jl_field_type_concrete(sty, 1);
        return sty->layout->size;
    }
    return -1;
}

static intptr_t genericmemoryype_maxsize(jl_value_t *ty) // the maxsize is strictly less than the return value
{
    ssize_t elsz = genericmemoryype_constelsize(ty);
    if (elsz <= 1)
        return INTPTR_MAX;
    return INTPTR_MAX / elsz;
}

static Value *emit_genericmemorylen(jl_codectx_t &ctx, Value *addr, jl_value_t *typ)
{
    addr = emit_bitcast(ctx, decay_derived(ctx, addr), ctx.types().T_jlgenericmemory->getPointerTo()),
    addr = ctx.builder.CreateStructGEP(ctx.types().T_jlgenericmemory, addr, 0);
    LoadInst *LI = ctx.builder.CreateAlignedLoad(ctx.types().T_jlgenericmemory->getElementType(0), addr, Align(sizeof(size_t)));
    jl_aliasinfo_t aliasinfo_mem = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_memorylen);
    aliasinfo_mem.decorateInst(LI);
    MDBuilder MDB(ctx.builder.getContext());
    auto rng = MDB.createRange(Constant::getNullValue(ctx.types().T_size), ConstantInt::get(ctx.types().T_size, genericmemoryype_maxsize(typ)));
    LI->setMetadata(LLVMContext::MD_range, rng);
    return LI;
}

static Value *emit_genericmemoryptr(jl_codectx_t &ctx, Value *mem, const jl_datatype_layout_t *layout, unsigned AS)
{
    ++EmittedArrayptr;
    PointerType *PT = cast<PointerType>(mem->getType());
    assert(PT == ctx.types().T_prjlvalue);
    Value *addr = emit_bitcast(ctx, mem, ctx.types().T_jlgenericmemory->getPointerTo(PT->getAddressSpace()));
    addr = decay_derived(ctx, addr);
    addr = ctx.builder.CreateStructGEP(ctx.types().T_jlgenericmemory, addr, 1);
    setName(ctx.emission_context, addr, ".data_ptr");
    PointerType *PPT = cast<PointerType>(ctx.types().T_jlgenericmemory->getElementType(1));
    LoadInst *LI = ctx.builder.CreateAlignedLoad(PPT, addr, Align(sizeof(char*)));
    LI->setOrdering(AtomicOrdering::NotAtomic);
    LI->setMetadata(LLVMContext::MD_nonnull, MDNode::get(ctx.builder.getContext(), None));
    jl_aliasinfo_t aliasinfo = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    aliasinfo.decorateInst(LI);
    Value *ptr = LI;
    if (AS) {
        assert(AS == AddressSpace::Loaded);
        ptr = ctx.builder.CreateCall(prepare_call(gc_loaded_func), { mem, ptr });
    }
    if (!layout->flags.arrayelem_isboxed)
        ptr = ctx.builder.CreateBitCast(ptr, PointerType::get(getInt8Ty(ctx.builder.getContext()), AS));
    return ptr;
}

static Value *emit_genericmemoryowner(jl_codectx_t &ctx, Value *t)
{
    Value *m = emit_bitcast(ctx, decay_derived(ctx, t), ctx.types().T_jlgenericmemory->getPointerTo(0));
    Value *addr = ctx.builder.CreateStructGEP(ctx.types().T_jlgenericmemory, m, 1);
    Type *T_data = ctx.types().T_jlgenericmemory->getElementType(1);
    LoadInst *LI = ctx.builder.CreateAlignedLoad(T_data, addr, Align(sizeof(char*)));
    LI->setOrdering(AtomicOrdering::NotAtomic);
    LI->setMetadata(LLVMContext::MD_nonnull, MDNode::get(ctx.builder.getContext(), None));
    jl_aliasinfo_t aliasinfo_mem = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_memoryown);
    aliasinfo_mem.decorateInst(LI);
    addr = ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_prjlvalue, emit_bitcast(ctx, m, LI->getType()), JL_SMALL_BYTE_ALIGNMENT / sizeof(void*));
    Value *foreign = ctx.builder.CreateICmpNE(addr, decay_derived(ctx, LI));
    return emit_guarded_test(ctx, foreign, t, [&] {
            addr = ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_jlgenericmemory, m, 1);
            LoadInst *owner = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, emit_bitcast(ctx, addr, ctx.types().T_pprjlvalue), Align(sizeof(void*)));
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
            ai.decorateInst(owner);
            return ctx.builder.CreateSelect(ctx.builder.CreateIsNull(owner), t, owner);
        });
}

// --- boxing ---

static Value *emit_allocobj(jl_codectx_t &ctx, jl_datatype_t *jt, bool fully_initialized);

static void init_bits_value(jl_codectx_t &ctx, Value *newv, Value *v, MDNode *tbaa,
                            unsigned alignment = sizeof(void*)) // min alignment in julia's gc is pointer-aligned
{
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
    // newv should already be tagged
    ai.decorateInst(ctx.builder.CreateAlignedStore(v, emit_bitcast(ctx, newv,
        PointerType::get(v->getType(), 0)), Align(alignment)));
}

static void init_bits_cgval(jl_codectx_t &ctx, Value *newv, const jl_cgval_t& v, MDNode *tbaa)
{
    // newv should already be tagged
    if (v.ispointer()) {
        unsigned align = std::max(julia_alignment(v.typ), (unsigned)sizeof(void*));
        emit_memcpy(ctx, newv, jl_aliasinfo_t::fromTBAA(ctx, tbaa), v, jl_datatype_size(v.typ), align, julia_alignment(v.typ));
    }
    else {
        init_bits_value(ctx, newv, v.V, tbaa);
    }
}

static jl_value_t *static_constant_instance(const llvm::DataLayout &DL, Constant *constant, jl_value_t *jt)
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
            return static_constant_instance(DL, ce->getOperand(0), jt);
        }
        return NULL;
    }

    if (isa<GlobalValue>(constant))
        return NULL;

    size_t nargs;
    if (const auto *CC = dyn_cast<ConstantAggregate>(constant))
        nargs = CC->getNumOperands();
    else if (const auto *CAZ = dyn_cast<ConstantAggregateZero>(constant)) {
        // SVE: Elsewhere we use `getMinKnownValue`
        nargs = CAZ->getElementCount().getFixedValue();
    }
    else if (const auto *CDS = dyn_cast<ConstantDataSequential>(constant))
        nargs = CDS->getNumElements();
    else
        return NULL;
    assert(nargs > 0 && !jl_is_datatype_singleton(jst));
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
            llvm_idx = convert_struct_offset(DL, constant->getType(), jl_field_offset(jst, i));
        Constant *fld = constant->getAggregateElement(llvm_idx);
        flds[i] = static_constant_instance(DL, fld, ft);
        if (flds[i] == NULL) {
            JL_GC_POP();
            return NULL; // must have been unreachable
        }
    }
    jl_value_t *obj = jl_new_structv(jst, flds, nargs);
    JL_GC_POP();
    return obj;
}

template<typename TypeFn_t>
static Value *call_with_attrs(jl_codectx_t &ctx, JuliaFunction<TypeFn_t> *intr, Value *v)
{
    Function *F = prepare_call(intr);
    CallInst *Call = ctx.builder.CreateCall(F, v);
    Call->setAttributes(F->getAttributes());
    return Call;
}

static jl_value_t *jl_ensure_rooted(jl_codectx_t &ctx, jl_value_t *val);

static Value *as_value(jl_codectx_t &ctx, Type *to, const jl_cgval_t &v)
{
    assert(!v.isboxed);
    return emit_unbox(ctx, to, v, v.typ);
}

static Value *load_i8box(jl_codectx_t &ctx, Value *v, jl_datatype_t *ty)
{
    auto jvar = ty == jl_int8_type ? jlboxed_int8_cache : jlboxed_uint8_cache;
    GlobalVariable *gv = prepare_global_in(jl_Module, jvar);
    Value *idx[] = {ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0), ctx.builder.CreateZExt(v, getInt32Ty(ctx.builder.getContext()))};
    auto slot = ctx.builder.CreateInBoundsGEP(gv->getValueType(), gv, idx);
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    return ai.decorateInst(maybe_mark_load_dereferenceable(
            ctx.builder.CreateAlignedLoad(ctx.types().T_pjlvalue, slot, Align(sizeof(void*))), false,
            (jl_value_t*)ty));
}

// some types have special boxing functions with small-value caches
// Returns ctx.types().T_prjlvalue
static Value *_boxed_special(jl_codectx_t &ctx, const jl_cgval_t &vinfo, Type *t)
{
    jl_value_t *jt = vinfo.typ;
    if (jt == (jl_value_t*)jl_bool_type)
        return track_pjlvalue(ctx, julia_bool(ctx, ctx.builder.CreateTrunc(as_value(ctx, t, vinfo), getInt1Ty(ctx.builder.getContext()))));
    if (t == getInt1Ty(ctx.builder.getContext()))
        return track_pjlvalue(ctx, julia_bool(ctx, as_value(ctx, t, vinfo)));

    if (ctx.linfo && jl_is_method(ctx.linfo->def.method) && !vinfo.ispointer()) { // don't bother codegen pre-boxing for toplevel
        if (Constant *c = dyn_cast<Constant>(vinfo.V)) {
            jl_value_t *s = static_constant_instance(jl_Module->getDataLayout(), c, jt);
            if (s) {
                s = jl_ensure_rooted(ctx, s);
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
        Value *v = as_value(ctx, t, vinfo);
        assert(v->getType() == ctx.emission_context.llvmtypes[jl_ssavalue_type]);
        v = ctx.builder.CreateExtractValue(v, 0);
        box = call_with_attrs(ctx, box_ssavalue_func, v);
    }
    else if (!jb->name->abstract && jl_datatype_nbits(jb) == 0) {
        // singleton
        assert(jl_is_datatype_singleton(jb));
        return track_pjlvalue(ctx, literal_pointer_val(ctx, jb->instance));
    }
    if (box) {
        setName(ctx.emission_context, box, [&]() {return "box_" + std::string(jl_symbol_name(jb->name->name));});
    }
    return box;
}

static Value *compute_box_tindex(jl_codectx_t &ctx, Value *datatype_tag, jl_value_t *supertype, jl_value_t *ut)
{
    Value *tindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0);
    unsigned counter = 0;
    for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                if (jl_subtype((jl_value_t*)jt, supertype)) {
                    Value *cmp = ctx.builder.CreateICmpEQ(emit_tagfrom(ctx, jt), datatype_tag);
                    tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), idx), tindex);
                }
            },
            ut,
            counter);
    setName(ctx.emission_context, tindex, datatype_tag->getName() + ".tindex");
    return tindex;
}

// get the runtime tindex value, assuming val is already converted to type typ if it has a TIndex
static Value *compute_tindex_unboxed(jl_codectx_t &ctx, const jl_cgval_t &val, jl_value_t *typ, bool maybenull=false)
{
    if (val.typ == jl_bottom_type)
        return UndefValue::get(getInt8Ty(ctx.builder.getContext()));
    if (val.constant)
        return ConstantInt::get(getInt8Ty(ctx.builder.getContext()), get_box_tindex((jl_datatype_t*)jl_typeof(val.constant), typ));
    if (val.TIndex)
        return ctx.builder.CreateAnd(val.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x7f));
    Value *typof = emit_typeof(ctx, val, maybenull, true);
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
        Type *AT = ArrayType::get(IntegerType::get(ctx.builder.getContext(), 8 * min_align), (nbytes + min_align - 1) / min_align);
        AllocaInst *lv = emit_static_alloca(ctx, AT);
        setName(ctx.emission_context, lv, "unionalloca");
        if (align > 1)
            lv->setAlignment(Align(align));
        return lv;
    }
    return NULL;
}

/*
 * Box unboxed values in a union. Optionally, skip certain unboxed values,
 * returning `Constant::getNullValue(ctx.types().T_pjlvalue)` in one of the skipped cases. If `skip` is not empty,
 * skip[0] (corresponding to unknown boxed) must always be set. In that
 * case, the calling code must separately deal with the case where
 * `vinfo` is already an unknown boxed union (union tag UNION_BOX_MARKER).
 */
// Returns ctx.types().T_prjlvalue
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
    BasicBlock *defaultBB = BasicBlock::Create(ctx.builder.getContext(), "box_union_isboxed", ctx.f);
    SwitchInst *switchInst = ctx.builder.CreateSwitch(tindex, defaultBB);
    BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_box_union", ctx.f);
    ctx.builder.SetInsertPoint(postBB);
    PHINode *box_merge = ctx.builder.CreatePHI(ctx.types().T_prjlvalue, 2);
    unsigned counter = 0;
    for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                if (idx < skip.size() && skip[idx])
                    return;
                Type *t = julia_type_to_llvm(ctx, (jl_value_t*)jt);
                BasicBlock *tempBB = BasicBlock::Create(ctx.builder.getContext(), "box_union", ctx.f);
                ctx.builder.SetInsertPoint(tempBB);
                switchInst->addCase(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), idx), tempBB);
                Value *box;
                if (type_is_ghost(t)) {
                    box = track_pjlvalue(ctx, literal_pointer_val(ctx, jt->instance));
                }
                else {
                    jl_cgval_t vinfo_r = jl_cgval_t(vinfo, (jl_value_t*)jt, NULL);
                    box = _boxed_special(ctx, vinfo_r, t);
                    if (!box) {
                        box = emit_allocobj(ctx, jt, true);
                        setName(ctx.emission_context, box, "unionbox");
                        init_bits_cgval(ctx, box, vinfo_r, jl_is_mutable(jt) ? ctx.tbaa().tbaa_mutab : ctx.tbaa().tbaa_immut);
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
        box_merge->addIncoming(Constant::getNullValue(ctx.types().T_prjlvalue), defaultBB);
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

static Function *mangleIntrinsic(IntrinsicInst *call) //mangling based on replaceIntrinsicUseWith
{
    Intrinsic::ID ID = call->getIntrinsicID();
    auto nargs = call->arg_size();
    SmallVector<Type*, 8> argTys(nargs);
    auto oldfType = call->getFunctionType();
    for (unsigned i = 0; i < oldfType->getNumParams(); i++) {
        auto argi = call->getArgOperand(i);
        argTys[i] = argi->getType();
    }

    auto newfType = FunctionType::get(
            oldfType->getReturnType(),
            ArrayRef<Type*>(argTys).slice(0, oldfType->getNumParams()),
            oldfType->isVarArg());

    // Accumulate an array of overloaded types for the given intrinsic
    // and compute the new name mangling schema
    SmallVector<Type*, 4> overloadTys;
    {
        SmallVector<Intrinsic::IITDescriptor, 8> Table;
        getIntrinsicInfoTableEntries(ID, Table);
        ArrayRef<Intrinsic::IITDescriptor> TableRef = Table;
        auto res = Intrinsic::matchIntrinsicSignature(newfType, TableRef, overloadTys);
        assert(res == Intrinsic::MatchIntrinsicTypes_Match);
        (void)res;
        bool matchvararg = !Intrinsic::matchIntrinsicVarArg(newfType->isVarArg(), TableRef);
        assert(matchvararg);
        (void)matchvararg;
    }
    auto newF = Intrinsic::getDeclaration(call->getModule(), ID, overloadTys);
    assert(newF->getFunctionType() == newfType);
    newF->setCallingConv(call->getCallingConv());
    return newF;
}


//Used for allocation hoisting in *boxed
static void recursively_adjust_ptr_type(llvm::Value *Val, unsigned FromAS, unsigned ToAS)
{
    for (auto *User : Val->users()) {
        if (isa<GetElementPtrInst>(User)) {
            GetElementPtrInst *Inst = cast<GetElementPtrInst>(User);
            #if JL_LLVM_VERSION >= 170000
            Inst->mutateType(PointerType::get(Inst->getType(), ToAS));
            #else
            Inst->mutateType(PointerType::getWithSamePointeeType(cast<PointerType>(Inst->getType()), ToAS));
            #endif
            recursively_adjust_ptr_type(Inst, FromAS, ToAS);
        }
        else if (isa<IntrinsicInst>(User)) {
            IntrinsicInst *call = cast<IntrinsicInst>(User);
            call->setCalledFunction(mangleIntrinsic(call));
        }
        else if (isa<BitCastInst>(User)) {
            BitCastInst *Inst = cast<BitCastInst>(User);
            #if JL_LLVM_VERSION >= 170000
            Inst->mutateType(PointerType::get(Inst->getType(), ToAS));
            #else
            Inst->mutateType(PointerType::getWithSamePointeeType(cast<PointerType>(Inst->getType()), ToAS));
            #endif
            recursively_adjust_ptr_type(Inst, FromAS, ToAS);
        }
    }
}

// this is used to wrap values for generic contexts, where a
// dynamically-typed value is required (e.g. argument to unknown function).
// if it's already a pointer it's left alone.
// Returns ctx.types().T_prjlvalue
static Value *boxed(jl_codectx_t &ctx, const jl_cgval_t &vinfo, bool is_promotable)
{
    jl_value_t *jt = vinfo.typ;
    if (jt == jl_bottom_type || jt == NULL)
        // We have an undef value on a (hopefully) dead branch
        return UndefValue::get(ctx.types().T_prjlvalue);
    if (vinfo.constant)
        return track_pjlvalue(ctx, literal_pointer_val(ctx, vinfo.constant));
    // This can happen in early bootstrap for `gc_preserve_begin` return value.
    if (jt == (jl_value_t*)jl_nothing_type)
        return track_pjlvalue(ctx, literal_pointer_val(ctx, jl_nothing));
    if (vinfo.isboxed) {
        assert(vinfo.V == vinfo.Vboxed && vinfo.V != nullptr);
        assert(vinfo.V->getType() == ctx.types().T_prjlvalue);
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
            bool do_promote = vinfo.promotion_point;
            if (do_promote && is_promotable) {
                auto IP = ctx.builder.saveIP();
                ctx.builder.SetInsertPoint(vinfo.promotion_point);
                box = emit_allocobj(ctx, (jl_datatype_t*)jt, true);
                Value *decayed = decay_derived(ctx, box);
                AllocaInst *originalAlloca = cast<AllocaInst>(vinfo.V);
                box->takeName(originalAlloca);
                #if JL_LLVM_VERSION >= 170000
                decayed = maybe_bitcast(ctx, decayed, PointerType::get(originalAlloca->getType(), AddressSpace::Derived));
                #else
                decayed = maybe_bitcast(ctx, decayed, PointerType::getWithSamePointeeType(originalAlloca->getType(), AddressSpace::Derived));
                #endif
                // Warning: Very illegal IR here temporarily
                originalAlloca->mutateType(decayed->getType());
                recursively_adjust_ptr_type(originalAlloca, 0, AddressSpace::Derived);
                originalAlloca->replaceAllUsesWith(decayed);
                // end illegal IR
                originalAlloca->eraseFromParent();
                ctx.builder.restoreIP(IP);
            } else {
                auto arg_typename = [&] JL_NOTSAFEPOINT {
                    return "box::" + std::string(jl_symbol_name(((jl_datatype_t*)(jt))->name->name));
                };
                box = emit_allocobj(ctx, (jl_datatype_t*)jt, true);
                setName(ctx.emission_context, box, arg_typename);
                init_bits_cgval(ctx, box, vinfo, jl_is_mutable(jt) ? ctx.tbaa().tbaa_mutab : ctx.tbaa().tbaa_immut);
            }
        }
    }
    return box;
}

// copy src to dest, if src is justbits. if skip is true, the value of dest is undefined
static void emit_unionmove(jl_codectx_t &ctx, Value *dest, MDNode *tbaa_dst, const jl_cgval_t &src, Value *skip, bool isVolatile=false)
{
    if (AllocaInst *ai = dyn_cast<AllocaInst>(dest))
        // TODO: make this a lifetime_end & dereferenceable annotation?
        ctx.builder.CreateAlignedStore(UndefValue::get(ai->getAllocatedType()), ai, ai->getAlign());
    if (jl_is_concrete_type(src.typ) || src.constant) {
        jl_value_t *typ = src.constant ? jl_typeof(src.constant) : src.typ;
        assert(skip || jl_is_pointerfree(typ));
        if (jl_is_pointerfree(typ)) {
            unsigned alignment = julia_alignment(typ);
            if (!src.ispointer() || src.constant) {
                emit_unbox_store(ctx, src, dest, tbaa_dst, alignment, isVolatile);
            }
            else {
                Value *src_ptr = data_pointer(ctx, src);
                unsigned nb = jl_datatype_size(typ);
                // TODO: this branch may be bad for performance, but is necessary to work around LLVM bugs with the undef option that we want to use:
                //   select copy dest -> dest to simulate an undef value / conditional copy
                // if (skip) src_ptr = ctx.builder.CreateSelect(skip, dest, src_ptr);
                auto f = [&] {
                    (void)emit_memcpy(ctx, dest, jl_aliasinfo_t::fromTBAA(ctx, tbaa_dst), src_ptr,
                                      jl_aliasinfo_t::fromTBAA(ctx, src.tbaa), nb, alignment, alignment, isVolatile);
                    return nullptr;
                };
                if (skip)
                    emit_guarded_test(ctx, skip, nullptr, f);
                else
                    f();
            }
        }
    }
    else if (src.TIndex) {
        Value *tindex = ctx.builder.CreateAnd(src.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x7f));
        if (skip)
            tindex = ctx.builder.CreateSelect(skip, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0), tindex);
        Value *src_ptr = data_pointer(ctx, src);
        src_ptr = src_ptr ? maybe_bitcast(ctx, src_ptr, getInt8PtrTy(ctx.builder.getContext())) : src_ptr;
        dest = maybe_bitcast(ctx, dest, getInt8PtrTy(ctx.builder.getContext()));
        BasicBlock *defaultBB = BasicBlock::Create(ctx.builder.getContext(), "union_move_skip", ctx.f);
        SwitchInst *switchInst = ctx.builder.CreateSwitch(tindex, defaultBB);
        BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_union_move", ctx.f);
        unsigned counter = 0;
        bool allunboxed = for_each_uniontype_small(
                [&](unsigned idx, jl_datatype_t *jt) {
                    unsigned nb = jl_datatype_size(jt);
                    unsigned alignment = julia_alignment((jl_value_t*)jt);
                    BasicBlock *tempBB = BasicBlock::Create(ctx.builder.getContext(), "union_move", ctx.f);
                    ctx.builder.SetInsertPoint(tempBB);
                    switchInst->addCase(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), idx), tempBB);
                    if (nb > 0) {
                        if (!src_ptr) {
                            Function *trap_func =
                                Intrinsic::getDeclaration(ctx.f->getParent(), Intrinsic::trap);
                            ctx.builder.CreateCall(trap_func);
                            ctx.builder.CreateUnreachable();
                            return;
                        } else {
                            emit_memcpy(ctx, dest, jl_aliasinfo_t::fromTBAA(ctx, tbaa_dst), src_ptr,
                                        jl_aliasinfo_t::fromTBAA(ctx, src.tbaa), nb, alignment, alignment, isVolatile);
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
        auto f = [&] {
            Value *datatype = emit_typeof(ctx, src, false, false);
            Value *copy_bytes = emit_datatype_size(ctx, datatype);
            (void)emit_memcpy(ctx, dest, jl_aliasinfo_t::fromTBAA(ctx, tbaa_dst), data_pointer(ctx, src),
                              jl_aliasinfo_t::fromTBAA(ctx, src.tbaa), copy_bytes, 1, 1, isVolatile);
            return nullptr;
        };
        if (skip)
            emit_guarded_test(ctx, skip, nullptr, f);
        else
            f();
    }
}


static void emit_cpointercheck(jl_codectx_t &ctx, const jl_cgval_t &x, const Twine &msg)
{
    ++EmittedCPointerChecks;
    Value *t = emit_typeof(ctx, x, false, false);

    Value *istype =
        ctx.builder.CreateICmpEQ(emit_datatype_name(ctx, t),
                                 literal_pointer_val(ctx, (jl_value_t*)jl_pointer_typename));
    setName(ctx.emission_context, istype, "istype");
    BasicBlock *failBB = BasicBlock::Create(ctx.builder.getContext(), "fail", ctx.f);
    BasicBlock *passBB = BasicBlock::Create(ctx.builder.getContext(), "pass");
    ctx.builder.CreateCondBr(istype, passBB, failBB);
    ctx.builder.SetInsertPoint(failBB);

    just_emit_type_error(ctx, x, literal_pointer_val(ctx, (jl_value_t*)jl_pointer_type), msg);
    ctx.builder.CreateUnreachable();

    passBB->insertInto(ctx.f);
    ctx.builder.SetInsertPoint(passBB);
}

// allocation for known size object
// returns a prjlvalue
static Value *emit_allocobj(jl_codectx_t &ctx, size_t static_size, Value *jt,
                            bool fully_initialized, unsigned align)
{
    ++EmittedAllocObjs;
    Value *current_task = get_current_task(ctx);
    Function *F = prepare_call(jl_alloc_obj_func);
    auto call = ctx.builder.CreateCall(F, {current_task, ConstantInt::get(ctx.types().T_size, static_size), maybe_decay_untracked(ctx, jt)});
    call->setAttributes(F->getAttributes());
    if (static_size > 0)
        call->addRetAttr(Attribute::getWithDereferenceableBytes(call->getContext(), static_size));
    call->addRetAttr(Attribute::getWithAlignment(call->getContext(), Align(align)));
    if (fully_initialized)
        call->addFnAttr(Attribute::get(call->getContext(), Attribute::AllocKind, uint64_t(AllocFnKind::Alloc | AllocFnKind::Uninitialized)));
    return call;
}

static Value *emit_allocobj(jl_codectx_t &ctx, jl_datatype_t *jt, bool fully_initialized)
{
    return emit_allocobj(ctx, jl_datatype_size(jt), ctx.builder.CreateIntToPtr(emit_tagfrom(ctx, jt), ctx.types().T_pjlvalue),
                         fully_initialized, julia_alignment((jl_value_t*)jt));
}

// allocation for unknown object from an untracked pointer
static Value *emit_new_bits(jl_codectx_t &ctx, Value *jt, Value *pval)
{
    pval = ctx.builder.CreateBitCast(pval, getInt8PtrTy(ctx.builder.getContext()));
    Function *F = prepare_call(jl_newbits_func);
    auto call = ctx.builder.CreateCall(F, { jt, pval });
    call->setAttributes(F->getAttributes());
    return call;
}

// if ptr is NULL this emits a write barrier _back_
static void emit_write_barrier(jl_codectx_t &ctx, Value *parent, Value *ptr)
{
    emit_write_barrier(ctx, parent, ArrayRef<Value*>(ptr));
}

static void emit_write_barrier(jl_codectx_t &ctx, Value *parent, ArrayRef<Value*> ptrs)
{
    ++EmittedWriteBarriers;
    // if there are no child objects we can skip emission
    if (ptrs.empty())
        return;
    SmallVector<Value*, 8> decay_ptrs;
    decay_ptrs.push_back(maybe_decay_untracked(ctx, emit_bitcast(ctx, parent, ctx.types().T_prjlvalue)));
    for (auto ptr : ptrs) {
        decay_ptrs.push_back(maybe_decay_untracked(ctx, emit_bitcast(ctx, ptr, ctx.types().T_prjlvalue)));
    }
    ctx.builder.CreateCall(prepare_call(jl_write_barrier_func), decay_ptrs);
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

static jl_cgval_t union_store(jl_codectx_t &ctx,
        Value *ptr, Value *ptindex, jl_cgval_t rhs, jl_cgval_t cmp,
        jl_value_t *jltype, MDNode *tbaa, MDNode *tbaa_tindex,
        AtomicOrdering Order, AtomicOrdering FailOrder,
        Value *needlock, bool issetfield, bool isreplacefield, bool isswapfield, bool ismodifyfield, bool issetfieldonce,
        const jl_cgval_t *modifyop, const Twine &fname)
{
    assert(Order == AtomicOrdering::NotAtomic);
    if (issetfieldonce)
        return mark_julia_const(ctx, jl_false);
    size_t fsz = 0, al = 0;
    int union_max = jl_islayout_inline(jltype, &fsz, &al);
    assert(union_max > 0);
    // compute tindex from rhs
    jl_cgval_t rhs_union = convert_julia_type(ctx, rhs, jltype);
    if (rhs_union.typ == jl_bottom_type)
        return jl_cgval_t();
    if (needlock)
        emit_lockstate_value(ctx, needlock, true);
    BasicBlock *ModifyBB = NULL;
    if (ismodifyfield) {
        ModifyBB = BasicBlock::Create(ctx.builder.getContext(), "modify_xchg", ctx.f);
        ctx.builder.CreateBr(ModifyBB);
        ctx.builder.SetInsertPoint(ModifyBB);
    }
    jl_cgval_t oldval = rhs;
    if (!issetfield)
        oldval = emit_unionload(ctx, ptr, ptindex, jltype, fsz, al, tbaa, true, union_max, tbaa_tindex);
    Value *Success = NULL;
    BasicBlock *DoneBB = NULL;
    if (isreplacefield || ismodifyfield) {
        if (ismodifyfield) {
            if (needlock)
                emit_lockstate_value(ctx, needlock, false);
            const jl_cgval_t argv[3] = { cmp, oldval, rhs };
            if (modifyop) {
                rhs = emit_invoke(ctx, *modifyop, argv, 3, (jl_value_t*)jl_any_type);
            }
            else {
                Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, 3, julia_call);
                rhs = mark_julia_type(ctx, callval, true, jl_any_type);
            }
            emit_typecheck(ctx, rhs, jltype, fname);
            rhs = update_julia_type(ctx, rhs, jltype);
            rhs_union = convert_julia_type(ctx, rhs, jltype);
            if (rhs_union.typ == jl_bottom_type)
                return jl_cgval_t();
            if (needlock)
                emit_lockstate_value(ctx, needlock, true);
            cmp = oldval;
            oldval = emit_unionload(ctx, ptr, ptindex, jltype, fsz, al, tbaa, true, union_max, tbaa_tindex);
        }
        BasicBlock *XchgBB = BasicBlock::Create(ctx.builder.getContext(), "xchg", ctx.f);
        DoneBB = BasicBlock::Create(ctx.builder.getContext(), "done_xchg", ctx.f);
        Success = emit_f_is(ctx, oldval, cmp);
        ctx.builder.CreateCondBr(Success, XchgBB, ismodifyfield ? ModifyBB : DoneBB);
        ctx.builder.SetInsertPoint(XchgBB);
    }
    Value *tindex = compute_tindex_unboxed(ctx, rhs_union, jltype);
    tindex = ctx.builder.CreateNUWSub(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 1));
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa_tindex);
    ai.decorateInst(ctx.builder.CreateAlignedStore(tindex, ptindex, Align(1)));
    // copy data
    if (!rhs.isghost) {
        emit_unionmove(ctx, ptr, tbaa, rhs, nullptr);
    }
    if (isreplacefield || ismodifyfield) {
        ctx.builder.CreateBr(DoneBB);
        ctx.builder.SetInsertPoint(DoneBB);
    }
    if (needlock)
        emit_lockstate_value(ctx, needlock, false);
    if (isreplacefield) {
        Success = ctx.builder.CreateZExt(Success, getInt8Ty(ctx.builder.getContext()));
        jl_cgval_t argv[2] = {oldval, mark_julia_type(ctx, Success, false, jl_bool_type)};
        jl_datatype_t *rettyp = jl_apply_cmpswap_type(jltype);
        oldval = emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
    }
    else if (ismodifyfield) {
        jl_cgval_t argv[2] = {oldval, rhs};
        jl_datatype_t *rettyp = jl_apply_modify_type(jltype);
        oldval = emit_new_struct(ctx, (jl_value_t*)rettyp, 2, argv);
    }
    return oldval;
}

static jl_cgval_t emit_setfield(jl_codectx_t &ctx,
        jl_datatype_t *sty, const jl_cgval_t &strct, size_t idx0,
        jl_cgval_t rhs, jl_cgval_t cmp,
        bool wb, AtomicOrdering Order, AtomicOrdering FailOrder,
        Value *needlock, bool issetfield, bool isreplacefield, bool isswapfield, bool ismodifyfield, bool issetfieldonce,
        const jl_cgval_t *modifyop, const Twine &fname)
{
    auto get_objname = [&]() {
        return strct.V ? strct.V->getName() : StringRef("");
    };
    ++EmittedSetfield;
    assert(strct.ispointer());
    size_t byte_offset = jl_field_offset(sty, idx0);
    auto tbaa = best_field_tbaa(ctx, strct, sty, idx0, byte_offset);
    Value *addr = data_pointer(ctx, strct);
    if (byte_offset > 0) {
        addr = ctx.builder.CreateInBoundsGEP(
                getInt8Ty(ctx.builder.getContext()),
                emit_bitcast(ctx, addr, getInt8PtrTy(ctx.builder.getContext())),
                ConstantInt::get(ctx.types().T_size, byte_offset));
        setNameWithField(ctx.emission_context, addr, get_objname, sty, idx0, Twine("_ptr"));
    }
    jl_value_t *jfty = jl_field_type(sty, idx0);
    bool isboxed = jl_field_isptr(sty, idx0);
    if (!isboxed && jl_is_uniontype(jfty)) {
        size_t fsz1 = jl_field_size(sty, idx0) - 1;
        Value *ptindex = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()),
                emit_bitcast(ctx, addr, getInt8PtrTy(ctx.builder.getContext())),
                ConstantInt::get(ctx.types().T_size, fsz1));
        setNameWithField(ctx.emission_context, ptindex, get_objname, sty, idx0, Twine(".tindex_ptr"));
        return union_store(ctx, addr, ptindex, rhs, cmp, jfty, tbaa, ctx.tbaa().tbaa_unionselbyte,
            Order, FailOrder,
            needlock, issetfield, isreplacefield, isswapfield, ismodifyfield, issetfieldonce,
            modifyop, fname);
    }
    unsigned align = jl_field_align(sty, idx0);
    size_t nfields = jl_datatype_nfields(sty);
    bool maybe_null = idx0 >= nfields - (unsigned)sty->name->n_uninitialized;
    return typed_store(ctx, addr, rhs, cmp, jfty, tbaa, nullptr,
        wb ? boxed(ctx, strct) : nullptr,
        isboxed, Order, FailOrder, align,
        needlock, issetfield, isreplacefield, isswapfield, ismodifyfield, issetfieldonce,
        maybe_null, modifyop, fname, nullptr, nullptr);
}

static jl_cgval_t emit_new_struct(jl_codectx_t &ctx, jl_value_t *ty, size_t nargs, ArrayRef<jl_cgval_t> argv, bool is_promotable)
{
    ++EmittedNewStructs;
    assert(jl_is_datatype(ty));
    assert(jl_is_concrete_type(ty));
    jl_datatype_t *sty = (jl_datatype_t*)ty;
    auto arg_typename = [&] JL_NOTSAFEPOINT {
        return "new::" + std::string(jl_symbol_name((sty)->name->name));
    };
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

            Instruction *promotion_point = nullptr;
            ssize_t promotion_ssa = -1;
            Value *strct;
            if (type_is_ghost(lt)) {
                strct = NULL;
            }
            else if (init_as_value) {
                if (tracked.count) {
                    strct = Constant::getNullValue(lt);
                }
                else {
                    strct = UndefValue::get(lt);
                    if (nargs < nf)
                        strct = ctx.builder.CreateFreeze(strct);
                }
            }
            else {
                strct = emit_static_alloca(ctx, lt);
                setName(ctx.emission_context, strct, arg_typename);
                if (nargs < nf)
                    promotion_point = ctx.builder.CreateStore(ctx.builder.CreateFreeze(UndefValue::get(lt)), strct);
                if (tracked.count)
                    undef_derived_strct(ctx, strct, sty, ctx.tbaa().tbaa_stack);
            }

            for (unsigned i = 0; i < na; i++) {
                jl_value_t *jtype = jl_svecref(sty->types, i); // n.b. ty argument must be concrete
                jl_cgval_t fval_info = argv[i];

                IRBuilderBase::InsertPoint savedIP;
                emit_typecheck(ctx, fval_info, jtype, "new");
                fval_info = update_julia_type(ctx, fval_info, jtype);
                if (fval_info.typ == jl_bottom_type)
                    return jl_cgval_t();
                // TODO: Use (post-)domination instead.
                bool field_promotable = !jl_is_uniontype(jtype) && !init_as_value && fval_info.promotion_ssa != -1 &&
                    fval_info.promotion_point && fval_info.promotion_point->getParent() == ctx.builder.GetInsertBlock();
                if (field_promotable) {
                    savedIP = ctx.builder.saveIP();
                    ctx.builder.SetInsertPoint(fval_info.promotion_point);
                }
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
                    Instruction *inst;
                    dest = inst = cast<Instruction>(ctx.builder.CreateConstInBoundsGEP2_32(lt, strct, 0, llvm_idx));
                    // Our promotion point needs to come before
                    //  A) All of our arguments' promotion points
                    //  B) Any instructions we insert at any of our arguments' promotion points
                    // N.B.: Do not use Instruction::comesBefore here. LLVM invalidates its instruction numbering after
                    // every insert, so querying it here makes code generation accidentally quadartic.
                    if (field_promotable) {
                        if (promotion_ssa == -1 || fval_info.promotion_ssa < promotion_ssa) {
                            promotion_point = inst;
                            promotion_ssa = fval_info.promotion_ssa;
                        }
                    }
                    else if (!promotion_point) {
                        promotion_point = inst;
                    }
                }
                Value *fval = NULL;
                if (jl_field_isptr(sty, i)) {
                    fval = boxed(ctx, fval_info, field_promotable);
                    if (!init_as_value) {
                        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack);
                        StoreInst *SI = cast<StoreInst>(ai.decorateInst(
                                ctx.builder.CreateAlignedStore(fval, dest, Align(jl_field_align(sty, i)))));
                        SI->setOrdering(AtomicOrdering::Unordered);
                    }
                }
                else if (jl_is_uniontype(jtype)) {
                    // compute tindex from rhs
                    jl_cgval_t rhs_union = convert_julia_type(ctx, fval_info, jtype);
                    if (rhs_union.typ == jl_bottom_type)
                        return jl_cgval_t();
                    Value *tindex = compute_tindex_unboxed(ctx, rhs_union, jtype);
                    tindex = ctx.builder.CreateNUWSub(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 1));
                    size_t fsz = 0, al = 0;
                    bool isptr = !jl_islayout_inline(jtype, &fsz, &al);
                    assert(!isptr && fsz < jl_field_size(sty, i)); (void)isptr;
                    size_t fsz1 = jl_field_size(sty, i) - 1;
                    if (init_as_value) {
                        // If you wanted to implement init_as_value,
                        // would need to emit the union-move into temporary memory,
                        // then load it and combine with the tindex.
                        // But more efficient to just store it directly.
                        unsigned ptindex = convert_struct_offset(ctx, lt, offs + fsz1);
                        if (fsz1 > 0 && !fval_info.isghost) {
                            Type *ET = IntegerType::get(ctx.builder.getContext(), 8 * al);
                            assert(lt->getStructElementType(llvm_idx) == ET);
                            AllocaInst *lv = emit_static_alloca(ctx, ET);
                            setName(ctx.emission_context, lv, "unioninit");
                            lv->setOperand(0, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), (fsz1 + al - 1) / al));
                            emit_unionmove(ctx, lv, ctx.tbaa().tbaa_stack, fval_info, nullptr);
                            // emit all of the align-sized words
                            unsigned i = 0;
                            for (; i < fsz1 / al; i++) {
                                Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                                jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack);
                                Value *fldv = ai.decorateInst(ctx.builder.CreateAlignedLoad(ET, fldp, Align(al)));
                                strct = ctx.builder.CreateInsertValue(strct, fldv, ArrayRef<unsigned>(llvm_idx + i));
                            }
                            // emit remaining bytes up to tindex
                            if (i < ptindex - llvm_idx) {
                                Value *staddr = ctx.builder.CreateConstInBoundsGEP1_32(ET, lv, i);
                                staddr = ctx.builder.CreateBitCast(staddr, getInt8PtrTy(ctx.builder.getContext()));
                                for (; i < ptindex - llvm_idx; i++) {
                                    Value *fldp = ctx.builder.CreateConstInBoundsGEP1_32(getInt8Ty(ctx.builder.getContext()), staddr, i);
                                    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack);
                                    Value *fldv = ai.decorateInst(ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), fldp, Align(1)));
                                    strct = ctx.builder.CreateInsertValue(strct, fldv, ArrayRef<unsigned>(llvm_idx + i));
                                }
                            }
                        }
                        llvm_idx = ptindex;
                        fval = tindex;
                        if (jl_is_vecelement_type(ty))
                            fval = ctx.builder.CreateInsertValue(strct, fval, ArrayRef<unsigned>(llvm_idx));
                    }
                    else {
                        Value *ptindex = emit_struct_gep(ctx, lt, strct, offs + fsz1);
                        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_unionselbyte);
                        ai.decorateInst(ctx.builder.CreateAlignedStore(tindex, ptindex, Align(1)));
                        if (!rhs_union.isghost)
                            emit_unionmove(ctx, dest, ctx.tbaa().tbaa_stack, fval_info, nullptr);
                    }
                }
                else {
                    if (field_promotable) {
                        fval_info.V->replaceAllUsesWith(dest);
                        cast<Instruction>(fval_info.V)->eraseFromParent();
                    } else if (init_as_value) {
                        fval = emit_unbox(ctx, fty, fval_info, jtype);
                    } else {
                        emit_unbox_store(ctx, fval_info, dest, ctx.tbaa().tbaa_stack, jl_field_align(sty, i));
                    }
                }
                if (init_as_value) {
                    assert(fval);
                    if (jl_is_vecelement_type(ty))
                        strct = fval;  // VecElement type comes unwrapped in LLVM.
                    else if (lt->isVectorTy())
                        strct = ctx.builder.CreateInsertElement(strct, fval, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), llvm_idx));
                    else if (lt->isAggregateType())
                        strct = ctx.builder.CreateInsertValue(strct, fval, ArrayRef<unsigned>(llvm_idx));
                    else
                        assert(false);
                }
                if (field_promotable) {
                    ctx.builder.restoreIP(savedIP);
                }
            }
            for (size_t i = nargs; i < nf; i++) {
                if (!jl_field_isptr(sty, i) && jl_is_uniontype(jl_field_type(sty, i))) {
                    unsigned offs = jl_field_offset(sty, i);
                    int fsz = jl_field_size(sty, i) - 1;
                    unsigned llvm_idx = convert_struct_offset(ctx, cast<StructType>(lt), offs + fsz);
                    if (init_as_value)
                        strct = ctx.builder.CreateInsertValue(strct, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0), ArrayRef<unsigned>(llvm_idx));
                    else {
                        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_unionselbyte);
                        ai.decorateInst(ctx.builder.CreateAlignedStore(
                                ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0),
                                ctx.builder.CreateConstInBoundsGEP2_32(lt, strct, 0, llvm_idx),
                                Align(1)));
                    }
                }
            }
            if (type_is_ghost(lt))
                return mark_julia_const(ctx, sty->instance);
            else if (init_as_value)
                return mark_julia_type(ctx, strct, false, ty);
            else {
                jl_cgval_t ret = mark_julia_slot(strct, ty, NULL, ctx.tbaa().tbaa_stack);
                if (is_promotable && promotion_point) {
                    ret.promotion_point = promotion_point;
                    ret.promotion_ssa = promotion_ssa;
                }
                return ret;
            }
        }
        Value *strct = emit_allocobj(ctx, sty, nargs >= nf);
        setName(ctx.emission_context, strct, arg_typename);
        jl_cgval_t strctinfo = mark_julia_type(ctx, strct, true, ty);
        strct = decay_derived(ctx, strct);
        undef_derived_strct(ctx, strct, sty, strctinfo.tbaa);
        for (size_t i = nargs; i < nf; i++) {
            if (!jl_field_isptr(sty, i) && jl_is_uniontype(jl_field_type(sty, i))) {
                jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_unionselbyte);
                ai.decorateInst(ctx.builder.CreateAlignedStore(
                        ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0),
                        ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), emit_bitcast(ctx, strct, getInt8PtrTy(ctx.builder.getContext())),
                                ConstantInt::get(ctx.types().T_size, jl_field_offset(sty, i) + jl_field_size(sty, i) - 1)),
                        Align(1)));
            }
        }
        // TODO: verify that nargs <= nf (currently handled by front-end)
        for (size_t i = 0; i < nargs; i++) {
            jl_cgval_t rhs = argv[i];
            bool need_wb; // set to true if the store might cause the allocation of a box newer than the struct
            if (jl_field_isptr(sty, i))
                need_wb = !rhs.isboxed;
            else
                need_wb = false;
            jl_value_t *ft = jl_svecref(sty->types, i);
            emit_typecheck(ctx, rhs, ft, "new"); // n.b. ty argument must be concrete
            rhs = update_julia_type(ctx, rhs, ft);
            if (rhs.typ == jl_bottom_type)
                return jl_cgval_t();
            emit_setfield(ctx, sty, strctinfo, i, rhs, jl_cgval_t(), need_wb, AtomicOrdering::NotAtomic, AtomicOrdering::NotAtomic, nullptr, true, false, false, false, false, nullptr, "new");
        }
        return strctinfo;
    }
    else {
        // 0 fields, ghost or primitive type
        if (jl_datatype_nbits(sty) == 0)
            return ghostValue(ctx, sty);
        // n.b. this is not valid IR form to construct a primitive type (use bitcast for example)
        bool isboxed;
        Type *lt = julia_type_to_llvm(ctx, ty, &isboxed);
        assert(!isboxed);
        return mark_julia_type(ctx, ctx.builder.CreateFreeze(UndefValue::get(lt)), false, ty);
    }
}

static void emit_signal_fence(jl_codectx_t &ctx)
{
    emit_signal_fence(ctx.builder);
}

static Value *emit_defer_signal(jl_codectx_t &ctx)
{
    ++EmittedDeferSignal;
    Value *ptls = emit_bitcast(ctx, get_current_ptls(ctx),
                               PointerType::get(ctx.types().T_sigatomic, 0));
    Constant *offset = ConstantInt::getSigned(getInt32Ty(ctx.builder.getContext()),
            offsetof(jl_tls_states_t, defer_signal) / sizeof(sig_atomic_t));
    return ctx.builder.CreateInBoundsGEP(ctx.types().T_sigatomic, ptls, ArrayRef<Value*>(offset), "jl_defer_signal");
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
           (a->safepoint_on_entry == b->safepoint_on_entry) &&
           (a->gcstack_arg == b->gcstack_arg) &&
           (a->use_jlplt == b->use_jlplt) &&
           (a->lookup == b->lookup);
}
#endif

static jl_cgval_t _emit_memoryref(jl_codectx_t &ctx, Value *mem, Value *data, const jl_datatype_layout_t *layout, jl_value_t *typ)
{
    //jl_cgval_t argv[] = {
    //    mark_julia_type(ctx, mem, true, jl_any_type),
    //    mark_julia_type(ctx, data, false, jl_voidpointer_type)
    //};
    //return emit_new_struct(ctx, typ, 3, argv);
    Value *ref = Constant::getNullValue(get_memoryref_type(ctx.builder.getContext(), ctx.types().T_size, layout, 0));
    ref = ctx.builder.CreateInsertValue(ref, data, 0);
    ref = ctx.builder.CreateInsertValue(ref, mem, 1);
    return mark_julia_type(ctx, ref, false, typ);
}

static jl_cgval_t _emit_memoryref(jl_codectx_t &ctx, const jl_cgval_t &mem, const jl_datatype_layout_t *layout, jl_value_t *typ)
{
    bool isboxed = layout->flags.arrayelem_isboxed;
    bool isunion = layout->flags.arrayelem_isunion;
    bool isghost = layout->size == 0;
    Value *data = (!isboxed && isunion) || isghost ? ConstantInt::get(ctx.types().T_size, 0) : emit_genericmemoryptr(ctx, boxed(ctx, mem), layout, 0);
    return _emit_memoryref(ctx, boxed(ctx, mem), data, layout, typ);
}

static Value *emit_memoryref_FCA(jl_codectx_t &ctx, const jl_cgval_t &ref, const jl_datatype_layout_t *layout)
{
    if (ref.ispointer()) {
        LLVMContext &C = ctx.builder.getContext();
        Type *type = get_memoryref_type(C, ctx.types().T_size, layout, 0);
        LoadInst *load = ctx.builder.CreateLoad(type, emit_bitcast(ctx, data_pointer(ctx, ref), PointerType::get(type, 0)));
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ref.tbaa);
        ai.decorateInst(load);
        return load;
    }
    else {
        return ref.V;
    }
}

static jl_cgval_t emit_memoryref(jl_codectx_t &ctx, const jl_cgval_t &ref, jl_cgval_t idx, jl_value_t *inbounds, const jl_datatype_layout_t *layout)
{
    ++EmittedArrayNdIndex;
    emit_typecheck(ctx, idx, (jl_value_t*)jl_long_type, "memoryref");
    idx = update_julia_type(ctx, idx, (jl_value_t*)jl_long_type);
    if (idx.typ == jl_bottom_type)
        return jl_cgval_t();
    Value *V = emit_memoryref_FCA(ctx, ref, layout);
    Value *data = CreateSimplifiedExtractValue(ctx, V, 0);
    Value *mem = CreateSimplifiedExtractValue(ctx, V, 1);
    Value *i = emit_unbox(ctx, ctx.types().T_size, idx, (jl_value_t*)jl_long_type);
    Value *offset = ctx.builder.CreateSub(i, ConstantInt::get(ctx.types().T_size, 1));
    Value *elsz = emit_genericmemoryelsize(ctx, mem, ref.typ, false);
    bool bc = bounds_check_enabled(ctx, inbounds);
#if 1
    Value *ovflw = nullptr;
#endif
    Value *newdata;
    bool isboxed = layout->flags.arrayelem_isboxed;
    bool isunion = layout->flags.arrayelem_isunion;
    bool isghost = layout->size == 0;
    if ((!isboxed && isunion) || isghost) {
        newdata = ctx.builder.CreateAdd(data, offset);
        if (bc) {
            BasicBlock *failBB, *endBB;
            failBB = BasicBlock::Create(ctx.builder.getContext(), "oob");
            endBB = BasicBlock::Create(ctx.builder.getContext(), "idxend");
            Value *mlen = emit_genericmemorylen(ctx, mem, ref.typ);
            Value *inbound = ctx.builder.CreateICmpULT(newdata, mlen);
            ctx.builder.CreateCondBr(inbound, endBB, failBB);
            failBB->insertInto(ctx.f);
            ctx.builder.SetInsertPoint(failBB);
            ctx.builder.CreateCall(prepare_call(jlboundserror_func),
                { mark_callee_rooted(ctx, boxed(ctx, ref)), i });
            ctx.builder.CreateUnreachable();
            endBB->insertInto(ctx.f);
            ctx.builder.SetInsertPoint(endBB);
        }
    }
    else {
        Value *boffset;
#if 0
        if (bc) {
            auto *MulF = Intrinsic::getDeclaration(jl_Module, Intrinsic::smul_with_overflow, offset->getType());
            CallInst *Mul = ctx.builder.CreateCall(MulF, {offset, elsz});
            boffset = ctx.builder.CreateExtractValue(Mul, 0);
            ovflw = ctx.builder.CreateExtractValue(Mul, 1);
        }
        else
#else
        if (bc) {
            // n.b. we could boundscheck that -len<=offset<=len instead of using smul.ovflw,
            // since we know that len*elsz does not overflow,
            // and we can further rearrange that as ovflw = !( offset+len < len+len ) as unsigned math
            Value *mlen = emit_genericmemorylen(ctx, mem, ref.typ);
            ovflw = ctx.builder.CreateICmpUGE(ctx.builder.CreateAdd(offset, mlen), ctx.builder.CreateNUWAdd(mlen, mlen));
        }
#endif
            boffset = ctx.builder.CreateMul(offset, elsz);
#if 0 // TODO: if opaque-pointers?
        newdata = emit_bitcast(ctx, data, getInt8PtrTy(ctx.builder.getContext()));
        newdata = ctx.builder.CreateGEP(getInt8Ty(ctx.builder.getContext()), newdata, boffset);
#else
        Type *elty = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, jl_tparam1(ref.typ));
        newdata = emit_bitcast(ctx, data, elty->getPointerTo(0));
        newdata = ctx.builder.CreateInBoundsGEP(elty, newdata, offset);
        (void)boffset; // LLVM is very bad at handling GEP with types different from the load
#endif
        newdata = emit_bitcast(ctx, newdata, data->getType());
        if (bc) {
            BasicBlock *failBB, *endBB;
            failBB = BasicBlock::Create(ctx.builder.getContext(), "oob");
            endBB = BasicBlock::Create(ctx.builder.getContext(), "idxend");
            Value *mlen = emit_genericmemorylen(ctx, mem, ref.typ);
            Value *mptr = emit_genericmemoryptr(ctx, mem, layout, 0);
            mptr = emit_bitcast(ctx, mptr, newdata->getType());
#if 0
            Value *mend = emit_bitcast(ctx, mptr, getInt8PtrTy(ctx.builder.getContext()));
            Value *blen = ctx.builder.CreateMul(mlen, elsz, "", true, true);
            mend = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), mend, blen);
            mend = emit_bitcast(ctx, mend, newdata->getType());
            Value *inbound = ctx.builder.CreateAnd(
                    ctx.builder.CreateICmpULE(mptr, newdata),
                    ctx.builder.CreateICmpULT(newdata, mend));
            inbound = ctx.builder.CreateAnd(
                    ctx.builder.CreateNot(ovflw),
                    inbound);
#elif 1
            Value *bidx0 = ctx.builder.CreateSub(
                ctx.builder.CreatePtrToInt(newdata, ctx.types().T_size),
                ctx.builder.CreatePtrToInt(mptr, ctx.types().T_size));
            Value *blen = ctx.builder.CreateMul(mlen, elsz, "", true, true);
            Value *inbound = ctx.builder.CreateICmpULT(bidx0, blen);
            inbound = ctx.builder.CreateAnd(ctx.builder.CreateNot(ovflw), inbound);
#else
            Value *idx0; // (newdata - mptr) / elsz
            idx0 = ctx.builder.CreateSub(
                ctx.builder.CreatePtrToInt(newdata, ctx.types().T_size),
                ctx.builder.CreatePtrToInt(mptr, ctx.types().T_size));
            idx0 = ctx.builder.CreateExactUDiv(idx0, elsz);
            Value *inbound = ctx.builder.CreateICmpULT(idx0, mlen);
#endif
            ctx.builder.CreateCondBr(inbound, endBB, failBB);
            failBB->insertInto(ctx.f);
            ctx.builder.SetInsertPoint(failBB);
            ctx.builder.CreateCall(prepare_call(jlboundserror_func),
                { mark_callee_rooted(ctx, boxed(ctx, ref)), i });
            ctx.builder.CreateUnreachable();
            endBB->insertInto(ctx.f);
            ctx.builder.SetInsertPoint(endBB);
        }
    }
    return _emit_memoryref(ctx, mem, newdata, layout, ref.typ);
}

static jl_cgval_t emit_memoryref_offset(jl_codectx_t &ctx, const jl_cgval_t &ref, const jl_datatype_layout_t *layout)
{
    Value *offset;
    Value *V = emit_memoryref_FCA(ctx, ref, layout);
    Value *data = CreateSimplifiedExtractValue(ctx, V, 0);
    if (layout->flags.arrayelem_isunion || layout->size == 0) {
        offset = data;
    }
    else {
        Value *mem = CreateSimplifiedExtractValue(ctx, V, 1);
        Value *mptr = emit_genericmemoryptr(ctx, mem, layout, 0);
        mptr = emit_bitcast(ctx, mptr, mem->getType());
        // (data - mptr) / elsz
        offset = ctx.builder.CreateSub(
            ctx.builder.CreatePtrToInt(data, ctx.types().T_size),
            ctx.builder.CreatePtrToInt(mptr, ctx.types().T_size));
        Value *elsz = emit_genericmemoryelsize(ctx, mem, ref.typ, false);
        offset = ctx.builder.CreateExactUDiv(offset, elsz);
    }
    offset = ctx.builder.CreateAdd(offset, ConstantInt::get(ctx.types().T_size, 1));
    return mark_julia_type(ctx, offset, false, jl_long_type);
}

static Value *emit_memoryref_mem(jl_codectx_t &ctx, const jl_cgval_t &ref, const jl_datatype_layout_t *layout)
{
    Value *V = emit_memoryref_FCA(ctx, ref, layout);
    return CreateSimplifiedExtractValue(ctx, V, 1);
}

static Value *emit_memoryref_ptr(jl_codectx_t &ctx, const jl_cgval_t &ref, const jl_datatype_layout_t *layout)
{
    assert(!layout->flags.arrayelem_isunion && layout->size != 0);
    Value *newref = emit_memoryref_FCA(ctx, ref, layout);
    Value *data = CreateSimplifiedExtractValue(ctx, newref, 0);
    unsigned AS = AddressSpace::Loaded;
    Value *mem = CreateSimplifiedExtractValue(ctx, newref, 1);
    // rebuild GEP on data, so that we manually hoist this gc_loaded_func call over it, back to the original load
    // we should add this to llvm-julia-licm too, so we can attempt hoisting over PhiNodes too (which aren't defined yet here)
    IRBuilder<>::InsertPointGuard resetIP(ctx.builder);
    SmallVector<GetElementPtrInst*,0> GEPlist;
    data = data->stripPointerCastsSameRepresentation();
    while (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(data)) { // ignoring bitcast will not be required with opaque pointers
        GEPlist.push_back(GEP);
        data = GEP->getPointerOperand()->stripPointerCastsSameRepresentation();
    }
    data = ctx.builder.CreateBitCast(data, ctx.types().T_pprjlvalue);
    data = ctx.builder.CreateCall(prepare_call(gc_loaded_func), { mem, data });
    if (!GEPlist.empty()) {
        for (auto &GEP : make_range(GEPlist.rbegin(), GEPlist.rend())) {
            data = ctx.builder.CreateBitCast(data, PointerType::get(GEP->getSourceElementType(), AS));
            Instruction *GEP2 = GEP->clone();
            GEP2->mutateType(PointerType::get(GEP->getResultElementType(), AS));
            GEP2->setOperand(GetElementPtrInst::getPointerOperandIndex(), data);
            ctx.builder.Insert(GEP2);
            data = GEP2;
        }
    }
    return data;
}

jl_value_t *jl_fptr_wait_for_compiled(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    // This relies on the invariant that the JIT will have set the invoke ptr
    // by the time it releases the codegen lock. If the code is refactored to make the
    // codegen lock more fine-grained, this will have to be replaced by a per-codeinstance futex.
    size_t nthreads = jl_atomic_load_acquire(&jl_n_threads);
    // This should only be possible if there's more than one thread. If not, either there's a bug somewhere
    // that resulted in this not getting cleared, or we're about to deadlock. Either way, that's bad.
    if (nthreads == 1) {
        jl_error("Internal error: Reached jl_fptr_wait_for_compiled in single-threaded execution.");
    }
    JL_LOCK(&jl_codegen_lock);
    JL_UNLOCK(&jl_codegen_lock);
    return jl_atomic_load_acquire(&m->invoke)(f, args, nargs, m);
}

// Reset us back to codegen debug type
#undef DEBUG_TYPE
#define DEBUG_TYPE "julia_irgen_codegen"
