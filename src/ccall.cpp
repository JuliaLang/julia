// --- the ccall intrinsic ---

// --- library symbol lookup ---

// map from "libX" to full soname "libX.so.ver"
#if defined(__linux__)
static std::map<std::string, std::string> sonameMap;
static bool got_sonames = false;

static void read_sonames()
{
    char *line=NULL;
    size_t sz=0;
    FILE *ldc = popen("/sbin/ldconfig -p", "r");

    while (!feof(ldc)) {
        ssize_t n = getline(&line, &sz, ldc);
        if (n == -1)
            break;
        if (n > 2 && isspace(line[0])) {
            int i=0;
            while (isspace(line[++i])) ;
            char *name = &line[i];
            char *dot = strstr(name, ".so");
            char *nxt = strchr(name, ' ');
            if (dot != NULL && nxt != NULL) {
                std::string pfx(name, dot - name);
                std::string soname(name, nxt - name);
                sonameMap[pfx] = soname;
            }
        }
    }

    free(line);
    pclose(ldc);
}

extern "C" const char *jl_lookup_soname(char *pfx, size_t n)
{
    if (!got_sonames) {
        read_sonames();
        got_sonames = true;
    }
    std::string str(pfx, n);
    if (sonameMap.find(str) != sonameMap.end()) {
        return sonameMap[str].c_str();
    }
    return NULL;
}
#endif

// map from user-specified lib names to handles
static std::map<std::string, void*> libMap;

int add_library_mapping(char *lib, void *hnd)
{
    if (libMap[lib] == NULL && hnd != NULL) {
        libMap[lib] = hnd;
        return 0;
    }
    else {
        return -1;
    }
}

static void *add_library_sym(char *name, char *lib)
{
    void *hnd;
    if (lib == NULL) {
        hnd = jl_dl_handle;
    }
    else {
        hnd = libMap[lib];
        if (hnd == NULL) {
            hnd = jl_load_dynamic_library(lib, JL_RTLD_DEFAULT);
            if (hnd != NULL)
            libMap[lib] = hnd;
            else
                return NULL;
        }
    }
    // add a symbol->address mapping for the JIT
    void *sval = jl_dlsym_e((uv_lib_t*)hnd, name);
    if (lib != NULL && hnd != jl_dl_handle) {
        void *exist = sys::DynamicLibrary::SearchForAddressOfSymbol(name);
        if (exist != NULL && exist != sval &&
            // openlibm conflicts with libm, and lots of our libraries
            // (including LLVM) link to libm. fortunately AddSymbol() is
            // able to resolve these in favor of openlibm, but this could
            // be an issue in the future (TODO).
            strcmp(lib,"libopenlibm")) {
            ios_printf(ios_stderr, "Warning: Possible conflict in library symbol %s\n", name);
        }
        sys::DynamicLibrary::AddSymbol(name, sval);
    }
    return sval;
}

// --- ABI Implementations ---
// Partially based on the LDC ABI implementations licensed under the BSD 3-clause license

#define ABI_X86_64 1

#if ABI_X86_64

// used to track the state of the ABI generator during
// code generation
struct AbiState {
    unsigned char int_regs, sse_regs;
};

const AbiState default_abi_state = {6,8};

enum ArgClass { Integer, Sse, SseUp, X87, X87Up, ComplexX87, NoClass, Memory };

struct Classification {
    bool isMemory;
    ArgClass classes[2];

    Classification() : isMemory(false) {
        classes[0] = NoClass;
        classes[1] = NoClass;
    }

    void addField(unsigned offset, ArgClass cl) {
        if (isMemory)
            return;

        // Note that we don't need to bother checking if it crosses 8 bytes.
        // We don't get here with unaligned fields, and anything that can be
        // big enough to cross 8 bytes (cdoubles, reals, structs and arrays)
        // is special-cased in classifyType()
        int idx = (offset < 8 ? 0 : 1);

        ArgClass nw = merge(classes[idx], cl);
        if (nw != classes[idx]) {
            classes[idx] = nw;

            if (nw == Memory) {
                classes[1-idx] = Memory;
                isMemory = true;
            }
        }
    }

    private:
        ArgClass merge(ArgClass accum, ArgClass cl) {
            if (accum == cl)
                return accum;
            if (accum == NoClass)
                return cl;
            if (cl == NoClass)
                return accum;
            if (accum == Memory || cl == Memory)
                return Memory;
            if (accum == Integer || cl == Integer)
                return Integer;
            if (accum == X87 || accum == X87Up || accum == ComplexX87 ||
                cl == X87 || cl == X87Up || cl == ComplexX87)
                return Memory;
            return Sse;
        }
};

/*else if (ty == jl_float80_type) { //if this is ever added
        accum.addField(offset, X87);
        accum.addField(offset+8, X87Up);
    } else if (ty->ty == Tcomplex80) {
        accum.addField(offset, ComplexX87);
        // make sure other half knows about it too:
        accum.addField(offset+16, ComplexX87);
    } */ 
void classifyType(Classification& accum, jl_value_t* ty, uint64_t offset) {
    if (jl_is_cpointer_type(ty)) {
        accum.addField(offset, Integer);
    } else if (jl_is_bitstype(ty) && jl_datatype_size(ty) == 16) {
        // Int128 or other 128bit wide INTEGER types
        accum.addField(offset, Integer);
        accum.addField(offset+8, Integer);
    } 
    // Floating point types
    else if (ty == (jl_value_t*)jl_float64_type || ty == (jl_value_t*)jl_float32_type) {
        accum.addField(offset, Sse);
    }
    // Other integer types
    else if (jl_is_bitstype(ty))
    {
        if(jl_datatype_size(ty) > 8)
            jl_error("Bitstype of this size not supported in the C ABI");
        accum.addField(offset,Integer);
    } else if (jl_isbits(ty) && jl_datatype_size(ty) > 16) {
        // This isn't creal, yet is > 16 bytes, so pass in memory.
        // Must be after creal case but before arrays and structs,
        // the other types that can get bigger than 16 bytes
        accum.addField(offset, Memory);
    } else if (jl_is_array_type(ty)) {
        jl_value_t* eltType = jl_tparam0(ty);
        assert(jl_isbits(eltType));
        uint64_t eltsize = jl_datatype_size(eltType);
        if (eltsize > 0) {
            uint16_t dim = 0;
            for (int i = 0; i<jl_array_ndims(ty); ++i)
                dim += jl_array_dim(ty,i);
            assert(dim <= 16
                    && "Array of non-empty type <= 16 bytes but > 16 elements?");
            for (int i = 0; i < dim; i++) {
                classifyType(accum, eltType, offset);
                offset += eltsize;
            }
        }
    } else if (jl_is_structtype(ty)) {
        int remaining_size = jl_datatype_size(ty);
        int i = 0;
        while(true) {
            classifyType(accum, jl_tupleref(((jl_datatype_t*)ty)->types,i), offset + jl_field_offset(ty,i));
            remaining_size -= jl_field_size(ty,i);
            i++;
            if(remaining_size <= 0)
                break;
        }
    } else {
        jl_error("Unsupported type in C ABI");
    }
}

Classification classify(jl_value_t* ty) {
    Classification cl;
    classifyType(cl, ty, 0);
    return cl;
}

bool use_sret(AbiState *state,jl_value_t *ty)
{
    int sret = classify(ty).isMemory;
    if(sret) {
        assert(state->int_regs>0 && "WTF? No int regs available?");
        state->int_regs--;
    }
    return sret;
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg)
{
    Classification cl = classify(ty);
    if (cl.isMemory) {
        *byRef = true;
    }
        

    // Figure out how many registers we want for this arg:
    AbiState wanted = { 0, 0 };
    for (int i = 0 ; i < 2; i++) {
        if (cl.classes[i] == Integer)
            wanted.int_regs++;
        else if (cl.classes[i] == Sse)
            wanted.sse_regs++;
    }

    if (wanted.int_regs <= state->int_regs && wanted.sse_regs <= state->sse_regs) {
        state->int_regs -= wanted.int_regs;
        state->sse_regs -= wanted.sse_regs;
        *inReg = true;
    }else if(jl_is_structtype(ty))
    {
        *byRef = true;
    }
}

#elif ABI_WIN64

#elif ABI_86

#endif

// --- argument passing and scratch space utilities ---

static Function *value_to_pointer_func;

// TODO: per-thread
static char *temp_arg_area;
static const uint32_t arg_area_sz = 4196;
static uint32_t arg_area_loc;
#define N_TEMP_ARG_BLOCKS 1024
static void *temp_arg_blocks[N_TEMP_ARG_BLOCKS];
static uint32_t arg_block_n = 0;
static Function *save_arg_area_loc_func;
static Function *restore_arg_area_loc_func;

static uint64_t save_arg_area_loc()
{
    return (((uint64_t)arg_block_n)<<32) | ((uint64_t)arg_area_loc);
}

static void restore_arg_area_loc(uint64_t l)
{
    arg_area_loc = l&0xffffffff;
    uint32_t ab = l>>32;
    while (arg_block_n > ab) {
        arg_block_n--;
        free(temp_arg_blocks[arg_block_n]);
    }
}

static void *alloc_temp_arg_space(uint32_t sz)
{
    void *p;
    if (arg_area_loc+sz > arg_area_sz) {
#ifdef JL_GC_MARKSWEEP
        if (arg_block_n >= N_TEMP_ARG_BLOCKS)
            jl_error("internal compiler error: out of temporary argument space in ccall");
        p = malloc(sz);
        temp_arg_blocks[arg_block_n++] = p;
#else
#error "fixme"
#endif
    }
    else {
        p = &temp_arg_area[arg_area_loc];
        arg_area_loc += sz;
    }
    return p;
}

static void *alloc_temp_arg_copy(void *obj, uint32_t sz)
{
    void *p = alloc_temp_arg_space(sz);
    memcpy(p, obj, sz);
    return p;
}

// this is a run-time function
// warning: cannot allocate memory except using alloc_temp_arg_space
extern "C" void *jl_value_to_pointer(jl_value_t *jt, jl_value_t *v, int argn,
                                     int addressof)
{
    jl_value_t *jvt = (jl_value_t*)jl_typeof(v);
    if (addressof) {
        if (jvt == jt) {
            if (jl_is_bitstype(jvt)) {
                size_t osz = jl_datatype_size(jt);
                return alloc_temp_arg_copy(jl_data_ptr(v), osz);
            }
            else if (!jl_is_tuple(jvt) && jl_is_leaf_type(jvt) && !jl_is_array_type(jvt)) {
                return v + 1;
            }
        }
        goto value_to_pointer_error;
    }
    else {
        if (jl_is_cpointer_type(jvt) && jl_tparam0(jvt) == jt) {
            return (void*)jl_unbox_voidpointer(v);
        }
    }

    if (((jl_value_t*)jl_uint8_type == jt ||
         (jl_value_t*)jl_int8_type == jt) && jl_is_byte_string(v)) {
        return jl_string_data(v);
    }
    if (jl_is_array_type(jvt)) {
        if (jl_tparam0(jl_typeof(v)) == jt || jt==(jl_value_t*)jl_bottom_type)
            return ((jl_array_t*)v)->data;
        if (jl_is_cpointer_type(jt)) {
            jl_array_t *ar = (jl_array_t*)v;
            void **temp=(void**)alloc_temp_arg_space(jl_array_len(ar)*sizeof(void*));
            size_t i;
            for(i=0; i < jl_array_len(ar); i++) {
                temp[i] = jl_value_to_pointer(jl_tparam0(jt),
                                              jl_arrayref(ar, i), argn, 0);
            }
            return temp;
        }
    }

 value_to_pointer_error:
    std::map<int, std::string>::iterator it = argNumberStrings.find(argn);
    if (it == argNumberStrings.end()) {
        std::stringstream msg;
        msg << "argument ";
        msg << argn;
        argNumberStrings[argn] = msg.str();
        it = argNumberStrings.find(argn);
    }
    jl_value_t *targ=NULL, *pty=NULL;
    JL_GC_PUSH2(&targ, &pty);
    targ = (jl_value_t*)jl_tuple1(jt);
    pty = (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type,
                                     (jl_tuple_t*)targ);
    jl_type_error_rt("ccall", (*it).second.c_str(), pty, v);
    // doesn't return
    return (jl_value_t*)jl_null;
}

static Value *julia_to_native(Type *ty, jl_value_t *jt, Value *jv,
                              jl_value_t *argex, bool addressOf,
                              bool byRef, bool inReg,
                              int argn, jl_codectx_t *ctx,
                              bool *mightNeedTempSpace)
{
    Type *vt = jv->getType();

    // We're passing any
    if (ty == jl_pvalue_llvmt) {
        return boxed(jv);
    }
    else if (ty == vt && !addressOf && !byRef) {
        return jv;
    }
    else if (vt != jl_pvalue_llvmt) {
        // argument value is unboxed
        if (addressOf || (byRef && inReg)) {
            if (ty->isPointerTy() && ty->getContainedType(0)==vt) {
                // pass the address of an alloca'd thing, not a box
                // since those are immutable.
                Value *slot = builder.CreateAlloca(vt);
                builder.CreateStore(jv, slot);
                return builder.CreateBitCast(slot, ty);
            }
        }
        else if ((vt->isIntegerTy() && ty->isIntegerTy()) ||
                 (vt->isFloatingPointTy() && ty->isFloatingPointTy()) ||
                 (vt->isPointerTy() && ty->isPointerTy())) {
            if (vt->getPrimitiveSizeInBits() ==
                ty->getPrimitiveSizeInBits()) {
                return builder.CreateBitCast(jv, ty);
            }
        }
        // error. box for error handling.
        jv = boxed(jv);
    }
    else if (jl_is_cpointer_type(jt)) {
        assert(ty->isPointerTy());
        jl_value_t *aty = expr_type(argex, ctx);
        if (jl_is_array_type(aty) &&
            (jl_tparam0(jt) == jl_tparam0(aty) ||
             jl_tparam0(jt) == (jl_value_t*)jl_bottom_type)) {
            // array to pointer
            return builder.CreateBitCast(emit_arrayptr(jv), ty);
        }
        if (aty == (jl_value_t*)jl_ascii_string_type || aty == (jl_value_t*)jl_utf8_string_type) {
            return builder.CreateBitCast(emit_arrayptr(emit_nthptr(jv,1)), ty);
        }
        if (jl_is_structtype(aty) && jl_is_leaf_type(aty) && !jl_is_array_type(aty)) {
            if (!addressOf) {
                emit_error("ccall: expected & on argument", ctx);
                return literal_pointer_val(jl_nothing);
            }
            return builder.CreateBitCast(emit_nthptr_addr(jv, (size_t)1), ty); // skip type tag field
        }
        *mightNeedTempSpace = true;
        Value *p = builder.CreateCall4(value_to_pointer_func,
                                       literal_pointer_val(jl_tparam0(jt)), jv,
                                       ConstantInt::get(T_int32, argn),
                                       ConstantInt::get(T_int32, (int)addressOf));
        return builder.CreateBitCast(p, ty);
    }
    else if (jl_is_structtype(jt)) {
        if (addressOf)
            jl_error("ccall: unexpected & on argument"); // the only "safe" thing to emit here is the expected struct
        assert (ty->isStructTy() && (Type*)((jl_datatype_t*)jt)->struct_decl == ty);
        jl_value_t *aty = expr_type(argex, ctx);
        if (aty != jt) {
            std::stringstream msg;
            msg << "ccall argument ";
            msg << argn;
            emit_typecheck(jv, jt, msg.str(), ctx);
        }
        //TODO: check instead that prefix matches
        //if (!jl_is_structtype(aty))
        //    emit_typecheck(emit_typeof(jv), (jl_value_t*)jl_struct_kind, "ccall: Struct argument called with something that isn't a struct", ctx);
        // //safe thing would be to also check that jl_typeof(aty)->size > sizeof(ty) here and/or at runtime
        Value *pjv = builder.CreateBitCast(emit_nthptr_addr(jv, (size_t)1), PointerType::get(ty,0));
        if(byRef)
            return pjv;
        else 
            builder.CreateLoad(pjv, false);
    }
    // TODO: error for & with non-pointer argument type
    assert(jl_is_bitstype(jt));
    std::stringstream msg;
    msg << "ccall argument ";
    msg << argn;
    emit_typecheck(jv, jt, msg.str(), ctx);
    Value *p = data_pointer(jv);
    return builder.CreateLoad(builder.CreateBitCast(p,
                                                    PointerType::get(ty,0)),
                              false);
}

static jl_value_t *jl_signed_type=NULL;

typedef struct {
    Value *jl_ptr;  // if the argument is a run-time computed pointer
    void *fptr;     // if the argument is a constant pointer
    char *f_name;   // if the symbol name is known
    char *f_lib;    // if a library name is specified
} native_sym_arg_t;

// --- parse :sym or (:sym, :lib) argument into address info ---
static native_sym_arg_t interpret_symbol_arg(jl_value_t *arg, jl_codectx_t *ctx, const char *fname)
{
    jl_value_t *ptr = NULL;
    Value *jl_ptr=NULL;

    ptr = static_eval(arg, ctx, true);
    if (ptr == NULL) {
        jl_value_t *ptr_ty = expr_type(arg, ctx);
        Value *arg1 = emit_unboxed(arg, ctx);
        if (!jl_is_cpointer_type(ptr_ty)) {
            emit_cpointercheck(arg1, 
                               !strcmp(fname,"ccall") ?
                               "ccall: first argument not a pointer or valid constant expression" :
                               "cglobal: first argument not a pointer or valid constant expression",
                               ctx);
        }
        jl_ptr = emit_unbox(T_size, T_psize, arg1);
    }

    void *fptr=NULL;
    char *f_name=NULL, *f_lib=NULL;
    if (ptr != NULL) {
        if (jl_is_tuple(ptr) && jl_tuple_len(ptr)==1) {
            ptr = jl_tupleref(ptr,0);
        }
        if (jl_is_symbol(ptr))
            f_name = ((jl_sym_t*)ptr)->name;
        else if (jl_is_byte_string(ptr))
            f_name = jl_string_data(ptr);
        if (f_name != NULL) {
            // just symbol, default to JuliaDLHandle
#ifdef _OS_WINDOWS_
         //TODO: store the f_lib name instead of fptr
        fptr = jl_dlsym_win32(f_name);
#else
            // will look in process symbol table
#endif
        }
        else if (jl_is_cpointer_type(jl_typeof(ptr))) {
            fptr = *(void**)jl_data_ptr(ptr);
        }
        else if (jl_is_tuple(ptr) && jl_tuple_len(ptr)>1) {
            jl_value_t *t0 = jl_tupleref(ptr,0);
            jl_value_t *t1 = jl_tupleref(ptr,1);
            if (jl_is_symbol(t0))
                f_name = ((jl_sym_t*)t0)->name;
            else if (jl_is_byte_string(t0))
                f_name = jl_string_data(t0);
            else
                JL_TYPECHKS(fname, symbol, t0);
            if (jl_is_symbol(t1))
                f_lib = ((jl_sym_t*)t1)->name;
            else if (jl_is_byte_string(t1))
                f_lib = jl_string_data(t1);
            else
                JL_TYPECHKS(fname, symbol, t1);
        }
        else {
            JL_TYPECHKS(fname, pointer, ptr);
        }
    }
    native_sym_arg_t r;
    r.jl_ptr = jl_ptr;
    r.fptr = fptr;
    r.f_name = f_name;
    r.f_lib = f_lib;
    return r;
}


#ifdef LLVM33
    typedef AttributeSet attr_type;
#else 
    typedef AttrListPtr attr_type;
#endif

// --- code generator for cglobal ---

static Value *emit_cglobal(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGS(cglobal, 1, 2);
    jl_value_t *rt=NULL;
    Value *res;
    JL_GC_PUSH1(&rt);

    if (nargs == 2) {
        rt = jl_interpret_toplevel_expr_in(ctx->module, args[2],
                                           &jl_tupleref(ctx->sp,0),
                                           jl_tuple_len(ctx->sp)/2);
        JL_TYPECHK(cglobal, type, rt);
        rt = (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type, jl_tuple1(rt));
    }
    else {
        rt = (jl_value_t*)jl_voidpointer_type;
    }
    Type *lrt = julia_type_to_llvm(rt);
    if (lrt == NULL) lrt = T_pint8;

    native_sym_arg_t sym = interpret_symbol_arg(args[1], ctx, "cglobal");

    if (sym.jl_ptr != NULL) {
        res = builder.CreateIntToPtr(sym.jl_ptr, lrt);
    }
    else if (sym.fptr != NULL) {
        res = literal_pointer_val(sym.fptr, lrt);
    }
    else {
        void *symaddr;
        if (sym.f_lib != NULL)
            symaddr = add_library_sym(sym.f_name, sym.f_lib);
        else
            symaddr = sys::DynamicLibrary::SearchForAddressOfSymbol(sym.f_name);
        if (symaddr == NULL) {
            std::stringstream msg;
            msg << "cglobal: could not find symbol ";
            msg << sym.f_name;
            if (sym.f_lib != NULL) {
                msg << " in library ";
                msg << sym.f_lib;
            }
            emit_error(msg.str(), ctx);
            res = literal_pointer_val(NULL, lrt);
        }
        else {
            res = jl_Module->getOrInsertGlobal(sym.f_name,
                                               lrt->getContainedType(0));
        }
    }

    JL_GC_POP();
    return mark_julia_type(res, rt);
}

// --- code generator for ccall itself ---

// ccall(pointer, rettype, (argtypes...), args...)
static Value *emit_ccall(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGSV(ccall, 3);
    jl_value_t *rt=NULL, *at=NULL;
    JL_GC_PUSH2(&rt, &at);

    native_sym_arg_t symarg = interpret_symbol_arg(args[1], ctx, "ccall");
    Value *jl_ptr=NULL;
    void *fptr = NULL;
    char *f_name = NULL, *f_lib = NULL;
    jl_ptr = symarg.jl_ptr;
    fptr = symarg.fptr;
    f_name = symarg.f_name;
    f_lib = symarg.f_lib;
    if (f_name == NULL && fptr == NULL && jl_ptr == NULL) {
        JL_GC_POP();
        emit_error("ccall: null function pointer", ctx);
        return literal_pointer_val(jl_nothing);
    }

    rt  = jl_interpret_toplevel_expr_in(ctx->module, args[2],
                                        &jl_tupleref(ctx->sp,0),
                                        jl_tuple_len(ctx->sp)/2);
    if (jl_is_tuple(rt)) {
        std::string msg = "in " + ctx->funcName +
            ": ccall: missing return type";
        jl_error(msg.c_str());
    }
    if (rt == (jl_value_t*)jl_pointer_type)
        jl_error("ccall: return type Ptr should have an element type, Ptr{T}");
    at  = jl_interpret_toplevel_expr_in(ctx->module, args[3],
                                        &jl_tupleref(ctx->sp,0),
                                        jl_tuple_len(ctx->sp)/2);

    JL_TYPECHK(ccall, type, rt);
    JL_TYPECHK(ccall, tuple, at);
    JL_TYPECHK(ccall, type, at);
    Type *lrt = julia_struct_to_llvm(rt);
    if (lrt == NULL) {
        JL_GC_POP();
        emit_error("ccall: return type doesn't correspond to a C type", ctx);
        return literal_pointer_val(jl_nothing);
    }
    jl_tuple_t *tt = (jl_tuple_t*)at;
    std::vector<Type *> fargt(0);
    std::vector<Type *> fargt_sig(0);
#if LLVM33
    AttrBuilder retattrs;
    std::vector<AttrBuilder> paramattrs;
#else
    std::vector<AttributeWithIndex> attrs;
#endif
    AbiState abi;
    int sret = 0;
    if (jl_isbits(rt) && use_sret(&abi,rt)) {
#if LLVM33
        paramattrs[0].clear();
        paramattrs[0].addAttribute(Attributes::StructRet);
#elif LLVM32
        attrs.push_back(AttributeWithIndex::get(getGlobalContext(), 1, Attributes::StructRet));
#else
        attrs.push_back(AttributeWithIndex::get(1, Attribute::StructRet));
#endif
        sret = 1;
    }
    size_t i;
    bool isVa = false;
    size_t nargt = jl_tuple_len(tt);

    for(i=0; i < nargt; i++) {
#if LLVM32 || LLVM33
        paramattrs.push_back(AttrBuilder());
#endif
        jl_value_t *tti = jl_tupleref(tt,i);
        if (tti == (jl_value_t*)jl_pointer_type)
            jl_error("ccall: argument type Ptr should have an element type, Ptr{T}");
        if (jl_is_vararg_type(tti)) {
            isVa = true;
            tti = jl_tparam0(tti);
        }
        if (jl_is_bitstype(tti)) {
            // see pull req #978. need to annotate signext/zeroext for
            // small integer arguments.
            jl_datatype_t *bt = (jl_datatype_t*)tti;
            if (bt->size < 4) {
                if (jl_signed_type == NULL) {
                    jl_signed_type = jl_get_global(jl_core_module,jl_symbol("Signed"));
                }
#if LLVM33 
                Attribute::AttrKind av;
#elif LLVM32 
                Attributes::AttrVal av;
#else 
                Attribute::AttrConst av;
#endif
#if LLVM32 && !LLVM33
                if (jl_signed_type && jl_subtype(tti, jl_signed_type, 0))
                    av = Attributes::SExt;
                else
                    av = Attributes::ZExt;
                attrs.push_back(AttributeWithIndex::get(getGlobalContext(), i+1+sret, av));
#else
                if (jl_signed_type && jl_subtype(tti, jl_signed_type, 0))
                    av = Attribute::SExt;
                else
                    av = Attribute::ZExt;
#endif
#if LLVM32 || LLVM33
                paramattrs[i+sret].addAttribute(av);
#else
                attrs.push_back(AttributeWithIndex::get(i+1+sret, av));
#endif
            }
        }
        Type *t = julia_struct_to_llvm(tti);
        if (t == NULL) {
            JL_GC_POP();
            std::stringstream msg;
            msg << "ccall: the type of argument ";
            msg << i+1;
            msg << " doesn't correspond to a C type";
            emit_error(msg.str(), ctx);
            return literal_pointer_val(jl_nothing);
        }
        if (0 && t->isStructTy()) {
            t = PointerType::get(t,0);
#ifdef LLVM32
            attrs.push_back(AttributeWithIndex::get(getGlobalContext(), i+1+sret, Attributes::ByVal));
#else
            attrs.push_back(AttributeWithIndex::get(i+1, Attribute::ByVal));
#endif
        }
        fargt.push_back(t);
        if (!isVa)
            fargt_sig.push_back(t);
    }
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
    
    if ((!isVa && jl_tuple_len(tt)  != (nargs-2)/2) ||
        ( isVa && jl_tuple_len(tt)-1 > (nargs-2)/2))
        jl_error("ccall: wrong number of arguments to C function");

    // some special functions
    if (fptr == &jl_array_ptr) {
        assert(lrt->isPointerTy());
        Value *ary = emit_expr(args[4], ctx);
        JL_GC_POP();
        return mark_julia_type(builder.CreateBitCast(emit_arrayptr(ary),lrt),
                               rt);
    }
    if (fptr == &jl_value_ptr) {
        assert(lrt->isPointerTy());
        jl_value_t *argi = args[4];
        bool addressOf = false;
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }
        Value *ary = boxed(emit_expr(argi, ctx));
        JL_GC_POP();
        return mark_julia_type(
                builder.CreateBitCast(emit_nthptr_addr(ary, addressOf?1:0),lrt),
                rt);
    }

    // make LLVM function object for the target
    Value *llvmf;
    JL_PRINTF(JL_STDOUT,"\n%s :",f_name);
    FunctionType *functype = FunctionType::get(lrt, fargt_sig, isVa);
    functype->dump();
    if (jl_ptr != NULL) {
        null_pointer_check(jl_ptr,ctx);
        Type *funcptype = PointerType::get(functype,0);
        llvmf = builder.CreateIntToPtr(jl_ptr, funcptype);
    }
    else if (fptr != NULL) {
        Type *funcptype = PointerType::get(functype,0);
        llvmf = literal_pointer_val(fptr, funcptype);
    }
    else {
        void *symaddr;
        if (f_lib != NULL)
            symaddr = add_library_sym(f_name, f_lib);
        else
            symaddr = sys::DynamicLibrary::SearchForAddressOfSymbol(f_name);
        if (symaddr == NULL) {
            JL_GC_POP();
            std::stringstream msg;
            msg << "ccall: could not find function ";
            msg << f_name;
            if (f_lib != NULL) {
                msg << " in library ";
                msg << f_lib;
            }
            emit_error(msg.str(), ctx);
            return literal_pointer_val(jl_nothing);
        }
        llvmf = jl_Module->getOrInsertFunction(f_name, functype);
    }


    // save place before arguments, for possible insertion of temp arg
    // area saving code.
    Value *saveloc=NULL;
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

    if (0 && f_name != NULL && f_lib != NULL) {
        // print the f_name before each ccall
        Value *zeros[2] = { ConstantInt::get(T_int32, 0),
                            ConstantInt::get(T_int32, 0) };
        std::stringstream msg;
            msg << "ccall: ";
            msg << f_name;
            msg << "(...)";
            if (f_lib != NULL) {
                msg << " in library ";
                msg << f_lib;
            }
            msg << "\n";
        builder.CreateCall2(jlputs_func,
                            builder.CreateGEP(stringConst(msg.str()),
                                         ArrayRef<Value*>(zeros)),
                            literal_pointer_val(JL_STDERR));
    }

    // emit arguments
    Value *argvals[(nargs-3)/2 + sret];
    Value *result;

    // First, if the ABI requires us to provide the space for the return
    // argument, allocate the box and store that as the first argument type 
    if (sret) {
        assert(jl_is_structtype(rt));
        result = builder.CreateCall(
                jlallocobj_func,
                ConstantInt::get(T_size,
                    sizeof(void*)+((jl_datatype_t*)rt)->size));
        //TODO: Fill type pointer fields with C_NULL's
        builder.CreateStore(
                literal_pointer_val((jl_value_t*)rt),
                emit_nthptr_addr(result, (size_t)0));
        argvals[0] = builder.CreateBitCast(
                emit_nthptr_addr(result, (size_t)1),
                fargt_sig[0]);
    }

    // save argument depth until after we're done emitting arguments
    int last_depth = ctx->argDepth;

    // number of parameters to the cfunction
    int nargty = jl_tuple_len(tt);
    bool needTempSpace = false;
    for(i=4; i < nargs+1; i+=2) {

        // Current C function parameter
        int ai = (i-4)/2;

        // Julia (expression) value of current parameter
        jl_value_t *argi = args[i];

        // pass the address of the argument rather than the argument itself
        bool addressOf = false;
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }

        // LLVM type of the current parameter
        Type *largty;

        // Julia type of the current parameter
        jl_value_t *jargty;
        if (isVa && ai >= nargty-1) {
            largty = fargt[nargty-1];
            jargty = jl_tparam0(jl_tupleref(tt,nargty-1));
        }
        else {
            largty = fargt[ai];
            jargty = jl_tupleref(tt,ai);
        }


        // Whether the ABI needs us to pass this by ref and/or in registers
        // Valid combinations are:
    
        // Whether or not LLVM wants us to emit a pointer to the data
        bool byRef = false;

        // Whether or not to pass this in registers
        bool inReg = false;

        if(jl_isbits(jargty))
            needPassByRef(&abi,jargty,&byRef,&inReg);

        // Add the appropriate LLVM parameter attributes
        // Note that even though the LLVM argument is called ByVal 
        // this really means that the thing we're passing is pointing to
        // the thing we want to pass by value 
#if LLVM33
        if(byRef)
            paramattrs[ai+sret+1].addAttribute(Attributes::ByVal)
        if(inReg)
            paramattrs[ai+sret+1].addAttribute(Attributes::InReg)
#elif LLVM32
        if(byRef)
            attrs.push_back(AttributeWithIndex::get(getGlobalContext(), ai+sret+1, Attributes::ByVal));
        if(inReg)
            attrs.push_back(AttributeWithIndex::get(getGlobalContext(), ai+sret+1, Attributes::InReg));
#else
        if(byRef)
            attrs.push_back(AttributeWithIndex::get(ai+sret+1, Attribute::ByVal));
        if(inReg)
            attrs.push_back(AttributeWithIndex::get(ai+sret+1, Attribute::InReg));
#endif

        Value *arg;
        if (largty == jl_pvalue_llvmt ||
                largty->isStructTy()) {
            arg = emit_expr(argi, ctx, true);
        }
        else {
            arg = emit_unboxed(argi, ctx);
            if (jl_is_bitstype(expr_type(argi, ctx))) {
                if (addressOf)
                    arg = emit_unbox(largty->getContainedType(0), largty, arg);
                else
                    arg = emit_unbox(largty, PointerType::get(largty,0), arg);
            }
        }

#ifdef JL_GC_MARKSWEEP
        // make sure args are rooted
        if (largty == jl_pvalue_llvmt &&
            !(jl_is_symbol(argi) || jl_is_symbolnode(argi))) {
            make_gcroot(arg, ctx);
        }
#endif

        bool mightNeed=false;
        argvals[ai+sret] = julia_to_native(largty, jargty, arg, argi, addressOf, byRef, inReg,
                                           ai+1, ctx, &mightNeed);
        needTempSpace |= mightNeed;
    }
    if (needTempSpace) {
        // save temp argument area stack pointer
        // TODO: inline this
        saveloc = CallInst::Create(save_arg_area_loc_func);
        stacksave = CallInst::Create(Intrinsic::getDeclaration(jl_Module,
                                                               Intrinsic::stacksave));
        if (savespot)
            instList.insertAfter(savespot, (Instruction*)saveloc);
        else
            instList.push_front((Instruction*)saveloc);
        instList.insertAfter((Instruction*)saveloc, (Instruction*)stacksave);
    }
    // the actual call
    Value *ret = builder.CreateCall(
            llvmf,
            ArrayRef<Value*>(&argvals[0],(nargs-3)/2+sret));
#ifdef LLVM32
    ((CallInst*)ret)->setAttributes(AttrListPtr::get(getGlobalContext(), ArrayRef<AttributeWithIndex>(attrs)));
#else
    attributes = AttrListPtr::get(attrs.data(),attrs.size());
#endif

    ((CallInst*)ret)->setAttributes(attributes);
    if (cc != CallingConv::C)
        ((CallInst*)ret)->setCallingConv(cc);
    if (!sret)
        result = ret;
    if (needTempSpace) {
        // restore temp argument area stack pointer
        assert(saveloc != NULL);
        builder.CreateCall(restore_arg_area_loc_func, saveloc);
        assert(stacksave != NULL);
        builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                     Intrinsic::stackrestore),
                           stacksave);
    }
    ctx->argDepth = last_depth;
    if (0) { // Enable this to turn on SSPREQ (-fstack-protector) on the function containing this ccall
#if LLVM32 && !LLVM33     
        ctx->f->addFnAttr(Attributes::StackProtectReq);
#else
        ctx->f->addFnAttr(Attribute::StackProtectReq);
#endif
    }

    JL_GC_POP();
    if (!sret && lrt == T_void)
        return literal_pointer_val((jl_value_t*)jl_nothing);
    
    // Finally we need to box the result into julia type 
    // However, if we have already created a box for the return 
    // type because we the ABI required us to pass a pointer (sret),
    // then we do not need to do this. 
    if (!sret && lrt->isStructTy()) {
        //fprintf(stderr, "ccall rt: %s -> %s\n", f_name, ((jl_tag_type_t*)rt)->name->name->name);
        assert(jl_is_structtype(rt));
        // Call jlallocobj_func with the appropriate size (argument size size_t)
        Value *strct =
            builder.CreateCall(jlallocobj_func,
                               ConstantInt::get(T_size,
                                    sizeof(void*)+((jl_datatype_t*)rt)->size));
        // Store the type into the first field
        builder.CreateStore(literal_pointer_val((jl_value_t*)rt),
                            emit_nthptr_addr(strct, (size_t)0));
        builder.CreateStore(result,
                            builder.CreateBitCast(
                                emit_nthptr_addr(strct, (size_t)1),
                                PointerType::get(lrt,0)));
        return mark_julia_type(strct, rt);
    }
    return mark_julia_type(result, rt);
}
