#ifdef _P64
#define NWORDS(sz) (((sz)+7)>>3)
#else
#define NWORDS(sz) (((sz)+3)>>2)
#endif

static int ALIGN2, ALIGN4, ALIGN8, ALIGNPTR;

value_t int8sym, uint8sym, int16sym, uint16sym, int32sym, uint32sym;
value_t int64sym, uint64sym;
value_t ptrdiffsym, sizesym, bytesym, wcharsym;
value_t floatsym, doublesym;
value_t gftypesym, stringtypesym, wcstringtypesym;
value_t emptystringsym;

value_t arraysym, cfunctionsym, voidsym, pointersym;

static htable_t TypeTable;
static htable_t reverse_dlsym_lookup_table;
static fltype_t *int8type, *uint8type;
static fltype_t *int16type, *uint16type;
static fltype_t *int32type, *uint32type;
static fltype_t *int64type, *uint64type;
static fltype_t *ptrdifftype, *sizetype;
static fltype_t *floattype, *doubletype;
       fltype_t *bytetype, *wchartype;
       fltype_t *stringtype, *wcstringtype;
       fltype_t *builtintype;

static void cvalue_init(fltype_t *type, value_t v, void *dest);

// cvalues-specific builtins
value_t cvalue_new(value_t *args, u_int32_t nargs);
value_t cvalue_sizeof(value_t *args, u_int32_t nargs);
value_t cvalue_typeof(value_t *args, u_int32_t nargs);

// trigger unconditional GC after this many bytes are allocated
#define ALLOC_LIMIT_TRIGGER 67108864

static size_t malloc_pressure = 0;

static cvalue_t **Finalizers = NULL;
static size_t nfinalizers=0;
static size_t maxfinalizers=0;

void add_finalizer(cvalue_t *cv)
{
    if (nfinalizers == maxfinalizers) {
        size_t nn = (maxfinalizers==0 ? 256 : maxfinalizers*2);
        cvalue_t **temp = (cvalue_t**)realloc(Finalizers, nn*sizeof(value_t));
        if (temp == NULL)
            lerror(MemoryError, "out of memory");
        Finalizers = temp;
        maxfinalizers = nn;
    }
    Finalizers[nfinalizers++] = cv;
}

// remove dead objects from finalization list in-place
static void sweep_finalizers(void)
{
    cvalue_t **lst = Finalizers;
    size_t n=0, ndel=0, l=nfinalizers;
    cvalue_t *tmp;
#define SWAP_sf(a,b) (tmp=a,a=b,b=tmp,1)
    if (l == 0)
        return;
    do {
        tmp = lst[n];
        if (isforwarded((value_t)tmp)) {
            // object is alive
            lst[n] = (cvalue_t*)ptr(forwardloc((value_t)tmp));
            n++;
        }
        else {
            fltype_t *t = cv_class(tmp);
            if (t->vtable != NULL && t->vtable->finalize != NULL) {
                t->vtable->finalize(tagptr(tmp, TAG_CVALUE));
            }
            if (!isinlined(tmp) && owned(tmp)) {
#ifndef NDEBUG
                memset(cv_data(tmp), 0xbb, cv_len(tmp));
#endif
                free(cv_data(tmp));
            }
            ndel++;
        }
    } while ((n < l-ndel) && SWAP_sf(lst[n],lst[n+ndel]));

    nfinalizers -= ndel;
#ifdef VERBOSEGC
    if (ndel > 0)
        printf("GC: finalized %d objects\n", ndel);
#endif

    malloc_pressure = 0;
}

// compute the size of the metadata object for a cvalue
static size_t cv_nwords(cvalue_t *cv)
{
    if (isinlined(cv)) {
        size_t n = cv_len(cv);
        if (n==0 || cv_isstr(cv))
            n++;
        return CVALUE_NWORDS - 1 + NWORDS(n);
    }
    return CVALUE_NWORDS;
}

static void autorelease(cvalue_t *cv)
{
    cv->type = (fltype_t*)(((uptrint_t)cv->type) | CV_OWNED_BIT);
    add_finalizer(cv);
}

void cv_autorelease(cvalue_t *cv)
{
    autorelease(cv);
}

static value_t cprim(fltype_t *type, size_t sz)
{
    cprim_t *pcp = (cprim_t*)alloc_words(CPRIM_NWORDS-1+NWORDS(sz));
    pcp->type = type;
    return tagptr(pcp, TAG_CPRIM);
}

value_t cvalue(fltype_t *type, size_t sz)
{
    cvalue_t *pcv;
    int str=0;

    if (valid_numtype(type->numtype)) {
        return cprim(type, sz);
    }
    if (type->eltype == bytetype) {
        if (sz == 0)
            return symbol_value(emptystringsym);
        sz++;
        str=1;
    }
    if (sz <= MAX_INL_SIZE) {
        size_t nw = CVALUE_NWORDS - 1 + NWORDS(sz) + (sz==0 ? 1 : 0);
        pcv = (cvalue_t*)alloc_words(nw);
        pcv->type = type;
        pcv->data = &pcv->_space[0];
        if (type->vtable != NULL && type->vtable->finalize != NULL)
            add_finalizer(pcv);
    }
    else {
        if (malloc_pressure > ALLOC_LIMIT_TRIGGER)
            gc(0);
        pcv = (cvalue_t*)alloc_words(CVALUE_NWORDS);
        pcv->type = type;
        pcv->data = malloc(sz);
        autorelease(pcv);
        malloc_pressure += sz;
    }
    if (str) {
        sz--;
        ((char*)pcv->data)[sz] = '\0';
    }
    pcv->len = sz;
    return tagptr(pcv, TAG_CVALUE);
}

value_t cvalue_from_data(fltype_t *type, void *data, size_t sz)
{
    value_t cv;
    cv = cvalue(type, sz);
    memcpy(cptr(cv), data, sz);
    return cv;
}

// this effectively dereferences a pointer
// just like *p in C, it only removes a level of indirection from the type,
// it doesn't copy any data.
// this method of creating a cvalue only allocates metadata.
// ptr is user-managed; we don't autorelease it unless the
// user explicitly calls (autorelease ) on the result of this function.
// 'parent' is an optional cvalue that this pointer is known to point
// into; NIL if none.
value_t cvalue_from_ref(fltype_t *type, void *ptr, size_t sz, value_t parent)
{
    cvalue_t *pcv;
    value_t cv;

    pcv = (cvalue_t*)alloc_words(CVALUE_NWORDS);
    pcv->data = ptr;
    pcv->len = sz;
    pcv->type = type;
    if (parent != NIL) {
        pcv->type = (fltype_t*)(((uptrint_t)pcv->type) | CV_PARENT_BIT);
        pcv->parent = parent;
    }
    cv = tagptr(pcv, TAG_CVALUE);
    return cv;
}

value_t cvalue_string(size_t sz)
{
    return cvalue(stringtype, sz);
}

value_t cvalue_static_cstring(const char *str)
{
    return cvalue_from_ref(stringtype, (char*)str, strlen(str), NIL);
}

value_t string_from_cstrn(char *str, size_t n)
{
    value_t v = cvalue_string(n);
    memcpy(cvalue_data(v), str, n);
    return v;
}

value_t string_from_cstr(char *str)
{
    return string_from_cstrn(str, strlen(str));
}

int fl_isstring(value_t v)
{
    return (iscvalue(v) && cv_isstr((cvalue_t*)ptr(v)));
}

// convert to malloc representation (fixed address)
void cv_pin(cvalue_t *cv)
{
    if (!isinlined(cv))
        return;
    size_t sz = cv_len(cv);
    if (cv_isstr(cv)) sz++;
    void *data = malloc(sz);
    memcpy(data, cv_data(cv), sz);
    cv->data = data;
    autorelease(cv);
}

#define num_init(ctype, cnvt, tag)                              \
static int cvalue_##ctype##_init(fltype_t *type, value_t arg,   \
                                 void *dest)                    \
{                                                               \
    fl_##ctype##_t n=0;                                         \
    (void)type;                                                 \
    if (isfixnum(arg)) {                                        \
        n = numval(arg);                                        \
    }                                                           \
    else if (iscprim(arg)) {                                    \
        cprim_t *cp = (cprim_t*)ptr(arg);                       \
        void *p = cp_data(cp);                                  \
        n = (fl_##ctype##_t)conv_to_##cnvt(p, cp_numtype(cp));  \
    }                                                           \
    else {                                                      \
        return 1;                                               \
    }                                                           \
    *((fl_##ctype##_t*)dest) = n;                               \
    return 0;                                                   \
}
num_init(int8, int32, T_INT8)
num_init(uint8, uint32, T_UINT8)
num_init(int16, int32, T_INT16)
num_init(uint16, uint32, T_UINT16)
num_init(int32, int32, T_INT32)
num_init(uint32, uint32, T_UINT32)
num_init(int64, int64, T_INT64)
num_init(uint64, uint64, T_UINT64)
num_init(float, double, T_FLOAT)
num_init(double, double, T_DOUBLE)

#define num_ctor_init(typenam, ctype, tag)                              \
value_t cvalue_##typenam(value_t *args, u_int32_t nargs)                \
{                                                                       \
    if (nargs==0) { PUSH(fixnum(0)); args = &Stack[SP-1]; }             \
    value_t cp = cprim(typenam##type, sizeof(fl_##ctype##_t));          \
    if (cvalue_##ctype##_init(typenam##type,                            \
                              args[0], cp_data((cprim_t*)ptr(cp))))     \
        type_error(#typenam, "number", args[0]);                        \
    return cp;                                                          \
}

#define num_ctor_ctor(typenam, ctype, tag)                              \
value_t mk_##typenam(fl_##ctype##_t n)                                  \
{                                                                       \
    value_t cp = cprim(typenam##type, sizeof(fl_##ctype##_t));          \
    *(fl_##ctype##_t*)cp_data((cprim_t*)ptr(cp)) = n;                   \
    return cp;                                                          \
}

#define num_ctor(typenam, ctype, tag) \
    num_ctor_init(typenam, ctype, tag) \
    num_ctor_ctor(typenam, ctype, tag)

num_ctor(int8, int8, T_INT8)
num_ctor(uint8, uint8, T_UINT8)
num_ctor(int16, int16, T_INT16)
num_ctor(uint16, uint16, T_UINT16)
num_ctor(int32, int32, T_INT32)
num_ctor(uint32, uint32, T_UINT32)
num_ctor(int64, int64, T_INT64)
num_ctor(uint64, uint64, T_UINT64)
num_ctor(byte,  uint8, T_UINT8)
num_ctor(wchar, int32, T_INT32)
#ifdef _P64
num_ctor(ptrdiff, int64, T_INT64)
num_ctor(size, uint64, T_UINT64)
#else
num_ctor(ptrdiff, int32, T_INT32)
num_ctor(size, uint32, T_UINT32)
#endif
num_ctor(float, float, T_FLOAT)
num_ctor(double, double, T_DOUBLE)

value_t size_wrap(size_t sz)
{
    if (fits_fixnum(sz))
        return fixnum(sz);
    assert(sizeof(void*) == sizeof(size_t));
    return mk_size(sz);
}

size_t tosize(value_t n, char *fname)
{
    if (isfixnum(n))
        return numval(n);
    if (iscprim(n)) {
        cprim_t *cp = (cprim_t*)ptr(n);
        return conv_to_size(cp_data(cp), cp_numtype(cp));
    }
    type_error(fname, "number", n);
    return 0;
}

static int isarray(value_t v)
{
    return iscvalue(v) && cv_class((cvalue_t*)ptr(v))->eltype != NULL;
}

static size_t predict_arraylen(value_t arg)
{
    if (isvector(arg))
        return vector_size(arg);
    else if (iscons(arg))
        return llength(arg);
    else if (arg == NIL)
        return 0;
    if (isarray(arg))
        return cvalue_arraylen(arg);
    return 1;
}

static int cvalue_array_init(fltype_t *ft, value_t arg, void *dest)
{
    value_t type = ft->type;
    size_t elsize, i, cnt, sz;
    fltype_t *eltype = ft->eltype;

    elsize = ft->elsz;
    cnt = predict_arraylen(arg);

    if (iscons(cdr_(cdr_(type)))) {
        size_t tc = tosize(car_(cdr_(cdr_(type))), "array");
        if (tc != cnt)
            lerror(ArgError, "array: size mismatch");
    }

    sz = elsize * cnt;

    if (isvector(arg)) {
        for(i=0; i < cnt; i++) {
            cvalue_init(eltype, vector_elt(arg,i), dest);
            dest += elsize;
        }
        return 0;
    }
    else if (iscons(arg) || arg==NIL) {
        i = 0;
        while (iscons(arg)) {
            if (i == cnt) { i++; break; } // trigger error
            cvalue_init(eltype, car_(arg), dest);
            i++;
            dest += elsize;
            arg = cdr_(arg);
        }
        if (i != cnt)
            lerror(ArgError, "array: size mismatch");
        return 0;
    }
    else if (iscvalue(arg)) {
        cvalue_t *cv = (cvalue_t*)ptr(arg);
        if (isarray(arg)) {
            fltype_t *aet = cv_class(cv)->eltype;
            if (aet == eltype) {
                if (cv_len(cv) == sz)
                    memcpy(dest, cv_data(cv), sz);
                else
                    lerror(ArgError, "array: size mismatch");
                return 0;
            }
            else {
                // TODO: initialize array from different type elements
                lerror(ArgError, "array: element type mismatch");
            }
        }
    }
    if (cnt == 1)
        cvalue_init(eltype, arg, dest);
    else
        type_error("array", "sequence", arg);
    return 0;
}

value_t cvalue_array(value_t *args, u_int32_t nargs)
{
    size_t elsize, cnt, sz, i;
    value_t arg;

    if (nargs < 1)
        argcount("array", nargs, 1);

    cnt = nargs - 1;
    fltype_t *type = get_array_type(args[0]);
    elsize = type->elsz;
    sz = elsize * cnt;

    value_t cv = cvalue(type, sz);
    char *dest = cv_data((cvalue_t*)ptr(cv));
    FOR_ARGS(i,1,arg,args) {
        cvalue_init(type->eltype, arg, dest);
        dest += elsize;
    }
    return cv;
}

// NOTE: v must be an array
size_t cvalue_arraylen(value_t v)
{
    cvalue_t *cv = (cvalue_t*)ptr(v);
    return cv_len(cv)/(cv_class(cv)->elsz);
}

// *palign is an output argument giving the alignment required by type
size_t ctype_sizeof(value_t type, int *palign)
{
    if (type == int8sym || type == uint8sym || type == bytesym) {
        *palign = 1;
        return 1;
    }
    if (type == int16sym || type == uint16sym) {
        *palign = ALIGN2;
        return 2;
    }
    if (type == int32sym || type == uint32sym || type == wcharsym ||
        type == floatsym) {
        *palign = ALIGN4;
        return 4;
    }
    if (type == int64sym || type == uint64sym || type == doublesym) {
        *palign = ALIGN8;
        return 8;
    }
    if (type == ptrdiffsym || type == sizesym) {
#ifdef _P64
        *palign = ALIGN8;
        return 8;
#else
        *palign = ALIGN4;
        return 4;
#endif
    }
    if (iscons(type)) {
        value_t hed = car_(type);
        if (hed == pointersym || hed == cfunctionsym) {
            *palign = ALIGNPTR;
            return sizeof(void*);
        }
        if (hed == arraysym) {
            value_t t = car(cdr_(type));
            if (!iscons(cdr_(cdr_(type))))
                lerror(ArgError, "sizeof: incomplete type");
            value_t n = car_(cdr_(cdr_(type)));
            size_t sz = tosize(n, "sizeof");
            return sz * ctype_sizeof(t, palign);
        }
    }
    lerror(ArgError, "sizeof: invalid c type");
    return 0;
}

extern fltype_t *iostreamtype;

// get pointer and size for any plain-old-data value
void to_sized_ptr(value_t v, char *fname, char **pdata, size_t *psz)
{
    if (iscvalue(v)) {
        cvalue_t *pcv = (cvalue_t*)ptr(v);
        ios_t *x = value2c(ios_t*,v);
        if (cv_class(pcv) == iostreamtype && (x->bm == bm_mem)) {
            *pdata = x->buf;
            *psz = x->size;
            return;
        }
        else if (cv_isPOD(pcv)) {
            *pdata = cv_data(pcv);
            *psz = cv_len(pcv);
            return;
        }
    }
    else if (iscprim(v)) {
        cprim_t *pcp = (cprim_t*)ptr(v);
        *pdata = cp_data(pcp);
        *psz = cp_class(pcp)->size;
        return;
    }
    type_error(fname, "plain-old-data", v);
}

value_t cvalue_sizeof(value_t *args, u_int32_t nargs)
{
    argcount("sizeof", nargs, 1);
    if (issymbol(args[0]) || iscons(args[0])) {
        int a;
        return size_wrap(ctype_sizeof(args[0], &a));
    }
    size_t n; char *data;
    to_sized_ptr(args[0], "sizeof", &data, &n);
    return size_wrap(n);
}

value_t cvalue_typeof(value_t *args, u_int32_t nargs)
{
    argcount("typeof", nargs, 1);
    switch(tag(args[0])) {
    case TAG_CONS: return pairsym;
    case TAG_NUM1:
    case TAG_NUM:  return fixnumsym;
    case TAG_SYM:  return symbolsym;
    case TAG_VECTOR: return vectorsym;
    case TAG_FUNCTION:
        if (args[0] == FL_T || args[0] == FL_F)
            return booleansym;
        if (args[0] == NIL)
            return nullsym;
        if (args[0] == FL_EOF)
            return symbol("eof-object");
        if (isbuiltin(args[0]))
            return builtinsym;
        return FUNCTION;
    }
    return cv_type((cvalue_t*)ptr(args[0]));
}

static value_t cvalue_relocate(value_t v)
{
    size_t nw;
    cvalue_t *cv = (cvalue_t*)ptr(v);
    cvalue_t *nv;
    value_t ncv;

    nw = cv_nwords(cv);
    nv = (cvalue_t*)alloc_words(nw);
    memcpy(nv, cv, nw*sizeof(value_t));
    if (isinlined(cv))
        nv->data = &nv->_space[0];
    ncv = tagptr(nv, TAG_CVALUE);
    fltype_t *t = cv_class(cv);
    if (t->vtable != NULL && t->vtable->relocate != NULL)
        t->vtable->relocate(v, ncv);
    forward(v, ncv);
    return ncv;
}

value_t cvalue_copy(value_t v)
{
    assert(iscvalue(v));
    PUSH(v);
    cvalue_t *cv = (cvalue_t*)ptr(v);
    size_t nw = cv_nwords(cv);
    cvalue_t *ncv = (cvalue_t*)alloc_words(nw);
    v = POP(); cv = (cvalue_t*)ptr(v);
    memcpy(ncv, cv, nw * sizeof(value_t));
    if (!isinlined(cv)) {
        size_t len = cv_len(cv);
        if (cv_isstr(cv)) len++;
        ncv->data = malloc(len);
        memcpy(ncv->data, cv_data(cv), len);
        autorelease(ncv);
        if (hasparent(cv)) {
            ncv->type = (fltype_t*)(((uptrint_t)ncv->type) & ~CV_PARENT_BIT);
            ncv->parent = NIL;
        }
    }
    else {
        ncv->data = &ncv->_space[0];
    }

    return tagptr(ncv, TAG_CVALUE);
}

value_t fl_copy(value_t *args, u_int32_t nargs)
{
    argcount("copy", nargs, 1);
    if (iscons(args[0]) || isvector(args[0]))
        lerror(ArgError, "copy: argument must be a leaf atom");
    if (!iscvalue(args[0]))
        return args[0];
    if (!cv_isPOD((cvalue_t*)ptr(args[0])))
        lerror(ArgError, "copy: argument must be a plain-old-data type");
    return cvalue_copy(args[0]);
}

value_t fl_podp(value_t *args, u_int32_t nargs)
{
    argcount("plain-old-data?", nargs, 1);
    return (iscprim(args[0]) ||
            (iscvalue(args[0]) && cv_isPOD((cvalue_t*)ptr(args[0])))) ?
        FL_T : FL_F;
}

static void cvalue_init(fltype_t *type, value_t v, void *dest)
{
    cvinitfunc_t f=type->init;

    if (f == NULL)
        lerror(ArgError, "c-value: invalid c type");

    f(type, v, dest);
}

static numerictype_t sym_to_numtype(value_t type)
{
    if (type == int8sym)
        return T_INT8;
    else if (type == uint8sym || type == bytesym)
        return T_UINT8;
    else if (type == int16sym)
        return T_INT16;
    else if (type == uint16sym)
        return T_UINT16;
#ifdef _P64
    else if (type == int32sym || type == wcharsym)
#else
    else if (type == int32sym || type == wcharsym || type == ptrdiffsym)
#endif
        return T_INT32;
#ifdef _P64
    else if (type == uint32sym)
#else
    else if (type == uint32sym || type == sizesym)
#endif
        return T_UINT32;
#ifdef _P64
    else if (type == int64sym || type == ptrdiffsym)
#else
    else if (type == int64sym)
#endif
        return T_INT64;
#ifdef _P64
    else if (type == uint64sym || type == sizesym)
#else
    else if (type == uint64sym)
#endif
        return T_UINT64;
    else if (type == floatsym)
        return T_FLOAT;
    else if (type == doublesym)
        return T_DOUBLE;
    assert(0);
    return N_NUMTYPES;
}

// (new type . args)
// this provides (1) a way to allocate values with a shared type for
// efficiency, (2) a uniform interface for allocating cvalues of any
// type, including user-defined.
value_t cvalue_new(value_t *args, u_int32_t nargs)
{
    if (nargs < 1 || nargs > 2)
        argcount("c-value", nargs, 2);
    value_t type = args[0];
    fltype_t *ft = get_type(type);
    value_t cv;
    if (ft->eltype != NULL) {
        // special case to handle incomplete array types bla[]
        size_t elsz = ft->elsz;
        size_t cnt;

        if (iscons(cdr_(cdr_(type))))
            cnt = tosize(car_(cdr_(cdr_(type))), "array");
        else if (nargs == 2)
            cnt = predict_arraylen(args[1]);
        else
            cnt = 0;
        cv = cvalue(ft, elsz * cnt);
        if (nargs == 2)
            cvalue_array_init(ft, args[1], cv_data((cvalue_t*)ptr(cv)));
    }
    else {
        cv = cvalue(ft, ft->size);
        if (nargs == 2)
            cvalue_init(ft, args[1], cptr(cv));
    }
    return cv;
}

// NOTE: this only compares lexicographically; it ignores numeric formats
value_t cvalue_compare(value_t a, value_t b)
{
    cvalue_t *ca = (cvalue_t*)ptr(a);
    cvalue_t *cb = (cvalue_t*)ptr(b);
    char *adata = cv_data(ca);
    char *bdata = cv_data(cb);
    size_t asz = cv_len(ca);
    size_t bsz = cv_len(cb);
    size_t minsz = asz < bsz ? asz : bsz;
    int diff = memcmp(adata, bdata, minsz);
    if (diff == 0) {
        if (asz > bsz)
            return fixnum(1);
        else if (asz < bsz)
            return fixnum(-1);
    }
    return fixnum(diff);
}

static void check_addr_args(char *fname, value_t arr, value_t ind,
                            char **data, size_t *index)
{
    size_t numel;
    cvalue_t *cv = (cvalue_t*)ptr(arr);
    *data = cv_data(cv);
    numel = cv_len(cv)/(cv_class(cv)->elsz);
    *index = tosize(ind, fname);
    if (*index >= numel)
        bounds_error(fname, arr, ind);
}

static value_t cvalue_array_aref(value_t *args)
{
    char *data; size_t index;
    fltype_t *eltype = cv_class((cvalue_t*)ptr(args[0]))->eltype;
    value_t el = 0;
    numerictype_t nt = eltype->numtype;
    if (nt >= T_INT32)
        el = cvalue(eltype, eltype->size);
    check_addr_args("aref", args[0], args[1], &data, &index);
    if (nt < T_INT32) {
        if (nt == T_INT8)
            return fixnum((int8_t)data[index]);
        else if (nt == T_UINT8)
            return fixnum((uint8_t)data[index]);
        else if (nt == T_INT16)
            return fixnum(((int16_t*)data)[index]);
        return fixnum(((uint16_t*)data)[index]);
    }
    char *dest = cptr(el);
    size_t sz = eltype->size;
    if (sz == 1)
        *dest = data[index];
    else if (sz == 2)
        *(int16_t*)dest = ((int16_t*)data)[index];
    else if (sz == 4)
        *(int32_t*)dest = ((int32_t*)data)[index];
    else if (sz == 8)
        *(int64_t*)dest = ((int64_t*)data)[index];
    else
        memcpy(dest, data + index*sz, sz);
    return el;
}

static value_t cvalue_array_aset(value_t *args)
{
    char *data; size_t index;
    fltype_t *eltype = cv_class((cvalue_t*)ptr(args[0]))->eltype;
    check_addr_args("aset!", args[0], args[1], &data, &index);
    char *dest = data + index*eltype->size;
    cvalue_init(eltype, args[2], dest);
    return args[2];
}

value_t fl_builtin(value_t *args, u_int32_t nargs)
{
    argcount("builtin", nargs, 1);
    symbol_t *name = tosymbol(args[0], "builtin");
    cvalue_t *cv;
    if (ismanaged(args[0]) || (cv=name->dlcache) == NULL) {
        lerrorf(ArgError, "builtin: function %s not found", name->name);
    }
    return tagptr(cv, TAG_CVALUE);
}

value_t cbuiltin(char *name, builtin_t f)
{
    cvalue_t *cv = (cvalue_t*)malloc(CVALUE_NWORDS * sizeof(value_t));
    cv->type = builtintype;
    cv->data = &cv->_space[0];
    cv->len = sizeof(value_t);
    *(void**)cv->data = f;

    value_t sym = symbol(name);
    ((symbol_t*)ptr(sym))->dlcache = cv;
    ptrhash_put(&reverse_dlsym_lookup_table, cv, (void*)sym);

    return tagptr(cv, TAG_CVALUE);
}

static value_t fl_logand(value_t *args, u_int32_t nargs);
static value_t fl_logior(value_t *args, u_int32_t nargs);
static value_t fl_logxor(value_t *args, u_int32_t nargs);
static value_t fl_lognot(value_t *args, u_int32_t nargs);
static value_t fl_ash(value_t *args, u_int32_t nargs);

static builtinspec_t cvalues_builtin_info[] = {
    { "c-value", cvalue_new },
    { "typeof", cvalue_typeof },
    { "sizeof", cvalue_sizeof },
    { "builtin", fl_builtin },
    { "copy", fl_copy },
    { "plain-old-data?", fl_podp },

    { "logand", fl_logand },
    { "logior", fl_logior },
    { "logxor", fl_logxor },
    { "lognot", fl_lognot },
    { "ash", fl_ash },
    // todo: autorelease
    { NULL, NULL }
};

#define cv_intern(tok) tok##sym = symbol(#tok)
#define ctor_cv_intern(tok) \
    cv_intern(tok);set(tok##sym, cbuiltin(#tok, cvalue_##tok))

#define mk_primtype(name) \
  name##type=get_type(name##sym);name##type->init = &cvalue_##name##_init

#define mk_primtype_(name,ctype) \
  name##type=get_type(name##sym);name##type->init = &cvalue_##ctype##_init

static void cvalues_init(void)
{
    htable_new(&TypeTable, 256);
    htable_new(&reverse_dlsym_lookup_table, 256);

    // compute struct field alignment required for primitives
    ALIGN2   = sizeof(struct { char a; int16_t i; }) - 2;
    ALIGN4   = sizeof(struct { char a; int32_t i; }) - 4;
    ALIGN8   = sizeof(struct { char a; int64_t i; }) - 8;
    ALIGNPTR = sizeof(struct { char a; void   *i; }) - sizeof(void*);

    builtintype = define_opaque_type(builtinsym, sizeof(builtin_t), NULL, NULL);

    ctor_cv_intern(int8);
    ctor_cv_intern(uint8);
    ctor_cv_intern(int16);
    ctor_cv_intern(uint16);
    ctor_cv_intern(int32);
    ctor_cv_intern(uint32);
    ctor_cv_intern(int64);
    ctor_cv_intern(uint64);
    ctor_cv_intern(byte);
    ctor_cv_intern(wchar);
    ctor_cv_intern(ptrdiff);
    ctor_cv_intern(size);
    ctor_cv_intern(float);
    ctor_cv_intern(double);

    ctor_cv_intern(array);
    cv_intern(pointer);
    cv_intern(void);
    cfunctionsym = symbol("c-function");

    assign_global_builtins(cvalues_builtin_info);

    stringtypesym = symbol("*string-type*");
    setc(stringtypesym, fl_list2(arraysym, bytesym));

    wcstringtypesym = symbol("*wcstring-type*");
    setc(wcstringtypesym, fl_list2(arraysym, wcharsym));

    mk_primtype(int8);
    mk_primtype(uint8);
    mk_primtype(int16);
    mk_primtype(uint16);
    mk_primtype(int32);
    mk_primtype(uint32);
    mk_primtype(int64);
    mk_primtype(uint64);
#ifdef _P64
    mk_primtype_(ptrdiff,int64);
    mk_primtype_(size,uint64);
#else
    mk_primtype_(ptrdiff,int32);
    mk_primtype_(size,uint32);
#endif
    mk_primtype_(byte,uint8);
    mk_primtype_(wchar,int32);
    mk_primtype(float);
    mk_primtype(double);

    stringtype = get_type(symbol_value(stringtypesym));
    wcstringtype = get_type(symbol_value(wcstringtypesym));

    emptystringsym = symbol("*empty-string*");
    setc(emptystringsym, cvalue_static_cstring(""));
}

#define RETURN_NUM_AS(var, type) return(mk_##type((fl_##type##_t)var))

value_t return_from_uint64(uint64_t Uaccum)
{
    if (fits_fixnum(Uaccum)) {
        return fixnum((fixnum_t)Uaccum);
    }
    if (Uaccum > (uint64_t)S64_MAX) {
        RETURN_NUM_AS(Uaccum, uint64);
    }
    else if (Uaccum > (uint64_t)INT_MAX) {
        RETURN_NUM_AS(Uaccum, int64);
    }
    RETURN_NUM_AS(Uaccum, int32);
}

value_t return_from_int64(int64_t Saccum)
{
    if (fits_fixnum(Saccum)) {
        return fixnum((fixnum_t)Saccum);
    }
    if (Saccum > (int64_t)INT_MAX || Saccum < (int64_t)INT_MIN) {
        RETURN_NUM_AS(Saccum, int64);
    }
    RETURN_NUM_AS(Saccum, int32);
}

static value_t fl_add_any(value_t *args, u_int32_t nargs, fixnum_t carryIn)
{
    uint64_t Uaccum=0;
    int64_t Saccum = carryIn;
    double Faccum=0;
    uint32_t i;
    value_t arg=NIL;

    FOR_ARGS(i,0,arg,args) {
        if (isfixnum(arg)) {
            Saccum += numval(arg);
            continue;
        }
        else if (iscprim(arg)) {
            cprim_t *cp = (cprim_t*)ptr(arg);
            void *a = cp_data(cp);
            int64_t i64;
            switch(cp_numtype(cp)) {
            case T_INT8:   Saccum += *(int8_t*)a; break;
            case T_UINT8:  Saccum += *(uint8_t*)a; break;
            case T_INT16:  Saccum += *(int16_t*)a; break;
            case T_UINT16: Saccum += *(uint16_t*)a; break;
            case T_INT32:  Saccum += *(int32_t*)a; break;
            case T_UINT32: Saccum += *(uint32_t*)a; break;
            case T_INT64:
                i64 = *(int64_t*)a;
                if (i64 > 0)
                    Uaccum += (uint64_t)i64;
                else
                    Saccum += i64;
                break;
            case T_UINT64: Uaccum += *(uint64_t*)a; break;
            case T_FLOAT:  Faccum += *(float*)a; break;
            case T_DOUBLE: Faccum += *(double*)a; break;
            default:
                goto add_type_error;
            }
            continue;
        }
    add_type_error:
        type_error("+", "number", arg);
    }
    if (Faccum != 0) {
        Faccum += Uaccum;
        Faccum += Saccum;
        return mk_double(Faccum);
    }
    else if (Saccum < 0) {
        uint64_t negpart = (uint64_t)(-Saccum);
        if (negpart > Uaccum) {
            Saccum += (int64_t)Uaccum;
            // return value in Saccum
            if (Saccum >= INT_MIN) {
                if (fits_fixnum(Saccum)) {
                    return fixnum((fixnum_t)Saccum);
                }
                RETURN_NUM_AS(Saccum, int32);
            }
            RETURN_NUM_AS(Saccum, int64);
        }
        Uaccum -= negpart;
    }
    else {
        Uaccum += (uint64_t)Saccum;
    }
    // return value in Uaccum
    return return_from_uint64(Uaccum);
}

static value_t fl_neg(value_t n)
{
    if (isfixnum(n)) {
        return fixnum(-numval(n));
    }
    else if (iscprim(n)) {
        cprim_t *cp = (cprim_t*)ptr(n);
        void *a = cp_data(cp);
        uint32_t ui32;
        int32_t i32;
        int64_t i64;
        switch(cp_numtype(cp)) {
        case T_INT8:   return fixnum(-(int32_t)*(int8_t*)a);
        case T_UINT8:  return fixnum(-(int32_t)*(uint8_t*)a);
        case T_INT16:  return fixnum(-(int32_t)*(int16_t*)a);
        case T_UINT16: return fixnum(-(int32_t)*(uint16_t*)a);
        case T_INT32:
            i32 = *(int32_t*)a;
            if (i32 == (int32_t)BIT31)
                return mk_uint32((uint32_t)BIT31);
            return mk_int32(-i32);
        case T_UINT32:
            ui32 = *(uint32_t*)a;
            if (ui32 <= ((uint32_t)INT_MAX)+1) return mk_int32(-(int32_t)ui32);
            return mk_int64(-(int64_t)ui32);
        case T_INT64:
            i64 = *(int64_t*)a;
            if (i64 == (int64_t)BIT63)
                return mk_uint64((uint64_t)BIT63);
            return mk_int64(-i64);
        case T_UINT64: return mk_int64(-(int64_t)*(uint64_t*)a);
        case T_FLOAT:  return mk_float(-*(float*)a);
        case T_DOUBLE: return mk_double(-*(double*)a);
            break;
        }
    }
    type_error("-", "number", n);
}

static value_t fl_mul_any(value_t *args, u_int32_t nargs, int64_t Saccum)
{
    uint64_t Uaccum=1;
    double Faccum=1;
    uint32_t i;
    value_t arg=NIL;

    FOR_ARGS(i,0,arg,args) {
        if (isfixnum(arg)) {
            Saccum *= numval(arg);
            continue;
        }
        else if (iscprim(arg)) {
            cprim_t *cp = (cprim_t*)ptr(arg);
            void *a = cp_data(cp);
            int64_t i64;
            switch(cp_numtype(cp)) {
            case T_INT8:   Saccum *= *(int8_t*)a; break;
            case T_UINT8:  Saccum *= *(uint8_t*)a; break;
            case T_INT16:  Saccum *= *(int16_t*)a; break;
            case T_UINT16: Saccum *= *(uint16_t*)a; break;
            case T_INT32:  Saccum *= *(int32_t*)a; break;
            case T_UINT32: Saccum *= *(uint32_t*)a; break;
            case T_INT64:
                i64 = *(int64_t*)a;
                if (i64 > 0)
                    Uaccum *= (uint64_t)i64;
                else
                    Saccum *= i64;
                break;
            case T_UINT64: Uaccum *= *(uint64_t*)a; break;
            case T_FLOAT:  Faccum *= *(float*)a; break;
            case T_DOUBLE: Faccum *= *(double*)a; break;
            default:
                goto mul_type_error;
            }
            continue;
        }
    mul_type_error:
        type_error("*", "number", arg);
    }
    if (Faccum != 1) {
        Faccum *= Uaccum;
        Faccum *= Saccum;
        return mk_double(Faccum);
    }
    else if (Saccum < 0) {
        Saccum *= (int64_t)Uaccum;
        if (Saccum >= INT_MIN) {
            if (fits_fixnum(Saccum)) {
                return fixnum((fixnum_t)Saccum);
            }
            RETURN_NUM_AS(Saccum, int32);
        }
        RETURN_NUM_AS(Saccum, int64);
    }
    else {
        Uaccum *= (uint64_t)Saccum;
    }
    return return_from_uint64(Uaccum);
}

static int num_to_ptr(value_t a, fixnum_t *pi, numerictype_t *pt, void **pp)
{
    cprim_t *cp;
    if (isfixnum(a)) {
        *pi = numval(a);
        *pp = pi;
        *pt = T_FIXNUM;
    }
    else if (iscprim(a)) {
        cp = (cprim_t*)ptr(a);
        *pp = cp_data(cp);
        *pt = cp_numtype(cp);
    }
    else {
        return 0;
    }
    return 1;
}

/*
  returns -1, 0, or 1 based on ordering of a and b
  eq: consider equality only, returning 0 or nonzero
  eqnans: NaNs considered equal to each other
          -0.0 not considered equal to 0.0
          inexact not considered equal to exact
  fname: if not NULL, throws type errors, else returns 2 for type errors
*/
int numeric_compare(value_t a, value_t b, int eq, int eqnans, char *fname)
{
    int_t ai, bi;
    numerictype_t ta, tb;
    void *aptr, *bptr;

    if (bothfixnums(a,b)) {
        if (a==b) return 0;
        if (numval(a) < numval(b)) return -1;
        return 1;
    }
    if (!num_to_ptr(a, &ai, &ta, &aptr)) {
        if (fname) type_error(fname, "number", a); else return 2;
    }
    if (!num_to_ptr(b, &bi, &tb, &bptr)) {
        if (fname) type_error(fname, "number", b); else return 2;
    }
    if (eq && eqnans && ((ta >= T_FLOAT) != (tb >= T_FLOAT)))
        return 1;
    if (cmp_eq(aptr, ta, bptr, tb, eqnans))
        return 0;
    if (eq) return 1;
    if (cmp_lt(aptr, ta, bptr, tb))
        return -1;
    return 1;
}

static void DivideByZeroError() __attribute__ ((__noreturn__));
static void DivideByZeroError(void)
{
    lerror(DivideError, "/: division by zero");
}

static value_t fl_div2(value_t a, value_t b)
{
    double da, db;
    int_t ai, bi;
    numerictype_t ta, tb;
    void *aptr, *bptr;

    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("/", "number", a);
    if (!num_to_ptr(b, &bi, &tb, &bptr))
        type_error("/", "number", b);

    da = conv_to_double(aptr, ta);
    db = conv_to_double(bptr, tb);

    if (db == 0 && tb < T_FLOAT)  // exact 0
        DivideByZeroError();

    da = da/db;

    if (ta < T_FLOAT && tb < T_FLOAT && (double)(int64_t)da == da)
        return return_from_int64((int64_t)da);
    return mk_double(da);
}

static value_t fl_idiv2(value_t a, value_t b)
{
    int_t ai, bi;
    numerictype_t ta, tb;
    void *aptr, *bptr;
    int64_t a64, b64;

    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("div0", "number", a);
    if (!num_to_ptr(b, &bi, &tb, &bptr))
        type_error("div0", "number", b);

    if (ta == T_UINT64) {
        if (tb == T_UINT64) {
            if (*(uint64_t*)bptr == 0) goto div_error;
            return return_from_uint64(*(uint64_t*)aptr / *(uint64_t*)bptr);
        }
        b64 = conv_to_int64(bptr, tb);
        if (b64 < 0) {
            return return_from_int64(-(int64_t)(*(uint64_t*)aptr /
                                                (uint64_t)(-b64)));
        }
        if (b64 == 0)
            goto div_error;
        return return_from_uint64(*(uint64_t*)aptr / (uint64_t)b64);
    }
    if (tb == T_UINT64) {
        if (*(uint64_t*)bptr == 0) goto div_error;
        a64 = conv_to_int64(aptr, ta);
        if (a64 < 0) {
            return return_from_int64(-((int64_t)((uint64_t)(-a64) /
                                                 *(uint64_t*)bptr)));
        }
        return return_from_uint64((uint64_t)a64 / *(uint64_t*)bptr);
    }

    b64 = conv_to_int64(bptr, tb);
    if (b64 == 0) goto div_error;

    return return_from_int64(conv_to_int64(aptr, ta) / b64);
 div_error:
    DivideByZeroError();
}

static value_t fl_bitwise_op(value_t a, value_t b, int opcode, char *fname)
{
    int_t ai, bi;
    numerictype_t ta, tb, itmp;
    void *aptr=NULL, *bptr=NULL, *ptmp;
    int64_t b64;

    if (!num_to_ptr(a, &ai, &ta, &aptr) || ta >= T_FLOAT)
        type_error(fname, "integer", a);
    if (!num_to_ptr(b, &bi, &tb, &bptr) || tb >= T_FLOAT)
        type_error(fname, "integer", b);

    if (ta < tb) {
        itmp = ta; ta = tb; tb = itmp;
        ptmp = aptr; aptr = bptr; bptr = ptmp;
    }
    // now a's type is larger than or same as b's
    b64 = conv_to_int64(bptr, tb);
    switch (opcode) {
    case 0:
    switch (ta) {
    case T_INT8:   return fixnum(   *(int8_t *)aptr  & (int8_t  )b64);
    case T_UINT8:  return fixnum(   *(uint8_t *)aptr & (uint8_t )b64);
    case T_INT16:  return fixnum(   *(int16_t*)aptr  & (int16_t )b64);
    case T_UINT16: return fixnum(   *(uint16_t*)aptr & (uint16_t)b64);
    case T_INT32:  return mk_int32( *(int32_t*)aptr  & (int32_t )b64);
    case T_UINT32: return mk_uint32(*(uint32_t*)aptr & (uint32_t)b64);
    case T_INT64:  return mk_int64( *(int64_t*)aptr  & (int64_t )b64);
    case T_UINT64: return mk_uint64(*(uint64_t*)aptr & (uint64_t)b64);
    case T_FLOAT:
    case T_DOUBLE: assert(0);
    }
    break;
    case 1:
    switch (ta) {
    case T_INT8:   return fixnum(   *(int8_t *)aptr  | (int8_t  )b64);
    case T_UINT8:  return fixnum(   *(uint8_t *)aptr | (uint8_t )b64);
    case T_INT16:  return fixnum(   *(int16_t*)aptr  | (int16_t )b64);
    case T_UINT16: return fixnum(   *(uint16_t*)aptr | (uint16_t)b64);
    case T_INT32:  return mk_int32( *(int32_t*)aptr  | (int32_t )b64);
    case T_UINT32: return mk_uint32(*(uint32_t*)aptr | (uint32_t)b64);
    case T_INT64:  return mk_int64( *(int64_t*)aptr  | (int64_t )b64);
    case T_UINT64: return mk_uint64(*(uint64_t*)aptr | (uint64_t)b64);
    case T_FLOAT:
    case T_DOUBLE: assert(0);
    }
    break;
    case 2:
    switch (ta) {
    case T_INT8:   return fixnum(   *(int8_t *)aptr  ^ (int8_t  )b64);
    case T_UINT8:  return fixnum(   *(uint8_t *)aptr ^ (uint8_t )b64);
    case T_INT16:  return fixnum(   *(int16_t*)aptr  ^ (int16_t )b64);
    case T_UINT16: return fixnum(   *(uint16_t*)aptr ^ (uint16_t)b64);
    case T_INT32:  return mk_int32( *(int32_t*)aptr  ^ (int32_t )b64);
    case T_UINT32: return mk_uint32(*(uint32_t*)aptr ^ (uint32_t)b64);
    case T_INT64:  return mk_int64( *(int64_t*)aptr  ^ (int64_t )b64);
    case T_UINT64: return mk_uint64(*(uint64_t*)aptr ^ (uint64_t)b64);
    case T_FLOAT:
    case T_DOUBLE: assert(0);
    }
    }
    assert(0);
    return NIL;
}

static value_t fl_logand(value_t *args, u_int32_t nargs)
{
    value_t v, e;
    int i;
    if (nargs == 0)
        return fixnum(-1);
    v = args[0];
    FOR_ARGS(i,1,e,args) {
        if (bothfixnums(v, e))
            v = v & e;
        else
            v = fl_bitwise_op(v, e, 0, "logand");
    }
    return v;
}

static value_t fl_logior(value_t *args, u_int32_t nargs)
{
    value_t v, e;
    int i;
    if (nargs == 0)
        return fixnum(0);
    v = args[0];
    FOR_ARGS(i,1,e,args) {
        if (bothfixnums(v, e))
            v = v | e;
        else
            v = fl_bitwise_op(v, e, 1, "logior");
    }
    return v;
}

static value_t fl_logxor(value_t *args, u_int32_t nargs)
{
    value_t v, e;
    int i;
    if (nargs == 0)
        return fixnum(0);
    v = args[0];
    FOR_ARGS(i,1,e,args) {
        if (bothfixnums(v, e))
            v = fixnum(numval(v) ^ numval(e));
        else
            v = fl_bitwise_op(v, e, 2, "logxor");
    }
    return v;
}

static value_t fl_lognot(value_t *args, u_int32_t nargs)
{
    argcount("lognot", nargs, 1);
    value_t a = args[0];
    if (isfixnum(a))
        return fixnum(~numval(a));
    cprim_t *cp;
    int ta;
    void *aptr;

    if (iscprim(a)) {
        cp = (cprim_t*)ptr(a);
        ta = cp_numtype(cp);
        aptr = cp_data(cp);
        switch (ta) {
        case T_INT8:   return fixnum(~*(int8_t *)aptr);
        case T_UINT8:  return fixnum(~*(uint8_t *)aptr);
        case T_INT16:  return fixnum(~*(int16_t *)aptr);
        case T_UINT16: return fixnum(~*(uint16_t*)aptr);
        case T_INT32:  return mk_int32(~*(int32_t *)aptr);
        case T_UINT32: return mk_uint32(~*(uint32_t*)aptr);
        case T_INT64:  return mk_int64(~*(int64_t *)aptr);
        case T_UINT64: return mk_uint64(~*(uint64_t*)aptr);
        }
    }
    type_error("lognot", "integer", a);
}

static value_t fl_ash(value_t *args, u_int32_t nargs)
{
    fixnum_t n;
    int64_t accum;
    argcount("ash", nargs, 2);
    value_t a = args[0];
    n = tofixnum(args[1], "ash");
    if (isfixnum(a)) {
        if (n <= 0)
            return fixnum(numval(a)>>(-n));
        accum = ((int64_t)numval(a))<<n;
        if (fits_fixnum(accum))
            return fixnum(accum);
        else
            return return_from_int64(accum);
    }
    cprim_t *cp;
    int ta;
    void *aptr;
    if (iscprim(a)) {
        if (n == 0) return a;
        cp = (cprim_t*)ptr(a);
        ta = cp_numtype(cp);
        aptr = cp_data(cp);
        if (n < 0) {
            n = -n;
            switch (ta) {
            case T_INT8:   return fixnum((*(int8_t *)aptr) >> n);
            case T_UINT8:  return fixnum((*(uint8_t *)aptr) >> n);
            case T_INT16:  return fixnum((*(int16_t *)aptr) >> n);
            case T_UINT16: return fixnum((*(uint16_t*)aptr) >> n);
            case T_INT32:  return mk_int32((*(int32_t *)aptr) >> n);
            case T_UINT32: return mk_uint32((*(uint32_t*)aptr) >> n);
            case T_INT64:  return mk_int64((*(int64_t *)aptr) >> n);
            case T_UINT64: return mk_uint64((*(uint64_t*)aptr) >> n);
            }
        }
        else {
            if (ta == T_UINT64)
                return return_from_uint64((*(uint64_t*)aptr)<<n);
            else if (ta < T_FLOAT) {
                int64_t i64 = conv_to_int64(aptr, ta);
                return return_from_int64(i64<<n);
            }
        }
    }
    type_error("ash", "integer", a);
    return NIL;
}
