#ifndef FLISP_H
#define FLISP_H

#include <setjmp.h>
#include <stdint.h>

#include "platform.h"
#include "libsupport.h"
#include "uv.h"

//#define MEMDEBUG
//#define MEMDEBUG2

typedef uptrint_t value_t;
typedef int_t fixnum_t;
#if NBITS==64
#define T_FIXNUM T_INT64
#define labs llabs
#else
#define T_FIXNUM T_INT32
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    value_t car;
    value_t cdr;
} cons_t;

typedef struct _symbol_t {
    uptrint_t flags;
    value_t binding;   // global value binding
    struct _fltype_t *type;
    uint32_t hash;
    void *dlcache;     // dlsym address
    // below fields are private
    struct _symbol_t *left;
    struct _symbol_t *right;
    union {
        char name[1];
        void *_pad;    // ensure field aligned to pointer size
    };
} symbol_t;

typedef struct {
    value_t isconst;
    value_t binding;   // global value binding
    struct _fltype_t *type;
    uint32_t id;
} gensym_t;

#define TAG_NUM      0x0
#define TAG_CPRIM    0x1
#define TAG_FUNCTION 0x2
#define TAG_VECTOR   0x3
#define TAG_NUM1     0x4
#define TAG_CVALUE   0x5
#define TAG_SYM      0x6
#define TAG_CONS     0x7
#define UNBOUND      ((value_t)0x1) // an invalid value
#define TAG_FWD      UNBOUND
#define tag(x) ((x)&0x7)
#define ptr(x) ((void*)((x)&(~(value_t)0x7)))
#define tagptr(p,t) (((value_t)(p)) | (t))
#define fixnum(x) ((value_t)(((fixnum_t)(x))<<2))
#define numval(x)  (((fixnum_t)(x))>>2)
#if NBITS==64
#define fits_fixnum(x) (((x)>>61) == 0 || (~((x)>>61)) == 0)
#else
#define fits_fixnum(x) (((x)>>29) == 0 || (~((x)>>29)) == 0)
#endif
#define fits_bits(x,b) (((x)>>(b-1)) == 0 || (~((x)>>(b-1))) == 0)
#define uintval(x)  (((unsigned int)(x))>>3)
#define builtin(n) tagptr((((int)n)<<3), TAG_FUNCTION)
#define iscons(x)    (tag(x) == TAG_CONS)
#define issymbol(x)  (tag(x) == TAG_SYM)
#define isfixnum(x)  (((x)&3) == TAG_NUM)
#define bothfixnums(x,y) ((((x)|(y))&3) == TAG_NUM)
#define isbuiltin(x) ((tag(x) == TAG_FUNCTION) && uintval(x) <= OP_ASET)
#define isvector(x) (tag(x) == TAG_VECTOR)
#define iscvalue(x) (tag(x) == TAG_CVALUE)
#define iscprim(x)  (tag(x) == TAG_CPRIM)
#define selfevaluating(x) (tag(x)<6)
// comparable with ==
#define eq_comparable(a,b) (!(((a)|(b))&1))
#define eq_comparablep(a) (!((a)&1))
// doesn't lead to other values
#define leafp(a) (((a)&3) != 3)

#define isforwarded(v) (((value_t*)ptr(v))[0] == TAG_FWD)
#define forwardloc(v)  (((value_t*)ptr(v))[1])
#define forward(v,to) do { (((value_t*)ptr(v))[0] = TAG_FWD); \
                           (((value_t*)ptr(v))[1] = to); } while (0)

#define vector_size(v) (((size_t*)ptr(v))[0]>>2)
#define vector_setsize(v,n) (((size_t*)ptr(v))[0] = ((n)<<2))
#define vector_elt(v,i) (((value_t*)ptr(v))[1+(i)])
#define vector_grow_amt(x) ((x)<8 ? 5 : 6*((x)>>3))
// functions ending in _ are unsafe, faster versions
#define car_(v) (((cons_t*)ptr(v))->car)
#define cdr_(v) (((cons_t*)ptr(v))->cdr)
#define car(v)  (tocons((v),"car")->car)
#define cdr(v)  (tocons((v),"cdr")->cdr)
#define fn_bcode(f) (((value_t*)ptr(f))[0])
#define fn_vals(f) (((value_t*)ptr(f))[1])
#define fn_env(f) (((value_t*)ptr(f))[2])
#define fn_name(f) (((value_t*)ptr(f))[3])

#define set(s, v)  (((symbol_t*)ptr(s))->binding = (v))
#define setc(s, v) do { ((symbol_t*)ptr(s))->flags |= 1; \
                        ((symbol_t*)ptr(s))->binding = (v); } while (0)
#define isconstant(s) ((s)->flags&0x1)
#define iskeyword(s) ((s)->flags&0x2)
#define symbol_value(s) (((symbol_t*)ptr(s))->binding)
#ifdef MEMDEBUG2
#define ismanaged(v) (!issymbol(v) && !isfixnum(v) && ((v)>(N_OPCODES<<3)) && !iscbuiltin(v))
#else
#define ismanaged(v) ((((unsigned char*)ptr(v)) >= fromspace) && \
                      (((unsigned char*)ptr(v)) < fromspace+heapsize))
#endif
#define isgensym(x)  (issymbol(x) && ismanaged(x))

#define isfunction(x) (tag(x) == TAG_FUNCTION && (x) > (N_BUILTINS<<3))
#define isclosure(x) isfunction(x)
#define iscbuiltin(x) (iscvalue(x) && (cv_class((cvalue_t*)ptr(x))==builtintype))

void fl_gc_handle(value_t *pv);
void fl_free_gc_handles(uint32_t n);

#include "opcodes.h"

// utility for iterating over all arguments in a builtin
// i=index, i0=start index, arg = var for each arg, args = arg array
// assumes "nargs" is the argument count
#define FOR_ARGS(i, i0, arg, args)     \
    for(i=i0; ((size_t)i)<nargs && ((arg=args[i]) || 1); i++)

#define N_BUILTINS ((int)N_OPCODES)

extern value_t FL_NIL, FL_T, FL_F, FL_EOF;

#define FL_UNSPECIFIED FL_T

/* read, eval, print main entry points */
value_t fl_read_sexpr(value_t f);
void fl_print(ios_t *f, value_t v);
value_t fl_toplevel_eval(value_t expr);
value_t fl_apply(value_t f, value_t l);
value_t fl_applyn(uint32_t n, value_t f, ...);

extern value_t printprettysym, printreadablysym, printwidthsym;

/* object model manipulation */
value_t fl_cons(value_t a, value_t b);
value_t fl_list2(value_t a, value_t b);
value_t fl_listn(size_t n, ...);
value_t symbol(char *str);
char *symbol_name(value_t v);
int fl_is_keyword_name(const char *str, size_t len);
value_t alloc_vector(size_t n, int init);
size_t llength(value_t v);
value_t fl_compare(value_t a, value_t b);  // -1, 0, or 1
value_t fl_equal(value_t a, value_t b);    // T or nil
int equal_lispvalue(value_t a, value_t b);
uptrint_t hash_lispvalue(value_t a);
int isnumtok_base(char *tok, value_t *pval, int base);

/* safe casts */
cons_t *tocons(value_t v, char *fname);
symbol_t *tosymbol(value_t v, char *fname);
fixnum_t tofixnum(value_t v, char *fname);
char *tostring(value_t v, char *fname);

/* error handling */
typedef struct _ectx_t {
    jmp_buf buf;
    uint32_t sp;
    uint32_t frame;
    uint32_t ngchnd;
    void *rdst;
    struct _ectx_t *prev;
} fl_exception_context_t;

extern fl_exception_context_t *fl_ctx;
extern uint32_t fl_throwing_frame;
extern value_t fl_lasterror;

#define FL_TRY_EXTERN                                                   \
  fl_exception_context_t _ctx; int l__tr, l__ca;                        \
  fl_savestate(&_ctx); fl_ctx = &_ctx;                                  \
  if (!setjmp(_ctx.buf))                                                \
    for (l__tr=1; l__tr; l__tr=0, (void)(fl_ctx=fl_ctx->prev))

#define FL_CATCH_EXTERN \
  else \
    for(l__ca=1; l__ca; l__ca=0, fl_restorestate(&_ctx))

#if defined(_OS_WINDOWS_)
__declspec(noreturn) void lerrorf(value_t e, char *format, ...);
__declspec(noreturn) void lerror(value_t e, const char *msg);
__declspec(noreturn) void fl_raise(value_t e);
__declspec(noreturn) void type_error(char *fname, char *expected, value_t got);
__declspec(noreturn) void bounds_error(char *fname, value_t arr, value_t ind);
#else
void lerrorf(value_t e, char *format, ...) __attribute__ ((__noreturn__));
void lerror(value_t e, const char *msg) __attribute__ ((__noreturn__));
void fl_raise(value_t e) __attribute__ ((__noreturn__));
void type_error(char *fname, char *expected, value_t got) __attribute__ ((__noreturn__));
void bounds_error(char *fname, value_t arr, value_t ind) __attribute__ ((__noreturn__));
#endif

void fl_savestate(fl_exception_context_t *_ctx);
void fl_restorestate(fl_exception_context_t *_ctx);

extern value_t ArgError, IOError, KeyError, OutOfMemoryError, EnumerationError;
extern value_t UnboundError;

static inline void argcount(char *fname, uint32_t nargs, uint32_t c)
{
    if (__unlikely(nargs != c))
        lerrorf(ArgError,"%s: too %s arguments", fname, nargs<c ? "few":"many");
}

typedef struct {
    void (*print)(value_t self, ios_t *f);
    void (*relocate)(value_t oldv, value_t newv);
    void (*finalize)(value_t self);
    void (*print_traverse)(value_t self);
} cvtable_t;

/* functions needed to implement the value interface (cvtable_t) */
value_t relocate_lispvalue(value_t v);
void print_traverse(value_t v);
void fl_print_chr(char c, ios_t *f);
void fl_print_str(char *s, ios_t *f);
void fl_print_child(ios_t *f, value_t v);

typedef int (*cvinitfunc_t)(struct _fltype_t*, value_t, void*);

typedef struct _fltype_t {
    value_t type;
    numerictype_t numtype;
    size_t size;
    size_t elsz;
    cvtable_t *vtable;
    struct _fltype_t *eltype;  // for arrays
    struct _fltype_t *artype;  // (array this)
    int marked;
    cvinitfunc_t init;
} fltype_t;

typedef struct {
    fltype_t *type;
    void *data;
    size_t len;            // length of *data in bytes
    union {
        value_t parent;    // optional
        char _space[1];    // variable size
    };
} cvalue_t;

#define CVALUE_NWORDS 4

typedef struct {
    fltype_t *type;
    char _space[1];
} cprim_t;

typedef struct {
    value_t bcode;
    value_t vals;
    value_t env;
    value_t name;
} function_t;

#define CPRIM_NWORDS 2
#define MAX_INL_SIZE 384

#define CV_OWNED_BIT  0x1
#define CV_PARENT_BIT 0x2
#define owned(cv)      ((uptrint_t)(cv)->type & CV_OWNED_BIT)
#define hasparent(cv)  ((uptrint_t)(cv)->type & CV_PARENT_BIT)
#define isinlined(cv)  ((cv)->data == &(cv)->_space[0])
#define cv_class(cv)   ((fltype_t*)(((uptrint_t)(cv)->type)&~3))
#define cv_len(cv)     ((cv)->len)
#define cv_type(cv)    (cv_class(cv)->type)
#define cv_data(cv)    ((cv)->data)
#define cv_isstr(cv)   (cv_class(cv)->eltype == bytetype)
#define cv_isPOD(cv)   (cv_class(cv)->init != NULL)

#define cvalue_data(v) cv_data((cvalue_t*)ptr(v))
#define cvalue_len(v) cv_len((cvalue_t*)ptr(v))
#define value2c(type, v) ((type)cv_data((cvalue_t*)ptr(v)))

#define valid_numtype(v) ((v) < N_NUMTYPES)
#define cp_class(cp)   ((cp)->type)
#define cp_type(cp)    (cp_class(cp)->type)
#define cp_numtype(cp) (cp_class(cp)->numtype)
#define cp_data(cp)    (&(cp)->_space[0])

// WARNING: multiple evaluation!
#define cptr(v) \
    (iscprim(v) ? cp_data((cprim_t*)ptr(v)) : cv_data((cvalue_t*)ptr(v)))

/* C type names corresponding to cvalues type names */
typedef int8_t   fl_int8_t;
typedef uint8_t  fl_uint8_t;
typedef int16_t  fl_int16_t;
typedef uint16_t fl_uint16_t;
typedef int32_t  fl_int32_t;
typedef uint32_t fl_uint32_t;
typedef int64_t  fl_int64_t;
typedef uint64_t fl_uint64_t;
typedef char     fl_char_t;
typedef char     char_t;
typedef ptrdiff_t fl_ptrdiff_t;
typedef size_t   fl_size_t;
typedef double   fl_double_t;
typedef float    fl_float_t;

typedef value_t (*builtin_t)(value_t*, uint32_t);

extern value_t QUOTE;
extern value_t int8sym, uint8sym, int16sym, uint16sym, int32sym, uint32sym;
extern value_t int64sym, uint64sym;
extern value_t ptrdiffsym, sizesym, bytesym, wcharsym;
extern value_t arraysym, cfunctionsym, voidsym, pointersym;
extern value_t stringtypesym, wcstringtypesym, emptystringsym;
extern value_t floatsym, doublesym;
extern fltype_t *bytetype, *wchartype;
extern fltype_t *stringtype, *wcstringtype;
extern fltype_t *builtintype;

value_t cvalue(fltype_t *type, size_t sz);
void add_finalizer(cvalue_t *cv);
void cv_autorelease(cvalue_t *cv);
void cv_pin(cvalue_t *cv);
size_t ctype_sizeof(value_t type, int *palign);
value_t cvalue_copy(value_t v);
value_t cvalue_from_data(fltype_t *type, void *data, size_t sz);
value_t cvalue_from_ref(fltype_t *type, void *ptr, size_t sz, value_t parent);
value_t cbuiltin(char *name, builtin_t f);
size_t cvalue_arraylen(value_t v);
value_t size_wrap(size_t sz);
size_t tosize(value_t n, char *fname);
value_t cvalue_string(size_t sz);
value_t cvalue_static_cstring(const char *str);
value_t string_from_cstr(char *str);
value_t string_from_cstrn(char *str, size_t n);
int fl_isstring(value_t v);
int fl_isnumber(value_t v);
int fl_isgensym(value_t v);
int fl_isiostream(value_t v);
ios_t *fl_toiostream(value_t v, char *fname);
value_t cvalue_compare(value_t a, value_t b);
int numeric_compare(value_t a, value_t b, int eq, int eqnans, char *fname);

void to_sized_ptr(value_t v, char *fname, char **pdata, size_t *psz);

fltype_t *get_type(value_t t);
fltype_t *get_array_type(value_t eltype);
fltype_t *define_opaque_type(value_t sym, size_t sz, cvtable_t *vtab,
                             cvinitfunc_t init);

value_t mk_double(fl_double_t n);
value_t mk_float(fl_float_t n);
value_t mk_uint32(uint32_t n);
value_t mk_uint64(uint64_t n);
value_t mk_wchar(int32_t n);
value_t return_from_uint64(uint64_t Uaccum);
value_t return_from_int64(int64_t Saccum);

typedef struct {
    char *name;
    builtin_t fptr;
} builtinspec_t;

void assign_global_builtins(builtinspec_t *b);

/* builtins */
value_t fl_hash(value_t *args, u_int32_t nargs);
value_t cvalue_byte(value_t *args, uint32_t nargs);
value_t cvalue_wchar(value_t *args, uint32_t nargs);

void fl_init(size_t initial_heapsize);
int fl_load_system_image(value_t ios);
int fl_load_system_image_str(char* str, size_t len);

/* julia extensions */
DLLEXPORT int jl_id_char(uint32_t wc);
DLLEXPORT int jl_id_start_char(uint32_t wc);

#ifdef __cplusplus
}
#endif

#endif
