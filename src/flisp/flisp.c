/*
  femtoLisp

  a compact interpreter for a minimal lisp/scheme dialect

  characteristics:
  * lexical scope, lisp-1
  * unrestricted macros
  * data types: 30-bit integer, symbol, pair, vector, char, string, table
      iostream, procedure, low-level data types
  * case-sensitive
  * simple compacting copying garbage collector
  * Scheme-style varargs (dotted formal argument lists)
  * "human-readable" bytecode with self-hosted compiler

  extra features:
  * circular structure can be printed and read
  * #. read macro for eval-when-read and readably printing builtins
  * read macros for backquote
  * symbol character-escaping printer
  * exceptions
  * gensyms (can be usefully read back in, too)
  * #| multiline comments |#, lots of other lexical syntax
  * generic compare function, cyclic equal
  * cvalues system providing C data types and a C FFI
  * constructor notation for nicely printing arbitrary values

  by Jeff Bezanson (C) 2009
  Distributed under the BSD License
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <wctype.h>
#include <sys/types.h>
#include <locale.h>
#include <limits.h>
#include <errno.h>

#include "platform.h"
#include "libsupport.h"
#include "flisp.h"
#include "opcodes.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
#include <malloc.h>
DLLEXPORT char * basename(char *);
DLLEXPORT char * dirname(char *);
#else
#include <libgen.h>
#endif

static char *builtin_names[] =
    { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
      NULL, NULL, NULL, NULL,
      // predicates
      "eq?", "eqv?", "equal?", "atom?", "not", "null?", "boolean?", "symbol?",
      "number?", "bound?", "pair?", "builtin?", "vector?", "fixnum?",
      "function?",

      // lists
      "cons", "list", "car", "cdr", "set-car!", "set-cdr!",

      // execution
      "apply",

      // arithmetic
      "+", "-", "*", "/", "div0", "=", "<", "compare",

      // sequences
      "vector", "aref", "aset!",
      "", "", "" };

#define ANYARGS -10000

static short builtin_arg_counts[] =
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, ANYARGS, 1, 1, 2, 2,
      -2,
      ANYARGS, -1, ANYARGS, -1, 2,  2, 2, 2,
      ANYARGS, 2, 3 };

static uint32_t N_STACK;
static value_t *Stack;
static uint32_t SP = 0;
static uint32_t curr_frame = 0;
#define PUSH(v) (Stack[SP++] = (v))
#define POP()   (Stack[--SP])
#define POPN(n) (SP-=(n))

#define N_GC_HANDLES 8192
static value_t *GCHandleStack[N_GC_HANDLES];
static uint32_t N_GCHND = 0;

value_t FL_NIL, FL_T, FL_F, FL_EOF, QUOTE;
value_t IOError, ParseError, TypeError, ArgError, UnboundError, MemoryError;
value_t DivideError, BoundsError, Error, KeyError, EnumerationError;
value_t printwidthsym, printreadablysym, printprettysym, printlengthsym;
value_t printlevelsym, builtins_table_sym;

static value_t NIL, LAMBDA, IF, TRYCATCH;
static value_t BACKQUOTE, COMMA, COMMAAT, COMMADOT, FUNCTION;

static value_t pairsym, symbolsym, fixnumsym, vectorsym, builtinsym, vu8sym;
static value_t definesym, defmacrosym, forsym, setqsym;
static value_t tsym, Tsym, fsym, Fsym, booleansym, nullsym, evalsym, fnsym;
// for reading characters
static value_t nulsym, alarmsym, backspacesym, tabsym, linefeedsym, newlinesym;
static value_t vtabsym, pagesym, returnsym, escsym, spacesym, deletesym;

static value_t apply_cl(uint32_t nargs);
static value_t *alloc_words(int n);
static value_t relocate(value_t v);

typedef struct _fl_readstate_t {
    htable_t backrefs;
    htable_t gensyms;
    value_t source;
    struct _fl_readstate_t *prev;
} fl_readstate_t;

static fl_readstate_t *readstate = NULL;

static void free_readstate(fl_readstate_t *rs)
{
    htable_free(&rs->backrefs);
    htable_free(&rs->gensyms);
}

static unsigned char *fromspace;
static unsigned char *tospace;
static unsigned char *curheap;
static unsigned char *lim;
static uint32_t heapsize;//bytes
static uint32_t *consflags;

// error utilities ------------------------------------------------------------

// saved execution state for an unwind target
fl_exception_context_t *fl_ctx = NULL;
uint32_t fl_throwing_frame=0;  // active frame when exception was thrown
value_t fl_lasterror;

#define FL_TRY \
  fl_exception_context_t _ctx; int l__tr, l__ca; \
  _ctx.sp=SP; _ctx.frame=curr_frame; _ctx.rdst=readstate; _ctx.prev=fl_ctx; \
  _ctx.ngchnd = N_GCHND; fl_ctx = &_ctx;                                    \
  if (!setjmp(_ctx.buf)) \
    for (l__tr=1; l__tr; l__tr=0, (void)(fl_ctx=fl_ctx->prev))

#define FL_CATCH \
  else \
    for(l__ca=1; l__ca; l__ca=0, \
      fl_lasterror=FL_NIL,fl_throwing_frame=0,SP=_ctx.sp,curr_frame=_ctx.frame)

void fl_savestate(fl_exception_context_t *_ctx)
{
    _ctx->sp = SP;
    _ctx->frame = curr_frame;
    _ctx->rdst = readstate;
    _ctx->prev = fl_ctx;
    _ctx->ngchnd = N_GCHND;
}

void fl_restorestate(fl_exception_context_t *_ctx)
{
    fl_lasterror = FL_NIL;
    fl_throwing_frame = 0;
    SP = _ctx->sp;
    curr_frame = _ctx->frame;
}

void fl_raise(value_t e)
{
    fl_lasterror = e;
    // unwind read state
    while (readstate != (fl_readstate_t*)fl_ctx->rdst) {
        free_readstate(readstate);
        readstate = readstate->prev;
    }
    if (fl_throwing_frame == 0)
        fl_throwing_frame = curr_frame;
    N_GCHND = fl_ctx->ngchnd;
    fl_exception_context_t *thisctx = fl_ctx;
    if (fl_ctx->prev)   // don't throw past toplevel
        fl_ctx = fl_ctx->prev;
    longjmp(thisctx->buf, 1);
}

static value_t make_error_msg(char *format, va_list args)
{
    char msgbuf[512];
    vsnprintf(msgbuf, sizeof(msgbuf), format, args);
    return string_from_cstr(msgbuf);
}

void lerrorf(value_t e, char *format, ...)
{
    va_list args;
    PUSH(e);
    va_start(args, format);
    value_t msg = make_error_msg(format, args);
    va_end(args);

    e = POP();
    fl_raise(fl_list2(e, msg));
}

void lerror(value_t e, const char *msg)
{
    PUSH(e);
    value_t m = cvalue_static_cstring(msg);
    e = POP();
    fl_raise(fl_list2(e, m));
}

void type_error(char *fname, char *expected, value_t got)
{
    fl_raise(fl_listn(4, TypeError, symbol(fname), symbol(expected), got));
}

void bounds_error(char *fname, value_t arr, value_t ind)
{
    fl_raise(fl_listn(4, BoundsError, symbol(fname), arr, ind));
}

// safe cast operators --------------------------------------------------------

#define isstring fl_isstring
#define SAFECAST_OP(type,ctype,cnvt)                                          \
ctype to##type(value_t v, char *fname)                                        \
{                                                                             \
    if (is##type(v))                                                          \
        return (ctype)cnvt(v);                                                \
    type_error(fname, #type, v);                                              \
}
SAFECAST_OP(cons,  cons_t*,  ptr)
SAFECAST_OP(symbol,symbol_t*,ptr)
SAFECAST_OP(fixnum,fixnum_t, numval)
SAFECAST_OP(cvalue,cvalue_t*,ptr)
SAFECAST_OP(string,char*,    cvalue_data)
#undef isstring

// symbol table ---------------------------------------------------------------

symbol_t *symtab = NULL;

int fl_is_keyword_name(const char *str, size_t len)
{
    return len>1 && ((str[0] == ':' || str[len-1] == ':') && str[1] != '\0');
}

static symbol_t *mk_symbol(const char *str)
{
    symbol_t *sym;
    size_t len = strlen(str);

    sym = (symbol_t*)malloc((sizeof(symbol_t)-sizeof(void*)+len+1+7)&-8);
    assert(((uptrint_t)sym & 0x7) == 0); // make sure malloc aligns 8
    sym->left = sym->right = NULL;
    sym->flags = 0;
    if (fl_is_keyword_name(str, len)) {
        value_t s = tagptr(sym, TAG_SYM);
        setc(s, s);
        sym->flags |= 0x2;
    }
    else {
        sym->binding = UNBOUND;
    }
    sym->type = NULL;
    sym->dlcache = NULL;
    sym->hash = memhash32(str, len)^0xAAAAAAAA;
    strcpy(&sym->name[0], str);
    return sym;
}

static symbol_t **symtab_lookup(symbol_t **ptree, const char *str)
{
    int x;

    while (*ptree != NULL) {
        x = strcmp(str, (*ptree)->name);
        if (x == 0)
            return ptree;
        if (x < 0)
            ptree = &(*ptree)->left;
        else
            ptree = &(*ptree)->right;
    }
    return ptree;
}

value_t symbol(char *str)
{
    symbol_t **pnode = symtab_lookup(&symtab, str);
    if (*pnode == NULL)
        *pnode = mk_symbol(str);
    return tagptr(*pnode, TAG_SYM);
}

static uint32_t _gensym_ctr=0;
// two static buffers for gensym printing so there can be two
// gensym names available at a time, mostly for compare()
static char gsname[2][16];
static int gsnameno=0;
value_t fl_gensym(value_t *args, uint32_t nargs)
{
#ifdef MEMDEBUG2
    gsnameno = 1-gsnameno;
    char *n = uint2str(gsname[gsnameno]+1, sizeof(gsname[0])-1, _gensym_ctr++, 10);
    *(--n) = 'g';
    return tagptr(mk_symbol(n), TAG_SYM);
#else
    argcount("gensym", nargs, 0);
    (void)args;
    gensym_t *gs = (gensym_t*)alloc_words(sizeof(gensym_t)/sizeof(void*));
    gs->id = _gensym_ctr++;
    gs->binding = UNBOUND;
    gs->isconst = 0;
    gs->type = NULL;
    return tagptr(gs, TAG_SYM);
#endif
}

int fl_isgensym(value_t v)
{
    return isgensym(v);
}

static value_t fl_gensymp(value_t *args, u_int32_t nargs)
{
    argcount("gensym?", nargs, 1);
    return isgensym(args[0]) ? FL_T : FL_F;
}

char *symbol_name(value_t v)
{
#ifndef MEMDEBUG2
    if (ismanaged(v)) {
        gensym_t *gs = (gensym_t*)ptr(v);
        gsnameno = 1-gsnameno;
        char *n = uint2str(gsname[gsnameno]+1, sizeof(gsname[0])-1, gs->id, 10);
        *(--n) = 'g';
        return n;
    }
#endif
    return ((symbol_t*)ptr(v))->name;
}

// conses ---------------------------------------------------------------------

#ifdef MEMDEBUG2
static void *tochain=NULL;
static long long n_allocd=0;
#define GC_INTERVAL 100000
#endif

void gc(int mustgrow);

static value_t mk_cons(void)
{
    cons_t *c;

#ifdef MEMDEBUG2
    if (n_allocd > GC_INTERVAL)
        gc(0);
    c = (cons_t*)((void**)malloc(3*sizeof(void*)) + 1);
    ((void**)c)[-1] = tochain;
    tochain = c;
    n_allocd += sizeof(cons_t);
#else
    if (__unlikely(curheap > lim))
        gc(0);
    c = (cons_t*)curheap;
    curheap += sizeof(cons_t);
#endif
    return tagptr(c, TAG_CONS);
}

static value_t *alloc_words(int n)
{
    value_t *first;

    assert(n > 0);
    n = LLT_ALIGN(n, 2);   // only allocate multiples of 2 words
#ifdef MEMDEBUG2
    if (n_allocd > GC_INTERVAL)
        gc(0);
    first = (value_t*)malloc((n+1)*sizeof(value_t)) + 1;
    first[-1] = (value_t)tochain;
    tochain = first;
    n_allocd += (n*sizeof(value_t));
#else
    if (__unlikely((value_t*)curheap > ((value_t*)lim)+2-n)) {
        gc(0);
        while ((value_t*)curheap > ((value_t*)lim)+2-n) {
            gc(1);
        }
    }
    first = (value_t*)curheap;
    curheap += (n*sizeof(value_t));
#endif
    return first;
}

// allocate n consecutive conses
#ifndef MEMDEBUG2
#define cons_reserve(n) tagptr(alloc_words((n)*2), TAG_CONS)
#endif

#ifndef MEMDEBUG2
#define cons_index(c)  (((cons_t*)ptr(c))-((cons_t*)fromspace))
#endif

#ifdef MEMDEBUG2
#define ismarked(c)    ((((value_t*)ptr(c))[-1]&1) != 0)
#define mark_cons(c)   ((((value_t*)ptr(c))[-1]) |= 1)
#define unmark_cons(c) ((((value_t*)ptr(c))[-1]) &= (~(value_t)1))
#else
#define ismarked(c)    bitvector_get(consflags, cons_index(c))
#define mark_cons(c)   bitvector_set(consflags, cons_index(c), 1)
#define unmark_cons(c) bitvector_set(consflags, cons_index(c), 0)
#endif

static value_t the_empty_vector;

value_t alloc_vector(size_t n, int init)
{
    if (n == 0) return the_empty_vector;
    value_t *c = alloc_words(n+1);
    value_t v = tagptr(c, TAG_VECTOR);
    vector_setsize(v, n);
    if (init) {
        unsigned int i;
        for(i=0; i < n; i++)
            vector_elt(v, i) = FL_UNSPECIFIED;
    }
    return v;
}

// cvalues --------------------------------------------------------------------

#include "cvalues.c"
#include "types.c"

// print ----------------------------------------------------------------------

static int isnumtok(char *tok, value_t *pval);
static inline int symchar(char c);

#include "print.c"

// collector ------------------------------------------------------------------

void fl_gc_handle(value_t *pv)
{
    if (N_GCHND >= N_GC_HANDLES)
        lerror(MemoryError, "out of gc handles");
    GCHandleStack[N_GCHND++] = pv;
}

void fl_free_gc_handles(uint32_t n)
{
    assert(N_GCHND >= n);
    N_GCHND -= n;
}

static value_t relocate(value_t v)
{
    value_t a, d, nc, first, *pcdr;
    uptrint_t t = tag(v);

    if (t == TAG_CONS) {
        // iterative implementation allows arbitrarily long cons chains
        pcdr = &first;
        do {
            if ((a=car_(v)) == TAG_FWD) {
                *pcdr = cdr_(v);
                return first;
            }
#ifdef MEMDEBUG2
            *pcdr = nc = mk_cons();
#else
            *pcdr = nc = tagptr((cons_t*)curheap, TAG_CONS);
            curheap += sizeof(cons_t);
#endif
            d = cdr_(v);
            car_(v) = TAG_FWD; cdr_(v) = nc;
            if ((tag(a)&3) == 0 || !ismanaged(a))
                car_(nc) = a;
            else
                car_(nc) = relocate(a);
            pcdr = &cdr_(nc);
            v = d;
        } while (iscons(v));
        *pcdr = (d==NIL) ? NIL : relocate(d);
        return first;
    }

    if ((t&3) == 0 || !ismanaged(v)) return v;
    if (isforwarded(v)) return forwardloc(v);

    if (t == TAG_VECTOR) {
        // N.B.: 0-length vectors secretly have space for a first element
        size_t i, sz = vector_size(v);
        if (vector_elt(v,-1) & 0x1) {
            // grown vector
            nc = relocate(vector_elt(v,0));
            forward(v, nc);
        }
        else {
            nc = tagptr(alloc_words(sz+1), TAG_VECTOR);
            vector_setsize(nc, sz);
            a = vector_elt(v,0);
            forward(v, nc);
            if (sz > 0) {
                vector_elt(nc,0) = relocate(a);
                for(i=1; i < sz; i++) {
                    a = vector_elt(v,i);
                    if ((tag(a)&3) == 0 || !ismanaged(a))
                        vector_elt(nc,i) = a;
                    else
                        vector_elt(nc,i) = relocate(a);
                }
            }
        }
        return nc;
    }
    else if (t == TAG_CPRIM) {
        cprim_t *pcp = (cprim_t*)ptr(v);
        size_t nw = CPRIM_NWORDS-1+NWORDS(cp_class(pcp)->size);
        cprim_t *ncp = (cprim_t*)alloc_words(nw);
        while (nw--)
            ((value_t*)ncp)[nw] = ((value_t*)pcp)[nw];
        nc = tagptr(ncp, TAG_CPRIM);
        forward(v, nc);
        return nc;
    }
    else if (t == TAG_CVALUE) {
        return cvalue_relocate(v);
    }
    else if (t == TAG_FUNCTION) {
        function_t *fn = (function_t*)ptr(v);
        function_t *nfn = (function_t*)alloc_words(4);
        nfn->bcode = fn->bcode;
        nfn->vals = fn->vals;
        nc = tagptr(nfn, TAG_FUNCTION);
        forward(v, nc);
        nfn->env = relocate(fn->env);
        nfn->vals = relocate(nfn->vals);
        nfn->bcode = relocate(nfn->bcode);
        nfn->name = fn->name;
        return nc;
    }
    else if (t == TAG_SYM) {
        gensym_t *gs = (gensym_t*)ptr(v);
        gensym_t *ng = (gensym_t*)alloc_words(sizeof(gensym_t)/sizeof(void*));
        ng->id = gs->id;
        ng->binding = gs->binding;
        ng->isconst = 0;
        nc = tagptr(ng, TAG_SYM);
        forward(v, nc);
        if (ng->binding != UNBOUND)
            ng->binding = relocate(ng->binding);
        return nc;
    }
    return v;
}

value_t relocate_lispvalue(value_t v)
{
    return relocate(v);
}

static void trace_globals(symbol_t *root)
{
    while (root != NULL) {
        if (root->binding != UNBOUND)
            root->binding = relocate(root->binding);
        trace_globals(root->left);
        root = root->right;
    }
}

static value_t memory_exception_value;

void gc(int mustgrow)
{
    void *temp;
    uint32_t i, f, top;
    fl_readstate_t *rs;
#ifdef MEMDEBUG2
    temp = tochain;
    tochain = NULL;
    n_allocd = -100000000000LL;
#else
    static int grew = 0;
    size_t hsz = grew ? heapsize*2 : heapsize;
#ifdef MEMDEBUG
    tospace = LLT_ALLOC(hsz);
#endif
    curheap = tospace;
    lim = curheap + hsz - sizeof(cons_t);
#endif

    if (fl_throwing_frame > curr_frame) {
        top = fl_throwing_frame - 3;
        f = Stack[fl_throwing_frame-3];
    }
    else {
        top = SP;
        f = curr_frame;
    }
    while (1) {
        for (i=f; i < top; i++)
            Stack[i] = relocate(Stack[i]);
        if (f == 0) break;
        top = f - 3;
        f = Stack[f-3];
    }
    for (i=0; i < N_GCHND; i++)
        *GCHandleStack[i] = relocate(*GCHandleStack[i]);
    trace_globals(symtab);
    relocate_typetable();
    rs = readstate;
    while (rs) {
        for(i=0; i < rs->backrefs.size; i++)
            rs->backrefs.table[i] = (void*)relocate((value_t)rs->backrefs.table[i]);
        for(i=0; i < rs->gensyms.size; i++)
            rs->gensyms.table[i] = (void*)relocate((value_t)rs->gensyms.table[i]);
        rs->source = relocate(rs->source);
        rs = rs->prev;
    }
    fl_lasterror = relocate(fl_lasterror);
    memory_exception_value = relocate(memory_exception_value);
    the_empty_vector = relocate(the_empty_vector);

    sweep_finalizers();

#ifdef MEMDEBUG2
    while (temp != NULL) {
        void *next = ((void**)temp)[-1];
        free(&((void**)temp)[-1]);
        temp = next;
    }
    n_allocd = 0;
#else
#ifdef VERBOSEGC
    printf("GC: found %d/%d live conses\n",
           (curheap-tospace)/sizeof(cons_t), heapsize/sizeof(cons_t));
#endif

    temp = tospace;
    tospace = fromspace;
    fromspace = (unsigned char*)temp;

    // if we're using > 80% of the space, resize tospace so we have
    // more space to fill next time. if we grew tospace last time,
    // grow the other half of the heap this time to catch up.
    if (grew || mustgrow
#ifdef MEMDEBUG
        // GC more often
        || ((lim-curheap) < (int)(heapsize/128))
#else
        || ((lim-curheap) < (int)(heapsize/5))
#endif
        ) {
        temp = LLT_REALLOC(tospace, heapsize*2);
        if (temp == NULL)
            fl_raise(memory_exception_value);
        tospace = (unsigned char*)temp;
        if (grew) {
            heapsize*=2;
            temp = bitvector_resize(consflags, 0, heapsize/sizeof(cons_t), 1);
            if (temp == NULL)
                fl_raise(memory_exception_value);
            consflags = (uint32_t*)temp;
        }
        grew = !grew;
    }
#ifdef MEMDEBUG
    LLT_FREE(tospace);
#endif
    if ((value_t*)curheap > ((value_t*)lim)-2) {
        // all data was live; gc again and grow heap.
        // but also always leave at least 4 words available, so a closure
        // can be allocated without an extra check.
        gc(0);
    }
#endif
}

static void grow_stack(void)
{
    size_t newsz = N_STACK + (N_STACK>>1);
    value_t *ns = (value_t*)realloc(Stack, newsz*sizeof(value_t));
    if (ns == NULL)
        lerror(MemoryError, "stack overflow");
    Stack = ns;
    N_STACK = newsz;
}

// utils ----------------------------------------------------------------------

// apply function with n args on the stack
static value_t _applyn(uint32_t n)
{
    value_t f = Stack[SP-n-1];
    uint32_t saveSP = SP;
    value_t v;
    if (iscbuiltin(f)) {
        v = ((builtin_t*)ptr(f))[3](&Stack[SP-n], n);
    }
    else if (isfunction(f)) {
        v = apply_cl(n);
    }
    else if (isbuiltin(f)) {
        value_t tab = symbol_value(builtins_table_sym);
        Stack[SP-n-1] = vector_elt(tab, uintval(f));
        v = apply_cl(n);
    }
    else {
        type_error("apply", "function", f);
    }
    SP = saveSP;
    return v;
}

value_t fl_apply(value_t f, value_t l)
{
    value_t v = l;
    uint32_t n = SP;

    PUSH(f);
    while (iscons(v)) {
        if (SP >= N_STACK)
            grow_stack();
        PUSH(car_(v));
        v = cdr_(v);
    }
    n = SP - n - 1;
    v = _applyn(n);
    POPN(n+1);
    return v;
}

value_t fl_applyn(uint32_t n, value_t f, ...)
{
    va_list ap;
    va_start(ap, f);
    size_t i;

    PUSH(f);
    while (SP+n > N_STACK)
        grow_stack();
    for(i=0; i < n; i++) {
        value_t a = va_arg(ap, value_t);
        PUSH(a);
    }
    value_t v = _applyn(n);
    POPN(n+1);
    va_end(ap);
    return v;
}

value_t fl_listn(size_t n, ...)
{
    va_list ap;
    va_start(ap, n);
    uint32_t si = SP;
    size_t i;

    while (SP+n > N_STACK)
        grow_stack();
    for(i=0; i < n; i++) {
        value_t a = va_arg(ap, value_t);
        PUSH(a);
    }
#ifdef MEMDEBUG2
    si = SP-1;
    value_t l = NIL;
    for(i=0; i < n; i++) {
        l = fl_cons(Stack[si--], l);
    }
    POPN(n);
    va_end(ap);
    return l;
#else
    cons_t *c = (cons_t*)alloc_words(n*2);
    cons_t *l = c;
    for(i=0; i < n; i++) {
        c->car = Stack[si++];
        c->cdr = tagptr(c+1, TAG_CONS);
        c++;
    }
    (c-1)->cdr = NIL;
    POPN(n);
    va_end(ap);
    return tagptr(l, TAG_CONS);
#endif
}

value_t fl_list2(value_t a, value_t b)
{
    PUSH(a);
    PUSH(b);
#ifdef MEMDEBUG2
    Stack[SP-1] = fl_cons(b, NIL);
    a = fl_cons(Stack[SP-2], Stack[SP-1]);
    POPN(2);
    return a;
#else
    cons_t *c = (cons_t*)alloc_words(4);
    b = POP();
    a = POP();
    c[0].car = a;
    c[0].cdr = tagptr(c+1, TAG_CONS);
    c[1].car = b;
    c[1].cdr = NIL;
    return tagptr(c, TAG_CONS);
#endif
}

value_t fl_cons(value_t a, value_t b)
{
    PUSH(a);
    PUSH(b);
    value_t c = mk_cons();
    cdr_(c) = POP();
    car_(c) = POP();
    return c;
}

int fl_isnumber(value_t v)
{
    if (isfixnum(v)) return 1;
    if (iscprim(v)) {
        cprim_t *c = (cprim_t*)ptr(v);
        return c->type != wchartype;
    }
    return 0;
}

// read -----------------------------------------------------------------------

#include "read.c"

// equal ----------------------------------------------------------------------

#include "equal.c"

// eval -----------------------------------------------------------------------

#define list(a,n) _list((a),(n),0)

static value_t _list(value_t *args, uint32_t nargs, int star)
{
    cons_t *c;
    int i;
    value_t v;
#ifdef MEMDEBUG2
    value_t n;
    i = nargs-1;
    if (star) {
        n = mk_cons();
        c = (cons_t*)ptr(n);
        c->car = args[i-1];
        c->cdr = args[i];
        i -= 2;
        v = n;
    }
    else {
        v = NIL;
    }
    PUSH(v);
    for(; i >= 0; i--) {
        n = mk_cons();
        c = (cons_t*)ptr(n);
        c->car = args[i];
        c->cdr = Stack[SP-1];
        Stack[SP-1] = n;
    }
    v = POP();
#else
    v = cons_reserve(nargs);
    c = (cons_t*)ptr(v);
    for(i=0; i < nargs; i++) {
        c->car = args[i];
        c->cdr = tagptr(c+1, TAG_CONS);
        c++;
    }
    if (star)
        (c-2)->cdr = (c-1)->car;
    else
        (c-1)->cdr = NIL;
#endif
    return v;
}

static value_t copy_list(value_t L)
{
    if (!iscons(L))
        return NIL;
    PUSH(NIL);
    PUSH(L);
    value_t *plcons = &Stack[SP-2];
    value_t *pL = &Stack[SP-1];
    value_t c;
    c = mk_cons(); PUSH(c);  // save first cons
    car_(c) = car_(*pL);
    cdr_(c) = NIL;
    *plcons = c;
    *pL = cdr_(*pL);
    while (iscons(*pL)) {
        c = mk_cons();
        car_(c) = car_(*pL);
        cdr_(c) = NIL;
        cdr_(*plcons) = c;
        *plcons = c;
        *pL = cdr_(*pL);
    }
    c = POP();  // first cons
    POPN(2);
    return c;
}

static value_t do_trycatch(void)
{
    uint32_t saveSP = SP;
    value_t v;
    value_t thunk = Stack[SP-2];
    Stack[SP-2] = Stack[SP-1];
    Stack[SP-1] = thunk;

    FL_TRY {
        v = apply_cl(0);
    }
    FL_CATCH {
        v = Stack[saveSP-2];
        PUSH(v);
        PUSH(fl_lasterror);
        v = apply_cl(1);
    }
    SP = saveSP;
    return v;
}

/*
  argument layout on stack is
  |--required args--|--opt args--|--kw args--|--rest args...
*/
static uint32_t process_keys(value_t kwtable,
                             uint32_t nreq, uint32_t nkw, uint32_t nopt,
                             uint32_t bp, uint32_t nargs, int va)
{
    uptrint_t n;
    uint32_t extr = nopt+nkw;
    uint32_t ntot = nreq+extr;
    value_t *args = (value_t*)alloca(extr*sizeof(value_t));
    value_t v;
    uint32_t i, a = 0, nrestargs;
    value_t s1 = Stack[SP-1];
    value_t s3 = Stack[SP-3];
    value_t s4 = Stack[SP-4];
    if (nargs < nreq)
        lerror(ArgError, "apply: too few arguments");
    for (i=0; i < extr; i++) args[i] = UNBOUND;
    for (i=nreq; i < nargs; i++) {
        v = Stack[bp+i];
        if (issymbol(v) && iskeyword((symbol_t*)ptr(v)))
            break;
        if (a >= nopt)
            goto no_kw;
        args[a++] = v;
    }
    if (i >= nargs) goto no_kw;
    // now process keywords
    n = vector_size(kwtable)/2;
    do {
        i++;
        if (i >= nargs)
            lerrorf(ArgError, "keyword %s requires an argument",
                    symbol_name(v));
        value_t hv = fixnum(((symbol_t*)ptr(v))->hash);
        uptrint_t x = 2*(labs(numval(hv)) % n);
        if (vector_elt(kwtable, x) == v) {
            uptrint_t idx = numval(vector_elt(kwtable, x+1));
            assert(idx < nkw);
            idx += nopt;
            if (args[idx] == UNBOUND) {
                // if duplicate key, keep first value
                args[idx] = Stack[bp+i];
            }
        }
        else {
            lerrorf(ArgError, "unsupported keyword %s", symbol_name(v));
        }
        i++;
        if (i >= nargs) break;
        v = Stack[bp+i];
    } while (issymbol(v) && iskeyword((symbol_t*)ptr(v)));
 no_kw:
    nrestargs = nargs - i;
    if (!va && nrestargs > 0)
        lerror(ArgError, "apply: too many arguments");
    nargs = ntot + nrestargs;
    if (nrestargs)
        memmove(&Stack[bp+ntot], &Stack[bp+i], nrestargs*sizeof(value_t));
    memcpy(&Stack[bp+nreq], args, extr*sizeof(value_t));
    SP = bp + nargs;
    assert(SP < N_STACK-4);
    PUSH(s4);
    PUSH(s3);
    PUSH(nargs);
    PUSH(s1);
    curr_frame = SP;
    return nargs;
}

#if BYTE_ORDER == BIG_ENDIAN
#define GET_INT32(a)                            \
    ((int32_t)                                  \
    ((((int32_t)a[0])<<0)  |                    \
     (((int32_t)a[1])<<8)  |                    \
     (((int32_t)a[2])<<16) |                    \
     (((int32_t)a[3])<<24)))
#define GET_INT16(a)                            \
    ((int16_t)                                  \
    ((((int16_t)a[0])<<0)  |                    \
     (((int16_t)a[1])<<8)))
#define PUT_INT32(a,i) (*(int32_t*)(a) = bswap_32((int32_t)(i)))
#else
#define GET_INT32(a) (*(int32_t*)a)
#define GET_INT16(a) (*(int16_t*)a)
#define PUT_INT32(a,i) (*(int32_t*)(a) = (int32_t)(i))
#endif
#define SWAP_INT32(a) (*(int32_t*)(a) = bswap_32(*(int32_t*)(a)))
#define SWAP_INT16(a) (*(int16_t*)(a) = bswap_16(*(int16_t*)(a)))

#ifdef USE_COMPUTED_GOTO
#define OP(x) L_##x:
#define NEXT_OP goto *vm_labels[*ip++]
#else
#define OP(x) case x:
#define NEXT_OP goto next_op
#endif

/*
  stack on entry: <func>  <nargs args...>
  caller's responsibility:
  - put the stack in this state
  - provide arg count
  - respect tail position
  - restore SP

  callee's responsibility:
  - check arg counts
  - allocate vararg array
  - push closed env, set up new environment
*/
static value_t apply_cl(uint32_t nargs)
{
    VM_LABELS;
    VM_APPLY_LABELS;
    uint32_t top_frame = curr_frame;
    // frame variables
    uint32_t n=0;
    uint32_t bp;
    const uint8_t *ip;
    fixnum_t s, hi;

    // temporary variables (not necessary to preserve across calls)
#ifndef USE_COMPUTED_GOTO
    uint32_t op;
#endif
    uint32_t i;
    symbol_t *sym;
    static cons_t *c;
    static value_t *pv;
    static int64_t accum;
    static value_t func, v, e;

 apply_cl_top:
    func = Stack[SP-nargs-1];
    ip = (uint8_t*)cv_data((cvalue_t*)ptr(fn_bcode(func)));
#ifndef MEMDEBUG2
    assert(!ismanaged((uptrint_t)ip));
#endif
    while (SP+GET_INT32(ip) > N_STACK) {
        grow_stack();
    }
    ip += 4;

    bp = SP-nargs;
    PUSH(fn_env(func));
    PUSH(curr_frame);
    PUSH(nargs);
    SP++;//PUSH(0); //ip
    curr_frame = SP;

    {
#ifdef USE_COMPUTED_GOTO
    {
        NEXT_OP;
#else
    next_op:
        op = *ip++;
    dispatch:
        switch (op) {
#endif
        OP(OP_ARGC)
            n = *ip++;
        do_argc:
            if (nargs != n) {
                if (nargs > n)
                    lerror(ArgError, "apply: too many arguments");
                else
                    lerror(ArgError, "apply: too few arguments");
            }
            NEXT_OP;
        OP(OP_VARGC)
            i = *ip++;
        do_vargc:
            s = (fixnum_t)nargs - (fixnum_t)i;
            if (s > 0) {
                v = list(&Stack[bp+i], s);
                Stack[bp+i] = v;
                if (s > 1) {
                    Stack[bp+i+1] = Stack[bp+nargs+0];
                    Stack[bp+i+2] = Stack[bp+nargs+1];
                    Stack[bp+i+3] = i+1;
                    Stack[bp+i+4] = 0;
                    SP =  bp+i+5;
                    curr_frame = SP;
                }
            }
            else if (s < 0) {
                lerror(ArgError, "apply: too few arguments");
            }
            else {
                SP++;
                Stack[SP-2] = i+1;
                Stack[SP-3] = Stack[SP-4];
                Stack[SP-4] = Stack[SP-5];
                Stack[SP-5] = NIL;
                curr_frame = SP;
            }
            nargs = i+1;
            NEXT_OP;
        OP(OP_LARGC)
            n = GET_INT32(ip); ip+=4;
            goto do_argc;
        OP(OP_LVARGC)
            i = GET_INT32(ip); ip+=4;
            goto do_vargc;
        OP(OP_BRBOUND)
            i = GET_INT32(ip); ip+=4;
            v = Stack[bp+i];
            if (v != UNBOUND) PUSH(FL_T);
            else PUSH(FL_F);
            NEXT_OP;
        OP(OP_DUP) SP++; Stack[SP-1] = Stack[SP-2]; NEXT_OP;
        OP(OP_POP) POPN(1); NEXT_OP;
        OP(OP_TCALL)
            n = *ip++;  // nargs
        do_tcall:
            func = Stack[SP-n-1];
            if (tag(func) == TAG_FUNCTION) {
                if (func > (N_BUILTINS<<3)) {
                    curr_frame = Stack[curr_frame-3];
                    for(s=-1; s < (fixnum_t)n; s++)
                        Stack[bp+s] = Stack[SP-n+s];
                    SP = bp+n;
                    nargs = n;
                    goto apply_cl_top;
                }
                else {
                    i = uintval(func);
                    if (i <= OP_ASET) {
                        s = builtin_arg_counts[i];
                        if (s >= 0)
                            argcount(builtin_names[i], n, s);
                        else if (s != ANYARGS && (signed)n < -s)
                            argcount(builtin_names[i], n, -s);
                        // remove function arg
                        for(s=SP-n-1; s < (int)SP-1; s++)
                            Stack[s] = Stack[s+1];
                        SP--;
#ifdef USE_COMPUTED_GOTO
                        if (i == OP_APPLY)
                            goto apply_tapply;
                        goto *vm_apply_labels[i];
#else
                        switch (i) {
                        case OP_LIST:   goto apply_list;
                        case OP_VECTOR: goto apply_vector;
                        case OP_APPLY:  goto apply_tapply;
                        case OP_ADD:    goto apply_add;
                        case OP_SUB:    goto apply_sub;
                        case OP_MUL:    goto apply_mul;
                        case OP_DIV:    goto apply_div;
                        default:
                            op = (uint8_t)i;
                            goto dispatch;
                        }
#endif
                    }
                }
            }
            else if (iscbuiltin(func)) {
                s = SP;
                v = ((builtin_t)(((void**)ptr(func))[3]))(&Stack[SP-n], n);
                SP = s-n;
                Stack[SP-1] = v;
                NEXT_OP;
            }
            type_error("apply", "function", func);
        // WARNING: repeated code ahead
        OP(OP_CALL)
            n = *ip++;  // nargs
        do_call:
            func = Stack[SP-n-1];
            if (tag(func) == TAG_FUNCTION) {
                if (func > (N_BUILTINS<<3)) {
                    Stack[curr_frame-1] = (uptrint_t)ip;
                    nargs = n;
                    goto apply_cl_top;
                }
                else {
                    i = uintval(func);
                    if (i <= OP_ASET) {
                        s = builtin_arg_counts[i];
                        if (s >= 0)
                            argcount(builtin_names[i], n, s);
                        else if (s != ANYARGS && (signed)n < -s)
                            argcount(builtin_names[i], n, -s);
                        // remove function arg
                        for(s=SP-n-1; s < (int)SP-1; s++)
                            Stack[s] = Stack[s+1];
                        SP--;
#ifdef USE_COMPUTED_GOTO
                        goto *vm_apply_labels[i];
#else
                        switch (i) {
                        case OP_LIST:   goto apply_list;
                        case OP_VECTOR: goto apply_vector;
                        case OP_APPLY:  goto apply_apply;
                        case OP_ADD:    goto apply_add;
                        case OP_SUB:    goto apply_sub;
                        case OP_MUL:    goto apply_mul;
                        case OP_DIV:    goto apply_div;
                        default:
                            op = (uint8_t)i;
                            goto dispatch;
                        }
#endif
                    }
                }
            }
            else if (iscbuiltin(func)) {
                s = SP;
                v = ((builtin_t)(((void**)ptr(func))[3]))(&Stack[SP-n], n);
                SP = s-n;
                Stack[SP-1] = v;
                NEXT_OP;
            }
            type_error("apply", "function", func);
        OP(OP_TCALLL) n = GET_INT32(ip); ip+=4; goto do_tcall;
        OP(OP_CALLL)  n = GET_INT32(ip); ip+=4; goto do_call;
        OP(OP_JMP) ip += (ptrint_t)GET_INT16(ip); NEXT_OP;
        OP(OP_BRF)
            v = POP();
            if (v == FL_F) ip += (ptrint_t)GET_INT16(ip);
            else ip += 2;
            NEXT_OP;
        OP(OP_BRT)
            v = POP();
            if (v != FL_F) ip += (ptrint_t)GET_INT16(ip);
            else ip += 2;
            NEXT_OP;
        OP(OP_JMPL) ip += (ptrint_t)GET_INT32(ip); NEXT_OP;
        OP(OP_BRFL)
            v = POP();
            if (v == FL_F) ip += (ptrint_t)GET_INT32(ip);
            else ip += 4;
            NEXT_OP;
        OP(OP_BRTL)
            v = POP();
            if (v != FL_F) ip += (ptrint_t)GET_INT32(ip);
            else ip += 4;
            NEXT_OP;
        OP(OP_BRNE)
            if (Stack[SP-2] != Stack[SP-1]) ip += (ptrint_t)GET_INT16(ip);
            else ip += 2;
            POPN(2);
            NEXT_OP;
        OP(OP_BRNEL)
            if (Stack[SP-2] != Stack[SP-1]) ip += (ptrint_t)GET_INT32(ip);
            else ip += 4;
            POPN(2);
            NEXT_OP;
        OP(OP_BRNN)
            v = POP();
            if (v != NIL) ip += (ptrint_t)GET_INT16(ip);
            else ip += 2;
            NEXT_OP;
        OP(OP_BRNNL)
            v = POP();
            if (v != NIL) ip += (ptrint_t)GET_INT32(ip);
            else ip += 4;
            NEXT_OP;
        OP(OP_BRN)
            v = POP();
            if (v == NIL) ip += (ptrint_t)GET_INT16(ip);
            else ip += 2;
            NEXT_OP;
        OP(OP_BRNL)
            v = POP();
            if (v == NIL) ip += (ptrint_t)GET_INT32(ip);
            else ip += 4;
            NEXT_OP;
        OP(OP_RET)
            v = POP();
            SP = curr_frame;
            curr_frame = Stack[SP-3];
            if (curr_frame == top_frame) return v;
            SP -= (4+nargs);
            ip = (uint8_t*)Stack[curr_frame-1];
            nargs        = Stack[curr_frame-2];
            bp           = curr_frame - 4 - nargs;
            Stack[SP-1] = v;
            NEXT_OP;

        OP(OP_EQ)
            Stack[SP-2] = ((Stack[SP-2] == Stack[SP-1]) ? FL_T : FL_F);
            POPN(1); NEXT_OP;
        OP(OP_EQV)
            if (Stack[SP-2] == Stack[SP-1]) {
                v = FL_T;
            }
            else if (!leafp(Stack[SP-2]) || !leafp(Stack[SP-1])) {
                v = FL_F;
            }
            else {
                v = (compare_(Stack[SP-2], Stack[SP-1], 1)==0 ? FL_T : FL_F);
            }
            Stack[SP-2] = v; POPN(1);
            NEXT_OP;
        OP(OP_EQUAL)
            if (Stack[SP-2] == Stack[SP-1]) {
                v = FL_T;
            }
            else {
                v = (compare_(Stack[SP-2], Stack[SP-1], 1)==0 ? FL_T : FL_F);
            }
            Stack[SP-2] = v; POPN(1);
            NEXT_OP;
        OP(OP_PAIRP)
            Stack[SP-1] = (iscons(Stack[SP-1]) ? FL_T : FL_F); NEXT_OP;
        OP(OP_ATOMP)
            Stack[SP-1] = (iscons(Stack[SP-1]) ? FL_F : FL_T); NEXT_OP;
        OP(OP_NOT)
            Stack[SP-1] = ((Stack[SP-1]==FL_F) ? FL_T : FL_F); NEXT_OP;
        OP(OP_NULLP)
            Stack[SP-1] = ((Stack[SP-1]==NIL) ? FL_T : FL_F); NEXT_OP;
        OP(OP_BOOLEANP)
            v = Stack[SP-1];
            Stack[SP-1] = ((v == FL_T || v == FL_F) ? FL_T:FL_F); NEXT_OP;
        OP(OP_SYMBOLP)
            Stack[SP-1] = (issymbol(Stack[SP-1]) ? FL_T : FL_F); NEXT_OP;
        OP(OP_NUMBERP)
            v = Stack[SP-1];
            Stack[SP-1] = (fl_isnumber(v) ? FL_T:FL_F); NEXT_OP;
        OP(OP_FIXNUMP)
            Stack[SP-1] = (isfixnum(Stack[SP-1]) ? FL_T : FL_F); NEXT_OP;
        OP(OP_BOUNDP)
            sym = tosymbol(Stack[SP-1], "bound?");
            Stack[SP-1] = ((sym->binding == UNBOUND) ? FL_F : FL_T);
            NEXT_OP;
        OP(OP_BUILTINP)
            v = Stack[SP-1];
            Stack[SP-1] = (isbuiltin(v) || iscbuiltin(v)) ? FL_T : FL_F;
            NEXT_OP;
        OP(OP_FUNCTIONP)
            v = Stack[SP-1];
            Stack[SP-1] = ((tag(v)==TAG_FUNCTION &&
                            (uintval(v)<=OP_ASET || v>(N_BUILTINS<<3))) ||
                           iscbuiltin(v)) ? FL_T : FL_F;
            NEXT_OP;
        OP(OP_VECTORP)
            Stack[SP-1] = (isvector(Stack[SP-1]) ? FL_T : FL_F); NEXT_OP;

        OP(OP_CONS)
#ifdef MEMDEBUG2
            c = (cons_t*)ptr(mk_cons());
#else
            if (curheap > lim)
                gc(0);
            c = (cons_t*)curheap;
            curheap += sizeof(cons_t);
#endif
            c->car = Stack[SP-2];
            c->cdr = Stack[SP-1];
            Stack[SP-2] = tagptr(c, TAG_CONS);
            POPN(1); NEXT_OP;
        OP(OP_CAR)
            v = Stack[SP-1];
            if (!iscons(v)) type_error("car", "cons", v);
            Stack[SP-1] = car_(v);
            NEXT_OP;
        OP(OP_CDR)
            v = Stack[SP-1];
            if (!iscons(v)) type_error("cdr", "cons", v);
            Stack[SP-1] = cdr_(v);
            NEXT_OP;
        OP(OP_CADR)
            v = Stack[SP-1];
            if (!iscons(v)) type_error("cdr", "cons", v);
            v = cdr_(v);
            if (!iscons(v)) type_error("car", "cons", v);
            Stack[SP-1] = car_(v);
            NEXT_OP;
        OP(OP_SETCAR)
            car(Stack[SP-2]) = Stack[SP-1];
            POPN(1); NEXT_OP;
        OP(OP_SETCDR)
            cdr(Stack[SP-2]) = Stack[SP-1];
            POPN(1); NEXT_OP;
        OP(OP_LIST)
            n = *ip++;
        apply_list:
            if (n > 0) {
                v = list(&Stack[SP-n], n);
                POPN(n);
                PUSH(v);
            }
            else {
                PUSH(NIL);
            }
            NEXT_OP;

        OP(OP_TAPPLY)
            n = *ip++;
        apply_tapply:
            v = POP();     // arglist
            n = SP-(n-2);  // n-2 == # leading arguments not in the list
            while (iscons(v)) {
                if (SP >= N_STACK)
                    grow_stack();
                PUSH(car_(v));
                v = cdr_(v);
            }
            n = SP-n;
            goto do_tcall;
        OP(OP_APPLY)
            n = *ip++;
        apply_apply:
            v = POP();     // arglist
            n = SP-(n-2);  // n-2 == # leading arguments not in the list
            while (iscons(v)) {
                if (SP >= N_STACK)
                    grow_stack();
                PUSH(car_(v));
                v = cdr_(v);
            }
            n = SP-n;
            goto do_call;

        OP(OP_ADD)
            n = *ip++;
        apply_add:
            s = 0;
            i = SP-n;
            for (; i < SP; i++) {
                if (isfixnum(Stack[i])) {
                    s += numval(Stack[i]);
                    if (!fits_fixnum(s)) {
                        i++;
                        goto add_ovf;
                    }
                }
                else {
                add_ovf:
                    v = fl_add_any(&Stack[i], SP-i, s);
                    break;
                }
            }
            if (i==SP)
                v = fixnum(s);
            POPN(n);
            PUSH(v);
            NEXT_OP;
        OP(OP_ADD2)
            if (bothfixnums(Stack[SP-1], Stack[SP-2])) {
                s = numval(Stack[SP-1]) + numval(Stack[SP-2]);
                if (fits_fixnum(s))
                    v = fixnum(s);
                else
                    v = mk_ptrdiff(s);
            }
            else {
                v = fl_add_any(&Stack[SP-2], 2, 0);
            }
            POPN(1);
            Stack[SP-1] = v;
            NEXT_OP;
        OP(OP_SUB)
            n = *ip++;
        apply_sub:
            if (n == 2) goto do_sub2;
            if (n == 1) goto do_neg;
            i = SP-n;
            // we need to pass the full arglist on to fl_add_any
            // so it can handle rest args properly
            PUSH(Stack[i]);
            Stack[i] = fixnum(0);
            Stack[i+1] = fl_neg(fl_add_any(&Stack[i], n, 0));
            Stack[i] = POP();
            v = fl_add_any(&Stack[i], 2, 0);
            POPN(n);
            PUSH(v);
            NEXT_OP;
        OP(OP_NEG)
        do_neg:
            if (isfixnum(Stack[SP-1]))
                Stack[SP-1] = fixnum(-numval(Stack[SP-1]));
            else
                Stack[SP-1] = fl_neg(Stack[SP-1]);
            NEXT_OP;
        OP(OP_SUB2)
        do_sub2:
            if (bothfixnums(Stack[SP-2], Stack[SP-1])) {
                s = numval(Stack[SP-2]) - numval(Stack[SP-1]);
                if (fits_fixnum(s))
                    v = fixnum(s);
                else
                    v = mk_ptrdiff(s);
            }
            else {
                Stack[SP-1] = fl_neg(Stack[SP-1]);
                v = fl_add_any(&Stack[SP-2], 2, 0);
            }
            POPN(1);
            Stack[SP-1] = v;
            NEXT_OP;
        OP(OP_MUL)
            n = *ip++;
        apply_mul:
            accum = 1;
            i = SP-n;
            for (; i < SP; i++) {
                if (isfixnum(Stack[i])) {
                    accum *= numval(Stack[i]);
                }
                else {
                    v = fl_mul_any(&Stack[i], SP-i, accum);
                    break;
                }
            }
            if (i == SP) {
                if (fits_fixnum(accum))
                    v = fixnum(accum);
                else
                    v = return_from_int64(accum);
            }
            POPN(n);
            PUSH(v);
            NEXT_OP;
        OP(OP_DIV)
            n = *ip++;
        apply_div:
            i = SP-n;
            if (n == 1) {
                Stack[SP-1] = fl_div2(fixnum(1), Stack[i]);
            }
            else {
                if (n > 2) {
                    PUSH(Stack[i]);
                    Stack[i] = fixnum(1);
                    Stack[i+1] = fl_mul_any(&Stack[i], n, 1);
                    Stack[i] = POP();
                }
                v = fl_div2(Stack[i], Stack[i+1]);
                POPN(n);
                PUSH(v);
            }
            NEXT_OP;
        OP(OP_IDIV)
            v = Stack[SP-2]; e = Stack[SP-1];
            if (bothfixnums(v, e)) {
                if (e==0) DivideByZeroError();
                v = fixnum(numval(v) / numval(e));
            }
            else
                v = fl_idiv2(v, e);
            POPN(1);
            Stack[SP-1] = v;
            NEXT_OP;
        OP(OP_NUMEQ)
            v = Stack[SP-2]; e = Stack[SP-1];
            if (bothfixnums(v, e))
                v = (v == e) ? FL_T : FL_F;
            else
                v = (!numeric_compare(v,e,1,0,"=")) ? FL_T : FL_F;
            POPN(1);
            Stack[SP-1] = v;
            NEXT_OP;
        OP(OP_LT)
            if (bothfixnums(Stack[SP-2], Stack[SP-1])) {
                v = (numval(Stack[SP-2]) < numval(Stack[SP-1])) ? FL_T : FL_F;
            }
            else {
                v = (numval(fl_compare(Stack[SP-2], Stack[SP-1])) < 0) ?
                    FL_T : FL_F;
            }
            POPN(1);
            Stack[SP-1] = v;
            NEXT_OP;
        OP(OP_COMPARE)
            Stack[SP-2] = compare_(Stack[SP-2], Stack[SP-1], 0);
            POPN(1);
            NEXT_OP;

        OP(OP_VECTOR)
            n = *ip++;
        apply_vector:
            v = alloc_vector(n, 0);
            if (n) {
                memcpy(&vector_elt(v,0), &Stack[SP-n], n*sizeof(value_t));
                POPN(n);
            }
            PUSH(v);
            NEXT_OP;

        OP(OP_AREF)
            v = Stack[SP-2];
            if (isvector(v)) {
                e = Stack[SP-1];
                if (isfixnum(e))
                    i = numval(e);
                else
                    i = (uint32_t)tosize(e, "aref");
                if ((unsigned)i >= vector_size(v))
                    bounds_error("aref", v, e);
                v = vector_elt(v, i);
            }
            else if (isarray(v)) {
                v = cvalue_array_aref(&Stack[SP-2]);
            }
            else {
                type_error("aref", "sequence", v);
            }
            POPN(1);
            Stack[SP-1] = v;
            NEXT_OP;
        OP(OP_ASET)
            e = Stack[SP-3];
            if (isvector(e)) {
                i = tofixnum(Stack[SP-2], "aset!");
                if ((unsigned)i >= vector_size(e))
                    bounds_error("aset!", v, Stack[SP-1]);
                vector_elt(e, i) = (v=Stack[SP-1]);
            }
            else if (isarray(e)) {
                v = cvalue_array_aset(&Stack[SP-3]);
            }
            else {
                type_error("aset!", "sequence", e);
            }
            POPN(2);
            Stack[SP-1] = v;
            NEXT_OP;
        OP(OP_FOR)
            s  = tofixnum(Stack[SP-3], "for");
            hi = tofixnum(Stack[SP-2], "for");
            //f = Stack[SP-1];
            v = FL_UNSPECIFIED;
            SP += 2;
            n = SP;
            for(; s <= hi; s++) {
                Stack[SP-2] = Stack[SP-3];
                Stack[SP-1] = fixnum(s);
                v = apply_cl(1);
                SP = n;
            }
            POPN(4);
            Stack[SP-1] = v;
            NEXT_OP;

        OP(OP_LOADT) PUSH(FL_T); NEXT_OP;
        OP(OP_LOADF) PUSH(FL_F); NEXT_OP;
        OP(OP_LOADNIL) PUSH(NIL); NEXT_OP;
        OP(OP_LOAD0) PUSH(fixnum(0)); NEXT_OP;
        OP(OP_LOAD1) PUSH(fixnum(1)); NEXT_OP;
        OP(OP_LOADI8) s = (int8_t)*ip++; PUSH(fixnum(s)); NEXT_OP;
        OP(OP_LOADV)
            v = fn_vals(Stack[bp-1]);
            assert(*ip < vector_size(v));
            v = vector_elt(v, *ip); ip++;
            PUSH(v);
            NEXT_OP;
        OP(OP_LOADVL)
            v = fn_vals(Stack[bp-1]);
            v = vector_elt(v, GET_INT32(ip)); ip+=4;
            PUSH(v);
            NEXT_OP;
        OP(OP_LOADGL)
            v = fn_vals(Stack[bp-1]);
            v = vector_elt(v, GET_INT32(ip)); ip+=4;
            goto do_loadg;
        OP(OP_LOADG)
            v = fn_vals(Stack[bp-1]);
            assert(*ip < vector_size(v));
            v = vector_elt(v, *ip); ip++;
        do_loadg:
            assert(issymbol(v));
            sym = (symbol_t*)ptr(v);
            if (sym->binding == UNBOUND)
                fl_raise(fl_list2(UnboundError, v));
            PUSH(sym->binding);
            NEXT_OP;

        OP(OP_SETGL)
            v = fn_vals(Stack[bp-1]);
            v = vector_elt(v, GET_INT32(ip)); ip+=4;
            goto do_setg;
        OP(OP_SETG)
            v = fn_vals(Stack[bp-1]);
            assert(*ip < vector_size(v));
            v = vector_elt(v, *ip); ip++;
        do_setg:
            assert(issymbol(v));
            sym = (symbol_t*)ptr(v);
            v = Stack[SP-1];
            if (!isconstant(sym))
                sym->binding = v;
            NEXT_OP;

        OP(OP_LOADA)
            i = *ip++;
            v = Stack[bp+i];
            PUSH(v);
            NEXT_OP;
        OP(OP_LOADA0)
            v = Stack[bp];
            PUSH(v);
            NEXT_OP;
        OP(OP_LOADA1)
            v = Stack[bp+1];
            PUSH(v);
            NEXT_OP;
        OP(OP_LOADAL)
            i = GET_INT32(ip); ip+=4;
            v = Stack[bp+i];
            PUSH(v);
            NEXT_OP;
        OP(OP_SETA)
            v = Stack[SP-1];
            i = *ip++;
            Stack[bp+i] = v;
            NEXT_OP;
        OP(OP_SETAL)
            v = Stack[SP-1];
            i = GET_INT32(ip); ip+=4;
            Stack[bp+i] = v;
            NEXT_OP;

        OP(OP_BOX)
            i = *ip++;
            v = mk_cons();
            car_(v) = Stack[bp+i];
            cdr_(v) = NIL;
            Stack[bp+i] = v;
            NEXT_OP;
        OP(OP_BOXL)
            i = GET_INT32(ip); ip+=4;
            v = mk_cons();
            car_(v) = Stack[bp+i];
            cdr_(v) = NIL;
            Stack[bp+i] = v;
            NEXT_OP;

        OP(OP_SHIFT)
            i = *ip++;
            Stack[SP-1-i] = Stack[SP-1];
            SP -= i;
            NEXT_OP;

        OP(OP_LOADC)
            i = *ip++;
            v = Stack[bp+nargs];
            assert(isvector(v));
            assert(i < vector_size(v));
            PUSH(vector_elt(v, i));
            NEXT_OP;

        OP(OP_LOADC0)
            PUSH(vector_elt(Stack[bp+nargs], 0));
            NEXT_OP;
        OP(OP_LOADC1)
            PUSH(vector_elt(Stack[bp+nargs], 1));
            NEXT_OP;

        OP(OP_LOADCL)
            i = GET_INT32(ip); ip+=4;
            v = Stack[bp+nargs];
            PUSH(vector_elt(v, i));
            NEXT_OP;

        OP(OP_CLOSURE)
            n = *ip++;
            assert(n > 0);
            pv = alloc_words(n + 1);
            v = tagptr(pv, TAG_VECTOR);
            pv[0] = fixnum(n);
            i = 1;
            do {
                pv[i] = Stack[SP-n + i-1];
                i++;
            } while (i<=n);
            POPN(n);
            PUSH(v);
#ifdef MEMDEBUG2
            pv = alloc_words(4);
#else
            if ((value_t*)curheap > ((value_t*)lim)-2)
                gc(0);
            pv = (value_t*)curheap;
            curheap += (4*sizeof(value_t));
#endif
            e = Stack[SP-2];  // closure to copy
            assert(isfunction(e));
            pv[0] = ((value_t*)ptr(e))[0];
            pv[1] = ((value_t*)ptr(e))[1];
            pv[2] = Stack[SP-1];  // env
            pv[3] = ((value_t*)ptr(e))[3];
            POPN(1);
            Stack[SP-1] = tagptr(pv, TAG_FUNCTION);
            NEXT_OP;

        OP(OP_TRYCATCH)
            v = do_trycatch();
            POPN(1);
            Stack[SP-1] = v;
            NEXT_OP;

        OP(OP_OPTARGS)
            i = GET_INT32(ip); ip+=4;
            n = GET_INT32(ip); ip+=4;
            if (nargs < i)
                lerror(ArgError, "apply: too few arguments");
            if ((int32_t)n > 0) {
                if (nargs > n)
                    lerror(ArgError, "apply: too many arguments");
            }
            else n = -n;
            if (n > nargs) {
                n -= nargs;
                SP += n;
                Stack[SP-1] = Stack[SP-n-1];
                Stack[SP-2] = nargs+n;
                Stack[SP-3] = Stack[SP-n-3];
                Stack[SP-4] = Stack[SP-n-4];
                curr_frame = SP;
                for(i=0; i < n; i++) {
                    Stack[bp+nargs+i] = UNBOUND;
                }
                nargs += n;
            }
            NEXT_OP;
        OP(OP_KEYARGS)
            v = fn_vals(Stack[bp-1]);
            v = vector_elt(v, 0);
            i = GET_INT32(ip); ip+=4;
            n = GET_INT32(ip); ip+=4;
            s = GET_INT32(ip); ip+=4;
            nargs = process_keys(v, i, n, abs(s)-(i+n), bp, nargs, s<0);
            NEXT_OP;

#ifndef USE_COMPUTED_GOTO
        default:
            goto dispatch;
#endif
        }
    }
#ifdef USE_COMPUTED_GOTO
    return UNBOUND;  // not reached
#else
    goto dispatch;
#endif
}

static uint32_t compute_maxstack(uint8_t *code, size_t len, int bswap)
{
    uint8_t *ip = code+4, *end = code+len;
    uint8_t op;
    uint32_t i, n, sp = 0, maxsp = 0;

    while (1) {
        if ((int32_t)sp > (int32_t)maxsp) maxsp = sp;
        if (ip >= end) break;
        op = *ip++;
        switch (op) {
        case OP_ARGC:
            n = *ip++;
            break;
        case OP_VARGC:
            n = *ip++;
            sp += (n+2);
            break;
        case OP_LARGC:
            if (bswap) SWAP_INT32(ip);
            n = GET_INT32(ip); ip+=4;
            break;
        case OP_LVARGC:
            if (bswap) SWAP_INT32(ip);
            n = GET_INT32(ip); ip+=4;
            sp += (n+2);
            break;
        case OP_OPTARGS:
            if (bswap) SWAP_INT32(ip);
            i = GET_INT32(ip); ip+=4;
            if (bswap) SWAP_INT32(ip);
            n = abs(GET_INT32(ip)); ip+=4;
            sp += (n-i);
            break;
        case OP_KEYARGS:
            if (bswap) SWAP_INT32(ip);
            i = GET_INT32(ip); ip+=4;
            if (bswap) SWAP_INT32(ip);
            n = GET_INT32(ip); ip+=4;
            if (bswap) SWAP_INT32(ip);
            n = abs(GET_INT32(ip)); ip+=4;
            sp += (n-i);
            break;
        case OP_BRBOUND:
            if (bswap) SWAP_INT32(ip);
            ip+=4;
            sp++;
            break;

        case OP_TCALL: case OP_CALL:
            n = *ip++;  // nargs
            sp -= n;
            break;
        case OP_TCALLL: case OP_CALLL:
            if (bswap) SWAP_INT32(ip);
            n = GET_INT32(ip); ip+=4;
            sp -= n;
            break;
        case OP_JMP:
            if (bswap) SWAP_INT16(ip);
            ip += 2; break;
        case OP_JMPL:
            if (bswap) SWAP_INT32(ip);
            ip += 4; break;
        case OP_BRF: case OP_BRT:
            if (bswap) SWAP_INT16(ip);
            ip+=2;
            sp--;
            break;
        case OP_BRFL: case OP_BRTL:
            if (bswap) SWAP_INT32(ip);
            ip += 4;
            sp--;
            break;
        case OP_BRNE:
            if (bswap) SWAP_INT16(ip);
            ip += 2;
            sp -= 2;
            break;
        case OP_BRNEL:
            if (bswap) SWAP_INT32(ip);
            ip += 4;
            sp -= 2;
            break;
        case OP_BRNN: case OP_BRN:
            if (bswap) SWAP_INT16(ip);
            ip += 2;
            sp--;
            break;
        case OP_BRNNL: case OP_BRNL:
            if (bswap) SWAP_INT32(ip);
            ip += 4;
            sp--;
            break;
        case OP_RET: sp--; break;

        case OP_CONS: case OP_SETCAR: case OP_SETCDR: case OP_POP:
        case OP_EQ: case OP_EQV: case OP_EQUAL: case OP_ADD2: case OP_SUB2:
        case OP_IDIV: case OP_NUMEQ: case OP_LT: case OP_COMPARE:
        case OP_AREF: case OP_TRYCATCH:
            sp--;
            break;

        case OP_PAIRP: case OP_ATOMP: case OP_NOT: case OP_NULLP:
        case OP_BOOLEANP: case OP_SYMBOLP: case OP_NUMBERP: case OP_FIXNUMP:
        case OP_BOUNDP: case OP_BUILTINP: case OP_FUNCTIONP: case OP_VECTORP:
        case OP_NOP: case OP_CAR: case OP_CDR: case OP_NEG:
            break;

        case OP_TAPPLY: case OP_APPLY:
            n = *ip++;
            sp -= (n-1);
            break;

        case OP_LIST: case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV:
        case OP_VECTOR:
            n = *ip++;
            sp -= (n-1);
            break;
        case OP_CLOSURE:
            n = *ip++;
            sp -= n;
            break;
        case OP_SHIFT:
            n = *ip++;
            sp -= n;
            break;

        case OP_ASET:
            sp -= 2;
            break;
        case OP_FOR:
            if (sp+2 > maxsp) maxsp = sp+2;
            sp -=2;
            break;

        case OP_LOADT: case OP_LOADF: case OP_LOADNIL: case OP_LOAD0:
        case OP_LOAD1: case OP_LOADA0: case OP_LOADA1: case OP_DUP:
        case OP_LOADC0: case OP_LOADC1:
            sp++;
            break;

        case OP_LOADI8: case OP_LOADV: case OP_LOADG: case OP_LOADA:
            ip++;
            sp++;
            break;
        case OP_LOADVL: case OP_LOADGL: case OP_LOADAL:
            if (bswap) SWAP_INT32(ip);
            ip+=4;
            sp++;
            break;

        case OP_SETG: case OP_SETA: case OP_BOX:
            ip++;
            break;
        case OP_SETGL: case OP_SETAL: case OP_BOXL:
            if (bswap) SWAP_INT32(ip);
            ip+=4;
            break;

        case OP_LOADC: ip+=1; sp++; break;
        case OP_LOADCL:
            if (bswap) SWAP_INT32(ip);
            ip+=4;
            sp++; break;
        }
    }
    return maxsp+4;
}

// top = top frame pointer to start at
static value_t _stacktrace(uint32_t top)
{
    uint32_t bp, sz;
    value_t v, lst = NIL;
    fl_gc_handle(&lst);
    while (top > 0) {
        sz = Stack[top-2]+1;
        bp = top-4-sz;
        v = alloc_vector(sz, 0);
        memcpy(&vector_elt(v,0), &Stack[bp], sz*sizeof(value_t));
        lst = fl_cons(v, lst);
        top = Stack[top-3];
    }
    fl_free_gc_handles(1);
    return lst;
}

// builtins -------------------------------------------------------------------

void assign_global_builtins(builtinspec_t *b)
{
    while (b->name != NULL) {
        setc(symbol(b->name), cbuiltin(b->name, b->fptr));
        b++;
    }
}

static value_t fl_function(value_t *args, uint32_t nargs)
{
    if (nargs == 1 && issymbol(args[0]))
        return fl_builtin(args, nargs);
    if (nargs < 2 || nargs > 4)
        argcount("function", nargs, 2);
    if (!fl_isstring(args[0]))
        type_error("function", "string", args[0]);
    if (!isvector(args[1]))
        type_error("function", "vector", args[1]);
    cvalue_t *arr = (cvalue_t*)ptr(args[0]);
    cv_pin(arr);
    char *data = (char*)cv_data(arr);
    int swap = 0;
    if ((uint8_t)data[4] >= N_OPCODES) {
        // read syntax, shifted 48 for compact text representation
        size_t i, sz = cv_len(arr);
        for(i=0; i < sz; i++)
            data[i] -= 48;
    }
    else {
#if BYTE_ORDER == BIG_ENDIAN
        swap = 1;
#endif
    }
    uint32_t ms = compute_maxstack((uint8_t*)data, cv_len(arr), swap);
    PUT_INT32(data, ms);
    function_t *fn = (function_t*)alloc_words(4);
    value_t fv = tagptr(fn, TAG_FUNCTION);
    fn->bcode = args[0];
    fn->vals = args[1];
    fn->env = NIL;
    fn->name = LAMBDA;
    if (nargs > 2) {
        if (issymbol(args[2])) {
            fn->name = args[2];
            if (nargs > 3)
                fn->env = args[3];
        }
        else {
            fn->env = args[2];
            if (nargs > 3) {
                if (!issymbol(args[3]))
                    type_error("function", "symbol", args[3]);
                fn->name = args[3];
            }
        }
        if (isgensym(fn->name))
            lerror(ArgError, "function: name should not be a gensym");
    }
    return fv;
}

static value_t fl_function_code(value_t *args, uint32_t nargs)
{
    argcount("function:code", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error("function:code", "function", v);
    return fn_bcode(v);
}
static value_t fl_function_vals(value_t *args, uint32_t nargs)
{
    argcount("function:vals", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error("function:vals", "function", v);
    return fn_vals(v);
}
static value_t fl_function_env(value_t *args, uint32_t nargs)
{
    argcount("function:env", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error("function:env", "function", v);
    return fn_env(v);
}
static value_t fl_function_name(value_t *args, uint32_t nargs)
{
    argcount("function:name", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error("function:name", "function", v);
    return fn_name(v);
}

value_t fl_copylist(value_t *args, u_int32_t nargs)
{
    argcount("copy-list", nargs, 1);
    return copy_list(args[0]);
}

value_t fl_append(value_t *args, u_int32_t nargs)
{
    if (nargs == 0)
        return NIL;
    value_t first=NIL, lst, lastcons=NIL;
    fl_gc_handle(&first);
    fl_gc_handle(&lastcons);
    uint32_t i=0;
    while (1) {
        lst = args[i++];
        if (i >= nargs) break;
        if (iscons(lst)) {
            lst = copy_list(lst);
            if (first == NIL)
                first = lst;
            else
                cdr_(lastcons) = lst;
#ifdef MEMDEBUG2
            lastcons = lst;
            while (cdr_(lastcons) != NIL)
                lastcons = cdr_(lastcons);
#else
            lastcons = tagptr((((cons_t*)curheap)-1), TAG_CONS);
#endif
        }
        else if (lst != NIL) {
            type_error("append", "cons", lst);
        }
    }
    if (first == NIL)
        first = lst;
    else
        cdr_(lastcons) = lst;
    fl_free_gc_handles(2);
    return first;
}

value_t fl_liststar(value_t *args, u_int32_t nargs)
{
    if (nargs == 1) return args[0];
    else if (nargs == 0) argcount("list*", nargs, 1);
    return _list(args, nargs, 1);
}

value_t fl_stacktrace(value_t *args, u_int32_t nargs)
{
    (void)args;
    argcount("stacktrace", nargs, 0);
    return _stacktrace(fl_throwing_frame ? fl_throwing_frame : curr_frame);
}

value_t fl_map1(value_t *args, u_int32_t nargs)
{
    if (nargs < 2)
        lerror(ArgError, "map: too few arguments");
    if (!iscons(args[1])) return NIL;
    value_t v;
    uint32_t first, last, argSP = args-Stack;
    assert(argSP >= 0 && argSP < N_STACK);
    if (nargs == 2) {
        if (SP+4 > N_STACK) grow_stack();
        PUSH(Stack[argSP]);
        PUSH(car_(Stack[argSP+1]));
        v = _applyn(1);
        POPN(2);
        PUSH(v);
        v = mk_cons();
        car_(v) = POP(); cdr_(v) = NIL;
        PUSH(v);
        PUSH(v);
        first = SP-2;
        last = SP-1;
        Stack[argSP+1] = cdr_(Stack[argSP+1]);
        while (iscons(Stack[argSP+1])) {
            PUSH(Stack[argSP]);
            PUSH(car_(Stack[argSP+1]));
            v = _applyn(1);
            POPN(2);
            PUSH(v);
            v = mk_cons();
            car_(v) = POP(); cdr_(v) = NIL;
            cdr_(Stack[last]) = v;
            Stack[last] = v;
            Stack[argSP+1] = cdr_(Stack[argSP+1]);
        }
        POPN(2);
    }
    else {
        size_t i;
        while (SP+nargs+1 > N_STACK) grow_stack();
        PUSH(Stack[argSP]);
        for(i=1; i < nargs; i++) {
            PUSH(car(Stack[argSP+i]));
            Stack[argSP+i] = cdr_(Stack[argSP+i]);
        }
        v = _applyn(nargs-1);
        POPN(nargs);
        PUSH(v);
        v = mk_cons();
        car_(v) = POP(); cdr_(v) = NIL;
        PUSH(v);
        PUSH(v);
        first = SP-2;
        last = SP-1;
        while (iscons(Stack[argSP+1])) {
            PUSH(Stack[argSP]);
            for(i=1; i < nargs; i++) {
                PUSH(car(Stack[argSP+i]));
                Stack[argSP+i] = cdr_(Stack[argSP+i]);
            }
            v = _applyn(nargs-1);
            POPN(nargs);
            PUSH(v);
            v = mk_cons();
            car_(v) = POP(); cdr_(v) = NIL;
            cdr_(Stack[last]) = v;
            Stack[last] = v;
        }
        POPN(2);
    }
    return Stack[first];
}

static builtinspec_t core_builtin_info[] = {
    { "function", fl_function },
    { "function:code", fl_function_code },
    { "function:vals", fl_function_vals },
    { "function:env", fl_function_env },
    { "function:name", fl_function_name },
    { "stacktrace", fl_stacktrace },
    { "gensym", fl_gensym },
    { "gensym?", fl_gensymp },
    { "hash", fl_hash },
    { "copy-list", fl_copylist },
    { "append", fl_append },
    { "list*", fl_liststar },
    { "map", fl_map1 },
    { NULL, NULL }
};

// initialization -------------------------------------------------------------

extern void builtins_init(void);
extern void comparehash_init(void);

static void lisp_init(size_t initial_heapsize)
{
    int i;

    libsupport_init();

    heapsize = initial_heapsize;

    fromspace = (unsigned char*)LLT_ALLOC(heapsize);
#ifdef MEMDEBUG
    tospace   = NULL;
#else
    tospace   = (unsigned char*)LLT_ALLOC(heapsize);
#endif
    curheap = fromspace;
    lim = curheap+heapsize-sizeof(cons_t);
    consflags = bitvector_new(heapsize/sizeof(cons_t), 1);
    htable_new(&printconses, 32);
    comparehash_init();
    N_STACK = 262144;
    Stack = (value_t*)malloc(N_STACK*sizeof(value_t));

    FL_NIL = NIL = builtin(OP_THE_EMPTY_LIST);
    FL_T = builtin(OP_BOOL_CONST_T);
    FL_F = builtin(OP_BOOL_CONST_F);
    FL_EOF = builtin(OP_EOF_OBJECT);
    LAMBDA = symbol("lambda");        FUNCTION = symbol("function");
    QUOTE = symbol("quote");          TRYCATCH = symbol("trycatch");
    BACKQUOTE = symbol("quasiquote");       COMMA = symbol("unquote");
    COMMAAT = symbol("unquote-splicing");   COMMADOT = symbol("unquote-nsplicing");
    IOError = symbol("io-error");     ParseError = symbol("parse-error");
    TypeError = symbol("type-error"); ArgError = symbol("arg-error");
    UnboundError = symbol("unbound-error");
    KeyError = symbol("key-error");   MemoryError = symbol("memory-error");
    BoundsError = symbol("bounds-error");
    DivideError = symbol("divide-error");
    EnumerationError = symbol("enumeration-error");
    Error = symbol("error");          pairsym = symbol("pair");
    symbolsym = symbol("symbol");     fixnumsym = symbol("fixnum");
    vectorsym = symbol("vector");     builtinsym = symbol("builtin");
    booleansym = symbol("boolean");   nullsym = symbol("null");
    definesym = symbol("define");     defmacrosym = symbol("define-macro");
    forsym = symbol("for");
    setqsym = symbol("set!");         evalsym = symbol("eval");
    vu8sym = symbol("vu8");           fnsym = symbol("fn");
    nulsym = symbol("nul");           alarmsym = symbol("alarm");
    backspacesym = symbol("backspace"); tabsym = symbol("tab");
    linefeedsym = symbol("linefeed"); vtabsym = symbol("vtab");
    pagesym = symbol("page");         returnsym = symbol("return");
    escsym = symbol("esc");           spacesym = symbol("space");
    deletesym = symbol("delete");     newlinesym = symbol("newline");
    tsym = symbol("t"); Tsym = symbol("T");
    fsym = symbol("f"); Fsym = symbol("F");
    set(printprettysym=symbol("*print-pretty*"), FL_T);
    set(printreadablysym=symbol("*print-readably*"), FL_T);
    set(printwidthsym=symbol("*print-width*"), fixnum(SCR_WIDTH));
    set(printlengthsym=symbol("*print-length*"), FL_F);
    set(printlevelsym=symbol("*print-level*"), FL_F);
    builtins_table_sym = symbol("*builtins*");
    fl_lasterror = NIL;
    i = 0;
    for (i=OP_EQ; i <= OP_ASET; i++) {
        setc(symbol(builtin_names[i]), builtin(i));
    }
    setc(symbol("eq"), builtin(OP_EQ));
    setc(symbol("procedure?"), builtin(OP_FUNCTIONP));
    setc(symbol("top-level-bound?"), builtin(OP_BOUNDP));

#if defined(_OS_LINUX_)
    set(symbol("*os-name*"), symbol("linux"));
#elif defined(_OS_WINDOWS_)
    set(symbol("*os-name*"), symbol("win32"));
#elif defined(_OS_DARWIN_)
    set(symbol("*os-name*"), symbol("macos"));
#else
    set(symbol("*os-name*"), symbol("unknown"));
#endif

    the_empty_vector = tagptr(alloc_words(1), TAG_VECTOR);
    vector_setsize(the_empty_vector, 0);

    cvalues_init();

    char exename[1024];
    size_t exe_size = sizeof(exename) / sizeof(exename[0]);
    if ( uv_exepath(exename, &exe_size) == 0 ) {
        setc(symbol("*install-dir*"), cvalue_static_cstring(strdup(dirname(exename))));
    }

    memory_exception_value = fl_list2(MemoryError,
                                      cvalue_static_cstring("out of memory"));

    assign_global_builtins(core_builtin_info);

    builtins_init();
}

// top level ------------------------------------------------------------------

value_t fl_toplevel_eval(value_t expr)
{
    return fl_applyn(1, symbol_value(evalsym), expr);
}

extern void fl_init_julia_extensions(void);

void fl_init(size_t initial_heapsize)
{
    lisp_init(initial_heapsize);
    fl_init_julia_extensions();
}

int fl_load_system_image(value_t sys_image_iostream)
{
    value_t e;
    int saveSP;
    symbol_t *sym;

    PUSH(sys_image_iostream);
    saveSP = SP;
    FL_TRY {
        while (1) {
            e = fl_read_sexpr(Stack[SP-1]);
            if (ios_eof(value2c(ios_t*,Stack[SP-1]))) break;
            if (isfunction(e)) {
                // stage 0 format: series of thunks
                PUSH(e);
                (void)_applyn(0);
                SP = saveSP;
            }
            else {
                // stage 1 format: list alternating symbol/value
                while (iscons(e)) {
                    sym = tosymbol(car_(e), "bootstrap");
                    e = cdr_(e);
                    (void)tocons(e, "bootstrap");
                    sym->binding = car_(e);
                    e = cdr_(e);
                }
                break;
            }
        }
    }
    FL_CATCH {
        ios_puts("fatal error during bootstrap:\n", ios_stderr);
        fl_print(ios_stderr, fl_lasterror);
        ios_putc('\n', ios_stderr);
        return 1;
    }
    ios_close(value2c(ios_t*,Stack[SP-1]));
    POPN(1);
    return 0;
}

#ifdef __cplusplus
}
#endif
