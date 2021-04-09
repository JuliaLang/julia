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

#if defined(_OS_WINDOWS_) && !defined(_COMPILER_GCC_)
#include <malloc.h>
JL_DLLEXPORT char * dirname(char *);
#else
#include <libgen.h>
#endif

static const char *const builtin_names[] =
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

static const short builtin_arg_counts[] =
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, ANYARGS, 1, 1, 2, 2,
      -2,
      ANYARGS, -1, ANYARGS, -1, 2,  2, 2, 2,
      ANYARGS, 2, 3 };

#define PUSH(fl_ctx, v) (fl_ctx->Stack[fl_ctx->SP++] = (v))
#define POP(fl_ctx)   (fl_ctx->Stack[--fl_ctx->SP])
#define POPN(fl_ctx, n) (fl_ctx->SP-=(n))

static value_t apply_cl(fl_context_t *fl_ctx, uint32_t nargs);
static value_t *alloc_words(fl_context_t *fl_ctx, int n);
static value_t relocate(fl_context_t *fl_ctx, value_t v);

typedef struct _fl_readstate_t {
    htable_t backrefs;
    htable_t gensyms;
    value_t source;
    struct _fl_readstate_t *prev;
} fl_readstate_t;

static void free_readstate(fl_readstate_t *rs)
{
    htable_free(&rs->backrefs);
    htable_free(&rs->gensyms);
}

// error utilities ------------------------------------------------------------

#define FL_TRY(fl_ctx)                           \
  fl_exception_context_t _ctx; int l__tr, l__ca; \
  _ctx.sp=fl_ctx->SP; _ctx.frame=fl_ctx->curr_frame; _ctx.rdst=fl_ctx->readstate; _ctx.prev=fl_ctx->exc_ctx; \
  _ctx.ngchnd = fl_ctx->N_GCHND; fl_ctx->exc_ctx = &_ctx;                                    \
  if (!fl_setjmp(_ctx.buf)) \
    for (l__tr=1; l__tr; l__tr=0, (void)(fl_ctx->exc_ctx=fl_ctx->exc_ctx->prev))

#define FL_CATCH(fl_ctx)                                                \
    else                                                                \
        for(l__ca=1; l__ca; l__ca=0,                                    \
                fl_ctx->lasterror=fl_ctx->NIL,fl_ctx->throwing_frame=0,fl_ctx->SP=_ctx.sp,fl_ctx->curr_frame=_ctx.frame)

void fl_savestate(fl_context_t *fl_ctx, fl_exception_context_t *_ctx)
{
    _ctx->sp = fl_ctx->SP;
    _ctx->frame = fl_ctx->curr_frame;
    _ctx->rdst = fl_ctx->readstate;
    _ctx->prev = fl_ctx->exc_ctx;
    _ctx->ngchnd = fl_ctx->N_GCHND;
}

void fl_restorestate(fl_context_t *fl_ctx, fl_exception_context_t *_ctx)
{
    fl_ctx->lasterror = fl_ctx->NIL;
    fl_ctx->throwing_frame = 0;
    fl_ctx->SP = _ctx->sp;
    fl_ctx->curr_frame = _ctx->frame;
}

void fl_raise(fl_context_t *fl_ctx, value_t e)
{
    fl_ctx->lasterror = e;
    // unwind read state
    while (fl_ctx->readstate != (fl_readstate_t*)fl_ctx->exc_ctx->rdst) {
        free_readstate(fl_ctx->readstate);
        fl_ctx->readstate = fl_ctx->readstate->prev;
    }
    if (fl_ctx->throwing_frame == 0)
        fl_ctx->throwing_frame = fl_ctx->curr_frame;
    fl_ctx->N_GCHND = fl_ctx->exc_ctx->ngchnd;
    fl_exception_context_t *thisctx = fl_ctx->exc_ctx;
    if (fl_ctx->exc_ctx->prev)   // don't throw past toplevel
        fl_ctx->exc_ctx = fl_ctx->exc_ctx->prev;
    fl_longjmp(thisctx->buf, 1);
}

static value_t make_error_msg(fl_context_t *fl_ctx, const char *format, va_list args)
{
    char msgbuf[512];
    size_t len = vsnprintf(msgbuf, sizeof(msgbuf), format, args);
    return string_from_cstrn(fl_ctx, msgbuf, len);
}

void lerrorf(fl_context_t *fl_ctx, value_t e, const char *format, ...)
{
    va_list args;
    PUSH(fl_ctx, e);
    va_start(args, format);
    value_t msg = make_error_msg(fl_ctx, format, args);
    va_end(args);

    e = POP(fl_ctx);
    fl_raise(fl_ctx, fl_list2(fl_ctx, e, msg));
}

void lerror(fl_context_t *fl_ctx, value_t e, const char *msg)
{
    PUSH(fl_ctx, e);
    value_t m = cvalue_static_cstring(fl_ctx, msg);
    e = POP(fl_ctx);
    fl_raise(fl_ctx, fl_list2(fl_ctx, e, m));
}

void type_error(fl_context_t *fl_ctx, const char *fname, const char *expected, value_t got)
{
    fl_raise(fl_ctx, fl_listn(fl_ctx, 4, fl_ctx->TypeError, symbol(fl_ctx, fname), symbol(fl_ctx, expected), got));
}

void bounds_error(fl_context_t *fl_ctx, const char *fname, value_t arr, value_t ind)
{
    fl_raise(fl_ctx, fl_listn(fl_ctx, 4, fl_ctx->BoundsError, symbol(fl_ctx, fname), arr, ind));
}

// safe cast operators --------------------------------------------------------

#define isstring(v) fl_isstring(fl_ctx, v)
#define SAFECAST_OP(type,ctype,cnvt)                                    \
    ctype to##type(fl_context_t *fl_ctx, value_t v, const char *fname)  \
    {                                                                   \
        if (is##type(v))                                                \
            return (ctype)cnvt(v);                                      \
        type_error(fl_ctx, fname, #type, v);                            \
    }
SAFECAST_OP(cons,  cons_t*,  ptr)
SAFECAST_OP(symbol,symbol_t*,ptr)
SAFECAST_OP(fixnum,fixnum_t, numval)
SAFECAST_OP(string,char*,    cvalue_data)
#undef isstring

// symbol table ---------------------------------------------------------------

int fl_is_keyword_name(const char *str, size_t len)
{
    return len>1 && ((str[0] == ':' || str[len-1] == ':') && str[1] != '\0');
}

#define CHECK_ALIGN8(p) assert((((uintptr_t)(p))&0x7)==0 && "flisp requires malloc to return 8-aligned pointers")

static symbol_t *mk_symbol(const char *str)
{
    symbol_t *sym;
    size_t len = strlen(str);

    sym = (symbol_t*)malloc((offsetof(symbol_t,name)+len+1+7)&-8);
    // TODO: if sym == NULL
    CHECK_ALIGN8(sym);
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

value_t symbol(fl_context_t *fl_ctx, const char *str)
{
    symbol_t **pnode = symtab_lookup(&fl_ctx->symtab, str);
    if (*pnode == NULL)
        *pnode = mk_symbol(str);
    return tagptr(*pnode, TAG_SYM);
}

value_t fl_gensym(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
#ifdef MEMDEBUG2
    fl_ctx->gsnameno = 1-fl_ctx->gsnameno;
    char *n = uint2str(fl_ctx->gsname[fl_ctx->gsnameno]+1, sizeof(fl_ctx->gsname[0])-1, fl_ctx->gensym_ctr++, 10);
    *(--n) = 'g';
    return tagptr(mk_symbol(n), TAG_SYM);
#else
    argcount(fl_ctx, "gensym", nargs, 0);
    (void)args;
    gensym_t *gs = (gensym_t*)alloc_words(fl_ctx, sizeof(gensym_t)/sizeof(void*));
    gs->id = fl_ctx->gensym_ctr++;
    gs->binding = UNBOUND;
    gs->isconst = 0;
    gs->type = NULL;
    return tagptr(gs, TAG_SYM);
#endif
}

int fl_isgensym(fl_context_t *fl_ctx, value_t v)
{
    return isgensym(fl_ctx, v);
}

static value_t fl_gensymp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "gensym?", nargs, 1);
    return isgensym(fl_ctx, args[0]) ? fl_ctx->T : fl_ctx->F;
}

char *symbol_name(fl_context_t *fl_ctx, value_t v)
{
#ifndef MEMDEBUG2
    if (ismanaged(fl_ctx, v)) {
        gensym_t *gs = (gensym_t*)ptr(v);
        fl_ctx->gsnameno = 1-fl_ctx->gsnameno;
        char *n = uint2str(fl_ctx->gsname[fl_ctx->gsnameno]+1, sizeof(fl_ctx->gsname[0])-1, gs->id, 10);
        *(--n) = 'g';
        return n;
    }
#else
    (void)fl_ctx;
#endif
    return ((symbol_t*)ptr(v))->name;
}

// conses ---------------------------------------------------------------------

#ifdef MEMDEBUG2
#define GC_INTERVAL 100000
#endif

void gc(fl_context_t *fl_ctx, int mustgrow);

static value_t mk_cons(fl_context_t *fl_ctx)
{
    cons_t *c;

#ifdef MEMDEBUG2
    if (fl_ctx->n_allocd > GC_INTERVAL)
        gc(fl_ctx, 0);
    c = (cons_t*)((void**)malloc(3*sizeof(void*)) + 1);
    // TODO: if c == NULL
    CHECK_ALIGN8(c);
    ((void**)c)[-1] = fl_ctx->tochain;
    fl_ctx->tochain = c;
    fl_ctx->n_allocd += sizeof(cons_t);
#else
    if (__unlikely(fl_ctx->curheap > fl_ctx->lim))
        gc(fl_ctx, 0);
    c = (cons_t*)fl_ctx->curheap;
    fl_ctx->curheap += sizeof(cons_t);
#endif
    return tagptr(c, TAG_CONS);
}

static value_t *alloc_words(fl_context_t *fl_ctx, int n)
{
    value_t *first;

    assert(n > 0);
    n = LLT_ALIGN(n, 2);   // only allocate multiples of 2 words
#ifdef MEMDEBUG2
    if (fl_ctx->n_allocd > GC_INTERVAL)
        gc(fl_ctx, 0);
    first = (value_t*)malloc((n+1)*sizeof(value_t)) + 1;
    // TODO: if first == NULL
    CHECK_ALIGN8(first);
    first[-1] = (value_t)fl_ctx->tochain;
    fl_ctx->tochain = first;
    fl_ctx->n_allocd += (n*sizeof(value_t));
#else
    if (__unlikely((value_t*)fl_ctx->curheap > ((value_t*)fl_ctx->lim)+2-n)) {
        gc(fl_ctx, 0);
        while ((value_t*)fl_ctx->curheap > ((value_t*)fl_ctx->lim)+2-n) {
            gc(fl_ctx, 1);
        }
    }
    first = (value_t*)fl_ctx->curheap;
    fl_ctx->curheap += (n*sizeof(value_t));
#endif
    return first;
}

// allocate n consecutive conses
#ifndef MEMDEBUG2
#define cons_reserve(fl_ctx, n) tagptr(alloc_words(fl_ctx, (n)*2), TAG_CONS)
#endif

#ifndef MEMDEBUG2
#define cons_index(fl_ctx, c)  (((cons_t*)ptr(c))-((cons_t*)fl_ctx->fromspace))
#endif

#ifdef MEMDEBUG2
#define ismarked(fl_ctx, c)    ((((value_t*)ptr(c))[-1]&1) != 0)
#define mark_cons(fl_ctx, c)   ((((value_t*)ptr(c))[-1]) |= 1)
#define unmark_cons(fl_ctx, c) ((((value_t*)ptr(c))[-1]) &= (~(value_t)1))
#else
#define ismarked(fl_ctx, c)    bitvector_get(fl_ctx->consflags, cons_index(fl_ctx, c))
#define mark_cons(fl_ctx, c)   bitvector_set(fl_ctx->consflags, cons_index(fl_ctx, c), 1)
#define unmark_cons(fl_ctx, c) bitvector_set(fl_ctx->consflags, cons_index(fl_ctx, c), 0)
#endif

value_t alloc_vector(fl_context_t *fl_ctx, size_t n, int init)
{
    if (n == 0) return fl_ctx->the_empty_vector;
    value_t *c = alloc_words(fl_ctx, n+1);
    value_t v = tagptr(c, TAG_VECTOR);
    vector_setsize(v, n);
    if (init) {
        unsigned int i;
        for(i=0; i < n; i++)
            vector_elt(v, i) = FL_UNSPECIFIED(fl_ctx);
    }
    return v;
}

// cvalues --------------------------------------------------------------------

#include "cvalues.c"
#include "types.c"

// print ----------------------------------------------------------------------

static int isnumtok(fl_context_t *fl_ctx, char *tok, value_t *pval);
static inline int symchar(char c);

#include "print.c"

// collector ------------------------------------------------------------------

void fl_gc_handle(fl_context_t *fl_ctx, value_t *pv)
{
    if (fl_ctx->N_GCHND >= FL_N_GC_HANDLES)
        lerror(fl_ctx, fl_ctx->OutOfMemoryError, "out of gc handles");
    fl_ctx->GCHandleStack[fl_ctx->N_GCHND++] = pv;
}

void fl_free_gc_handles(fl_context_t *fl_ctx, uint32_t n)
{
    assert(fl_ctx->N_GCHND >= n);
    fl_ctx->N_GCHND -= n;
}

value_t relocate_lispvalue(fl_context_t *fl_ctx, value_t v)
{
    return relocate(fl_ctx, v);
}

static void trace_globals(fl_context_t *fl_ctx, symbol_t *root)
{
    while (root != NULL) {
        if (root->binding != UNBOUND)
            root->binding = relocate(fl_ctx, root->binding);
        trace_globals(fl_ctx, root->left);
        root = root->right;
    }
}

static value_t relocate(fl_context_t *fl_ctx, value_t v)
{
    value_t a, d, nc, first, *pcdr;
    uintptr_t t = tag(v);

    if (t == TAG_CONS) {
        // iterative implementation allows arbitrarily long cons chains
        pcdr = &first;
        do {
            if ((a=car_(v)) == TAG_FWD) {
                *pcdr = cdr_(v);
                return first;
            }
#ifdef MEMDEBUG2
            *pcdr = nc = mk_cons(fl_ctx);
#else
            *pcdr = nc = tagptr((cons_t*)fl_ctx->curheap, TAG_CONS);
            fl_ctx->curheap += sizeof(cons_t);
#endif
            d = cdr_(v);
            car_(v) = TAG_FWD; cdr_(v) = nc;
            if ((tag(a)&3) == 0 || !ismanaged(fl_ctx, a))
                car_(nc) = a;
            else
                car_(nc) = relocate(fl_ctx, a);
            pcdr = &cdr_(nc);
            v = d;
        } while (iscons(v));
        *pcdr = (d==fl_ctx->NIL) ? fl_ctx->NIL : relocate(fl_ctx, d);
        return first;
    }

    if ((t&3) == 0 || !ismanaged(fl_ctx, v)) return v;
    if (isforwarded(v)) return forwardloc(v);

    if (t == TAG_VECTOR) {
        // N.B.: 0-length vectors secretly have space for a first element
        size_t i, sz = vector_size(v);
        if (vector_elt(v,-1) & 0x1) {
            // grown vector
            nc = relocate(fl_ctx, vector_elt(v,0));
            forward(v, nc);
        }
        else {
            nc = tagptr(alloc_words(fl_ctx, sz+1), TAG_VECTOR);
            vector_setsize(nc, sz);
            a = vector_elt(v,0);
            forward(v, nc);
            if (sz > 0) {
                vector_elt(nc,0) = relocate(fl_ctx, a);
                for(i=1; i < sz; i++) {
                    a = vector_elt(v,i);
                    if ((tag(a)&3) == 0 || !ismanaged(fl_ctx, a))
                        vector_elt(nc,i) = a;
                    else
                        vector_elt(nc,i) = relocate(fl_ctx, a);
                }
            }
        }
        return nc;
    }
    else if (t == TAG_CPRIM) {
        cprim_t *pcp = (cprim_t*)ptr(v);
        size_t nw = CPRIM_NWORDS-1+NWORDS(cp_class(pcp)->size);
        cprim_t *ncp = (cprim_t*)alloc_words(fl_ctx, nw);
        while (nw--)
            ((value_t*)ncp)[nw] = ((value_t*)pcp)[nw];
        nc = tagptr(ncp, TAG_CPRIM);
        forward(v, nc);
        return nc;
    }
    else if (t == TAG_CVALUE) {
        return cvalue_relocate(fl_ctx, v);
    }
    else if (t == TAG_FUNCTION) {
        function_t *fn = (function_t*)ptr(v);
        function_t *nfn = (function_t*)alloc_words(fl_ctx, 4);
        nfn->bcode = fn->bcode;
        nfn->vals = fn->vals;
        nc = tagptr(nfn, TAG_FUNCTION);
        forward(v, nc);
        nfn->env = relocate(fl_ctx, fn->env);
        nfn->vals = relocate(fl_ctx, nfn->vals);
        nfn->bcode = relocate(fl_ctx, nfn->bcode);
        nfn->name = fn->name;
        return nc;
    }
    else if (t == TAG_SYM) {
        gensym_t *gs = (gensym_t*)ptr(v);
        gensym_t *ng = (gensym_t*)alloc_words(fl_ctx, sizeof(gensym_t)/sizeof(void*));
        ng->id = gs->id;
        ng->binding = gs->binding;
        ng->isconst = 0;
        nc = tagptr(ng, TAG_SYM);
        forward(v, nc);
        if (ng->binding != UNBOUND)
            ng->binding = relocate(fl_ctx, ng->binding);
        return nc;
    }
    return v;
}

void gc(fl_context_t *fl_ctx, int mustgrow)
{
    void *temp;
    uint32_t i, f, top;
    fl_readstate_t *rs;
#ifdef MEMDEBUG2
    temp = fl_ctx->tochain;
    fl_ctx->tochain = NULL;
    fl_ctx->n_allocd = -100000000000LL;
#else
    size_t hsz = fl_ctx->gc_grew ? fl_ctx->heapsize*2 : fl_ctx->heapsize;
#ifdef MEMDEBUG
    fl_ctx->tospace = LLT_ALLOC(hsz);
#endif
    fl_ctx->curheap = fl_ctx->tospace;
    fl_ctx->lim = fl_ctx->curheap + hsz - sizeof(cons_t);
#endif

    if (fl_ctx->throwing_frame > fl_ctx->curr_frame) {
        top = fl_ctx->throwing_frame - 3;
        f = fl_ctx->Stack[fl_ctx->throwing_frame-3];
    }
    else {
        top = fl_ctx->SP;
        f = fl_ctx->curr_frame;
    }
    while (1) {
        for (i=f; i < top; i++)
            fl_ctx->Stack[i] = relocate(fl_ctx, fl_ctx->Stack[i]);
        if (f == 0) break;
        top = f - 3;
        f = fl_ctx->Stack[f-3];
    }
    for (i=0; i < fl_ctx->N_GCHND; i++)
        *fl_ctx->GCHandleStack[i] = relocate(fl_ctx, *fl_ctx->GCHandleStack[i]);
    trace_globals(fl_ctx, fl_ctx->symtab);
    relocate_typetable(fl_ctx);
    rs = fl_ctx->readstate;
    while (rs) {
        for(i=0; i < rs->backrefs.size; i++)
            rs->backrefs.table[i] = (void*)relocate(fl_ctx, (value_t)rs->backrefs.table[i]);
        for(i=0; i < rs->gensyms.size; i++)
            rs->gensyms.table[i] = (void*)relocate(fl_ctx, (value_t)rs->gensyms.table[i]);
        rs->source = relocate(fl_ctx, rs->source);
        rs = rs->prev;
    }
    fl_ctx->lasterror = relocate(fl_ctx, fl_ctx->lasterror);
    fl_ctx->memory_exception_value = relocate(fl_ctx, fl_ctx->memory_exception_value);
    fl_ctx->the_empty_vector = relocate(fl_ctx, fl_ctx->the_empty_vector);

    sweep_finalizers(fl_ctx);

#ifdef MEMDEBUG2
    while (temp != NULL) {
        void *next = ((void**)temp)[-1];
        free(&((void**)temp)[-1]);
        temp = next;
    }
    fl_ctx->n_allocd = 0;
#else
#ifdef VERBOSEGC
    printf("GC: found %d/%d live conses\n",
           (fl_ctx->curheap-fl_ctx->tospace)/sizeof(cons_t), fl_ctx->heapsize/sizeof(cons_t));
#endif

    temp = fl_ctx->tospace;
    fl_ctx->tospace = fl_ctx->fromspace;
    fl_ctx->fromspace = (unsigned char*)temp;

    // if we're using > 80% of the space, resize tospace so we have
    // more space to fill next time. if we grew tospace last time,
    // grow the other half of the heap this time to catch up.
    if (fl_ctx->gc_grew || mustgrow
#ifdef MEMDEBUG
        // GC more often
        || ((fl_ctx->lim-fl_ctx->curheap) < (int)(fl_ctx->heapsize/128))
#else
        || ((fl_ctx->lim-fl_ctx->curheap) < (int)(fl_ctx->heapsize/5))
#endif
        ) {
        temp = LLT_REALLOC(fl_ctx->tospace, fl_ctx->heapsize*2);
        if (temp == NULL)
            fl_raise(fl_ctx, fl_ctx->memory_exception_value);
        fl_ctx->tospace = (unsigned char*)temp;
        if (fl_ctx->gc_grew) {
            fl_ctx->heapsize*=2;
            temp = bitvector_resize(fl_ctx->consflags, 0, fl_ctx->heapsize/sizeof(cons_t), 1);
            if (temp == NULL)
                fl_raise(fl_ctx, fl_ctx->memory_exception_value);
            fl_ctx->consflags = (uint32_t*)temp;
        }
        fl_ctx->gc_grew = !fl_ctx->gc_grew;
    }
#ifdef MEMDEBUG
    LLT_FREE(fl_ctx->tospace);
#endif
    if ((value_t*)fl_ctx->curheap > ((value_t*)fl_ctx->lim)-2) {
        // all data was live; gc again and grow heap.
        // but also always leave at least 4 words available, so a closure
        // can be allocated without an extra check.
        gc(fl_ctx, 0);
    }
#endif
}

static void grow_stack(fl_context_t *fl_ctx)
{
    size_t newsz = fl_ctx->N_STACK + (fl_ctx->N_STACK>>1);
    value_t *ns = (value_t*)realloc(fl_ctx->Stack, newsz*sizeof(value_t));
    if (ns == NULL)
        lerror(fl_ctx, fl_ctx->OutOfMemoryError, "stack overflow");
    fl_ctx->Stack = ns;
    fl_ctx->N_STACK = newsz;
}

// utils ----------------------------------------------------------------------

// apply function with n args on the stack
static value_t _applyn(fl_context_t *fl_ctx, uint32_t n)
{
    value_t f = fl_ctx->Stack[fl_ctx->SP-n-1];
    uint32_t saveSP = fl_ctx->SP;
    value_t v;
    if (iscbuiltin(fl_ctx, f)) {
        v = ((builtin_t*)ptr(f))[3](fl_ctx, &fl_ctx->Stack[fl_ctx->SP-n], n);
    }
    else if (isfunction(f)) {
        v = apply_cl(fl_ctx, n);
    }
    else if (isbuiltin(f)) {
        value_t tab = symbol_value(fl_ctx->builtins_table_sym);
        fl_ctx->Stack[fl_ctx->SP-n-1] = vector_elt(tab, uintval(f));
        v = apply_cl(fl_ctx, n);
    }
    else {
        type_error(fl_ctx, "apply", "function", f);
    }
    fl_ctx->SP = saveSP;
    return v;
}

value_t fl_apply(fl_context_t *fl_ctx, value_t f, value_t l)
{
    value_t v = l;
    uint32_t n = fl_ctx->SP;

    PUSH(fl_ctx, f);
    while (iscons(v)) {
        if (fl_ctx->SP >= fl_ctx->N_STACK)
            grow_stack(fl_ctx);
        PUSH(fl_ctx, car_(v));
        v = cdr_(v);
    }
    n = fl_ctx->SP - n - 1;
    v = _applyn(fl_ctx, n);
    POPN(fl_ctx, n+1);
    return v;
}

value_t fl_applyn(fl_context_t *fl_ctx, uint32_t n, value_t f, ...)
{
    va_list ap;
    va_start(ap, f);
    size_t i;

    PUSH(fl_ctx, f);
    while (fl_ctx->SP+n > fl_ctx->N_STACK)
        grow_stack(fl_ctx);
    for(i=0; i < n; i++) {
        value_t a = va_arg(ap, value_t);
        PUSH(fl_ctx, a);
    }
    value_t v = _applyn(fl_ctx, n);
    POPN(fl_ctx, n+1);
    va_end(ap);
    return v;
}

value_t fl_listn(fl_context_t *fl_ctx, size_t n, ...)
{
    va_list ap;
    va_start(ap, n);
    uint32_t si = fl_ctx->SP;
    size_t i;

    while (fl_ctx->SP+n > fl_ctx->N_STACK)
        grow_stack(fl_ctx);
    for(i=0; i < n; i++) {
        value_t a = va_arg(ap, value_t);
        PUSH(fl_ctx, a);
    }
#ifdef MEMDEBUG2
    si = fl_ctx->SP-1;
    value_t l = fl_ctx->NIL;
    for(i=0; i < n; i++) {
        l = fl_cons(fl_ctx, fl_ctx->Stack[si--], l);
    }
    POPN(fl_ctx, n);
    va_end(ap);
    return l;
#else
    cons_t *c = (cons_t*)alloc_words(fl_ctx, n*2);
    cons_t *l = c;
    for(i=0; i < n; i++) {
        c->car = fl_ctx->Stack[si++];
        c->cdr = tagptr(c+1, TAG_CONS);
        c++;
    }
    (c-1)->cdr = fl_ctx->NIL;
    POPN(fl_ctx, n);
    va_end(ap);
    return tagptr(l, TAG_CONS);
#endif
}

value_t fl_list2(fl_context_t *fl_ctx, value_t a, value_t b)
{
    PUSH(fl_ctx, a);
    PUSH(fl_ctx, b);
#ifdef MEMDEBUG2
    fl_ctx->Stack[fl_ctx->SP-1] = fl_cons(fl_ctx, b, fl_ctx->NIL);
    a = fl_cons(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2], fl_ctx->Stack[fl_ctx->SP-1]);
    POPN(fl_ctx, 2);
    return a;
#else
    cons_t *c = (cons_t*)alloc_words(fl_ctx, 4);
    b = POP(fl_ctx);
    a = POP(fl_ctx);
    c[0].car = a;
    c[0].cdr = tagptr(c+1, TAG_CONS);
    c[1].car = b;
    c[1].cdr = fl_ctx->NIL;
    return tagptr(c, TAG_CONS);
#endif
}

value_t fl_cons(fl_context_t *fl_ctx, value_t a, value_t b)
{
    PUSH(fl_ctx, a);
    PUSH(fl_ctx, b);
    value_t c = mk_cons(fl_ctx);
    cdr_(c) = POP(fl_ctx);
    car_(c) = POP(fl_ctx);
    return c;
}

int fl_isnumber(fl_context_t *fl_ctx, value_t v)
{
    if (isfixnum(v)) return 1;
    if (iscprim(v)) {
        cprim_t *c = (cprim_t*)ptr(v);
        return c->type != fl_ctx->wchartype;
    }
    return 0;
}

// read -----------------------------------------------------------------------

#include "read.c"

// equal ----------------------------------------------------------------------

#include "equal.c"

// eval -----------------------------------------------------------------------

#define list(fl_ctx, a,n) _list(fl_ctx, (a), (n), 0)

static value_t _list(fl_context_t *fl_ctx, value_t *args, uint32_t nargs, int star)
{
    cons_t *c;
    int i;
    value_t v;
#ifdef MEMDEBUG2
    value_t n;
    i = nargs-1;
    if (star) {
        n = mk_cons(fl_ctx);
        c = (cons_t*)ptr(n);
        c->car = args[i-1];
        c->cdr = args[i];
        i -= 2;
        v = n;
    }
    else {
        v = fl_ctx->NIL;
    }
    PUSH(fl_ctx, v);
    for(; i >= 0; i--) {
        n = mk_cons(fl_ctx);
        c = (cons_t*)ptr(n);
        c->car = args[i];
        c->cdr = fl_ctx->Stack[fl_ctx->SP-1];
        fl_ctx->Stack[fl_ctx->SP-1] = n;
    }
    v = POP(fl_ctx);
#else
    v = cons_reserve(fl_ctx, nargs);
    c = (cons_t*)ptr(v);
    for(i=0; i < nargs; i++) {
        c->car = args[i];
        c->cdr = tagptr(c+1, TAG_CONS);
        c++;
    }
    if (star)
        (c-2)->cdr = (c-1)->car;
    else
        (c-1)->cdr = fl_ctx->NIL;
#endif
    return v;
}

static value_t copy_list(fl_context_t *fl_ctx, value_t L)
{
    if (!iscons(L))
        return fl_ctx->NIL;
    PUSH(fl_ctx, fl_ctx->NIL);
    PUSH(fl_ctx, L);
    value_t *plcons = &fl_ctx->Stack[fl_ctx->SP-2];
    value_t *pL = &fl_ctx->Stack[fl_ctx->SP-1];
    value_t c;
    c = mk_cons(fl_ctx); PUSH(fl_ctx, c);  // save first cons
    car_(c) = car_(*pL);
    cdr_(c) = fl_ctx->NIL;
    *plcons = c;
    *pL = cdr_(*pL);
    while (iscons(*pL)) {
        c = mk_cons(fl_ctx);
        car_(c) = car_(*pL);
        cdr_(c) = fl_ctx->NIL;
        cdr_(*plcons) = c;
        *plcons = c;
        *pL = cdr_(*pL);
    }
    c = POP(fl_ctx);  // first cons
    POPN(fl_ctx, 2);
    return c;
}

static value_t do_trycatch(fl_context_t *fl_ctx)
{
    uint32_t saveSP = fl_ctx->SP;
    value_t v;
    value_t thunk = fl_ctx->Stack[fl_ctx->SP-2];
    fl_ctx->Stack[fl_ctx->SP-2] = fl_ctx->Stack[fl_ctx->SP-1];
    fl_ctx->Stack[fl_ctx->SP-1] = thunk;

    FL_TRY(fl_ctx) {
        v = apply_cl(fl_ctx, 0);
    }
    FL_CATCH(fl_ctx) {
        v = fl_ctx->Stack[saveSP-2];
        PUSH(fl_ctx, v);
        PUSH(fl_ctx, fl_ctx->lasterror);
        v = apply_cl(fl_ctx, 1);
    }
    fl_ctx->SP = saveSP;
    return v;
}

/*
  argument layout on stack is
  |--required args--|--opt args--|--kw args--|--rest args...
*/
static uint32_t process_keys(fl_context_t *fl_ctx, value_t kwtable,
                             uint32_t nreq, uint32_t nkw, uint32_t nopt,
                             uint32_t bp, uint32_t nargs, int va)
{
    uintptr_t n;
    uint32_t extr = nopt+nkw;
    uint32_t ntot = nreq+extr;
    value_t *args = (value_t*)alloca(extr*sizeof(value_t));
    value_t v;
    uint32_t i, a = 0, nrestargs;
    value_t s1 = fl_ctx->Stack[fl_ctx->SP-1];
    value_t s3 = fl_ctx->Stack[fl_ctx->SP-3];
    value_t s4 = fl_ctx->Stack[fl_ctx->SP-4];
    if (nargs < nreq)
        lerror(fl_ctx, fl_ctx->ArgError, "apply: too few arguments");
    for (i=0; i < extr; i++) args[i] = UNBOUND;
    for (i=nreq; i < nargs; i++) {
        v = fl_ctx->Stack[bp+i];
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
            lerrorf(fl_ctx, fl_ctx->ArgError, "keyword %s requires an argument",
                    symbol_name(fl_ctx, v));
        value_t hv = fixnum(((symbol_t*)ptr(v))->hash);
        uintptr_t x = 2*(labs(numval(hv)) % n);
        if (vector_elt(kwtable, x) == v) {
            uintptr_t idx = numval(vector_elt(kwtable, x+1));
            assert(idx < nkw);
            idx += nopt;
            if (args[idx] == UNBOUND) {
                // if duplicate key, keep first value
                args[idx] = fl_ctx->Stack[bp+i];
            }
        }
        else {
            lerrorf(fl_ctx, fl_ctx->ArgError, "unsupported keyword %s", symbol_name(fl_ctx, v));
        }
        i++;
        if (i >= nargs) break;
        v = fl_ctx->Stack[bp+i];
    } while (issymbol(v) && iskeyword((symbol_t*)ptr(v)));
 no_kw:
    nrestargs = nargs - i;
    if (!va && nrestargs > 0)
        lerror(fl_ctx, fl_ctx->ArgError, "apply: too many arguments");
    nargs = ntot + nrestargs;
    if (nrestargs)
        memmove(&fl_ctx->Stack[bp+ntot], &fl_ctx->Stack[bp+i], nrestargs*sizeof(value_t));
    memcpy(&fl_ctx->Stack[bp+nreq], args, extr*sizeof(value_t));
    fl_ctx->SP = bp + nargs;
    assert(fl_ctx->SP < fl_ctx->N_STACK-4);
    PUSH(fl_ctx, s4);
    PUSH(fl_ctx, s3);
    PUSH(fl_ctx, nargs);
    PUSH(fl_ctx, s1);
    fl_ctx->curr_frame = fl_ctx->SP;
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
#define PUT_INT32(a,i) jl_store_unaligned_i32((void*)a,
    (uint32_t)bswap_32((int32_t)(i)))
#else
#define GET_INT32(a) (int32_t)jl_load_unaligned_i32((void*)a)
#define GET_INT16(a) (int16_t)jl_load_unaligned_i16((void*)a)
#define PUT_INT32(a,i) jl_store_unaligned_i32((void*)a, (uint32_t)(i))
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
  - restore fl_ctx->SP

  callee's responsibility:
  - check arg counts
  - allocate vararg array
  - push closed env, set up new environment
*/
JL_EXTENSION static value_t apply_cl(fl_context_t *fl_ctx, uint32_t nargs)
{
    VM_LABELS;
    VM_APPLY_LABELS;
    uint32_t top_frame = fl_ctx->curr_frame;
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
#define fl_apply_c fl_ctx->apply_c
#define fl_apply_pv fl_ctx->apply_pv
#define fl_apply_accum fl_ctx->apply_accum
#define fl_apply_func fl_ctx->apply_func
#define fl_apply_v fl_ctx->apply_v
#define fl_apply_e fl_ctx->apply_e

 apply_cl_top:
    fl_apply_func = fl_ctx->Stack[fl_ctx->SP-nargs-1];
    ip = (uint8_t*)cv_data((cvalue_t*)ptr(fn_bcode(fl_apply_func)));
#ifndef MEMDEBUG2
    assert(!ismanaged(fl_ctx, (uintptr_t)ip));
#endif
    while (fl_ctx->SP+GET_INT32(ip) > fl_ctx->N_STACK) {
        grow_stack(fl_ctx);
    }
    ip += 4;

    bp = fl_ctx->SP-nargs;
    PUSH(fl_ctx, fn_env(fl_apply_func));
    PUSH(fl_ctx, fl_ctx->curr_frame);
    PUSH(fl_ctx, nargs);
    fl_ctx->SP++;//PUSH(fl_ctx, 0); //ip
    fl_ctx->curr_frame = fl_ctx->SP;

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
                    lerror(fl_ctx, fl_ctx->ArgError, "apply: too many arguments");
                else
                    lerror(fl_ctx, fl_ctx->ArgError, "apply: too few arguments");
            }
            NEXT_OP;
        OP(OP_VARGC)
            i = *ip++;
        do_vargc:
            s = (fixnum_t)nargs - (fixnum_t)i;
            if (s > 0) {
                fl_apply_v = list(fl_ctx, &fl_ctx->Stack[bp+i], s);
                fl_ctx->Stack[bp+i] = fl_apply_v;
                if (s > 1) {
                    fl_ctx->Stack[bp+i+1] = fl_ctx->Stack[bp+nargs+0];
                    fl_ctx->Stack[bp+i+2] = fl_ctx->Stack[bp+nargs+1];
                    fl_ctx->Stack[bp+i+3] = i+1;
                    fl_ctx->Stack[bp+i+4] = 0;
                    fl_ctx->SP =  bp+i+5;
                    fl_ctx->curr_frame = fl_ctx->SP;
                }
            }
            else if (s < 0) {
                lerror(fl_ctx, fl_ctx->ArgError, "apply: too few arguments");
            }
            else {
                fl_ctx->SP++;
                fl_ctx->Stack[fl_ctx->SP-2] = i+1;
                fl_ctx->Stack[fl_ctx->SP-3] = fl_ctx->Stack[fl_ctx->SP-4];
                fl_ctx->Stack[fl_ctx->SP-4] = fl_ctx->Stack[fl_ctx->SP-5];
                fl_ctx->Stack[fl_ctx->SP-5] = fl_ctx->NIL;
                fl_ctx->curr_frame = fl_ctx->SP;
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
            fl_apply_v = fl_ctx->Stack[bp+i];
            if (fl_apply_v != UNBOUND) PUSH(fl_ctx, fl_ctx->T);
            else PUSH(fl_ctx, fl_ctx->F);
            NEXT_OP;
        OP(OP_DUP) fl_ctx->SP++; fl_ctx->Stack[fl_ctx->SP-1] = fl_ctx->Stack[fl_ctx->SP-2]; NEXT_OP;
        OP(OP_POP) POPN(fl_ctx, 1); NEXT_OP;
        OP(OP_TCALL)
            n = *ip++;  // nargs
        do_tcall:
            fl_apply_func = fl_ctx->Stack[fl_ctx->SP-n-1];
            if (tag(fl_apply_func) == TAG_FUNCTION) {
                if (fl_apply_func > (N_BUILTINS<<3)) {
                    fl_ctx->curr_frame = fl_ctx->Stack[fl_ctx->curr_frame-3];
                    for(s=-1; s < (fixnum_t)n; s++)
                        fl_ctx->Stack[bp+s] = fl_ctx->Stack[fl_ctx->SP-n+s];
                    fl_ctx->SP = bp+n;
                    nargs = n;
                    goto apply_cl_top;
                }
                else {
                    i = uintval(fl_apply_func);
                    if (i <= OP_ASET) {
                        s = builtin_arg_counts[i];
                        if (s >= 0)
                            argcount(fl_ctx, builtin_names[i], n, s);
                        else if (s != ANYARGS && (signed)n < -s)
                            argcount(fl_ctx, builtin_names[i], n, -s);
                        // remove function arg
                        for(s=fl_ctx->SP-n-1; s < (int)fl_ctx->SP-1; s++)
                            fl_ctx->Stack[s] = fl_ctx->Stack[s+1];
                        fl_ctx->SP--;
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
            else if (iscbuiltin(fl_ctx, fl_apply_func)) {
                s = fl_ctx->SP;
                fl_apply_v = ((builtin_t)(uintptr_t)(((void**)ptr(fl_apply_func))[3]))(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-n], n);
                fl_ctx->SP = s-n;
                fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
                NEXT_OP;
            }
            type_error(fl_ctx, "apply", "function", fl_apply_func);
        // WARNING: repeated code ahead
        OP(OP_CALL)
            n = *ip++;  // nargs
        do_call:
            fl_apply_func = fl_ctx->Stack[fl_ctx->SP-n-1];
            if (tag(fl_apply_func) == TAG_FUNCTION) {
                if (fl_apply_func > (N_BUILTINS<<3)) {
                    fl_ctx->Stack[fl_ctx->curr_frame-1] = (uintptr_t)ip;
                    nargs = n;
                    goto apply_cl_top;
                }
                else {
                    i = uintval(fl_apply_func);
                    if (i <= OP_ASET) {
                        s = builtin_arg_counts[i];
                        if (s >= 0)
                            argcount(fl_ctx, builtin_names[i], n, s);
                        else if (s != ANYARGS && (signed)n < -s)
                            argcount(fl_ctx, builtin_names[i], n, -s);
                        // remove function arg
                        for(s=fl_ctx->SP-n-1; s < (int)fl_ctx->SP-1; s++)
                            fl_ctx->Stack[s] = fl_ctx->Stack[s+1];
                        fl_ctx->SP--;
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
            else if (iscbuiltin(fl_ctx, fl_apply_func)) {
                s = fl_ctx->SP;
                fl_apply_v = ((builtin_t)(uintptr_t)(((void**)ptr(fl_apply_func))[3]))(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-n], n);
                fl_ctx->SP = s-n;
                fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
                NEXT_OP;
            }
            type_error(fl_ctx, "apply", "function", fl_apply_func);
        OP(OP_TCALLL) n = GET_INT32(ip); ip+=4; goto do_tcall;
        OP(OP_CALLL)  n = GET_INT32(ip); ip+=4; goto do_call;
        OP(OP_JMP) ip += (intptr_t)GET_INT16(ip); NEXT_OP;
        OP(OP_BRF)
            fl_apply_v = POP(fl_ctx);
            if (fl_apply_v == fl_ctx->F) ip += (intptr_t)GET_INT16(ip);
            else ip += 2;
            NEXT_OP;
        OP(OP_BRT)
            fl_apply_v = POP(fl_ctx);
            if (fl_apply_v != fl_ctx->F) ip += (intptr_t)GET_INT16(ip);
            else ip += 2;
            NEXT_OP;
        OP(OP_JMPL) ip += (intptr_t)GET_INT32(ip); NEXT_OP;
        OP(OP_BRFL)
            fl_apply_v = POP(fl_ctx);
            if (fl_apply_v == fl_ctx->F) ip += (intptr_t)GET_INT32(ip);
            else ip += 4;
            NEXT_OP;
        OP(OP_BRTL)
            fl_apply_v = POP(fl_ctx);
            if (fl_apply_v != fl_ctx->F) ip += (intptr_t)GET_INT32(ip);
            else ip += 4;
            NEXT_OP;
        OP(OP_BRNE)
            if (fl_ctx->Stack[fl_ctx->SP-2] != fl_ctx->Stack[fl_ctx->SP-1]) ip += (intptr_t)GET_INT16(ip);
            else ip += 2;
            POPN(fl_ctx, 2);
            NEXT_OP;
        OP(OP_BRNEL)
            if (fl_ctx->Stack[fl_ctx->SP-2] != fl_ctx->Stack[fl_ctx->SP-1]) ip += (intptr_t)GET_INT32(ip);
            else ip += 4;
            POPN(fl_ctx, 2);
            NEXT_OP;
        OP(OP_BRNN)
            fl_apply_v = POP(fl_ctx);
            if (fl_apply_v != fl_ctx->NIL) ip += (intptr_t)GET_INT16(ip);
            else ip += 2;
            NEXT_OP;
        OP(OP_BRNNL)
            fl_apply_v = POP(fl_ctx);
            if (fl_apply_v != fl_ctx->NIL) ip += (intptr_t)GET_INT32(ip);
            else ip += 4;
            NEXT_OP;
        OP(OP_BRN)
            fl_apply_v = POP(fl_ctx);
            if (fl_apply_v == fl_ctx->NIL) ip += (intptr_t)GET_INT16(ip);
            else ip += 2;
            NEXT_OP;
        OP(OP_BRNL)
            fl_apply_v = POP(fl_ctx);
            if (fl_apply_v == fl_ctx->NIL) ip += (intptr_t)GET_INT32(ip);
            else ip += 4;
            NEXT_OP;
        OP(OP_RET)
            fl_apply_v = POP(fl_ctx);
            fl_ctx->SP = fl_ctx->curr_frame;
            fl_ctx->curr_frame = fl_ctx->Stack[fl_ctx->SP-3];
            if (fl_ctx->curr_frame == top_frame) return fl_apply_v;
            fl_ctx->SP -= (4+nargs);
            ip = (uint8_t*)fl_ctx->Stack[fl_ctx->curr_frame-1];
            nargs        = fl_ctx->Stack[fl_ctx->curr_frame-2];
            bp           = fl_ctx->curr_frame - 4 - nargs;
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;

        OP(OP_EQ)
            fl_ctx->Stack[fl_ctx->SP-2] = ((fl_ctx->Stack[fl_ctx->SP-2] == fl_ctx->Stack[fl_ctx->SP-1]) ? fl_ctx->T : fl_ctx->F);
            POPN(fl_ctx, 1); NEXT_OP;
        OP(OP_EQV)
            if (fl_ctx->Stack[fl_ctx->SP-2] == fl_ctx->Stack[fl_ctx->SP-1]) {
                fl_apply_v = fl_ctx->T;
            }
            else if (!leafp(fl_ctx->Stack[fl_ctx->SP-2]) || !leafp(fl_ctx->Stack[fl_ctx->SP-1])) {
                fl_apply_v = fl_ctx->F;
            }
            else {
                fl_apply_v = (compare_(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2], fl_ctx->Stack[fl_ctx->SP-1], 1)==0 ? fl_ctx->T : fl_ctx->F);
            }
            fl_ctx->Stack[fl_ctx->SP-2] = fl_apply_v; POPN(fl_ctx, 1);
            NEXT_OP;
        OP(OP_EQUAL)
            if (fl_ctx->Stack[fl_ctx->SP-2] == fl_ctx->Stack[fl_ctx->SP-1]) {
                fl_apply_v = fl_ctx->T;
            }
            else {
                fl_apply_v = (compare_(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2], fl_ctx->Stack[fl_ctx->SP-1], 1)==0 ? fl_ctx->T : fl_ctx->F);
            }
            fl_ctx->Stack[fl_ctx->SP-2] = fl_apply_v; POPN(fl_ctx, 1);
            NEXT_OP;
        OP(OP_PAIRP)
            fl_ctx->Stack[fl_ctx->SP-1] = (iscons(fl_ctx->Stack[fl_ctx->SP-1]) ? fl_ctx->T : fl_ctx->F); NEXT_OP;
        OP(OP_ATOMP)
            fl_ctx->Stack[fl_ctx->SP-1] = (iscons(fl_ctx->Stack[fl_ctx->SP-1]) ? fl_ctx->F : fl_ctx->T); NEXT_OP;
        OP(OP_NOT)
            fl_ctx->Stack[fl_ctx->SP-1] = ((fl_ctx->Stack[fl_ctx->SP-1]==fl_ctx->F) ? fl_ctx->T : fl_ctx->F); NEXT_OP;
        OP(OP_NULLP)
            fl_ctx->Stack[fl_ctx->SP-1] = ((fl_ctx->Stack[fl_ctx->SP-1]==fl_ctx->NIL) ? fl_ctx->T : fl_ctx->F); NEXT_OP;
        OP(OP_BOOLEANP)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            fl_ctx->Stack[fl_ctx->SP-1] = ((fl_apply_v == fl_ctx->T || fl_apply_v == fl_ctx->F) ? fl_ctx->T:fl_ctx->F); NEXT_OP;
        OP(OP_SYMBOLP)
            fl_ctx->Stack[fl_ctx->SP-1] = (issymbol(fl_ctx->Stack[fl_ctx->SP-1]) ? fl_ctx->T : fl_ctx->F); NEXT_OP;
        OP(OP_NUMBERP)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            fl_ctx->Stack[fl_ctx->SP-1] = (fl_isnumber(fl_ctx, fl_apply_v) ? fl_ctx->T:fl_ctx->F); NEXT_OP;
        OP(OP_FIXNUMP)
            fl_ctx->Stack[fl_ctx->SP-1] = (isfixnum(fl_ctx->Stack[fl_ctx->SP-1]) ? fl_ctx->T : fl_ctx->F); NEXT_OP;
        OP(OP_BOUNDP)
            sym = tosymbol(fl_ctx, fl_ctx->Stack[fl_ctx->SP-1], "bound?");
            fl_ctx->Stack[fl_ctx->SP-1] = ((sym->binding == UNBOUND) ? fl_ctx->F : fl_ctx->T);
            NEXT_OP;
        OP(OP_BUILTINP)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
        fl_ctx->Stack[fl_ctx->SP-1] = (isbuiltin(fl_apply_v) || iscbuiltin(fl_ctx, fl_apply_v)) ? fl_ctx->T : fl_ctx->F;
            NEXT_OP;
        OP(OP_FUNCTIONP)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            fl_ctx->Stack[fl_ctx->SP-1] = ((tag(fl_apply_v)==TAG_FUNCTION &&
                            (uintval(fl_apply_v)<=OP_ASET || fl_apply_v>(N_BUILTINS<<3))) ||
                                 iscbuiltin(fl_ctx, fl_apply_v)) ? fl_ctx->T : fl_ctx->F;
            NEXT_OP;
        OP(OP_VECTORP)
            fl_ctx->Stack[fl_ctx->SP-1] = (isvector(fl_ctx->Stack[fl_ctx->SP-1]) ? fl_ctx->T : fl_ctx->F); NEXT_OP;

        OP(OP_CONS)
#ifdef MEMDEBUG2
            fl_apply_c = (cons_t*)ptr(mk_cons(fl_ctx));
#else
            if (fl_ctx->curheap > fl_ctx->lim)
                gc(fl_ctx, 0);
            fl_apply_c = (cons_t*)fl_ctx->curheap;
            fl_ctx->curheap += sizeof(cons_t);
#endif
            fl_apply_c->car = fl_ctx->Stack[fl_ctx->SP-2];
            fl_apply_c->cdr = fl_ctx->Stack[fl_ctx->SP-1];
            fl_ctx->Stack[fl_ctx->SP-2] = tagptr(fl_apply_c, TAG_CONS);
            POPN(fl_ctx, 1); NEXT_OP;
        OP(OP_CAR)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            if (!iscons(fl_apply_v)) type_error(fl_ctx, "car", "cons", fl_apply_v);
            fl_ctx->Stack[fl_ctx->SP-1] = car_(fl_apply_v);
            NEXT_OP;
        OP(OP_CDR)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            if (!iscons(fl_apply_v)) type_error(fl_ctx, "cdr", "cons", fl_apply_v);
            fl_ctx->Stack[fl_ctx->SP-1] = cdr_(fl_apply_v);
            NEXT_OP;
        OP(OP_CADR)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            if (!iscons(fl_apply_v)) type_error(fl_ctx, "cdr", "cons", fl_apply_v);
            fl_apply_v = cdr_(fl_apply_v);
            if (!iscons(fl_apply_v)) type_error(fl_ctx, "car", "cons", fl_apply_v);
            fl_ctx->Stack[fl_ctx->SP-1] = car_(fl_apply_v);
            NEXT_OP;
        OP(OP_SETCAR)
            car(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2]) = fl_ctx->Stack[fl_ctx->SP-1];
            POPN(fl_ctx, 1); NEXT_OP;
        OP(OP_SETCDR)
            cdr(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2]) = fl_ctx->Stack[fl_ctx->SP-1];
            POPN(fl_ctx, 1); NEXT_OP;
        OP(OP_LIST)
            n = *ip++;
        apply_list:
            if (n > 0) {
                fl_apply_v = list(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-n], n);
                POPN(fl_ctx, n);
                PUSH(fl_ctx, fl_apply_v);
            }
            else {
                PUSH(fl_ctx, fl_ctx->NIL);
            }
            NEXT_OP;

        OP(OP_TAPPLY)
            n = *ip++;
        apply_tapply:
            fl_apply_v = POP(fl_ctx);     // arglist
            n = fl_ctx->SP-(n-2);  // n-2 == # leading arguments not in the list
            while (iscons(fl_apply_v)) {
                if (fl_ctx->SP >= fl_ctx->N_STACK)
                    grow_stack(fl_ctx);
                PUSH(fl_ctx, car_(fl_apply_v));
                fl_apply_v = cdr_(fl_apply_v);
            }
            n = fl_ctx->SP-n;
            goto do_tcall;
        OP(OP_APPLY)
            n = *ip++;
        apply_apply:
            fl_apply_v = POP(fl_ctx);     // arglist
            n = fl_ctx->SP-(n-2);  // n-2 == # leading arguments not in the list
            while (iscons(fl_apply_v)) {
                if (fl_ctx->SP >= fl_ctx->N_STACK)
                    grow_stack(fl_ctx);
                PUSH(fl_ctx, car_(fl_apply_v));
                fl_apply_v = cdr_(fl_apply_v);
            }
            n = fl_ctx->SP-n;
            goto do_call;

        OP(OP_ADD)
            n = *ip++;
        apply_add:
            s = 0;
            i = fl_ctx->SP-n;
            for (; i < fl_ctx->SP; i++) {
                if (isfixnum(fl_ctx->Stack[i])) {
                    s += numval(fl_ctx->Stack[i]);
                    if (!fits_fixnum(s)) {
                        i++;
                        goto add_ovf;
                    }
                }
                else {
                add_ovf:
                    fl_apply_v = fl_add_any(fl_ctx, &fl_ctx->Stack[i], fl_ctx->SP-i, s);
                    break;
                }
            }
            if (i==fl_ctx->SP)
                fl_apply_v = fixnum(s);
            POPN(fl_ctx, n);
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_ADD2)
            if (bothfixnums(fl_ctx->Stack[fl_ctx->SP-1], fl_ctx->Stack[fl_ctx->SP-2])) {
                s = numval(fl_ctx->Stack[fl_ctx->SP-1]) + numval(fl_ctx->Stack[fl_ctx->SP-2]);
                if (fits_fixnum(s))
                    fl_apply_v = fixnum(s);
                else
                    fl_apply_v = mk_ptrdiff(fl_ctx, s);
            }
            else {
                fl_apply_v = fl_add_any(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-2], 2, 0);
            }
            POPN(fl_ctx, 1);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;
        OP(OP_SUB)
            n = *ip++;
        apply_sub:
            if (n == 2) goto do_sub2;
            if (n == 1) goto do_neg;
            i = fl_ctx->SP-n;
            // we need to pass the full arglist on to fl_add_any
            // so it can handle rest args properly
            PUSH(fl_ctx, fl_ctx->Stack[i]);
            fl_ctx->Stack[i] = fixnum(0);
            fl_ctx->Stack[i+1] = fl_neg(fl_ctx, fl_add_any(fl_ctx, &fl_ctx->Stack[i], n, 0));
            fl_ctx->Stack[i] = POP(fl_ctx);
            fl_apply_v = fl_add_any(fl_ctx, &fl_ctx->Stack[i], 2, 0);
            POPN(fl_ctx, n);
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_NEG)
        do_neg:
            if (isfixnum(fl_ctx->Stack[fl_ctx->SP-1]))
                fl_ctx->Stack[fl_ctx->SP-1] = fixnum(-numval(fl_ctx->Stack[fl_ctx->SP-1]));
            else
                fl_ctx->Stack[fl_ctx->SP-1] = fl_neg(fl_ctx, fl_ctx->Stack[fl_ctx->SP-1]);
            NEXT_OP;
        OP(OP_SUB2)
        do_sub2:
            if (bothfixnums(fl_ctx->Stack[fl_ctx->SP-2], fl_ctx->Stack[fl_ctx->SP-1])) {
                s = numval(fl_ctx->Stack[fl_ctx->SP-2]) - numval(fl_ctx->Stack[fl_ctx->SP-1]);
                if (fits_fixnum(s))
                    fl_apply_v = fixnum(s);
                else
                    fl_apply_v = mk_ptrdiff(fl_ctx, s);
            }
            else {
                fl_ctx->Stack[fl_ctx->SP-1] = fl_neg(fl_ctx, fl_ctx->Stack[fl_ctx->SP-1]);
                fl_apply_v = fl_add_any(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-2], 2, 0);
            }
            POPN(fl_ctx, 1);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;
        OP(OP_MUL)
            n = *ip++;
        apply_mul:
            fl_apply_accum = 1;
            i = fl_ctx->SP-n;
            for (; i < fl_ctx->SP; i++) {
                if (isfixnum(fl_ctx->Stack[i])) {
                    fl_apply_accum *= numval(fl_ctx->Stack[i]);
                }
                else {
                    fl_apply_v = fl_mul_any(fl_ctx, &fl_ctx->Stack[i], fl_ctx->SP-i, fl_apply_accum);
                    break;
                }
            }
            if (i == fl_ctx->SP) {
                if (fits_fixnum(fl_apply_accum))
                    fl_apply_v = fixnum(fl_apply_accum);
                else
                    fl_apply_v = return_from_int64(fl_ctx, fl_apply_accum);
            }
            POPN(fl_ctx, n);
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_DIV)
            n = *ip++;
        apply_div:
            i = fl_ctx->SP-n;
            if (n == 1) {
                fl_ctx->Stack[fl_ctx->SP-1] = fl_div2(fl_ctx, fixnum(1), fl_ctx->Stack[i]);
            }
            else {
                if (n > 2) {
                    PUSH(fl_ctx, fl_ctx->Stack[i]);
                    fl_ctx->Stack[i] = fixnum(1);
                    fl_ctx->Stack[i+1] = fl_mul_any(fl_ctx, &fl_ctx->Stack[i], n, 1);
                    fl_ctx->Stack[i] = POP(fl_ctx);
                }
                fl_apply_v = fl_div2(fl_ctx, fl_ctx->Stack[i], fl_ctx->Stack[i+1]);
                POPN(fl_ctx, n);
                PUSH(fl_ctx, fl_apply_v);
            }
            NEXT_OP;
        OP(OP_IDIV)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-2]; fl_apply_e = fl_ctx->Stack[fl_ctx->SP-1];
            if (bothfixnums(fl_apply_v, fl_apply_e)) {
                if (fl_apply_e==0) DivideByZeroError(fl_ctx);
                fl_apply_v = fixnum(numval(fl_apply_v) / numval(fl_apply_e));
            }
            else
                fl_apply_v = fl_idiv2(fl_ctx, fl_apply_v, fl_apply_e);
            POPN(fl_ctx, 1);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;
        OP(OP_NUMEQ)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-2]; fl_apply_e = fl_ctx->Stack[fl_ctx->SP-1];
            if (bothfixnums(fl_apply_v, fl_apply_e))
                fl_apply_v = (fl_apply_v == fl_apply_e) ? fl_ctx->T : fl_ctx->F;
            else
                fl_apply_v = (!numeric_compare(fl_ctx,fl_apply_v,fl_apply_e,1,0,"=")) ? fl_ctx->T : fl_ctx->F;
            POPN(fl_ctx, 1);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;
        OP(OP_LT)
            if (bothfixnums(fl_ctx->Stack[fl_ctx->SP-2], fl_ctx->Stack[fl_ctx->SP-1])) {
                fl_apply_v = (numval(fl_ctx->Stack[fl_ctx->SP-2]) < numval(fl_ctx->Stack[fl_ctx->SP-1])) ? fl_ctx->T : fl_ctx->F;
            }
            else {
                fl_apply_v = (numval(fl_compare(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2], fl_ctx->Stack[fl_ctx->SP-1])) < 0) ?
                    fl_ctx->T : fl_ctx->F;
            }
            POPN(fl_ctx, 1);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;
        OP(OP_COMPARE)
            fl_ctx->Stack[fl_ctx->SP-2] = compare_(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2], fl_ctx->Stack[fl_ctx->SP-1], 0);
            POPN(fl_ctx, 1);
            NEXT_OP;

        OP(OP_VECTOR)
            n = *ip++;
        apply_vector:
            fl_apply_v = alloc_vector(fl_ctx, n, 0);
            if (n) {
                memcpy(&vector_elt(fl_apply_v,0), &fl_ctx->Stack[fl_ctx->SP-n], n*sizeof(value_t));
                POPN(fl_ctx, n);
            }
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;

        OP(OP_AREF)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-2];
            if (isvector(fl_apply_v)) {
                fl_apply_e = fl_ctx->Stack[fl_ctx->SP-1];
                if (isfixnum(fl_apply_e))
                    i = numval(fl_apply_e);
                else
                    i = (uint32_t)tosize(fl_ctx, fl_apply_e, "aref");
                if ((unsigned)i >= vector_size(fl_apply_v))
                    bounds_error(fl_ctx, "aref", fl_apply_v, fl_apply_e);
                fl_apply_v = vector_elt(fl_apply_v, i);
            }
            else if (isarray(fl_apply_v)) {
                fl_apply_v = cvalue_array_aref(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-2]);
            }
            else {
                type_error(fl_ctx, "aref", "sequence", fl_apply_v);
            }
            POPN(fl_ctx, 1);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;
        OP(OP_ASET)
            fl_apply_e = fl_ctx->Stack[fl_ctx->SP-3];
            if (isvector(fl_apply_e)) {
                i = tofixnum(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2], "aset!");
                if ((unsigned)i >= vector_size(fl_apply_e))
                    bounds_error(fl_ctx, "aset!", fl_apply_v, fl_ctx->Stack[fl_ctx->SP-1]);
                vector_elt(fl_apply_e, i) = (fl_apply_v=fl_ctx->Stack[fl_ctx->SP-1]);
            }
            else if (isarray(fl_apply_e)) {
                fl_apply_v = cvalue_array_aset(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-3]);
            }
            else {
                type_error(fl_ctx, "aset!", "sequence", fl_apply_e);
            }
            POPN(fl_ctx, 2);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;
        OP(OP_FOR)
            s  = tofixnum(fl_ctx, fl_ctx->Stack[fl_ctx->SP-3], "for");
            hi = tofixnum(fl_ctx, fl_ctx->Stack[fl_ctx->SP-2], "for");
            //f = fl_ctx->Stack[fl_ctx->SP-1];
            fl_apply_v = FL_UNSPECIFIED(fl_ctx);
            fl_ctx->SP += 2;
            n = fl_ctx->SP;
            for(; s <= hi; s++) {
                fl_ctx->Stack[fl_ctx->SP-2] = fl_ctx->Stack[fl_ctx->SP-3];
                fl_ctx->Stack[fl_ctx->SP-1] = fixnum(s);
                fl_apply_v = apply_cl(fl_ctx, 1);
                fl_ctx->SP = n;
            }
            POPN(fl_ctx, 4);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;

        OP(OP_LOADT) PUSH(fl_ctx, fl_ctx->T); NEXT_OP;
        OP(OP_LOADF) PUSH(fl_ctx, fl_ctx->F); NEXT_OP;
        OP(OP_LOADNIL) PUSH(fl_ctx, fl_ctx->NIL); NEXT_OP;
        OP(OP_LOAD0) PUSH(fl_ctx, fixnum(0)); NEXT_OP;
        OP(OP_LOAD1) PUSH(fl_ctx, fixnum(1)); NEXT_OP;
        OP(OP_LOADI8) s = (int8_t)*ip++; PUSH(fl_ctx, fixnum(s)); NEXT_OP;
        OP(OP_LOADV)
            fl_apply_v = fn_vals(fl_ctx->Stack[bp-1]);
            assert(*ip < vector_size(fl_apply_v));
            fl_apply_v = vector_elt(fl_apply_v, *ip); ip++;
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_LOADVL)
            fl_apply_v = fn_vals(fl_ctx->Stack[bp-1]);
            fl_apply_v = vector_elt(fl_apply_v, GET_INT32(ip)); ip+=4;
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_LOADGL)
            fl_apply_v = fn_vals(fl_ctx->Stack[bp-1]);
            fl_apply_v = vector_elt(fl_apply_v, GET_INT32(ip)); ip+=4;
            goto do_loadg;
        OP(OP_LOADG)
            fl_apply_v = fn_vals(fl_ctx->Stack[bp-1]);
            assert(*ip < vector_size(fl_apply_v));
            fl_apply_v = vector_elt(fl_apply_v, *ip); ip++;
        do_loadg:
            assert(issymbol(fl_apply_v));
            sym = (symbol_t*)ptr(fl_apply_v);
            if (sym->binding == UNBOUND)
                fl_raise(fl_ctx, fl_list2(fl_ctx, fl_ctx->UnboundError, fl_apply_v));
            PUSH(fl_ctx, sym->binding);
            NEXT_OP;

        OP(OP_SETGL)
            fl_apply_v = fn_vals(fl_ctx->Stack[bp-1]);
            fl_apply_v = vector_elt(fl_apply_v, GET_INT32(ip)); ip+=4;
            goto do_setg;
        OP(OP_SETG)
            fl_apply_v = fn_vals(fl_ctx->Stack[bp-1]);
            assert(*ip < vector_size(fl_apply_v));
            fl_apply_v = vector_elt(fl_apply_v, *ip); ip++;
        do_setg:
            assert(issymbol(fl_apply_v));
            sym = (symbol_t*)ptr(fl_apply_v);
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            if (!isconstant(sym))
                sym->binding = fl_apply_v;
            NEXT_OP;

        OP(OP_LOADA)
            i = *ip++;
            fl_apply_v = fl_ctx->Stack[bp+i];
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_LOADA0)
            fl_apply_v = fl_ctx->Stack[bp];
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_LOADA1)
            fl_apply_v = fl_ctx->Stack[bp+1];
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_LOADAL)
            i = GET_INT32(ip); ip+=4;
            fl_apply_v = fl_ctx->Stack[bp+i];
            PUSH(fl_ctx, fl_apply_v);
            NEXT_OP;
        OP(OP_SETA)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            i = *ip++;
            fl_ctx->Stack[bp+i] = fl_apply_v;
            NEXT_OP;
        OP(OP_SETAL)
            fl_apply_v = fl_ctx->Stack[fl_ctx->SP-1];
            i = GET_INT32(ip); ip+=4;
            fl_ctx->Stack[bp+i] = fl_apply_v;
            NEXT_OP;

        OP(OP_BOX)
            i = *ip++;
            fl_apply_v = mk_cons(fl_ctx);
            car_(fl_apply_v) = fl_ctx->Stack[bp+i];
            cdr_(fl_apply_v) = fl_ctx->NIL;
            fl_ctx->Stack[bp+i] = fl_apply_v;
            NEXT_OP;
        OP(OP_BOXL)
            i = GET_INT32(ip); ip+=4;
            fl_apply_v = mk_cons(fl_ctx);
            car_(fl_apply_v) = fl_ctx->Stack[bp+i];
            cdr_(fl_apply_v) = fl_ctx->NIL;
            fl_ctx->Stack[bp+i] = fl_apply_v;
            NEXT_OP;

        OP(OP_SHIFT)
            i = *ip++;
            fl_ctx->Stack[fl_ctx->SP-1-i] = fl_ctx->Stack[fl_ctx->SP-1];
            fl_ctx->SP -= i;
            NEXT_OP;

        OP(OP_LOADC)
            i = *ip++;
            fl_apply_v = fl_ctx->Stack[bp+nargs];
            assert(isvector(fl_apply_v));
            assert(i < vector_size(fl_apply_v));
            PUSH(fl_ctx, vector_elt(fl_apply_v, i));
            NEXT_OP;

        OP(OP_LOADC0)
            PUSH(fl_ctx, vector_elt(fl_ctx->Stack[bp+nargs], 0));
            NEXT_OP;
        OP(OP_LOADC1)
            PUSH(fl_ctx, vector_elt(fl_ctx->Stack[bp+nargs], 1));
            NEXT_OP;

        OP(OP_LOADCL)
            i = GET_INT32(ip); ip+=4;
            fl_apply_v = fl_ctx->Stack[bp+nargs];
            PUSH(fl_ctx, vector_elt(fl_apply_v, i));
            NEXT_OP;

        OP(OP_CLOSURE)
            n = *ip++;
            assert(n > 0);
            fl_apply_pv = alloc_words(fl_ctx, n + 1);
            fl_apply_v = tagptr(fl_apply_pv, TAG_VECTOR);
            fl_apply_pv[0] = fixnum(n);
            i = 1;
            do {
                fl_apply_pv[i] = fl_ctx->Stack[fl_ctx->SP-n + i-1];
                i++;
            } while (i<=n);
            POPN(fl_ctx, n);
            PUSH(fl_ctx, fl_apply_v);
#ifdef MEMDEBUG2
            fl_apply_pv = alloc_words(fl_ctx, 4);
#else
            if ((value_t*)fl_ctx->curheap > ((value_t*)fl_ctx->lim)-2)
                gc(fl_ctx, 0);
            fl_apply_pv = (value_t*)fl_ctx->curheap;
            fl_ctx->curheap += (4*sizeof(value_t));
#endif
            fl_apply_e = fl_ctx->Stack[fl_ctx->SP-2];  // closure to copy
            assert(isfunction(fl_apply_e));
            fl_apply_pv[0] = ((value_t*)ptr(fl_apply_e))[0];
            fl_apply_pv[1] = ((value_t*)ptr(fl_apply_e))[1];
            fl_apply_pv[2] = fl_ctx->Stack[fl_ctx->SP-1];  // env
            fl_apply_pv[3] = ((value_t*)ptr(fl_apply_e))[3];
            POPN(fl_ctx, 1);
            fl_ctx->Stack[fl_ctx->SP-1] = tagptr(fl_apply_pv, TAG_FUNCTION);
            NEXT_OP;

        OP(OP_TRYCATCH)
            fl_apply_v = do_trycatch(fl_ctx);
            POPN(fl_ctx, 1);
            fl_ctx->Stack[fl_ctx->SP-1] = fl_apply_v;
            NEXT_OP;

        OP(OP_OPTARGS)
            i = GET_INT32(ip); ip+=4;
            n = GET_INT32(ip); ip+=4;
            if (nargs < i)
                lerror(fl_ctx, fl_ctx->ArgError, "apply: too few arguments");
            if ((int32_t)n > 0) {
                if (nargs > n)
                    lerror(fl_ctx, fl_ctx->ArgError, "apply: too many arguments");
            }
            else n = -n;
            if (n > nargs) {
                n -= nargs;
                fl_ctx->SP += n;
                fl_ctx->Stack[fl_ctx->SP-1] = fl_ctx->Stack[fl_ctx->SP-n-1];
                fl_ctx->Stack[fl_ctx->SP-2] = nargs+n;
                fl_ctx->Stack[fl_ctx->SP-3] = fl_ctx->Stack[fl_ctx->SP-n-3];
                fl_ctx->Stack[fl_ctx->SP-4] = fl_ctx->Stack[fl_ctx->SP-n-4];
                fl_ctx->curr_frame = fl_ctx->SP;
                for(i=0; i < n; i++) {
                    fl_ctx->Stack[bp+nargs+i] = UNBOUND;
                }
                nargs += n;
            }
            NEXT_OP;
        OP(OP_KEYARGS)
            fl_apply_v = fn_vals(fl_ctx->Stack[bp-1]);
            fl_apply_v = vector_elt(fl_apply_v, 0);
            i = GET_INT32(ip); ip+=4;
            n = GET_INT32(ip); ip+=4;
            s = GET_INT32(ip); ip+=4;
            nargs = process_keys(fl_ctx, fl_apply_v, i, n, llabs(s)-(i+n), bp, nargs, s<0);
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
static value_t _stacktrace(fl_context_t *fl_ctx, uint32_t top)
{
    uint32_t bp, sz;
    value_t v, lst = fl_ctx->NIL;
    fl_gc_handle(fl_ctx, &lst);
    while (top > 0) {
        sz = fl_ctx->Stack[top-2]+1;
        bp = top-4-sz;
        v = alloc_vector(fl_ctx, sz, 0);
        memcpy(&vector_elt(v,0), &fl_ctx->Stack[bp], sz*sizeof(value_t));
        lst = fl_cons(fl_ctx, v, lst);
        top = fl_ctx->Stack[top-3];
    }
    fl_free_gc_handles(fl_ctx, 1);
    return lst;
}

// builtins -------------------------------------------------------------------

void assign_global_builtins(fl_context_t *fl_ctx, const builtinspec_t *b)
{
    while (b->name != NULL) {
        setc(symbol(fl_ctx, b->name), cbuiltin(fl_ctx, b->name, b->fptr));
        b++;
    }
}

static value_t fl_function(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs == 1 && issymbol(args[0]))
        return fl_builtin(fl_ctx, args, nargs);
    if (nargs < 2 || nargs > 4)
        argcount(fl_ctx, "function", nargs, 2);
    if (!fl_isstring(fl_ctx, args[0]))
        type_error(fl_ctx, "function", "string", args[0]);
    if (!isvector(args[1]))
        type_error(fl_ctx, "function", "vector", args[1]);
    cvalue_t *arr = (cvalue_t*)ptr(args[0]);
    cv_pin(fl_ctx, arr);
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
    function_t *fn = (function_t*)alloc_words(fl_ctx, 4);
    value_t fv = tagptr(fn, TAG_FUNCTION);
    fn->bcode = args[0];
    fn->vals = args[1];
    fn->env = fl_ctx->NIL;
    fn->name = fl_ctx->LAMBDA;
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
                    type_error(fl_ctx, "function", "symbol", args[3]);
                fn->name = args[3];
            }
        }
        if (isgensym(fl_ctx, fn->name))
            lerror(fl_ctx, fl_ctx->ArgError, "function: name should not be a gensym");
    }
    return fv;
}

static value_t fl_function_code(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "function:code", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error(fl_ctx, "function:code", "function", v);
    return fn_bcode(v);
}
static value_t fl_function_vals(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "function:vals", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error(fl_ctx, "function:vals", "function", v);
    return fn_vals(v);
}
static value_t fl_function_env(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "function:env", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error(fl_ctx, "function:env", "function", v);
    return fn_env(v);
}
static value_t fl_function_name(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "function:name", nargs, 1);
    value_t v = args[0];
    if (!isclosure(v)) type_error(fl_ctx, "function:name", "function", v);
    return fn_name(v);
}

value_t fl_copylist(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "copy-list", nargs, 1);
    return copy_list(fl_ctx, args[0]);
}

value_t fl_append(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs == 0)
        return fl_ctx->NIL;
    value_t first=fl_ctx->NIL, lst, lastcons=fl_ctx->NIL;
    fl_gc_handle(fl_ctx, &first);
    fl_gc_handle(fl_ctx, &lastcons);
    uint32_t i=0;
    while (1) {
        lst = args[i++];
        if (i >= nargs) break;
        if (iscons(lst)) {
            lst = copy_list(fl_ctx, lst);
            if (first == fl_ctx->NIL)
                first = lst;
            else
                cdr_(lastcons) = lst;
#ifdef MEMDEBUG2
            lastcons = lst;
            while (cdr_(lastcons) != fl_ctx->NIL)
                lastcons = cdr_(lastcons);
#else
            lastcons = tagptr((((cons_t*)fl_ctx->curheap)-1), TAG_CONS);
#endif
        }
        else if (lst != fl_ctx->NIL) {
            type_error(fl_ctx, "append", "cons", lst);
        }
    }
    if (first == fl_ctx->NIL)
        first = lst;
    else
        cdr_(lastcons) = lst;
    fl_free_gc_handles(fl_ctx, 2);
    return first;
}

value_t fl_liststar(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs == 1) return args[0];
    else if (nargs == 0) argcount(fl_ctx, "list*", nargs, 1);
    return _list(fl_ctx, args, nargs, 1);
}

value_t fl_stacktrace(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    (void)args;
    argcount(fl_ctx, "stacktrace", nargs, 0);
    return _stacktrace(fl_ctx, fl_ctx->throwing_frame ? fl_ctx->throwing_frame : fl_ctx->curr_frame);
}

value_t fl_map1(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 2)
        lerror(fl_ctx, fl_ctx->ArgError, "map: too few arguments");
    if (!iscons(args[1])) return fl_ctx->NIL;
    value_t v;
    uint32_t first, last, argSP = args-fl_ctx->Stack;
    assert(args >= fl_ctx->Stack && argSP < fl_ctx->N_STACK);
    if (nargs == 2) {
        if (fl_ctx->SP+4 > fl_ctx->N_STACK) grow_stack(fl_ctx);
        PUSH(fl_ctx, fl_ctx->Stack[argSP]);
        PUSH(fl_ctx, car_(fl_ctx->Stack[argSP+1]));
        v = _applyn(fl_ctx, 1);
        POPN(fl_ctx, 2);
        PUSH(fl_ctx, v);
        v = mk_cons(fl_ctx);
        car_(v) = POP(fl_ctx); cdr_(v) = fl_ctx->NIL;
        PUSH(fl_ctx, v);
        PUSH(fl_ctx, v);
        first = fl_ctx->SP-2;
        last = fl_ctx->SP-1;
        fl_ctx->Stack[argSP+1] = cdr_(fl_ctx->Stack[argSP+1]);
        while (iscons(fl_ctx->Stack[argSP+1])) {
            PUSH(fl_ctx, fl_ctx->Stack[argSP]);
            PUSH(fl_ctx, car_(fl_ctx->Stack[argSP+1]));
            v = _applyn(fl_ctx, 1);
            POPN(fl_ctx, 2);
            PUSH(fl_ctx, v);
            v = mk_cons(fl_ctx);
            car_(v) = POP(fl_ctx); cdr_(v) = fl_ctx->NIL;
            cdr_(fl_ctx->Stack[last]) = v;
            fl_ctx->Stack[last] = v;
            fl_ctx->Stack[argSP+1] = cdr_(fl_ctx->Stack[argSP+1]);
        }
        POPN(fl_ctx, 2);
    }
    else {
        size_t i;
        while (fl_ctx->SP+nargs+1 > fl_ctx->N_STACK) grow_stack(fl_ctx);
        PUSH(fl_ctx, fl_ctx->Stack[argSP]);
        for(i=1; i < nargs; i++) {
            PUSH(fl_ctx, car(fl_ctx, fl_ctx->Stack[argSP+i]));
            fl_ctx->Stack[argSP+i] = cdr_(fl_ctx->Stack[argSP+i]);
        }
        v = _applyn(fl_ctx, nargs-1);
        POPN(fl_ctx, nargs);
        PUSH(fl_ctx, v);
        v = mk_cons(fl_ctx);
        car_(v) = POP(fl_ctx); cdr_(v) = fl_ctx->NIL;
        PUSH(fl_ctx, v);
        PUSH(fl_ctx, v);
        first = fl_ctx->SP-2;
        last = fl_ctx->SP-1;
        while (iscons(fl_ctx->Stack[argSP+1])) {
            PUSH(fl_ctx, fl_ctx->Stack[argSP]);
            for(i=1; i < nargs; i++) {
                PUSH(fl_ctx, car(fl_ctx, fl_ctx->Stack[argSP+i]));
                fl_ctx->Stack[argSP+i] = cdr_(fl_ctx->Stack[argSP+i]);
            }
            v = _applyn(fl_ctx, nargs-1);
            POPN(fl_ctx, nargs);
            PUSH(fl_ctx, v);
            v = mk_cons(fl_ctx);
            car_(v) = POP(fl_ctx); cdr_(v) = fl_ctx->NIL;
            cdr_(fl_ctx->Stack[last]) = v;
            fl_ctx->Stack[last] = v;
        }
        POPN(fl_ctx, 2);
    }
    return fl_ctx->Stack[first];
}

value_t fl_foreach(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs != 2)
        lerror(fl_ctx, fl_ctx->ArgError, "for-each: expected 2 arguments");
    uint32_t argSP = args-fl_ctx->Stack;
    assert(args >= fl_ctx->Stack && argSP < fl_ctx->N_STACK);
    if (fl_ctx->SP+2 > fl_ctx->N_STACK) grow_stack(fl_ctx);
    PUSH(fl_ctx, fl_ctx->T);
    PUSH(fl_ctx, fl_ctx->T);
    while (iscons(fl_ctx->Stack[argSP+1])) {
        fl_ctx->Stack[fl_ctx->SP-2] = fl_ctx->Stack[argSP];
        fl_ctx->Stack[fl_ctx->SP-1] = car_(fl_ctx->Stack[argSP+1]);
        _applyn(fl_ctx, 1);
        fl_ctx->Stack[argSP+1] = cdr_(fl_ctx->Stack[argSP+1]);
    }
    POPN(fl_ctx, 2);
    return fl_ctx->T;
}

static const builtinspec_t core_builtin_info[] = {
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
    { "for-each", fl_foreach },
    { NULL, NULL }
};

// initialization -------------------------------------------------------------

extern void builtins_init(fl_context_t *fl_ctx);
extern void comparehash_init(fl_context_t *fl_ctx);

static void lisp_init(fl_context_t *fl_ctx, size_t initial_heapsize)
{
    int i;

    libsupport_init();

    fl_ctx->SP = 0;
    fl_ctx->curr_frame = 0;
    fl_ctx->N_GCHND = 0;
    fl_ctx->readstate = NULL;
    fl_ctx->gensym_ctr = 0;
    fl_ctx->gsnameno = 0;

#ifdef MEMDEBUG2
    fl_ctx->tochain = NULL;
    fl_ctx->n_allocd = 0;
#endif

    fl_ctx->heapsize = initial_heapsize;

    fl_ctx->fromspace = (unsigned char*)LLT_ALLOC(fl_ctx->heapsize);
#ifdef MEMDEBUG
    fl_ctx->tospace   = NULL;
#else
    fl_ctx->tospace   = (unsigned char*)LLT_ALLOC(fl_ctx->heapsize);
#endif
    fl_ctx->curheap = fl_ctx->fromspace;
    fl_ctx->lim = fl_ctx->curheap+fl_ctx->heapsize-sizeof(cons_t);
    fl_ctx->consflags = bitvector_new(fl_ctx->heapsize/sizeof(cons_t), 1);
    fl_print_init(fl_ctx);
    comparehash_init(fl_ctx);
    fl_ctx->N_STACK = 262144;
    fl_ctx->Stack = (value_t*)malloc(fl_ctx->N_STACK*sizeof(value_t));
    // TODO: if fl_ctx->Stack == NULL
    CHECK_ALIGN8(fl_ctx->Stack);

    fl_ctx->NIL = builtin(OP_THE_EMPTY_LIST);
    fl_ctx->T = builtin(OP_BOOL_CONST_T);
    fl_ctx->F = builtin(OP_BOOL_CONST_F);
    fl_ctx->FL_EOF = builtin(OP_EOF_OBJECT);
    fl_ctx->LAMBDA = symbol(fl_ctx, "lambda");        fl_ctx->FUNCTION = symbol(fl_ctx, "function");
    fl_ctx->QUOTE = symbol(fl_ctx, "quote");          fl_ctx->TRYCATCH = symbol(fl_ctx, "trycatch");
    fl_ctx->BACKQUOTE = symbol(fl_ctx, "quasiquote");       fl_ctx->COMMA = symbol(fl_ctx, "unquote");
    fl_ctx->COMMAAT = symbol(fl_ctx, "unquote-splicing");   fl_ctx->COMMADOT = symbol(fl_ctx, "unquote-nsplicing");
    fl_ctx->IOError = symbol(fl_ctx, "io-error");     fl_ctx->ParseError = symbol(fl_ctx, "parse-error");
    fl_ctx->TypeError = symbol(fl_ctx, "type-error"); fl_ctx->ArgError = symbol(fl_ctx, "arg-error");
    fl_ctx->UnboundError = symbol(fl_ctx, "unbound-error");
    fl_ctx->KeyError = symbol(fl_ctx, "key-error");   fl_ctx->OutOfMemoryError = symbol(fl_ctx, "memory-error");
    fl_ctx->BoundsError = symbol(fl_ctx, "bounds-error");
    fl_ctx->DivideError = symbol(fl_ctx, "divide-error");
    fl_ctx->EnumerationError = symbol(fl_ctx, "enumeration-error");
    fl_ctx->pairsym = symbol(fl_ctx, "pair");
    fl_ctx->symbolsym = symbol(fl_ctx, "symbol");     fl_ctx->fixnumsym = symbol(fl_ctx, "fixnum");
    fl_ctx->vectorsym = symbol(fl_ctx, "vector");     fl_ctx->builtinsym = symbol(fl_ctx, "builtin");
    fl_ctx->booleansym = symbol(fl_ctx, "boolean");   fl_ctx->nullsym = symbol(fl_ctx, "null");
    fl_ctx->definesym = symbol(fl_ctx, "define");     fl_ctx->defmacrosym = symbol(fl_ctx, "define-macro");
    fl_ctx->forsym = symbol(fl_ctx, "for");
    fl_ctx->setqsym = symbol(fl_ctx, "set!");         fl_ctx->evalsym = symbol(fl_ctx, "eval");
    fl_ctx->vu8sym = symbol(fl_ctx, "vu8");           fl_ctx->fnsym = symbol(fl_ctx, "fn");
    fl_ctx->nulsym = symbol(fl_ctx, "nul");           fl_ctx->alarmsym = symbol(fl_ctx, "alarm");
    fl_ctx->backspacesym = symbol(fl_ctx, "backspace"); fl_ctx->tabsym = symbol(fl_ctx, "tab");
    fl_ctx->linefeedsym = symbol(fl_ctx, "linefeed"); fl_ctx->vtabsym = symbol(fl_ctx, "vtab");
    fl_ctx->pagesym = symbol(fl_ctx, "page");         fl_ctx->returnsym = symbol(fl_ctx, "return");
    fl_ctx->escsym = symbol(fl_ctx, "esc");           fl_ctx->spacesym = symbol(fl_ctx, "space");
    fl_ctx->deletesym = symbol(fl_ctx, "delete");     fl_ctx->newlinesym = symbol(fl_ctx, "newline");
    fl_ctx->tsym = symbol(fl_ctx, "t"); fl_ctx->Tsym = symbol(fl_ctx, "T");
    fl_ctx->fsym = symbol(fl_ctx, "f"); fl_ctx->Fsym = symbol(fl_ctx, "F");
    set(fl_ctx->printprettysym=symbol(fl_ctx, "*print-pretty*"), fl_ctx->T);
    set(fl_ctx->printreadablysym=symbol(fl_ctx, "*print-readably*"), fl_ctx->T);
    set(fl_ctx->printwidthsym=symbol(fl_ctx, "*print-width*"), fixnum(fl_ctx->SCR_WIDTH));
    set(fl_ctx->printlengthsym=symbol(fl_ctx, "*print-length*"), fl_ctx->F);
    set(fl_ctx->printlevelsym=symbol(fl_ctx, "*print-level*"), fl_ctx->F);
    fl_ctx->builtins_table_sym = symbol(fl_ctx, "*builtins*");
    fl_ctx->lasterror = fl_ctx->NIL;
    i = 0;
    for (i=OP_EQ; i <= OP_ASET; i++) {
        setc(symbol(fl_ctx, builtin_names[i]), builtin(i));
    }
    setc(symbol(fl_ctx, "eq"), builtin(OP_EQ));
    setc(symbol(fl_ctx, "procedure?"), builtin(OP_FUNCTIONP));
    setc(symbol(fl_ctx, "top-level-bound?"), builtin(OP_BOUNDP));

#if defined(_OS_LINUX_)
    set(symbol(fl_ctx, "*os-name*"), symbol(fl_ctx, "linux"));
#elif defined(_OS_WINDOWS_)
    set(symbol(fl_ctx, "*os-name*"), symbol(fl_ctx, "win32"));
#elif defined(_OS_DARWIN_)
    set(symbol(fl_ctx, "*os-name*"), symbol(fl_ctx, "macos"));
#else
    set(symbol(fl_ctx, "*os-name*"), symbol(fl_ctx, "unknown"));
#endif

    fl_ctx->jl_sym = symbol(fl_ctx, "julia_value");

    fl_ctx->the_empty_vector = tagptr(alloc_words(fl_ctx, 1), TAG_VECTOR);
    vector_setsize(fl_ctx->the_empty_vector, 0);

    cvalues_init(fl_ctx);

    char exename[1024];
    size_t exe_size = sizeof(exename) / sizeof(exename[0]);
    if ( uv_exepath(exename, &exe_size) == 0 ) {
        setc(symbol(fl_ctx, "*install-dir*"), cvalue_static_cstring(fl_ctx, strdup(dirname(exename))));
    }

    fl_ctx->memory_exception_value = fl_list2(fl_ctx, fl_ctx->OutOfMemoryError,
                                              cvalue_static_cstring(fl_ctx, "out of memory"));

    assign_global_builtins(fl_ctx, core_builtin_info);

    fl_read_init(fl_ctx);

    builtins_init(fl_ctx);
}

// top level ------------------------------------------------------------------

value_t fl_toplevel_eval(fl_context_t *fl_ctx, value_t expr)
{
    return fl_applyn(fl_ctx, 1, symbol_value(fl_ctx->evalsym), expr);
}

extern void fl_init_julia_extensions(fl_context_t *fl_ctx);

void fl_init(fl_context_t *fl_ctx, size_t initial_heapsize)
{
    lisp_init(fl_ctx, initial_heapsize);
    fl_init_julia_extensions(fl_ctx);
}

int fl_load_system_image_str(fl_context_t *fl_ctx, char *str, size_t len)
{
    value_t img = cvalue(fl_ctx, fl_ctx->iostreamtype, sizeof(ios_t));
    ios_t *pi = value2c(ios_t*, img);
    ios_static_buffer(pi, str, len);

    return fl_load_system_image(fl_ctx, img);
}

int fl_load_system_image(fl_context_t *fl_ctx, value_t sys_image_iostream)
{
    value_t e;
    int saveSP;
    symbol_t *sym;

    PUSH(fl_ctx, sys_image_iostream);
    saveSP = fl_ctx->SP;
    FL_TRY(fl_ctx) {
        while (1) {
            e = fl_read_sexpr(fl_ctx, fl_ctx->Stack[fl_ctx->SP-1]);
            if (ios_eof(value2c(ios_t*,fl_ctx->Stack[fl_ctx->SP-1]))) break;
            if (isfunction(e)) {
                // stage 0 format: series of thunks
                PUSH(fl_ctx, e);
                (void)_applyn(fl_ctx, 0);
                fl_ctx->SP = saveSP;
            }
            else {
                // stage 1 format: list alternating symbol/value
                while (iscons(e)) {
                    sym = tosymbol(fl_ctx, car_(e), "bootstrap");
                    e = cdr_(e);
                    (void)tocons(fl_ctx, e, "bootstrap");
                    sym->binding = car_(e);
                    e = cdr_(e);
                }
                break;
            }
        }
    }
    FL_CATCH(fl_ctx) {
        ios_puts("fatal error during bootstrap:\n", ios_stderr);
        fl_print(fl_ctx, ios_stderr, fl_ctx->lasterror);
        ios_putc('\n', ios_stderr);
        return 1;
    }
    ios_close(value2c(ios_t*,fl_ctx->Stack[fl_ctx->SP-1]));
    POPN(fl_ctx, 1);
    return 0;
}

#ifdef __cplusplus
}
#endif
