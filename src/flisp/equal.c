#define BOUNDED_COMPARE_BOUND 4096
#define BOUNDED_HASH_BOUND    16384

// comparable tag
#define cmptag(v) (isfixnum(v) ? TAG_NUM : tag(v))

static value_t eq_class(htable_t *table, value_t key)
{
    value_t c = (value_t)ptrhash_get(table, (void*)key);
    if (c == (value_t)HT_NOTFOUND)
        return NIL;
    if (c == key)
        return c;
    return eq_class(table, c);
}

static void eq_union(htable_t *table, value_t a, value_t b,
                     value_t c, value_t cb)
{
    value_t ca = (c==NIL ? a : c);
    if (cb != NIL)
        ptrhash_put(table, (void*)cb, (void*)ca);
    ptrhash_put(table, (void*)a, (void*)ca);
    ptrhash_put(table, (void*)b, (void*)ca);
}

static value_t bounded_compare(value_t a, value_t b, int bound, int eq);
static value_t cyc_compare(value_t a, value_t b, htable_t *table, int eq);

static value_t bounded_vector_compare(value_t a, value_t b, int bound, int eq)
{
    size_t la = vector_size(a);
    size_t lb = vector_size(b);
    size_t m, i;
    if (eq && (la!=lb)) return fixnum(1);
    m = la < lb ? la : lb;
    for (i = 0; i < m; i++) {
        value_t d = bounded_compare(vector_elt(a,i), vector_elt(b,i),
                                    bound-1, eq);
        if (d==NIL || numval(d)!=0) return d;
    }
    if (la < lb) return fixnum(-1);
    if (la > lb) return fixnum(1);
    return fixnum(0);
}

// strange comparisons are resolved arbitrarily but consistently.
// ordering: number < cprim < function < vector < cvalue < symbol < cons
static value_t bounded_compare(value_t a, value_t b, int bound, int eq)
{
    value_t d;

 compare_top:
    if (a == b) return fixnum(0);
    if (bound <= 0)
        return NIL;
    int taga = tag(a);
    int tagb = cmptag(b);
    int c;
    switch (taga) {
    case TAG_NUM :
    case TAG_NUM1:
        if (isfixnum(b)) {
            return (numval(a) < numval(b)) ? fixnum(-1) : fixnum(1);
        }
        if (iscprim(b)) {
            if (cp_class((cprim_t*)ptr(b)) == wchartype)
                return fixnum(1);
            return fixnum(numeric_compare(a, b, eq, 1, NULL));
        }
        return fixnum(-1);
    case TAG_SYM:
        if (eq) return fixnum(1);
        if (tagb < TAG_SYM) return fixnum(1);
        if (tagb > TAG_SYM) return fixnum(-1);
        return fixnum(strcmp(symbol_name(a), symbol_name(b)));
    case TAG_VECTOR:
        if (isvector(b))
            return bounded_vector_compare(a, b, bound, eq);
        break;
    case TAG_CPRIM:
        if (cp_class((cprim_t*)ptr(a)) == wchartype) {
            if (!iscprim(b) || cp_class((cprim_t*)ptr(b)) != wchartype)
                return fixnum(-1);
        }
        else if (iscprim(b) && cp_class((cprim_t*)ptr(b)) == wchartype) {
            return fixnum(1);
        }
        c = numeric_compare(a, b, eq, 1, NULL);
        if (c != 2)
            return fixnum(c);
        break;
    case TAG_CVALUE:
        if (iscvalue(b)) {
            if (cv_isPOD((cvalue_t*)ptr(a)) && cv_isPOD((cvalue_t*)ptr(b)))
                return cvalue_compare(a, b);
            return fixnum(1);
        }
        break;
    case TAG_FUNCTION:
        if (tagb == TAG_FUNCTION) {
            if (uintval(a) > N_BUILTINS && uintval(b) > N_BUILTINS) {
                function_t *fa = (function_t*)ptr(a);
                function_t *fb = (function_t*)ptr(b);
                d = bounded_compare(fa->bcode, fb->bcode, bound-1, eq);
                if (d==NIL || numval(d) != 0) return d;
                d = bounded_compare(fa->vals, fb->vals, bound-1, eq);
                if (d==NIL || numval(d) != 0) return d;
                d = bounded_compare(fa->env, fb->env, bound-1, eq);
                if (d==NIL || numval(d) != 0) return d;
                return fixnum(0);
            }
            return (uintval(a) < uintval(b)) ? fixnum(-1) : fixnum(1);
        }
        break;
    case TAG_CONS:
        if (tagb < TAG_CONS) return fixnum(1);
        d = bounded_compare(car_(a), car_(b), bound-1, eq);
        if (d==NIL || numval(d) != 0) return d;
        a = cdr_(a); b = cdr_(b);
        bound--;
        goto compare_top;
    }
    return (taga < tagb) ? fixnum(-1) : fixnum(1);
}

static value_t cyc_vector_compare(value_t a, value_t b, htable_t *table,
                                  int eq)
{
    size_t la = vector_size(a);
    size_t lb = vector_size(b);
    size_t m, i;
    value_t d, xa, xb, ca, cb;

    // first try to prove them different with no recursion
    if (eq && (la!=lb)) return fixnum(1);
    m = la < lb ? la : lb;
    for (i = 0; i < m; i++) {
        xa = vector_elt(a,i);
        xb = vector_elt(b,i);
        if (leafp(xa) || leafp(xb)) {
            d = bounded_compare(xa, xb, 1, eq);
            if (d!=NIL && numval(d)!=0) return d;
        }
        else if (tag(xa) < tag(xb)) {
            return fixnum(-1);
        }
        else if (tag(xa) > tag(xb)) {
            return fixnum(1);
        }
    }

    ca = eq_class(table, a);
    cb = eq_class(table, b);
    if (ca!=NIL && ca==cb)
        return fixnum(0);

    eq_union(table, a, b, ca, cb);

    for (i = 0; i < m; i++) {
        xa = vector_elt(a,i);
        xb = vector_elt(b,i);
        if (!leafp(xa) || tag(xa)==TAG_FUNCTION) {
            d = cyc_compare(xa, xb, table, eq);
            if (numval(d)!=0)
                return d;
        }
    }

    if (la < lb) return fixnum(-1);
    if (la > lb) return fixnum(1);
    return fixnum(0);
}

static value_t cyc_compare(value_t a, value_t b, htable_t *table, int eq)
{
    value_t d, ca, cb;
 cyc_compare_top:
    if (a==b)
        return fixnum(0);
    if (iscons(a)) {
        if (iscons(b)) {
            value_t aa = car_(a); value_t da = cdr_(a);
            value_t ab = car_(b); value_t db = cdr_(b);
            int tagaa = tag(aa); int tagda = tag(da);
            int tagab = tag(ab); int tagdb = tag(db);
            if (leafp(aa) || leafp(ab)) {
                d = bounded_compare(aa, ab, 1, eq);
                if (d!=NIL && numval(d)!=0) return d;
            }
            else if (tagaa < tagab)
                return fixnum(-1);
            else if (tagaa > tagab)
                return fixnum(1);
            if (leafp(da) || leafp(db)) {
                d = bounded_compare(da, db, 1, eq);
                if (d!=NIL && numval(d)!=0) return d;
            }
            else if (tagda < tagdb)
                return fixnum(-1);
            else if (tagda > tagdb)
                return fixnum(1);

            ca = eq_class(table, a);
            cb = eq_class(table, b);
            if (ca!=NIL && ca==cb)
                return fixnum(0);

            eq_union(table, a, b, ca, cb);
            d = cyc_compare(aa, ab, table, eq);
            if (numval(d)!=0) return d;
            a = da;
            b = db;
            goto cyc_compare_top;
        }
        else {
            return fixnum(1);
        }
    }
    else if (isvector(a) && isvector(b)) {
        return cyc_vector_compare(a, b, table, eq);
    }
    else if (isclosure(a) && isclosure(b)) {
        function_t *fa = (function_t*)ptr(a);
        function_t *fb = (function_t*)ptr(b);
        d = bounded_compare(fa->bcode, fb->bcode, 1, eq);
        if (numval(d) != 0) return d;

        ca = eq_class(table, a);
        cb = eq_class(table, b);
        if (ca!=NIL && ca==cb)
            return fixnum(0);

        eq_union(table, a, b, ca, cb);
        d = cyc_compare(fa->vals, fb->vals, table, eq);
        if (numval(d) != 0) return d;
        a = fa->env;
        b = fb->env;
        goto cyc_compare_top;
    }
    return bounded_compare(a, b, 1, eq);
}

static htable_t equal_eq_hashtable;
void comparehash_init(void)
{
    htable_new(&equal_eq_hashtable, 512);
}

// 'eq' means unordered comparison is sufficient
static value_t compare_(value_t a, value_t b, int eq)
{
    value_t guess = bounded_compare(a, b, BOUNDED_COMPARE_BOUND, eq);
    if (guess == NIL) {
        guess = cyc_compare(a, b, &equal_eq_hashtable, eq);
        htable_reset(&equal_eq_hashtable, 512);
    }
    return guess;
}

value_t fl_compare(value_t a, value_t b)
{
    return compare_(a, b, 0);
}

value_t fl_equal(value_t a, value_t b)
{
    if (eq_comparable(a, b))
        return (a == b) ? FL_T : FL_F;
    return (numval(compare_(a,b,1))==0 ? FL_T : FL_F);
}

/*
  optimizations:
  - use hash updates instead of calling lookup then insert. i.e. get the
    bp once and use it twice.
  * preallocate hash table and call reset() instead of new/free
  * less redundant tag checking, 3-bit tags
*/

#ifdef _P64
#define MIX(a, b) int64hash((int64_t)(a) ^ (int64_t)(b));
#define doublehash(a) int64hash(a)
#else
#define MIX(a, b) int64to32hash(((int64_t)(a))<<32 | ((int64_t)(b)))
#define doublehash(a) int64to32hash(a)
#endif

// *oob: output argument, means we hit the limit specified by 'bound'
static uptrint_t bounded_hash(value_t a, int bound, int *oob)
{
    *oob = 0;
    union {
        double d;
        int64_t i64;
    } u;
    numerictype_t nt;
    size_t i, len;
    cvalue_t *cv;
    cprim_t *cp;
    void *data;
    uptrint_t h = 0;
    int oob2, tg = tag(a);
    switch(tg) {
    case TAG_NUM :
    case TAG_NUM1:
        u.d = (double)numval(a);
        return doublehash(u.i64);
    case TAG_FUNCTION:
        if (uintval(a) > N_BUILTINS)
            return bounded_hash(((function_t*)ptr(a))->bcode, bound, oob);
        return inthash(a);
    case TAG_SYM:
        return ((symbol_t*)ptr(a))->hash;
    case TAG_CPRIM:
        cp = (cprim_t*)ptr(a);
        data = cp_data(cp);
        if (cp_class(cp) == wchartype)
            return inthash(*(int32_t*)data);
        nt = cp_numtype(cp);
        u.d = conv_to_double(data, nt);
        return doublehash(u.i64);
    case TAG_CVALUE:
        cv = (cvalue_t*)ptr(a);
        data = cv_data(cv);
        return memhash((char*)data, cv_len(cv));

    case TAG_VECTOR:
        if (bound <= 0) {
            *oob = 1;
            return 1;
        }
        len = vector_size(a);
        for(i=0; i < len; i++) {
            h = MIX(h, bounded_hash(vector_elt(a,i), bound/2, &oob2)^1);
            if (oob2)
                bound/=2;
            *oob = *oob || oob2;
        }
        return h;

    case TAG_CONS:
        do {
            if (bound <= 0) {
                *oob = 1;
                return h;
            }
            h = MIX(h, bounded_hash(car_(a), bound/2, &oob2));
            // bounds balancing: try to share the bounds efficiently
            // so we can hash better when a list is cdr-deep (a common case)
            if (oob2)
                bound/=2;
            else
                bound--;
            // recursive OOB propagation. otherwise this case is slow:
            // (hash '#2=((#0=(#1=(#1#) . #0#)) . #2#))
            *oob = *oob || oob2;
            a = cdr_(a);
        } while (iscons(a));
        h = MIX(h, bounded_hash(a, bound-1, &oob2)^2);
        *oob = *oob || oob2;
        return h;
    }
    return 0;
}

int equal_lispvalue(value_t a, value_t b)
{
    if (eq_comparable(a, b))
        return (a==b);
    return (numval(compare_(a,b,1))==0);
}

uptrint_t hash_lispvalue(value_t a)
{
    int oob=0;
    uptrint_t n = bounded_hash(a, BOUNDED_HASH_BOUND, &oob);
    return n;
}

value_t fl_hash(value_t *args, u_int32_t nargs)
{
    argcount("hash", nargs, 1);
    return fixnum(hash_lispvalue(args[0]));
}
