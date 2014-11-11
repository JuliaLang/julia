extern void *memrchr(const void *s, int c, size_t n);

static htable_t printconses;
static u_int32_t printlabel;
static int print_pretty;
static int print_princ;
static fixnum_t print_length;
static fixnum_t print_level;
static fixnum_t P_LEVEL;
static int SCR_WIDTH = 80;

static int HPOS=0, VPOS;
static void outc(char c, ios_t *f)
{
    ios_putc(c, f);
    if (c == '\n')
        HPOS = 0;
    else
        HPOS++;
}
static void outs(char *s, ios_t *f)
{
    ios_puts(s, f);
    HPOS += u8_strwidth(s);
}
static void outsn(char *s, ios_t *f, size_t n)
{
    ios_write(f, s, n);
    HPOS += u8_strwidth(s);
}
static int outindent(int n, ios_t *f)
{
    // move back to left margin if we get too indented
    if (n > SCR_WIDTH-12)
        n = 2;
    int n0 = n;
    ios_putc('\n', f);
    VPOS++;
    HPOS = n;
    while (n >= 8) {
        ios_putc('\t', f);
        n -= 8;
    }
    while (n) {
        ios_putc(' ', f);
        n--;
    }
    return n0;
}

void fl_print_chr(char c, ios_t *f)
{
    outc(c, f);
}

void fl_print_str(char *s, ios_t *f)
{
    outs(s, f);
}

void print_traverse(value_t v)
{
    value_t *bp;
    while (iscons(v)) {
        if (ismarked(v)) {
            bp = (value_t*)ptrhash_bp(&printconses, (void*)v);
            if (*bp == (value_t)HT_NOTFOUND)
                *bp = fixnum(printlabel++);
            return;
        }
        mark_cons(v);
        print_traverse(car_(v));
        v = cdr_(v);
    }
    if (!ismanaged(v) || issymbol(v))
        return;
    if (ismarked(v)) {
        bp = (value_t*)ptrhash_bp(&printconses, (void*)v);
        if (*bp == (value_t)HT_NOTFOUND)
            *bp = fixnum(printlabel++);
        return;
    }
    if (isvector(v)) {
        if (vector_size(v) > 0)
            mark_cons(v);
        unsigned int i;
        for(i=0; i < vector_size(v); i++)
            print_traverse(vector_elt(v,i));
    }
    else if (iscprim(v)) {
        mark_cons(v);
    }
    else if (isclosure(v)) {
        mark_cons(v);
        function_t *f = (function_t*)ptr(v);
        print_traverse(f->bcode);
        print_traverse(f->vals);
        print_traverse(f->env);
    }
    else {
        assert(iscvalue(v));
        cvalue_t *cv = (cvalue_t*)ptr(v);
        // don't consider shared references to ""
        if (!cv_isstr(cv) || cv_len(cv)!=0)
            mark_cons(v);
        fltype_t *t = cv_class(cv);
        if (t->vtable != NULL && t->vtable->print_traverse != NULL)
            t->vtable->print_traverse(v);
    }
}

static void print_symbol_name(ios_t *f, char *name)
{
    int i, escape=0, charescape=0;

    if ((name[0] == '\0') ||
        (name[0] == '.' && name[1] == '\0') ||
        (name[0] == '#') ||
        isnumtok(name, NULL))
        escape = 1;
    i=0;
    while (name[i]) {
        if (!symchar(name[i])) {
            escape = 1;
            if (name[i]=='|' || name[i]=='\\') {
                charescape = 1;
                break;
            }
        }
        i++;
    }
    if (escape) {
        if (charescape) {
            outc('|', f);
            i=0;
            while (name[i]) {
                if (name[i]=='|' || name[i]=='\\')
                    outc('\\', f);
                outc(name[i], f);
                i++;
            }
            outc('|', f);
        }
        else {
            outc('|', f);
            outs(name, f);
            outc('|', f);
        }
    }
    else {
        outs(name, f);
    }
}

/*
  The following implements a simple pretty-printing algorithm. This is
  an unlimited-width approach that doesn't require an extra pass.
  It uses some heuristics to guess whether an expression is "small",
  and avoids wrapping symbols across lines. The result is high
  performance and nice output for typical code. Quality is poor for
  pathological or deeply-nested expressions, but those are difficult
  to print anyway.
*/
#define SMALL_STR_LEN 20
static inline int tinyp(value_t v)
{
    if (issymbol(v))
        return (u8_strwidth(symbol_name(v)) < SMALL_STR_LEN);
    if (fl_isstring(v))
        return (cv_len((cvalue_t*)ptr(v)) < SMALL_STR_LEN);
    return (isfixnum(v) || isbuiltin(v) || v==FL_F || v==FL_T || v==FL_NIL ||
            v == FL_EOF);
}

static int smallp(value_t v)
{
    if (tinyp(v)) return 1;
    if (fl_isnumber(v)) return 1;
    if (iscons(v)) {
        if (tinyp(car_(v)) && (tinyp(cdr_(v)) ||
                               (iscons(cdr_(v)) && tinyp(car_(cdr_(v))) &&
                                cdr_(cdr_(v))==NIL)))
            return 1;
        return 0;
    }
    if (isvector(v)) {
        size_t s = vector_size(v);
        return (s == 0 || (tinyp(vector_elt(v,0)) &&
                           (s == 1 || (s == 2 &&
                                       tinyp(vector_elt(v,1))))));
    }
    return 0;
}

static int specialindent(value_t head)
{
    // indent these forms 2 spaces, not lined up with the first argument
    if (head == LAMBDA || head == TRYCATCH || head == definesym ||
        head == defmacrosym || head == forsym)
        return 2;
    return -1;
}

static int lengthestimate(value_t v)
{
    // get the width of an expression if we can do so cheaply
    if (issymbol(v))
        return u8_strwidth(symbol_name(v));
    return -1;
}

static int allsmallp(value_t v)
{
    int n = 1;
    while (iscons(v)) {
        if (!smallp(car_(v)))
            return 0;
        v = cdr_(v);
        n++;
        if (n > 25)
            return n;
    }
    return n;
}

static int indentafter3(value_t head, value_t v)
{
    // for certain X always indent (X a b c) after b
    return ((head == forsym) && !allsmallp(cdr_(v)));
}

static int indentafter2(value_t head, value_t v)
{
    // for certain X always indent (X a b) after a
    return ((head == definesym || head == defmacrosym) &&
            !allsmallp(cdr_(v)));
}

static int indentevery(value_t v)
{
    // indent before every subform of a special form, unless every
    // subform is "small"
    value_t c = car_(v);
    if (c == LAMBDA || c == setqsym)
        return 0;
    if (c == IF) // TODO: others
        return !allsmallp(cdr_(v));
    return 0;
}

static int blockindent(value_t v)
{
    // in this case we switch to block indent mode, where the head
    // is no longer considered special:
    // (a b c d e
    //  f g h i j)
    return (allsmallp(v) > 9);
}

static void print_pair(ios_t *f, value_t v)
{
    value_t cd;
    char *op = NULL;
    if (iscons(cdr_(v)) && cdr_(cdr_(v)) == NIL &&
        !ptrhash_has(&printconses, (void*)cdr_(v)) &&
        (((car_(v) == QUOTE)     && (op = "'"))  ||
         ((car_(v) == BACKQUOTE) && (op = "`"))  ||
         ((car_(v) == COMMA)     && (op = ","))  ||
         ((car_(v) == COMMAAT)   && (op = ",@")) ||
         ((car_(v) == COMMADOT)  && (op = ",.")))) {
        // special prefix syntax
        unmark_cons(v);
        unmark_cons(cdr_(v));
        outs(op, f);
        fl_print_child(f, car_(cdr_(v)));
        return;
    }
    int startpos = HPOS;
    outc('(', f);
    int newindent=HPOS, blk=blockindent(v);
    int lastv, n=0, si, ind=0, est, always=0, nextsmall, thistiny;
    if (!blk) always = indentevery(v);
    value_t head = car_(v);
    int after3 = indentafter3(head, v);
    int after2 = indentafter2(head, v);
    int n_unindented = 1;
    while (1) {
        cd = cdr_(v);
        if (print_length >= 0 && n >= print_length && cd!=NIL) {
            outsn("...)", f, 4);
            break;
        }
        lastv = VPOS;
        unmark_cons(v);
        fl_print_child(f, car_(v));
        if (!iscons(cd) || ptrhash_has(&printconses, (void*)cd)) {
            if (cd != NIL) {
                outsn(" . ", f, 3);
                fl_print_child(f, cd);
            }
            outc(')', f);
            break;
        }

        if (!print_pretty ||
            ((head == LAMBDA) && n == 0)) {
            // never break line before lambda-list
            ind = 0;
        }
        else {
            est = lengthestimate(car_(cd));
            nextsmall = smallp(car_(cd));
            thistiny = tinyp(car_(v));
            ind = (((VPOS > lastv) ||
                    (HPOS>SCR_WIDTH/2 && !nextsmall && !thistiny && n>0)) ||

                   (HPOS > SCR_WIDTH-4) ||

                   (est!=-1 && (HPOS+est > SCR_WIDTH-2)) ||

                   ((head == LAMBDA) && !nextsmall) ||

                   (n > 0 && always) ||

                   (n == 2 && after3) ||
                   (n == 1 && after2) ||

                   (n_unindented >= 3 && !nextsmall) ||

                   (n == 0 && !smallp(head)));
        }

        if (ind) {
            newindent = outindent(newindent, f);
            n_unindented = 1;
        }
        else {
            n_unindented++;
            outc(' ', f);
            if (n==0) {
                // set indent level after printing head
                si = specialindent(head);
                if (si != -1)
                    newindent = startpos + si;
                else if (!blk)
                    newindent = HPOS;
            }
        }
        n++;
        v = cd;
    }
}

static void cvalue_print(ios_t *f, value_t v);

static int print_circle_prefix(ios_t *f, value_t v)
{
    value_t label;
    char buf[64];
    char *str;
    if ((label=(value_t)ptrhash_get(&printconses, (void*)v)) !=
        (value_t)HT_NOTFOUND) {
        if (!ismarked(v)) {
            //HPOS+=ios_printf(f, "#%ld#", numval(label));
            outc('#', f);
            str = uint2str(buf, sizeof(buf)-1, numval(label), 10);
            outs(str, f);
            outc('#', f);
            return 1;
        }
        //HPOS+=ios_printf(f, "#%ld=", numval(label));
        outc('#', f);
        str = uint2str(buf, sizeof(buf)-1, numval(label), 10);
        outs(str, f);
        outc('=', f);
    }
    if (ismanaged(v))
        unmark_cons(v);
    return 0;
}

void fl_print_child(ios_t *f, value_t v)
{
    char *name, *str;
    char buf[64];
    if (print_level >= 0 && P_LEVEL >= print_level &&
        (iscons(v) || isvector(v) || isclosure(v))) {
        outc('#', f);
        return;
    }
    P_LEVEL++;

    switch (tag(v)) {
    case TAG_NUM :
    case TAG_NUM1: //HPOS+=ios_printf(f, "%ld", numval(v)); break;
        str = uint2str(&buf[1], sizeof(buf)-1, labs(numval(v)), 10);
        if (numval(v)<0)
            *(--str) = '-';
        outs(str, f);
        break;
    case TAG_SYM:
        name = symbol_name(v);
        if (print_princ)
            outs(name, f);
        else if (ismanaged(v)) {
            outsn("#:", f, 2);
            outs(name, f);
        }
        else
            print_symbol_name(f, name);
        break;
    case TAG_FUNCTION:
        if (v == FL_T) {
            outsn("#t", f, 2);
        }
        else if (v == FL_F) {
            outsn("#f", f, 2);
        }
        else if (v == FL_NIL) {
            outsn("()", f, 2);
        }
        else if (v == FL_EOF) {
            outsn("#<eof>", f, 6);
        }
        else if (isbuiltin(v)) {
            if (!print_princ)
                outsn("#.", f, 2);
            outs(builtin_names[uintval(v)], f);
        }
        else {
            assert(isclosure(v));
            if (!print_princ) {
                if (print_circle_prefix(f, v)) break;
                function_t *fn = (function_t*)ptr(v);
                outs("#fn(", f);
                char *data = (char*)cvalue_data(fn->bcode);
                size_t i, sz = cvalue_len(fn->bcode);
                for(i=0; i < sz; i++) data[i] += 48;
                fl_print_child(f, fn->bcode);
                for(i=0; i < sz; i++) data[i] -= 48;
                outc(' ', f);
                fl_print_child(f, fn->vals);
                if (fn->env != NIL) {
                    outc(' ', f);
                    fl_print_child(f, fn->env);
                }
                if (fn->name != LAMBDA) {
                    outc(' ', f);
                    fl_print_child(f, fn->name);
                }
                outc(')', f);
            }
            else {
                outs("#<function>", f);
            }
        }
        break;
    case TAG_CVALUE:
    case TAG_CPRIM:
        if (v == UNBOUND) { outs("#<undefined>", f); break; }
    case TAG_VECTOR:
    case TAG_CONS:
        if (print_circle_prefix(f, v)) break;
        if (isvector(v)) {
            outc('[', f);
            int newindent = HPOS, est;
            int i, sz = vector_size(v);
            for(i=0; i < sz; i++) {
                if (print_length >= 0 && i >= print_length && i < sz-1) {
                    outsn("...", f, 3);
                    break;
                }
                fl_print_child(f, vector_elt(v,i));
                if (i < sz-1) {
                    if (!print_pretty) {
                        outc(' ', f);
                    }
                    else {
                        est = lengthestimate(vector_elt(v,i+1));
                        if (HPOS > SCR_WIDTH-4 ||
                            (est!=-1 && (HPOS+est > SCR_WIDTH-2)) ||
                            (HPOS > SCR_WIDTH/2 &&
                             !smallp(vector_elt(v,i+1)) &&
                             !tinyp(vector_elt(v,i))))
                            newindent = outindent(newindent, f);
                        else
                            outc(' ', f);
                    }
                }
            }
            outc(']', f);
            break;
        }
        if (iscvalue(v) || iscprim(v))
            cvalue_print(f, v);
        else
            print_pair(f, v);
        break;
    }
    P_LEVEL--;
}

static void print_string(ios_t *f, char *str, size_t sz)
{
    char buf[512];
    size_t i = 0;
    uint8_t c;
    static char hexdig[] = "0123456789abcdef";

    outc('"', f);
    if (!u8_isvalid(str, sz)) {
        // alternate print algorithm that preserves data if it's not UTF-8
        for(i=0; i < sz; i++) {
            c = str[i];
            if (c == '\\')
                outsn("\\\\", f, 2);
            else if (c == '"')
                outsn("\\\"", f, 2);
            else if (c >= 32 && c < 0x7f)
                outc(c, f);
            else {
                outsn("\\x", f, 2);
                outc(hexdig[c>>4], f);
                outc(hexdig[c&0xf], f);
            }
        }
    }
    else {
        while (i < sz) {
            size_t n = u8_escape(buf, sizeof(buf), str, &i, sz, 1, 0);
            outsn(buf, f, n-1);
        }
    }
    outc('"', f);
}

static numerictype_t sym_to_numtype(value_t type);
#ifndef _OS_WINDOWS_
#define __USE_GNU
#include <dlfcn.h>
#undef __USE_GNU
#endif

#define sign_bit(r) ((*(int64_t*)&(r)) & BIT63)
#define DFINITE(d) (((*(int64_t*)&(d))&0x7ff0000000000000LL)!=0x7ff0000000000000LL)

// 'weak' means we don't need to accurately reproduce the type, so
// for example #int32(0) can be printed as just 0. this is used
// printing in a context where a type is already implied, e.g. inside
// an array.
static void cvalue_printdata(ios_t *f, void *data, size_t len, value_t type,
                             int weak)
{
    if (type == bytesym) {
        unsigned char ch = *(unsigned char*)data;
        if (print_princ)
            outc(ch, f);
        else if (weak)
            HPOS+=ios_printf(f, "0x%hhx", ch);
        else
            HPOS+=ios_printf(f, "#byte(0x%hhx)", ch);
    }
    else if (type == wcharsym) {
        uint32_t wc = *(uint32_t*)data;
        char seq[8];
        size_t nb = u8_toutf8(seq, sizeof(seq), &wc, 1);
        seq[nb] = '\0';
        if (print_princ) {
            // TODO: better multibyte handling
            outs(seq, f);
        }
        else {
            outsn("#\\", f, 2);
            if      (wc == 0x00) outsn("nul", f, 3);
            else if (wc == 0x07) outsn("alarm", f, 5);
            else if (wc == 0x08) outsn("backspace", f, 9);
            else if (wc == 0x09) outsn("tab", f, 3);
            else if (wc == 0x0A) outsn("linefeed", f, 8);
            //else if (wc == 0x0A) outsn("newline", f, 7);
            else if (wc == 0x0B) outsn("vtab", f, 4);
            else if (wc == 0x0C) outsn("page", f, 4);
            else if (wc == 0x0D) outsn("return", f, 6);
            else if (wc == 0x1B) outsn("esc", f, 3);
            else if (wc == 0x20) outsn("space", f, 5);
            else if (wc == 0x7F) outsn("delete", f, 6);
            else if (iswprint(wc)) outs(seq, f);
            else HPOS+=ios_printf(f, "x%04x", (int)wc);
        }
    }
    else if (type == floatsym || type == doublesym) {
        char buf[64];
        double d;
        if (type == floatsym) { d = (double)*(float*)data; }
        else { d = *(double*)data; }
        if (!DFINITE(d)) {
            char *rep;
            if (d != d)
                rep = (char*)(sign_bit(d) ? "-nan.0" : "+nan.0");
            else
                rep = (char*)(sign_bit(d) ? "-inf.0" : "+inf.0");
            if (type == floatsym && !print_princ && !weak)
                HPOS+=ios_printf(f, "#%s(%s)", symbol_name(type), rep);
            else
                outs(rep, f);
        }
        else if (d == 0) {
            if (sign_bit(d))
                outsn("-0.0", f, 4);
            else
                outsn("0.0", f, 3);
            if (type == floatsym && !print_princ && !weak)
                outc('f', f);
        }
        else {
            double ad = d < 0 ? -d : d;
            if ((long)d == d && ad < 1e6 && ad >= 1e-4) {
                snprintf(buf, sizeof(buf), "%g", d);
            }
            else {
                if (type == floatsym)
                    snprintf(buf, sizeof(buf), "%.8g", d);
                else
                    snprintf(buf, sizeof(buf), "%.16g", d);
            }
            int hasdec = (strpbrk(buf, ".eE") != NULL);
            outs(buf, f);
            if (!hasdec) outsn(".0", f, 2);
            if (type == floatsym && !print_princ && !weak)
                outc('f', f);
        }
    }
    else if (type == uint64sym
#ifdef _P64
             || type == sizesym
#endif
             ) {
        uint64_t ui64 = *(uint64_t*)data;
        if (weak || print_princ)
            HPOS += ios_printf(f, "%llu", ui64);
        else
            HPOS += ios_printf(f, "#%s(%llu)", symbol_name(type), ui64);
    }
    else if (issymbol(type)) {
        // handle other integer prims. we know it's smaller than uint64
        // at this point, so int64 is big enough to capture everything.
        numerictype_t nt = sym_to_numtype(type);
        if (nt == N_NUMTYPES) {
            static size_t (*jl_static_print)(ios_t*, void*) = 0;
            static int init = 0;
            static value_t jl_sym = 0;
            if (init == 0) {
                init = 1;
#if defined(RTLD_SELF)
                jl_static_print = (size_t (*)(ios_t *, void *)) dlsym(RTLD_SELF, "jl_static_show");
#elif defined(RTLD_DEFAULT)
                jl_static_print = (size_t (*)(ios_t *, void *)) dlsym(RTLD_DEFAULT, "jl_static_show");
#endif
                jl_sym = symbol("julia_value");
            }
            if (jl_static_print != 0 && jl_sym == type) {
                HPOS += ios_printf(f, "#<julia: ");
                HPOS += jl_static_print(f, *(void**)data);
                HPOS += ios_printf(f, ">");
            }
            else
                HPOS += ios_printf(f, "#<%s>", symbol_name(type));
        }
        else {
            int64_t i64 = conv_to_int64(data, nt);
            if (weak || print_princ)
                HPOS += ios_printf(f, "%lld", i64);
            else
                HPOS += ios_printf(f, "#%s(%lld)", symbol_name(type), i64);
        }
    }
    else if (iscons(type)) {
        if (car_(type) == arraysym) {
            value_t eltype = car(cdr_(type));
            size_t cnt, elsize;
            if (iscons(cdr_(cdr_(type)))) {
                cnt = tosize(car_(cdr_(cdr_(type))), "length");
                elsize = cnt ? len/cnt : 0;
            }
            else {
                // incomplete array type
                int junk;
                elsize = ctype_sizeof(eltype, &junk);
                cnt = elsize ? len/elsize : 0;
            }
            if (eltype == bytesym) {
                if (print_princ) {
                    ios_write(f, (char*)data, len);
                    /*
                    char *nl = memrchr(data, '\n', len);
                    if (nl)
                        HPOS = u8_strwidth(nl+1);
                    else
                        HPOS += u8_strwidth(data);
                    */
                }
                else {
                    print_string(f, (char*)data, len);
                }
                return;
            }
            else if (eltype == wcharsym) {
                // TODO wchar
            }
            else {
            }
            size_t i;
            if (!weak) {
                if (eltype == uint8sym) {
                    outsn("#vu8(", f, 5);
                }
                else {
                    outsn("#array(", f, 7);
                    fl_print_child(f, eltype);
                    if (cnt > 0)
                        outc(' ', f);
                }
            }
            else {
                outc('[', f);
            }
            for(i=0; i < cnt; i++) {
                if (i > 0)
                    outc(' ', f);
                cvalue_printdata(f, data, elsize, eltype, 1);
                data = (char *)data + elsize;
            }
            if (!weak)
                outc(')', f);
            else
                outc(']', f);
        }
    }
}

static void cvalue_print(ios_t *f, value_t v)
{
    cvalue_t *cv = (cvalue_t*)ptr(v);
    void *data = cptr(v);
    value_t label;

    if (cv_class(cv) == builtintype) {
        void *fptr = *(void**)data;
        label = (value_t)ptrhash_get(&reverse_dlsym_lookup_table, cv);
        if (label == (value_t)HT_NOTFOUND) {
            HPOS += ios_printf(f, "#<builtin @0x%08zx>",
                               (size_t)(builtin_t)fptr);
        }
        else {
            if (print_princ) {
                outs(symbol_name(label), f);
            }
            else {
                outsn("#fn(", f, 4);
                outs(symbol_name(label), f);
                outc(')', f);
            }
        }
    }
    else if (cv_class(cv)->vtable != NULL &&
             cv_class(cv)->vtable->print != NULL) {
        cv_class(cv)->vtable->print(v, f);
    }
    else {
        value_t type = cv_type(cv);
        size_t len = iscprim(v) ? cv_class(cv)->size : cv_len(cv);
        cvalue_printdata(f, data, len, type, 0);
    }
}

static void set_print_width(void)
{
    value_t pw = symbol_value(printwidthsym);
    if (!isfixnum(pw)) return;
    SCR_WIDTH = numval(pw);
}

void fl_print(ios_t *f, value_t v)
{
    print_pretty = (symbol_value(printprettysym) != FL_F);
    if (print_pretty)
        set_print_width();
    print_princ = (symbol_value(printreadablysym) == FL_F);

    value_t pl = symbol_value(printlengthsym);
    if (isfixnum(pl)) print_length = numval(pl);
    else print_length = -1;
    pl = symbol_value(printlevelsym);
    if (isfixnum(pl)) print_level = numval(pl);
    else print_level = -1;
    P_LEVEL = 0;

    printlabel = 0;
    print_traverse(v);
    HPOS = VPOS = 0;

    fl_print_child(f, v);

    if (print_level >= 0 || print_length >= 0) {
        memset(consflags, 0, 4*bitvector_nwords(heapsize/sizeof(cons_t)));
    }

    if ((iscons(v) || isvector(v) || isfunction(v) || iscvalue(v)) &&
        !fl_isstring(v) && v!=FL_T && v!=FL_F && v!=FL_NIL) {
        htable_reset(&printconses, 32);
    }
}
