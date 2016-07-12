#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <setjmp.h>
#include "flisp.h"

#ifdef __cplusplus
extern "C" {
#endif

void print_iostream(fl_context_t *fl_ctx, value_t v, ios_t *f)
{
    (void)v;
    fl_print_str(fl_ctx, "#<io stream>", f);
}

void free_iostream(fl_context_t *fl_ctx, value_t self)
{
    (void)fl_ctx;
    ios_t *s = value2c(ios_t*, self);
    ios_close(s);
}

void relocate_iostream(fl_context_t *fl_ctx, value_t oldv, value_t newv)
{
    (void)fl_ctx;
    ios_t *olds = value2c(ios_t*, oldv);
    ios_t *news = value2c(ios_t*, newv);
    if (news->buf == &olds->local[0]) {
        news->buf = &news->local[0];
    }
}

const cvtable_t iostream_vtable = { print_iostream, relocate_iostream,
                                    free_iostream, NULL };

int fl_isiostream(fl_context_t *fl_ctx, value_t v)
{
    return iscvalue(v) && cv_class((cvalue_t*)ptr(v)) == fl_ctx->iostreamtype;
}

value_t fl_iostreamp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "iostream?", nargs, 1);
    return fl_isiostream(fl_ctx, args[0]) ? fl_ctx->T : fl_ctx->F;
}

value_t fl_eof_object(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    (void)args;
    argcount(fl_ctx, "eof-object", nargs, 0);
    return fl_ctx->FL_EOF;
}

value_t fl_eof_objectp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "eof-object?", nargs, 1);
    return (fl_ctx->FL_EOF == args[0]) ? fl_ctx->T : fl_ctx->F;
}

static ios_t *toiostream(fl_context_t *fl_ctx, value_t v, const char *fname)
{
    if (!fl_isiostream(fl_ctx, v))
        type_error(fl_ctx, fname, "iostream", v);
    return value2c(ios_t*, v);
}

ios_t *fl_toiostream(fl_context_t *fl_ctx, value_t v, const char *fname)
{
    return toiostream(fl_ctx, v, fname);
}

value_t fl_file(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 1)
        argcount(fl_ctx, "file", nargs, 1);
    int i, r=0, w=0, c=0, t=0, a=0;
    for(i=1; i < (int)nargs; i++) {
        if      (args[i] == fl_ctx->wrsym)    w = 1;
        else if (args[i] == fl_ctx->apsym)    { a = 1; w = 1; }
        else if (args[i] == fl_ctx->crsym)    { c = 1; w = 1; }
        else if (args[i] == fl_ctx->truncsym) { t = 1; w = 1; }
        else if (args[i] == fl_ctx->rdsym)    r = 1;
    }
    if ((r|w|c|t|a) == 0) r = 1;  // default to reading
    value_t f = cvalue(fl_ctx, fl_ctx->iostreamtype, sizeof(ios_t));
    char *fname = tostring(fl_ctx, args[0], "file");
    ios_t *s = value2c(ios_t*, f);
    if (ios_file(s, fname, r, w, c, t) == NULL)
        lerrorf(fl_ctx, fl_ctx->IOError, "file: could not open \"%s\"", fname);
    if (a) ios_seek_end(s);
    return f;
}

value_t fl_buffer(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "buffer", nargs, 0);
    (void)args;
    value_t f = cvalue(fl_ctx, fl_ctx->iostreamtype, sizeof(ios_t));
    ios_t *s = value2c(ios_t*, f);
    if (ios_mem(s, 0) == NULL)
        lerror(fl_ctx, fl_ctx->OutOfMemoryError, "buffer: could not allocate stream");
    return f;
}

value_t fl_read(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    value_t arg = 0;
    if (nargs > 1) {
        argcount(fl_ctx, "read", nargs, 1);
    }
    else if (nargs == 0) {
        arg = symbol_value(fl_ctx->instrsym);
    }
    else {
        arg = args[0];
    }
    (void)toiostream(fl_ctx, arg, "read");
    fl_gc_handle(fl_ctx, &arg);
    value_t v = fl_read_sexpr(fl_ctx, arg);
    fl_free_gc_handles(fl_ctx, 1);
    if (ios_eof(value2c(ios_t*,arg)))
        return fl_ctx->FL_EOF;
    return v;
}

value_t fl_iogetc(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.getc", nargs, 1);
    ios_t *s = toiostream(fl_ctx, args[0], "io.getc");
    uint32_t wc;
    if (ios_getutf8(s, &wc) == IOS_EOF)
        //lerror(fl_ctx, IOError, "io.getc: end of file reached");
        return fl_ctx->FL_EOF;
    return mk_wchar(fl_ctx, wc);
}

value_t fl_iopeekc(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.peekc", nargs, 1);
    ios_t *s = toiostream(fl_ctx, args[0], "io.peekc");
    uint32_t wc;
    if (ios_peekutf8(s, &wc) == IOS_EOF)
        return fl_ctx->FL_EOF;
    return mk_wchar(fl_ctx, wc);
}

value_t fl_ioputc(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.putc", nargs, 2);
    ios_t *s = toiostream(fl_ctx, args[0], "io.putc");
    if (!iscprim(args[1]) || ((cprim_t*)ptr(args[1]))->type != fl_ctx->wchartype)
        type_error(fl_ctx, "io.putc", "wchar", args[1]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[1]));
    return fixnum(ios_pututf8(s, wc));
}

value_t fl_ioungetc(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.ungetc", nargs, 2);
    ios_t *s = toiostream(fl_ctx, args[0], "io.ungetc");
    if (!iscprim(args[1]) || ((cprim_t*)ptr(args[1]))->type != fl_ctx->wchartype)
        type_error(fl_ctx, "io.ungetc", "wchar", args[1]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[1]));
    if (wc >= 0x80) {
        lerror(fl_ctx, fl_ctx->ArgError, "io_ungetc: unicode not yet supported");
    }
    return fixnum(ios_ungetc((int)wc,s));
}

value_t fl_ioflush(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.flush", nargs, 1);
    ios_t *s = toiostream(fl_ctx, args[0], "io.flush");
    if (ios_flush(s) != 0)
        return fl_ctx->F;
    return fl_ctx->T;
}

value_t fl_ioclose(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.close", nargs, 1);
    ios_t *s = toiostream(fl_ctx, args[0], "io.close");
    ios_close(s);
    return fl_ctx->T;
}

value_t fl_iopurge(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.discardbuffer", nargs, 1);
    ios_t *s = toiostream(fl_ctx, args[0], "io.discardbuffer");
    ios_purge(s);
    return fl_ctx->T;
}

value_t fl_ioeof(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.eof?", nargs, 1);
    ios_t *s = toiostream(fl_ctx, args[0], "io.eof?");
    return (ios_eof(s) ? fl_ctx->T : fl_ctx->F);
}

value_t fl_iolineno(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "input-port-line", nargs, 1);
    ios_t *s = toiostream(fl_ctx, args[0], "input-port-line");
    return size_wrap(fl_ctx, s->lineno);
}

value_t fl_ioseek(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.seek", nargs, 2);
    ios_t *s = toiostream(fl_ctx, args[0], "io.seek");
    size_t pos = tosize(fl_ctx, args[1], "io.seek");
    int64_t res = ios_seek(s, pos);
    if (res < 0)
        return fl_ctx->F;
    return fl_ctx->T;
}

value_t fl_iopos(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.pos", nargs, 1);
    ios_t *s = toiostream(fl_ctx, args[0], "io.pos");
    int64_t res = ios_pos(s);
    if (res == -1)
        return fl_ctx->F;
    return size_wrap(fl_ctx, (size_t)res);
}

value_t fl_write(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 1 || nargs > 2)
        argcount(fl_ctx, "write", nargs, 1);
    ios_t *s;
    if (nargs == 2)
        s = toiostream(fl_ctx, args[1], "write");
    else
        s = toiostream(fl_ctx, symbol_value(fl_ctx->outstrsym), "write");
    fl_print(fl_ctx, s, args[0]);
    return args[0];
}

value_t fl_ioread(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs != 3)
        argcount(fl_ctx, "io.read", nargs, 2);
    (void)toiostream(fl_ctx, args[0], "io.read");
    size_t n;
    fltype_t *ft;
    if (nargs == 3) {
        // form (io.read s type count)
        ft = get_array_type(fl_ctx, args[1]);
        n = tosize(fl_ctx, args[2], "io.read") * ft->elsz;
    }
    else {
        ft = get_type(fl_ctx, args[1]);
        if (ft->eltype != NULL && !iscons(cdr_(cdr_(args[1]))))
            lerror(fl_ctx, fl_ctx->ArgError, "io.read: incomplete type");
        n = ft->size;
    }
    value_t cv = cvalue(fl_ctx, ft, n);
    char *data;
    if (iscvalue(cv)) data = (char*)cv_data((cvalue_t*)ptr(cv));
    else data = cp_data((cprim_t*)ptr(cv));
    size_t got = ios_read(value2c(ios_t*,args[0]), data, n);
    if (got < n)
        //lerror(fl_ctx, IOError, "io.read: end of input reached");
        return fl_ctx->FL_EOF;
    return cv;
}

// args must contain data[, offset[, count]]
static void get_start_count_args(fl_context_t *fl_ctx, value_t *args, uint32_t nargs, size_t sz,
                                 size_t *offs, size_t *nb, char *fname)
{
    if (nargs > 1) {
        *offs = tosize(fl_ctx, args[1], fname);
        if (nargs > 2)
            *nb = tosize(fl_ctx, args[2], fname);
        else
            *nb = sz - *offs;
        if (*offs >= sz || *offs + *nb > sz)
            bounds_error(fl_ctx, fname, args[0], args[1]);
    }
}

value_t fl_iowrite(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 2 || nargs > 4)
        argcount(fl_ctx, "io.write", nargs, 2);
    ios_t *s = toiostream(fl_ctx, args[0], "io.write");
    if (iscprim(args[1]) && ((cprim_t*)ptr(args[1]))->type == fl_ctx->wchartype) {
        if (nargs > 2)
            lerror(fl_ctx, fl_ctx->ArgError,
                   "io.write: offset argument not supported for characters");
        uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[1]));
        return fixnum(ios_pututf8(s, wc));
    }
    char *data;
    size_t sz, offs=0;
    to_sized_ptr(fl_ctx, args[1], "io.write", &data, &sz);
    size_t nb = sz;
    if (nargs > 2) {
        get_start_count_args(fl_ctx, &args[1], nargs-1, sz, &offs, &nb, "io.write");
        data += offs;
    }
    return size_wrap(fl_ctx, ios_write(s, data, nb));
}

static char get_delim_arg(fl_context_t *fl_ctx, value_t arg, char *fname)
{
    size_t uldelim = tosize(fl_ctx, arg, fname);
    if (uldelim > 0x7f) {
        // wchars > 0x7f, or anything else > 0xff, are out of range
        if ((iscprim(arg) && cp_class((cprim_t*)ptr(arg))==fl_ctx->wchartype) ||
            uldelim > 0xff)
            lerrorf(fl_ctx, fl_ctx->ArgError, "%s: delimiter out of range", fname);
    }
    return (char)uldelim;
}

value_t fl_ioreaduntil(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.readuntil", nargs, 2);
    value_t str = cvalue_string(fl_ctx, 80);
    cvalue_t *cv = (cvalue_t*)ptr(str);
    char *data = (char*)cv_data(cv);
    ios_t dest;
    ios_mem(&dest, 0);
    ios_setbuf(&dest, data, 80, 0);
    char delim = get_delim_arg(fl_ctx, args[1], "io.readuntil");
    ios_t *src = toiostream(fl_ctx, args[0], "io.readuntil");
    size_t n = ios_copyuntil(&dest, src, delim);
    cv->len = n;
    if (dest.buf != data) {
        // outgrew initial space
        cv->data = dest.buf;
        cv_autorelease(fl_ctx, cv);
    }
    ((char*)cv->data)[n] = '\0';
    if (n == 0 && ios_eof(src))
        return fl_ctx->FL_EOF;
    return str;
}

value_t fl_iocopyuntil(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.copyuntil", nargs, 3);
    ios_t *dest = toiostream(fl_ctx, args[0], "io.copyuntil");
    ios_t *src = toiostream(fl_ctx, args[1], "io.copyuntil");
    char delim = get_delim_arg(fl_ctx, args[2], "io.copyuntil");
    return size_wrap(fl_ctx, ios_copyuntil(dest, src, delim));
}

value_t fl_iocopy(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 2 || nargs > 3)
        argcount(fl_ctx, "io.copy", nargs, 2);
    ios_t *dest = toiostream(fl_ctx, args[0], "io.copy");
    ios_t *src = toiostream(fl_ctx, args[1], "io.copy");
    if (nargs == 3) {
        size_t n = tosize(fl_ctx, args[2], "io.copy");
        return size_wrap(fl_ctx, ios_copy(dest, src, n));
    }
    return size_wrap(fl_ctx, ios_copyall(dest, src));
}

value_t stream_to_string(fl_context_t *fl_ctx, value_t *ps)
{
    value_t str;
    size_t n;
    ios_t *st = value2c(ios_t*,*ps);
    if (st->buf == &st->local[0]) {
        n = (size_t)st->size;
        str = cvalue_string(fl_ctx, n);
        st = value2c(ios_t*,*ps); // reload after allocating str
        memcpy(cvalue_data(str), st->buf, n);
        ios_trunc(st, 0);
    }
    else {
        char *b = ios_takebuf(st, &n); n--;
        b[n] = '\0';
        str = cvalue_from_ref(fl_ctx, fl_ctx->stringtype, b, n, fl_ctx->NIL);
        cv_autorelease(fl_ctx, (cvalue_t*)ptr(str));
    }
    return str;
}

value_t fl_iotostring(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "io.tostring!", nargs, 1);
    ios_t *src = toiostream(fl_ctx, args[0], "io.tostring!");
    if (src->bm != bm_mem)
        lerror(fl_ctx, fl_ctx->ArgError, "io.tostring!: requires memory stream");
    return stream_to_string(fl_ctx, &args[0]);
}

static const builtinspec_t iostreamfunc_info[] = {
    { "iostream?", fl_iostreamp },
    { "eof-object", fl_eof_object },
    { "eof-object?", fl_eof_objectp },
    { "file", fl_file },
    { "buffer", fl_buffer },
    { "read", fl_read },
    { "write", fl_write },
    { "io.flush", fl_ioflush },
    { "io.close", fl_ioclose },
    { "io.eof?" , fl_ioeof },
    { "io.seek" , fl_ioseek },
    { "io.pos",   fl_iopos },
    { "io.getc" , fl_iogetc },
    { "io.ungetc", fl_ioungetc },
    { "io.putc" , fl_ioputc },
    { "io.peekc" , fl_iopeekc },
    { "io.discardbuffer", fl_iopurge },
    { "io.read", fl_ioread },
    { "io.write", fl_iowrite },
    { "io.copy", fl_iocopy },
    { "io.readuntil", fl_ioreaduntil },
    { "io.copyuntil", fl_iocopyuntil },
    { "io.tostring!", fl_iotostring },
    { "input-port-line", fl_iolineno },

    { NULL, NULL }
};

void iostream_init(fl_context_t *fl_ctx)
{
    fl_ctx->iostreamsym = symbol(fl_ctx, "iostream");
    fl_ctx->rdsym = symbol(fl_ctx, ":read");
    fl_ctx->wrsym = symbol(fl_ctx, ":write");
    fl_ctx->apsym = symbol(fl_ctx, ":append");
    fl_ctx->crsym = symbol(fl_ctx, ":create");
    fl_ctx->truncsym = symbol(fl_ctx, ":truncate");
    fl_ctx->instrsym = symbol(fl_ctx, "*input-stream*");
    fl_ctx->outstrsym = symbol(fl_ctx, "*output-stream*");
    fl_ctx->iostreamtype = define_opaque_type(fl_ctx->iostreamsym, sizeof(ios_t),
                                              &iostream_vtable, NULL);
    assign_global_builtins(fl_ctx, iostreamfunc_info);

    setc(symbol(fl_ctx, "*stdout*"), cvalue_from_ref(fl_ctx, fl_ctx->iostreamtype, ios_stdout,
                                                     sizeof(ios_t), fl_ctx->NIL));
    setc(symbol(fl_ctx, "*stderr*"), cvalue_from_ref(fl_ctx, fl_ctx->iostreamtype, ios_stderr,
                                                     sizeof(ios_t), fl_ctx->NIL));
    setc(symbol(fl_ctx, "*stdin*" ), cvalue_from_ref(fl_ctx, fl_ctx->iostreamtype, ios_stdin,
                                                     sizeof(ios_t), fl_ctx->NIL));
}

#ifdef __cplusplus
}
#endif
