#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <setjmp.h>
#include "flisp.h"

static value_t iostreamsym, rdsym, wrsym, apsym, crsym, truncsym;
static value_t instrsym, outstrsym;
fltype_t *iostreamtype;

void print_iostream(value_t v, ios_t *f)
{
    (void)v;
    fl_print_str("#<io stream>", f);
}

void free_iostream(value_t self)
{
    ios_t *s = value2c(ios_t*, self);
    ios_close(s);
}

void relocate_iostream(value_t oldv, value_t newv)
{
    ios_t *olds = value2c(ios_t*, oldv);
    ios_t *news = value2c(ios_t*, newv);
    cvalue_t *cv = (cvalue_t*)ptr(oldv);
    if (isinlined(cv)) {
        if (olds->buf == &olds->local[0]) {
            news->buf = &news->local[0];
        }
    }
}

cvtable_t iostream_vtable = { print_iostream, relocate_iostream,
                              free_iostream, NULL };

int fl_isiostream(value_t v)
{
    return iscvalue(v) && cv_class((cvalue_t*)ptr(v)) == iostreamtype;
}

value_t fl_iostreamp(value_t *args, uint32_t nargs)
{
    argcount("iostream?", nargs, 1);
    return fl_isiostream(args[0]) ? FL_T : FL_F;
}

value_t fl_eof_object(value_t *args, uint32_t nargs)
{
    (void)args;
    argcount("eof-object", nargs, 0);
    return FL_EOF;
}

value_t fl_eof_objectp(value_t *args, uint32_t nargs)
{
    argcount("eof-object?", nargs, 1);
    return (FL_EOF == args[0]) ? FL_T : FL_F;
}

static ios_t *toiostream(value_t v, char *fname)
{
    if (!fl_isiostream(v))
        type_error(fname, "iostream", v);
    return value2c(ios_t*, v);
}

ios_t *fl_toiostream(value_t v, char *fname)
{
    return toiostream(v, fname);
}

value_t fl_file(value_t *args, uint32_t nargs)
{
    if (nargs < 1)
        argcount("file", nargs, 1);
    int i, r=0, w=0, c=0, t=0, a=0;
    for(i=1; i < (int)nargs; i++) {
        if      (args[i] == wrsym)    w = 1;
        else if (args[i] == apsym)    { a = 1; w = 1; }
        else if (args[i] == crsym)    { c = 1; w = 1; }
        else if (args[i] == truncsym) { t = 1; w = 1; }
        else if (args[i] == rdsym)    r = 1;
    }
    if ((r|w|c|t|a) == 0) r = 1;  // default to reading
    value_t f = cvalue(iostreamtype, sizeof(ios_t));
    char *fname = tostring(args[0], "file");
    ios_t *s = value2c(ios_t*, f);
    if (ios_file(s, fname, r, w, c, t) == NULL)
        lerrorf(IOError, "file: could not open \"%s\"", fname);
    if (a) ios_seek_end(s);
    return f;
}

value_t fl_buffer(value_t *args, u_int32_t nargs)
{
    argcount("buffer", nargs, 0);
    (void)args;
    value_t f = cvalue(iostreamtype, sizeof(ios_t));
    ios_t *s = value2c(ios_t*, f);
    if (ios_mem(s, 0) == NULL)
        lerror(MemoryError, "buffer: could not allocate stream");
    return f;
}

value_t fl_read(value_t *args, u_int32_t nargs)
{
    value_t arg;
    if (nargs > 1) {
        argcount("read", nargs, 1);
    }
    else if (nargs == 0) {
        arg = symbol_value(instrsym);
    }
    else {
        arg = args[0];
    }
    (void)toiostream(arg, "read");
    fl_gc_handle(&arg);
    value_t v = fl_read_sexpr(arg);
    fl_free_gc_handles(1);
    if (ios_eof(value2c(ios_t*,arg)))
        return FL_EOF;
    return v;
}

value_t fl_iogetc(value_t *args, u_int32_t nargs)
{
    argcount("io.getc", nargs, 1);
    ios_t *s = toiostream(args[0], "io.getc");
    uint32_t wc;
    if (ios_getutf8(s, &wc) == IOS_EOF)
        //lerror(IOError, "io.getc: end of file reached");
        return FL_EOF;
    return mk_wchar(wc);
}

value_t fl_iopeekc(value_t *args, u_int32_t nargs)
{
    argcount("io.peekc", nargs, 1);
    ios_t *s = toiostream(args[0], "io.peekc");
    uint32_t wc;
    if (ios_peekutf8(s, &wc) == IOS_EOF)
        return FL_EOF;
    return mk_wchar(wc);
}

value_t fl_ioputc(value_t *args, u_int32_t nargs)
{
    argcount("io.putc", nargs, 2);
    ios_t *s = toiostream(args[0], "io.putc");
    if (!iscprim(args[1]) || ((cprim_t*)ptr(args[1]))->type != wchartype)
        type_error("io.putc", "wchar", args[1]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[1]));
    return fixnum(ios_pututf8(s, wc));
}

value_t fl_ioungetc(value_t *args, u_int32_t nargs)
{
    argcount("io.ungetc", nargs, 2);
    ios_t *s = toiostream(args[0], "io.ungetc");
    if (!iscprim(args[1]) || ((cprim_t*)ptr(args[1]))->type != wchartype)
        type_error("io.ungetc", "wchar", args[1]);
    uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[1]));
    if (wc >= 0x80) {
        lerror(ArgError, "io_ungetc: unicode not yet supported");
    }
    return fixnum(ios_ungetc((int)wc,s));
}

value_t fl_ioflush(value_t *args, u_int32_t nargs)
{
    argcount("io.flush", nargs, 1);
    ios_t *s = toiostream(args[0], "io.flush");
    if (ios_flush(s) != 0)
        return FL_F;
    return FL_T;
}

value_t fl_ioclose(value_t *args, u_int32_t nargs)
{
    argcount("io.close", nargs, 1);
    ios_t *s = toiostream(args[0], "io.close");
    ios_close(s);
    return FL_T;
}

value_t fl_iopurge(value_t *args, u_int32_t nargs)
{
    argcount("io.discardbuffer", nargs, 1);
    ios_t *s = toiostream(args[0], "io.discardbuffer");
    ios_purge(s);
    return FL_T;
}

value_t fl_ioeof(value_t *args, u_int32_t nargs)
{
    argcount("io.eof?", nargs, 1);
    ios_t *s = toiostream(args[0], "io.eof?");
    return (ios_eof(s) ? FL_T : FL_F);
}

value_t fl_iolineno(value_t *args, u_int32_t nargs)
{
    argcount("input-port-line", nargs, 1);
    ios_t *s = toiostream(args[0], "input-port-line");
    return size_wrap(s->lineno);
}

value_t fl_ioseek(value_t *args, u_int32_t nargs)
{
    argcount("io.seek", nargs, 2);
    ios_t *s = toiostream(args[0], "io.seek");
    size_t pos = tosize(args[1], "io.seek");
    off_t res = ios_seek(s, (off_t)pos);
    if (res == -1)
        return FL_F;
    return FL_T;
}

value_t fl_iopos(value_t *args, u_int32_t nargs)
{
    argcount("io.pos", nargs, 1);
    ios_t *s = toiostream(args[0], "io.pos");
    off_t res = ios_pos(s);
    if (res == -1)
        return FL_F;
    return size_wrap((size_t)res);
}

value_t fl_write(value_t *args, u_int32_t nargs)
{
    if (nargs < 1 || nargs > 2)
        argcount("write", nargs, 1);
    ios_t *s;
    if (nargs == 2)
        s = toiostream(args[1], "write");
    else
        s = toiostream(symbol_value(outstrsym), "write");
    fl_print(s, args[0]);
    return args[0];
}

value_t fl_ioread(value_t *args, u_int32_t nargs)
{
    if (nargs != 3)
        argcount("io.read", nargs, 2);
    (void)toiostream(args[0], "io.read");
    size_t n;
    fltype_t *ft;
    if (nargs == 3) {
        // form (io.read s type count)
        ft = get_array_type(args[1]);
        n = tosize(args[2], "io.read") * ft->elsz;
    }
    else {
        ft = get_type(args[1]);
        if (ft->eltype != NULL && !iscons(cdr_(cdr_(args[1]))))
            lerror(ArgError, "io.read: incomplete type");
        n = ft->size;
    }
    value_t cv = cvalue(ft, n);
    char *data;
    if (iscvalue(cv)) data = cv_data((cvalue_t*)ptr(cv));
    else data = cp_data((cprim_t*)ptr(cv));
    size_t got = ios_read(value2c(ios_t*,args[0]), data, n);
    if (got < n)
        //lerror(IOError, "io.read: end of input reached");
        return FL_EOF;
    return cv;
}

// args must contain data[, offset[, count]]
static void get_start_count_args(value_t *args, uint32_t nargs, size_t sz,
                                 size_t *offs, size_t *nb, char *fname)
{
    if (nargs > 1) {
        *offs = tosize(args[1], fname);
        if (nargs > 2)
            *nb = tosize(args[2], fname);
        else
            *nb = sz - *offs;
        if (*offs >= sz || *offs + *nb > sz)
            bounds_error(fname, args[0], args[1]);
    }
}

value_t fl_iowrite(value_t *args, u_int32_t nargs)
{
    if (nargs < 2 || nargs > 4)
        argcount("io.write", nargs, 2);
    ios_t *s = toiostream(args[0], "io.write");
    if (iscprim(args[1]) && ((cprim_t*)ptr(args[1]))->type == wchartype) {
        if (nargs > 2)
            lerror(ArgError,
                   "io.write: offset argument not supported for characters");
        uint32_t wc = *(uint32_t*)cp_data((cprim_t*)ptr(args[1]));
        return fixnum(ios_pututf8(s, wc));
    }
    char *data;
    size_t sz, offs=0;
    to_sized_ptr(args[1], "io.write", &data, &sz);
    size_t nb = sz;
    if (nargs > 2) {
        get_start_count_args(&args[1], nargs-1, sz, &offs, &nb, "io.write");
        data += offs;
    }
    return size_wrap(ios_write(s, data, nb));
}

static char get_delim_arg(value_t arg, char *fname)
{
    size_t uldelim = tosize(arg, fname);
    if (uldelim > 0x7f) {
        // wchars > 0x7f, or anything else > 0xff, are out of range
        if ((iscprim(arg) && cp_class((cprim_t*)ptr(arg))==wchartype) ||
            uldelim > 0xff)
            lerrorf(ArgError, "%s: delimiter out of range", fname);
    }
    return (char)uldelim;
}

value_t fl_ioreaduntil(value_t *args, u_int32_t nargs)
{
    argcount("io.readuntil", nargs, 2);
    value_t str = cvalue_string(80);
    cvalue_t *cv = (cvalue_t*)ptr(str);
    char *data = cv_data(cv);
    ios_t dest;
    ios_mem(&dest, 0);
    ios_setbuf(&dest, data, 80, 0);
    char delim = get_delim_arg(args[1], "io.readuntil");
    ios_t *src = toiostream(args[0], "io.readuntil");
    size_t n = ios_copyuntil(&dest, src, delim);
    cv->len = n;
    if (dest.buf != data) {
        // outgrew initial space
        cv->data = dest.buf;
        cv_autorelease(cv);
    }
    ((char*)cv->data)[n] = '\0';
    if (n == 0 && ios_eof(src))
        return FL_EOF;
    return str;
}

value_t fl_iocopyuntil(value_t *args, u_int32_t nargs)
{
    argcount("io.copyuntil", nargs, 3);
    ios_t *dest = toiostream(args[0], "io.copyuntil");
    ios_t *src = toiostream(args[1], "io.copyuntil");
    char delim = get_delim_arg(args[2], "io.copyuntil");
    return size_wrap(ios_copyuntil(dest, src, delim));
}

value_t fl_iocopy(value_t *args, u_int32_t nargs)
{
    if (nargs < 2 || nargs > 3)
        argcount("io.copy", nargs, 2);
    ios_t *dest = toiostream(args[0], "io.copy");
    ios_t *src = toiostream(args[1], "io.copy");
    if (nargs == 3) {
        size_t n = tosize(args[2], "io.copy");
        return size_wrap(ios_copy(dest, src, n));
    }
    return size_wrap(ios_copyall(dest, src));
}

value_t stream_to_string(value_t *ps)
{
    value_t str;
    size_t n;
    ios_t *st = value2c(ios_t*,*ps);
    if (st->buf == &st->local[0]) {
        n = st->size;
        str = cvalue_string(n);
        memcpy(cvalue_data(str), value2c(ios_t*,*ps)->buf, n);
        ios_trunc(st, 0);
    }
    else {
        char *b = ios_takebuf(st, &n); n--;
        b[n] = '\0';
        str = cvalue_from_ref(stringtype, b, n, FL_NIL);
        cv_autorelease((cvalue_t*)ptr(str));
    }
    return str;
}

value_t fl_iotostring(value_t *args, u_int32_t nargs)
{
    argcount("io.tostring!", nargs, 1);
    ios_t *src = toiostream(args[0], "io.tostring!");
    if (src->bm != bm_mem)
        lerror(ArgError, "io.tostring!: requires memory stream");
    return stream_to_string(&args[0]);
}

static builtinspec_t iostreamfunc_info[] = {
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

void iostream_init(void)
{
    iostreamsym = symbol("iostream");
    rdsym = symbol(":read");
    wrsym = symbol(":write");
    apsym = symbol(":append");
    crsym = symbol(":create");
    truncsym = symbol(":truncate");
    instrsym = symbol("*input-stream*");
    outstrsym = symbol("*output-stream*");
    iostreamtype = define_opaque_type(iostreamsym, sizeof(ios_t),
                                      &iostream_vtable, NULL);
    assign_global_builtins(iostreamfunc_info);

    setc(symbol("*stdout*"), cvalue_from_ref(iostreamtype, ios_stdout,
                                             sizeof(ios_t), FL_NIL));
    setc(symbol("*stderr*"), cvalue_from_ref(iostreamtype, ios_stderr,
                                             sizeof(ios_t), FL_NIL));
    setc(symbol("*stdin*" ), cvalue_from_ref(iostreamtype, ios_stdin,
                                             sizeof(ios_t), FL_NIL));
}
