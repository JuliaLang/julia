// This file is a part of Julia. License is MIT: https://julialang.org/license

// Test driver for a juliac-compiled libjulia-frontend implementation.
//
// Loads the given frontend library as the process's Julia image and calls
// the jl_frontend_*_impl entry points through the same C ABI that the
// runtime's frontend trampolines use, printing results to stdout so they
// can be compared against the flisp reference frontend.
//
// Usage:
//   frontend_driver LIB smoke
//   frontend_driver LIB parse FILE         # parse with options=:all, print repr
//   frontend_driver LIB lower EXPR         # parse + lower a statement, print repr
//   frontend_driver LIB eval EXPR          # parse + lower + eval, print repr
//   frontend_driver LIB macroexpand EXPR
//   frontend_driver LIB ops FILE           # one candidate per line; prints TSV

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include <julia.h>

typedef jl_value_t *(*parse_fptr_t)(const char *, size_t, jl_value_t *, size_t, size_t, jl_value_t *);
typedef jl_value_t *(*lower_fptr_t)(jl_value_t *, jl_module_t *, const char *, int, size_t, int);
typedef jl_value_t *(*macroexpand_fptr_t)(jl_value_t *, jl_module_t *, int, int, int);
typedef void (*init_fptr_t)(void);
typedef int (*opquery_fptr_t)(const char *);

static void *lookup(void *handle, const char *name)
{
    void *p = dlsym(handle, name);
    if (p == NULL) {
        fprintf(stderr, "ERROR: missing symbol %s in frontend library\n", name);
        exit(2);
    }
    return p;
}

static char *read_file(const char *path, size_t *len)
{
    FILE *f = strcmp(path, "-") == 0 ? stdin : fopen(path, "rb");
    if (!f) {
        perror("fopen");
        exit(2);
    }
    size_t cap = 1 << 16, n = 0;
    char *buf = malloc(cap);
    size_t r;
    while ((r = fread(buf + n, 1, cap - n, f)) > 0) {
        n += r;
        if (n == cap) {
            cap *= 2;
            buf = realloc(buf, cap);
        }
    }
    if (f != stdin)
        fclose(f);
    *len = n;
    return buf;
}

// n.b. all output after jl_init goes through jl_printf: julia puts the
// stdio fds in non-blocking mode when they are pipes, which makes plain
// printf silently drop data on EAGAIN for large outputs
static void print_repr(jl_value_t *v)
{
    jl_value_t *str = NULL;
    JL_GC_PUSH2(&v, &str);
    jl_value_t *repr = jl_get_function(jl_base_module, "repr");
    str = jl_call1(repr, v);
    if (jl_exception_occurred()) {
        fprintf(stderr, "ERROR: repr failed\n");
        exit(3);
    }
    jl_printf(JL_STDOUT, "%s\n", jl_string_data(str));
    JL_GC_POP();
}

static void check_exception(const char *what)
{
    if (jl_exception_occurred()) {
        jl_value_t *exc = jl_current_exception(jl_current_task);
        jl_printf(jl_stderr_stream(), "ERROR: %s threw: ", what);
        jl_static_show(jl_stderr_stream(), exc);
        jl_printf(jl_stderr_stream(), "\n");
        exit(3);
    }
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "usage: %s LIB MODE [ARG]\n", argv[0]);
        return 2;
    }
    const char *libpath = argv[1];
    const char *mode = argv[2];

    void *handle = dlopen(libpath, RTLD_NOW | RTLD_GLOBAL);
    if (handle == NULL) {
        fprintf(stderr, "ERROR: dlopen(%s): %s\n", libpath, dlerror());
        return 2;
    }
    // Initialize Julia using the image embedded in the frontend library
    jl_init_with_image_handle(handle);

    init_fptr_t fe_init = (init_fptr_t)lookup(handle, "jl_frontend_init_impl");
    parse_fptr_t fe_parse = (parse_fptr_t)lookup(handle, "jl_frontend_parse_impl");
    lower_fptr_t fe_lower = (lower_fptr_t)lookup(handle, "jl_frontend_lower_impl");
    macroexpand_fptr_t fe_macroexpand = (macroexpand_fptr_t)lookup(handle, "jl_macroexpand_impl");
    fe_init();

    int ret = 0;
    if (strcmp(mode, "smoke") == 0) {
        const char *text = "f(x) = 2x + 1";
        jl_value_t *fname = NULL, *result = NULL, *expr = NULL;
        JL_GC_PUSH3(&fname, &result, &expr);
        fname = jl_cstr_to_string("none");
        result = fe_parse(text, strlen(text), fname, 1, 0,
                          (jl_value_t*)jl_symbol("all"));
        check_exception("parse");
        expr = jl_svecref(result, 0);
        print_repr(expr);
        result = fe_lower(expr, jl_main_module, "none", 1, ~(size_t)0, 0);
        check_exception("lower");
        print_repr(jl_svecref(result, 0));
        JL_GC_POP();
        jl_printf(JL_STDOUT, "SMOKE OK\n");
    }
    else if (strcmp(mode, "parse") == 0 || strcmp(mode, "lower") == 0 ||
             strcmp(mode, "eval") == 0 || strcmp(mode, "macroexpand") == 0) {
        if (argc < 4) {
            fprintf(stderr, "missing argument for mode %s\n", mode);
            return 2;
        }
        size_t len;
        char *text = strcmp(mode, "parse") == 0 ? read_file(argv[3], &len)
                                                : (len = strlen(argv[3]), strdup(argv[3]));
        jl_value_t *fname = NULL, *result = NULL, *expr = NULL;
        JL_GC_PUSH3(&fname, &result, &expr);
        fname = jl_cstr_to_string(strcmp(mode, "parse") == 0 ? argv[3] : "none");
        // `lower`/`macroexpand` demonstrate processing of a single statement;
        // `eval` parses a whole input (evaluation of :toplevel re-enters
        // lowering through Core._lower for each statement, which this image
        // routes to JuliaLowering as well)
        const char *rule = strcmp(mode, "parse") == 0 || strcmp(mode, "eval") == 0
                           ? "all" : "statement";
        result = fe_parse(text, len, fname, 1, 0, (jl_value_t*)jl_symbol(rule));
        check_exception("parse");
        expr = jl_svecref(result, 0);
        if (strcmp(mode, "parse") == 0) {
            print_repr(expr);
        }
        else if (strcmp(mode, "macroexpand") == 0) {
            expr = fe_macroexpand(expr, jl_main_module, 1, 0, 1);
            check_exception("macroexpand");
            print_repr(expr);
        }
        else {
            result = fe_lower(expr, jl_main_module, "none", 1, ~(size_t)0, 0);
            check_exception("lower");
            expr = jl_svecref(result, 0);
            if (strcmp(mode, "lower") == 0) {
                print_repr(expr);
            }
            else {
                expr = jl_toplevel_eval(jl_main_module, expr);
                check_exception("eval");
                print_repr(expr);
            }
        }
        JL_GC_POP();
        free(text);
    }
    else if (strcmp(mode, "ops") == 0) {
        if (argc < 4) {
            fprintf(stderr, "missing candidates file\n");
            return 2;
        }
        opquery_fptr_t is_op = (opquery_fptr_t)lookup(handle, "jl_is_operator_impl");
        opquery_fptr_t is_un = (opquery_fptr_t)lookup(handle, "jl_is_unary_operator_impl");
        opquery_fptr_t is_ub = (opquery_fptr_t)lookup(handle, "jl_is_unary_and_binary_operator_impl");
        opquery_fptr_t is_syn = (opquery_fptr_t)lookup(handle, "jl_is_syntactic_operator_impl");
        opquery_fptr_t prec = (opquery_fptr_t)lookup(handle, "jl_operator_precedence_impl");
        size_t len;
        char *data = read_file(argv[3], &len);
        char *line = strtok(data, "\n");
        while (line != NULL) {
            if (*line != '\0') {
                jl_printf(JL_STDOUT, "%s\t%d\t%d\t%d\t%d\t%d\n", line, is_op(line), is_un(line),
                          is_ub(line), is_syn(line), prec(line));
            }
            line = strtok(NULL, "\n");
        }
        free(data);
    }
    else {
        fprintf(stderr, "unknown mode %s\n", mode);
        ret = 2;
    }

    jl_atexit_hook(ret);
    return ret;
}
