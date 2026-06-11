// This file is a part of Julia. License is MIT: https://julialang.org/license

// Two-runtimes-in-one-process test: initialize a normal host julia, then
// load the self-contained standalone frontend library and exercise its
// jl_frontend_*_impl ABI with *host* values, validating the results
// against the host's own parser.

#include <stdio.h>
#include <string.h>
#include <dlfcn.h>

#include <julia.h>

// internal-but-exported host entry point (declared in julia_internal.h)
extern jl_value_t *jl_parse(const char *text, size_t text_len, jl_value_t *filename,
                            size_t lineno, size_t offset, jl_value_t *options,
                            jl_module_t *inmodule);

typedef jl_value_t *(*parse_fptr_t)(const char *, size_t, jl_value_t *, size_t, size_t, jl_value_t *);
typedef jl_value_t *(*lower_fptr_t)(jl_value_t *, jl_module_t *, const char *, int, size_t, int);
typedef int (*opquery_fptr_t)(const char *);
typedef void (*init_fptr_t)(void);

static int nfail = 0;

static void check(const char *name, int cond)
{
    printf("%s %s\n", cond ? "PASS" : "FAIL", name);
    if (!cond)
        nfail++;
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "usage: %s LIBJULIA_FRONTEND_STANDALONE\n", argv[0]);
        return 2;
    }

    // Host runtime
    jl_init();

    // Guest frontend runtime
    // RTLD_LOCAL keeps the guest runtime's symbols out of the global scope
    // (the host can never bind to them); RTLD_DEEPBIND makes the guest
    // prefer its own symbols over the host's same-named globals.
    void *handle = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL | RTLD_DEEPBIND);
    if (handle == NULL) {
        fprintf(stderr, "ERROR: dlopen(%s): %s\n", argv[1], dlerror());
        return 2;
    }
    init_fptr_t fe_init = (init_fptr_t)dlsym(handle, "jl_frontend_init_impl");
    parse_fptr_t fe_parse = (parse_fptr_t)dlsym(handle, "jl_frontend_parse_impl");
    lower_fptr_t fe_lower = (lower_fptr_t)dlsym(handle, "jl_frontend_lower_impl");
    opquery_fptr_t fe_isop = (opquery_fptr_t)dlsym(handle, "jl_is_operator_impl");
    opquery_fptr_t fe_prec = (opquery_fptr_t)dlsym(handle, "jl_operator_precedence_impl");
    if (!fe_init || !fe_parse || !fe_lower || !fe_isop || !fe_prec) {
        fprintf(stderr, "ERROR: missing ABI symbols in %s\n", argv[1]);
        return 2;
    }
    fe_init(); // boots the embedded second runtime

    // --- parse through the guest runtime, with host values ---
    const char *tests[] = {
        "f(x) = 2x + 1",
        "for i in 1:10\n    s += i^2.5\nend",
        "[a for a in xs if a > 0x1f]",
        "x = \"str\\n\" * 'c'",
        "module M\nstruct P{T}\n    x::T\nend\nend",
        "a .+= b' - c\"lit\"foo",
    };
    for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
        const char *text = tests[i];
        jl_value_t *fname = NULL, *result = NULL, *expected = NULL, *got = NULL;
        JL_GC_PUSH4(&fname, &result, &expected, &got);
        fname = jl_cstr_to_string("none");
        result = fe_parse(text, strlen(text), fname, 1, 0, (jl_value_t *)jl_symbol("all"));
        if (jl_exception_occurred()) {
            jl_static_show(jl_stderr_stream(), jl_current_exception(jl_current_task));
            jl_printf(jl_stderr_stream(), "\n");
            check("guest parse (exception)", 0);
            JL_GC_POP();
            continue;
        }
        got = jl_svecref(result, 0);
        // reference: the host's own parser on the same input
        expected = jl_svecref(jl_parse(text, strlen(text), fname, 1, 0,
                                       (jl_value_t *)jl_symbol("all"), NULL), 0);
        char name[128];
        snprintf(name, sizeof(name), "cross-runtime parse %zu", i + 1);
        // n.b. Expr is mutable, so egal is identity; compare structurally
        jl_value_t *eqf = jl_get_function(jl_base_module, "==");
        jl_value_t *eq = jl_call2(eqf, got, expected);
        int equal = eq != NULL && jl_unbox_bool(eq);
        check(name, equal);
        if (!equal) {
            jl_printf(jl_stderr_stream(), "  got:      ");
            jl_static_show(jl_stderr_stream(), got);
            jl_printf(jl_stderr_stream(), "\n  expected: ");
            jl_static_show(jl_stderr_stream(), expected);
            jl_printf(jl_stderr_stream(), "\n");
        }
        JL_GC_POP();
    }

    // EOF returns svec(nothing, len)
    {
        jl_value_t *fname = jl_cstr_to_string("none");
        JL_GC_PUSH1(&fname);
        jl_value_t *result = fe_parse("  ", 2, fname, 1, 0, (jl_value_t *)jl_symbol("statement"));
        check("cross-runtime parse EOF -> nothing", jl_svecref(result, 0) == jl_nothing);
        JL_GC_POP();
    }

    // Parse errors come back as host Expr(:error, ...) / Expr(:incomplete, ...)
    {
        jl_value_t *fname = jl_cstr_to_string("none");
        JL_GC_PUSH1(&fname);
        jl_value_t *result = fe_parse("1 +", 3, fname, 1, 0, (jl_value_t *)jl_symbol("statement"));
        jl_value_t *ex = jl_svecref(result, 0);
        check("cross-runtime parse incomplete",
              jl_is_expr(ex) && ((jl_expr_t *)ex)->head == jl_symbol("incomplete"));
        JL_GC_POP();
    }

    // --- cross-runtime lowering: lower host exprs in the guest, then
    // evaluate the returned thunk in the host ---
    {
        // n.b. no closures or generators here: JuliaLowering currently
        // embeds eagerly-created closure type objects in its output, which
        // cannot cross the runtime boundary (needs lowering to emit those
        // definitions as code; see README)
        const struct { const char *code; int64_t expected; } lowers[] = {
            {"1 + 2", 3},
            {"let s = 0\n    for i = 1:10\n        s += i\n    end\n    s\nend", 55},
            {"begin\n    a = [1, 2, 3]\n    a[1] + a[3] * 10\nend", 31},
            {"begin\n    t = try\n        error(\"x\")\n        1\n    catch\n        2\n    end\n    t + 40\nend", 42},
            {"begin\n    x = 7\n    if x > 3\n        x = x * 6\n    else\n        x = 0\n    end\n    x\nend", 42},
            {"let g = 0\n    while g < 5\n        g += 1\n    end\n    g\nend", 5},
        };
        for (size_t i = 0; i < sizeof(lowers) / sizeof(lowers[0]); i++) {
            const char *code = lowers[i].code;
            char name[64];
            snprintf(name, sizeof(name), "cross-runtime lower+eval %zu", i + 1);
            jl_value_t *fname = NULL, *expr = NULL, *lowered = NULL, *val = NULL;
            JL_GC_PUSH4(&fname, &expr, &lowered, &val);
            JL_TRY {
                fname = jl_cstr_to_string("none");
                expr = jl_svecref(jl_parse(code, strlen(code), fname, 1, 0,
                                           (jl_value_t *)jl_symbol("statement"), NULL), 0);
                lowered = fe_lower(expr, jl_main_module, "none", 1, (size_t)-1, 0);
                val = jl_toplevel_eval(jl_main_module, jl_svecref(lowered, 0));
                int ok = jl_is_int64(val) && jl_unbox_int64(val) == lowers[i].expected;
                check(name, ok);
                if (!ok) {
                    jl_printf(jl_stderr_stream(), "  value: ");
                    jl_static_show(jl_stderr_stream(), val);
                    jl_printf(jl_stderr_stream(), "\n  lowered: ");
                    jl_static_show(jl_stderr_stream(), jl_svecref(lowered, 0));
                    jl_printf(jl_stderr_stream(), "\n");
                }
            }
            JL_CATCH {
                jl_static_show(jl_stderr_stream(), jl_current_exception(jl_current_task));
                jl_printf(jl_stderr_stream(), "\n");
                check(name, 0);
            }
            JL_GC_POP();
        }
        // macro calls are rejected with a clear error for now
        {
            int threw = 0;
            jl_value_t *fname = NULL, *expr = NULL;
            JL_GC_PUSH2(&fname, &expr);
            JL_TRY {
                fname = jl_cstr_to_string("none");
                const char *mc = "@assert true";
                expr = jl_svecref(jl_parse(mc, strlen(mc), fname, 1, 0,
                                           (jl_value_t *)jl_symbol("statement"), NULL), 0);
                fe_lower(expr, jl_main_module, "none", 1, (size_t)-1, 0);
            }
            JL_CATCH {
                threw = 1;
                jl_exception_clear();
            }
            check("cross-runtime lower macrocall rejected", threw);
            JL_GC_POP();
        }
    }

    // --- operator queries: guest answers must match the host frontend ---
    {
        const char *ops[] = {"+", ".+", "+=", "&&", "::", "<:", "~", "in", "where",
                             "⊕", "√", "im", "foo", "...", "->", "==", "⊻="};
        int bad = 0;
        for (size_t i = 0; i < sizeof(ops) / sizeof(ops[0]); i++) {
            if (fe_isop(ops[i]) != jl_is_operator(ops[i]) ||
                fe_prec(ops[i]) != jl_operator_precedence(ops[i])) {
                fprintf(stderr, "  op mismatch: %s\n", ops[i]);
                bad++;
            }
        }
        check("cross-runtime operator queries", bad == 0);
    }

    printf(nfail == 0 ? "HOST DRIVER: ALL PASSED\n" : "HOST DRIVER: %d FAILURES\n", nfail);
    jl_atexit_hook(nfail == 0 ? 0 : 1);
    return nfail == 0 ? 0 : 1;
}
