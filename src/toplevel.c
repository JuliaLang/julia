// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  evaluating top-level expressions, loading source files
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <sys/types.h>
#include <errno.h>
#if defined(_OS_WINDOWS_)
#include <malloc.h>
#else
#include <unistd.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"
#include "intrinsics.h"
#include "builtin_proto.h"

#ifdef __cplusplus
extern "C" {
#endif

// current line number in a file
JL_DLLEXPORT int jl_lineno = 0; // need to update jl_critical_error if this is TLS
// current file name
JL_DLLEXPORT const char *jl_filename = "none"; // need to update jl_critical_error if this is TLS

htable_t jl_current_modules;
jl_mutex_t jl_modules_mutex;

// During incremental compilation, the following gets set
jl_module_t *jl_precompile_toplevel_module = NULL;   // the toplevel module currently being defined

jl_module_t *jl_add_standard_imports(jl_module_t *m)
{
    jl_module_t *base_module = jl_base_relative_to(m);
    assert(base_module != NULL);
    // using Base
    jl_module_initial_using(m, base_module);
    return base_module;
}

// create a new top-level module
void jl_init_main_module(void)
{
    assert(jl_main_module == NULL);
    jl_main_module = jl_new_module_(jl_symbol("Main"), NULL, 0, 1); // baremodule Main; end
    jl_set_initial_const(jl_core_module, jl_symbol("Main"), (jl_value_t*)jl_main_module, 0); // const Main.Core = Core
    jl_set_initial_const(jl_main_module, jl_symbol("Core"), (jl_value_t*)jl_core_module, 0); // const Core.Main = Main
}

static jl_function_t *jl_module_get_initializer(jl_module_t *m JL_PROPAGATES_ROOT)
{
    return (jl_function_t*)jl_get_global(m, jl_symbol("__init__"));
}


void jl_module_run_initializer(jl_module_t *m)
{
    JL_TIMING(INIT_MODULE, INIT_MODULE);
    jl_timing_show_module(m, JL_TIMING_DEFAULT_BLOCK);
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    JL_TRY {
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        jl_function_t *f = jl_module_get_initializer(m);
        if (f != NULL)
            jl_apply(&f, 1);
        ct->world_age = last_age;
    }
    JL_CATCH {
        if (jl_initerror_type == NULL) {
            jl_rethrow();
        }
        else {
            jl_rethrow_other(jl_new_struct(jl_initerror_type, m->name,
                                           jl_current_exception(ct)));
        }
    }
}

static void jl_register_root_module(jl_module_t *m)
{
    static jl_value_t *register_module_func = NULL;
    assert(jl_base_module);
    if (register_module_func == NULL)
        register_module_func = jl_get_global(jl_base_module, jl_symbol("register_root_module"));
    assert(register_module_func);
    jl_value_t *args[2];
    args[0] = register_module_func;
    args[1] = (jl_value_t*)m;
    jl_apply(args, 2);
}

jl_array_t *jl_get_loaded_modules(void)
{
    static jl_value_t *loaded_modules_array = NULL;
    if (loaded_modules_array == NULL && jl_base_module != NULL)
        loaded_modules_array = jl_get_global(jl_base_module, jl_symbol("loaded_modules_array"));
    if (loaded_modules_array != NULL)
        return (jl_array_t*)jl_call0((jl_function_t*)loaded_modules_array);
    return NULL;
}

static int jl_is__toplevel__mod(jl_module_t *mod)
{
    return jl_base_module &&
        (jl_value_t*)mod == jl_get_global(jl_base_module, jl_symbol("__toplevel__"));
}

// TODO: add locks around global state mutation operations
static jl_value_t *jl_eval_module_expr(jl_module_t *parent_module, jl_expr_t *ex)
{
    jl_task_t *ct = jl_current_task;
    assert(ex->head == jl_module_sym);
    if (jl_array_nrows(ex->args) != 3 || !jl_is_expr(jl_exprarg(ex, 2))) {
        jl_error("syntax: malformed module expression");
    }

    if (((jl_expr_t *)(jl_exprarg(ex, 2)))->head != jl_symbol("block")) {
        jl_error("syntax: module expression third argument must be a block");
    }

    int std_imports = (jl_exprarg(ex, 0) == jl_true);
    jl_sym_t *name = (jl_sym_t*)jl_exprarg(ex, 1);
    if (!jl_is_symbol(name)) {
        jl_type_error("module", (jl_value_t*)jl_symbol_type, (jl_value_t*)name);
    }

    int is_parent__toplevel__ = jl_is__toplevel__mod(parent_module);
    // If we have `Base`, don't also try to import `Core` - the `Base` exports are a superset.
    // While we allow multiple imports of the same binding from different modules, various error printing
    // performs reflection on which module a binding came from and we'd prefer users see "Base" here.
    jl_module_t *newm = jl_new_module_(name, is_parent__toplevel__ ? NULL : parent_module, std_imports && jl_base_module != NULL ? 0 : 1, 1);
    jl_value_t *form = (jl_value_t*)newm;
    JL_GC_PUSH1(&form);
    JL_LOCK(&jl_modules_mutex);
    ptrhash_put(&jl_current_modules, (void*)newm, (void*)((uintptr_t)HT_NOTFOUND + 1));
    JL_UNLOCK(&jl_modules_mutex);
    // copy parent environment info into submodule
    newm->uuid = parent_module->uuid;
    jl_array_t *exprs = ((jl_expr_t*)jl_exprarg(ex, 2))->args;
    int lineno = 0;
    const char *filename = "none";
    if (jl_array_nrows(exprs) > 0) {
        jl_value_t *lineex = jl_array_ptr_ref(exprs, 0);
        if (jl_is_linenode(lineex)) {
            lineno = jl_linenode_line(lineex);
            jl_value_t *file = jl_linenode_file(lineex);
            if (jl_is_symbol(file))
                filename = jl_symbol_name((jl_sym_t*)file);
        }
    }
    newm->file = jl_symbol(filename);
    jl_gc_wb_knownold(newm, newm->file);
    newm->line = lineno;

    // add standard imports unless baremodule
    if (std_imports && jl_base_module != NULL) {
        jl_module_t *base = jl_add_standard_imports(newm);
        jl_datatype_t *include_into = (jl_datatype_t *)jl_get_global(base, jl_symbol("IncludeInto"));
        if (include_into) {
            form = jl_new_struct(include_into, newm);
            jl_set_initial_const(newm, jl_symbol("include"), form, 0);
        }
        jl_datatype_t *eval_into = (jl_datatype_t *)jl_get_global(jl_core_module, jl_symbol("EvalInto"));
        if (eval_into) {
            form = jl_new_struct(eval_into, newm);
            jl_set_initial_const(newm, jl_symbol("eval"), form, 0);
        }
    }

    jl_module_t *old_toplevel_module = jl_precompile_toplevel_module;
    size_t last_age = ct->world_age;

    if (parent_module == jl_main_module && name == jl_symbol("Base") && jl_base_module == NULL) {
        // pick up Base module during bootstrap
        jl_base_module = newm;
    }

    if (is_parent__toplevel__) {
        jl_register_root_module(newm);
        if (jl_options.incremental) {
            jl_precompile_toplevel_module = newm;
        }
    }
    else {
        jl_declare_constant_val(NULL, parent_module, name, (jl_value_t*)newm);
    }

    for (int i = 0; i < jl_array_nrows(exprs); i++) {
        // process toplevel form
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        form = jl_expand_stmt_with_loc(jl_array_ptr_ref(exprs, i), newm, filename, lineno);
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        (void)jl_toplevel_eval_flex(newm, form, 1, 1, &filename, &lineno);
    }
    ct->world_age = last_age;

    JL_LOCK(&jl_modules_mutex);
    uintptr_t *refcnt = (uintptr_t*)ptrhash_bp(&jl_current_modules, (void*)newm);
    assert(*refcnt > (uintptr_t)HT_NOTFOUND);
    *refcnt -= 1;
    // newm should be reachable from somewhere else by now

    if (jl_module_init_order == NULL)
        jl_module_init_order = jl_alloc_vec_any(0);
    jl_array_ptr_1d_push(jl_module_init_order, (jl_value_t*)newm);

    // defer init of children until parent is done being defined
    // then initialize all in definition-finished order
    // at build time, don't run them at all (defer for runtime)
    form = NULL;
    if (!jl_generating_output()) {
        if (!ptrhash_has(&jl_current_modules, (void*)newm->parent)) {
            size_t i, l = jl_array_nrows(jl_module_init_order);
            size_t ns = 0;
            form = (jl_value_t*)jl_alloc_vec_any(0);
            for (i = 0; i < l; i++) {
                jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(jl_module_init_order, i);
                if (jl_is_submodule(m, newm)) {
                    jl_array_ptr_1d_push((jl_array_t*)form, (jl_value_t*)m);
                }
                else if (ns++ != i) {
                    jl_array_ptr_set(jl_module_init_order, ns - 1, (jl_value_t*)m);
                }
            }
            if (ns < l)
                jl_array_del_end(jl_module_init_order, l - ns);
        }
    }
    JL_UNLOCK(&jl_modules_mutex);

    if (form) {
        size_t i, l = jl_array_nrows(form);
        for (i = 0; i < l; i++) {
            jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(form, i);
            JL_GC_PROMISE_ROOTED(m);
            jl_module_run_initializer(m);
        }
    }

    jl_precompile_toplevel_module = old_toplevel_module;

    JL_GC_POP();
    return (jl_value_t*)newm;
}

static jl_value_t *jl_eval_dot_expr(jl_module_t *m, jl_value_t *x, jl_value_t *f, int fast, const char **toplevel_filename, int *toplevel_lineno)
{
    jl_task_t *ct = jl_current_task;
    jl_value_t **args;
    JL_GC_PUSHARGS(args, 3);
    args[1] = jl_toplevel_eval_flex(m, x, fast, 0, toplevel_filename, toplevel_lineno);
    args[2] = jl_toplevel_eval_flex(m, f, fast, 0, toplevel_filename, toplevel_lineno);
    if (jl_is_module(args[1])) {
        JL_TYPECHK(getglobal, symbol, args[2]);
        args[0] = jl_eval_global_var((jl_module_t*)args[1], (jl_sym_t*)args[2]);
    }
    else {
        args[0] = jl_eval_global_var(jl_base_relative_to(m), jl_symbol("getproperty"));
        size_t last_age = ct->world_age;
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        args[0] = jl_apply(args, 3);
        ct->world_age = last_age;
    }
    JL_GC_POP();
    return args[0];
}

extern void check_safe_newbinding(jl_module_t *m, jl_sym_t *var);
void jl_declare_global(jl_module_t *m, jl_value_t *arg, jl_value_t *set_type, int strong) {
    // create uninitialized mutable binding for "global x" decl sometimes or probably
    jl_module_t *gm;
    jl_sym_t *gs;
    assert(!jl_is_expr(arg)); // Should have been resolved before this
    if (jl_is_globalref(arg)) {
        gm = jl_globalref_mod(arg);
        gs = jl_globalref_name(arg);
    }
    else {
        assert(jl_is_symbol(arg));
        gm = m;
        gs = (jl_sym_t*)arg;
    }
    JL_LOCK(&world_counter_lock);
    size_t new_world = jl_atomic_load_relaxed(&jl_world_counter) + 1;
    jl_binding_t *b = jl_get_module_binding(gm, gs, 1);
    jl_binding_partition_t *bpart = NULL;
    if (!strong && set_type)
        jl_error("Weak global definitions cannot have types");
    enum jl_partition_kind new_kind = strong ? PARTITION_KIND_GLOBAL : PARTITION_KIND_DECLARED;
    jl_value_t *global_type = set_type;
    if (strong && !global_type)
        global_type = (jl_value_t*)jl_any_type;
    while (1) {
        bpart = jl_get_binding_partition(b, new_world);
        enum jl_partition_kind kind = jl_binding_kind(bpart);
        if (kind != PARTITION_KIND_GLOBAL) {
            if (jl_bkind_is_some_implicit(kind) || kind == PARTITION_KIND_DECLARED) {
                if (kind == new_kind) {
                    if (!set_type)
                        goto done;
                    goto check_type;
                }
                check_safe_newbinding(gm, gs);
                if (jl_atomic_load_relaxed(&bpart->min_world) == new_world) {
                    bpart->kind = new_kind | (bpart->kind & PARTITION_MASK_FLAG);
                    bpart->restriction = global_type;
                    if (global_type)
                        jl_gc_wb(bpart, global_type);
                    continue;
                } else {
                    jl_replace_binding_locked(b, bpart, global_type, new_kind, new_world);
                }
                break;
            } else if (set_type) {
                if (jl_bkind_is_some_constant(kind)) {
                    jl_errorf("cannot set type for constant %s.%s.",
                            jl_symbol_name(gm->name), jl_symbol_name(gs));
                } else {
                    jl_errorf("cannot set type for imported binding %s.%s.",
                            jl_symbol_name(gm->name), jl_symbol_name(gs));
                }
            }
        }
        if (set_type)
        {
check_type: ;
            jl_value_t *old_ty = bpart->restriction;
            JL_GC_PROMISE_ROOTED(old_ty);
            if (!jl_types_equal(set_type, old_ty)) {
                jl_errorf("cannot set type for global %s.%s. It already has a value or is already set to a different type.",
                        jl_symbol_name(gm->name), jl_symbol_name(gs));
            }

        }
        goto done;
    }
    jl_atomic_store_release(&jl_world_counter, new_world);
done:
    JL_UNLOCK(&world_counter_lock);
}

void jl_eval_global_expr(jl_module_t *m, jl_expr_t *ex, int set_type)
{
    size_t i, l = jl_array_nrows(ex->args);
    for (i = 0; i < l; i++) {
        jl_value_t *arg = jl_exprarg(ex, i);
        jl_declare_global(m, arg, NULL, 0);
    }
}

// module referenced by (top ...) from within m
// this is only needed because of the bootstrapping process:
// - initially Base doesn't exist and top === Core
// - later, it refers to either old Base or new Base
JL_DLLEXPORT jl_module_t *jl_base_relative_to(jl_module_t *m)
{
    for (;;) {
        if (m->istopmod)
            return m;
        if (m == m->parent)
            break;
        m = m->parent;
    }
    return jl_top_module;
}

static void expr_attributes(jl_value_t *v, jl_array_t *body, int *has_ccall, int *has_defs, int *has_opaque)
{
    if (!jl_is_expr(v))
        return;
    jl_expr_t *e = (jl_expr_t*)v;
    jl_sym_t *head = e->head;
    if (head == jl_toplevel_sym || head == jl_thunk_sym) {
        return;
    }
    else if (head == jl_global_sym) {
        // this could be considered has_defs, but loops that assign to globals
        // might still need to be optimized.
        return;
    }
    else if (head == jl_const_sym || head == jl_copyast_sym) {
        // Note: `copyast` is included here since it indicates the presence of
        // `quote` and probably `eval`.
        *has_defs = 1;
        return;
    }
    else if (head == jl_method_sym || jl_is_toplevel_only_expr(v)) {
        *has_defs = 1;
    }
    else if (head == jl_cfunction_sym) {
        *has_ccall = 1;
        return;
    }
    else if (head == jl_foreigncall_sym) {
        *has_ccall = 1;
        return;
    }
    else if (head == jl_new_opaque_closure_sym) {
        *has_opaque = 1;
        return;
    }
    else if (head == jl_call_sym && jl_expr_nargs(e) > 0) {
        jl_value_t *called = NULL;
        jl_value_t *f = jl_exprarg(e, 0);
        if (jl_is_ssavalue(f)) {
            f = jl_array_ptr_ref(body, ((jl_ssavalue_t*)f)->id - 1);
        }
        if (jl_is_globalref(f)) {
            jl_module_t *mod = jl_globalref_mod(f);
            jl_sym_t *name = jl_globalref_name(f);
            jl_binding_t *b = jl_get_binding(mod, name);
            called = jl_get_binding_value_if_const(b);
        }
        else if (jl_is_quotenode(f)) {
            called = jl_quotenode_value(f);
        }
        if (called != NULL) {
            if (jl_is_intrinsic(called) && jl_unbox_int32(called) == (int)llvmcall) {
                *has_ccall = 1;
            }
            if (called == jl_builtin__typebody) { // TODO: rely on latestworld instead of function callee detection here (or add it to jl_is_toplevel_only_expr)
                *has_defs = 1;
            }
        }
        return;
    }
    int i;
    for (i = 0; i < jl_array_nrows(e->args); i++) {
        jl_value_t *a = jl_exprarg(e, i);
        if (jl_is_expr(a))
            expr_attributes(a, body, has_ccall, has_defs, has_opaque);
    }
}

int jl_code_requires_compiler(jl_code_info_t *src, int include_force_compile)
{
    jl_array_t *body = src->code;
    assert(jl_typetagis(body, jl_array_any_type));
    size_t i;
    int has_ccall = 0, has_defs = 0, has_opaque = 0;
    if (include_force_compile && jl_has_meta(body, jl_force_compile_sym))
        return 1;
    for(i=0; i < jl_array_nrows(body); i++) {
        jl_value_t *stmt = jl_array_ptr_ref(body,i);
        expr_attributes(stmt, body, &has_ccall, &has_defs, &has_opaque);
        if (has_ccall)
            return 1;
    }
    return 0;
}

static void body_attributes(jl_array_t *body, int *has_ccall, int *has_defs, int *has_loops, int *has_opaque, int *forced_compile)
{
    size_t i;
    *has_loops = 0;
    for(i=0; i < jl_array_nrows(body); i++) {
        jl_value_t *stmt = jl_array_ptr_ref(body,i);
        if (!*has_loops) {
            if (jl_is_gotonode(stmt)) {
                if (jl_gotonode_label(stmt) <= i)
                    *has_loops = 1;
            }
            else if (jl_is_gotoifnot(stmt)) {
                if (jl_gotoifnot_label(stmt) <= i)
                    *has_loops = 1;
            }
        }
        expr_attributes(stmt, body, has_ccall, has_defs, has_opaque);
    }
    *forced_compile = jl_has_meta(body, jl_force_compile_sym);
}

extern size_t jl_require_world;
static jl_module_t *call_require(jl_task_t *ct, jl_module_t *mod, jl_sym_t *var) JL_GLOBALLY_ROOTED
{
    JL_TIMING(LOAD_IMAGE, LOAD_Require);
    jl_timing_printf(JL_TIMING_DEFAULT_BLOCK, "%s", jl_symbol_name(var));

    int build_mode = jl_options.incremental && jl_generating_output();
    jl_module_t *m = NULL;
    static jl_value_t *require_func = NULL;
    if (require_func == NULL && jl_base_module != NULL) {
        require_func = jl_get_global(jl_base_module, jl_symbol("require"));
    }
    if (require_func != NULL) {
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        if (build_mode && jl_require_world < ct->world_age)
            ct->world_age = jl_require_world;
        jl_value_t *reqargs[3];
        reqargs[0] = require_func;
        reqargs[1] = (jl_value_t*)mod;
        reqargs[2] = (jl_value_t*)var;
        m = (jl_module_t*)jl_apply(reqargs, 3);
    }
    if (m == NULL || !jl_is_module(m)) {
        jl_errorf("failed to load module %s", jl_symbol_name(var));
    }
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    return m;
}

// either:
//   - sets *name and returns the module to import *name from
//   - sets *name to NULL and returns a module to import
// also updates world_age
static jl_module_t *eval_import_path(jl_task_t *ct, jl_module_t *where, jl_module_t *from JL_PROPAGATES_ROOT,
                                     jl_array_t *args, jl_sym_t **name, const char *keyword) JL_GLOBALLY_ROOTED
{
    if (jl_array_nrows(args) == 0)
        jl_errorf("malformed \"%s\" statement", keyword);
    jl_sym_t *var = (jl_sym_t*)jl_array_ptr_ref(args, 0);
    size_t i = 1;
    jl_module_t *m = NULL;
    *name = NULL;
    if (!jl_is_symbol(var))
        jl_type_error(keyword, (jl_value_t*)jl_symbol_type, (jl_value_t*)var);

    if (from != NULL) {
        m = from;
        i = 0;
    }
    else if (var != jl_dot_sym) {
        // `A.B`: call the loader to obtain the root A in the current environment.
        if (jl_core_module && var == jl_core_module->name) {
            m = jl_core_module;
        }
        else if (jl_base_module && var == jl_base_module->name) {
            m = jl_base_module;
        }
        else {
            m = call_require(ct, where, var);
        }
        if (i == jl_array_nrows(args))
            return m;
    }
    else {
        // `.A.B.C`: strip off leading dots by following parent links
        m = where;
        while (1) {
            if (i >= jl_array_nrows(args))
                jl_error("invalid module path");
            var = (jl_sym_t*)jl_array_ptr_ref(args, i);
            if (var != jl_dot_sym)
                break;
            i++;
            assert(m);
            m = m->parent;
        }
    }

    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);

    while (1) {
        var = (jl_sym_t*)jl_array_ptr_ref(args, i);
        if (!jl_is_symbol(var))
            jl_type_error(keyword, (jl_value_t*)jl_symbol_type, (jl_value_t*)var);
        if (var == jl_dot_sym)
            jl_errorf("invalid %s path: \".\" in identifier path", keyword);
        if (i == jl_array_nrows(args)-1)
            break;
        m = (jl_module_t*)jl_eval_global_var(m, var);
        JL_GC_PROMISE_ROOTED(m);
        if (!jl_is_module(m))
            jl_errorf("invalid %s path: \"%s\" does not name a module", keyword, jl_symbol_name(var));
        i++;
    }
    *name = var;
    return m;
}

int jl_is_toplevel_only_expr(jl_value_t *e) JL_NOTSAFEPOINT
{
    return jl_is_expr(e) &&
        (((jl_expr_t*)e)->head == jl_module_sym ||
         ((jl_expr_t*)e)->head == jl_import_sym ||
         ((jl_expr_t*)e)->head == jl_using_sym ||
         ((jl_expr_t*)e)->head == jl_export_sym ||
         ((jl_expr_t*)e)->head == jl_public_sym ||
         ((jl_expr_t*)e)->head == jl_thunk_sym ||
         ((jl_expr_t*)e)->head == jl_global_sym ||
         ((jl_expr_t*)e)->head == jl_globaldecl_sym ||
         ((jl_expr_t*)e)->head == jl_const_sym ||
         ((jl_expr_t*)e)->head == jl_toplevel_sym ||
         ((jl_expr_t*)e)->head == jl_error_sym ||
         ((jl_expr_t*)e)->head == jl_incomplete_sym);
}

int jl_needs_lowering(jl_value_t *e) JL_NOTSAFEPOINT
{
    if (!jl_is_expr(e))
        return 0;
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_sym_t *head = ex->head;
    if (head == jl_module_sym || head == jl_import_sym || head == jl_using_sym ||
        head == jl_export_sym || head == jl_public_sym || head == jl_thunk_sym ||
        head == jl_toplevel_sym || head == jl_error_sym || head == jl_incomplete_sym ||
        head == jl_method_sym) {
        return 0;
    }
    if (head == jl_global_sym || head == jl_const_sym) {
        size_t i, l = jl_array_nrows(ex->args);
        for (i = 0; i < l; i++) {
            jl_value_t *a = jl_exprarg(ex, i);
            if (!jl_is_symbol(a) && !jl_is_globalref(a))
                return 1;
        }
        return 0;
    }
    return 1;
}

JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst_for_uninferred(jl_method_instance_t *mi, jl_code_info_t *src)
{
    // Do not compress this, we expect it to be shortlived.
    jl_code_instance_t *ci = jl_new_codeinst(mi, (jl_value_t*)jl_uninferred_sym,
        (jl_value_t*)jl_any_type, (jl_value_t*)jl_any_type, jl_nothing,
        (jl_value_t*)src, 0, src->min_world, src->max_world,
        0, NULL, NULL, NULL);
    return ci;
}

JL_DLLEXPORT jl_method_instance_t *jl_method_instance_for_thunk(jl_code_info_t *src, jl_module_t *module)
{
    jl_method_instance_t *mi = jl_new_method_instance_uninit();
    mi->specTypes = (jl_value_t*)jl_emptytuple_type;
    mi->def.module = module;
    JL_GC_PUSH1(&mi);

    jl_code_instance_t *ci = jl_new_codeinst_for_uninferred(mi, src);
    jl_atomic_store_relaxed(&mi->cache, ci);
    jl_gc_wb(mi, ci);

    JL_GC_POP();
    return mi;
}

static void import_module(jl_task_t *ct, jl_module_t *JL_NONNULL m, jl_module_t *import, jl_sym_t *asname)
{
    assert(m);
    jl_sym_t *name = asname ? asname : import->name;
    // TODO: this is a bit race-y with what error message we might print
    jl_binding_t *b = jl_get_module_binding(m, name, 1);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, ct->world_age);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (!jl_bkind_is_some_implicit(kind) && kind != PARTITION_KIND_DECLARED) {
        // Unlike regular constant declaration, we allow this as long as we eventually end up at a constant.
        jl_walk_binding_inplace(&b, &bpart, ct->world_age);
        if (jl_bkind_is_some_constant(jl_binding_kind(bpart))) {
            // Already declared (e.g. on another thread) or imported.
            if (bpart->restriction == (jl_value_t*)import)
                return;
        }
        jl_errorf("importing %s into %s conflicts with an existing global",
                    jl_symbol_name(name), jl_symbol_name(m->name));
    }
    jl_declare_constant_val2(b, m, name, (jl_value_t*)import, PARTITION_KIND_CONST_IMPORT);
}

// in `import A.B: x, y, ...`, evaluate the `A.B` part if it exists
static jl_module_t *eval_import_from(jl_task_t *ct, jl_module_t *m JL_PROPAGATES_ROOT, jl_expr_t *ex, const char *keyword)
{
    if (jl_expr_nargs(ex) == 1 && jl_is_expr(jl_exprarg(ex, 0))) {
        jl_expr_t *fr = (jl_expr_t*)jl_exprarg(ex, 0);
        if (fr->head == jl_colon_sym) {
            if (jl_expr_nargs(fr) > 0 && jl_is_expr(jl_exprarg(fr, 0))) {
                jl_expr_t *path = (jl_expr_t*)jl_exprarg(fr, 0);
                if (((jl_expr_t*)path)->head == jl_dot_sym) {
                    jl_sym_t *name = NULL;
                    jl_module_t *from = eval_import_path(ct, m, NULL, path->args, &name, keyword);
                    if (name != NULL) {
                        from = (jl_module_t*)jl_eval_global_var(from, name);
                        if (!from || !jl_is_module(from))
                            jl_errorf("invalid %s path: \"%s\" does not name a module", keyword, jl_symbol_name(name));
                    }
                    return from;
                }
            }
            jl_errorf("malformed \"%s:\" statement", keyword);
        }
    }
    return NULL;
}

static void check_macro_rename(jl_sym_t *from, jl_sym_t *to, const char *keyword)
{
    char *n1 = jl_symbol_name(from), *n2 = jl_symbol_name(to);
    if (n1[0] == '@' && n2[0] != '@')
        jl_errorf("cannot rename macro \"%s\" to non-macro \"%s\" in \"%s\"", n1, n2, keyword);
    if (n1[0] != '@' && n2[0] == '@')
        jl_errorf("cannot rename non-macro \"%s\" to macro \"%s\" in \"%s\"", n1, n2, keyword);
}

// Eval `throw(ErrorException(msg)))` in module `m`.
// Used in `jl_toplevel_eval_flex` instead of `jl_throw` so that the error
// location in julia code gets into the backtrace.
static void jl_eval_throw(jl_module_t *m, jl_value_t *exc, const char *filename, int lineno)
{
    jl_value_t *throw_ex = (jl_value_t*)jl_exprn(jl_call_sym, 2);
    JL_GC_PUSH1(&throw_ex);
    jl_exprargset(throw_ex, 0, jl_builtin_throw);
    jl_exprargset(throw_ex, 1, exc);
    jl_toplevel_eval_flex(m, throw_ex, 0, 0, &filename, &lineno);
    JL_GC_POP();
}

// Format error message and call jl_eval
static void jl_eval_errorf(jl_module_t *m, const char *filename, int lineno, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    jl_value_t *exc = jl_vexceptionf(jl_errorexception_type, fmt, args);
    va_end(args);
    JL_GC_PUSH1(&exc);
    jl_eval_throw(m, exc, filename, lineno);
    JL_GC_POP();
}

JL_DLLEXPORT jl_binding_partition_t *jl_declare_constant_val2(
    jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *val,
    enum jl_partition_kind constant_kind)
{
    JL_LOCK(&world_counter_lock);
    size_t new_world = jl_atomic_load_relaxed(&jl_world_counter) + 1;
    jl_binding_partition_t *bpart = jl_declare_constant_val3(b, mod, var, val, constant_kind, new_world);
    if (jl_atomic_load_relaxed(&bpart->min_world) == new_world)
        jl_atomic_store_release(&jl_world_counter, new_world);
    JL_UNLOCK(&world_counter_lock);
    return bpart;
}

JL_DLLEXPORT jl_binding_partition_t *jl_declare_constant_val(jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *val)
{
    return jl_declare_constant_val2(b, mod, var, val, val ? PARTITION_KIND_CONST : PARTITION_KIND_UNDEF_CONST);
}

JL_DLLEXPORT void jl_eval_const_decl(jl_module_t *m, jl_value_t *arg, jl_value_t *val)
{
    jl_module_t *gm;
    jl_sym_t *gs;
    if (jl_is_globalref(arg)) {
        gm = jl_globalref_mod(arg);
        gs = jl_globalref_name(arg);
    }
    else {
        assert(jl_is_symbol(arg));
        gm = m;
        gs = (jl_sym_t*)arg;
    }
    jl_binding_t *b = jl_get_module_binding(gm, gs, 1);
    jl_declare_constant_val(b, gm, gs, val);
}

JL_DLLEXPORT jl_value_t *jl_toplevel_eval_flex(jl_module_t *JL_NONNULL m, jl_value_t *e, int fast, int expanded, const char **toplevel_filename, int *toplevel_lineno)
{
    jl_task_t *ct = jl_current_task;
    if (!jl_is_expr(e)) {
        if (jl_is_linenode(e)) {
            *toplevel_lineno = jl_linenode_line(e);
            jl_value_t *file = jl_linenode_file(e);
            if (file != jl_nothing) {
                assert(jl_is_symbol(file));
                *toplevel_filename = jl_symbol_name((jl_sym_t*)file);
            }
            // Not thread safe. For debugging and last resort error messages (jl_critical_error) only.
            jl_filename = *toplevel_filename;
            jl_lineno = *toplevel_lineno;
            return jl_nothing;
        }
        if (jl_is_symbol(e)) {
            char *n = jl_symbol_name((jl_sym_t*)e), *n0 = n;
            while (*n == '_') ++n;
            if (*n == 0 && n > n0)
                jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno, "all-underscore identifiers are write-only and their values cannot be used in expressions");
        }
        return jl_interpret_toplevel_expr_in(m, e, NULL, NULL);
    }

    jl_expr_t *ex = (jl_expr_t*)e;

    if (ex->head == jl_dot_sym && jl_expr_nargs(ex) != 1) {
        if (jl_expr_nargs(ex) != 2)
            jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno, "syntax: malformed \".\" expression");
        jl_value_t *lhs = jl_exprarg(ex, 0);
        jl_value_t *rhs = jl_exprarg(ex, 1);
        // only handle `a.b` syntax here, so qualified names can be eval'd in pure contexts
        if (jl_is_quotenode(rhs) && jl_is_symbol(jl_fieldref(rhs, 0))) {
            return jl_eval_dot_expr(m, lhs, rhs, fast, toplevel_filename, toplevel_lineno);
        }
    }

    if (ct->ptls->in_pure_callback) {
        jl_error("eval cannot be used in a generated function");
    }

    jl_method_instance_t *mfunc = NULL;
    jl_code_info_t *thk = NULL;
    jl_value_t *root = NULL;
    JL_GC_PUSH4(&mfunc, &thk, &ex, &root);

    size_t last_age = ct->world_age;
    if (!expanded && jl_needs_lowering(e)) {
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        ex = (jl_expr_t*)jl_expand_with_loc_warn(e, m, *toplevel_filename, *toplevel_lineno);
        ct->world_age = last_age;
    }
    jl_sym_t *head = jl_is_expr(ex) ? ex->head : NULL;

    if (head == jl_module_sym) {
        jl_value_t *val = jl_eval_module_expr(m, ex);
        JL_GC_POP();
        return val;
    }
    else if (head == jl_using_sym) {
        jl_sym_t *name = NULL;
        jl_module_t *from = eval_import_from(ct, m, ex, "using");
        size_t i = 0;
        if (from) {
            i = 1;
            ex = (jl_expr_t*)jl_exprarg(ex, 0);
        }
        for (; i < jl_expr_nargs(ex); i++) {
            jl_value_t *a = jl_exprarg(ex, i);
            if (jl_is_expr(a) && ((jl_expr_t*)a)->head == jl_dot_sym) {
                name = NULL;
                jl_module_t *import = eval_import_path(ct, m, from, ((jl_expr_t*)a)->args, &name, "using");
                if (from) {
                    // `using A: B` and `using A: B.c` syntax
                    jl_module_use(ct, m, import, name);
                }
                else {
                    jl_module_t *u = import;
                    if (name != NULL)
                        u = (jl_module_t*)jl_eval_global_var(import, name);
                    if (!jl_is_module(u))
                        jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno,
                            "invalid using path: \"%s\" does not name a module",
                                       jl_symbol_name(name));
                    // `using A` and `using A.B` syntax
                    jl_module_using(m, u);
                    if (m == jl_main_module && name == NULL) {
                        // TODO: for now, `using A` in Main also creates an explicit binding for `A`
                        // This will possibly be extended to all modules.
                        import_module(ct, m, u, NULL);
                    }
                }
                continue;
            }
            else if (from && jl_is_expr(a) && ((jl_expr_t*)a)->head == jl_as_sym && jl_expr_nargs(a) == 2 &&
                     jl_is_expr(jl_exprarg(a, 0)) && ((jl_expr_t*)jl_exprarg(a, 0))->head == jl_dot_sym) {
                jl_sym_t *asname = (jl_sym_t*)jl_exprarg(a, 1);
                if (jl_is_symbol(asname)) {
                    jl_expr_t *path = (jl_expr_t*)jl_exprarg(a, 0);
                    name = NULL;
                    jl_module_t *import = eval_import_path(ct, m, from, ((jl_expr_t*)path)->args, &name, "using");
                    assert(name);
                    check_macro_rename(name, asname, "using");
                    // `using A: B as C` syntax
                    jl_module_use_as(ct, m, import, name, asname);
                    continue;
                }
            }
            jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno,
                "syntax: malformed \"using\" statement");
        }
        JL_GC_POP();
        ct->world_age = last_age;
        return jl_nothing;
    }
    else if (head == jl_import_sym) {
        jl_sym_t *name = NULL;
        jl_module_t *from = eval_import_from(ct, m, ex, "import");
        size_t i = 0;
        if (from) {
            i = 1;
            ex = (jl_expr_t*)jl_exprarg(ex, 0);
        }
        for (; i < jl_expr_nargs(ex); i++) {
            jl_value_t *a = jl_exprarg(ex, i);
            if (jl_is_expr(a) && ((jl_expr_t*)a)->head == jl_dot_sym) {
                name = NULL;
                jl_module_t *import = eval_import_path(ct, m, from, ((jl_expr_t*)a)->args, &name, "import");
                if (name == NULL) {
                    // `import A` syntax
                    import_module(ct, m, import, NULL);
                }
                else {
                    // `import A.B` or `import A: B` syntax
                    jl_module_import(ct, m, import, name);
                }
                continue;
            }
            else if (jl_is_expr(a) && ((jl_expr_t*)a)->head == jl_as_sym && jl_expr_nargs(a) == 2 &&
                     jl_is_expr(jl_exprarg(a, 0)) && ((jl_expr_t*)jl_exprarg(a, 0))->head == jl_dot_sym) {
                jl_sym_t *asname = (jl_sym_t*)jl_exprarg(a, 1);
                if (jl_is_symbol(asname)) {
                    jl_expr_t *path = (jl_expr_t*)jl_exprarg(a, 0);
                    name = NULL;
                    jl_module_t *import = eval_import_path(ct, m, from, ((jl_expr_t*)path)->args, &name, "import");
                    if (name == NULL) {
                        // `import A as B` syntax
                        import_module(ct, m, import, asname);
                    }
                    else {
                        check_macro_rename(name, asname, "import");
                        // `import A.B as C` syntax
                        jl_module_import_as(ct, m, import, name, asname);
                    }
                    continue;
                }
            }
            jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno,
                "syntax: malformed \"import\" statement");
        }
        JL_GC_POP();
        ct->world_age = last_age;
        return jl_nothing;
    }
    else if (head == jl_export_sym || head == jl_public_sym) {
        int exp = (head == jl_export_sym);
        volatile int any_new = 0;
        JL_LOCK(&world_counter_lock);
        size_t new_world = jl_atomic_load_acquire(&jl_world_counter)+1;
        JL_TRY {
            for (size_t i = 0; i < jl_array_nrows(ex->args); i++) {
                jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(ex->args, i);
                if (!jl_is_symbol(name))
                    jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno,
                         exp ? "syntax: malformed \"export\" statement" :
                               "syntax: malformed \"public\" statement");
                if (jl_module_public_(m, name, exp, new_world))
                    any_new = 1;
            }
        }
        JL_CATCH {
            if (any_new)
                jl_atomic_store_release(&jl_world_counter, new_world);
            JL_UNLOCK(&world_counter_lock);
            jl_rethrow();
        }
        if (any_new)
            jl_atomic_store_release(&jl_world_counter, new_world);
        JL_UNLOCK(&world_counter_lock);
        JL_GC_POP();
        return jl_nothing;
    }
    else if (head == jl_global_sym) {
        size_t i, l = jl_array_nrows(ex->args);
        for (i = 0; i < l; i++) {
            jl_value_t *arg = jl_exprarg(ex, i);
            jl_declare_global(m, arg, NULL, 0);
        }
        JL_GC_POP();
        return jl_nothing;
    }
    else if (head == jl_const_sym) {
        jl_eval_const_decl(m, jl_exprarg(ex, 0), NULL);
        JL_GC_POP();
        return jl_nothing;
    }
    else if (head == jl_toplevel_sym) {
        jl_value_t *res = jl_nothing;
        int i;
        for (i = 0; i < jl_array_nrows(ex->args); i++) {
            root = jl_array_ptr_ref(ex->args, i);
            if (jl_needs_lowering(root)) {
                ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
                root = jl_expand_with_loc_warn(root, m, *toplevel_filename, *toplevel_lineno);
            }
            ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
            res = jl_toplevel_eval_flex(m, root, fast, 1, toplevel_filename, toplevel_lineno);
        }
        ct->world_age = last_age;
        JL_GC_POP();
        return res;
    }
    else if (head == jl_error_sym || head == jl_incomplete_sym) {
        if (jl_expr_nargs(ex) == 0)
            jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno,
                "malformed \"%s\" expression", jl_symbol_name(head));
        if (jl_is_string(jl_exprarg(ex, 0)))
            jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno,
                "syntax: %s", jl_string_data(jl_exprarg(ex, 0)));
        jl_eval_throw(m, jl_exprarg(ex, 0), *toplevel_filename, *toplevel_lineno);
    }
    else if (jl_is_symbol(ex)) {
        JL_GC_POP();
        return jl_eval_global_var(m, (jl_sym_t*)ex);
    }
    else if (head == NULL) {
        JL_GC_POP();
        return (jl_value_t*)ex;
    }

    int has_ccall = 0, has_defs = 0, has_loops = 0, has_opaque = 0, forced_compile = 0;
    assert(head == jl_thunk_sym);
    thk = (jl_code_info_t*)jl_exprarg(ex, 0);
    if (!jl_is_code_info(thk) || !jl_typetagis(thk->code, jl_array_any_type)) {
        jl_eval_errorf(m, *toplevel_filename, *toplevel_lineno,
            "malformed \"thunk\" statement");
    }
    body_attributes((jl_array_t*)thk->code, &has_ccall, &has_defs, &has_loops, &has_opaque, &forced_compile);

    jl_value_t *result;
    if (has_ccall ||
            ((forced_compile || (!has_defs && fast && has_loops)) &&
            jl_options.compile_enabled != JL_OPTIONS_COMPILE_OFF &&
            jl_options.compile_enabled != JL_OPTIONS_COMPILE_MIN &&
            jl_get_module_compile(m) != JL_OPTIONS_COMPILE_OFF &&
            jl_get_module_compile(m) != JL_OPTIONS_COMPILE_MIN)) {
        // use codegen
        mfunc = jl_method_instance_for_thunk(thk, m);
        jl_resolve_definition_effects_in_ir((jl_array_t*)thk->code, m, NULL, NULL, 0);
        // Don't infer blocks containing e.g. method definitions, since it's probably not worthwhile.
        size_t world = jl_atomic_load_acquire(&jl_world_counter);
        ct->world_age = world;
        if (!has_defs && jl_get_module_infer(m) != 0) {
            (void)jl_type_infer(mfunc, world, SOURCE_MODE_ABI);
        }
        result = jl_invoke(/*func*/NULL, /*args*/NULL, /*nargs*/0, mfunc);
        ct->world_age = last_age;
    }
    else {
        // use interpreter
        assert(thk);
        if (has_opaque) {
            jl_resolve_definition_effects_in_ir((jl_array_t*)thk->code, m, NULL, NULL, 0);
        }
        size_t world = jl_atomic_load_acquire(&jl_world_counter);
        ct->world_age = world;
        result = jl_interpret_toplevel_thunk(m, thk);
        ct->world_age = last_age;
    }

    JL_GC_POP();
    return result;
}

JL_DLLEXPORT jl_value_t *jl_toplevel_eval(jl_module_t *m, jl_value_t *v)
{
    const char *filename = jl_filename;
    int lineno = jl_lineno;
    return jl_toplevel_eval_flex(m, v, 1, 0, &filename, &lineno);
}

// Check module `m` is open for `eval/include`, or throw an error.
JL_DLLEXPORT void jl_check_top_level_effect(jl_module_t *m, char *fname)
{
    if (jl_current_task->ptls->in_pure_callback)
        jl_errorf("%s cannot be used in a generated function", fname);
    if (jl_options.incremental && jl_generating_output()) {
        if (m != jl_main_module) { // TODO: this was grand-fathered in
            JL_LOCK(&jl_modules_mutex);
            int open = ptrhash_has(&jl_current_modules, (void*)m);
            if (!open && jl_module_init_order != NULL) {
                size_t i, l = jl_array_nrows(jl_module_init_order);
                for (i = 0; i < l; i++) {
                    if (m == (jl_module_t*)jl_array_ptr_ref(jl_module_init_order, i)) {
                        open = 1;
                        break;
                    }
                }
            }
            JL_UNLOCK(&jl_modules_mutex);
            if (!open && !jl_is__toplevel__mod(m)) {
                const char* name = jl_symbol_name(m->name);
                jl_errorf("Evaluation into the closed module `%s` breaks incremental compilation "
                          "because the side effects will not be permanent. "
                          "This is likely due to some other module mutating `%s` with `%s` during "
                          "precompilation - don't do this.", name, name, fname);
            }
        }
    }
}

JL_DLLEXPORT jl_value_t *jl_toplevel_eval_in(jl_module_t *m, jl_value_t *ex)
{
    jl_check_top_level_effect(m, "eval");
    jl_value_t *v = NULL;
    int last_lineno = jl_lineno;
    const char *last_filename = jl_filename;
    jl_task_t *ct = jl_current_task;
    jl_lineno = 1;
    jl_filename = "none";
    size_t last_age = ct->world_age;
    JL_TRY {
        ct->world_age = jl_atomic_load_relaxed(&jl_world_counter);
        v = jl_toplevel_eval(m, ex);
    }
    JL_CATCH {
        jl_lineno = last_lineno;
        jl_filename = last_filename;
        jl_rethrow();
    }
    jl_lineno = last_lineno;
    jl_filename = last_filename;
    ct->world_age = last_age;
    assert(v);
    return v;
}

//------------------------------------------------------------------------------
// Code loading: combined parse+eval for include()

// Parse julia code from the string `text` at top level, attributing it to
// `filename`. This is used during bootstrap, but the real Base.include() is
// implemented in Julia code.
static jl_value_t *jl_parse_eval_all(jl_module_t *module, jl_value_t *text,
                                     jl_value_t *filename)
{
    if (!jl_is_string(text) || !jl_is_string(filename)) {
        jl_errorf("Expected `String`s for `text` and `filename`");
    }
    jl_check_top_level_effect(module, "include");

    jl_value_t *result = jl_nothing;
    jl_value_t *ast = NULL;
    jl_value_t *expression = NULL;
    JL_GC_PUSH3(&ast, &result, &expression);

    ast = jl_svecref(jl_parse(jl_string_data(text), jl_string_len(text),
                              filename, 1, 0, (jl_value_t*)jl_all_sym), 0);
    if (!jl_is_expr(ast) || ((jl_expr_t*)ast)->head != jl_toplevel_sym) {
        jl_errorf("jl_parse_all() must generate a top level expression");
    }

    jl_task_t *ct = jl_current_task;
    int last_lineno = jl_lineno;
    const char *last_filename = jl_filename;
    int lineno = 0;
    jl_lineno = 0;
    const char *filename_str = jl_string_data(filename);
    jl_filename = filename_str;

    JL_TRY {
        size_t last_age = ct->world_age;
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        for (size_t i = 0; i < jl_expr_nargs(ast); i++) {
            expression = jl_exprarg(ast, i);
            if (jl_is_linenode(expression)) {
                // filename is already set above.
                lineno = jl_linenode_line(expression);
                jl_lineno = lineno;
                continue;
            }
            ct->world_age = jl_atomic_load_relaxed(&jl_world_counter);
            expression = jl_expand_with_loc_warn(expression, module,
                                                 jl_string_data(filename), lineno);
            ct->world_age = jl_atomic_load_relaxed(&jl_world_counter);
            result = jl_toplevel_eval_flex(module, expression, 1, 1, &filename_str, &lineno);
        }
        ct->world_age = last_age;
    }
    JL_CATCH {
        result = jl_box_long(lineno); // (ab)use result to root error line
        jl_lineno = last_lineno;
        jl_filename = last_filename;
        if (jl_loaderror_type == NULL)
            jl_rethrow();
        else
            jl_rethrow_other(jl_new_struct(jl_loaderror_type, filename, result,
                                           jl_current_exception(ct)));
    }
    jl_lineno = last_lineno;
    jl_filename = last_filename;
    JL_GC_POP();
    return result;
}

// Synchronously read content of entire file into a julia String
static jl_value_t *jl_file_content_as_string(jl_value_t *filename)
{
    const char *fname = jl_string_data(filename);
    ios_t f;
    if (ios_file(&f, fname, 1, 0, 0, 0) == NULL)
        jl_errorf("File \"%s\" not found", fname);
    ios_bufmode(&f, bm_none);
    ios_seek_end(&f);
    size_t len = ios_pos(&f);
    jl_value_t *text = jl_alloc_string(len);
    ios_seek(&f, 0);
    if (ios_readall(&f, jl_string_data(text), len) != len)
        jl_errorf("Error reading file \"%s\"", fname);
    ios_close(&f);
    return text;
}

// Load and parse julia code from the file `filename`. Eval the resulting
// statements into `module` after applying `mapexpr` to each one.
JL_DLLEXPORT jl_value_t *jl_load_(jl_module_t *module, jl_value_t *filename)
{
    jl_value_t *text = jl_file_content_as_string(filename);
    JL_GC_PUSH1(&text);
    jl_value_t *result = jl_parse_eval_all(module, text, filename);
    JL_GC_POP();
    return result;
}

// Code loading - julia.h C API with native C types

// Parse julia code from `filename` and eval into `module`.
JL_DLLEXPORT jl_value_t *jl_load(jl_module_t *module, const char *filename)
{
    jl_value_t *filename_ = NULL;
    JL_GC_PUSH1(&filename_);
    filename_ = jl_cstr_to_string(filename);
    jl_value_t *result = jl_load_(module, filename_);
    JL_GC_POP();
    return result;
}

// Parse julia code from the string `text` of length `len`, attributing it to
// `filename`. Eval the resulting statements into `module`.
JL_DLLEXPORT jl_value_t *jl_load_file_string(const char *text, size_t len,
                                             char *filename, jl_module_t *module)
{
    jl_value_t *text_ = NULL;
    jl_value_t *filename_ = NULL;
    JL_GC_PUSH2(&text_, &filename_);
    text_ = jl_pchar_to_string(text, len);
    filename_ = jl_cstr_to_string(filename);
    jl_value_t *result = jl_parse_eval_all(module, text_, filename_);
    JL_GC_POP();
    return result;
}


//--------------------------------------------------
// Code loading helpers for bootstrap

JL_DLLEXPORT jl_value_t *jl_prepend_cwd(jl_value_t *str)
{
    size_t sz = 1024;
    char path[1024];
    int c = uv_cwd(path, &sz);
    if (c < 0) {
        jl_errorf("could not get current directory");
    }
    path[sz] = '/';  // fix later with normpath if Windows
    const char *fstr = (const char*)jl_string_data(str);
    if (strlen(fstr) + sz >= 1024) {
        jl_errorf("use a bigger buffer for jl_fullpath");
    }
    strcpy(path + sz + 1, fstr);
    return jl_cstr_to_string(path);
}

JL_DLLEXPORT jl_value_t *jl_prepend_string(jl_value_t *prefix, jl_value_t *str)
{
    char path[1024];
    const char *pstr = (const char*)jl_string_data(prefix);
    size_t sz = strlen(pstr);
    const char *fstr = (const char*)jl_string_data(str);
    if (strlen(fstr) + sz >= sizeof(path)) {
        jl_errorf("use a bigger buffer for jl_fullpath");
    }
    strcpy(path, pstr);
    strcpy(path + sz, fstr);
    return jl_cstr_to_string(path);
}


#ifdef __cplusplus
}
#endif
