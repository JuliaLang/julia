// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  precompile.c
  Generating compiler output artifacts (object files, etc.)
*/

#include <stdlib.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT int jl_generating_output(void)
{
    return jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputji || jl_options.outputasm;
}

static void *jl_precompile(int all);

void jl_write_compiler_output(void)
{
    if (!jl_generating_output()) {
        return;
    }

    void *native_code = NULL;
    if (!jl_options.incremental)
        native_code = jl_precompile(jl_options.compile_enabled == JL_OPTIONS_COMPILE_ALL);

    if (!jl_module_init_order) {
        jl_printf(JL_STDERR, "WARNING: --output requested, but no modules defined during run\n");
        return;
    }

    jl_array_t *worklist = jl_module_init_order;
    JL_GC_PUSH1(&worklist);
    jl_module_init_order = jl_alloc_vec_any(0);
    int i, l = jl_array_len(worklist);
    for (i = 0; i < l; i++) {
        jl_value_t *m = jl_ptrarrayref(worklist, i);
        jl_value_t *f = jl_get_global((jl_module_t*)m, jl_symbol("__init__"));
        if (f) {
            jl_array_ptr_1d_push(jl_module_init_order, m);
            int setting = jl_get_module_compile((jl_module_t*)m);
            if (setting != JL_OPTIONS_COMPILE_OFF &&
                setting != JL_OPTIONS_COMPILE_MIN) {
                // TODO: this would be better handled if moved entirely to jl_precompile
                // since it's a slightly duplication of effort
                jl_value_t *tt = jl_is_type(f) ? (jl_value_t*)jl_wrap_Type(f) : jl_typeof(f);
                JL_GC_PUSH1(&tt);
                tt = (jl_value_t*)jl_apply_tuple_type_v(&tt, 1);
                jl_compile_hint((jl_tupletype_t*)tt);
                JL_GC_POP();
            }
        }
    }

    if (jl_options.incremental) {
        if (jl_options.outputji)
            if (jl_save_incremental(jl_options.outputji, worklist))
                jl_exit(1);
        if (jl_options.outputbc || jl_options.outputunoptbc)
            jl_printf(JL_STDERR, "WARNING: incremental output to a .bc file is not implemented\n");
        if (jl_options.outputo)
            jl_printf(JL_STDERR, "WARNING: incremental output to a .o file is not implemented\n");
        if (jl_options.outputasm)
            jl_printf(JL_STDERR, "WARNING: incremental output to a .s file is not implemented\n");
    }
    else {
        ios_t *s = NULL;
        if (jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputasm)
            s = jl_create_system_image(native_code);

        if (jl_options.outputji) {
            if (s == NULL) {
                jl_save_system_image(jl_options.outputji);
            }
            else {
                ios_t f;
                if (ios_file(&f, jl_options.outputji, 1, 1, 1, 1) == NULL)
                    jl_errorf("cannot open system image file \"%s\" for writing", jl_options.outputji);
                ios_write(&f, (const char*)s->buf, (size_t)s->size);
                ios_close(&f);
            }
        }

        if (jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputasm) {
            assert(s);
            jl_dump_native(native_code,
                           jl_options.outputbc,
                           jl_options.outputunoptbc,
                           jl_options.outputo,
                           jl_options.outputasm,
                           (const char*)s->buf, (size_t)s->size);
            jl_postoutput_hook();
        }
    }
    for (size_t i = 0; i < jl_current_modules.size; i += 2) {
        if (jl_current_modules.table[i + 1] != HT_NOTFOUND) {
            jl_printf(JL_STDERR, "\nWARNING: detected unclosed module: ");
            jl_static_show(JL_STDERR, (jl_value_t*)jl_current_modules.table[i]);
            jl_printf(JL_STDERR, "\n  ** incremental compilation may be broken for this module **\n\n");
        }
    }
    JL_GC_POP();
}

// f{<:Union{...}}(...) is a common pattern
// and expanding the Union may give a leaf function
static void _compile_all_tvar_union(jl_value_t *methsig)
{
    int tvarslen = jl_subtype_env_size(methsig);
    jl_value_t *sigbody = methsig;
    jl_value_t **roots;
    JL_GC_PUSHARGS(roots, 1 + 2 * tvarslen);
    jl_value_t **env = roots + 1;
    int *idx = (int*)alloca(sizeof(int) * tvarslen);
    int i;
    for (i = 0; i < tvarslen; i++) {
        assert(jl_is_unionall(sigbody));
        idx[i] = 0;
        env[2 * i] = (jl_value_t*)((jl_unionall_t*)sigbody)->var;
        env[2 * i + 1] = jl_bottom_type; // initialize the list with Union{}, since T<:Union{} is always a valid option
        sigbody = ((jl_unionall_t*)sigbody)->body;
    }

    for (i = 0; i < tvarslen; /* incremented by inner loop */) {
        jl_value_t **sig = &roots[0];
        JL_TRY {
            // TODO: wrap in UnionAll for each tvar in env[2*i + 1] ?
            // currently doesn't matter much, since jl_compile_hint doesn't work on abstract types
            *sig = (jl_value_t*)jl_instantiate_type_with(sigbody, env, tvarslen);
        }
        JL_CATCH {
            goto getnext; // sigh, we found an invalid type signature. should we warn the user?
        }
        if (!jl_has_concrete_subtype(*sig))
            goto getnext; // signature wouldn't be callable / is invalid -- skip it
        if (jl_is_concrete_type(*sig)) {
            if (jl_compile_hint((jl_tupletype_t *)*sig))
                goto getnext; // success
        }

    getnext:
        for (i = 0; i < tvarslen; i++) {
            jl_tvar_t *tv = (jl_tvar_t*)env[2 * i];
            if (jl_is_uniontype(tv->ub)) {
                size_t l = jl_count_union_components(tv->ub);
                size_t j = idx[i];
                if (j == l) {
                    env[2 * i + 1] = jl_bottom_type;
                    idx[i] = 0;
                }
                else {
                    jl_value_t *ty = jl_nth_union_component(tv->ub, j);
                    if (!jl_is_concrete_type(ty))
                        ty = (jl_value_t*)jl_new_typevar(tv->name, tv->lb, ty);
                    env[2 * i + 1] = ty;
                    idx[i] = j + 1;
                    break;
                }
            }
            else {
                env[2 * i + 1] = (jl_value_t*)tv;
            }
        }
    }
    JL_GC_POP();
}

// f(::Union{...}, ...) is a common pattern
// and expanding the Union may give a leaf function
static void _compile_all_union(jl_value_t *sig)
{
    jl_tupletype_t *sigbody = (jl_tupletype_t*)jl_unwrap_unionall(sig);
    size_t count_unions = 0;
    size_t i, l = jl_svec_len(sigbody->parameters);
    jl_svec_t *p = NULL;
    jl_value_t *methsig = NULL;

    for (i = 0; i < l; i++) {
        jl_value_t *ty = jl_svecref(sigbody->parameters, i);
        if (jl_is_uniontype(ty))
            ++count_unions;
        else if (ty == jl_bottom_type)
            return; // why does this method exist?
        else if (jl_is_datatype(ty) && !jl_has_free_typevars(ty) &&
                 ((!jl_is_kind(ty) && ((jl_datatype_t*)ty)->isconcretetype) ||
                  ((jl_datatype_t*)ty)->name == jl_type_typename))
            return; // no amount of union splitting will make this a leaftype signature
    }

    if (count_unions == 0 || count_unions >= 6) {
        _compile_all_tvar_union(sig);
        return;
    }

    int *idx = (int*)alloca(sizeof(int) * count_unions);
    for (i = 0; i < count_unions; i++) {
        idx[i] = 0;
    }

    JL_GC_PUSH2(&p, &methsig);
    int idx_ctr = 0, incr = 0;
    while (!incr) {
        p = jl_alloc_svec_uninit(l);
        for (i = 0, idx_ctr = 0, incr = 1; i < l; i++) {
            jl_value_t *ty = jl_svecref(sigbody->parameters, i);
            if (jl_is_uniontype(ty)) {
                assert(idx_ctr < count_unions);
                size_t l = jl_count_union_components(ty);
                size_t j = idx[idx_ctr];
                jl_svecset(p, i, jl_nth_union_component(ty, j));
                ++j;
                if (incr) {
                    if (j == l) {
                        idx[idx_ctr] = 0;
                    }
                    else {
                        idx[idx_ctr] = j;
                        incr = 0;
                    }
                }
                ++idx_ctr;
            }
            else {
                jl_svecset(p, i, ty);
            }
        }
        methsig = (jl_value_t*)jl_apply_tuple_type(p);
        methsig = jl_rewrap_unionall(methsig, sig);
        _compile_all_tvar_union(methsig);
    }

    JL_GC_POP();
}

static int compile_all_collect__(jl_typemap_entry_t *ml, void *env)
{
    jl_array_t *allmeths = (jl_array_t*)env;
    jl_method_t *m = ml->func.method;
    if (m->source) {
        // method has a non-generated definition; can be compiled generically
        jl_array_ptr_1d_push(allmeths, (jl_value_t*)m);
    }
    return 1;
}

static int compile_all_collect_(jl_methtable_t *mt, void *env)
{
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), compile_all_collect__, env);
    return 1;
}

static void jl_compile_all_defs(jl_array_t *mis)
{
    jl_array_t *allmeths = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&allmeths);

    jl_foreach_reachable_mtable(compile_all_collect_, allmeths);

    size_t i, l = jl_array_len(allmeths);
    for (i = 0; i < l; i++) {
        jl_method_t *m = (jl_method_t*)jl_array_ptr_ref(allmeths, i);
        if (jl_isa_compileable_sig((jl_tupletype_t*)m->sig, m)) {
            // method has a single compilable specialization, e.g. its definition
            // signature is concrete. in this case we can just hint it.
            jl_compile_hint((jl_tupletype_t*)m->sig);
        }
        else {
            // first try to create leaf signatures from the signature declaration and compile those
            _compile_all_union(m->sig);

            // finally, compile a fully generic fallback that can work for all arguments
            jl_method_instance_t *unspec = jl_get_unspecialized(m);
            if (unspec)
                jl_array_ptr_1d_push(mis, (jl_value_t*)unspec);
        }
    }

    JL_GC_POP();
}

static int precompile_enq_specialization_(jl_method_instance_t *mi, void *closure)
{
    assert(jl_is_method_instance(mi));
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        int do_compile = 0;
        if (jl_atomic_load_relaxed(&codeinst->invoke) != jl_fptr_const_return) {
            jl_value_t *inferred = jl_atomic_load_relaxed(&codeinst->inferred);
            if (inferred &&
                inferred != jl_nothing &&
                jl_ir_flag_inferred((jl_array_t*)inferred) &&
                (jl_ir_inlining_cost((jl_array_t*)inferred) == UINT16_MAX)) {
                do_compile = 1;
            }
            else if (jl_atomic_load_relaxed(&codeinst->invoke) != NULL || jl_atomic_load_relaxed(&codeinst->precompile)) {
                do_compile = 1;
            }
        }
        if (do_compile) {
            jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)mi);
            return 1;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    return 1;
}

static int precompile_enq_all_specializations__(jl_typemap_entry_t *def, void *closure)
{
    jl_method_t *m = def->func.method;
    if ((m->name == jl_symbol("__init__") || m->ccallable) && jl_is_dispatch_tupletype(m->sig)) {
        // ensure `__init__()` and @ccallables get strongly-hinted, specialized, and compiled
        jl_method_instance_t *mi = jl_specializations_get_linfo(m, m->sig, jl_emptysvec);
        jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)mi);
    }
    else {
        jl_svec_t *specializations = jl_atomic_load_relaxed(&def->func.method->specializations);
        size_t i, l = jl_svec_len(specializations);
        for (i = 0; i < l; i++) {
            jl_value_t *mi = jl_svecref(specializations, i);
            if (mi != jl_nothing)
                precompile_enq_specialization_((jl_method_instance_t*)mi, closure);
        }
    }
    if (m->ccallable)
        jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)m->ccallable);
    return 1;
}

static int precompile_enq_all_specializations_(jl_methtable_t *mt, void *env)
{
    return jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), precompile_enq_all_specializations__, env);
}

static void *jl_precompile(int all)
{
    // array of MethodInstances and ccallable aliases to include in the output
    jl_array_t *m = jl_alloc_vec_any(0);
    jl_array_t *m2 = NULL;
    jl_method_instance_t *mi = NULL;
    JL_GC_PUSH3(&m, &m2, &mi);
    if (all)
        jl_compile_all_defs(m);
    jl_foreach_reachable_mtable(precompile_enq_all_specializations_, m);
    m2 = jl_alloc_vec_any(0);
    for (size_t i = 0; i < jl_array_len(m); i++) {
        jl_value_t *item = jl_array_ptr_ref(m, i);
        if (jl_is_method_instance(item)) {
            mi = (jl_method_instance_t*)item;
            size_t min_world = 0;
            size_t max_world = ~(size_t)0;
            if (mi != jl_atomic_load_relaxed(&mi->def.method->unspecialized) && !jl_isa_compileable_sig((jl_tupletype_t*)mi->specTypes, mi->def.method))
                mi = jl_get_specialization1((jl_tupletype_t*)mi->specTypes, jl_atomic_load_acquire(&jl_world_counter), &min_world, &max_world, 0);
            if (mi)
                jl_array_ptr_1d_push(m2, (jl_value_t*)mi);
        }
        else {
            assert(jl_is_simplevector(item));
            assert(jl_svec_len(item) == 2);
            jl_array_ptr_1d_push(m2, item);
        }
    }
    m = NULL;
    void *native_code = jl_create_native(m2, NULL, NULL, 0);
    JL_GC_POP();
    return native_code;
}

#ifdef __cplusplus
}
#endif
