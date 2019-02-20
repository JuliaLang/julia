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
    return jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputji;
}

void jl_precompile(int all);

void jl_write_compiler_output(void)
{
    if (!jl_generating_output()) {
        if (jl_options.outputjitbc)
            jl_dump_native(NULL, jl_options.outputjitbc, NULL, NULL, 0);
        return;
    }

    if (!jl_options.incremental)
        jl_precompile(jl_options.compile_enabled == JL_OPTIONS_COMPILE_ALL);

    if (!jl_module_init_order) {
        jl_printf(JL_STDERR, "WARNING: --output requested, but no modules defined during run\n");
        return;
    }

    if (jl_options.outputjitbc) {
        jl_printf(JL_STDERR, "WARNING: --output-jit-bc is meaningless with options for dumping sysimage data\n");
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
            // TODO: this would be better handled if moved entirely to jl_precompile
            // since it's a slightly duplication of effort
            jl_value_t *tt = jl_is_type(f) ? (jl_value_t*)jl_wrap_Type(f) : jl_typeof(f);
            JL_GC_PUSH1(&tt);
            tt = (jl_value_t*)jl_apply_tuple_type_v(&tt, 1);
            jl_compile_hint((jl_tupletype_t*)tt);
            JL_GC_POP();
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
    }
    else {
        ios_t *s = NULL;
        if (jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc)
            s = jl_create_system_image();

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

        if (jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc) {
            assert(s);
            jl_dump_native(jl_options.outputbc,
                           jl_options.outputunoptbc,
                           jl_options.outputo,
                           (const char*)s->buf, (size_t)s->size);
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
    if (!jl_is_unionall(methsig) && jl_is_dispatch_tupletype(methsig)) {
        // usually can create a specialized version of the function,
        // if the signature is already a dispatch type
        if (jl_compile_hint((jl_tupletype_t*)methsig))
            return;
    }

    int tvarslen = jl_subtype_env_size(methsig);
    jl_value_t *sigbody = methsig;
    jl_value_t **env;
    JL_GC_PUSHARGS(env, 2 * tvarslen);
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
        jl_value_t *sig;
        JL_TRY {
            // TODO: wrap in UnionAll for each tvar in env[2*i + 1] ?
            // currently doesn't matter much, since jl_compile_hint doesn't work on abstract types
            sig = (jl_value_t*)jl_instantiate_type_with(sigbody, env, tvarslen);
        }
        JL_CATCH {
            goto getnext; // sigh, we found an invalid type signature. should we warn the user?
        }
        if (!jl_has_concrete_subtype(sig))
            goto getnext; // signature wouldn't be callable / is invalid -- skip it
        if (jl_is_concrete_type(sig)) {
            JL_GC_PROMISE_ROOTED(sig); // `sig` is rooted because it's a leaftype (JL_ALWAYS_LEAFTYPE)
            if (jl_compile_hint((jl_tupletype_t*)sig))
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

static void _compile_all_deq(jl_array_t *found)
{
    int found_i, found_l = jl_array_len(found);
    jl_printf(JL_STDERR, "found %d uncompiled methods for compile-all\n", (int)found_l);
    jl_method_instance_t *linfo = NULL;
    jl_value_t *src = NULL;
    JL_GC_PUSH2(&linfo, &src);
    for (found_i = 0; found_i < found_l; found_i++) {
        if (found_i % (1 + found_l / 300) == 0 || found_i == found_l - 1) // show 300 progress steps, to show progress without overwhelming log files
            jl_printf(JL_STDERR, " %d / %d\r", found_i + 1, found_l);
        jl_typemap_entry_t *ml = (jl_typemap_entry_t*)jl_array_ptr_ref(found, found_i);
        jl_method_t *m = ml->func.method;
        if (m->source == NULL)  // TODO: generic implementations of generated functions
            continue;
        linfo = m->unspecialized;
        if (!linfo) {
            linfo = jl_get_specialized(m, (jl_value_t*)m->sig, jl_emptysvec);
            m->unspecialized = linfo;
            jl_gc_wb(m, linfo);
        }

        if (linfo->invoke != jl_fptr_trampoline)
            continue;
        src = m->source;
        // TODO: the `unspecialized` field is not yet world-aware, so we can't store
        // an inference result there.
        //src = jl_type_infer(&linfo, jl_world_counter, 1);
        //m->unspecialized = linfo;
        //jl_gc_wb(m, linfo);
        //if (linfo->trampoline != jl_fptr_trampoline)
        //    continue;

        // first try to create leaf signatures from the signature declaration and compile those
        _compile_all_union((jl_value_t*)ml->sig);
        // then also compile the generic fallback
        jl_compile_linfo(&linfo, (jl_code_info_t*)src, jl_world_counter, &jl_default_cgparams);
        assert(linfo->functionObjectsDecls.functionObject != NULL);
    }
    JL_GC_POP();
    jl_printf(JL_STDERR, "\n");
}

static int compile_all_enq__(jl_typemap_entry_t *ml, void *env)
{
    jl_array_t *found = (jl_array_t*)env;
    // method definition -- compile template field
    jl_method_t *m = ml->func.method;
    if (m->source &&
        (!m->unspecialized ||
         (m->unspecialized->functionObjectsDecls.functionObject == NULL &&
          m->unspecialized->invoke == jl_fptr_trampoline))) {
        // found a lambda that still needs to be compiled
        jl_array_ptr_1d_push(found, (jl_value_t*)ml);
    }
    return 1;
}


static void compile_all_enq_(jl_methtable_t *mt, void *env)
{
    jl_typemap_visitor(mt->defs, compile_all_enq__, env);
}

void jl_foreach_reachable_mtable(void (*visit)(jl_methtable_t *mt, void *env), void *env);

static void jl_compile_all_defs(void)
{
    // this "found" array will contain
    // TypeMapEntries for Methods and MethodInstances that need to be compiled
    jl_array_t *m = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&m);
    while (1) {
        jl_foreach_reachable_mtable(compile_all_enq_, m);
        size_t changes = jl_array_len(m);
        if (!changes)
            break;
        _compile_all_deq(m);
        jl_array_del_end(m, changes);
    }
    JL_GC_POP();
}

static int precompile_enq_all_cache__(jl_typemap_entry_t *l, void *closure)
{
    jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)l->sig);
    return 1;
}

static int precompile_enq_specialization_(jl_typemap_entry_t *l, void *closure)
{
    if (jl_is_method_instance(l->func.value) &&
            l->func.linfo->functionObjectsDecls.functionObject == NULL &&
            l->func.linfo->invoke != jl_fptr_const_return &&
            (l->func.linfo->inferred &&
             l->func.linfo->inferred != jl_nothing &&
             jl_ast_flag_inferred((jl_array_t*)l->func.linfo->inferred) &&
             !jl_ast_flag_inlineable((jl_array_t*)l->func.linfo->inferred)))
        jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)l->sig);
    return 1;
}

static int precompile_enq_all_specializations__(jl_typemap_entry_t *def, void *closure)
{
    jl_method_t *m = def->func.method;
    if (m->name == jl_symbol("__init__") && jl_is_dispatch_tupletype(m->sig)) {
        // ensure `__init__()` gets strongly-hinted, specialized, and compiled
        jl_specializations_get_linfo(m, m->sig, jl_emptysvec, jl_world_counter);
        jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)m->sig);
    }
    else {
        jl_typemap_visitor(def->func.method->specializations, precompile_enq_specialization_, closure);
    }
    return 1;
}

static void precompile_enq_all_specializations_(jl_methtable_t *mt, void *env)
{
    jl_typemap_visitor(mt->defs, precompile_enq_all_specializations__, env);
    jl_typemap_visitor(mt->cache, precompile_enq_all_cache__, env);
}

static void jl_compile_specializations(void)
{
    // this "found" array will contain function
    // type signatures that were inferred but haven't been compiled
    jl_array_t *m = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&m);
    jl_foreach_reachable_mtable(precompile_enq_all_specializations_, m);
    // Ensure stable ordering to make inference problems more reproducible (#29923)
    jl_sort_types((jl_value_t**)jl_array_data(m), jl_array_len(m));
    size_t i, l;
    for (i = 0, l = jl_array_len(m); i < l; i++) {
        jl_compile_hint((jl_tupletype_t*)jl_array_ptr_ref(m, i));
    }
    JL_GC_POP();
}

void jl_precompile(int all)
{
    if (all)
        jl_compile_all_defs();
    jl_compile_specializations();
}

#ifdef __cplusplus
}
#endif
