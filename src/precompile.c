// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  precompile.c
  Generating compiler output artifacts (object files, etc.)
*/

#include <stdlib.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"
#include "serialize.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT int jl_generating_output(void)
{
    return jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputji || jl_options.outputasm;
}

void write_srctext(ios_t *f, jl_array_t *udeps, int64_t srctextpos) {
    // Write the source-text for the dependent files
    if (udeps) {
        // Go back and update the source-text position to point to the current position
        int64_t posfile = ios_pos(f);
        ios_seek(f, srctextpos);
        write_uint64(f, posfile);
        ios_seek_end(f);
        // Each source-text file is written as
        //   int32: length of abspath
        //   char*: abspath
        //   uint64: length of src text
        //   char*: src text
        // At the end we write int32(0) as a terminal sentinel.
        size_t len = jl_array_nrows(udeps);
        ios_t srctext;
        jl_value_t *replace_depot_func = NULL;
        jl_value_t *normalize_depots_func = NULL;
        jl_value_t *deptuple = NULL;
        jl_value_t *depots = NULL;
        jl_task_t *ct = jl_current_task;
        size_t last_age = ct->world_age;
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        JL_GC_PUSH4(&deptuple, &depots, &replace_depot_func, &normalize_depots_func);
        replace_depot_func = jl_eval_global_var(jl_base_module, jl_symbol("replace_depot_path"), jl_current_task->world_age);
        normalize_depots_func = jl_eval_global_var(jl_base_module, jl_symbol("normalize_depots_for_relocation"), jl_current_task->world_age);
        depots = jl_apply(&normalize_depots_func, 1);
        jl_datatype_t *deptuple_p[5] = {jl_module_type, jl_string_type, jl_uint64_type, jl_uint32_type, jl_float64_type};
        jl_value_t *jl_deptuple_type = jl_apply_tuple_type_v((jl_value_t**)deptuple_p, 5);
        JL_GC_PROMISE_ROOTED(jl_deptuple_type);
#define jl_is_deptuple(v) (jl_typeis((v), jl_deptuple_type))
        for (size_t i = 0; i < len; i++) {
            deptuple = jl_array_ptr_ref(udeps, i);
            jl_value_t *depmod = jl_fieldref_noalloc(deptuple, 0);  // module
            // Dependencies declared with `include_dependency` are excluded
            // because these may not be Julia code (and could be huge)
            JL_TYPECHK(write_srctext, deptuple, deptuple);
            if (depmod != (jl_value_t*)jl_main_module) {
                jl_value_t *abspath = jl_fieldref_noalloc(deptuple, 1);  // file abspath
                const char *abspathstr = jl_string_data(abspath);
                if (!abspathstr[0])
                    continue;
                ios_t *srctp = ios_file(&srctext, abspathstr, 1, 0, 0, 0);
                if (!srctp) {
                    jl_printf(JL_STDERR, "WARNING: could not cache source text for \"%s\".\n",
                              abspathstr);
                    continue;
                }

                jl_value_t *replace_depot_args[3];
                replace_depot_args[0] = replace_depot_func;
                replace_depot_args[1] = abspath;
                replace_depot_args[2] = depots;
                jl_value_t *depalias = (jl_value_t*)jl_apply(replace_depot_args, 3);

                size_t slen = jl_string_len(depalias);
                write_int32(f, slen);
                ios_write(f, jl_string_data(depalias), slen);
                posfile = ios_pos(f);
                write_uint64(f, 0);   // placeholder for length of this file in bytes
                uint64_t filelen = (uint64_t) ios_copyall(f, &srctext);
                ios_close(&srctext);
                ios_seek(f, posfile);
                write_uint64(f, filelen);
                ios_seek_end(f);
            }
        }
        ct->world_age = last_age;
#undef jl_is_deptuple
        JL_GC_POP();
    }
    write_int32(f, 0); // mark the end of the source text
}

JL_DLLEXPORT void jl_write_compiler_output(void)
{
    if (!jl_generating_output()) {
        return;
    }

    jl_task_wait_empty(); // wait for most work to finish (except possibly finalizers)
    jl_gc_collect(JL_GC_FULL);
    jl_gc_collect(JL_GC_INCREMENTAL); // sweep finalizers
    jl_task_t *ct = jl_current_task;
    jl_gc_enable_finalizers(ct, 0); // now disable finalizers, as they could schedule more work or make other unexpected changes to reachability
    jl_task_wait_empty(); // then make sure we are the only thread alive that could be running user code past here

    if (!jl_module_init_order) {
        jl_printf(JL_STDERR, "WARNING: --output requested, but no modules defined during run\n");
        return;
    }

    jl_array_t *worklist = jl_module_init_order;
    jl_array_t *udeps = NULL;
    JL_GC_PUSH2(&worklist, &udeps);
    jl_module_init_order = jl_alloc_vec_any(0);
    int i, l = jl_array_nrows(worklist);
    for (i = 0; i < l; i++) {
        jl_value_t *m = jl_array_ptr_ref(worklist, i);
        jl_value_t *f = jl_get_global((jl_module_t*)m, jl_symbol("__init__"));
        if (f) {
            jl_array_ptr_1d_push(jl_module_init_order, m);
            int setting = jl_get_module_compile((jl_module_t*)m);
            if ((setting != JL_OPTIONS_COMPILE_OFF && (jl_options.trim ||
                (setting != JL_OPTIONS_COMPILE_MIN)))) {
                // TODO: this would be better handled if moved entirely to jl_precompile
                // since it's a slightly duplication of effort
                jl_value_t *tt = jl_is_type(f) ? (jl_value_t*)jl_wrap_Type(f) : jl_typeof(f);
                JL_GC_PUSH1(&tt);
                tt = jl_apply_tuple_type_v(&tt, 1);
                jl_compile_hint((jl_tupletype_t*)tt);
                if (jl_options.trim)
                    jl_add_entrypoint((jl_tupletype_t*)tt);
                JL_GC_POP();
            }
        }
    }

    void *native_code = NULL;

    bool_t emit_native = jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputasm;

    const char *outputji = jl_options.outputji;

    bool_t emit_split = outputji && emit_native;

    ios_t *s = NULL;
    ios_t *z = NULL;
    int64_t srctextpos = 0 ;
    jl_create_system_image(emit_native ? &native_code : NULL,
                           jl_options.incremental ? worklist : NULL,
                           emit_split, &s, &z, &udeps, &srctextpos);

    if (!emit_split)
        z = s;

    ios_t f;

    if (outputji) {
        if (ios_file(&f, outputji, 1, 1, 1, 1) == NULL)
            jl_errorf("cannot open system image file \"%s\" for writing", outputji);
        ios_write(&f, (const char *)s->buf, (size_t)s->size);
        ios_close(s);
        free(s);
    }

    // jl_dump_native writes the clone_targets into `s`
    // We need to postpone the srctext writing after that.
    if (native_code) {
        ios_t *targets = outputji ? &f : NULL;
        // jl_dump_native will close and free z when appropriate
        // this is a horrible abstraction, but
        // this helps reduce live memory significantly
        jl_dump_native(native_code,
                        jl_options.outputbc,
                        jl_options.outputunoptbc,
                        jl_options.outputo,
                        jl_options.outputasm,
                        z, targets, NULL);
        jl_postoutput_hook();
    }

    if (outputji) {
        if (jl_options.incremental) {
            write_srctext(&f, udeps, srctextpos);
        }
        ios_close(&f);
    }

    for (size_t i = 0; i < jl_current_modules.size; i += 2) {
        if (jl_current_modules.table[i + 1] != HT_NOTFOUND) {
            jl_printf(JL_STDERR, "\nWARNING: detected unclosed module: ");
            jl_static_show(JL_STDERR, (jl_value_t*)jl_current_modules.table[i]);
            jl_printf(JL_STDERR, "\n  ** incremental compilation may be broken for this module **\n\n");
        }
    }
    if (jl_options.trim) {
        exit(0); // Some finalizers need to run and we've blown up the bindings table
        // TODO: Is this still needed
    }
    JL_GC_POP();
    jl_gc_enable_finalizers(ct, 1);
}

#ifdef __cplusplus
}
#endif
