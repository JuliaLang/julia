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
        size_t len = jl_array_len(udeps);
        ios_t srctext;
        for (size_t i = 0; i < len; i++) {
            jl_value_t *deptuple = jl_array_ptr_ref(udeps, i);
            jl_value_t *depmod = jl_fieldref(deptuple, 0);  // module
            // Dependencies declared with `include_dependency` are excluded
            // because these may not be Julia code (and could be huge)
            if (depmod != (jl_value_t*)jl_main_module) {
                jl_value_t *dep = jl_fieldref(deptuple, 1);  // file abspath
                const char *depstr = jl_string_data(dep);
                if (!depstr[0])
                    continue;
                ios_t *srctp = ios_file(&srctext, depstr, 1, 0, 0, 0);
                if (!srctp) {
                    jl_printf(JL_STDERR, "WARNING: could not cache source text for \"%s\".\n",
                            jl_string_data(dep));
                    continue;
                }
                size_t slen = jl_string_len(dep);
                write_int32(f, slen);
                ios_write(f, depstr, slen);
                posfile = ios_pos(f);
                write_uint64(f, 0);   // placeholder for length of this file in bytes
                uint64_t filelen = (uint64_t) ios_copyall(f, &srctext);
                ios_close(&srctext);
                ios_seek(f, posfile);
                write_uint64(f, filelen);
                ios_seek_end(f);
            }
        }
    }
    write_int32(f, 0); // mark the end of the source text
}

JL_DLLEXPORT void jl_write_compiler_output(void)
{
    if (!jl_generating_output()) {
        return;
    }

    jl_task_wait_empty();

    if (!jl_module_init_order) {
        jl_printf(JL_STDERR, "WARNING: --output requested, but no modules defined during run\n");
        return;
    }

    jl_array_t *worklist = jl_module_init_order;
    jl_array_t *udeps = NULL;
    JL_GC_PUSH2(&worklist, &udeps);
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
                tt = jl_apply_tuple_type_v(&tt, 1);
                jl_compile_hint((jl_tupletype_t*)tt);
                JL_GC_POP();
            }
        }
    }

    assert(jl_precompile_toplevel_module == NULL);
    void *native_code = NULL;

    bool_t emit_native = jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputasm;

    bool_t emit_split = jl_options.outputji && emit_native;

    ios_t *s = NULL;
    ios_t *z = NULL;
    int64_t srctextpos = 0 ;
    jl_create_system_image(emit_native ? &native_code : NULL,
                           jl_options.incremental ? worklist : NULL,
                           emit_split, &s, &z, &udeps, &srctextpos);

    if (!emit_split)
        z = s;

    // jl_dump_native writes the clone_targets into `s`
    // We need to postpone the srctext writing after that.
    if (native_code) {
        jl_dump_native(native_code,
                        jl_options.outputbc,
                        jl_options.outputunoptbc,
                        jl_options.outputo,
                        jl_options.outputasm,
                        (const char*)z->buf, (size_t)z->size, s);
        jl_postoutput_hook();
    }

    if ((jl_options.outputji || emit_native) && jl_options.incremental) {
        write_srctext(s, udeps, srctextpos);
    }

    if (jl_options.outputji) {
        ios_t f;
        if (ios_file(&f, jl_options.outputji, 1, 1, 1, 1) == NULL)
            jl_errorf("cannot open system image file \"%s\" for writing", jl_options.outputji);
        ios_write(&f, (const char*)s->buf, (size_t)s->size);
        ios_close(&f);
    }

    if (s) {
        ios_close(s);
        free(s);
    }

    if (emit_split) {
        ios_close(z);
        free(z);
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

#ifdef __cplusplus
}
#endif
