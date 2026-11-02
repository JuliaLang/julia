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

static void write_srctext(ios_t *f, jl_array_t *udeps, int64_t srctextpos) JL_CANSAFEPOINT {
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

// Wait until no other task is running and run the pending finalizers, then disable finalizers
// on this thread
static void jl_wait_for_tasks_and_finalizers(jl_task_t *ct) JL_CANSAFEPOINT
{
    jl_task_wait_empty(); // wait for most work to finish (except possibly finalizers)
    jl_gc_collect(JL_GC_FULL);
    jl_gc_collect(JL_GC_INCREMENTAL); // sweep finalizers
    jl_gc_enable_finalizers(ct, 0); // now disable finalizers, as they could schedule more work or make other unexpected changes to reachability
    jl_task_wait_empty(); // then make sure we are the only thread alive that could be running user code past here
}

// Generate the native code for the image, and collect the CodeInstances of foreign interpreters
// to add to a package image in `ext_foreign_cis`
static void *jl_create_native_for_output(jl_array_t *worklist, jl_array_t *module_init_order,
                                         jl_array_t *ext_foreign_cis, int trim, int compile_all) JL_CANSAFEPOINT
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    if (worklist) {
        if (jl_precompile_suppressed())
            worklist = (jl_array_t*)jl_an_empty_vec_any;
        return jl_create_native(NULL, 0, 1, world, NULL, worklist, 0, module_init_order, ext_foreign_cis);
    }
    jl_array_t *mod_array = jl_get_loaded_modules();
    JL_GC_PUSH1(&mod_array);
    void *native_code = jl_create_native(NULL, trim, 0, world, mod_array, NULL, compile_all,
                                         module_init_order, ext_foreign_cis);
    JL_GC_POP();
    return native_code;
}

// the files to write an image and its native code to (each NULL if not wanted)
typedef struct {
    const char *ji;        // the serialized image (otherwise it goes in the native code)
    const char *o;         // native code as an object file
    const char *bc;        // ... as bitcode
    const char *unopt_bc;  // ... as unoptimized bitcode
    const char *s;         // ... as assembly
} jl_output_files_t;

// Write the image in `s` and its native code to `files`, and for a package image the source
// text of its dependencies
static void jl_emit_compiler_output(const jl_output_files_t *files, ios_t *s, void *native_code,
                                    uint32_t checksum, bool_t emit_split, int comp,
                                    int incremental, jl_array_t *udeps, int64_t srctextpos) JL_CANSAFEPOINT
{
    const char *outputji = files->ji;
    ios_t f;

    if (outputji) {
        if (ios_file(&f, outputji, 1, 1, 1, 1) == NULL)
            jl_errorf("cannot open system image file \"%s\" for writing", outputji);
        // It would be a waste to allocate a huge buffer only to write it all
        // immediately to the file.
        ios_bufmode(&f, bm_none);
        ios_write(&f, (const char *)s->buf, (size_t)s->size);
        ios_close(s);
        free(s);
        ios_bufmode(&f, bm_block);
    }

    if (native_code) {
        const char *unpack_func =
            emit_split ? (comp ? "jl_image_unpack_split_zstd" : "jl_image_unpack_split") :
                         (comp ? "jl_image_unpack_zstd" : "jl_image_unpack_uncomp");

        // jl_dump_native will close and free s when appropriate
        // this is a horrible abstraction, but
        // this helps reduce live memory significantly
        jl_dump_native(native_code, files->bc, files->unopt_bc, files->o, files->s,
                       outputji ? NULL : s, checksum, unpack_func, NULL);
        jl_postoutput_hook();
    }

    if (outputji) {
        if (incremental) {
            write_srctext(&f, udeps, srctextpos);
        }
        ios_close(&f);
    }
}

JL_DLLEXPORT void jl_write_compiler_output(void)
{
    if (!jl_generating_output()) {
        return;
    }

    jl_task_t *ct = jl_current_task;
    // the process exits next, so wait for all other work to finish
    jl_wait_for_tasks_and_finalizers(ct);

    jl_array_t *worklist = jl_module_init_order;
    if (!worklist) {
        jl_printf(JL_STDERR, "WARNING: --output requested, but no modules defined during run\n");
        jl_gc_enable_finalizers(ct, 1);
        return;
    }

    jl_array_t *udeps = NULL, *init_order = NULL, *ext_foreign_cis = NULL, *mod_array = NULL;
    JL_GC_PUSH5(&worklist, &udeps, &init_order, &ext_foreign_cis, &mod_array);
    // the modules with an `__init__`, in order
    init_order = jl_alloc_vec_any(0);
    int i, l = jl_array_nrows(worklist);
    for (i = 0; i < l; i++) {
        jl_value_t *m = jl_array_ptr_ref(worklist, i);
        jl_value_t *f = jl_get_global((jl_module_t*)m, jl_symbol("__init__"));
        if (f) {
            jl_array_ptr_1d_push(init_order, m);
        }
    }

    void *native_code = NULL;
    bool_t emit_native = jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputasm;
    bool_t emit_split = jl_options.outputji && emit_native;
    int comp = jl_options.compress_sysimage;

    jl_array_t *image_worklist = jl_options.incremental ? worklist : NULL;
    if (emit_native) {
        ext_foreign_cis = jl_alloc_vec_any(0);
        native_code = jl_create_native_for_output(image_worklist, init_order, ext_foreign_cis,
                                                  jl_options.trim,
                                                  jl_options.compile_enabled == JL_OPTIONS_COMPILE_ALL);
    }
    // no more IR is retained to generate native code (the serializer leaves out what was)
    jl_set_precompile_keep_ir(0);

    jl_image_options_t opts;
    opts.trim = jl_options.trim;
    opts.strip_ir = jl_options.strip_ir;
    opts.strip_metadata = jl_options.strip_metadata;
    // the IR is only retained to emit an object file (see `jl_precompile_keep_ir`)
    opts.keep_codegen_ir = jl_options.outputo == NULL;
    opts.emit_split = emit_split;
    opts.compress = comp;
    opts.cpu_target = jl_options.cpu_target;

    int64_t srctextpos = 0, checksumpos = 0;
    ios_t *s = jl_prepare_system_image(image_worklist, &opts, &mod_array, &udeps, &srctextpos, &checksumpos);
    uint32_t checksum =
        jl_create_system_image(native_code, image_worklist, &opts, s, checksumpos, mod_array,
                               init_order, ext_foreign_cis);

    jl_output_files_t files;
    files.ji = jl_options.outputji;
    files.o = jl_options.outputo;
    files.bc = jl_options.outputbc;
    files.unopt_bc = jl_options.outputunoptbc;
    files.s = jl_options.outputasm;
    jl_emit_compiler_output(&files, s, native_code, checksum, emit_split, comp,
                            jl_options.incremental, udeps, srctextpos);

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
