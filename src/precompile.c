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

// Infer the code of the image for the modules defined in `worklist`, to generate native code for
// unless `native` is false, and for a trimmed image with `trim` (see `Compiler.infer_image_code`)
static jl_svec_t *jl_infer_image_code(jl_array_t *worklist, int native, int trim) JL_CANSAFEPOINT
{
    JL_TIMING(INFERENCE, INFERENCE);
    if (!jl_compile_and_emit_func)
        jl_error("inference not available for generating compiled output");
    jl_task_t *ct = jl_current_task;
    int timed = (ct->reentrant_timing & 1) == 0;
    if (timed)
        ct->reentrant_timing |= 1;
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();

    jl_value_t **args;
    JL_GC_PUSHARGS(args, 4);
    args[0] = jl_compile_and_emit_func;
    args[1] = (jl_value_t*)worklist;
    args[2] = jl_box_bool(native);
    args[3] = jl_box_bool(trim);
    size_t last_age = ct->world_age;
    ct->world_age = jl_typeinf_world;
    jl_value_t *code = jl_apply(args, 4);
    ct->world_age = last_age;
    JL_TYPECHK(jl_infer_image, simplevector, code);
    if (jl_svec_len(code) != 5)
        jl_error("jl_infer_image: unexpected result from inference");
    JL_GC_POP();

    if (timed) {
        if (measure_compile_time_enabled) {
            uint64_t end = jl_hrtime();
            jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, end - compiler_start_time);
        }
        ct->reentrant_timing &= ~1ull;
    }
    return (jl_svec_t*)code;
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

// set once native code is generated, after which the IR for it is no longer kept
static int native_generated = 0;

// Generate the native code for the inferred `code` of an image (see `jl_infer_image_code`)
static void *jl_generate_native_code(jl_svec_t *code) JL_CANSAFEPOINT
{
    jl_value_t *codeinfos = jl_svecref(code, 0);
    jl_value_t *ci_order = jl_svecref(code, 1);
    if (codeinfos == jl_nothing)
        jl_error("the image was inferred without native code");
    JL_TYPECHK(jl_generate_native, array_any, codeinfos);
    JL_TYPECHK(jl_generate_native, array_any, ci_order);
    if (native_generated)
        jl_error("native code can only be generated once");
    native_generated = 1;
    void *native_code = jl_create_native((jl_array_t*)codeinfos, (jl_array_t*)ci_order, jl_options.incremental,
                                         jl_svecref(code, 4) == jl_true);
    // no more IR is retained to generate native code (the serializer leaves out what was)
    jl_set_precompile_keep_ir(0);
    return native_code;
}

// Serialize the heap of the image for the modules defined in `worklist`, from its inferred
// `code` and its native code (NULL if none), after `jl_wait_for_tasks_and_finalizers`. This
// returns the stream, and sets its checksum, and for a package image its dependencies and the
// position of their source text.
static ios_t *jl_serialize_heap_code(jl_array_t *worklist, jl_svec_t *code,
                                     void *native_code, const jl_image_options_t *opts,
                                     uint32_t *checksum, jl_array_t **udeps JL_REQUIRE_ROOTED_SLOT,
                                     int64_t *srctextpos) JL_CANSAFEPOINT
{
    jl_value_t *init_order = jl_svecref(code, 2);
    jl_value_t *ext_foreign_cis = jl_svecref(code, 3);
    JL_TYPECHK(jl_serialize_heap, array_any, init_order);
    if (ext_foreign_cis != jl_nothing)
        JL_TYPECHK(jl_serialize_heap, array_any, ext_foreign_cis);
    jl_array_t *image_worklist = jl_options.incremental ? worklist : NULL;
    jl_array_t *mod_array = NULL;
    JL_GC_PUSH1(&mod_array);
    int64_t checksumpos = 0;
    ios_t *s = jl_prepare_system_image(image_worklist, opts, &mod_array, udeps, srctextpos, &checksumpos);
    *checksum = jl_create_system_image(native_code, image_worklist, opts, s, checksumpos, mod_array,
                                       (jl_array_t*)init_order,
                                       ext_foreign_cis == jl_nothing ? NULL : (jl_array_t*)ext_foreign_cis);
    JL_GC_POP();

    for (size_t i = 0; i < jl_current_modules.size; i += 2) {
        if (jl_current_modules.table[i + 1] != HT_NOTFOUND) {
            jl_printf(JL_STDERR, "\nWARNING: detected unclosed module: ");
            jl_static_show(JL_STDERR, (jl_value_t*)jl_current_modules.table[i]);
            jl_printf(JL_STDERR, "\n  ** incremental compilation may be broken for this module **\n\n");
        }
    }
    return s;
}

// set once an image is written, so that none is written at exit
static int image_written = 0;

static jl_array_t *jl_image_worklist(void) JL_CANSAFEPOINT
{
    if (!jl_generating_output())
        jl_error("no image is written without an `--output-*` option");
    if (!jl_module_init_order)
        jl_error("no modules were defined to write an image for");
    return jl_module_init_order;
}

static jl_svec_t *jl_check_image_code(jl_value_t *code) JL_CANSAFEPOINT
{
    JL_TYPECHK(jl_check_image_code, simplevector, code);
    if (jl_svec_len(code) != 5)
        jl_error("expected the result of jl_infer_image");
    return (jl_svec_t*)code;
}

JL_DLLEXPORT jl_value_t *jl_infer_image(int native, int trim)
{
    return (jl_value_t*)jl_infer_image_code(jl_image_worklist(), native, trim);
}

JL_DLLEXPORT void *jl_generate_native(jl_value_t *code)
{
    jl_image_worklist();
    return jl_generate_native_code(jl_check_image_code(code));
}

// Returns svec(stream, checksum, udeps, srctextpos) (see `jl_serialize_heap_code`)
JL_DLLEXPORT jl_value_t *jl_serialize_heap(jl_value_t *code, void *native_code, int split,
                                           int keep_codegen_ir, int strip_ir, int strip_metadata,
                                           int compress)
{
    jl_array_t *worklist = jl_image_worklist();
    jl_svec_t *image_code = jl_check_image_code(code);
    if (split && !native_code)
        jl_error("a split image needs native code");
    if (compress && !split)
        jl_error("only a split image can be compressed");
    int trim = jl_svecref(image_code, 4) == jl_true;
    // without trimming, codegen keeps the roots of its code in method roots unless `--strip-ir`
    if (native_code && strip_ir && !trim && !jl_options.strip_ir)
        jl_error("stripping the IR of an image with native code needs `--strip-ir`");
    jl_image_options_t opts;
    opts.trim = trim;
    opts.strip_ir = strip_ir;
    opts.strip_metadata = strip_metadata;
    opts.keep_codegen_ir = keep_codegen_ir;
    opts.emit_split = split;
    opts.compress = compress;
    opts.cpu_target = jl_options.cpu_target;

    jl_task_t *ct = jl_current_task;
    jl_array_t *udeps = NULL;
    jl_value_t *result = NULL;
    JL_GC_PUSH2(&udeps, &result);
    uint32_t checksum = 0;
    int64_t srctextpos = 0;
    jl_wait_for_tasks_and_finalizers(ct);
    ios_t *s;
    JL_TRY {
        s = jl_serialize_heap_code(worklist, image_code, native_code, &opts, &checksum, &udeps,
                                   &srctextpos);
    }
    JL_CATCH {
        jl_gc_enable_finalizers(ct, 1);
        jl_rethrow();
    }
    jl_gc_enable_finalizers(ct, 1);
    result = jl_box_voidpointer(s);
    result = (jl_value_t*)jl_svec(4, result, jl_box_uint32(checksum),
                                  udeps ? (jl_value_t*)udeps : jl_nothing, jl_box_int64(srctextpos));
    JL_GC_POP();
    return result;
}

// Write the serialized heap `s` to `ji` and the native code to the other files (each NULL if not
// wanted). This consumes both.
JL_DLLEXPORT void jl_write_image(void *s, uint32_t checksum, int split, int compress,
                                 jl_value_t *udeps, int64_t srctextpos, void *native_code,
                                 const char *ji, const char *o, const char *bc,
                                 const char *unopt_bc, const char *asm_)
{
    if (native_code ? !(o || bc || unopt_bc || asm_) : (o || bc || unopt_bc || asm_))
        jl_error("native output files need native code, and native code needs one");
    if (split ? !ji : (ji != NULL) == (native_code != NULL))
        jl_error("a split image needs a `ji` file, and an image without native code needs one");
    jl_output_files_t files;
    files.ji = ji;
    files.o = o;
    files.bc = bc;
    files.unopt_bc = unopt_bc;
    files.s = asm_;
    jl_emit_compiler_output(&files, (ios_t*)s, native_code, checksum, split, compress,
                            jl_options.incremental, udeps == jl_nothing ? NULL : (jl_array_t*)udeps,
                            srctextpos);
    image_written = 1;
}

JL_DLLEXPORT void jl_write_compiler_output(void)
{
    if (!jl_generating_output() || image_written) {
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

    bool_t emit_native = jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputasm;
    jl_svec_t *code = jl_infer_image_code(worklist, emit_native, jl_options.trim != 0);
    jl_array_t *udeps = NULL;
    JL_GC_PUSH2(&code, &udeps);
    void *native_code = NULL;
    if (emit_native)
        native_code = jl_generate_native_code(code);
    else
        jl_set_precompile_keep_ir(0);

    jl_image_options_t opts;
    opts.trim = jl_svecref(code, 4) == jl_true;
    opts.strip_ir = jl_options.strip_ir;
    opts.strip_metadata = jl_options.strip_metadata;
    // the IR is only retained to emit an object file (see `jl_precompile_keep_ir`)
    opts.keep_codegen_ir = jl_options.outputo == NULL;
    opts.emit_split = jl_options.outputji && emit_native;
    opts.compress = jl_options.compress_sysimage;
    opts.cpu_target = jl_options.cpu_target;

    uint32_t checksum = 0;
    int64_t srctextpos = 0;
    ios_t *s = jl_serialize_heap_code(worklist, code, native_code, &opts, &checksum, &udeps,
                                      &srctextpos);

    jl_output_files_t files;
    files.ji = jl_options.outputji;
    files.o = jl_options.outputo;
    files.bc = jl_options.outputbc;
    files.unopt_bc = jl_options.outputunoptbc;
    files.s = jl_options.outputasm;
    jl_emit_compiler_output(&files, s, native_code, checksum, opts.emit_split, opts.compress,
                            jl_options.incremental, udeps, srctextpos);
    image_written = 1;
    JL_GC_POP();
    jl_gc_enable_finalizers(ct, 1);
}

#ifdef __cplusplus
}
#endif
