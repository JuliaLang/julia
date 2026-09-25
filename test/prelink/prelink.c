// This file is a part of Julia. License is MIT: https://julialang.org/license

// The C entry of a program that holds its own system image. The image is
// linked into this program, and the program is linked `-no-pie`, so the image
// is at one address at every start and can be pre-relocated.
//
// Nothing here calls the parser: `jl_eval_string` would compile it to read one
// line, which costs more than everything this program does.

#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

#include "julia.h"
#include "uv.h"

JULIA_DEFINE_FAST_TLS

int main(int argc, char *argv[])
{
    argv = uv_setup_args(argc, argv);

    // The arguments after `--julia-args` are the runtime's, not the program's.
    int program_argc = argc;
    for (int i = 0; i < argc; i++) {
        if (strcmp(argv[i], "--julia-args") == 0) {
            program_argc = i;
            break;
        }
    }
    int julia_argc = argc - program_argc;
    if (julia_argc > 0) {
        argv[program_argc] = argv[0];
        char **julia_argv = &argv[program_argc];
        jl_parse_opts(&julia_argc, &julia_argv);
    }

    // The image is this program: the loader finds it through the handle of the
    // running program rather than through a file.
    void *self = dlopen(NULL, RTLD_NOW | RTLD_NOLOAD | RTLD_LOCAL);
    if (self == NULL) {
        fprintf(stderr, "cannot open this program as a library: %s\n", dlerror());
        return 1;
    }
    jl_init_with_image_handle(self);
    jl_set_ARGS(program_argc, argv);

    jl_module_t *module = (jl_module_t*)jl_get_global(jl_main_module, jl_symbol("Prelinked"));
    if (module == NULL || !jl_is_module(module)) {
        fprintf(stderr, "the image holds no module Prelinked\n");
        return 1;
    }
    jl_value_t *entry = jl_get_function(module, "julia_main");
    if (entry == NULL) {
        fprintf(stderr, "the image holds no Prelinked.julia_main\n");
        return 1;
    }
    jl_value_t *result = jl_call0(entry);
    if (jl_exception_occurred()) {
        jl_call2(jl_get_function(jl_base_module, "showerror"),
                 jl_stderr_obj(), jl_exception_occurred());
        jl_printf(jl_stderr_stream(), "\n");
        return 1;
    }
    int code = (result != NULL && jl_typeis(result, jl_int32_type)) ? jl_unbox_int32(result) : 1;
    jl_atexit_hook(code);
    return code;
}
