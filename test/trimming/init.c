#define _GNU_SOURCE
#include <dlfcn.h>
#include <julia.h>

__attribute__((constructor)) void static_init(void)
{
    if (jl_is_initialized())
        return;
    jl_init_with_image_handle((void *)RTLD_DEFAULT);
    jl_exception_clear();
}
