#include <julia.h>

__attribute__((constructor)) void static_init(void)
{
    if (jl_is_initialized())
        return;
    julia_init(JL_IMAGE_IN_MEMORY);
    jl_exception_clear();
}
