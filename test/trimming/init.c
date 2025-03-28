#include <julia.h>
int juliac_main(void*);
int main(int argc, char *argv[])
{
    julia_init(JL_IMAGE_IN_MEMORY);
    jl_exception_clear();
    jl_set_ARGS(argc, argv);
    return juliac_main(jl_get_global(jl_core_module, jl_symbol("ARGS")));
}
