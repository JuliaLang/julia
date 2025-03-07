#include <julia.h>
int main(int argc, char *argv[])
{
    julia_init(JL_IMAGE_IN_MEMORY);
    jl_exception_clear();
    jl_set_ARGS(argc, argv);
    jl_call1(jl_get_global(jl_main_module, jl_symbol("main")),
             jl_get_global(jl_core_module, jl_symbol("ARGS")));
    return 0;
}
