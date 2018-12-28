extern "C" {
JL_DLLEXPORT jl_value_t *jl_get_cpu_name(void)
{
    return jl_cstr_to_string("wasm");
}

extern "C" JL_DLLEXPORT
jl_value_t *jl_get_JIT(void)
{
    return jl_cstr_to_string("interpreter");
}
}
