// This file is a part of Julia. License is MIT: https://julialang.org/license

// #include'd from interpreter.c

#ifndef NO_INTERP_BT

// Backtrace support
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_) || defined(_OS_WINDOWS_)
extern uintptr_t __start_jl_interpreter_frame_val;
uintptr_t __start_jl_interpreter_frame = (uintptr_t)&__start_jl_interpreter_frame_val;
extern uintptr_t __stop_jl_interpreter_frame_val;
uintptr_t __stop_jl_interpreter_frame = (uintptr_t)&__stop_jl_interpreter_frame_val;

#elif defined(_OS_DARWIN_)
extern uintptr_t __start_jl_interpreter_frame_val __asm("section$start$__TEXT$__jif");
uintptr_t __start_jl_interpreter_frame = (uintptr_t)&__start_jl_interpreter_frame_val;
extern uintptr_t __stop_jl_interpreter_frame_val __asm("section$end$__TEXT$__jif");
uintptr_t __stop_jl_interpreter_frame = (uintptr_t)&__stop_jl_interpreter_frame_val;

#endif

JL_DLLEXPORT int jl_is_interpreter_frame(uintptr_t ip)
{
    return __start_jl_interpreter_frame <= ip && ip <= __stop_jl_interpreter_frame;
}

#else // NO_INTERP_BT

JL_DLLEXPORT int jl_is_interpreter_frame(uintptr_t ip)
{
    return 0;
}

#endif

