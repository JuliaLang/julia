// This file is a part of Julia. License is MIT: https://julialang.org/license

// A forwarding target in another shared library than the trampolines, as
// libjulia-internal is for libjulia: it has its own data, and on some
// architectures (PPC64LE) its own TOC register value.

static volatile int other_lib_data = 1000;

int other_lib_target(int x)
{
    return x + other_lib_data;
}
