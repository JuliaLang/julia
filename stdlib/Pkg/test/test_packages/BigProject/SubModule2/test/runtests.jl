# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using SubModule2

@test SubModule2.f() == 1
