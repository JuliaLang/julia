# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using SubModule

@test SubModule.f() == 1
