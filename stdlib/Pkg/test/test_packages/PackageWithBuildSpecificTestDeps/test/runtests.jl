# This file is a part of Julia. License is MIT: https://julialang.org/license

using Example
using PackageWithBuildSpecificTestDeps
using Test

@test PackageWithBuildSpecificTestDeps.f(3) == 3
