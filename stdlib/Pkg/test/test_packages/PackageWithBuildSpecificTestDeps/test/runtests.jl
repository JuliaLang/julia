using Example
using PackageWithBuildSpecificTestDeps
using Test

@test PackageWithBuildSpecificTestDeps.f(3) == 3