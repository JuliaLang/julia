# This file is a part of Julia. License is MIT: https://julialang.org/license

module Foo
include("SubFoo1.jl")
include(joinpath("subdir", "SubFoo2.jl"))
import Bar, Baz, Qux

# This tests pkgdir and pathof when executed in toplevel
# on both the package itself and on its dependencies
@assert isdir(pkgdir(Foo))
@assert isdir(pkgdir(Bar))
@assert isfile(pathof(Foo))
@assert isfile(pathof(Bar))

this = "Foo1"
which = "path"
end
