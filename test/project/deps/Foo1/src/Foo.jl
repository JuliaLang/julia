# This file is a part of Julia. License is MIT: https://julialang.org/license

module Foo
include("SubFoo1.jl")
include(joinpath("subdir", "SubFoo2.jl"))
import Bar, Baz, Qux
this = "Foo1"
which = "path"
end
