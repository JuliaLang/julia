# This file is a part of Julia. License is MIT: https://julialang.org/license

module Foo
include("SubFoo.jl")
import Bar, Baz, Qux
this = "Foo1"
which = "path"
end
