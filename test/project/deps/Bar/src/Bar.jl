# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)
module Bar
import Baz, Foo
this = "Bar"
end
