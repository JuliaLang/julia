# This file is a part of Julia. License is MIT: http://julialang.org/license

# test for Core.Inference correctness and precision

const Bottom = Union{}

# issue 9770
@noinline x9770() = false
function f9770(x)
    if x9770()
        g9770(:a, :foo)
    else
        x
    end
end
function g9770(x,y)
   if isa(y, Symbol)
       f9770(x)
   else
       g9770(:a, :foo)
   end
end
@test g9770(:a, "c") === :a
@test g9770(:b, :c) === :b
