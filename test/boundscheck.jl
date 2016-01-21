# This file is a part of Julia. License is MIT: http://julialang.org/license

module TestBoundsCheck

using Base.Test

# test for boundscheck block eliminated at same level
@inline function A1()
    r = 0
    @boundscheck r += 1
    return r
end

@noinline function A1_noinline()
    r = 0
    @boundscheck r += 1
    return r
end

function A1_inbounds()
    r = 0
    @inbounds begin
        @boundscheck r += 1
    end
    return r
end

@test A1() == 1
@test A1_inbounds() == 0

# test for boundscheck block eliminated one layer deep, if the called method is inlined
@inline function A2()
    r = A1()+1
    return r
end

function A2_inbounds()
    @inbounds r = A1()+1
    return r
end

function A2_notinlined()
    @inbounds r = A1_noinline()+1
    return r
end

Base.@propagate_inbounds function A2_propagate_inbounds()
    r = A1()+1
    return r
end

@test A2() == 2
@test A2_inbounds() == 1
@test A2_notinlined() == 2
@test A2_propagate_inbounds() == 2

# test boundscheck NOT eliminated two layers deep, unless propagated

function A3()
    r = A2()+1
    return r
end

function A3_inbounds()
    @inbounds r = A2()+1
    return r
end

function A3_inbounds2()
    @inbounds r = A2_propagate_inbounds()+1
    return r
end

@test A3() == 3
@test A3_inbounds() == 3
@test A3_inbounds2() == 2

# swapped nesting order of @boundscheck and @inbounds
function A1_nested()
    r = 0
    @boundscheck @inbounds r += 1
    return r
end

@test A1_nested() == 1

# elide a throw
cb(x) = x > 0 || throw("cb:error")

function B1()
    y = [1,2,3]
    @inbounds begin
        @boundscheck cb(0)
    end
    return 0
end

@test B1() == 0

# elide a simple branch
cond(x) = x > 0 ? x : -x

function B2()
    y = [1,2,3]
    @inbounds begin
        @boundscheck cond(0)
    end
    return 0
end

@test B2() == 0

end
