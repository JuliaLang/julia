
module TestBoundsCheck

using Base.Test

# test for boundscheck block eliminated at same level
@inline function A1()
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

# test for boundscheck block eliminated one layer deep
function A2()
    r = A1()+1
    return r
end

function A2_inbounds()
    @inbounds r = A1()+1
    return r
end

@test A2() == 2
@test A2_inbounds() == 1

# test boundscheck NOT eliminated two layers deep

function A3()
    r = A2()+1
    return r
end

function A3_inbounds()
    @inbounds r = A2()+1
    return r
end

@test A3() == 3
@test A3_inbounds() == 3

@inline B() = A1() + 1
function A3_inbounds2()
    @inbounds r = B()+1
    return r
end

@test A3_inbounds2() == 3

# swapped nesting order of @boundscheck and @inbounds
function A1_nested()
    r = 0
    @boundscheck @inbounds r += 1
    return r
end

@test A1_nested() == 1

end
