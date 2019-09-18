# This file is a part of Julia. License is MIT: https://julialang.org/license

function simd_loop_example_from_manual(x, y, z)
    s = zero(eltype(z))
    n = min(length(x),length(y),length(z))
    @simd for i in 1:n
        @inbounds begin
            z[i] = x[i]-y[i]
            s += z[i]*z[i]
        end
    end
    s
end

function simd_loop_axpy!(a, X, Y)
    @simd ivdep for i in eachindex(X)
        @inbounds Y[i] += a*X[i]
    end
    return Y
end

function simd_loop_with_multiple_reductions(x, y, z)
    # Use non-zero initial value to make sure reduction values include it.
    (s,t) = (one(eltype(x)),one(eltype(y)))
    @simd for i in 1:length(z)
        @inbounds begin
            s += x[i]
            t += 2*y[i]
            s += z[i]   # Two reductions go into s
        end
    end
    (s,t)
end

for T in [Int32,Int64,Float32,Float64]
    # Try various lengths to make sure "remainder loop" works
    for n in [0,1,2,3,4,255,256,257]
        local n, a, b, c, s, t
        # Dataset chosen so that results will be exact with only 24 bits of mantissa
        a = convert(Array{T},[2*j+1 for j in 1:n])
        b = convert(Array{T},[3*j+2 for j in 1:n])
        c = convert(Array{T},[5*j+3 for j in 1:n])
        s = simd_loop_example_from_manual(a,b,c)

        @test a==[2*j+1 for j in 1:n]
        @test b==[3*j+2 for j in 1:n]
        @test c==[-j-1 for j in 1:n]
        @test s==sum(c.*c)
        (s,t) = simd_loop_with_multiple_reductions(a,b,c)
        @test s==sum(a)+sum(c)+1
        @test t==2*sum(b)+1

        X = ones(T, n)
        Y = zeros(T, n)
        simd_loop_axpy!(T(2), X, Y)
        @test all(y->y==T(2), Y)
    end
end

# Test that scope rules match regular for
let j=4, k=4
    # Use existing local variable.
    @simd for j=1:0 end
          for k=1:0 end
    @test j==k
    @simd for j=1:3 end
          for k=1:3 end
    @test j==k

    # Use global variable
    global simd_glob = 4
    global glob = 4
    @simd for simd_glob=1:0 end
          for      glob=1:0 end
    @test simd_glob==glob
    @simd for simd_glob=1:3 end
          for      glob=1:3 end
    @test simd_glob==glob

    # Index that is local to loop
    @simd for simd_loop_local=1:0 end
    simd_loop_local_present = true
    try
        simd_loop_local += 1
    catch
        simd_loop_local_present = false
    end
    @test !simd_loop_local_present
end

import Base.SimdLoop.SimdError

# Test that @simd rejects inner loop body with invalid control flow statements
# issue #8613
macro test_throws(ty, ex)
    return quote
        Test.@test_throws $(esc(ty)) try
            $(esc(ex))
        catch err
            @test err isa LoadError
            @test err.file === $(string(__source__.file))
            @test err.line === $(__source__.line + 1)
            rethrow(err.error)
        end
    end
end

@test_throws SimdError("break is not allowed inside a @simd loop body") @macroexpand begin
    @simd for x = 1:10
        x == 1 && break
    end
end

@test_throws SimdError("continue is not allowed inside a @simd loop body") @macroexpand begin
    @simd for x = 1:10
        x < 5 && continue
    end
end

@test_throws SimdError("@goto is not allowed inside a @simd loop body") @macroexpand begin
    @simd for x = 1:10
        x == 1 || @goto exit_loop
    end
    @label exit_loop
end

# @simd with cartesian iteration
function simd_cartesian_range!(indices, crng)
    @simd for I in crng
        push!(indices, I)
    end
    indices
end

crng = CartesianIndices(map(Base.Slice, (2:4, 0:1, 1:1, 3:5)))
indices = simd_cartesian_range!(Vector{eltype(crng)}(), crng)
@test indices == vec(collect(crng))

crng = CartesianIndices(map(Base.Slice, (-1:1, 1:3)))
indices = simd_cartesian_range!(Vector{eltype(crng)}(), crng)
@test indices == vec(collect(crng))

crng = CartesianIndices(map(Base.Slice, (-1:-1, 1:3)))
indices = simd_cartesian_range!(Vector{eltype(crng)}(), crng)
@test indices == vec(collect(crng))

crng = CartesianIndices(map(Base.Slice, (2:4,)))
indices = simd_cartesian_range!(Vector{eltype(crng)}(), crng)
@test indices == collect(crng)

crng = CartesianIndices(())
indices = simd_cartesian_range!(Vector{eltype(crng)}(), crng)
@test indices == vec(collect(crng))

# @simd with array as "range"
# issue #13869
function simd_sum_over_array(a)
    s = zero(eltype(a))
    @inbounds @simd for x in a
        s += x
    end
    s
end
@test 2001000 == simd_sum_over_array(Vector(1:2000))
@test 2001000 == simd_sum_over_array(Float32[i+j*500 for i=1:500, j=0:3])

#Opt out of simd
struct iter31113{T}
    parent::T
end
Base.iterate(it::iter31113, args...) = iterate(it.parent, args...)
Base.eltype(it::iter31113) = eltype(it.parent)
Base.SimdLoop.simd_index(v::iter31113, j, i) = j
Base.SimdLoop.simd_inner_length(v::iter31113, j) = 1
Base.SimdLoop.simd_outer_range(v::iter31113) = v
@test 2001000 == simd_sum_over_array(iter31113(Vector(1:2000)))
