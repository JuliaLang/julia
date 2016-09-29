# This file is a part of Julia. License is MIT: http://julialang.org/license

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
    end
end

# Test that scope rules match regular for
let j=4
    # Use existing local variable.
    @simd for j=1:0 end
    @test j==4
    @simd for j=1:3 end
    @test j==3

    # Use global variable
    global simd_glob = 4
    @simd for simd_glob=1:0 end
    @test simd_glob==4
    @simd for simd_glob=1:3 end
    @test simd_glob==3

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
@test_throws SimdError eval(:(begin
    @simd for x = 1:10
        x == 1 && break
    end
end))

@test_throws SimdError eval(:(begin
    @simd for x = 1:10
        x < 5 && continue
    end
end))

@test_throws SimdError eval(:(begin
    @simd for x = 1:10
        x == 1 || @goto exit_loop
    end
    @label exit_loop
end))

# @simd with cartesian iteration
function simd_cartesian_range!(indexes, crng)
    @simd for I in crng
        push!(indexes, I)
    end
    indexes
end

crng = CartesianRange(CartesianIndex{4}(2,0,1,3),
                      CartesianIndex{4}(4,1,1,5))
indexes = simd_cartesian_range!(Array{eltype(crng)}(0), crng)
@test indexes == vec(collect(crng))

crng = CartesianRange(CartesianIndex{2}(-1,1),
                      CartesianIndex{2}(1,3))
indexes = simd_cartesian_range!(Array{eltype(crng)}(0), crng)
@test indexes == vec(collect(crng))

crng = CartesianRange(CartesianIndex{2}(-1,1),
                      CartesianIndex{2}(-1,3))
indexes = simd_cartesian_range!(Array{eltype(crng)}(0), crng)
@test indexes == vec(collect(crng))

crng = CartesianRange(CartesianIndex{1}(2),
                      CartesianIndex{1}(4))
indexes = simd_cartesian_range!(Array{eltype(crng)}(0), crng)
@test indexes == collect(crng)

crng = CartesianRange(CartesianIndex{0}(),
                      CartesianIndex{0}())
indexes = simd_cartesian_range!(Array{eltype(crng)}(0), crng)
@test indexes == vec(collect(crng))

# @simd with array as "range"
# issue #13869
function simd_sum_over_array(a)
    s = zero(eltype(a))
    @inbounds @simd for x in a
        s += x
    end
    s
end
@test 2001000 == simd_sum_over_array(collect(1:2000))
@test 2001000 == simd_sum_over_array(Float32[i+j*500 for i=1:500, j=0:3])

