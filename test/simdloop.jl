# This file is a part of Julia. License is MIT: http://julialang.org/license

# Check that the julia process is running with options that will allow vectorization:
@test Base.JLOptions().check_bounds != 1
@test Base.JLOptions().can_inline == 1
@test Base.JLOptions().fast_math <= 1

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
indexes = simd_cartesian_range!(Array(eltype(crng), 0), crng)
@test indexes == collect(crng)

crng = CartesianRange(CartesianIndex{2}(-1,1),
                      CartesianIndex{2}(1,3))
indexes = simd_cartesian_range!(Array(eltype(crng), 0), crng)
@test indexes == collect(crng)

crng = CartesianRange(CartesianIndex{2}(-1,1),
                      CartesianIndex{2}(-1,3))
indexes = simd_cartesian_range!(Array(eltype(crng), 0), crng)
@test indexes == collect(crng)

crng = CartesianRange(CartesianIndex{1}(2),
                      CartesianIndex{1}(4))
indexes = simd_cartesian_range!(Array(eltype(crng), 0), crng)
@test indexes == collect(crng)

crng = CartesianRange(CartesianIndex{0}(),
                      CartesianIndex{0}())
indexes = simd_cartesian_range!(Array(eltype(crng), 0), crng)
@test indexes == collect(crng)

# ==================================================================
# Codegen tests: check that vectorized LLVM code is actually emitted
# https://github.com/JuliaLang/julia/issues/13686

function check_llvm_vector(func, argtypes...)
    # look for "vector.body:" in the generated LLVM code:
    if contains(Base._dump_function(func, argtypes, false, false, true, false), "vector.body:")
        return true
    else
        println("\n===========")
        code_warntype(func, argtypes)
        println("===========")
        code_llvm(func, argtypes)
        println("===========")
        error("Not vectorized: $func$argtypes")
    end
end

function simd_loop_long_expr(x, y, z)
    # SIMD loop with a longer expression
    @simd for i=1:length(x)
        @inbounds begin
            x[i] = y[i] * (z[i] > y[i]) * (z[i] < y[i]) * (z[i] >= y[i]) * (z[i] <= y[i])
        end
    end
end

function simd_loop_local_arrays()
    # SIMD loop on local arrays declared without type annotations
    x = Array(Float32,1000)
    y = Array(Float32,1000)
    z = Array(Float32,1000)
    @simd for i = 1:length(x)
        @inbounds x[i] = y[i] * z[i]
    end
end

immutable ImmutableFields
    x::Array{Float32, 1}
    y::Array{Float32, 1}
    z::Array{Float32, 1}
end

type MutableFields
    x::Array{Float32, 1}
    y::Array{Float32, 1}
    z::Array{Float32, 1}
end

function simd_loop_fields(obj)
    # SIMD loop with field access
    @simd for i = 1:length(obj.x)
        @inbounds obj.x[i] = obj.y[i] * obj.z[i]
    end
end

@generated function simd_loop_call{func}(::Type{Val{func}}, argtuple)
    # SIMD loop calling a configurable function on local arrays
    arrays = []
    decls = []
    for (T, sym) in zip(argtuple.types, "abcdefghij")
        arr = Symbol(string(sym))
        push!(arrays, :($arr[idx]))
        # add type annotations to avoid conflating those failures due to
        # function calls in the loop, with those due to type inference:
        push!(decls, :($arr::Array{$T,1} = Array($T,1000)))
    end
    code = quote
        $(Expr(:meta, :fastmath))
        $(decls...)
        @simd for idx=1:length(a)
            @inbounds a[idx] = $func($(arrays[2:end]...))
        end
    end
    code
end

# Check that the basic SIMD examples above actually generated vectorized LLVM code.
for T in [Int32,Int64,Float32,Float64]
    AR = Array{T,1}
    @test check_llvm_vector(simd_loop_example_from_manual, AR, AR, AR)
    @test check_llvm_vector(simd_loop_long_expr, AR, AR, AR)

    # TODO: uncomment the following tests
    # @test check_llvm_vector(simd_loop_with_multiple_reductions, AR, AR, AR)
    # TODO: uncomment the above tests
end

# Test for vectorization of intrinsic functions that LLVM supports:
# cf. http://llvm.org/docs/Vectorizers.html#vectorization-of-function-calls
for T in [Float32,Float64]
    # sanity check or "meta-test" for the @generated function we use:
    # this should not fail if the basic tests above passed
    @test check_llvm_vector(simd_loop_call, Type{Val{:+}}, Tuple{T, T, T})

    # Functions that LLVM should be able to vectorize:
    @test check_llvm_vector(simd_loop_call, Type{Val{:muladd}}, Tuple{T, T, T, T})
    @test check_llvm_vector(simd_loop_call, Type{Val{:abs}}, Tuple{T, T})
    # TODO: uncomment the following tests
    # @test check_llvm_vector(simd_loop_call, Type{Val{:sqrt}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:sin}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:cos}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:exp}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:log}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:log2}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:log10}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:floor}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:ceil}}, Tuple{T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:^}}, Tuple{T, T, T})
    # @test check_llvm_vector(simd_loop_call, Type{Val{:fma}}, Tuple{T, T, T, T})
    # TODO: uncomment the above tests
end

# Test for vectorization of local arrays without type annotations:
# TODO: uncomment the following tests
# @test check_llvm_vector(simd_loop_local_arrays)
# TODO: uncomment the above tests

# Test for vectorization of arrays accessed through fields:
@test check_llvm_vector(simd_loop_fields, ImmutableFields)
# TODO: uncomment the following tests
# @test check_llvm_vector(simd_loop_fields, MutableFields)
# TODO: uncomment the above tests
