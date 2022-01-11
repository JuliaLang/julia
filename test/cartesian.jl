# This file is a part of Julia. License is MIT: https://julialang.org/license

@test Base.Cartesian.exprresolve(:(1 + 3)) == 4
ex = Base.Cartesian.exprresolve(:(if 5 > 4; :x; else :y; end))
@test ex.args[2] == QuoteNode(:x)

@test Base.Cartesian.lreplace!("val_col", Base.Cartesian.LReplace{String}(:col, "col", 1)) == "val_1"
@test Base.setindex(CartesianIndex(1,5,4),3,2) == CartesianIndex(1, 3, 4)

@testset "CartesianIndices constructions" begin
    @testset "AbstractUnitRange" begin
        for oinds in [
            (2, 3),
            (UInt8(2), 3),
            (2, UInt8(3)),
            (2, 1:3),
            (Base.OneTo(2), 1:3)
        ]
            R = CartesianIndices(oinds)
            @test size(R) == (2, 3)
            @test axes(R) == (Base.OneTo(2), Base.OneTo(3))
            @test step.(R.indices) == (1, 1)
            @test step(R) == CartesianIndex(1, 1)

            @test R[begin] == CartesianIndex(1, 1)
            @test R[2] == CartesianIndex(2, 1)
            @test R[1, 2] == CartesianIndex(1, 2)
            @test R[end] == CartesianIndex(2, 3)
        end
        @test CartesianIndices((2, 3)) == CartesianIndex(1, 1):CartesianIndex(2, 3)

        R = CartesianIndices((0:5, 0:5))
        @test R[begin] == R[1] == first(R) == CartesianIndex(0, 0)
        @test R[2, 1] == R[2] == CartesianIndex(1, 0)
        @test R[1, 2] == R[7] == CartesianIndex(0, 1)
        @test R[end] == R[length(R)] == last(R) == CartesianIndex(5, 5)

        for oinds in [(2, ), (2, 3), (2, 3, 4)]
            R = CartesianIndices(oinds)
            @test eltype(R) == CartesianIndex{length(oinds)}
            @test ndims(R) == length(oinds)
            @test size(R) == oinds
        end

        # generic iterators doesn't have axes interface
        iter = Iterators.repeated([1 2], 4)
        @test_throws MethodError CartesianIndices(iter)
    end

    @testset "Step Range" begin
        for oinds in [
            (2, 1:2:6),
            (Base.OneTo(2), 1:2:6),
            (UInt8(2), 1:2:6),
            (2, UInt8(1):UInt8(2):UInt8(6))
        ]
            R = CartesianIndices(oinds)
            @test size(R) == (2, 3)
            @test axes(R) == (Base.OneTo(2), Base.OneTo(3))
            @test step.(R.indices) == (1, 2)
            @test step(R) == CartesianIndex(1, 2)

            @test R[begin] == CartesianIndex(1, 1)
            @test R[2] == CartesianIndex(2, 1)
            @test R[1, 2] == CartesianIndex(1, 3)
            @test R[end] == CartesianIndex(2, 5)
        end

        @test CartesianIndices((1:2:5, 1:3:7)) == CartesianIndex(1, 1):CartesianIndex(2,3):CartesianIndex(5,7)

        R = CartesianIndex(0, 0):CartesianIndex(2, 3):CartesianIndex(5, 7)
        @test R[begin] == R[1] == first(R) == CartesianIndex(0, 0)
        @test R[2, 1] == R[2] == CartesianIndex(2, 0)
        @test R[1, 2] == R[4] == CartesianIndex(0, 3)
        @test R[end] == R[length(R)] == last(R) == CartesianIndex(4, 6)

        for oinds in [(1:2:5, ), (1:2:5, 1:3:7), (1:2:5, 1:3:7, 1:4:11)]
            R = CartesianIndices(oinds)
            @test eltype(R) == CartesianIndex{length(oinds)}
            @test ndims(R) == length(oinds)
            @test size(R) == length.(oinds)
        end

        R = CartesianIndices((1:2:5, 7:-3:1))
        @test R == CartesianIndex(1, 7):CartesianIndex(2,-3):CartesianIndex(5, 1)
        @test step.(R.indices) == (2, -3)
        @test R[begin] == R[1] == first(R) == CartesianIndex(1, 7)
        @test R[2, 1] == R[2] == CartesianIndex(3, 7)
        @test R[1, 2] == R[4] == CartesianIndex(1, 4)
        @test R[end] == R[length(R)] == last(R) == CartesianIndex(5, 1)
    end

    @testset "IdentityUnitRange" begin
        function _collect(A)
            rst = eltype(A)[]
            for i in A
                push!(rst, i)
            end
            rst
        end
        function _simd_collect(A)
            rst = eltype(A)[]
            @simd for i in A
                push!(rst, i)
            end
            rst
        end

        for oinds in [
            (Base.IdentityUnitRange(0:1),),
            (Base.IdentityUnitRange(0:1), Base.IdentityUnitRange(0:2)),
            (Base.IdentityUnitRange(0:1), Base.OneTo(3)),
        ]
            R = CartesianIndices(oinds)
            @test axes(R) === oinds
            @test _collect(R) == _simd_collect(R) == vec(collect(R))
        end
        R = CartesianIndices((Base.IdentityUnitRange(0:1), 0:1))
        @test axes(R) == (Base.IdentityUnitRange(0:1), Base.OneTo(2))

    end

    for oinds in [(2, 3), (0:1, 0:2), (0:1:1, 0:1:2), (Base.IdentityUnitRange(0:1), Base.IdentityUnitRange(0:2)) ]
        R = CartesianIndices(oinds)
        @test vec(LinearIndices(R)) == 1:6
    end
    # TODO: non-1 steps are not supported yet, but may change in the future
    @test_throws ArgumentError LinearIndices(CartesianIndices((1:2:5, )))
    @test_throws ArgumentError LinearIndices(CartesianIndices((1:1:5, 1:2:5)))
end

module TestOffsetArray
    isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
    using .Main.OffsetArrays
    using Test

    A = OffsetArray(rand(2, 3), -1, -1)
    R = CartesianIndices(A)
    @test R == CartesianIndices((0:1, 0:2))
    @test axes(R) == (0:1, 0:2)
    for i in eachindex(A)
        @test A[i] == A[R[i]]
    end
    for i in R
        @test A[i] == A[Tuple(i)...]
    end
end

@testset "CartesianIndices getindex" begin
    @testset "0D array" begin
        a = zeros()
        c = CartesianIndices(a)
        @test a[c] == a
        @test c[c] === c
        @test c[] == CartesianIndex()
    end

    @testset "AbstractUnitRange" begin
        for oinds in [(2, ), (2, 3), (2, 3, 4)]
            A = rand(1:10, oinds)
            R = CartesianIndices(A)
            @test R == CartesianIndices(oinds)

            @test A[R] == A
            @test axes(A) == axes(R)
            @test all(i->A[i]==A[R[i]], eachindex(A))
            @test all(i->A[i]==A[R[i]], R)
            @test all(i->A[i]==A[R[i]], collect(R))
            @test all(i->i in R, collect(R))

            # Indexing a CartesianIndices with another CartesianIndices having the same ndims
            # forwards the indexing to the component ranges and retains the wrapper
            @test R[R] === R

            R_array = collect(R)

            all_onetoone = ntuple(x -> 1:1, Val(ndims(R)))
            R2 = R[all_onetoone...]
            @test R2 isa CartesianIndices{ndims(R)}

            all_one = ntuple(x -> 1, Val(ndims(R)))
            @test R2[all_one...] == R_array[all_one...]

            @test R2 == R_array[all_onetoone...]

            R3 = R[ntuple(x -> Colon(), Val(ndims(R)))...]
            @test R3 === R

            # test a mix of Colons and ranges
            # up to two leading axes are colons, while the rest are UnitRanges
            indstrailing = (1:1 for _ in min(ndims(R), 2)+1:ndims(R))
            R4 = R[(Colon() for _ in 1:min(ndims(R), 2))..., indstrailing...]
            @test R4 isa CartesianIndices{ndims(R)}
            indsleading = CartesianIndices(axes(A)[1:min(ndims(A), 2)])
            for I in indsleading
                @test R4[I, indstrailing...] == R_array[I, indstrailing...]
            end
        end
    end

    @testset "StepRange" begin
        for oinds in [(1:2:5, ), (1:2:5, 1:3:7), (1:2:5, 1:3:7, 1:4:11)]
            A = rand(1:10, last.(oinds))
            R = CartesianIndices(A)

            SR = CartesianIndex(first.(oinds)):CartesianIndex(step.(oinds)):CartesianIndex(last.(oinds))
            @test A[oinds...] == A[SR]
            @test A[SR] == A[R[SR]]

            # TODO: A[SR] == A[Linearindices(SR)] should hold for StepRange CartesianIndices
            @test_broken A[SR] == A[LinearIndices(SR)]

            # Create a CartesianIndices with StepRange indices to test indexing into it
            R = CartesianIndices(oinds)
            R_array = collect(R)

            all_onetoone = ntuple(x -> 1:1, Val(ndims(R)))
            R2 = R[all_onetoone...]
            @test R2 isa CartesianIndices{ndims(R)}

            all_one = ntuple(x -> 1, Val(ndims(R)))
            @test R2[all_one...] == R_array[all_one...]
            @test R2 == R_array[all_onetoone...]

            R3 = R[ntuple(x -> Colon(), Val(ndims(R)))...]
            @test R3 === R

            # test a mix of Colons and ranges
            # up to two leading axes are colons, while the rest are UnitRanges
            indstrailing = (1:1 for _ in min(ndims(R), 2)+1:ndims(R))
            R4 = R[(Colon() for _ in 1:min(ndims(R), 2))..., indstrailing...]
            @test R4 isa CartesianIndices{ndims(R)}
            indsleading = CartesianIndices(axes(R)[1:min(ndims(R), 2)])
            for I in indsleading
                @test R4[I, indstrailing...] == R_array[I, indstrailing...]
            end
        end

        # CartesianIndices whole indices have a unit step may be their own axes
        for oinds in [(1:1:4, ), (1:1:4, 1:1:5), (1:1:4, 1:1:5, 1:1:3)]
            R = CartesianIndices(oinds)
            @test R[R] === R
            # test a mix of UnitRanges and StepRanges
            R = CartesianIndices((oinds..., 1:3))
            @test R[R] === R
            R = CartesianIndices((1:3, oinds...))
            @test R[R] === R
        end
    end

    @testset "logical indexing of CartesianIndices with ranges" begin
        c = CartesianIndices((1:0, 1:2))
        c2 = c[true:false, 1:2]
        @test c2 == c

        for (inds, r) in Any[(1:2, false:true), (1:2, false:true:true),
            (1:2:3, false:true), (1:2:3, false:true:true)]

            c = CartesianIndices((inds, 1:2))
            c2 = c[r, 1:2]
            @test c2 isa CartesianIndices{ndims(c)}
            @test c2[1, :] == c[2, :]
        end

        for (inds, r) in Any[(1:1, true:true), (1:1, true:true:true),
            (1:1:1, true:true), (1:1:1, true:true:true)]

            c = CartesianIndices((inds, 1:2))
            c2 = c[r, 1:2]
            @test c2 isa CartesianIndices{ndims(c)}
            @test c2[1, :] == c[1, :]
        end

        for (inds, r) in Any[(1:1, false:false), (1:1, false:true:false),
            (1:1:1, false:false), (1:1:1, false:true:false)]

            c = CartesianIndices((inds, 1:2))
            c2 = c[r, 1:2]
            @test c2 isa CartesianIndices{ndims(c)}
            @test size(c2, 1) == 0
        end
    end
end

@testset "range interface" begin
    for (I, i, i_next) in [
        (CartesianIndices((1:2:5, )), CartesianIndex(2, ), CartesianIndex(4, )),
        (1:2:5, 2, 4),
    ]
        # consistent with ranges behavior
        @test !(i in I)
        @test iterate(I, i) == (i_next, i_next)
    end

    # check iteration behavior on boundary
    R = CartesianIndex(1, 1):CartesianIndex(2, 3):CartesianIndex(4, 5)
    @test R.indices == (1:2:3, 1:3:4)
    i = CartesianIndex(4, 1)
    i_next = CartesianIndex(1, 4)
    @test !(i in R) && iterate(R, i) == (i_next, i_next)

    for R in [
        CartesianIndices((1:-1:-1, 1:2:5)),
        CartesianIndices((2, 3)),
        CartesianIndex(1, 2) .- CartesianIndices((1:-1:-1, 1:2:5)),
        CartesianIndex(1, 2) .- CartesianIndices((2, 3)),
    ]
        Rc = collect(R)
        @test all(map(==, R, Rc))
    end
end

@testset "Cartesian simd/broadcasting" begin
    @testset "AbstractUnitRange" begin
        A = rand(-5:5, 64, 64)
        @test abs.(A) == map(abs, A)

        function test_simd(f, @nospecialize(A); init=zero(eltype(A)))
            val_simd = init
            @simd for i in CartesianIndices(A)
                val_simd = f(val_simd, A[i])
            end

            val_iter = init
            for i in CartesianIndices(A)
                val_iter = f(val_iter, A[i])
            end

            @test val_iter == reduce(f, A, init=init)
            @test val_iter ≈ val_simd
        end

        test_simd(+, A)
    end

    R = CartesianIndex(-1, -1):CartesianIndex(6, 7)
    @test R .+ CartesianIndex(1, 2) == CartesianIndex(0, 1):CartesianIndex(7, 9)
    @test R .- CartesianIndex(1, 2) == CartesianIndex(-2, -3):CartesianIndex(5, 5)
    # 37867: collect is needed
    @test collect(CartesianIndex(1, 2) .- R) == CartesianIndex(2, 3):CartesianIndex(-1, -1):CartesianIndex(-5, -5)

    R = CartesianIndex(-1, -1):CartesianIndex(2, 3):CartesianIndex(6, 7)
    @test R .+ CartesianIndex(2, 2) == CartesianIndex(1, 1):CartesianIndex(2, 3):CartesianIndex(8, 9)
    @test R .- CartesianIndex(2, 2) == CartesianIndex(-3, -3):CartesianIndex(2, 3):CartesianIndex(4, 5)
    # 37867: collect is needed
    @test collect(CartesianIndex(1, 1) .- R) == CartesianIndex(2, 2):CartesianIndex(-2, -3):CartesianIndex(-4, -4)
end

@testset "Iterators" begin
    @testset "Reverse" begin
        R = CartesianIndices((0:5, 0:5))
        RR = Iterators.Reverse(R)
        rR = reverse(R)
        @test rR == collect(RR)
        @test rR.indices == (5:-1:0, 5:-1:0)

        @test eltype(RR) == CartesianIndex{2}
        @test size(RR) == size(R)
        @test axes(RR) == axes(R)

        @test first(RR) == last(R) == CartesianIndex(5, 5)
        @test last(RR) == first(R) == CartesianIndex(0, 0)
        RRR = collect(Iterators.Reverse(collect(RR)))
        @test R == RRR
    end

    @testset "collect" begin
        for oinds in [(0:5, ), (2:2:7, ), (2:-1:0, ),
                      (0:5, 2:8), (2:2:7, 3:3:10), (2:-1:0, 2:7),]
            R = CartesianIndices(oinds)
            @test collect(R) == R
        end
    end
end

@testset "set operations" begin
    R1 = CartesianIndices((3, 4, 5))
    R2 = CartesianIndices((-2:2, -3:3, -4:4))
    R = CartesianIndices((2, 3, 4))
    @test intersect(R1, R2) == R
end

# test conversions for CartesianIndex

@testset "CartesianIndex Conversions" begin
    @test convert(Int, CartesianIndex(42)) === 42
    @test convert(Float64, CartesianIndex(42)) === 42.0
    @test convert(Tuple, CartesianIndex(42, 1)) === (42, 1)

    # can't convert higher-dimensional indices to Int
    @test_throws MethodError convert(Int, CartesianIndex(42, 1))
end

@testset "CartesianIndices overflow" begin
    @testset "incremental steps" begin
        I = CartesianIndices((1:typemax(Int),))
        i = last(I)
        @test iterate(I, i) === nothing

        I = CartesianIndices((1:2:typemax(Int), ))
        i = CartesianIndex(typemax(Int)-1)
        @test iterate(I, i) === nothing

        I = CartesianIndices((1:(typemax(Int)-1),))
        i = CartesianIndex(typemax(Int))
        @test iterate(I, i) === nothing

        I = CartesianIndices((1:2:typemax(Int)-1, ))
        i = CartesianIndex(typemax(Int)-1)
        @test iterate(I, i) === nothing

        I = CartesianIndices((1:typemax(Int), 1:typemax(Int)))
        i = last(I)
        @test iterate(I, i) === nothing

        I = CartesianIndices((1:2:typemax(Int), 1:2:typemax(Int)))
        i = CartesianIndex(typemax(Int)-1, typemax(Int)-1)
        @test iterate(I, i) === nothing

        I = CartesianIndices((1:typemax(Int), 1:typemax(Int)))
        i = CartesianIndex(typemax(Int), 1)
        @test iterate(I, i) === (CartesianIndex(1, 2), CartesianIndex(1,2))

        I = CartesianIndices((1:2:typemax(Int), 1:2:typemax(Int)))
        i = CartesianIndex(typemax(Int)-1, 1)
        @test iterate(I, i) === (CartesianIndex(1, 3), CartesianIndex(1, 3))

        I = CartesianIndices((typemin(Int):(typemin(Int)+3),))
        i = last(I)
        @test iterate(I, i) === nothing

        I = CartesianIndices(((typemin(Int):2:typemin(Int)+3), ))
        i = CartesianIndex(typemin(Int)+2)
        @test iterate(I, i) === nothing
    end

    @testset "decremental steps" begin
        I = Iterators.Reverse(CartesianIndices((typemin(Int):typemin(Int)+10, )))
        i = last(I)
        @test iterate(I, i) === nothing

        I = Iterators.Reverse(CartesianIndices((typemin(Int):2:typemin(Int)+10, )))
        i = last(I)
        @test iterate(I, i) === nothing

        I = Iterators.Reverse(CartesianIndices((typemin(Int):typemin(Int)+10, )))
        i = CartesianIndex(typemin(Int))
        @test iterate(I, i) === nothing

        I = Iterators.Reverse(CartesianIndices((typemin(Int):2:typemin(Int)+10, )))
        i = CartesianIndex(typemin(Int))
        @test iterate(I, i) === nothing

        I = Iterators.Reverse(CartesianIndices((typemin(Int):typemin(Int)+10, typemin(Int):typemin(Int)+10)))
        i = last(I)
        @test iterate(I, i) === nothing

        I = Iterators.Reverse(CartesianIndices((typemin(Int):2:typemin(Int)+10, typemin(Int):2:typemin(Int)+10)))
        i = CartesianIndex(typemin(Int), typemin(Int))
        @test iterate(I, i) === nothing

        I = Iterators.Reverse(CartesianIndices((typemin(Int):typemin(Int)+10, typemin(Int):typemin(Int)+10)))
        i = CartesianIndex(typemin(Int), typemin(Int)+1)
        @test iterate(I, i) === (CartesianIndex(typemin(Int)+10, typemin(Int)), CartesianIndex(typemin(Int)+10, typemin(Int)))

        I = Iterators.Reverse(CartesianIndices((typemin(Int):2:typemin(Int)+10, typemin(Int):2:typemin(Int)+10)))
        i = CartesianIndex(typemin(Int), typemin(Int)+2)
        @test iterate(I, i) === (CartesianIndex(typemin(Int)+10, typemin(Int)), CartesianIndex(typemin(Int)+10, typemin(Int)))

        I = CartesianIndices((typemax(Int):-1:typemax(Int)-10, ))
        i = last(I)
        @test iterate(I, i) === nothing

        I = CartesianIndices((typemax(Int):-2:typemax(Int)-10, ))
        i = last(I)
        @test iterate(I, i) === nothing
    end
end

@testset "CartesianIndices iteration" begin
    I = CartesianIndices((2:4, 0:1, 1:1, 3:5))
    indices = Vector{eltype(I)}()
    for i in I
        push!(indices, i)
    end
    @test length(I) == length(indices)
    @test vec(I) == indices

    empty!(indices)
    I = Iterators.reverse(I)
    for i in I
        push!(indices, i)
    end
    @test length(I) == length(indices)
    @test vec(collect(I)) == indices

    # test invalid state
    I = CartesianIndices((2:4, 3:5))
    @test iterate(I, CartesianIndex(typemax(Int), 3))[1] == CartesianIndex(2,4)
    @test iterate(I, CartesianIndex(typemax(Int), 4))[1] == CartesianIndex(2,5)
    @test iterate(I, CartesianIndex(typemax(Int), 5))    === nothing

    @test iterate(I, CartesianIndex(3, typemax(Int)))[1] == CartesianIndex(4,typemax(Int))
    @test iterate(I, CartesianIndex(4, typemax(Int)))    === nothing
end

@testset "CartesianIndices operations" begin
    I = CartesianIndices((1:3, 4:6))
    J = CartesianIndices((2:4, 3:5))

    @test @inferred(intersect(I, J)) == CartesianIndices((2:3, 4:5))
end

# issue #39705
f39705() = Base.Cartesian.@nany 0 _ -> true
@test f39705() === false

@testset "CartesianIndices with Bool" begin
    @test @inferred(CartesianIndices((true,))) == CartesianIndices((1,))
    @test @inferred(CartesianIndices((false,))) == CartesianIndices((0,))
    @test @inferred(CartesianIndices((true, false))) == CartesianIndices((1, 0))
    @test @inferred(CartesianIndices((false, true))) == CartesianIndices((0, 1))
end
