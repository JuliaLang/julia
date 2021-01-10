@testset "fill array constructors and convert" begin

    @test Fill(1) ≡ Fill{Int}(1) ≡ Fill{Int,0}(1) ≡ Fill{Int,0,Tuple{}}(1,())
    @test Fill(1,(-1,5)) ≡ Fill(1,(0,5))
    @test Fill(1.0,5) isa AbstractVector{Float64}
    @test Fill(1.0,5,5) isa AbstractMatrix{Float64}
    @test Fill(1,5) ≡ Fill(1,(5,))
    @test Fill(1,5,5) ≡ Fill(1,(5,5))
    @test eltype(Fill(1.0,5,5)) == Float64

    @test_throws InexactError Matrix{Float64}(Fill(1.0+1.0im,10,10))

    for T in (Int, Float64)
        F = Fill{T}(one(T), 5)

        @test eltype(F) == T
        @test Array(F) == fill(one(T),5)
        @test Array{T}(F) == fill(one(T),5)
        @test Array{T,1}(F) == fill(one(T),5)

        F = Fill{T}(one(T), 5, 5)
        @test eltype(F) == T
        @test Array(F) == fill(one(T),5,5)
        @test Array{T}(F) == fill(one(T),5,5)
        @test Array{T,2}(F) == fill(one(T),5,5)

        @test convert(AbstractArray,F) ≡ F
        @test convert(AbstractArray{T},F) ≡ AbstractArray{T}(F) ≡ F
        @test convert(AbstractMatrix{T},F) ≡ AbstractMatrix{T}(F) ≡ F

        @test convert(AbstractArray{Float32},F) ≡ AbstractArray{Float32}(F) ≡
                Fill{Float32}(one(Float32),5,5)
        @test convert(AbstractMatrix{Float32},F) ≡ AbstractMatrix{Float32}(F) ≡
                Fill{Float32}(one(Float32),5,5)

        @test Fill{T}(F) ≡ Fill{T,2}(F) ≡ typeof(F)(F) ≡ F
    end

    @testset "copy should return Fill" begin
        x = Fill(1.0,10)
        @test copy(x) ≡ x
    end
end

# @testset "indexing" begin
#     A = Fill(3.0,5)
#     @test A[1:3] ≡ Fill(3.0,3)
#     @test A[1:3,1:1] ≡ Fill(3.0,3,1)
#     @test_throws BoundsError A[1:3,2]
#     @test_throws BoundsError A[1:26]
#     @test A[[true, false, true, false, false]] ≡ Fill(3.0, 2)
#     A = Fill(3.0, 2, 2)
#     @test A[[true true; true false]] ≡ Fill(3.0, 3)
#     @test_throws DimensionMismatch A[[true, false]]

#     @testset "colon" begin
#         @test Fill(3.0,2)[:] ≡ Fill(3.0,2)[Base.Slice(Base.OneTo(2))] ≡ Fill(3.0,2)

#         @test Fill(3.0,2,2)[:,:] ≡ Fill(3.0,2,2)[Base.Slice(Base.OneTo(2)),Base.Slice(Base.OneTo(2))] ≡ Fill(3.0,2,2)
#     end

#     @testset "mixed integer / vector /colon" begin
#         a = Fill(2.0,5)
#         z = Zeros(5)
#         @test a[1:5] ≡ a[:] ≡ a
#         @test z[1:5] ≡ z[:] ≡ z

#         A = Fill(2.0,5,6)
#         Z = Zeros(5,6)
#         @test A[:,1] ≡ A[1:5,1] ≡ Fill(2.0,5)
#         @test A[1,:] ≡ A[1,1:6] ≡ Fill(2.0,6)
#         @test A[:,:] ≡ A[1:5,1:6] ≡ A[1:5,:] ≡ A[:,1:6] ≡ A
#         @test Z[:,1] ≡ Z[1:5,1] ≡ Zeros(5)
#         @test Z[1,:] ≡ Z[1,1:6] ≡ Zeros(6)
#         @test Z[:,:] ≡ Z[1:5,1:6] ≡ Z[1:5,:] ≡ Z[:,1:6] ≡ Z
        
#         A = Fill(2.0,5,6,7)
#         Z = Zeros(5,6,7)
#         @test A[:,1,1] ≡ A[1:5,1,1] ≡ Fill(2.0,5)
#         @test A[1,:,1] ≡ A[1,1:6,1] ≡ Fill(2.0,6)
#         @test A[:,:,:] ≡ A[1:5,1:6,1:7] ≡ A[1:5,:,1:7] ≡ A[:,1:6,1:7] ≡ A
#     end
# end


@testset "Broadcast" begin
    x = Fill(5,5)
    @test (.+)(x) ≡ x
    @test (.-)(x) ≡ -x
    @test exp.(x) ≡ Fill(exp(5),5)
    @test x .+ 1 ≡ Fill(6,5)
    @test 1 .+ x ≡ Fill(6,5)
    @test x .+ x ≡ Fill(10,5)
    f = (x,y) -> cos(x*y)

    @testset "support Ref" begin
        @test Fill(1,10) .- 1 ≡ Fill(1,10) .- Ref(1) ≡ Fill(1,10) .- Ref(1I)
        @test Fill([1 2; 3 4],10) .- Ref(1I) == Fill([0 2; 3 3],10)
        @test Ref(1I) .+ Fill([1 2; 3 4],10) == Fill([2 2; 3 5],10)
    end
end

@testset "map" begin
    x = Fill(2,5,3)
    @test map(exp,x) === Fill(exp(2),5,3)
end

@testset "0-dimensional" begin
    A = Fill{Int,0,Tuple{}}(3, ())

    @test A[] ≡ A[1] ≡ 3
    @test A ≡ Fill{Int,0}(3, ()) ≡ Fill(3, ()) ≡ Fill(3)
    @test size(A) == ()
    @test axes(A) == ()
end

@testset "setindex!/fill!" begin
    F = Fill(1,10)
    @test (F[1] = 1) == 1
    @test_throws BoundsError (F[11] = 1)
    @test_throws ArgumentError (F[10] = 2)

    F = Fill(1,10,5)
    @test (F[1] = 1) == 1
    @test (F[3,3] = 1) == 1
    @test_throws BoundsError (F[51] = 1)
    @test_throws BoundsError (F[1,6] = 1)
    @test_throws ArgumentError (F[10] = 2)
    @test_throws ArgumentError (F[10,1] = 2)

    @test (F[:,1] .= 1) == fill(1,10)
    @test_throws ArgumentError (F[:,1] .= 2)

    @test fill!(F,1) == F
    @test_throws ArgumentError fill!(F,2)
end
