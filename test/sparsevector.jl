using Base.Test

s = SparseVector(91,Int64[1:10:100],Int64[1:10:100])

@test size(s) == (91,)
@test length(s) == 91
@test nnz(s) == 10
@test countnz(s) == 10
@test eltype(s) == Int64
@test eltype(reinterpret(Float64,s)) == Float64
@test eltype(float(s)) == Float64
@test eltype(complex(s)) == Complex{Int64}

s1 = copy(s)
@test !(s === s1)
#@test all(s .== s1)

s2 = similar(s)
@test eltype(s2) == Int64
@test length(s2) == 91

s3 = similar(s,Float64)
@test eltype(s3) == Float64
@test length(s3) == 91

s4 = similar(s,Int32,Int32)
@test eltype(s4) == Int32
@test length(s4) == 91
@test eltype(s4.rowval) == Int32

s41 = convert(typeof(s),s4)
@test eltype(s41) == Int64
@test length(s41) == 91

s42 = convert(SpV.SparseVector{Float64},s4)
@test eltype(s42) == Float64
@test length(s42) == 91

s5 = similar(s,Float64,(5,5))
@test size(s5) == (5,5)
@test eltype(s5) == Float64
@test nnz(s5) == 0

v = Int64[1,0,2,0,3,0,4,0,5]
s6 = convert(typeof(s),v)
@test eltype(s6) == Int64
@test length(s6) == 9
@test nnz(s6) == 5

v1 = full(s)
@test length(v1) == length(s)
@test eltype(v1) == eltype(s)

v2 = full(s6)
@test length(v2) == length(s6)
@test eltype(v2) == eltype(s6)

