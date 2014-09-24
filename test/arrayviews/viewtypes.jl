debug = false
debug && println("test contiguousview.jl")

using Base.Test

using Base.ArrayViews: ContRank, contiguousrank, contiguous_view, iscontiguous, strided_view

debug && println("Testing for Contiguous Views")

avparent = rand(5, 4, 3, 2)

debug && println("1D")
v = contiguous_view(avparent, 10, (20,))
@test isa(v, ContiguousView{Float64,1})
@test eltype(v) == eltype(avparent)
@test ndims(v) == 1
@test length(v) == 20

@test size(v) == (20,)
@test [size(v,i) for i=1:2] == [20, 1]
@test strides(v) == (1,)
@test [stride(v,i) for i=1:2] == [1,20]

@test [v[i] for i=1:20] == avparent[11:30]
@test [v[i,1] for i=1:20] == avparent[11:30]

debug && println("2D")
v = contiguous_view(avparent, 10, (5, 12))
@test isa(v, ContiguousView{Float64,2})
@test eltype(v) == eltype(avparent)
@test ndims(v) == 2
@test length(v) == 60

@test size(v) == (5, 12)
@test [size(v,i) for i=1:3] == [5,12,1]
@test strides(v) == (1,5)
@test [stride(v,i) for i=1:3] == [1,5,60]

@test [v[i] for i=1:60] == avparent[11:70]
@test [v[i,j] for i=1:5, j=1:12] == avparent[:,3:14]
@test [v[i,j,1] for i=1:5, j=1:12] == avparent[:,3:14]

debug && println("3D")
v = contiguous_view(avparent, (5, 4, 6))
@test isa(v, ContiguousView{Float64,3})
@test eltype(v) == eltype(avparent)
@test ndims(v) == 3
@test length(v) == 120

@test size(v) == (5, 4, 6)
@test [size(v,i) for i=1:4] == [5,4,6,1]
@test strides(v) == (1,5,20)
@test [stride(v,i) for i=1:4] == [1,5,20,120]

@test [v[i] for i=1:120] == avparent[1:120]
@test [v[i,j] for i=1:5, j=1:24] == avparent[1:5,1:24]
@test [v[i,j,k] for i=1:5, j=1:4, k=1:6] == avparent[1:5, 1:4, 1:6]
@test [v[i,j,k,1] for i=1:5, j=1:4, k=1:6] == avparent[1:5, 1:4, 1:6]

debug && println("4D")
v = contiguous_view(avparent, 0, (5, 4, 3, 2))
@test isa(v, ContiguousView{Float64,4})
@test eltype(v) == eltype(avparent)
@test ndims(v) == 4
@test length(v) == 120

@test size(v) == (5, 4, 3, 2)
@test [size(v,i) for i=1:5] == [5,4,3,2,1]
@test strides(v) == (1,5,20,60)
@test [stride(v,i) for i=1:5] == [1,5,20,60,120]

@test [v[i] for i=1:120] == avparent[1:120]
@test [v[i,j] for i=1:5, j=1:24] == avparent[1:5, 1:24]
@test [v[i,j,k] for i=1:5, j=1:4, k=1:6] == avparent[1:5, 1:4, 1:6]
@test [v[i,j,k,l] for i=1:5, j=1:4, k=1:3, l=1:2] == avparent[1:5, 1:4, 1:3, 1:2]
@test [v[i,j,k,l,1] for i=1:5, j=1:4, k=1:3, l=1:2] == avparent[1:5, 1:4, 1:3, 1:2]


debug && println("Testing for Strided Views")

avparent = reshape(1.:1680., (8, 7, 6, 5))

debug && println("N=1, M=1 ")
v = strided_view(avparent, 10, (12,), ContRank{1}, (1,))
isa(v, StridedView{Float64, 1, 1})
@test ndims(v) == 1
@test length(v) == 12
@test iscontiguous(v) == true
@test contiguousrank(v) == 1

@test size(v) == (12,)
@test Int[size(v, i) for i=1:2] == [12, 1]
@test strides(v) == (1,)
@test stride(v,1) == 1

@test [v[i] for i=1:12] == avparent[11:22]
@test [v[i,1] for i=1:12] == avparent[11:22]
@test [v[i,1,1] for i=1:12] == avparent[11:22]

debug && println("N=1, M=0")
v = strided_view(avparent, 10, (12,), ContRank{0}, (2,))
isa(v, StridedView{Float64, 1, 0})
@test ndims(v) == 1
@test length(v) == 12
@test iscontiguous(v) == false
@test contiguousrank(v) == 0

@test size(v) == (12,)
@test Int[size(v, i) for i=1:2] == [12, 1]
@test strides(v) == (2,)
@test stride(v,1) == 2

@test [v[i] for i=1:12] == avparent[11:2:33]
@test [v[i,1] for i=1:12] == avparent[11:2:33]
@test [v[i,1,1] for i=1:12] == avparent[11:2:33]

debug && println("N=2, M=2")
v = strided_view(avparent, 8, (8, 7), ContRank{2}, (1, 8))
isa(v, StridedView{Float64, 2, 2})
@test ndims(v) == 2
@test length(v) == 56
@test iscontiguous(v) == true
@test contiguousrank(v) == 2

@test size(v) == (8, 7)
@test Int[size(v, i) for i=1:3] == [8, 7, 1]
@test strides(v) == (1, 8)
@test Int[stride(v, i) for i=1:2] == [1, 8]

@test [v[i,j] for i=1:8, j=1:7] == avparent[1:8, 2:8]
@test [v[i,j,1] for i=1:8, j=1:7] == avparent[1:8, 2:8]
@test [v[i] for i=1:56] == vec(avparent[1:8, 2:8])

debug && println("N=2, M=1")
v = strided_view(avparent, 8, (6, 7), ContRank{1}, (1, 8))
isa(v, StridedView{Float64, 2, 1})
@test ndims(v) == 2
@test length(v) == 42
@test iscontiguous(v) == false
@test contiguousrank(v) == 1

@test size(v) == (6, 7)
@test Int[size(v, i) for i=1:3] == [6, 7, 1]
@test strides(v) == (1, 8)
@test Int[stride(v, i) for i=1:2] == [1, 8]

@test [v[i,j] for i=1:6, j=1:7] == avparent[1:6, 2:8]
@test [v[i,j,1] for i=1:6, j=1:7] == avparent[1:6, 2:8]
@test [v[i] for i=1:42] == vec(avparent[1:6, 2:8])

debug && println("N=2, M=0")
v = strided_view(avparent, 8, (4, 7), ContRank{0}, (2, 8))
isa(v, StridedView{Float64, 2, 0})
@test ndims(v) == 2
@test length(v) == 28
@test iscontiguous(v) == false
@test contiguousrank(v) == 0

@test size(v) == (4, 7)
@test Int[size(v, i) for i=1:3] == [4, 7, 1]
@test strides(v) == (2, 8)
@test Int[stride(v, i) for i=1:2] == [2, 8]

@test [v[i,j] for i=1:4, j=1:7] == avparent[1:2:7, 2:8]
@test [v[i,j,1] for i=1:4, j=1:7] == avparent[1:2:7, 2:8]
@test [v[i] for i=1:28] == vec(avparent[1:2:7, 2:8])

debug && println("N=3, M=3")
v = strided_view(avparent, (8, 7, 6), ContRank{3}, (1, 8, 56))
isa(v, StridedView{Float64, 3, 3})
@test ndims(v) == 3
@test length(v) == 336
@test iscontiguous(v) == true
@test contiguousrank(v) == 3

@test size(v) == (8, 7, 6)
@test Int[size(v, i) for i=1:4] == [8, 7, 6, 1]
@test strides(v) == (1, 8, 56)
@test Int[stride(v, i) for i=1:3] == [1, 8, 56]

vr = avparent[1:8, 1:7, 1:6]
@test [v[i,j,k] for i=1:8, j=1:7, k=1:6] == vr
@test [v[i,j,k,1] for i=1:8, j=1:7, k=1:6] == vr
@test [v[i,j] for i=1:8, j=1:42] == vr[:,:]
@test [v[i] for i=1:336] == vr[:]

debug && println("N=3, M=2")
v = strided_view(avparent, (8, 6, 5), ContRank{2}, (1, 8, 56))
isa(v, StridedView{Float64, 3, 2})
@test ndims(v) == 3
@test length(v) == 240
@test iscontiguous(v) == false
@test contiguousrank(v) == 2

@test size(v) == (8, 6, 5)
@test Int[size(v, i) for i=1:4] == [8, 6, 5, 1]
@test strides(v) == (1, 8, 56)
@test Int[stride(v, i) for i=1:3] == [1, 8, 56]

vr = avparent[1:8, 1:6, 1:5]
@test [v[i,j,k] for i=1:8, j=1:6, k=1:5] == vr
@test [v[i,j,k,1] for i=1:8, j=1:6, k=1:5] == vr
@test [v[i,j] for i=1:8, j=1:30] == vr[:,:]
@test [v[i] for i=1:240] == vr[:]

debug && println("N=3, M=1")
v = strided_view(avparent, (7, 6, 5), ContRank{1}, (1, 8, 56))
isa(v, StridedView{Float64, 3, 2})
@test ndims(v) == 3
@test length(v) == 210
@test iscontiguous(v) == false
@test contiguousrank(v) == 1

@test size(v) == (7, 6, 5)
@test Int[size(v, i) for i=1:4] == [7, 6, 5, 1]
@test strides(v) == (1, 8, 56)
@test Int[stride(v, i) for i=1:3] == [1, 8, 56]

vr = avparent[1:7, 1:6, 1:5]
@test [v[i,j,k] for i=1:7, j=1:6, k=1:5] == vr
@test [v[i,j,k,1] for i=1:7, j=1:6, k=1:5] == vr
@test [v[i,j] for i=1:7, j=1:30] == vr[:,:]
@test [v[i] for i=1:210] == vr[:]

debug && println("N=3, M=0")
v = strided_view(avparent, (4, 6, 5), ContRank{0}, (2, 8, 56))
isa(v, StridedView{Float64, 3, 2})
@test ndims(v) == 3
@test length(v) == 120
@test iscontiguous(v) == false
@test contiguousrank(v) == 0

@test size(v) == (4, 6, 5)
@test Int[size(v, i) for i=1:4] == [4, 6, 5, 1]
@test strides(v) == (2, 8, 56)
@test Int[stride(v, i) for i=1:3] == [2, 8, 56]

vr = avparent[1:2:7, 1:6, 1:5]
@test [v[i,j,k] for i=1:4, j=1:6, k=1:5] == vr
@test [v[i,j,k,1] for i=1:4, j=1:6, k=1:5] == vr
@test [v[i,j] for i=1:4, j=1:30] == vr[:,:]
@test [v[i] for i=1:120] == vr[:]

debug && println("N=4, M=2")
v = strided_view(avparent, (8, 6, 4, 3), ContRank{2}, (1, 8, 56, 336))
isa(v, StridedView{Float64, 4, 2})
@test ndims(v) == 4
@test length(v) == 576
@test iscontiguous(v) == false
@test contiguousrank(v) == 2

@test size(v) == (8, 6, 4, 3)
@test Int[size(v,i) for i=1:5] == [8, 6, 4, 3, 1]
@test strides(v) == (1, 8, 56, 336)
@test Int[stride(v,i) for i=1:4] == [1, 8, 56, 336]

vr = avparent[1:8, 1:6, 1:4, 1:3]
@test [v[i,j,k,l] for i=1:8, j=1:6, k=1:4, l=1:3] == vr
@test [v[i,j,k,l,1] for i=1:8, j=1:6, k=1:4, l=1:3] == vr
@test [v[i,j,k] for i=1:8, j=1:6, k=1:12] == vr[:,:,:]
@test [v[i,j] for i=1:8, j=1:72] == vr[:,:]
@test [v[i] for i=1:576] == vr[:]

debug && println("N=4, M=0")
v = strided_view(avparent, (4, 6, 4, 3), ContRank{0}, (2, 8, 56, 336))
isa(v, StridedView{Float64, 4, 2})
@test ndims(v) == 4
@test length(v) == 288
@test iscontiguous(v) == false
@test contiguousrank(v) == 0

@test size(v) == (4, 6, 4, 3)
@test Int[size(v,i) for i=1:5] == [4, 6, 4, 3, 1]
@test strides(v) == (2, 8, 56, 336)
@test Int[stride(v,i) for i=1:4] == [2, 8, 56, 336]

vr = avparent[1:2:7, 1:6, 1:4, 1:3]
@test [v[i,j,k,l] for i=1:4,j=1:6,k=1:4,l=1:3] == vr
@test [v[i,j,k,l,1] for i=1:4,j=1:6,k=1:4,l=1:3] == vr
@test [v[i,j,k] for i=1:4,j=1:6,k=1:12] == vr[:,:,:]
@test [v[i,j] for i=1:4,j=1:72] == vr[:,:]
@test [v[i] for i=1:288] == vr[:]
