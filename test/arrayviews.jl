importall Base.ArrayViews
import Base.ArrayViews.ContRank

#### Testing for Contiguous Views

avparent = rand(5, 4, 3, 2)

## 1D
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

## 2D
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

## 3D
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

## 4D
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


#### Testing for Strided Views

avparent = reshape(1.:1680., (8, 7, 6, 5))

# N=1, M=1 
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

# N=1, M=0
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

# N=2, M=2
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

### N=2, M=1
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

### N=2, M=0
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

### N=3, M=3
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

### N=3, M=2
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

### N=3, M=1
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

### N=3, M=0
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

### N=4, M=2
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

### N=4, M=0
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


#### Test Subviews

## tools to facilitate array view testing
function _test_arrview(a, r, subs...)
    v = view(a, subs...)

    siz_r = size(r)
    siz_v = size(v)

    if siz_r != siz_v
        error("Incorrect size: get $(siz_v), but expect $(siz_r)")
    end

    for i = 1 : length(v)
        if v[i] != r[i]         
            println("v = ")
            println(v)
            println("r = ")
            println(r)
            error("Incorrect content.")
        end
    end
end

macro test_arrview(a_, subs...)
    esc(:(_test_arrview($a_, ($a_)[$(subs...)], $(subs...))))
end

#### test views from arrays

avparent = reshape(1.:1680., (8, 7, 6, 5))

# 1D
@test_arrview(avparent, 3)
@test_arrview(avparent, :)
@test_arrview(avparent, 1:12)
@test_arrview(avparent, 3:2:36)

# 2D
@test_arrview(avparent, 4, 2)
@test_arrview(avparent, 4, :)
@test_arrview(avparent, 4, 3:10)
@test_arrview(avparent, 4, 2:2:10)

@test_arrview(avparent, :, 2)
@test_arrview(avparent, :, :)
@test_arrview(avparent, :, 3:10)
@test_arrview(avparent, :, 2:2:10)

@test_arrview(avparent, 1:6, 2)
@test_arrview(avparent, 1:6, :)
@test_arrview(avparent, 1:6, 3:10)
@test_arrview(avparent, 1:6, 2:2:10)

@test_arrview(avparent, 1:2:8, 2)
@test_arrview(avparent, 1:2:8, :)
@test_arrview(avparent, 1:2:8, 3:10)
@test_arrview(avparent, 1:2:8, 2:2:10)

# 3D
@test_arrview(avparent, 4, 3, 2)
@test_arrview(avparent, 4, 3, :)
@test_arrview(avparent, 4, 3, 2:5)
@test_arrview(avparent, 4, 3, 1:2:6)

@test_arrview(avparent, 4, :, 2)
@test_arrview(avparent, 4, :, :)
@test_arrview(avparent, 4, :, 2:5)
@test_arrview(avparent, 4, :, 1:2:6)

@test_arrview(avparent, 4, 3:7, 2)
@test_arrview(avparent, 4, 3:7, :)
@test_arrview(avparent, 4, 3:7, 2:5)
@test_arrview(avparent, 4, 3:7, 1:2:6)

@test_arrview(avparent, 4, 1:2:5, 2)
@test_arrview(avparent, 4, 1:2:5, :)
@test_arrview(avparent, 4, 1:2:5, 2:5)
@test_arrview(avparent, 4, 1:2:5, 1:2:6)

@test_arrview(avparent, :, 3, 2)
@test_arrview(avparent, :, 3, :)
@test_arrview(avparent, :, 3, 2:5)
@test_arrview(avparent, :, 3, 1:2:6)

@test_arrview(avparent, :, :, 2)
@test_arrview(avparent, :, :, :)
@test_arrview(avparent, :, :, 2:5)
@test_arrview(avparent, :, :, 1:2:6)

@test_arrview(avparent, :, 3:7, 2)
@test_arrview(avparent, :, 3:7, :)
@test_arrview(avparent, :, 3:7, 2:5)
@test_arrview(avparent, :, 3:7, 1:2:6)

@test_arrview(avparent, :, 1:2:5, 2)
@test_arrview(avparent, :, 1:2:5, :)
@test_arrview(avparent, :, 1:2:5, 2:5)
@test_arrview(avparent, :, 1:2:5, 1:2:6)

@test_arrview(avparent, 2:7, 3, 2)
@test_arrview(avparent, 2:7, 3, :)
@test_arrview(avparent, 2:7, 3, 2:5)
@test_arrview(avparent, 2:7, 3, 1:2:6)

@test_arrview(avparent, 2:7, :, 2)
@test_arrview(avparent, 2:7, :, :)
@test_arrview(avparent, 2:7, :, 2:5)
@test_arrview(avparent, 2:7, :, 1:2:6)

@test_arrview(avparent, 2:7, 3:7, 2)
@test_arrview(avparent, 2:7, 3:7, :)
@test_arrview(avparent, 2:7, 3:7, 2:5)
@test_arrview(avparent, 2:7, 3:7, 1:2:6)

@test_arrview(avparent, 2:7, 1:2:5, 2)
@test_arrview(avparent, 2:7, 1:2:5, :)
@test_arrview(avparent, 2:7, 1:2:5, 2:5)
@test_arrview(avparent, 2:7, 1:2:5, 1:2:6)

@test_arrview(avparent, 1:2:7, 3, 2)
@test_arrview(avparent, 1:2:7, 3, :)
@test_arrview(avparent, 1:2:7, 3, 2:5)
@test_arrview(avparent, 1:2:7, 3, 1:2:6)

@test_arrview(avparent, 1:2:7, :, 2)
@test_arrview(avparent, 1:2:7, :, :)
@test_arrview(avparent, 1:2:7, :, 2:5)
@test_arrview(avparent, 1:2:7, :, 1:2:6)

@test_arrview(avparent, 1:2:7, 3:7, 2)
@test_arrview(avparent, 1:2:7, 3:7, :)
@test_arrview(avparent, 1:2:7, 3:7, 2:5)
@test_arrview(avparent, 1:2:7, 3:7, 1:2:6)

@test_arrview(avparent, 1:2:7, 1:2:5, 2)
@test_arrview(avparent, 1:2:7, 1:2:5, :)
@test_arrview(avparent, 1:2:7, 1:2:5, 2:5)
@test_arrview(avparent, 1:2:7, 1:2:5, 1:2:6)

# Some 4D Tests
@test_arrview(avparent, 4, :,     3, 4)
@test_arrview(avparent, 4, :,     :, 4)
@test_arrview(avparent, 4, :,   3:5, 4)
@test_arrview(avparent, 4, :, 1:2:5, 4)

@test_arrview(avparent, :, :,     3, 4)
@test_arrview(avparent, :, :,     :, 4)
@test_arrview(avparent, :, :,   3:5, 4)
@test_arrview(avparent, :, :, 1:2:5, 4)

@test_arrview(avparent, 2:7, :,     3, 4)
@test_arrview(avparent, 2:7, :,     :, 4)
@test_arrview(avparent, 2:7, :,   3:5, 4)
@test_arrview(avparent, 2:7, :, 1:2:5, 4)

@test_arrview(avparent, 1:2:7, :,     3, 4)
@test_arrview(avparent, 1:2:7, :,     :, 4)
@test_arrview(avparent, 1:2:7, :,   3:5, 4)
@test_arrview(avparent, 1:2:7, :, 1:2:5, 4)

@test_arrview(avparent, 4, :,     3, :)
@test_arrview(avparent, 4, :,     :, :)
@test_arrview(avparent, 4, :,   3:5, :)
@test_arrview(avparent, 4, :, 1:2:5, :)

@test_arrview(avparent, :, :,     3, :)
@test_arrview(avparent, :, :,     :, :)
@test_arrview(avparent, :, :,   3:5, :)
@test_arrview(avparent, :, :, 1:2:5, :)

@test_arrview(avparent, 2:7, :,     3, :)
@test_arrview(avparent, 2:7, :,     :, :)
@test_arrview(avparent, 2:7, :,   3:5, :)
@test_arrview(avparent, 2:7, :, 1:2:5, :)

@test_arrview(avparent, 1:2:7, :,     3, :)
@test_arrview(avparent, 1:2:7, :,     :, :)
@test_arrview(avparent, 1:2:7, :,   3:5, :)
@test_arrview(avparent, 1:2:7, :, 1:2:5, :)

@test_arrview(avparent, 4, :,     3, 2:5)
@test_arrview(avparent, 4, :,     :, 2:5)
@test_arrview(avparent, 4, :,   3:5, 2:5)
@test_arrview(avparent, 4, :, 1:2:5, 2:5)

@test_arrview(avparent, :, :,     3, 2:5)
@test_arrview(avparent, :, :,     :, 2:5)
@test_arrview(avparent, :, :,   3:5, 2:5)
@test_arrview(avparent, :, :, 1:2:5, 2:5)

@test_arrview(avparent, 2:7, :,     3, 2:5)
@test_arrview(avparent, 2:7, :,     :, 2:5)
@test_arrview(avparent, 2:7, :,   3:5, 2:5)
@test_arrview(avparent, 2:7, :, 1:2:5, 2:5)

@test_arrview(avparent, 1:2:7, :,     3, 2:5)
@test_arrview(avparent, 1:2:7, :,     :, 2:5)
@test_arrview(avparent, 1:2:7, :,   3:5, 2:5)
@test_arrview(avparent, 1:2:7, :, 1:2:5, 2:5)


#### Test Subviews of Views

function print_subscripts(subs1, subs2)
    println("Error happens on: ")

    subs1s = join([repr(i) for i in subs1], ", ")
    subs2s = join([repr(i) for i in subs2], ", ")

    println("  subs1 = [$subs1s]")
    println("  subs2 = [$subs2s]")
end

function test_arrview2(a, subs1, subs2)
    v = view(a, subs1...)
    v2 = view(v, subs2...)
    v2r = view(copy(v), subs2...)

    siz_v = size(v2)
    siz_r = size(v2r)

    if siz_v != siz_r
        print_subscripts(subs1, subs2)
        error("Incorrect size: get $(siz_v), but expect $(siz_r)")
    end

    for i = 1 : length(v2)
        if v2[i] != v2r[i]         
            print_subscripts(subs1, subs2)
            println("v = ")
            println(v2)
            println("r = ")
            println(v2r)
            error("Incorrect content.")
        end
    end
end

avparent = reshape(1:6912, (12, 12, 8, 6))

# 1D --> 1D
for sa in {(:), 1:36, 2:2:36}
    v1 = view(avparent, sa)
    for sb in {4, (:), 1:length(v1), 3:2:length(v1)}
        test_arrview2(avparent, (sa,), (sb,))
    end
end

# 2D --> 2D
for sa1 in {(:), 1:10, 2:2:12}, sa2 = {(:), 1:12, 2:2:16}
    v1 = view(avparent, sa1, sa2)
    for sb1 in {4, (:), 2:size(v1,1), 2:2:size(v1,1)}, 
        sb2 in {4, (:), 2:size(v1,2), 2:2:size(v1,2)}
        test_arrview2(avparent, (sa1, sa2), (sb1, sb2))
    end
end

# 3D --> 3D
for sa1 in {(:), 1:10, 2:2:12}, 
    sa2 in {(:), 1:10, 2:2:12}, 
    sa3 in {(:), 1:7, 2:2:8}
    v1 = view(avparent, sa1, sa2, sa3)
    (d1, d2, d3) = size(v1)
    for sb1 in {(:), 2:d1, 2:2:d1}, 
        sb2 in {(:), 2:d2, 2:2:d2}, 
        sb3 in {(:), 2:d3, 2:2:d3}
        test_arrview2(avparent, (sa1, sa2, sa3), (sb1, sb2, sb3))
    end
end

# Test Linear Algebra on views

avparent = rand(7, 8)
bvparent = rand(8, 5)

_ab = avparent * bvparent

@test _ab == view(avparent, :, :) * bvparent
@test _ab == avparent * view(bvparent, :, :)
@test _ab == view(avparent, :, :) * view(bvparent, :, :)

for j = 1:size(bvparent,2)
    @test_approx_eq _ab[:,j] view(avparent,:,:) * view(bvparent,:,j)
end

@test avparent[:, 2:2:7] * bvparent[1:3, 1:2:5] == view(avparent, :, 2:2:7) * view(bvparent, 1:3, 1:2:5)

