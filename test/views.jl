import Base.Views
using Iterators, Base.Test

A = reshape(1:75, 5, 5, 3)
# Copy the values to a subarray. Do this manually since getindex(A, ...) will have a new meaning
indexes1 = (2:4, 3, 1:2)
Aslice1 = Array(eltype(A), length(indexes1[1]), length(indexes1[3]))
i = 1
for p in product(indexes1...)
    Aslice1[i] = A[p...]
    i += 1
end
Asub1 = reshape(Aslice1, map(length, indexes1))
indexes2 = (2:4, 1:2, 3)
Aslice2 = Array(eltype(A), length(indexes2[1]), length(indexes2[2]))
i = 1
for p in product(indexes2...)
    Aslice2[i] = A[p...]
    i += 1
end
Asub2 = Aslice2  # drop the last singleton dimension

# sliceview
for (indexes,Aslice) in ((indexes1,Aslice1), (indexes2,Aslice2))
    B = Views.sliceview(A, indexes...)
    @test ndims(B) == 2
    @test B[1,1] == Aslice[1,1]
    @test B[2,2] == Aslice[2,2]
    @test B[3] == Aslice[3]
    @test B[4] == Aslice[4]
    @test Aslice == B

#     C = Views.sliceview(B, 1:3, 1:2)
#     @test C == B
#     C = Views.sliceview(B, 2:3, 2)
#     @test ndims(C) == 1
#     @test C[1] == B[2,2]
#     @test C[2] == B[3,2]
#     C = Views.sliceview(B, 2, 1:2)
#     @test ndims(C) == 1
#     @test C[1] == B[2,1]
#     @test C[2] == B[2,2]
# 
#     @test C[1,1] == B[2,1]
#     @test_throws BoundsError C[1,2]
end

# subview
for (indexes,Asub) in ((indexes1,Asub1), (indexes2,Asub2))
    B = Views.subview(A, indexes...)
    @test Asub == B

#     C = Views.subview(B, 1:3, 1, 1:2)
#     @test C == B
#     C = Views.subview(B, 2:3, 1, 2)
#     @test ndims(C) == 1
#     @test C[1] == B[2,2]
#     @test C[2] == B[3,2]
#     C = Views.subview(B, 2, 1, 1:2)
#     @test ndims(C) == 3
#     @test C[1] == B[2,1]
#     @test C[2] == B[2,2]
end

