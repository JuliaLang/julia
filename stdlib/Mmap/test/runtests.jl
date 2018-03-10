# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Mmap, Random

file = tempname()
write(file, "Hello World\n")
t = b"Hello World"
@test Mmap.mmap(file, Array{UInt8,3}, (11,1,1)) == reshape(t,(11,1,1))
GC.gc(); GC.gc()
@test Mmap.mmap(file, Array{UInt8,3}, (1,11,1)) == reshape(t,(1,11,1))
GC.gc(); GC.gc()
@test Mmap.mmap(file, Array{UInt8,3}, (1,1,11)) == reshape(t,(1,1,11))
GC.gc(); GC.gc()
@test Mmap.mmap(file, Array{UInt8,3}, (11,0,1)) == Array{UInt8}(undef, (0,0,0))
@test Mmap.mmap(file, Vector{UInt8}, (11,)) == t
GC.gc(); GC.gc()
@test Mmap.mmap(file, Array{UInt8,2}, (1,11)) == t'
GC.gc(); GC.gc()
@test Mmap.mmap(file, Array{UInt8,2}, (0,12)) == Array{UInt8}(undef, (0,0))
m = Mmap.mmap(file, Array{UInt8,3}, (1,2,1))
@test m == reshape(b"He",(1,2,1))
finalize(m); m=nothing; GC.gc()

# constructors
@test length(@inferred Mmap.mmap(file)) == 12
@test length(@inferred Mmap.mmap(file, Vector{Int8})) == 12
@test length(@inferred Mmap.mmap(file, Matrix{Int8}, (12,1))) == 12
@test length(@inferred Mmap.mmap(file, Matrix{Int8}, (12,1), 0)) == 12
@test length(@inferred Mmap.mmap(file, Matrix{Int8}, (12,1), 0; grow=false)) == 12
@test length(@inferred Mmap.mmap(file, Matrix{Int8}, (12,1), 0; shared=false)) == 12
@test length(@inferred Mmap.mmap(file, Vector{Int8}, 12)) == 12
@test length(@inferred Mmap.mmap(file, Vector{Int8}, 12, 0)) == 12
@test length(@inferred Mmap.mmap(file, Vector{Int8}, 12, 0; grow=false)) == 12
@test length(@inferred Mmap.mmap(file, Vector{Int8}, 12, 0; shared=false)) == 12
s = open(file)
@test length(@inferred Mmap.mmap(s)) == 12
@test length(@inferred Mmap.mmap(s, Vector{Int8})) == 12
@test length(@inferred Mmap.mmap(s, Matrix{Int8}, (12,1))) == 12
@test length(@inferred Mmap.mmap(s, Matrix{Int8}, (12,1), 0)) == 12
@test length(@inferred Mmap.mmap(s, Matrix{Int8}, (12,1), 0; grow=false)) == 12
@test length(@inferred Mmap.mmap(s, Matrix{Int8}, (12,1), 0; shared=false)) == 12
@test length(@inferred Mmap.mmap(s, Vector{Int8}, 12)) == 12
@test length(@inferred Mmap.mmap(s, Vector{Int8}, 12, 0)) == 12
@test length(@inferred Mmap.mmap(s, Vector{Int8}, 12, 0; grow=false)) == 12
@test length(@inferred Mmap.mmap(s, Vector{Int8}, 12, 0; shared=false)) == 12
close(s)
@test_throws ErrorException Mmap.mmap(file, Vector{Ref}) # must be bit-type
GC.gc(); GC.gc()

s = open(f->f,file,"w")
@test Mmap.mmap(file) == Vector{UInt8}() # requested len=0 on empty file
@test Mmap.mmap(file,Vector{UInt8},0) == Vector{UInt8}()
s = open(file, "r+")
m = Mmap.mmap(s,Vector{UInt8},12)
m[:] = b"Hello World\n"
Mmap.sync!(m)
close(s); finalize(m); m=nothing; GC.gc()
@test open(x->read(x, String),file) == "Hello World\n"

s = open(file, "r")
close(s)
@test_throws Base.UVError Mmap.mmap(s) # closed IOStream
@test_throws ArgumentError Mmap.mmap(s,Vector{UInt8},12,0) # closed IOStream
@test_throws SystemError Mmap.mmap("")

# negative length
@test_throws ArgumentError Mmap.mmap(file, Vector{UInt8}, -1)
# negative offset
@test_throws ArgumentError Mmap.mmap(file, Vector{UInt8}, 1, -1)

for i = 0x01:0x0c
    @test length(Mmap.mmap(file, Vector{UInt8}, i)) == Int(i)
end
GC.gc(); GC.gc()

sz = filesize(file)
s = open(file, "r+")
m = Mmap.mmap(s, Vector{UInt8}, sz+1)
@test length(m) == sz+1 # test growing
@test m[end] == 0x00
close(s); finalize(m); m=nothing; GC.gc()
sz = filesize(file)
s = open(file, "r+")
m = Mmap.mmap(s, Vector{UInt8}, 1, sz)
@test length(m) == 1
@test m[1] == 0x00
close(s); finalize(m); m=nothing; GC.gc()
sz = filesize(file)
# test where offset is actually > than size of file; file is grown with zeroed bytes
s = open(file, "r+")
m = Mmap.mmap(s, Vector{UInt8}, 1, sz+1)
@test length(m) == 1
@test m[1] == 0x00
close(s); finalize(m); m=nothing; GC.gc()

s = open(file, "r")
m = Mmap.mmap(s)
@test_throws ReadOnlyMemoryError m[5] = UInt8('x') # tries to setindex! on read-only array
finalize(m); m=nothing; GC.gc()

write(file, "Hello World\n")

s = open(file, "r")
m = Mmap.mmap(s)
close(s)
finalize(m); m=nothing; GC.gc()
m = Mmap.mmap(file)
s = open(file, "r+")
c = Mmap.mmap(s)
d = Mmap.mmap(s)
c[1] = UInt8('J')
Mmap.sync!(c)
close(s)
@test m[1] == UInt8('J')
@test d[1] == UInt8('J')
finalize(m); finalize(c); finalize(d)
m=nothing; c=nothing; d=nothing; GC.gc()

write(file, "Hello World\n")

s = open(file, "r")
@test isreadonly(s) == true
c = Mmap.mmap(s, Vector{UInt8}, (11,))
@test c == b"Hello World"
finalize(c); c=nothing; GC.gc()
c = Mmap.mmap(s, Vector{UInt8}, (UInt16(11),))
@test c == b"Hello World"
finalize(c); c=nothing; GC.gc()
@test_throws ArgumentError Mmap.mmap(s, Vector{UInt8}, (Int16(-11),))
@test_throws ArgumentError Mmap.mmap(s, Vector{UInt8}, (typemax(UInt),))
close(s)
s = open(file, "r+")
@test isreadonly(s) == false
c = Mmap.mmap(s, Vector{UInt8}, (11,))
c[5] = UInt8('x')
Mmap.sync!(c)
close(s)
s = open(file, "r")
str = readline(s)
close(s)
@test startswith(str, "Hellx World")
finalize(c); c=nothing; GC.gc()

c = Mmap.mmap(file)
@test c == b"Hellx World\n"
finalize(c); c=nothing; GC.gc()
c = Mmap.mmap(file, Vector{UInt8}, 3)
@test c == b"Hel"
finalize(c); c=nothing; GC.gc()
s = open(file, "r")
c = Mmap.mmap(s, Vector{UInt8}, 6)
@test c == b"Hellx "
close(s)
finalize(c); c=nothing; GC.gc()
c = Mmap.mmap(file, Vector{UInt8}, 5, 6)
@test c == b"World"
finalize(c); c=nothing; GC.gc()

s = open(file, "w")
write(s, "Hello World\n")
close(s)

# test Mmap.mmap
m = Mmap.mmap(file)
tdata = b"Hello World\n"
for i = 1:12
    @test m[i] == tdata[i]
end
@test_throws BoundsError m[13]
finalize(m); m=nothing; GC.gc()

m = Mmap.mmap(file,Vector{UInt8},6)
@test m[1] == b"H"[1]
@test m[2] == b"e"[1]
@test m[3] == b"l"[1]
@test m[4] == b"l"[1]
@test m[5] == b"o"[1]
@test m[6] == b" "[1]
@test_throws BoundsError m[7]
finalize(m); m=nothing; GC.gc()

m = Mmap.mmap(file,Vector{UInt8},2,6)
@test m[1] == b"W"[1]
@test m[2] == b"o"[1]
@test_throws BoundsError m[3]
finalize(m); m = nothing; GC.gc()

s = open(file, "w")
write(s, [0xffffffffffffffff,
          0xffffffffffffffff,
          0xffffffffffffffff,
          0x000000001fffffff])
close(s)
s = open(file, "r")
@test isreadonly(s)
b = @inferred Mmap.mmap(s, BitArray, (17,13))
@test Test._check_bitarray_consistency(b)
@test b == trues(17,13)
@test_throws ArgumentError Mmap.mmap(s, BitArray, (7,3))
close(s)
s = open(file, "r+")
b = Mmap.mmap(s, BitArray, (17,19))
@test Test._check_bitarray_consistency(b)
rand!(b)
Mmap.sync!(b)
b0 = copy(b)
@test Test._check_bitarray_consistency(b0)
close(s)
s = open(file, "r")
@test isreadonly(s)
b = Mmap.mmap(s, BitArray, (17,19))
@test Test._check_bitarray_consistency(b)
@test b == b0
close(s)
finalize(b); finalize(b0)
b = nothing; b0 = nothing
GC.gc()

open(file,"w") do f
    write(f,UInt64(1))
    write(f,UInt8(1))
end
@test filesize(file) == 9
s = open(file, "r+")
m = Mmap.mmap(s, BitArray, (72,))
@test Test._check_bitarray_consistency(m)
@test length(m) == 72
close(s); finalize(m); m = nothing; GC.gc()
rm(file)

# Mmap.mmap with an offset
A = rand(1:20, 500, 300)
fname = tempname()
s = open(fname, "w+")
write(s, size(A,1))
write(s, size(A,2))
write(s, A)
close(s)
s = open(fname)
m = read(s, Int)
n = read(s, Int)
A2 = Mmap.mmap(s, Matrix{Int}, (m,n))
@test A == A2
seek(s, 0)
A3 = Mmap.mmap(s, Matrix{Int}, (m,n), convert(Int64, 2*sizeof(Int)))
@test A == A3
A4 = Mmap.mmap(s, Matrix{Int}, (m,150), convert(Int64, (2+150*m)*sizeof(Int)))
@test A[:, 151:end] == A4
close(s)
finalize(A2); finalize(A3); finalize(A4)
A2 = A3 = A4 = nothing
GC.gc()
rm(fname)

# Mmap.Anonymous
m = Mmap.Anonymous()
@test m.name == ""
@test !m.readonly
@test m.create
@test isopen(m)
@test isreadable(m)
@test iswritable(m)

m = Mmap.mmap(Vector{UInt8}, 12)
@test length(m) == 12
@test all(m .== 0x00)
@test m[1] === 0x00
@test m[end] === 0x00
m[1] = 0x0a
Mmap.sync!(m)
@test m[1] === 0x0a
m = Mmap.mmap(Vector{UInt8}, 12; shared=false)
m = Mmap.mmap(Vector{Int}, 12)
@test length(m) == 12
@test all(m .== 0)
@test m[1] === 0
@test m[end] === 0
m = Mmap.mmap(Vector{Float64}, 12)
@test length(m) == 12
@test all(m .== 0.0)
m = Mmap.mmap(Matrix{Int8}, (12,12))
@test size(m) == (12,12)
@test all(m == zeros(Int8, (12,12)))
@test sizeof(m) == prod((12,12))
n = similar(m)
@test size(n) == (12,12)
n = similar(m, (2,2))
@test size(n) == (2,2)
n = similar(m, 12)
@test length(n) == 12
@test size(n) == (12,)
finalize(m); m = nothing; GC.gc()

# test #14885
file = tempname()
touch(file)
open(file, "r+") do s
    A = Mmap.mmap(s, Vector{UInt8}, (10,), 0)
    Mmap.sync!(A)
    finalize(A); A = nothing; GC.gc()
    A = Mmap.mmap(s, Vector{UInt8}, (10,), 1)
    Mmap.sync!(A)
    finalize(A); A = nothing; GC.gc()
end
rm(file)
