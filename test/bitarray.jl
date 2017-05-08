# This file is a part of Julia. License is MIT: https://julialang.org/license

module BitArrayTests

using Base.Test
using Base: findprevnot, findnextnot

tc{N}(r1::NTuple{N,Any}, r2::NTuple{N,Any}) = all(x->tc(x...), [zip(r1,r2)...])
tc{N}(r1::BitArray{N}, r2::Union{BitArray{N},Array{Bool,N}}) = true
tc(r1::RowVector{Bool,BitVector}, r2::Union{RowVector{Bool,BitVector},RowVector{Bool,Vector{Bool}}}) = true
tc{T}(r1::T, r2::T) = true
tc(r1,r2) = false

bitcheck(b::BitArray) = Base._check_bitarray_consistency(b)
bitcheck(x) = true

function check_bitop_call(ret_type, func, args...)
    r1 = func(args...)
    r2 = func(map(x->(isa(x, BitArray) ? Array(x) : x), args)...)
    ret_type ≢ nothing && !isa(r1, ret_type) && @show ret_type, r1
    ret_type ≢ nothing && @test isa(r1, ret_type)
    @test tc(r1, r2)
    @test isequal(r1, ret_type ≡ nothing ? r2 : convert(ret_type, r2))
    @test bitcheck(r1)
end
macro check_bit_operation(ex, ret_type)
    @assert Meta.isexpr(ex, :call)
    Expr(:call, :check_bitop_call, esc(ret_type), map(esc, ex.args)...)
end
macro check_bit_operation(ex)
    @assert Meta.isexpr(ex, :call)
    Expr(:call, :check_bitop_call, nothing, map(esc, ex.args)...)
end

let t0 = time()
    global timesofar
    function timesofar(str)
        return # no-op, comment to see timings
        t1 = time()
        println(str, ": ", t1-t0, " seconds")
        t0 = t1
    end
end

@testset "empty bitvector" begin
    @test BitVector() == BitVector(0)
end

# vectors size
v1 = 260
# matrices size
n1, n2 = 17, 20
# arrays size
s1, s2, s3, s4 = 5, 8, 3, 7

allsizes = [((), BitArray{0}), ((v1,), BitVector),
            ((n1,n2), BitMatrix), ((s1,s2,s3,s4), BitArray{4})]

@testset "trues and falses for size $sz" for (sz,T) in allsizes
    a = falses(sz...)
    @test a == falses(sz)
    @test !any(a)
    @test sz == size(a)
    b = trues(sz...)
    @test b == trues(sz)
    @test all(b)
    @test sz == size(b)
    c = trues(a)
    @test all(c)
    @test !any(a)
    @test sz == size(c)
    d = falses(b)
    @test !any(d)
    @test all(b)
    @test sz == size(d)
end


@testset "Conversions for size $sz" for (sz, T) in allsizes
    b1 = rand!(falses(sz...))
    @test isequal(BitArray(Array(b1)), b1)
    @test isequal(convert(Array{Float64,ndims(b1)}, b1),
                  convert(Array{Float64,ndims(b1)}, Array(b1)))
    @test isequal(convert(AbstractArray{Float64,ndims(b1)}, b1),
                  convert(AbstractArray{Float64,ndims(b1)}, Array(b1)))

    i1 = rand!(zeros(Bool, sz...), false:true)
    @test isequal(Array(BitArray(i1)), i1)
end

timesofar("conversions")

@testset "utility functions" begin
    b1 = bitrand(v1)
    @test isequal(fill!(b1, true), trues(size(b1)))
    @test isequal(fill!(b1, false), falses(size(b1)))

    for (sz,T) in allsizes
        @test isequal(Array(trues(sz...)), ones(Bool, sz...))
        @test isequal(Array(falses(sz...)), zeros(Bool, sz...))

        b1 = rand!(falses(sz...))
        @test isa(b1, T)

        @check_bit_operation length(b1) Int
        @check_bit_operation ndims(b1)  Int
        @check_bit_operation size(b1)   Tuple{Vararg{Int}}

        b2 = similar(b1)
        u1 = Array(b1)
        @check_bit_operation copy!(b2, b1) T
        @check_bit_operation copy!(b2, u1) T
    end

    @testset "copy!" begin
        for n in [1; 1023:1025]
            b1 = falses(n)
            for m in [1; 10; 1023:1025]
                u1 = ones(Bool, m)
                for fu! in [u->fill!(u, true), u->rand!(u)]
                    fu!(u1)
                    c1 = convert(Vector{Int}, u1)
                    for i1 in [1; 10; 53:65; 1013:1015; 1020:1025], i2 in [1; 3; 10; 511:513], l in [1; 5; 10; 511:513; 1023:1025]
                        for fb! in [b->fill!(b, false), b->rand!(b)]
                            fb!(b1)
                            if i1 < 1 || i1 > n || (i2 + l - 1 > m) || (i1 + l - 1 > n)
                                @test_throws BoundsError copy!(b1, i1, u1, i2, l)
                            else
                                @check_bit_operation copy!(b1, i1, u1, i2, l) BitArray
                                @check_bit_operation copy!(b1, i1, c1, i2, l) BitArray
                            end
                        end
                    end
                end
            end
        end
    end

    @test_throws BoundsError size(trues(5), 0)

    @testset "reshape and resize!" begin
        b1 = bitrand(n1, n2)
        @check_bit_operation reshape(b1, (n2,n1)) BitMatrix
        @test_throws DimensionMismatch reshape(b1, (1,n1))

        @test @inferred(reshape(b1, n1*n2)) == @inferred(reshape(b1, (n1*n2,))) == @inferred(reshape(b1, Val{1})) == @inferred(reshape(b1, :))
        @test @inferred(reshape(b1, n1, n2)) === @inferred(reshape(b1, Val{2})) === b1
        @test @inferred(reshape(b1, n2, :)) == @inferred(reshape(b1, (n2, n1))) != @inferred(reshape(b1, Val{2}))

        b1 = bitrand(s1, s2, s3, s4)
        @check_bit_operation reshape(b1, (s3,s1,s2,s4)) BitArray{4}
        @test_throws DimensionMismatch reshape(b1, (1,n1))

        b1 = bitrand(v1)
        @test_throws BoundsError resize!(b1, -1)
        @check_bit_operation resize!(b1, v1 ÷ 2) BitVector
        gr(b) = (resize!(b, v1)[(v1÷2):end] = 1; b)
        @check_bit_operation gr(b1) BitVector
    end

    @testset "sizeof (issue #7515)" begin
        @test sizeof(BitArray(64)) == 8
        @test sizeof(BitArray(65)) == 16
    end
end

timesofar("utils")

@testset "Constructors" begin
    @testset "non-Int dims constructors" begin
        b1 = BitArray(Int32(v1))
        b2 = BitArray(Int64(v1))
        @test size(b1) == size(b2)

        for c in [trues, falses]
            b1 = c(Int32(v1))
            b2 = c(Int64(v1))
            @test b1 == b2
        end
    end

    @testset "constructors from iterables" begin
        for g in ((x%7==3 for x = 1:v1),
                  (x%7==3 for x = 1:v1 if x>5),
                  ((x+y)%5==2 for x = 1:n1, y = 1:n2),
                  ((x+y+z+t)%5==2 for x = 1:s2, y = 1:s2, z = 1:s3, t = 1:s4),
                  ((x+y)%5==2 for x = 1:n1 for y = 1:n2))
            @test BitArray(g) == BitArray(collect(g))
        end
    end

    @testset "one" begin
        @test Array(one(BitMatrix(2,2))) == eye(2,2)
        @test_throws DimensionMismatch one(BitMatrix(2,3))
    end
end

timesofar("constructors")

@testset "Indexing" begin
    @testset "0d for size $sz" for (sz,T) in allsizes
        b1 = rand!(falses(sz...))
        @check_bit_operation getindex(b1)         Bool
        @check_bit_operation setindex!(b1, true)  T
        @check_bit_operation setindex!(b1, false) T
    end

    @testset "linear for size $sz" for (sz,T) in allsizes[2:end]
        l = *(sz...)
        b1 = rand!(falses(sz...))
        for j = 1:l
            @check_bit_operation getindex(b1, j) Bool
        end
        for j in [0, 1, 63, 64, 65, 127, 128, 129, 191, 192, 193, l-1, l]
            @check_bit_operation getindex(b1, 1:j)   BitVector
            @check_bit_operation getindex(b1, j+1:l) BitVector
        end
        for j in [1, 63, 64, 65, 127, 128, 129, div(l,2)]
            m1 = j:(l-j)
            @check_bit_operation getindex(b1, m1) BitVector
        end

        t1 = find(bitrand(l))
        @check_bit_operation getindex(b1, t1)        BitVector

        for j = 1:l
            x = rand(Bool)
            @check_bit_operation setindex!(b1, x, j) T
        end

        y = rand(0.0:1.0)
        @check_bit_operation setindex!(b1, y, 100) T

        for j in [1, 63, 64, 65, 127, 128, 129, 191, 192, 193, l-1]
            x = rand(Bool)
            @check_bit_operation setindex!(b1, x, 1:j) T
            b2 = bitrand(j)
            for bb in (b2, view(b2, 1:j), view(Array{Any}(b2), :))
                @check_bit_operation setindex!(b1, bb, 1:j) T
            end
            x = rand(Bool)
            @check_bit_operation setindex!(b1, x, j+1:l) T
            b2 = bitrand(l-j)
            @check_bit_operation setindex!(b1, b2, j+1:l) T
        end
        for j in [1, 63, 64, 65, 127, 128, 129, div(l,2)]
            m1 = j:(l-j)
            x = rand(Bool)
            @check_bit_operation setindex!(b1, x, m1) T
            b2 = bitrand(length(m1))
            @check_bit_operation setindex!(b1, b2, m1) T
        end
        x = rand(Bool)
        @check_bit_operation setindex!(b1, x, 1:100) T
        b2 = bitrand(100)
        @check_bit_operation setindex!(b1, b2, 1:100) T

        y = rand(0.0:1.0)
        @check_bit_operation setindex!(b1, y, 1:100) T

        t1 = find(bitrand(l))
        x = rand(Bool)
        @check_bit_operation setindex!(b1, x, t1) T
        b2 = bitrand(length(t1))
        @check_bit_operation setindex!(b1, b2, t1) T

        y = rand(0.0:1.0)
        @check_bit_operation setindex!(b1, y, t1) T
    end

    @testset "multidimensional" begin
        rand_m1m2() = rand(1:n1), rand(1:n2)
        b1 = bitrand(n1, n2)
        m1, m2 = rand_m1m2()
        b2 = bitrand(m1, m2)
        @check_bit_operation copy!(b1, b2) BitMatrix

        function gen_getindex_data(c)
            m1, m2 = rand_m1m2()
            put!(c, (m1, m2, Bool))
            m1, m2 = rand_m1m2()
            put!(c, (m1, 1:m2, BitVector))
            put!(c, (m1, :, BitVector))
            m1, m2 = rand_m1m2()
            put!(c, (m1, randperm(m2), BitVector))
            m1, m2 = rand_m1m2()
            put!(c, (1:m1, m2, BitVector))
            put!(c, (:, m2, BitVector))
            m1, m2 = rand_m1m2()
            put!(c, (1:m1, 1:m2, BitMatrix))
            put!(c, (:, :, BitMatrix))
            m1, m2 = rand_m1m2()
            put!(c, (1:m1, randperm(m2), BitMatrix))
            put!(c, (:, randperm(m2), BitMatrix))
            m1, m2 = rand_m1m2()
            put!(c, (randperm(m1), m2, BitVector))
            m1, m2 = rand_m1m2()
            put!(c, (randperm(m1), 1:m2, BitMatrix))
            put!(c, (randperm(m1), :, BitMatrix))
            m1, m2 = rand_m1m2()
            put!(c, (randperm(m1), randperm(m2), BitMatrix))
        end

        for (k1, k2, T) in Channel(gen_getindex_data)
            # println(typeof(k1), " ", typeof(k2), " ", T) # uncomment to debug
            @check_bit_operation getindex(b1, k1, k2) T
        end

        m1, m2 = rand_m1m2()
        @check_bit_operation getindex(b1, 1:m1, m2, 1) BitVector
        @check_bit_operation getindex(b1, :, randperm(m2), 1) BitMatrix

        b1 = bitrand(s1, s2, s3, s4)
        function gen_getindex_data4(c)
            m1, m2, m3, m4 = (:, :, :, :)
            put!(c, (m1, m2, m3, m4, BitArray{4}))

            m1, m2, m3, m4 = (2, :, :, :)
            put!(c, (m1, m2, m3, m4, BitArray{3}))
            m1, m2, m3, m4 = (:, :, 2, :)
            put!(c, (m1, m2, m3, m4, BitArray{3}))
            m1, m2, m3, m4 = (:, :, :, 2)
            put!(c, (m1, m2, m3, m4, BitArray{3}))

            m1, m2, m3, m4 = (2, :, :, 2)
            put!(c, (m1, m2, m3, m4, BitArray{2}))
            m1, m2, m3, m4 = (:, 2, :, 2)
            put!(c, (m1, m2, m3, m4, BitArray{2}))
            m1, m2, m3, m4 = (2, :, 2, :)
            put!(c, (m1, m2, m3, m4, BitArray{2}))
            m1, m2, m3, m4 = (2, 2, :, :)
            put!(c, (m1, m2, m3, m4, BitArray{2}))

            m1, m2, m3, m4 = (:, 2, 2, 2)
            put!(c, (m1, m2, m3, m4, BitArray{1}))
            m1, m2, m3, m4 = (2, 2, :, 2)
            put!(c, (m1, m2, m3, m4, BitArray{1}))

            m1, m2, m3, m4 = (4, 3:7, 2:2, 2)
            put!(c, (m1, m2, m3, m4, BitArray{2}))
            m1, m2, m3, m4 = (1:2, 5, 1, 2:7)
            put!(c, (m1, m2, m3, m4, BitArray{2}))

            m1, m2, m3, m4 = (2:3, 2:7, 1:2, 4:6)
            put!(c, (m1, m2, m3, m4, BitArray{4}))
        end

        for (k1, k2, k3, k4, T) in Channel(gen_getindex_data4)
            #println(typeof(k1), " ", typeof(k2), " ", typeof(k3), " ", typeof(k4), " ", T) # uncomment to debug
            @check_bit_operation getindex(b1, k1, k2, k3, k4) T
        end

        b1 = bitrand(n1, n2)
        function gen_setindex_data(c)
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), m1, m2))
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), m1, 1:m2))
            put!(c, (rand(Bool), m1, :))
            put!(c, (bitrand(m2), m1, 1:m2))
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), m1, randperm(m2)))
            put!(c, (bitrand(m2), m1, randperm(m2)))
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), 1:m1, m2))
            put!(c, (rand(Bool), :, m2))
            put!(c, (bitrand(m1), 1:m1, m2))
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), 1:m1, 1:m2))
            put!(c, (rand(Bool), :, :))
            put!(c, (bitrand(m1, m2), 1:m1, 1:m2))
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), 1:m1, randperm(m2)))
            put!(c, (rand(Bool), :, randperm(m2)))
            put!(c, (bitrand(m1, m2), 1:m1, randperm(m2)))
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), randperm(m1), m2))
            put!(c, (bitrand(m1), randperm(m1), m2))
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), randperm(m1), 1:m2))
            put!(c, (rand(Bool), randperm(m1), :))
            put!(c, (bitrand(m1,m2), randperm(m1), 1:m2))
            m1, m2 = rand_m1m2()
            put!(c, (rand(Bool), randperm(m1), randperm(m2)))
            put!(c, (bitrand(m1,m2), randperm(m1), randperm(m2)))
        end

        for (b2, k1, k2) in Channel(gen_setindex_data)
            # println(typeof(b2), " ", typeof(k1), " ", typeof(k2)) # uncomment to debug
            for bb in ((b2 isa AbstractArray) ? (b2, view(b2, :), view(Array{Any}(b2), :)) : (b2,))
                @check_bit_operation setindex!(b1, bb, k1, k2) BitMatrix
            end
        end

        m1, m2 = rand_m1m2()
        b2 = bitrand(1, 1, m2)
        @check_bit_operation setindex!(b1, b2, m1, 1:m2) BitMatrix
        x = rand(Bool)
        b2 = bitrand(1, m2, 1)
        @check_bit_operation setindex!(b1, x, m1, 1:m2, 1)  BitMatrix
        @check_bit_operation setindex!(b1, b2, m1, 1:m2, 1) BitMatrix

        b1 = bitrand(s1, s2, s3, s4)
        function gen_setindex_data4(c)
            m1, m2, m3, m4 = (:, :, :, :)
            put!(c, (rand(Bool), m1, m2, m3, m4))
            put!(c, (bitrand(s1, s2, s3, s4), m1, m2, m3, m4))

            m1, m2, m3, m4 = (2, :, :, :)
            put!(c, (rand(Bool), m1, m2, m3, m4))
            put!(c, (bitrand(s2, s3, s4), m1, m2, m3, m4))
            m1, m2, m3, m4 = (:, :, 2, :)
            put!(c, (bitrand(s1, s2, s4), m1, m2, m3, m4))
            m1, m2, m3, m4 = (:, :, :, 2)
            put!(c, (rand(Bool), m1, m2, m3, m4))
            put!(c, (bitrand(s1, s2, s3), m1, m2, m3, m4))

            m1, m2, m3, m4 = (2, :, :, 2)
            put!(c, (rand(Bool), m1, m2, m3, m4))
            put!(c, (bitrand(s2, s3), m1, m2, m3, m4))
            m1, m2, m3, m4 = (:, 2, :, 2)
            put!(c, (rand(Bool), m1, m2, m3, m4))
            put!(c, (bitrand(s1, s3), m1, m2, m3, m4))
            m1, m2, m3, m4 = (2, :, 2, :)
            put!(c, (bitrand(s2, s4), m1, m2, m3, m4))
            m1, m2, m3, m4 = (:, 2, 2, :)
            put!(c, (bitrand(s1, s4), m1, m2, m3, m4))

            m1, m2, m3, m4 = (:, 2, 2, 2)
            put!(c, (bitrand(s1), m1, m2, m3, m4))
            m1, m2, m3, m4 = (2, 2, :, 2)
            put!(c, (bitrand(s3), m1, m2, m3, m4))

            m1, m2, m3, m4 = (4, 3:7, 2:2, 2)
            put!(c, (bitrand(5, 1), m1, m2, m3, m4))
            m1, m2, m3, m4 = (1:2, 5, 1, 2:7)
            put!(c, (rand(Bool), m1, m2, m3, m4))
            put!(c, (bitrand(2, 6), m1, m2, m3, m4))

            m1, m2, m3, m4 = (2:3, 2:7, 1:2, 4:6)
            put!(c, (bitrand(2, 6, 2, 3), m1, m2, m3, m4))
        end

        for (b2, k1, k2, k3, k4) in Channel(gen_setindex_data4)
            # println(typeof(b2), " ", typeof(k1), " ", typeof(k2), " ", typeof(k3), " ", typeof(k4)) # uncomment to debug
            @check_bit_operation setindex!(b1, b2, k1, k2, k3, k4) BitArray{4}
        end

        for p1 = [rand(1:v1) 1 63 64 65 191 192 193]
            for p2 = [rand(1:v1) 1 63 64 65 191 192 193]
                for n = 0 : min(v1 - p1 + 1, v1 - p2 + 1)
                    b1 = bitrand(v1)
                    b2 = bitrand(v1)
                    @check_bit_operation copy!(b1, p1, b2, p2, n) BitVector
                end
            end
        end
    end

    @testset "logical indexing" begin
        b1 = bitrand(n1, n2)

        t1 = bitrand(n1, n2)
        @test isequal(Array(b1[t1]), Array(b1)[t1])
        @test isequal(Array(b1[t1]), Array(b1)[Array(t1)])

        t1 = bitrand(n1)
        t2 = bitrand(n2)
        @test isequal(Array(b1[t1, t2]), Array(b1)[t1, t2])
        @test isequal(Array(b1[t1, t2]), Array(b1)[Array(t1), Array(t2)])

        b1 = bitrand(n1, n2)
        t1 = bitrand(n1, n2)
        @check_bit_operation setindex!(b1, true, t1) BitMatrix

        t1 = bitrand(n1, n2)
        b2 = bitrand(countnz(t1))
        @check_bit_operation setindex!(b1, b2, t1) BitMatrix

        m1 = rand(1:n1)
        m2 = rand(1:n2)
        t1 = bitrand(n1)
        b2 = bitrand(countnz(t1), m2)
        k2 = randperm(m2)
        @check_bit_operation setindex!(b1, b2, t1, 1:m2)       BitMatrix
        @check_bit_operation setindex!(b1, b2, t1, n2-m2+1:n2) BitMatrix
        @check_bit_operation setindex!(b1, b2, t1, k2)         BitMatrix

        t2 = bitrand(n2)
        b2 = bitrand(m1, countnz(t2))
        k1 = randperm(m1)
        @check_bit_operation setindex!(b1, b2, 1:m1, t2)       BitMatrix
        @check_bit_operation setindex!(b1, b2, n1-m1+1:n1, t2) BitMatrix
        @check_bit_operation setindex!(b1, b2, k1, t2)         BitMatrix
    end
end

timesofar("indexing")

@testset "Deque Functionality" begin
    b1 = BitArray(0)
    i1 = Bool[]
    for m = 1:v1
        x = rand(Bool)
        push!(b1, x)
        push!(i1, x)
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
    end

    for m1 = 0:v1, m2 = [0, 1, 63, 64, 65, 127, 128, 129]
        b1 = bitrand(m1)
        b2 = bitrand(m2)
        i1 = Array(b1)
        i2 = Array(b2)
        @test isequal(Array(append!(b1, b2)), append!(i1, i2))
        @test isequal(Array(append!(b1, i2)), append!(i1, b2))
        @test bitcheck(b1)
    end

    for m1 = 0:v1, m2 = [0, 1, 63, 64, 65, 127, 128, 129]
        b1 = bitrand(m1)
        b2 = bitrand(m2)
        i1 = Array(b1)
        i2 = Array(b2)
        @test isequal(Array(prepend!(b1, b2)), prepend!(i1, i2))
        @test isequal(Array(prepend!(b1, i2)), prepend!(i1, b2))
        @test bitcheck(b1)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m = 1:v1
        jb = pop!(b1)
        ji = pop!(i1)
        @test jb == ji
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
    end

    @test length(b1) == 0

    b1 = BitArray(0)
    i1 = Bool[]
    for m = 1:v1
        x = rand(Bool)
        unshift!(b1, x)
        unshift!(i1, x)
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m = 1:v1
        jb = shift!(b1)
        ji = shift!(i1)
        @test jb == ji
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
    end
    @test length(b1) == 0

    b1 = BitArray(0)
    @test_throws BoundsError insert!(b1, 2, false)
    @test_throws BoundsError insert!(b1, 0, false)
    i1 = Array(b1)
    for m = 1:v1
        j = rand(1:m)
        x = rand(Bool)
        @test insert!(b1, j, x) === b1
        insert!(i1, j, x)
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
        x = rand(0:1)
        @test insert!(b1, j, x) === b1
        insert!(i1, j, x)
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m = v1:-1:1
        j = rand(1:m)
        b = splice!(b1, j)
        i = splice!(i1, j)
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
        @test b == i
        @test bitcheck(b)
    end
    @test length(b1) == 0

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m = v1:-1:1
        j = rand(1:m)
        deleteat!(b1, j)
        deleteat!(i1, j)
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
    end
    @test length(b1) == 0
    b1 = bitrand(v1)
    @test_throws ArgumentError deleteat!(b1, [1, 1, 2])
    @test_throws BoundsError deleteat!(b1, [1, length(b1)+1])

    b1 = bitrand(v1)
    i1 = Array(b1)
    for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
        b = splice!(b1, j)
        i = splice!(i1, j)
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
        @test b == i
        @test bitcheck(b)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for j in [63, 64, 65, 127, 128, 129, 191, 192, 193]
        deleteat!(b1, j)
        deleteat!(i1, j)
        @test isequal(Array(b1), i1)
        @test bitcheck(b1)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m1 = 1:v1, m2 = m1:v1
        b2 = copy(b1)
        i2 = copy(i1)
        b = splice!(b2, m1:m2)
        i = splice!(i2, m1:m2)
        @test isequal(Array(b2), i2)
        @test bitcheck(b2)
        @test b == i
        @test bitcheck(b)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m1 = 1:v1, m2 = m1:v1
        b2 = copy(b1)
        i2 = copy(i1)
        deleteat!(b2, m1:m2)
        deleteat!(i2, m1:m2)
        @test isequal(Array(b2), i2)
        @test bitcheck(b2)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m1 = [1:7:v1; v1+1], m2 = [(m1-1):5:(v1-1); v1], v2 = [0, 1, 63, 64, 65, 127, 128, 129, 191, 192, 193, rand(1:v1)]
        b2 = copy(b1)
        i2 = copy(i1)
        b3 = bitrand(v2)
        i3 = Array(b3)
        b = splice!(b2, m1:m2, b3)
        i = splice!(i2, m1:m2, i3)
        @test isequal(Array(b2), i2)
        @test bitcheck(b2)
        @test b == i
        @test bitcheck(b)

        b2 = copy(b1)
        i2 = copy(i1)
        i3 = map(Int, bitrand(v2))
        b = splice!(b2, m1:m2, i3)
        i = splice!(i2, m1:m2, i3)
        @test isequal(Array(b2), i2)
        @test bitcheck(b2)
        @test b == i
        @test bitcheck(b)
        b2 = copy(b1)
        i2 = copy(i1)
        i3 = Dict(j => rand(0:1) for j = 1:v2)
        b = splice!(b2, m1:m2, values(i3))
        i = splice!(i2, m1:m2, values(i3))
        @test isequal(Array(b2), i2)
        @test bitcheck(b2)
        @test b == i
        @test bitcheck(b)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m1 = 1:v1, v2 = [0, 1, 63, 64, 65, 127, 128, 129, 191, 192, 193, rand(1:v1)]
        b2 = copy(b1)
        i2 = copy(i1)
        b3 = bitrand(v2)
        i3 = Array(b3)
        b = splice!(b2, m1, b3)
        i = splice!(i2, m1, i3)
        @test isequal(Array(b2), i2)
        @test bitcheck(b2)
        @test b == i
        @test bitcheck(b)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    for m1 = 1:(v1-1), m2 = (m1+1):v1
        locs = bitrand(m2-m1+1)
        m = [m1:m2...][locs]
        b2 = copy(b1)
        i2 = copy(i1)
        deleteat!(b2, m)
        deleteat!(i2, m)
        @test isequal(Array(b2), i2)
        @test bitcheck(b2)
    end

    b1 = bitrand(v1)
    i1 = Array(b1)
    empty!(b1)
    empty!(i1)
    @test isequal(Array(b1), i1)
    @test bitcheck(b1)
end

timesofar("dequeue")

@testset "Unary operators" begin
    b1 = bitrand(n1, n2)
    @check_bit_operation broadcast(~, b1)  BitMatrix
    @check_bit_operation broadcast(!, b1)  BitMatrix
    @check_bit_operation (-)(b1)  Matrix{Int}
    @check_bit_operation broadcast(sign, b1) BitMatrix
    @check_bit_operation real(b1) BitMatrix
    @check_bit_operation imag(b1) BitMatrix
    @check_bit_operation conj(b1) BitMatrix

    b0 = falses(0)
    @check_bit_operation broadcast(~, b0)  BitVector
    @check_bit_operation broadcast(!, b0)  BitVector
    @check_bit_operation (-)(b0)  Vector{Int}
    @check_bit_operation broadcast(sign, b0) BitVector

    @testset "flipbits!" begin
        b1 = bitrand(n1, n2)
        i1 = Array(b1)
        @test flipbits!(b1) == .~i1
        @test bitcheck(b1)
    end
end

timesofar("unary arithmetic")


end # module
