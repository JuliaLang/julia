# This file is a part of Julia. License is MIT: https://julialang.org/license

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

@testset "Binary arithmetic operators" begin
    @testset "Matrix{Bool}/Matrix{Bool}" begin
        b1 = bitrand(n1, n2)
        b2 = bitrand(n1, n2)
        @check_bit_operation broadcast(&, b1, b2)  BitMatrix
        @check_bit_operation broadcast(|, b1, b2)  BitMatrix
        @check_bit_operation broadcast(xor, b1, b2)  BitMatrix
        @check_bit_operation (+)(b1, b2)  Matrix{Int}
        @check_bit_operation (-)(b1, b2)  Matrix{Int}
        @check_bit_operation broadcast(*, b1, b2) BitMatrix
        @check_bit_operation broadcast(/, b1, b2) Matrix{Float64}
        @check_bit_operation broadcast(^, b1, b2) BitMatrix
        @check_bit_operation (/)(b1,1) Matrix{Float64}

        b2 = trues(n1, n2)
        @check_bit_operation broadcast(div, b1, b2) BitMatrix
        @check_bit_operation broadcast(mod, b1, b2) BitMatrix
        @check_bit_operation broadcast(div, b1, Array(b2)) BitMatrix
        @check_bit_operation broadcast(mod, b1, Array(b2)) BitMatrix
        @check_bit_operation broadcast(div, Array(b1), b2) BitMatrix
        @check_bit_operation broadcast(mod, Array(b1), b2) BitMatrix

        b1 = bitrand(n1, n1)
        while abs(det(Array{Float64}(b1))) ≤ 1e-6
            b1 = bitrand(n1, n1)
        end
        b2 = bitrand(n1, n1)

        @check_bit_operation (*)(b1, b2) Matrix{Int}
        @check_bit_operation (/)(b1, b1) Matrix{Float64}
        @check_bit_operation (\)(b1, b1) Matrix{Float64}

        b0 = falses(0)
        @check_bit_operation broadcast(&, b0, b0)  BitVector
        @check_bit_operation broadcast(|, b0, b0)  BitVector
        @check_bit_operation broadcast(xor, b0, b0)  BitVector
        @check_bit_operation broadcast(*, b0, b0) BitVector
        @check_bit_operation (*)(b0, b0') BitMatrix
    end

    @testset "Matrix{Bool}/Matrix{Int}" begin
        b1 = bitrand(n1, n2)
        i2 = rand(1:10, n1, n2)
        @check_bit_operation broadcast(&, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(|, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(xor, b1, i2)  Matrix{Int}
        @check_bit_operation (+)(b1, i2)  Matrix{Int}
        @check_bit_operation (-)(b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(*, b1, i2) Matrix{Int}
        @check_bit_operation broadcast(/, b1, i2) Matrix{Float64}
        @check_bit_operation broadcast(^, b1, i2) BitMatrix
        @check_bit_operation broadcast(div, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(mod, b1, i2)  Matrix{Int}
    end

    @testset "Matrix{Bool}/Matrix{Float64}" begin
        b1 = bitrand(n1, n2)
        f2 = 1.0 .+ rand(n1, n2)
        @check_bit_operation broadcast(*, b1, f2) Matrix{Float64}
        @check_bit_operation broadcast(/, b1, f2) Matrix{Float64}
        @check_bit_operation broadcast(^, b1, f2) Matrix{Float64}
        @check_bit_operation broadcast(div, b1, f2)  Matrix{Float64}
        @check_bit_operation broadcast(mod, b1, f2)  Matrix{Float64}
    end

    @testset "Number/Matrix" begin
        b2 = bitrand(n1, n2)
        i1 = rand(1:10)
        u1 = UInt8(i1)
        f1 = Float64(i1)
        ci1 = complex(i1)
        cu1 = complex(u1)
        cf1 = complex(f1)

        @check_bit_operation broadcast(&, i1, b2)  Matrix{Int}
        @check_bit_operation broadcast(|, i1, b2)  Matrix{Int}
        @check_bit_operation broadcast(xor, i1, b2)  Matrix{Int}
        @check_bit_operation broadcast(+, i1, b2)  Matrix{Int}
        @check_bit_operation broadcast(-, i1, b2)  Matrix{Int}
        @check_bit_operation broadcast(*, i1, b2) Matrix{Int}

        @check_bit_operation broadcast(&, u1, b2)  Matrix{UInt8}
        @check_bit_operation broadcast(|, u1, b2)  Matrix{UInt8}
        @check_bit_operation broadcast(xor, u1, b2)  Matrix{UInt8}
        @check_bit_operation broadcast(+, u1, b2)  Matrix{UInt8}
        @check_bit_operation broadcast(-, u1, b2)  Matrix{UInt8}
        @check_bit_operation broadcast(*, u1, b2) Matrix{UInt8}

        for (x1,t1) = [(f1, Float64),
                       (ci1, Complex{Int}),
                       (cu1, Complex{UInt8}),
                       (cf1, Complex128)]
            @check_bit_operation broadcast(+, x1, b2)  Matrix{t1}
            @check_bit_operation broadcast(-, x1, b2)  Matrix{t1}
            @check_bit_operation broadcast(*, x1, b2) Matrix{t1}
        end

        b2 = trues(n1, n2)
        @check_bit_operation broadcast(/, true, b2)  Matrix{Float64}
        @check_bit_operation broadcast(div, true, b2)   BitMatrix
        @check_bit_operation broadcast(mod, true, b2)   BitMatrix
        @check_bit_operation broadcast(/, false, b2) Matrix{Float64}
        @check_bit_operation broadcast(div, false, b2)  BitMatrix
        @check_bit_operation broadcast(mod, false, b2)  BitMatrix

        @check_bit_operation broadcast(/, i1, b2) Matrix{Float64}
        @check_bit_operation broadcast(div, i1, b2)  Matrix{Int}
        @check_bit_operation broadcast(mod, i1, b2)  Matrix{Int}

        @check_bit_operation broadcast(/, u1, b2) Matrix{Float64}
        @check_bit_operation broadcast(div, u1, b2)  Matrix{UInt8}
        @check_bit_operation broadcast(mod, u1, b2)  Matrix{UInt8}

        @check_bit_operation broadcast(/, f1, b2) Matrix{Float64}
        @check_bit_operation broadcast(div, f1, b2)  Matrix{Float64}
        @check_bit_operation broadcast(mod, f1, b2)  Matrix{Float64}

        @check_bit_operation broadcast(/, ci1, b2) Matrix{Complex128}
        @check_bit_operation broadcast(/, cu1, b2) Matrix{Complex128}
        @check_bit_operation broadcast(/, cf1, b2) Matrix{Complex128}

        b2 = bitrand(n1, n2)
        @check_bit_operation broadcast(^, false, b2) BitMatrix
        @check_bit_operation broadcast(^, true, b2)  BitMatrix
        @check_bit_operation broadcast(^, 0x0, b2)   Matrix{UInt8}
        @check_bit_operation broadcast(^, 0x1, b2)   Matrix{UInt8}
        @check_bit_operation broadcast(^, -1, b2)    Matrix{Int}
        @check_bit_operation broadcast(^, 0, b2)     Matrix{Int}
        @check_bit_operation broadcast(^, 1, b2)     Matrix{Int}
        @check_bit_operation broadcast(^, 0.0, b2)   Matrix{Float64}
        @check_bit_operation broadcast(^, 1.0, b2)   Matrix{Float64}
        @check_bit_operation broadcast(^, 0.0im, b2) Matrix{Complex128}
        @check_bit_operation broadcast(^, 1.0im, b2) Matrix{Complex128}
        @check_bit_operation broadcast(^, 0im, b2)   Matrix{Complex{Int}}
        @check_bit_operation broadcast(^, 1im, b2)   Matrix{Complex{Int}}
        @check_bit_operation broadcast(^, 0x0im, b2) Matrix{Complex{UInt8}}
        @check_bit_operation broadcast(^, 0x1im, b2) Matrix{Complex{UInt8}}
    end

    @testset "Matrix/Number" begin
        b1 = bitrand(n1, n2)
        i2 = rand(1:10)
        u2 = UInt8(i2)
        f2 = Float64(i2)
        ci2 = complex(i2)
        cu2 = complex(u2)
        cf2 = complex(f2)
        b2 = Array(bitrand(n1,n2))

        @check_bit_operation broadcast(&, b1, true)   BitMatrix
        @check_bit_operation broadcast(&, b1, false)  BitMatrix
        @check_bit_operation broadcast(&, true, b1)   BitMatrix
        @check_bit_operation broadcast(&, false, b1)  BitMatrix
        @check_bit_operation broadcast(|, b1, true)   BitMatrix
        @check_bit_operation broadcast(|, b1, false)  BitMatrix
        @check_bit_operation broadcast(|, true, b1)   BitMatrix
        @check_bit_operation broadcast(|, false, b1)  BitMatrix
        @check_bit_operation broadcast(xor, b1, true)   BitMatrix
        @check_bit_operation broadcast(xor, b1, false)  BitMatrix
        @check_bit_operation broadcast(xor, true, b1)   BitMatrix
        @check_bit_operation broadcast(xor, false, b1)  BitMatrix
        @check_bit_operation broadcast(+, b1, true)   Matrix{Int}
        @check_bit_operation broadcast(+, b1, false)  Matrix{Int}
        @check_bit_operation broadcast(-, b1, true)   Matrix{Int}
        @check_bit_operation broadcast(-, b1, false)  Matrix{Int}
        @check_bit_operation broadcast(*, b1, true)  BitMatrix
        @check_bit_operation broadcast(*, b1, false) BitMatrix
        @check_bit_operation broadcast(*, true, b1)  BitMatrix
        @check_bit_operation broadcast(*, false, b1) BitMatrix
        @check_bit_operation broadcast(/, b1, true)  Matrix{Float64}
        @check_bit_operation broadcast(/, b1, false) Matrix{Float64}
        @check_bit_operation broadcast(div, b1, true)   BitMatrix
        @check_bit_operation broadcast(mod,b1, true)    BitMatrix

        @check_bit_operation broadcast(&, b1, b2)  BitMatrix
        @check_bit_operation broadcast(|, b1, b2)  BitMatrix
        @check_bit_operation broadcast(xor, b1, b2)  BitMatrix
        @check_bit_operation broadcast(&, b2, b1)  BitMatrix
        @check_bit_operation broadcast(|, b2, b1)  BitMatrix
        @check_bit_operation broadcast(xor, b2, b1)  BitMatrix
        @check_bit_operation broadcast(&, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(|, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(xor, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(+, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(-, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(*, b1, i2) Matrix{Int}
        @check_bit_operation broadcast(/, b1, i2) Matrix{Float64}
        @check_bit_operation broadcast(div, b1, i2)  Matrix{Int}
        @check_bit_operation broadcast(mod, b1, i2)  Matrix{Int}

        @check_bit_operation broadcast(&, b1, u2)  Matrix{UInt8}
        @check_bit_operation broadcast(|, b1, u2)  Matrix{UInt8}
        @check_bit_operation broadcast(xor, b1, u2)  Matrix{UInt8}
        @check_bit_operation broadcast(+, b1, u2)  Matrix{UInt8}
        @check_bit_operation broadcast(-, b1, u2)  Matrix{UInt8}
        @check_bit_operation broadcast(*, b1, u2) Matrix{UInt8}
        @check_bit_operation broadcast(/, b1, u2) Matrix{Float64}
        @check_bit_operation broadcast(div, b1, u2)  Matrix{UInt8}
        @check_bit_operation broadcast(mod, b1, u2)  Matrix{UInt8}

        @check_bit_operation broadcast(+, b1, f2)  Matrix{Float64}
        @check_bit_operation broadcast(-, b1, f2)  Matrix{Float64}
        @check_bit_operation broadcast(*, b1, f2) Matrix{Float64}
        @check_bit_operation broadcast(/, b1, f2) Matrix{Float64}
        @check_bit_operation broadcast(div, b1, f2)  Matrix{Float64}
        @check_bit_operation broadcast(mod, b1, f2)  Matrix{Float64}

        @check_bit_operation broadcast(+, b1, ci2)  Matrix{Complex{Int}}
        @check_bit_operation broadcast(-, b1, ci2)  Matrix{Complex{Int}}
        @check_bit_operation broadcast(*, b1, ci2) Matrix{Complex{Int}}
        @check_bit_operation broadcast(/, b1, ci2) Matrix{Complex128}

        @check_bit_operation broadcast(+, b1, cu2)  Matrix{Complex{UInt8}}
        @check_bit_operation broadcast(-, b1, cu2)  Matrix{Complex{UInt8}}
        @check_bit_operation broadcast(*, b1, cu2) Matrix{Complex{UInt8}}
        @check_bit_operation broadcast(/, b1, cu2) Matrix{Complex128}

        @check_bit_operation broadcast(+, b1, cf2)  Matrix{Complex128}
        @check_bit_operation broadcast(-, b1, cf2)  Matrix{Complex128}
        @check_bit_operation broadcast(*, b1, cf2) Matrix{Complex128}
        @check_bit_operation broadcast(/, b1, cf2) Matrix{Complex128}

        @check_bit_operation broadcast(^, b1, false) BitMatrix
        @check_bit_operation broadcast(^, b1, true)  BitMatrix
        @check_bit_operation broadcast(^, b1, 0x0)   BitMatrix
        @check_bit_operation broadcast(^, b1, 0x1)   BitMatrix
        @check_bit_operation broadcast(^, b1, 0)     BitMatrix
        @check_bit_operation broadcast(^, b1, 1)     BitMatrix
        @check_bit_operation broadcast(^, b1, -1.0)  Matrix{Float64}
        @check_bit_operation broadcast(^, b1, 0.0)   Matrix{Float64}
        @check_bit_operation broadcast(^, b1, 1.0)   Matrix{Float64}
        @check_bit_operation broadcast(^, b1, 0.0im) Matrix{Complex128}
        @check_bit_operation broadcast(^, b1, 0x0im) Matrix{Complex128}
        @check_bit_operation broadcast(^, b1, 0im)   Matrix{Complex128}
        @test_throws DomainError broadcast(^, b1, -1)

        b1 = trues(n1, n2)
        @check_bit_operation broadcast(^, b1, -1.0im) Matrix{Complex128}
        @check_bit_operation broadcast(^, b1, 1.0im)  Matrix{Complex128}
        @check_bit_operation broadcast(^, b1, -1im)   Matrix{Complex128}
        @check_bit_operation broadcast(^, b1, 1im)    Matrix{Complex128}
        @check_bit_operation broadcast(^, b1, 0x1im)  Matrix{Complex128}
    end
end

timesofar("binary arithmetic")

@testset "Binary comparison operators" begin
    b1 = bitrand(n1, n2)
    b2 = bitrand(n1, n2)
    @check_bit_operation broadcast(==, b1, b2) BitMatrix
    @check_bit_operation broadcast(!=, b1, b2) BitMatrix
    @check_bit_operation broadcast(<, b1, b2) BitMatrix
    @check_bit_operation broadcast(<=, b1, b2) BitMatrix
end

timesofar("binary comparison")

@testset "Data movement" begin
    b1 = bitrand(s1, s2, s3, s4)
    for d = 1:4
        j = rand(1:size(b1, d))
        #for j = 1 : size(b1, d)
            @check_bit_operation slicedim(b1, d, j) BitArray{3}
        #end
        @check_bit_operation flipdim(b1, d) BitArray{4}
    end
    @test_throws ArgumentError flipdim(b1, 5)

    b1 = bitrand(n1, n2)
    for k = 1:4
        @check_bit_operation rotl90(b1, k) BitMatrix
    end

    for m = 0:v1
        b1 = bitrand(m)
        @check_bit_operation reverse(b1) BitVector
    end

    b1 = bitrand(v1)
    for m = [rand(1:v1)-1, 0, 1, 63, 64, 65, 191, 192, 193, v1-1]
        @test isequal(b1 << m, [ b1[m+1:end]; falses(m) ])
        @test isequal(b1 >>> m, [ falses(m); b1[1:end-m] ])
        @test isequal(b1 << -m, b1 >> m)
        @test isequal(b1 >>> -m, b1 << m)
        @test isequal(rol(b1, m), [ b1[m+1:end]; b1[1:m] ])
        @test isequal(ror(b1, m), [ b1[end-m+1:end]; b1[1:end-m] ])
        @test isequal(ror(b1, m), rol(b1, -m))
        @test isequal(rol(b1, m), ror(b1, -m))
    end

    b = bitrand(v1)
    i = bitrand(v1)
    for m = [rand(1:v1), 63, 64, 65, 191, 192, 193, v1-1]
        j = rand(1:m)
        b1 = ror!(i, b, j)
        i1 = ror!(b, j)
        @test b1 == i1
        b2 = rol!(i1, b1, j)
        i2 = rol!(b1, j)
        @test b2 == i2
    end
end

timesofar("datamove")

@testset "countnz & find" begin
    for m = 0:v1, b1 in Any[bitrand(m), trues(m), falses(m)]
        @check_bit_operation countnz(b1) Int

        @check_bit_operation findfirst(b1) Int

        @check_bit_operation findfirst(b1, true)  Int
        @check_bit_operation findfirst(b1, false) Int
        @check_bit_operation findfirst(b1, 3)     Int

        @check_bit_operation findfirst(x->x, b1)     Int
        @check_bit_operation findfirst(x->!x, b1)    Int
        @check_bit_operation findfirst(x->true, b1)  Int
        @check_bit_operation findfirst(x->false, b1) Int

        @check_bit_operation find(b1) Vector{Int}
    end

    b1 = trues(v1)
    for i = 0:(v1-1)
        @test findfirst(b1 >> i) == i+1
        @test Base.findfirstnot(.~(b1 >> i)) == i+1
    end

    for i = 3:(v1-1), j = 2:i
        submask = b1 << (v1-j+1)
        @test findnext((b1 >> i) .| submask, j) == i+1
        @test findnextnot((.~(b1 >> i)) .⊻ submask, j) == i+1
    end

    b1 = bitrand(n1, n2)
    @check_bit_operation findnz(b1) Tuple{Vector{Int}, Vector{Int}, BitArray}
end

timesofar("nnz&find")

@testset "Findnext/findprev" begin
    b1 = trues(v1)
    b2 = falses(v1)
    for i = 1:v1
        @test findprev(b1, i) == findprev(b1, true, i) == findprev(identity, b1, i)
        @test findprevnot(b2, i) == findprev(!, b2, i) == i
    end


    odds = broadcast(isodd, 1:2000)
    evens = broadcast(iseven, 1:2000)

    for i = 1:2:2000
        @test findprev(odds,i)  == findprevnot(evens,i) == i
        @test findnext(odds,i)  == findnextnot(evens,i) == i
        @test findprev(evens,i) == findprevnot(odds,i)  == i-1
        @test findnext(evens,i) == findnextnot(odds,i)  == (i < 2000 ? i+1 : 0)
    end
    for i = 2:2:2000
        @test findprev(odds,i)  == findprevnot(evens,i) == i-1
        @test findprev(evens,i) == findprevnot(odds,i)  == i
        @test findnext(evens,i) == findnextnot(odds,i)  == i
        @test findnext(odds,i)  == findnextnot(evens,i) == (i < 2000 ? i+1 : 0)
    end

    elts = (1:64:(64*64+1)) .+ (0:64)
    n = maximum(elts)
    for c = [falses, trues]
        b1 = c(n)
        b1[elts] = .!b1[elts]
        b2 = .~b1
        i1 = Array(b1)
        for i = 1:n
            @test findprev(b1, i) == findprev(i1, i) == findprevnot(b2, i) == findprev(!, b2, i)
            @test findnext(b1, i) == findnext(i1, i) == findnextnot(b2, i) == findnext(!, b2, i)
        end
    end

    b1 = falses(1000)
    b1[77] = true
    b1[777] = true
    b2 = .~b1
    @test_throws BoundsError findprev(b1, 1001)
    @test_throws BoundsError findprevnot(b2, 1001)
    @test_throws BoundsError findprev(!, b2, 1001)
    @test_throws BoundsError findprev(identity, b1, 1001)
    @test_throws BoundsError findprev(x->false, b1, 1001)
    @test_throws BoundsError findprev(x->true, b1, 1001)
    @test findprev(b1, 1000) == findprevnot(b2, 1000) == findprev(!, b2, 1000) == 777
    @test findprev(b1, 777)  == findprevnot(b2, 777)  == findprev(!, b2, 777)  == 777
    @test findprev(b1, 776)  == findprevnot(b2, 776)  == findprev(!, b2, 776)  == 77
    @test findprev(b1, 77)   == findprevnot(b2, 77)   == findprev(!, b2, 77)   == 77
    @test findprev(b1, 76)   == findprevnot(b2, 76)   == findprev(!, b2, 76)   == 0
    @test findprev(b1, -1)   == findprevnot(b2, -1)   == findprev(!, b2, -1)   == 0
    @test findprev(identity, b1, -1) == findprev(x->false, b1, -1) == findprev(x->true, b1, -1) == 0
    @test_throws BoundsError findnext(b1, -1)
    @test_throws BoundsError findnextnot(b2, -1)
    @test_throws BoundsError findnext(!, b2, -1)
    @test_throws BoundsError findnext(identity, b1, -1)
    @test_throws BoundsError findnext(x->false, b1, -1)
    @test_throws BoundsError findnext(x->true, b1, -1)
    @test findnext(b1, 1)    == findnextnot(b2, 1)    == findnext(!, b2, 1)    == 77
    @test findnext(b1, 77)   == findnextnot(b2, 77)   == findnext(!, b2, 77)   == 77
    @test findnext(b1, 78)   == findnextnot(b2, 78)   == findnext(!, b2, 78)   == 777
    @test findnext(b1, 777)  == findnextnot(b2, 777)  == findnext(!, b2, 777)  == 777
    @test findnext(b1, 778)  == findnextnot(b2, 778)  == findnext(!, b2, 778)  == 0
    @test findnext(b1, 1001) == findnextnot(b2, 1001) == findnext(!, b2, 1001) == 0
    @test findnext(identity, b1, 1001) == findnext(x->false, b1, 1001) == findnext(x->true, b1, 1001) == 0

    @test findlast(b1) == Base.findlastnot(b2) == 777
    @test findfirst(b1) == Base.findfirstnot(b2) == 77

    b0 = BitVector(0)
    @test findprev(x->true, b0, -1) == 0
    @test_throws BoundsError findprev(x->true, b0, 1)
    @test_throws BoundsError findnext(x->true, b0, -1)
    @test findnext(x->true, b0, 1) == 0

    b1 = falses(10)
    @test findprev(x->true, b1, 5) == 5
    @test findnext(x->true, b1, 5) == 5
    @test findprev(x->true, b1, -1) == 0
    @test findnext(x->true, b1, 11) == 0
    @test findprev(x->false, b1, 5) == 0
    @test findnext(x->false, b1, 5) == 0
    @test findprev(x->false, b1, -1) == 0
    @test findnext(x->false, b1, 11) == 0
    @test_throws BoundsError findprev(x->true, b1, 11)
    @test_throws BoundsError findnext(x->true, b1, -1)

    for l = [1, 63, 64, 65, 127, 128, 129]
        f = falses(l)
        t = trues(l)
        @test findprev(f, l) == findprevnot(t, l) == 0
        @test findprev(t, l) == findprevnot(f, l) == l
        b1 = falses(l)
        b1[end] = true
        b2 = .~b1
        @test findprev(b1, l) == findprevnot(b2, l) == l
        @test findprevnot(b1, l) == findprev(b2, l) == l-1
        if l > 1
            b1 = falses(l)
            b1[end-1] = true
            b2 = .~b1
            @test findprev(b1, l) == findprevnot(b2, l) == l-1
            @test findprevnot(b1, l) == findprev(b2, l) == l
        end
    end
end

@testset "Reductions" begin
    b1 = bitrand(s1, s2, s3, s4)
    m1 = 1
    m2 = 3
    @check_bit_operation maximum(b1, (m1, m2)) BitArray{4}
    @check_bit_operation minimum(b1, (m1, m2)) BitArray{4}
    @check_bit_operation sum(b1, (m1, m2)) Array{Int,4}

    @check_bit_operation maximum(b1) Bool
    @check_bit_operation minimum(b1) Bool
    @check_bit_operation any(b1) Bool
    @check_bit_operation all(b1) Bool
    @check_bit_operation sum(b1) Int

    b0 = falses(0)
    @check_bit_operation any(b0) Bool
    @check_bit_operation all(b0) Bool
    @check_bit_operation sum(b0) Int
end

timesofar("reductions")

@testset "map over bitarrays" begin
    for l = [0, 1, 63, 64, 65, 127, 128, 129, 255, 256, 257, 6399, 6400, 6401]
        b1 = bitrand(l)
        b2 = bitrand(l)
        @test map(~, b1) == map(x->~x, b1) == broadcast(~, b1)
        @test map(identity, b1) == map(x->x, b1) == b1

        @test map(&, b1, b2) == map((x,y)->x&y, b1, b2) == broadcast(&, b1, b2)
        @test map(|, b1, b2) == map((x,y)->x|y, b1, b2) == broadcast(|, b1, b2)
        @test map(⊻, b1, b2) == map((x,y)->x⊻y, b1, b2) == broadcast(⊻, b1, b2) == broadcast(xor, b1, b2)

        @test map(^, b1, b2) == map((x,y)->x^y, b1, b2) == b1 .^ b2
        @test map(*, b1, b2) == map((x,y)->x*y, b1, b2) == b1 .* b2

        @test map(min, b1, b2) == map((x,y)->min(x,y), b1, b2) == min.(b1, b2)
        @test map(max, b1, b2) == map((x,y)->max(x,y), b1, b2) == max.(b1, b2)

        @test map(<, b1, b2)  == map((x,y)->x<y, b1, b2)  == (b1 .< b2)
        @test map(<=, b1, b2) == map((x,y)->x<=y, b1, b2) == (b1 .<= b2)
        @test map(==, b1, b2) == map((x,y)->x==y, b1, b2) == (b1 .== b2)
        @test map(>=, b1, b2) == map((x,y)->x>=y, b1, b2) == (b1 .>= b2)
        @test map(>, b1, b2)  == map((x,y)->x>y, b1, b2)  == (b1 .> b2)
        @test map(!=, b1, b2) == map((x,y)->x!=y, b1, b2) == (b1 .!= b2)

        @testset "map! for length $l" begin
            b = BitArray(l)
            @test map!(~, b, b1) == map!(x->~x, b, b1) == broadcast(~, b1) == b
            @test map!(!, b, b1) == map!(x->!x, b, b1) == broadcast(~, b1) == b
            @test map!(identity, b, b1) == map!(x->x, b, b1) == b1 == b
            @test map!(zero, b, b1) == map!(x->false, b, b1) == falses(l) == b
            @test map!(one, b, b1) == map!(x->true, b, b1) == trues(l) == b

            @test map!(&, b, b1, b2) == map!((x,y)->x&y, b, b1, b2) == broadcast(&, b1, b2) == b
            @test map!(|, b, b1, b2) == map!((x,y)->x|y, b, b1, b2) == broadcast(|, b1, b2) == b
            @test map!(⊻, b, b1, b2) == map!((x,y)->x⊻y, b, b1, b2) == broadcast(⊻, b1, b2) == broadcast(xor, b1, b2) == b

            @test map!(^, b, b1, b2) == map!((x,y)->x^y, b, b1, b2) == b1 .^ b2 == b
            @test map!(*, b, b1, b2) == map!((x,y)->x*y, b, b1, b2) == b1 .* b2 == b

            @test map!(min, b, b1, b2) == map!((x,y)->min(x,y), b, b1, b2) == min.(b1, b2) == b
            @test map!(max, b, b1, b2) == map!((x,y)->max(x,y), b, b1, b2) == max.(b1, b2) == b

            @test map!(<, b, b1, b2)  == map!((x,y)->x<y, b, b1, b2)  == (b1 .< b2)  == b
            @test map!(<=, b, b1, b2) == map!((x,y)->x<=y, b, b1, b2) == (b1 .<= b2) == b
            @test map!(==, b, b1, b2) == map!((x,y)->x==y, b, b1, b2) == (b1 .== b2) == b
            @test map!(>=, b, b1, b2) == map!((x,y)->x>=y, b, b1, b2) == (b1 .>= b2) == b
            @test map!(>, b, b1, b2)  == map!((x,y)->x>y, b, b1, b2)  == (b1 .> b2)  == b
            @test map!(!=, b, b1, b2) == map!((x,y)->x!=y, b, b1, b2) == (b1 .!= b2) == b
        end
    end

    @testset "Issue #17970" begin
        A17970 = [1,2,3] .== [3,2,1]
        B17970 = map(x -> x ? 1 : 2, A17970)
        @test B17970::Array{Int,1} == [2,1,2]
        C17970 = map(x -> x ? false : true, A17970)
        @test C17970::BitArray{1} == map(~, A17970)
    end
end

## Filter ##

# TODO

@testset "Transpose" begin
    b1 = bitrand(v1)
    @check_bit_operation transpose(b1) RowVector{Bool,BitVector}

    for m1 = 0:n1, m2 = 0:n2
        b1 = bitrand(m1, m2)
        @check_bit_operation transpose(b1) BitMatrix
    end
end

timesofar("transpose")

@testset "Permutedims" begin
    b1 = bitrand(s1, s2, s3, s4)
    p = randperm(4)
    @check_bit_operation permutedims(b1, p) BitArray{4}
    @check_bit_operation permutedims(b1, tuple(p...)) BitArray{4}
end

timesofar("permutedims")

@testset "Concatenation" begin
    b1 = bitrand(v1)
    b2 = bitrand(v1)
    @check_bit_operation hcat(b1, b2) BitMatrix
    for m = 1:(v1-1)
        @check_bit_operation vcat(b1[1:m], b1[m+1:end]) BitVector
    end
    @test_throws DimensionMismatch hcat(b1,trues(n1+1))
    @test_throws DimensionMismatch hcat(hcat(b1, b2),trues(n1+1))

    b1 = bitrand(n1, n2)
    b2 = bitrand(n1)
    b3 = bitrand(n1, n2)
    b4 = bitrand(1, n2)
    @check_bit_operation hcat(b1, b2, b3) BitMatrix
    @check_bit_operation vcat(b1, b4, b3) BitMatrix
    @test_throws DimensionMismatch vcat(b1, b4, trues(n1,n2+1))

    b1 = bitrand(s1, s2, s3, s4)
    b2 = bitrand(s1, s3, s3, s4)
    b3 = bitrand(s1, s2, s3, s1)
    @check_bit_operation cat(2, b1, b2) BitArray{4}
    @check_bit_operation cat(4, b1, b3) BitArray{4}
    @check_bit_operation cat(6, b1, b1) BitArray{6}

    b1 = bitrand(1, v1, 1)
    @check_bit_operation cat(2, 0, b1, 1, 1, b1) Array{Int,3}
    @check_bit_operation cat(2, 3, b1, 4, 5, b1) Array{Int,3}
    @check_bit_operation cat(2, false, b1, true, true, b1) BitArray{3}

    b1 = bitrand(n1, n2)
    for m1 = 1:(n1-1), m2 = 1:(n2-1)
        @test isequal([b1[1:m1,1:m2] b1[1:m1,m2+1:end]; b1[m1+1:end,1:m2] b1[m1+1:end,m2+1:end]], b1)
    end
end

timesofar("cat")

@testset "Linear algebra" begin
    b1 = bitrand(v1)
    b2 = bitrand(v1)
    @check_bit_operation dot(b1, b2) Int

    b1 = bitrand(n1, n2)
    for k = (-n1):n2
        @check_bit_operation tril(b1, k) BitMatrix
        @check_bit_operation triu(b1, k) BitMatrix
    end

    for sz = [(n1,n1), (n1,n2), (n2,n1)], (f,isf) = [(tril,istril), (triu,istriu)]
        b1 = bitrand(sz...)
        @check_bit_operation isf(b1) Bool
        b1 = f(bitrand(sz...))
        @check_bit_operation isf(b1) Bool
    end

    b1 = bitrand(n1,n1)
    b1 .|= b1.'
    @check_bit_operation issymmetric(b1) Bool
    @check_bit_operation ishermitian(b1) Bool

    b1 = bitrand(n1)
    b2 = bitrand(n2)
    @check_bit_operation kron(b1, b2) BitVector

    b1 = bitrand(s1, s2)
    b2 = bitrand(s3, s4)
    @check_bit_operation kron(b1, b2) BitMatrix

    b1 = bitrand(v1)
    @check_bit_operation diff(b1) Vector{Int}

    b1 = bitrand(n1, n2)
    @check_bit_operation diff(b1) Matrix{Int}

    b1 = bitrand(n1, n1)
    @check_bit_operation svd(b1)
    @check_bit_operation qr(b1)

    b1 = bitrand(v1)
    @check_bit_operation gradient(b1)
    @check_bit_operation gradient(b1, 1.0)

    b1 = bitrand(v1)
    @check_bit_operation diagm(b1) BitMatrix

    b1 = bitrand(n1, n2)
    @test_throws DimensionMismatch diagm(b1)

    b1 = bitrand(n1, n1)
    @check_bit_operation diag(b1)
end

timesofar("linalg")

@testset "findmax, findmin" begin
    b1 = trues(0)
    @test_throws ArgumentError findmax(b1)
    @test_throws ArgumentError findmin(b1)

    for b1 in [falses(v1), trues(v1),
               BitArray([1,0,1,1,0]),
               BitArray([0,0,1,1,0]),
               bitrand(v1)]
        @check_bit_operation findmin(b1)
        @check_bit_operation findmax(b1)
    end
end

@testset "I/O" begin
    b1 = bitrand(v1)
    fname = ""
    b1[v1 ÷ 2 + 1] = true
    b1[end] = true
    try
        fname = tempname()
        open(fname, "w") do f
            write(f, b1)
        end
        b2 = falses(v1)
        read!(fname, b2)
        @test bitcheck(b2)
        @test b1 == b2
        b2 = falses(v1 ÷ 10, 10)
        read!(fname, b2)
        @test bitcheck(b2)
        @test reshape(b1, v1 ÷ 10, 10) == b2
        b2 = falses(v1 + 65)
        @test bitcheck(b2)
        @test_throws EOFError read!(fname, b2)
        @test bitcheck(b2)
        b2 = falses(v1 ÷ 2)
        @test_throws DimensionMismatch read!(fname, b2)
        @test bitcheck(b2)
        b2 = falses(v1 - 1)
        @test_throws DimensionMismatch read!(fname, b2)
        @test bitcheck(b2)

        b1 = BitArray(0)
        open(fname, "w") do f
            write(f, b1)
        end
        b2 = BitArray(0)
        read!(fname, b2)
        @test b1 == b2
        @test bitcheck(b2)
        b2 = trues(1)
        @test_throws EOFError read!(fname, b2)
        @test bitcheck(b2)
    finally
         isfile(fname) && rm(fname)
    end
end

timesofar("I/O")
