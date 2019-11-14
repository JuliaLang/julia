# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct GenericIterator{N} end
Base.iterate(::GenericIterator{N}, i=1) where {N} = i > N ? nothing : (i, i + 1)
Base.IteratorSize(::Type{GenericIterator{N}}) where {N} = Base.SizeUnknown()

function generic_map_tests(mapf, inplace_mapf=nothing)
    for typ in (Float16, Float32, Float64,
                Int8, Int16, Int32, Int64, Int128,
                UInt8, UInt16, UInt32, UInt64, UInt128),
        arg_typ in (Integer,
                    Signed,
                    Unsigned)
        X = typ[1:10...]
        _typ = typeof(arg_typ(one(typ)))
        @test mapf(arg_typ, X) == _typ[1:10...]
    end

    # generic map
    f(x) = x + 1
    I = GenericIterator{10}()
    @test mapf(f, I) == Any[2:11...]

    # AbstractArray map for 2 arg case
    f(x, y) = x + y
    B = Float64[1:10...]
    C = Float64[1:10...]
    @test mapf(f, convert(Vector{Int},B), C) == Float64[ 2 * i for i in 1:10 ]
    @test mapf(f, Int[], Float64[]) == Union{}[]
    # map with different result types
    let m = mapf(x->x+1, Number[1, 2.0])
        @test isa(m, Vector{Real})
        @test m == Real[2, 3.0]
    end

    # AbstractArray map for N-arg case
    A = Array{Int}(undef, 10)
    f(x, y, z) = x + y + z
    D = Float64[1:10...]

    @test map!(f, A, B, C, D) == Int[ 3 * i for i in 1:10 ]
    @test mapf(f, B, C, D) == Float64[ 3 * i for i in 1:10 ]
    @test mapf(f, Int[], Int[], Complex{Int}[]) == Union{}[]

    # In-place map
    if inplace_mapf != nothing
        A = Float64[1:10...]
        inplace_mapf(x -> x*x, A, A)
        @test A == map(x -> x*x, Float64[1:10...])

        # Map to destination collection
        B = inplace_mapf((x,y,z)->x*y*z, A, Float64[1:10...], Float64[1:10...], Float64[1:10...])
        @test A == map(x->x*x*x, Float64[1:10...])
        @test A === B
    end
end

function testmap_equivalence(mapf, f, c...)
    x1 = mapf(f,c...)
    x2 = map(f,c...)

    if Base.IteratorSize isa Base.HasShape
        @test size(x1) == size(x2)
    else
        @test length(x1) == length(x2)
    end

    @test eltype(x1) == eltype(x2)

    for (v1,v2) in zip(x1,x2)
        @test v1==v2
    end
end

function run_map_equivalence_tests(mapf)
    testmap_equivalence(mapf, identity, (1,2,3,4))
    testmap_equivalence(mapf, (x,y,z)->x+y+z, 1,2,3)
    testmap_equivalence(mapf, x->x ? false : true, BitMatrix(undef, 10,10))
    testmap_equivalence(mapf, x->"foobar", BitMatrix(undef, 10,10))
    testmap_equivalence(mapf, (x,y,z)->string(x,y,z), BitVector(undef, 10), fill(1.0, 10), "1234567890")
end
