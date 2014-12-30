replstr(x) = sprint((io,x) -> writemime(io,MIME("text/plain"),x), x)

@test replstr(cell(2)) == "2-element Array{Any,1}:\n #undef\n #undef"
@test replstr(cell(2,2)) == "2x2 Array{Any,2}:\n #undef  #undef\n #undef  #undef"
@test replstr(cell(2,2,2)) == "2x2x2 Array{Any,3}:\n[:, :, 1] =\n #undef  #undef\n #undef  #undef\n\n[:, :, 2] =\n #undef  #undef\n #undef  #undef"

immutable T5589
    names::Vector{UTF8String}
end
@test replstr(T5589(Array(UTF8String,100))) == "T5589(UTF8String[#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef  …  #undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef,#undef])"

@test replstr(parse("type X end")) == ":(type X\n    end)"
@test replstr(parse("immutable X end")) == ":(immutable X\n    end)"
s = "ccall(:f,Int,(Ptr{Void},),&x)"
@test replstr(parse(s)) == ":($s)"

# expression printing

macro test_repr(x)
    quote
        # Note: We can't just compare x1 and x2 because interpolated
        # strings get converted to string Exprs by the first show().
        # This could produce a few false positives, but until string
        # interpolation works we don't really have a choice.
        local x1 = parse($x)
        local x2 = eval(parse(repr(x1)))
        local x3 = eval(parse(repr(x2)))
        x3 == x1 ? nothing : error(string(
            "repr test failed:",
            "\noriginal: ", $x,
            "\n\nparsed: ", x2, "\n", sprint(dump, x2),
            "\n\nreparsed: ", x3, "\n", sprint(dump, x3)
        ))
    end
end

# primitive types
@test_repr "x"
@test_repr "123"
@test_repr "\"123\""
@test_repr ":()"
@test_repr ":(x, y)"

# basic expressions
@test_repr "x + y"
@test_repr "2e"
@test_repr "!x"
@test_repr "f(1,2,3)"
@test_repr "x = ~y"
@test_repr ":(:x, :y)"
@test_repr ":(:(:(x)))"

# order of operations
@test_repr "x + y * z"
@test_repr "x * y + z"
@test_repr "x * (y + z)"
@test_repr "!x^y"
@test_repr "!x^(y+z)"
@test_repr "!(x^y+z)"
@test_repr "x^-y"
@test_repr "x^-(y+z)"
@test_repr "x^-f(y+z)"
@test_repr "+(w-x)^-f(y+z)"
#@test_repr "w = (x = y) = z" # Doesn't pass, but it's an invalid assignment loc
@test_repr "a & b && c"
@test_repr "a & (b && c)"

# precedence tie resolution
@test_repr "(a * b) * (c * d)"
@test_repr "(a / b) / (c / d / e)"
@test_repr "(a == b == c) != (c == d < e)"

# control structures (shamelessly stolen from base/bitarray.jl)
@test_repr """type BitArray{N} <: AbstractArray{Bool, N}
    chunks::Vector{UInt64}
    len::Int
    dims::NTuple{N,Int}
    function BitArray(dims::Int...)
        if length(dims) != N
            error(\"number of dimensions must be \$N (got \$(length(dims)))\")
        end
        n = 1
        for d in dims
            if d < 0
                error(\"dimension size must be nonnegative (got \$d)\")
            end
            n *= d
        end
        nc = num_bit_chunks(n)
        chunks = Array(UInt64, nc)
        if nc > 0
            chunks[end] = uint64(0)
        end
        b = new(chunks, n)
        if N != 1
            b.dims = dims
        end
        return b
    end
end"""

@test_repr """function copy_chunks(dest::Vector{UInt64}, pos_d::Integer, src::Vector{UInt64}, pos_s::Integer, numbits::Integer)
    if numbits == 0
        return
    end
    if dest === src && pos_d > pos_s
        return copy_chunks_rtol(dest, pos_d, pos_s, numbits)
    end
    kd0, ld0 = get_chunks_id(pos_d)
    kd1, ld1 = get_chunks_id(pos_d + numbits - 1)
    ks0, ls0 = get_chunks_id(pos_s)
    ks1, ls1 = get_chunks_id(pos_s + numbits - 1)
    delta_kd = kd1 - kd0
    delta_ks = ks1 - ks0
    u = _msk64
    if delta_kd == 0
        msk_d0 = ~(u << ld0) | (u << ld1 << 1)
    else
        msk_d0 = ~(u << ld0)
        msk_d1 = (u << ld1 << 1)
    end
    if delta_ks == 0
        msk_s0 = (u << ls0) & ~(u << ls1 << 1)
    else
        msk_s0 = (u << ls0)
    end
    chunk_s0 = glue_src_bitchunks(src, ks0, ks1, msk_s0, ls0)
    dest[kd0] = (dest[kd0] & msk_d0) | ((chunk_s0 << ld0) & ~msk_d0)
    if delta_kd == 0
        return
    end
    for i = 1 : kd1 - kd0 - 1
        chunk_s1 = glue_src_bitchunks(src, ks0 + i, ks1, msk_s0, ls0)
        chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1) | (chunk_s1 << ld0)
        dest[kd0 + i] = chunk_s
        chunk_s0 = chunk_s1
    end
    if ks1 >= ks0 + delta_kd
        chunk_s1 = glue_src_bitchunks(src, ks0 + delta_kd, ks1, msk_s0, ls0)
    else
        chunk_s1 = uint64(0)
    end
    chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1) | (chunk_s1 << ld0)
    dest[kd1] = (dest[kd1] & msk_d1) | (chunk_s & ~msk_d1)
    return
end"""

# issue #7188
@test sprint(show, :foo) == ":foo"
@test sprint(show, symbol("foo bar")) == "symbol(\"foo bar\")"
@test sprint(show, symbol("foo \"bar")) == "symbol(\"foo \\\"bar\")"
@test sprint(show, :+) == ":+"
@test sprint(show, :end) == ":end"

# Function and array reference precedence
@test_repr "([2] + 3)[1]"
@test_repr "foo.bar[1]"
@test_repr "foo.bar()"
@test_repr "(foo + bar)()"

# issue #7921
@test replace(sprint(show, Expr(:function, :(==(a, b)), Expr(:block,:(return a == b)))), r"\s+", " ") == ":(function ==(a,b) return a == b end)"

# unicode operator printing
@test sprint(show, :(1 ⊕ (2 ⊗ 3))) == ":(1 ⊕ 2 ⊗ 3)"
@test sprint(show, :((1 ⊕ 2) ⊗ 3)) == ":((1 ⊕ 2) ⊗ 3)"

# issue #8155
@test_repr "foo(x,y; z=bar)"
@test_repr "foo(x,y,z=bar)"

@test_repr "Int[i for i=1:10]"
@test_repr "Int[(i, j) for (i, j) in zip(1:10,1:0)]"

@test_repr "[1 2 3; 4 5 6; 7 8 9]'"

@test_repr "baremodule X
importall ..A.b
import ...B.c
import D
import B.C.D.E.F.g
end"
@test_repr "baremodule Y
export A, B, C
export D, E, F
end"

# issue #8994
@test_repr "get! => 2"
@test_repr "< : 2"
@test_repr "< :: T"
@test_repr "S{< <: T}"
@test_repr "+ + +"

# issue #9474
for s in ("(1::Int64 == 1::Int64)::Bool", "(1:2:3) + 4", "x = 1:2:3")
    @test sprint(show, parse(s)) == ":("*s*")"
end
