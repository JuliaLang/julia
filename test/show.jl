# This file is a part of Julia. License is MIT: http://julialang.org/license

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

# recursive array printing
# issue #10353
let
    a = Any[]
    push!(a,a)
    show(IOBuffer(), a)
    push!(a,a)
    show(IOBuffer(), a)
end

# expression printing

macro test_repr(x)
    quote
        # Note: We can't just compare x1 and x2 because interpolated
        # strings get converted to string Exprs by the first show().
        # This could produce a few false positives, but until string
        # interpolation works we don't really have a choice.
        let
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
@test_repr "(a => b) in c"
@test_repr "a => b in c"

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
            chunks[end] = UInt64(0)
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
        chunk_s1 = UInt64(0)
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

# issue #12477
@test sprint(show, Union{Int64,Int32,Int16,Int8,Float64}) == "Union{Float64,Int16,Int32,Int64,Int8}"

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
@test_repr "(<) : 2"
@test_repr "(<) :: T"
@test_repr "S{(<) <: T}"
@test_repr "+ + +"

# issue #9474
for s in ("(1::Int64 == 1::Int64)::Bool", "(1:2:3) + 4", "x = 1:2:3")
    @test sprint(show, parse(s)) == ":("*s*")"
end

# parametric type instantiation printing
immutable TParametricPrint{a}; end
@test sprint(show, :(TParametricPrint{false}())) == ":(TParametricPrint{false}())"

# issue #9797
let q1 = parse(repr(:("$(a)b"))),
    q2 = parse(repr(:("$ab")))
    @test isa(q1, Expr)
    @test q1.args[1].head === :string
    @test q1.args[1].args == [:a, "b"]

    @test isa(q2, Expr)
    @test q2.args[1].head == :string
    @test q2.args[1].args == [:ab,]
end

x8d003 = 2
let a = Expr(:quote,Expr(:$,:x8d003))
    @test eval(parse(repr(a))) == a
    @test eval(eval(parse(repr(a)))) == 2
end

# issue #9865
@test ismatch(r"^Set\(\[.+….+\]\)$", replstr(Set(1:100)))

# issue #11413
@test string(:(*{1,2})) == "*{1,2}"
@test string(:(*{1,x})) == "*{1,x}"
@test string(:(-{x}))   == "-{x}"

# issue #11393
@test_repr "@m(x,y) + z"
@test_repr "(@m(x,y),z)"
@test_repr "[@m(x,y),z]"
@test_repr "A[@m(x,y),z]"
@test_repr "T{@m(x,y),z}"
@test_repr "@m x @n(y) z"
@test_repr "f(@m(x,y);z=@n(a))"
@test_repr "@m(x,y).z"
@test_repr "::@m(x,y)+z"
@test_repr "[@m(x) y z]"
@test_repr "[@m(x) y; z]"
@test_repr "let @m(x), y=z; end"

@test repr(:(@m x y))    == ":(@m x y)"
@test string(:(@m x y))  ==   "@m x y"
@test string(:(@m x y;)) == "begin \n    @m x y\nend"

# issue #11436
@test_repr "1 => 2 => 3"
@test_repr "1 => (2 => 3)"
@test_repr "(1 => 2) => 3"

# pr 12008
@test_repr "bitstype A B"
@test_repr "bitstype 100 B"
@test repr(:(bitstype A B)) == ":(bitstype A B)"
@test repr(:(bitstype 100 B)) == ":(bitstype 100 B)"

oldout = STDOUT
try
    rd, wr = redirect_stdout()
    @test dump(STDERR) == nothing
    @test xdump(STDERR) == nothing
finally
    redirect_stdout(oldout)
end

# issue #12960
type T12960 end
let
    A = speye(3)
    B = similar(A, T12960)
    @test sprint(show, B)  == "\n\t[1, 1]  =  #undef\n\t[2, 2]  =  #undef\n\t[3, 3]  =  #undef"
    @test sprint(print, B) == "\n\t[1, 1]  =  #undef\n\t[2, 2]  =  #undef\n\t[3, 3]  =  #undef"
    B[1,2] = T12960()
    @test sprint(show, B)  == "\n\t[1, 1]  =  #undef\n\t[1, 2]  =  T12960()\n\t[2, 2]  =  #undef\n\t[3, 3]  =  #undef"
    @test sprint(print, B) == "\n\t[1, 1]  =  #undef\n\t[1, 2]  =  T12960()\n\t[2, 2]  =  #undef\n\t[3, 3]  =  #undef"
end

# issue #13127
function f13127()
    buf = IOBuffer()
    f() = 1
    show(buf, f)
    takebuf_string(buf)
end
@test f13127() == "f"

let a = Pair(1.0,2.0)
    @test sprint(show,a) == "1.0=>2.0"
end
let a = Pair(Pair(1,2),Pair(3,4))
    @test sprint(show,a) == "(1=>2)=>(3=>4)"
end

#test methodshow.jl functions
@test Base.inbase(Base)
@test Base.inbase(LinAlg)

@test contains(sprint(io -> writemime(io,"text/plain",methods(Base.inbase))),"inbase(m::Module)")
@test contains(sprint(io -> writemime(io,"text/html",methods(Base.inbase))),"inbase(m::<b>Module</b>)")

if isempty(Base.GIT_VERSION_INFO.commit)
    @test contains(Base.url(methods(eigs).defs),"https://github.com/JuliaLang/julia/tree/v$VERSION/base/linalg/arnoldi.jl#L")
else
    @test contains(Base.url(methods(eigs).defs),"https://github.com/JuliaLang/julia/tree/$(Base.GIT_VERSION_INFO.commit)/base/linalg/arnoldi.jl#L")
end

# print_matrix should be able to handle small and large objects easily, test by
# calling writemime. This also indirectly tests print_matrix_row, which
# is used repeatedly by print_matrix.
# This fits on screen:
@test replstr(eye(10)) == "10x10 Array{Float64,2}:\n 1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0"
# an array too long vertically to fit on screen, and too long horizontally:
@test replstr(collect(1.:100.)) == "100-element Array{Float64,1}:\n   1.0\n   2.0\n   3.0\n   4.0\n   5.0\n   6.0\n   7.0\n   8.0\n   9.0\n  10.0\n   ⋮  \n  92.0\n  93.0\n  94.0\n  95.0\n  96.0\n  97.0\n  98.0\n  99.0\n 100.0"
@test replstr(collect(1.:100.)') == "1x100 Array{Float64,2}:\n 1.0  2.0  3.0  4.0  5.0  6.0  7.0  …  95.0  96.0  97.0  98.0  99.0  100.0"
# too big in both directions to fit on screen:
@test replstr((1.:100.)*(1:100)') == "100x100 Array{Float64,2}:\n   1.0    2.0    3.0    4.0    5.0    6.0  …    97.0    98.0    99.0    100.0\n   2.0    4.0    6.0    8.0   10.0   12.0      194.0   196.0   198.0    200.0\n   3.0    6.0    9.0   12.0   15.0   18.0      291.0   294.0   297.0    300.0\n   4.0    8.0   12.0   16.0   20.0   24.0      388.0   392.0   396.0    400.0\n   5.0   10.0   15.0   20.0   25.0   30.0      485.0   490.0   495.0    500.0\n   6.0   12.0   18.0   24.0   30.0   36.0  …   582.0   588.0   594.0    600.0\n   7.0   14.0   21.0   28.0   35.0   42.0      679.0   686.0   693.0    700.0\n   8.0   16.0   24.0   32.0   40.0   48.0      776.0   784.0   792.0    800.0\n   9.0   18.0   27.0   36.0   45.0   54.0      873.0   882.0   891.0    900.0\n  10.0   20.0   30.0   40.0   50.0   60.0      970.0   980.0   990.0   1000.0\n   ⋮                                  ⋮    ⋱                                 \n  92.0  184.0  276.0  368.0  460.0  552.0     8924.0  9016.0  9108.0   9200.0\n  93.0  186.0  279.0  372.0  465.0  558.0     9021.0  9114.0  9207.0   9300.0\n  94.0  188.0  282.0  376.0  470.0  564.0     9118.0  9212.0  9306.0   9400.0\n  95.0  190.0  285.0  380.0  475.0  570.0     9215.0  9310.0  9405.0   9500.0\n  96.0  192.0  288.0  384.0  480.0  576.0  …  9312.0  9408.0  9504.0   9600.0\n  97.0  194.0  291.0  388.0  485.0  582.0     9409.0  9506.0  9603.0   9700.0\n  98.0  196.0  294.0  392.0  490.0  588.0     9506.0  9604.0  9702.0   9800.0\n  99.0  198.0  297.0  396.0  495.0  594.0     9603.0  9702.0  9801.0   9900.0\n 100.0  200.0  300.0  400.0  500.0  600.0     9700.0  9800.0  9900.0  10000.0"

# Issue 14121
@test_repr "(A'x)'"
