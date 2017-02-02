# This file is a part of Julia. License is MIT: http://julialang.org/license

const curmod = current_module()
const curmod_name = fullname(curmod)
const curmod_prefix = "$(["$m." for m in curmod_name]...)"

replstr(x) = sprint((io,x) -> show(IOContext(io, :limit => true), MIME("text/plain"), x), x)

@test replstr(Array{Any}(2)) == "2-element Array{Any,1}:\n #undef\n #undef"
@test replstr(Array{Any}(2,2)) == "2×2 Array{Any,2}:\n #undef  #undef\n #undef  #undef"
@test replstr(Array{Any}(2,2,2)) == "2×2×2 Array{Any,3}:\n[:, :, 1] =\n #undef  #undef\n #undef  #undef\n\n[:, :, 2] =\n #undef  #undef\n #undef  #undef"
@test replstr([1f10]) == "1-element Array{Float32,1}:\n 1.0f10"

immutable T5589
    names::Vector{String}
end
@test replstr(T5589(Array{String,1}(100))) == "$(curmod_prefix)T5589(String[#undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef  …  #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef])"

@test replstr(parse("type X end")) == ":(type X # none, line 1:\n    end)"
@test replstr(parse("immutable X end")) == ":(immutable X # none, line 1:\n    end)"
s = "ccall(:f, Int, (Ptr{Void},), &x)"
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
@test_repr "f(1, 2, 3)"
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
@test_repr "*(a..., b)"
@test_repr "+(a, b, c...)"

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
        chunks = Array{UInt64,1}(nc)
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
@test sprint(show, Symbol("foo bar")) == "Symbol(\"foo bar\")"
@test sprint(show, Symbol("foo \"bar")) == "Symbol(\"foo \\\"bar\")"
@test sprint(show, :+) == ":+"
@test sprint(show, :end) == ":end"

# issue #12477
@test sprint(show,  Union{Int64, Int32, Int16, Int8, Float64}) == "Union{Float64, Int16, Int32, Int64, Int8}"

# Function and array reference precedence
@test_repr "([2] + 3)[1]"
@test_repr "foo.bar[1]"
@test_repr "foo.bar()"
@test_repr "(foo + bar)()"

# issue #7921
@test replace(sprint(show, Expr(:function, :(==(a, b)), Expr(:block,:(return a == b)))),
              r"\s+", " ") == ":(function ==(a, b) return a == b end)"

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

# issue #19840
@test_repr "Array{Int}(0)"
@test_repr "Array{Int}(0,0)"
@test_repr "Array{Int}(0,0,0)"
@test_repr "Array{Int}(0,1)"
@test_repr "Array{Int}(0,0,1)"

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
@test string(:(*{1, 2})) == "*{1, 2}"
@test string(:(*{1, x})) == "*{1, x}"
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

# `where` syntax
@test_repr "A where T<:B"
@test_repr "A where T<:(Array{T} where T<:Real)"
@test_repr "Array{T} where T<:Array{S} where S<:Real"
@test_repr "x::Array{T} where T"
@test_repr "(a::b) where T"
@test_repr "a::b where T"
@test_repr "X where (T=1)"
@test_repr "X where T = 1"

let oldout = STDOUT, olderr = STDERR
    local rdout, wrout, rderr, wrerr, out, err, rd, wr
    try
        # pr 16917
        rdout, wrout = redirect_stdout()
        @test wrout === STDOUT
        out = @async readstring(rdout)
        rderr, wrerr = redirect_stderr()
        @test wrerr === STDERR
        err = @async readstring(rderr)
        @test dump(Int64) === nothing
        if !is_windows()
            close(wrout)
            close(wrerr)
        end

        for io in (Core.STDOUT, Core.STDERR)
            Core.println(io, "TESTA")
            println(io, "TESTB")
            print(io, 'Α', 1)
            Core.print(io, 'Β', 2)
            Core.show(io, "A")
            println(io)
        end
        Core.println("A")
        Core.print("1", 2, 3.0)
        Core.show("C")
        Core.println()
        redirect_stdout(oldout)
        redirect_stderr(olderr)
        close(wrout)
        close(wrerr)
        @test wait(out) == "Int64 <: Signed\nTESTA\nTESTB\nΑ1Β2\"A\"\nA\n123\"C\"\n"
        @test wait(err) == "TESTA\nTESTB\nΑ1Β2\"A\"\n"
    finally
        redirect_stdout(oldout)
        redirect_stderr(olderr)
    end
end

let filename = tempname()
    ret = open(filename, "w") do f
        redirect_stdout(f) do
            println("hello")
            [1,3]
        end
    end
    @test ret == [1,3]
    @test chomp(readstring(filename)) == "hello"
    ret = open(filename, "w") do f
        redirect_stderr(f) do
            warn("hello")
            [2]
        end
    end
    @test ret == [2]
    @test contains(readstring(filename), "WARNING: hello")
    ret = open(filename) do f
        redirect_stdin(f) do
            readline()
        end
    end
    @test contains(ret, "WARNING: hello")
    rm(filename)
end

# issue #12960
type T12960 end
let
    A = speye(3)
    B = similar(A, T12960)
    @test sprint(show, B)  == "\n  [1, 1]  =  #undef\n  [2, 2]  =  #undef\n  [3, 3]  =  #undef"
    @test sprint(print, B) == "\n  [1, 1]  =  #undef\n  [2, 2]  =  #undef\n  [3, 3]  =  #undef"
    B[1,2] = T12960()
    @test sprint(show, B)  == "\n  [1, 1]  =  #undef\n  [1, 2]  =  $(curmod_prefix)T12960()\n  [2, 2]  =  #undef\n  [3, 3]  =  #undef"
    @test sprint(print, B) == "\n  [1, 1]  =  #undef\n  [1, 2]  =  $(curmod_prefix)T12960()\n  [2, 2]  =  #undef\n  [3, 3]  =  #undef"
end

# issue #13127
function f13127()
    buf = IOBuffer()
    f() = 1
    show(buf, f)
    String(take!(buf))
end
@test f13127() == "$(curmod_prefix)f"

let a = Pair(1.0,2.0)
    @test sprint(show,a) == "1.0=>2.0"
end
let a = Pair(Pair(1,2),Pair(3,4))
    @test sprint(show,a) == "(1=>2)=>(3=>4)"
end

#test methodshow.jl functions
@test Base.inbase(Base)
@test Base.inbase(LinAlg)
@test !Base.inbase(Core)

let repr = sprint(io -> show(io,"text/plain", methods(Base.inbase)))
    @test contains(repr, "inbase(m::Module)")
end
let repr = sprint(io -> show(io,"text/html", methods(Base.inbase)))
    @test contains(repr, "inbase(m::<b>Module</b>)")
end

f5971(x, y...; z=1, w...) = nothing
let repr = sprint(io -> show(io,"text/plain", methods(f5971)))
    @test contains(repr, "f5971(x, y...; z, w...)")
end
let repr = sprint(io -> show(io,"text/html", methods(f5971)))
    @test contains(repr, "f5971(x, y...; <i>z, w...</i>)")
end
f16580(x, y...; z=1, w=y+x, q...) = nothing
let repr = sprint(io -> show(io,"text/html", methods(f16580)))
    @test contains(repr, "f16580(x, y...; <i>z, w, q...</i>)")
end

if isempty(Base.GIT_VERSION_INFO.commit)
    @test contains(Base.url(first(methods(eigs))),"https://github.com/JuliaLang/julia/tree/v$VERSION/base/linalg/arnoldi.jl#L")
else
    @test contains(Base.url(first(methods(eigs))),"https://github.com/JuliaLang/julia/tree/$(Base.GIT_VERSION_INFO.commit)/base/linalg/arnoldi.jl#L")
end

# print_matrix should be able to handle small and large objects easily, test by
# calling show. This also indirectly tests print_matrix_row, which
# is used repeatedly by print_matrix.
# This fits on screen:
@test replstr(eye(10)) == "10×10 Array{Float64,2}:\n 1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0"
# an array too long vertically to fit on screen, and too long horizontally:
@test replstr(collect(1.:100.)) == "100-element Array{Float64,1}:\n   1.0\n   2.0\n   3.0\n   4.0\n   5.0\n   6.0\n   7.0\n   8.0\n   9.0\n  10.0\n   ⋮  \n  92.0\n  93.0\n  94.0\n  95.0\n  96.0\n  97.0\n  98.0\n  99.0\n 100.0"
@test replstr(collect(1.:100.)') == "1×100 RowVector{Float64,Array{Float64,1}}:\n 1.0  2.0  3.0  4.0  5.0  6.0  7.0  …  95.0  96.0  97.0  98.0  99.0  100.0"
# too big in both directions to fit on screen:
@test replstr((1.:100.)*(1:100)') == "100×100 Array{Float64,2}:\n   1.0    2.0    3.0    4.0    5.0    6.0  …    97.0    98.0    99.0    100.0\n   2.0    4.0    6.0    8.0   10.0   12.0      194.0   196.0   198.0    200.0\n   3.0    6.0    9.0   12.0   15.0   18.0      291.0   294.0   297.0    300.0\n   4.0    8.0   12.0   16.0   20.0   24.0      388.0   392.0   396.0    400.0\n   5.0   10.0   15.0   20.0   25.0   30.0      485.0   490.0   495.0    500.0\n   6.0   12.0   18.0   24.0   30.0   36.0  …   582.0   588.0   594.0    600.0\n   7.0   14.0   21.0   28.0   35.0   42.0      679.0   686.0   693.0    700.0\n   8.0   16.0   24.0   32.0   40.0   48.0      776.0   784.0   792.0    800.0\n   9.0   18.0   27.0   36.0   45.0   54.0      873.0   882.0   891.0    900.0\n  10.0   20.0   30.0   40.0   50.0   60.0      970.0   980.0   990.0   1000.0\n   ⋮                                  ⋮    ⋱                                 \n  92.0  184.0  276.0  368.0  460.0  552.0     8924.0  9016.0  9108.0   9200.0\n  93.0  186.0  279.0  372.0  465.0  558.0     9021.0  9114.0  9207.0   9300.0\n  94.0  188.0  282.0  376.0  470.0  564.0     9118.0  9212.0  9306.0   9400.0\n  95.0  190.0  285.0  380.0  475.0  570.0     9215.0  9310.0  9405.0   9500.0\n  96.0  192.0  288.0  384.0  480.0  576.0  …  9312.0  9408.0  9504.0   9600.0\n  97.0  194.0  291.0  388.0  485.0  582.0     9409.0  9506.0  9603.0   9700.0\n  98.0  196.0  294.0  392.0  490.0  588.0     9506.0  9604.0  9702.0   9800.0\n  99.0  198.0  297.0  396.0  495.0  594.0     9603.0  9702.0  9801.0   9900.0\n 100.0  200.0  300.0  400.0  500.0  600.0     9700.0  9800.0  9900.0  10000.0"

# Issue 14121
@test_repr "(A'x)'"


# issue #14481
@test_repr "in(1,2,3)"
@test_repr "<(1,2,3)"
@test_repr "+(1,2,3)"
@test_repr "-(1,2,3)"
@test_repr "*(1,2,3)"


# issue #15309
l1, l2, l2n = Expr(:line,42), Expr(:line,42,:myfile), LineNumberNode(42)
@test string(l2n) == " # line 42:"
@test string(l2)  == " # myfile, line 42:"
@test string(l1)  == string(l2n)
ex = Expr(:block, l1, :x, l2, :y, l2n, :z)
@test replace(string(ex)," ","") == replace("""
begin  # line 42:
    x # myfile, line 42:
    y # line 42:
    z
end""", " ", "")
# Test the printing of whatever form of line number representation
# that is used in the arguments to a macro looks the same as for
# regular quoting
macro strquote(ex)
    QuoteNode(string(ex))
end
str_ex2a, str_ex2b = @strquote(begin x end), string(quote x end)
@test str_ex2a == str_ex2b


# test structured zero matrix printing for select structured types
A = reshape(1:16,4,4)
@test replstr(Diagonal(A)) == "4×4 Diagonal{$Int}:\n 1  ⋅   ⋅   ⋅\n ⋅  6   ⋅   ⋅\n ⋅  ⋅  11   ⋅\n ⋅  ⋅   ⋅  16"
@test replstr(Bidiagonal(A,true)) == "4×4 Bidiagonal{$Int}:\n 1  5   ⋅   ⋅\n ⋅  6  10   ⋅\n ⋅  ⋅  11  15\n ⋅  ⋅   ⋅  16"
@test replstr(Bidiagonal(A,false)) == "4×4 Bidiagonal{$Int}:\n 1  ⋅   ⋅   ⋅\n 2  6   ⋅   ⋅\n ⋅  7  11   ⋅\n ⋅  ⋅  12  16"
@test replstr(SymTridiagonal(A+A')) == "4×4 SymTridiagonal{$Int}:\n 2   7   ⋅   ⋅\n 7  12  17   ⋅\n ⋅  17  22  27\n ⋅   ⋅  27  32"
@test replstr(Tridiagonal(diag(A,-1),diag(A),diag(A,+1))) == "4×4 Tridiagonal{$Int}:\n 1  5   ⋅   ⋅\n 2  6  10   ⋅\n ⋅  7  11  15\n ⋅  ⋅  12  16"
@test replstr(UpperTriangular(copy(A))) == "4×4 UpperTriangular{$Int,Array{$Int,2}}:\n 1  5   9  13\n ⋅  6  10  14\n ⋅  ⋅  11  15\n ⋅  ⋅   ⋅  16"
@test replstr(LowerTriangular(copy(A))) == "4×4 LowerTriangular{$Int,Array{$Int,2}}:\n 1  ⋅   ⋅   ⋅\n 2  6   ⋅   ⋅\n 3  7  11   ⋅\n 4  8  12  16"

# Vararg methods in method tables
function test_mt(f, str)
    mt = methods(f)
    @test length(mt) == 1
    defs = first(mt)
    io = IOBuffer()
    show(io, defs)
    strio = String(take!(io))
    strio = split(strio, " at")[1]
    @test strio[1:length(str)] == str
end
show_f1(x...) = [x...]
show_f2(x::Vararg{Any}) = [x...]
show_f3(x::Vararg) = [x...]
show_f4(x::Vararg{Any,3}) = [x...]
show_f5{T, N}(A::AbstractArray{T,N}, indexes::Vararg{Int,N}) = [indexes...]
test_mt(show_f1, "show_f1(x...)")
test_mt(show_f2, "show_f2(x...)")
test_mt(show_f3, "show_f3(x...)")
test_mt(show_f4, "show_f4(x::Vararg{Any,3})")
test_mt(show_f5, "show_f5{T, N}(A::AbstractArray{T,N}, indexes::Vararg{$Int,N})")

# Issue #15525, printing of vcat
@test sprint(show, :([a;])) == ":([a;])"
@test sprint(show, :([a; b])) == ":([a; b])"
@test_repr "[a;]"
@test_repr "[a; b]"

# Printing of :(function f end)
@test sprint(show, :(function f end)) == ":(function f end)"
@test_repr "function g end"

# Issue #15765 printing of continue and break
@test sprint(show, :(continue)) == ":(continue)"
@test sprint(show, :(break)) == ":(break)"
@test_repr "continue"
@test_repr "break"

let x = [], y = []
    push!(x, y)
    push!(y, x)
    @test replstr(x) == "1-element Array{Any,1}:\n Any[Any[Any[#= circular reference @-2 =#]]]"
end

# PR 16221
# Printing of upper and lower bound of a TypeVar
@test string(TypeVar(:V, Signed, Real)) == "Signed<:V<:Real"
# Printing of primary type in type parameter place should not show the type
# parameter names.
@test string(Array) == "Array"
@test string(Tuple{Array}) == "Tuple{Array}"

# PR #16651
@test !contains(repr(ones(10,10)), "\u2026")
@test contains(sprint((io, x) -> show(IOContext(io, :limit => true), x), ones(30, 30)), "\u2026")

# showcompact() also sets :multiline=>false (#16817)
let io = IOBuffer()
    x = [1, 2]
    showcompact(io, x)
    @test String(take!(io)) == "[1, 2]"
    showcompact(IOContext(io, :compact => true), x)
    @test String(take!(io)) == "[1, 2]"
end

# PR 17117
# test show array
let s = IOBuffer(Array{UInt8}(0), true, true)
    Base.showarray(s, [1, 2, 3], false, header = false)
    @test String(resize!(s.data, s.size)) == " 1\n 2\n 3"
end

let repr = sprint(dump, :(x = 1))
    @test repr == "Expr\n  head: Symbol =\n  args: Array{Any}((2,))\n    1: Symbol x\n    2: $Int 1\n  typ: Any\n"
end
let repr = sprint(dump, Pair{String,Int64})
    @test repr == "Pair{String,Int64} <: Any\n  first::String\n  second::Int64\n"
end
let repr = sprint(dump, Tuple)
    @test repr == "Tuple <: Any\n"
end
let repr = sprint(dump, Int64)
    @test repr == "Int64 <: Signed\n"
end
# Make sure a `TypeVar` in a `Union` doesn't break subtype dump.
typealias BreakDump17529{T} Union{T,Void}
let repr = sprint(dump, Any)
    @test length(repr) > 100000
    @test ismatch(r"^Any\n  [^ \t\n]", repr)
    @test endswith(repr, '\n')
    @test contains(repr, "     Base.Vector{T} = Array{T,1} where T\n")
    @test !contains(repr, "Core.Vector{T}")
end
let repr = sprint(dump, Integer)
    @test contains(repr, "UInt128")
    @test !contains(repr, "Any")
end
let repr = sprint(dump, Union{Integer, Float32})
    @test repr == "Union{Integer, Float32}\n" || repr == "Union{Float32, Integer}\n"
end
let repr = sprint(dump, Core.svec())
    @test repr == "empty SimpleVector\n"
end
let sv = Core.svec(:a, :b, :c)
    # unsafe replacement of :c with #undef to test handling of incomplete SimpleVectors
    unsafe_store!(convert(Ptr{Ptr{Void}}, Base.data_pointer_from_objref(sv)) + 3 * sizeof(Ptr), C_NULL)
    repr = sprint(dump, sv)
    @test repr == "SimpleVector\n  1: Symbol a\n  2: Symbol b\n  3: #undef\n"
end
let repr = sprint(dump, sin)
    @test repr == "sin (function of type Base.#sin)\n"
end
let repr = sprint(dump, Base.Test)
    @test repr == "Module Base.Test\n"
end
let a = Array{Any}(10000)
    a[2] = "elemA"
    a[4] = "elemB"
    a[11] = "elemC"
    repr = sprint(0, dump, a; env= (:limit => true))
    @test repr == "Array{Any}((10000,))\n  1: #undef\n  2: String \"elemA\"\n  3: #undef\n  4: String \"elemB\"\n  5: #undef\n  ...\n  9996: #undef\n  9997: #undef\n  9998: #undef\n  9999: #undef\n  10000: #undef\n"
end

# issue #17338
@test repr(Core.svec(1, 2)) == "svec(1, 2)"

# showing generator and comprehension expressions
@test repr(:(x for x in y for z in w)) == ":((x for x = y for z = w))"
@test repr(:(x for x in y if aa for z in w if bb)) == ":((x for x = y if aa for z = w if bb))"
@test repr(:([x for x = y])) == ":([x for x = y])"
@test repr(:([x for x = y if z])) == ":([x for x = y if z])"
@test repr(:(z for z = 1:5, y = 1:5)) == ":((z for z = 1:5, y = 1:5))"

for op in (:(.=), :(.+=), :(.&=))
    @test repr(parse("x $op y")) == ":(x $op y)"
end

# pretty-printing of compact broadcast expressions (#17289)
@test repr(:(f.(X, Y))) == ":(f.(X, Y))"
@test repr(:(f.(X))) == ":(f.(X))"
@test repr(:(f.())) == ":(f.())"

# Test compact printing of homogeneous tuples
@test repr(NTuple{7,Int64}) == "NTuple{7,Int64}"
@test repr(Tuple{Float64, Float64, Float64, Float64}) == "NTuple{4,Float64}"
@test repr(Tuple{Float32, Float32, Float32}) == "Tuple{Float32,Float32,Float32}"

# Test that REPL/mime display of invalid UTF-8 data doesn't throw an exception:
@test isa(stringmime("text/plain", String(UInt8[0x00:0xff;])), String)

# don't use julia-specific `f` in Float32 printing (PR #18053)
@test sprint(print, 1f-7) == "1.0e-7"

# test that the REPL TextDisplay works for displaying arbitrary textual MIME types
let d = TextDisplay(IOBuffer())
    display(d, "text/csv", [3 1 4])
    @test String(take!(d.io)) == "3,1,4\n"
    @test_throws MethodError display(d, "text/foobar", [3 1 4])
    try
        display(d, "text/foobar", [3 1 4])
    catch e
        @test e.f == show
    end
end
