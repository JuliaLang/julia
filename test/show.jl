# This file is a part of Julia. License is MIT: https://julialang.org/license

using LinearAlgebra, SparseArrays

# For curmod_*
include("testenv.jl")

replstr(x, kv::Pair...) = sprint((io,x) -> show(IOContext(io, :limit => true, :displaysize => (24, 80), kv...), MIME("text/plain"), x), x)
showstr(x, kv::Pair...) = sprint((io,x) -> show(IOContext(io, :limit => true, :displaysize => (24, 80), kv...), x), x)

@testset "IOContext" begin
    io = IOBuffer()
    ioc = IOContext(io)
    @test ioc.io == io
    @test ioc.dict == Base.ImmutableDict{Symbol, Any}()
    ioc = IOContext(io, :x => 1)
    @test ioc.io == io
    @test ioc.dict == Base.ImmutableDict{Symbol, Any}(:x, 1)
    ioc = IOContext(io, :x => 1, :y => 2)
    @test ioc.io == io
    @test ioc.dict == Base.ImmutableDict(Base.ImmutableDict{Symbol, Any}(:x, 1),
                                         :y => 2)
end

@test replstr(Array{Any}(undef, 2)) == "2-element Array{Any,1}:\n #undef\n #undef"
@test replstr(Array{Any}(undef, 2,2)) == "2×2 Array{Any,2}:\n #undef  #undef\n #undef  #undef"
@test replstr(Array{Any}(undef, 2,2,2)) == "2×2×2 Array{Any,3}:\n[:, :, 1] =\n #undef  #undef\n #undef  #undef\n\n[:, :, 2] =\n #undef  #undef\n #undef  #undef"
@test replstr([1f10]) == "1-element Array{Float32,1}:\n 1.0e10"

struct T5589
    names::Vector{String}
end
@test replstr(T5589(Vector{String}(undef, 100))) == "$(curmod_prefix)T5589([#undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef  …  #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef, #undef])"

@test replstr(Meta.parse("mutable struct X end")) == ":(mutable struct X\n      #= none:1 =#\n  end)"
@test replstr(Meta.parse("struct X end")) == ":(struct X\n      #= none:1 =#\n  end)"
let s = "ccall(:f, Int, (Ptr{Cvoid},), &x)"
    @test replstr(Meta.parse(s)) == ":($s)"
end

# recursive array printing
# issue #10353
let a = Any[]
    push!(a,a)
    show(IOBuffer(), a)
    push!(a,a)
    show(IOBuffer(), a)
end

# expression printing

macro test_repr(x)
    # this is a macro instead of function so we can avoid getting useful backtraces :)
    return :(test_repr($(esc(x))))
end
function test_repr(x::String)
    # Note: We can't just compare x1 and x2 because interpolated
    # strings get converted to string Exprs by the first show().
    # This could produce a few false positives, but until string
    # interpolation works we don't really have a choice.
    x1 = Meta.parse(x)
    x2 = eval(Meta.parse(repr(x1)))
    x3 = eval(Meta.parse(repr(x2)))
    if x3 != x1
        error(string(
            "repr test failed:",
            "\noriginal: ", x,
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
@test_repr "f(1, 2, 3)"
@test_repr "x = ~y"
@test_repr ":(:x, :y)"
@test_repr ":(:(:(x)))"
@test_repr "-\"\""
@test_repr "-(<=)"

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
@test_repr "w = ((x = y) = z)" # parens aren't necessary, but not wrong
@test_repr "w = ((x, y) = z)" # parens aren't necessary, but not wrong
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

# Exponentiation (>= operator_precedence(:^)) and unary operators
@test_repr "(-1)^a"
@test_repr "(-2.1)^-1"
@test_repr "(-x)^a"
@test_repr "(-a)^-1"
@test_repr "(!x)↑!a"
@test_repr "(!x).a"
@test_repr "(!x)::a"

# invalid UTF-8 strings
@test_repr "\"\\ud800\""
@test_repr "\"\\udfff\""
@test_repr "\"\\xc0\\xb0\""
@test_repr "\"\\xe0\\xb0\\xb0\""
@test_repr "\"\\xf0\\xb0\\xb0\\xb0\""

# import statements
@test_repr "using A"
@test_repr "using A, B.C, D"
@test_repr "using A: b"
@test_repr "using A: a, x, y.z"
@test_repr "using A.B.C: a, x, y.z"
@test_repr "using ..A: a, x, y.z"
@test_repr "import A"
@test_repr "import A, B.C, D"
@test_repr "import A: b"
@test_repr "import A: a, x, y.z"
@test_repr "import A.B.C: a, x, y.z"
@test_repr "import ..A: a, x, y.z"

# range syntax
@test_repr "1:2"
@test_repr "3:4:5"
let ex4 = Expr(:call, :(:), 1, 2, 3, 4),
    ex1 = Expr(:call, :(:), 1)
    @test eval(Meta.parse(repr(ex4))) == ex4
    @test eval(Meta.parse(repr(ex1))) == ex1
end

# Complex

# Meta.parse(repr(:(...))) returns a double-quoted block, so we need to eval twice to unquote it
@test iszero(eval(eval(Meta.parse(repr(:($(1 + 2im) - $(1 + 2im)))))))


# control structures (shamelessly stolen from base/bitarray.jl)
@test_repr """mutable struct BitArray{N} <: AbstractArray{Bool, N}
    # line meta
    chunks::Vector{UInt64}
    # line meta
    len::Int
    # line meta
    dims::NTuple{N,Int}
    # line meta
    function BitArray(undef, dims::Int...)
        # line meta
        if length(dims) != N
            # line meta
            error(\"number of dimensions must be \$N (got \$(length(dims)))\")
        end
        # line meta
        n = 1
        # line meta
        for d in dims
            # line meta
            if d < 0
                # line meta
                error(\"dimension size must be nonnegative (got \$d)\")
            end
            # line meta
            n *= d
        end
        # line meta
        nc = num_bit_chunks(n)
        # line meta
        chunks = Vector{UInt64}(undef, nc)
        # line meta
        if nc > 0
            # line meta
            chunks[end] = UInt64(0)
        end
        # line meta
        b = new(chunks, n)
        # line meta
        if N != 1
            # line meta
            b.dims = dims
        end
        # line meta
        return b
    end
end"""

@test_repr """function copy_chunks(dest::Vector{UInt64}, pos_d::Integer, src::Vector{UInt64}, pos_s::Integer, numbits::Integer)
    # line meta
    if numbits == 0
        # line meta
        return
    end
    # line meta
    if dest === src && pos_d > pos_s
        # line meta
        return copy_chunks_rtol(dest, pos_d, pos_s, numbits)
    end
    # line meta
    kd0, ld0 = get_chunks_id(pos_d)
    # line meta
    kd1, ld1 = get_chunks_id(pos_d + numbits - 1)
    # line meta
    ks0, ls0 = get_chunks_id(pos_s)
    # line meta
    ks1, ls1 = get_chunks_id(pos_s + numbits - 1)
    # line meta
    delta_kd = kd1 - kd0
    # line meta
    delta_ks = ks1 - ks0
    # line meta
    u = _msk64
    # line meta
    if delta_kd == 0
        # line meta
        msk_d0 = ~(u << ld0) | (u << ld1 << 1)
    else
        # line meta
        msk_d0 = ~(u << ld0)
        # line meta
        msk_d1 = (u << ld1 << 1)
    end
    # line meta
    if delta_ks == 0
        # line meta
        msk_s0 = (u << ls0) & ~(u << ls1 << 1)
    else
        # line meta
        msk_s0 = (u << ls0)
    end
    # line meta
    chunk_s0 = glue_src_bitchunks(src, ks0, ks1, msk_s0, ls0)
    # line meta
    dest[kd0] = (dest[kd0] & msk_d0) | ((chunk_s0 << ld0) & ~msk_d0)
    # line meta
    if delta_kd == 0
        # line meta
        return
    end
    # line meta
    for i = 1 : kd1 - kd0 - 1
        # line meta
        chunk_s1 = glue_src_bitchunks(src, ks0 + i, ks1, msk_s0, ls0)
        # line meta
        chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1) | (chunk_s1 << ld0)
        # line meta
        dest[kd0 + i] = chunk_s
        # line meta
        chunk_s0 = chunk_s1
    end
    # line meta
    if ks1 >= ks0 + delta_kd
        # line meta
        chunk_s1 = glue_src_bitchunks(src, ks0 + delta_kd, ks1, msk_s0, ls0)
    else
        # line meta
        chunk_s1 = UInt64(0)
    end
    # line meta
    chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1) | (chunk_s1 << ld0)
    # line meta
    dest[kd1] = (dest[kd1] & msk_d1) | (chunk_s & ~msk_d1)
    # line meta
    return
end"""

@test_repr """if a
# line meta
b
end
"""

@test_repr """if a
# line meta
b
elseif c
# line meta
d
end
"""

@test_repr """if a
# line meta
b
elseif c
# line meta
d
else
# line meta
e
end
"""

@test_repr """if a
# line meta
b
elseif c
# line meta
d
elseif e
# line meta
f
end
"""

@test_repr """f(x, y) do z, w
# line meta
a
# line meta
b
end
"""

@test_repr """f(x, y) do z
# line meta
a
# line meta
b
end
"""

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
              r"\s+" => " ") == ":(function ==(a, b) return a == b end)"

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
# line meta
# line meta
import ...B.c
# line meta
import D
# line meta
import B.C.D.E.F.g
end"
@test_repr "baremodule Y
# line meta
# line meta
export A, B, C
# line meta
export D, E, F
end"

# issue #19840
@test_repr "Array{Int}(undef, 0)"
@test_repr "Array{Int}(undef, 0,0)"
@test_repr "Array{Int}(undef, 0,0,0)"
@test_repr "Array{Int}(undef, 0,1)"
@test_repr "Array{Int}(undef, 0,0,1)"

# issue #8994
@test_repr "get! => 2"
@test_repr "(<) : 2"
@test_repr "(<) :: T"
@test_repr "S{(<) <: T}"
@test_repr "+ + +"

# issue #9474
for s in ("(1::Int64 == 1::Int64)::Bool", "(1:2:3) + 4", "x = 1:2:3")
    local s
    @test sprint(show, Meta.parse(s)) == ":("*s*")"
end

# parametric type instantiation printing
struct TParametricPrint{a}; end
@test sprint(show, :(TParametricPrint{false}())) == ":(TParametricPrint{false}())"

# issue #9797
let q1 = Meta.parse(repr(:("$(a)b"))),
    q2 = Meta.parse(repr(:("$ab")))
    @test isa(q1, Expr)
    @test q1.args[1].head === :string
    @test q1.args[1].args == [:a, "b"]

    @test isa(q2, Expr)
    @test q2.args[1].head == :string
    @test q2.args[1].args == [:ab,]
end

x8d003 = 2
let a = Expr(:quote,Expr(:$,:x8d003))
    @test eval(Meta.parse(repr(a))) == a
    @test eval(eval(Meta.parse(repr(a)))) == 2
end

# issue #9865
@test occursin(r"^Set\(\[.+….+\]\)$", replstr(Set(1:100)))

# issue #11413
@test string(:(*{1, 2})) == "*{1, 2}"
@test string(:(*{1, x})) == "*{1, x}"
@test string(:(-{x}))   == "-{x}"

# issue #11393
@test_repr "@m(x, y) + z"
@test_repr "(@m(x, y), z)"
@test_repr "[@m(x, y), z]"
@test_repr "A[@m(x, y), z]"
@test_repr "T{@m(x, y), z}"
@test_repr "@m x @n(y) z"
@test_repr "f(@m(x, y); z=@n(a))"
@test_repr "@m(x, y).z"
@test_repr "::@m(x, y) + z"
@test_repr "[@m(x) y z]"
@test_repr "[@m(x) y; z]"
@test_repr "let @m(x), y=z; end"

@test repr(:(@m x y))    == ":(#= $(@__FILE__):$(@__LINE__) =# @m x y)"
@test string(:(@m x y))  ==   "#= $(@__FILE__):$(@__LINE__) =# @m x y"
@test string(:(@m x y;)) == "begin\n    #= $(@__FILE__):$(@__LINE__) =# @m x y\nend"

# issue #11436
@test_repr "1 => 2 => 3"
@test_repr "1 => (2 => 3)"
@test_repr "(1 => 2) => 3"

# pr 12008
@test_repr "primitive type A B end"
@test_repr "primitive type B 100 end"
@test repr(:(primitive type A B end)) == ":(primitive type A B end)"
@test repr(:(primitive type B 100 end)) == ":(primitive type B 100 end)"

# `where` syntax
@test_repr "A where T<:B"
@test_repr "A where T<:(Array{T} where T<:Real)"
@test_repr "Array{T} where T<:Array{S} where S<:Real"
@test_repr "x::Array{T} where T"
@test_repr "(a::b) where T"
@test_repr "a::b where T"
@test_repr "X where (T=1)"
@test_repr "X where T = 1"
@test_repr "Array{<:Real}"
@test_repr "Array{>:Real}"

let oldout = stdout, olderr = stderr
    local rdout, wrout, rderr, wrerr, out, err, rd, wr, io
    try
        # pr 16917
        rdout, wrout = redirect_stdout()
        @test wrout === stdout
        out = @async read(rdout, String)
        rderr, wrerr = redirect_stderr()
        @test wrerr === stderr
        err = @async read(rderr, String)
        @test dump(Int64) === nothing
        if !Sys.iswindows()
            close(wrout)
            close(wrerr)
        end

        for io in (Core.stdout, Core.stderr)
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
        @test fetch(out) == "Int64 <: Signed\nTESTA\nTESTB\nΑ1Β2\"A\"\nA\n123\"C\"\n"
        @test fetch(err) == "TESTA\nTESTB\nΑ1Β2\"A\"\n"
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
    @test chomp(read(filename, String)) == "hello"
    ret = open(filename, "w") do f
        redirect_stderr(f) do
            println(stderr, "WARNING: hello")
            [2]
        end
    end
    @test ret == [2]

    # stdin is unavailable on the workers. Run test on master.
    @test occursin("WARNING: hello", read(filename, String))
    ret = Core.eval(Main, quote
        remotecall_fetch(1, $filename) do fname
            open(fname) do f
                redirect_stdin(f) do
                    readline()
                end
            end
        end
    end)

    @test occursin("WARNING: hello", ret)
    rm(filename)
end

# issue #12960
mutable struct T12960 end
let
    A = sparse(1.0I, 3, 3)
    B = similar(A, T12960)
    @test sprint(show, B)  == "\n  [1, 1]  =  #undef\n  [2, 2]  =  #undef\n  [3, 3]  =  #undef"
    @test sprint(print, B) == "\n  [1, 1]  =  #undef\n  [2, 2]  =  #undef\n  [3, 3]  =  #undef"
    B[1,2] = T12960()
    @test sprint(show, B)  == "\n  [1, 1]  =  #undef\n  [1, 2]  =  T12960()\n  [2, 2]  =  #undef\n  [3, 3]  =  #undef"
    @test sprint(print, B) == "\n  [1, 1]  =  #undef\n  [1, 2]  =  T12960()\n  [2, 2]  =  #undef\n  [3, 3]  =  #undef"
end

# issue #13127
function f13127()
    buf = IOBuffer()
    f() = 1
    show(buf, f)
    String(take!(buf))
end
@test startswith(f13127(), "getfield($(@__MODULE__), Symbol(\"")

@test startswith(sprint(show, typeof(x->x), context = :module=>@__MODULE__), "getfield($(@__MODULE__), Symbol(\"")

#test methodshow.jl functions
@test Base.inbase(Base)
@test !Base.inbase(LinearAlgebra)
@test !Base.inbase(Core)

let repr = sprint(show, "text/plain", methods(Base.inbase))
    @test occursin("inbase(m::Module)", repr)
end
let repr = sprint(show, "text/html", methods(Base.inbase))
    @test occursin("inbase(m::<b>Module</b>)", repr)
end

f5971(x, y...; z=1, w...) = nothing
let repr = sprint(show, "text/plain", methods(f5971))
    @test occursin("f5971(x, y...; z, w...)", repr)
end
let repr = sprint(show, "text/html", methods(f5971))
    @test occursin("f5971(x, y...; <i>z, w...</i>)", repr)
end
f16580(x, y...; z=1, w=y+x, q...) = nothing
let repr = sprint(show, "text/html", methods(f16580))
    @test occursin("f16580(x, y...; <i>z, w, q...</i>)", repr)
end

if isempty(Base.GIT_VERSION_INFO.commit)
    @test occursin("https://github.com/JuliaLang/julia/tree/v$VERSION/base/special/trig.jl#L", Base.url(which(sin, (Float64,))))
else
    @test occursin("https://github.com/JuliaLang/julia/tree/$(Base.GIT_VERSION_INFO.commit)/base/special/trig.jl#L", Base.url(which(sin, (Float64,))))
end

# print_matrix should be able to handle small and large objects easily, test by
# calling show. This also indirectly tests print_matrix_row, which
# is used repeatedly by print_matrix.
# This fits on screen:
@test replstr(Matrix(1.0I, 10, 10)) == "10×10 Array{Float64,2}:\n 1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0\n 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0"
# an array too long vertically to fit on screen, and too long horizontally:
@test replstr(Vector(1.:100.)) == "100-element Array{Float64,1}:\n   1.0\n   2.0\n   3.0\n   4.0\n   5.0\n   6.0\n   7.0\n   8.0\n   9.0\n  10.0\n   ⋮  \n  92.0\n  93.0\n  94.0\n  95.0\n  96.0\n  97.0\n  98.0\n  99.0\n 100.0"
@test occursin(r"1×100 (LinearAlgebra\.)?Adjoint{Float64,Array{Float64,1}}:\n 1.0  2.0  3.0  4.0  5.0  6.0  7.0  …  95.0  96.0  97.0  98.0  99.0  100.0", replstr(Vector(1.:100.)'))
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
let ex,
    l1 = Expr(:line, 42),
    l2 = Expr(:line, 42, :myfile),
    l2n = LineNumberNode(42)
    @test string(l2n) == "#= line 42 =#"
    @test string(l2)  == "#= myfile:42 =#"
    @test string(l1)  == string(l2n)
    ex = Expr(:block, l1, :x, l2, :y, l2n, :z)
    @test replace(string(ex)," " => "") == replace("""
    begin
        #= line 42 =#
        x
        #= myfile:42 =#
        y
        #= line 42 =#
        z
    end""", " " => "")
end
# Test the printing of whatever form of line number representation
# that is used in the arguments to a macro looks the same as for
# regular quoting
macro strquote(ex)
    return QuoteNode(string(ex))
end
let str_ex2a = @strquote(begin x end), str_ex2b = string(quote x end)
    @test str_ex2a == str_ex2b
end


# test structured zero matrix printing for select structured types
let A = reshape(1:16, 4, 4)
    @test occursin(r"4×4 (LinearAlgebra\.)?Diagonal{Int(32|64),Array{Int(32|64),1}}:\n 1  ⋅   ⋅   ⋅\n ⋅  6   ⋅   ⋅\n ⋅  ⋅  11   ⋅\n ⋅  ⋅   ⋅  16", replstr(Diagonal(A)))
    @test occursin(r"4×4 (LinearAlgebra\.)?Bidiagonal{Int(32|64),Array{Int(32|64),1}}:\n 1  5   ⋅   ⋅\n ⋅  6  10   ⋅\n ⋅  ⋅  11  15\n ⋅  ⋅   ⋅  16", replstr(Bidiagonal(A, :U)))
    @test occursin(r"4×4 (LinearAlgebra\.)?Bidiagonal{Int(32|64),Array{Int(32|64),1}}:\n 1  ⋅   ⋅   ⋅\n 2  6   ⋅   ⋅\n ⋅  7  11   ⋅\n ⋅  ⋅  12  16", replstr(Bidiagonal(A, :L)))
    @test occursin(r"4×4 (LinearAlgebra\.)?SymTridiagonal{Int(32|64),Array{Int(32|64),1}}:\n 2   7   ⋅   ⋅\n 7  12  17   ⋅\n ⋅  17  22  27\n ⋅   ⋅  27  32", replstr(SymTridiagonal(A + A')))
    @test occursin(r"4×4 (LinearAlgebra\.)?Tridiagonal{Int(32|64),Array{Int(32|64),1}}:\n 1  5   ⋅   ⋅\n 2  6  10   ⋅\n ⋅  7  11  15\n ⋅  ⋅  12  16", replstr(Tridiagonal(diag(A, -1), diag(A), diag(A, +1))))
    @test occursin(r"4×4 (LinearAlgebra\.)?UpperTriangular{Int(32|64),Array{Int(32|64),2}}:\n 1  5   9  13\n ⋅  6  10  14\n ⋅  ⋅  11  15\n ⋅  ⋅   ⋅  16", replstr(UpperTriangular(copy(A))))
    @test occursin(r"4×4 (LinearAlgebra\.)?LowerTriangular{Int(32|64),Array{Int(32|64),2}}:\n 1  ⋅   ⋅   ⋅\n 2  6   ⋅   ⋅\n 3  7  11   ⋅\n 4  8  12  16", replstr(LowerTriangular(copy(A))))
end

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
show_f5(A::AbstractArray{T, N}, indices::Vararg{Int,N}) where {T, N} = [indices...]
test_mt(show_f1, "show_f1(x...)")
test_mt(show_f2, "show_f2(x...)")
test_mt(show_f3, "show_f3(x...)")
test_mt(show_f4, "show_f4(x::Vararg{Any,3})")
test_mt(show_f5, "show_f5(A::AbstractArray{T,N}, indices::Vararg{$Int,N})")

# Issue #15525, printing of vcat
@test sprint(show, :([a;])) == ":([a;])"
@test sprint(show, :([a; b])) == ":([a; b])"
@test_repr "[a;]"
@test_repr "[a; b]"

# other brackets and braces
@test_repr "[a]"
@test_repr "[a,b]"
@test_repr "[a;b;c]"
@test_repr "[a b]"
@test_repr "[a b;]"
@test_repr "[a b c]"
@test_repr "[a b; c d]"
@test_repr "{a}"
@test_repr "{a,b}"
@test_repr "{a;b;c}"
@test_repr "{a b}"
@test_repr "{a b;}"
@test_repr "{a b c}"
@test_repr "{a b; c d}"

# Printing of :(function f end)
@test sprint(show, :(function f end)) == ":(function f end)"
@test_repr "function g end"

# Issue #15765 printing of continue and break
@test sprint(show, :(continue)) == ":(continue)"
@test sprint(show, :(break)) == ":(break)"
@test_repr "continue"
@test_repr "break"

let x = [], y = [], z = Base.ImmutableDict(x => y)
    push!(x, y)
    push!(y, x)
    push!(y, z)
    @test replstr(x) == "1-element Array{Any,1}:\n Any[Any[Any[#= circular reference @-2 =#]], Base.ImmutableDict([Any[#= circular reference @-3 =#]]=>[#= circular reference @-2 =#])]"
    @test repr(z) == "Base.ImmutableDict([Any[Any[#= circular reference @-2 =#], Base.ImmutableDict(#= circular reference @-3 =#)]]=>[Any[Any[#= circular reference @-2 =#]], Base.ImmutableDict(#= circular reference @-2 =#)])"
    @test sprint(dump, x) == """
        Array{Any}((1,))
          1: Array{Any}((2,))
            1: Array{Any}((1,))#= circular reference @-2 =#
            2: Base.ImmutableDict{Array{Any,1},Array{Any,1}}
              parent: Base.ImmutableDict{Array{Any,1},Array{Any,1}}
                parent: #undef
                key: #undef
                value: #undef
              key: Array{Any}((1,))#= circular reference @-3 =#
              value: Array{Any}((2,))#= circular reference @-2 =#
        """
    dz = sprint(dump, z)
    @test 10 < countlines(IOBuffer(dz)) < 40
    @test sum(x -> 1, eachmatch(r"circular reference", dz)) == 4
end

# PR 16221
# Printing of upper and lower bound of a TypeVar
@test string(TypeVar(:V, Signed, Real)) == "Signed<:V<:Real"
# Printing of primary type in type parameter place should not show the type
# parameter names.
@test string(Array) == "Array"
@test string(Tuple{Array}) == "Tuple{Array}"

# PR #16651
@test !occursin("\u2026", repr(fill(1.,10,10)))
@test occursin("\u2026", sprint((io, x) -> show(IOContext(io, :limit => true), x), fill(1.,30,30)))

let io = IOBuffer()
    ioc = IOContext(io, :limit => true)
    @test sprint(show, ioc) == "IOContext($(sprint(show, ioc.io)))"
end

@testset "PR 17117: print_array" begin
    s = IOBuffer(Vector{UInt8}(), read=true, write=true)
    Base.print_array(s, [1, 2, 3])
    @test String(resize!(s.data, s.size)) == " 1\n 2\n 3"
    close(s)
    s2 = IOBuffer(Vector{UInt8}(), read=true, write=true)
    z = zeros(0,0,0,0,0,0,0,0)
    Base.print_array(s2, z)
    @test String(resize!(s2.data, s2.size)) == ""
    close(s2)
end

let repr = sprint(dump, :(x = 1))
    @test repr == "Expr\n  head: Symbol =\n  args: Array{Any}((2,))\n    1: Symbol x\n    2: $Int 1\n"
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
let repr = sprint(dump, Any)
    @test length(repr) == 4
    @test occursin(r"^Any\n", repr)
    @test endswith(repr, '\n')
end
let repr = sprint(dump, Integer)
    @test occursin("Integer <: Real", repr)
    @test !occursin("Any", repr)
end
let repr = sprint(dump, Union{Integer, Float32})
    @test repr == "Union{Integer, Float32}\n" || repr == "Union{Float32, Integer}\n"
end
let repr = sprint(dump, Ptr{UInt8}(UInt(1)))
    @test repr == "Ptr{UInt8} @$(Base.repr(UInt(1)))\n"
end
let repr = sprint(dump, Core.svec())
    @test repr == "empty SimpleVector\n"
end
let sv = Core.svec(:a, :b, :c)
    # unsafe replacement of :c with #undef to test handling of incomplete SimpleVectors
    unsafe_store!(convert(Ptr{Ptr{Cvoid}}, Base.pointer_from_objref(sv)) + 3 * sizeof(Ptr), C_NULL)
    repr = sprint(dump, sv)
    @test repr == "SimpleVector\n  1: Symbol a\n  2: Symbol b\n  3: #undef\n"
end
let repr = sprint(dump, sin)
    @test repr == "sin (function of type typeof(sin))\n"
end
let repr = sprint(dump, Test)
    @test repr == "Module Test\n"
end
let repr = sprint(dump, nothing)
    @test repr == "Nothing nothing\n"
end
let a = Vector{Any}(undef, 10000)
    a[2] = "elemA"
    a[4] = "elemB"
    a[11] = "elemC"
    repr = sprint(dump, a; context=(:limit => true), sizehint=0)
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
@test_repr "(x for i in a, b in c)"
@test_repr "(x for a in b, c in d for e in f)"

for op in (:(.=), :(.+=), :(.&=))
    @test repr(Meta.parse("x $op y")) == ":(x $op y)"
end

# pretty-printing of compact broadcast expressions (#17289)
@test repr(:(f.(X, Y))) == ":(f.(X, Y))"
@test repr(:(f.(X))) == ":(f.(X))"
@test repr(:(f.())) == ":(f.())"
# broadcasted operators (#26517)
@test_repr ":(y .= (+).(x, (*).(3, sin.(x))))"
@test repr(:(y .= (+).(x, (*).(3, (sin).(x))))) == ":(y .= (+).(x, (*).(3, sin.(x))))"

# pretty-printing of other `.` exprs
test_repr("a.b")
test_repr("a.in")
test_repr(":a.b")
test_repr("a.:+")
test_repr("(+).a")
test_repr("(+).:-")
test_repr("(!).:~")
test_repr("a.:(begin
        #= none:3 =#
    end)")
test_repr("a.:(=)")
test_repr("a.:(:)")
test_repr("(:).a")
@test repr(Expr(:., :a, :b, :c)) == ":(\$(Expr(:., :a, :b, :c)))"
@test repr(Expr(:., :a, :b)) == ":(\$(Expr(:., :a, :b)))"
@test repr(Expr(:., :a)) == ":(\$(Expr(:., :a)))"
@test repr(Expr(:.)) == ":(\$(Expr(:.)))"
@test repr(GlobalRef(Main, :a)) == ":(Main.a)"
@test repr(GlobalRef(Main, :in)) == ":(Main.in)"
@test repr(GlobalRef(Main, :+)) == ":(Main.:+)"
@test repr(GlobalRef(Main, :(:))) == ":(Main.:(:))"

# Test compact printing of homogeneous tuples
@test repr(NTuple{7,Int64}) == "NTuple{7,Int64}"
@test repr(Tuple{Float64, Float64, Float64, Float64}) == "NTuple{4,Float64}"
@test repr(Tuple{Float32, Float32, Float32}) == "Tuple{Float32,Float32,Float32}"

# Test that REPL/mime display of invalid UTF-8 data doesn't throw an exception:
@test isa(repr("text/plain", String(UInt8[0x00:0xff;])), String)

# don't use julia-specific `f` in Float32 printing (PR #18053)
@test sprint(print, 1f-7) == "1.0e-7"

let d = TextDisplay(IOBuffer())
    @test_throws MethodError display(d, "text/foobar", [3 1 4])
    try
        display(d, "text/foobar", [3 1 4])
    catch e
        @test e.f == show
    end
end

struct TypeWith4Params{a,b,c,d}
end
@test endswith(string(TypeWith4Params{Int8,Int8,Int8,Int8}), "TypeWith4Params{Int8,Int8,Int8,Int8}")

# issues #20332 and #20781
struct T20332{T}
end

(::T20332{T})(x) where T = 0

let m = which(T20332{Int}(), (Int,)),
    mi = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt),
               m, Tuple{T20332{T}, Int} where T, Core.svec(), typemax(UInt))
    # test that this doesn't throw an error
    @test occursin("MethodInstance for", repr(mi))
end

@test sprint(show, Main) == "Main"

@test sprint(Base.show_supertypes, Int64) == "Int64 <: Signed <: Integer <: Real <: Number <: Any"
@test sprint(Base.show_supertypes, Vector{String}) == "Array{String,1} <: DenseArray{String,1} <: AbstractArray{String,1} <: Any"

# static_show

function static_shown(x)
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    ccall(:jl_static_show, Cvoid, (Ptr{Cvoid}, Any), p.in, x)
    @async close(p.in)
    return read(p.out, String)
end

# Test for PR 17803
@test static_shown(Int128(-1)) == "Int128(0xffffffffffffffffffffffffffffffff)"

# PR #22160
@test static_shown(:aa) == ":aa"
@test static_shown(:+) == ":+"
@test static_shown(://) == "://"
@test static_shown(://=) == "://="
@test static_shown(Symbol("")) == "Symbol(\"\")"
@test static_shown(Symbol("a/b")) == "Symbol(\"a/b\")"
@test static_shown(Symbol("a-b")) == "Symbol(\"a-b\")"
@test static_shown(UnionAll) == "UnionAll"

@test static_shown(QuoteNode(:x)) == ":(:x)"

# Test @show
let fname = tempname()
    try
        open(fname, "w") do fout
            redirect_stdout(fout) do
                @show zeros(2, 2)
            end
        end
        @test read(fname, String) == "zeros(2, 2) = [0.0 0.0; 0.0 0.0]\n"
    finally
        rm(fname, force=true)
    end
end

struct f_with_params{t} <: Function
end

(::f_with_params)(x) = 2x

let io = IOBuffer()
    show(io, MIME"text/html"(), f_with_params.body.name.mt)
    @test occursin("f_with_params", String(take!(io)))
end

@testset "printing of Val's" begin
    @test sprint(show, Val(Float64))  == "Val{Float64}()"  # Val of a type
    @test sprint(show, Val(:Float64)) == "Val{:Float64}()" # Val of a symbol
    @test sprint(show, Val(true))     == "Val{true}()"     # Val of a value
end

@testset "printing of Pair's" begin
    for (p, s) in (Pair(1.0,2.0)                          => "1.0 => 2.0",
                   Pair(Pair(1,2), Pair(3,4))             => "(1=>2) => (3=>4)",
                   Pair{Integer,Int64}(1, 2)              => "Pair{Integer,Int64}(1, 2)",
                   (Pair{Integer,Int64}(1, 2) => 3)       => "Pair{Integer,Int64}(1, 2) => 3",
                   ((1+2im) => (3+4im))                   => "1+2im => 3+4im",
                   (1 => 2 => Pair{Real,Int64}(3, 4))     => "1 => (2=>Pair{Real,Int64}(3, 4))")
        local s
        @test sprint(show, p) == s
    end
    # - when the context has :compact=>false, print pair's member non-compactly
    # - if one member is printed as "Pair{...}(...)", no need to put parens around
    s = IOBuffer()
    show(IOContext(s, :compact => false), (1=>2) => Pair{Any,Any}(3,4))
    @test String(take!(s)) == "(1 => 2) => Pair{Any,Any}(3, 4)"

    # issue #28327
    d = Dict(Pair{Integer,Integer}(1,2)=>Pair{Integer,Integer}(1,2))
    @test showstr(d) == "Dict((1=>2)=>(1=>2))" # correct parenthesis
end

@testset "alignment for pairs" begin  # (#22899)
    @test replstr([1=>22,33=>4]) == "2-element Array{Pair{$Int,$Int},1}:\n  1 => 22\n 33 => 4 "
    # first field may have "=>" in its representation
    @test replstr(Pair[(1=>2)=>3, 4=>5]) ==
        "2-element Array{Pair,1}:\n (1=>2) => 3\n      4 => 5"
    @test replstr(Any[Dict(1=>2)=> (3=>4), 1=>2]) ==
        "2-element Array{Any,1}:\n Dict(1=>2) => (3=>4)\n          1 => 2     "
    # left-alignment when not using the "=>" symbol
    @test replstr(Any[Pair{Integer,Int64}(1, 2), Pair{Integer,Int64}(33, 4)]) ==
        "2-element Array{Any,1}:\n Pair{Integer,Int64}(1, 2) \n Pair{Integer,Int64}(33, 4)"
end

@testset "display arrays non-compactly when size(⋅, 2) == 1" begin
    # 0-dim
    @test replstr(zeros(Complex{Int})) == "0-dimensional Array{Complex{$Int},0}:\n0 + 0im"
    A = Array{Pair,0}(undef); A[] = 1=>2
    @test replstr(A) == "0-dimensional Array{Pair,0}:\n1 => 2"
    # 1-dim
    @test replstr(zeros(Complex{Int}, 2)) ==
        "2-element Array{Complex{$Int},1}:\n 0 + 0im\n 0 + 0im"
    @test replstr([1=>2, 3=>4]) == "2-element Array{Pair{$Int,$Int},1}:\n 1 => 2\n 3 => 4"
    # 2-dim
    @test replstr(zeros(Complex{Int}, 2, 1)) ==
        "2×1 Array{Complex{$Int},2}:\n 0 + 0im\n 0 + 0im"
    @test replstr(zeros(Complex{Int}, 1, 2)) ==
        "1×2 Array{Complex{$Int},2}:\n 0+0im  0+0im"
    @test replstr([1=>2 3=>4]) == "1×2 Array{Pair{$Int,$Int},2}:\n 1=>2  3=>4"
    @test replstr([1=>2 for x in 1:2, y in 1:1]) ==
        "2×1 Array{Pair{$Int,$Int},2}:\n 1 => 2\n 1 => 2"
    # 3-dim
    @test replstr(zeros(Complex{Int}, 1, 1, 1)) ==
        "1×1×1 Array{Complex{$Int},3}:\n[:, :, 1] =\n 0 + 0im"
    @test replstr(zeros(Complex{Int}, 1, 2, 1)) ==
        "1×2×1 Array{Complex{$Int},3}:\n[:, :, 1] =\n 0+0im  0+0im"
end

@testset "arrays printing follows the :compact property when specified" begin
    x = 3.141592653589793
    @test showstr(x) == "3.141592653589793"
    @test showstr([x, x]) == showstr([x, x], :compact => true) == "[3.14159, 3.14159]"
    @test showstr([x, x], :compact => false) == "[3.141592653589793, 3.141592653589793]"
    @test showstr([x x; x x]) == showstr([x x; x x], :compact => true) ==
        "[3.14159 3.14159; 3.14159 3.14159]"
    @test showstr([x x; x x], :compact => false) ==
        "[3.141592653589793 3.141592653589793; 3.141592653589793 3.141592653589793]"
    @test replstr([x, x]) == replstr([x, x], :compact => false) ==
        "2-element Array{Float64,1}:\n 3.141592653589793\n 3.141592653589793"
    @test replstr([x, x], :compact => true) ==
        "2-element Array{Float64,1}:\n 3.14159\n 3.14159"
    @test replstr([x x; x x]) == replstr([x x; x x], :compact => true) ==
        "2×2 Array{Float64,2}:\n 3.14159  3.14159\n 3.14159  3.14159"
    @test showstr([x x; x x], :compact => false) ==
        "[3.141592653589793 3.141592653589793; 3.141592653589793 3.141592653589793]"
end

@testset "Array printing with limited rows" begin
    arrstr = let buf = IOBuffer()
        function (A, rows)
            show(IOContext(buf, :displaysize => (rows, 80), :limit => true), "text/plain", A)
            String(take!(buf))
        end
    end
    A = Int64[1]
    @test arrstr(A, 4) == "1-element Array{Int64,1}: …"
    @test arrstr(A, 5) == "1-element Array{Int64,1}:\n 1"
    push!(A, 2)
    @test arrstr(A, 5) == "2-element Array{Int64,1}:\n ⋮"
    @test arrstr(A, 6) == "2-element Array{Int64,1}:\n 1\n 2"
    push!(A, 3)
    @test arrstr(A, 6) == "3-element Array{Int64,1}:\n 1\n ⋮"

    @test arrstr(zeros(4, 3), 4)  == "4×3 Array{Float64,2}: …"
    @test arrstr(zeros(4, 30), 4) == "4×30 Array{Float64,2}: …"
    @test arrstr(zeros(4, 3), 5)  == "4×3 Array{Float64,2}:\n ⋮      ⋱  "
    @test arrstr(zeros(4, 30), 5) == "4×30 Array{Float64,2}:\n ⋮      ⋱  "
    @test arrstr(zeros(4, 3), 6)  == "4×3 Array{Float64,2}:\n 0.0  0.0  0.0\n ⋮            "
    @test arrstr(zeros(4, 30), 6) ==
              string("4×30 Array{Float64,2}:\n",
                     " 0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  …  0.0  0.0  0.0  0.0  0.0  0.0  0.0\n",
                     " ⋮                        ⋮              ⋱            ⋮                      ")
end

module UnexportedOperators
function + end
function == end
end

@testset "Parseable printing of types" begin
    @test repr(typeof(print)) == "typeof(print)"
    @test repr(typeof(Base.show_default)) == "typeof(Base.show_default)"
    @test repr(typeof(UnexportedOperators.:+)) == "typeof($(curmod_prefix)UnexportedOperators.:+)"
    @test repr(typeof(UnexportedOperators.:(==))) == "typeof($(curmod_prefix)UnexportedOperators.:(==))"
    anonfn = x->2x
    modname = string(@__MODULE__)
    anonfn_type_repr = "getfield($modname, Symbol(\"$(typeof(anonfn).name.name)\"))"
    @test repr(typeof(anonfn)) == anonfn_type_repr
    @test repr(anonfn) == anonfn_type_repr * "()"
    @test repr("text/plain", anonfn) == "$(typeof(anonfn).name.mt.name) (generic function with 1 method)"
    mkclosure = x->y->x+y
    clo = mkclosure(10)
    @test repr("text/plain", clo) == "$(typeof(clo).name.mt.name) (generic function with 1 method)"
    @test repr(UnionAll) == "UnionAll"
end

let x = TypeVar(:_), y = TypeVar(:_)
    @test repr(UnionAll(x, UnionAll(y, Pair{x,y}))) == "Pair{_1,_2} where _2 where _1"
    @test repr(UnionAll(x, UnionAll(y, Pair{UnionAll(x,Ref{x}),y}))) == "Pair{Ref{_1} where _1,_1} where _1"
    x = TypeVar(:a)
    y = TypeVar(:a)
    z = TypeVar(:a)
    @test repr(UnionAll(z, UnionAll(x, UnionAll(y, Tuple{x,y,z})))) == "Tuple{a1,a2,a} where a2 where a1 where a"
end

@testset "showarg" begin
    A = reshape(Vector(Int16(1):Int16(2*3*5)), 2, 3, 5)
    @test summary(A) == "2×3×5 Array{Int16,3}"
    v = view(A, :, 3, 2:5)
    @test summary(v) == "2×4 view(::Array{Int16,3}, :, 3, 2:5) with eltype Int16"
    r = reshape(v, 4, 2)
    @test summary(r) == "4×2 reshape(view(::Array{Int16,3}, :, 3, 2:5), 4, 2) with eltype Int16"
    p = PermutedDimsArray(r, (2, 1))
    @test summary(p) == "2×4 PermutedDimsArray(reshape(view(::Array{Int16,3}, :, 3, 2:5), 4, 2), (2, 1)) with eltype Int16"
end

@testset "Methods" begin
    m = which(sin, (Float64,))
    io = IOBuffer()
    show(io, "text/html", m)
    s = String(take!(io))
    @test occursin(" in Base.Math ", s)
end

module AlsoExportsPair
Pair = 0
export Pair
end

module TestShowType
    export TypeA
    struct TypeA end
    using ..AlsoExportsPair
end

@testset "module prefix when printing type" begin
    @test sprint(show, TestShowType.TypeA) == "$(@__MODULE__).TestShowType.TypeA"

    b = IOBuffer()
    show(IOContext(b, :module => @__MODULE__), TestShowType.TypeA)
    @test String(take!(b)) == "$(@__MODULE__).TestShowType.TypeA"

    b = IOBuffer()
    show(IOContext(b, :module => TestShowType), TestShowType.TypeA)
    @test String(take!(b)) == "TypeA"

    using .TestShowType

    @test sprint(show, TypeA) == "$(@__MODULE__).TestShowType.TypeA"

    b = IOBuffer()
    show(IOContext(b, :module => @__MODULE__), TypeA)
    @test String(take!(b)) == "TypeA"

    # issue #26354; make sure testing for symbol visibility doesn't cause
    # spurious binding resolutions
    show(IOContext(b, :module => TestShowType), Base.Pair)
    @test !Base.isbindingresolved(TestShowType, :Pair)
    @test String(take!(b)) == "Base.Pair"
    show(IOContext(b, :module => TestShowType), Base.Complex)
    @test Base.isbindingresolved(TestShowType, :Complex)
    @test String(take!(b)) == "Complex"
end

@testset "typeinfo" begin
    @test replstr([[Int16(1)]]) == "1-element Array{Array{Int16,1},1}:\n [1]"
    @test showstr([[Int16(1)]]) == "Array{Int16,1}[[1]]"
    @test showstr(Set([[Int16(1)]])) == "Set(Array{Int16,1}[[1]])"
    @test showstr([Float16(1)]) == "Float16[1.0]"
    @test showstr([[Float16(1)]]) == "Array{Float16,1}[[1.0]]"
    @test replstr(Real[Float16(1)]) == "1-element Array{Real,1}:\n Float16(1.0)"
    @test replstr(Array{Real}[Real[1]]) == "1-element Array{Array{Real,N} where N,1}:\n [1]"
    # printing tuples (Issue #25042)
    @test replstr(fill((Int64(1), zeros(Float16, 3)), 1)) ==
                 "1-element Array{Tuple{Int64,Array{Float16,1}},1}:\n (1, [0.0, 0.0, 0.0])"
    @testset "nested Any eltype" begin
        x = Any[Any[Any[1]]]
        # The element of x (i.e. x[1]) has an eltype which can't be deduced
        # from eltype(x), so this must also be printed
        @test replstr(x) == "1-element Array{Any,1}:\n Any[Any[1]]"
    end
    # Issue #25038
    A = [0.0, 1.0]
    @test replstr(view(A, [1], :)) == "1×1 view(::Array{Float64,2}, [1], :) with eltype Float64:\n 0.0"

    # issue #27680
    @test replstr(Set([(1.0,1.0), (2.0,2.0), (3.0, 3.0)])) == (sizeof(Int) == 8 ?
              "Set(Tuple{Float64,Float64}[(3.0, 3.0), (2.0, 2.0), (1.0, 1.0)])" :
              "Set(Tuple{Float64,Float64}[(1.0, 1.0), (2.0, 2.0), (3.0, 3.0)])")

    # issue #27747
    let t = (x = Integer[1, 2],)
        v = [t, t]
        @test showstr(v) == "NamedTuple{(:x,),Tuple{Array{Integer,1}}}[(x = [1, 2],), (x = [1, 2],)]"
        @test replstr(v) == "2-element Array{NamedTuple{(:x,),Tuple{Array{Integer,1}}},1}:\n (x = [1, 2],)\n (x = [1, 2],)"
    end

    # issue #25857
    @test repr([(1,),(1,2),(1,2,3)]) == "Tuple{$Int,Vararg{$Int,N} where N}[(1,), (1, 2), (1, 2, 3)]"

    # issues #25466 & #26256
    @test replstr([:A => [1]]) == "1-element Array{Pair{Symbol,Array{$Int,1}},1}:\n :A => [1]"

    # issue #26881
    @test showstr([keys(Dict('a' => 'b'))]) == "Base.KeySet{Char,Dict{Char,Char}}[['a']]"
    @test showstr([values(Dict('a' => 'b'))]) == "Base.ValueIterator{Dict{Char,Char}}[['b']]"
    @test replstr([keys(Dict('a' => 'b'))]) == "1-element Array{Base.KeySet{Char,Dict{Char,Char}},1}:\n ['a']"

    @test showstr(Pair{Integer,Integer}(1, 2), :typeinfo => Pair{Integer,Integer}) == "1 => 2"
    @test showstr([Pair{Integer,Integer}(1, 2)]) == "Pair{Integer,Integer}[1=>2]"
    @test showstr(Dict{Integer,Integer}(1 => 2)) == "Dict{Integer,Integer}(1=>2)"

    # issue #27979 (dislaying arrays of pairs containing arrays as first member)
    @test replstr([[1.0]=>1.0]) == "1-element Array{Pair{Array{Float64,1},Float64},1}:\n [1.0] => 1.0"

    # issue #28159
    @test replstr([(a=1, b=2), (a=3,c=4)]) == "2-element Array{NamedTuple{names,Tuple{$Int,$Int}} where names,1}:\n (a = 1, b = 2)\n (a = 3, c = 4)"

    @test replstr(Vector[Any[1]]) == "1-element Array{Array{T,1} where T,1}:\n Any[1]"
    @test replstr(AbstractDict{Integer,Integer}[Dict{Integer,Integer}(1=>2)]) ==
        "1-element Array{AbstractDict{Integer,Integer},1}:\n Dict(1=>2)"
end

@testset "#14684: `display` should print associative types in full" begin
    d = Dict(1 => 2, 3 => 45)
    buf = IOBuffer()
    td = TextDisplay(buf)

    display(td, d)
    result = String(take!(td.io))
    @test occursin(summary(d), result)

    # Is every pair in the string?
    for el in d
        @test occursin(string(el), result)
    end
end

@testset "#20111 show for function" begin
    K20111(x) = y -> x
    buf = IOBuffer()
    show(buf, methods(K20111(1)))
    @test occursin(" 1 method for generic function", String(take!(buf)))
end

@generated f22798(x::Integer, y) = :x
@testset "#22798" begin
    buf = IOBuffer()
    show(buf, methods(f22798))
    @test occursin("f22798(x::Integer, y)", String(take!(buf)))
end

@testset "Intrinsic printing" begin
    @test sprint(show, Core.Intrinsics.arraylen) == "arraylen"
    let io = IOBuffer()
        show(io, MIME"text/plain"(), Core.Intrinsics.arraylen)
        str = String(take!(io))
        @test occursin("arraylen", str)
        @test occursin("(intrinsic function", str)
    end
end

@testset "repr(mime, x)" begin
    @test repr("text/plain", UInt8[1 2;3 4]) == "2×2 Array{UInt8,2}:\n 0x01  0x02\n 0x03  0x04"
    @test repr("text/html", "raw html data") == "raw html data"
    @test repr("text/plain", "string") == "\"string\""
    @test repr("image/png", UInt8[2,3,4,7]) == UInt8[2,3,4,7]
    @test repr("text/plain", 3.141592653589793) == "3.141592653589793"
    @test repr("text/plain", 3.141592653589793, context=:compact=>true) == "3.14159"
    @test repr("text/plain", context=:compact=>true) == "\"text/plain\""
    @test repr(MIME("text/plain"), context=:compact=>true) == "MIME type text/plain"
end

@testset "#26799 BigInt summary" begin
    @test Base.dims2string(tuple(BigInt(10))) == "10-element"
    @test Base.inds2string(tuple(BigInt(10))) == "10"
    @test summary(BigInt(1):BigInt(10)) == "10-element UnitRange{BigInt}"
    @test summary(Base.OneTo(BigInt(10))) == "10-element Base.OneTo{BigInt}"
end

# Tests for code_typed linetable annotations
function compute_annotations(f, types)
    src = code_typed(f, types)[1][1]
    ir = Core.Compiler.inflate_ir(src)
    la, lb, ll = Base.IRShow.compute_ir_line_annotations(ir)
    max_loc_method = maximum(length(s) for s in la)
    return join((strip(string(a, " "^(max_loc_method-length(a)), b)) for (a, b) in zip(la, lb)), '\n')
end

@noinline leaffunc() = print()

@inline g_line() = leaffunc()

# Test that separate instances of the same function do not get merged
@inline function f_line()
   g_line()
   g_line()
   g_line()
   nothing
end
h_line() = f_line()
@test startswith(compute_annotations(h_line, Tuple{}), """
    │╻╷ f_line
    ││╻  g_line
    ││╻  g_line""")

# Tests for printing Core.Compiler internal objects
@test repr(Core.Compiler.SSAValue(23)) == ":(%23)"
@test repr(Core.Compiler.SSAValue(-2)) == ":(%-2)"
@test repr(Core.Compiler.ReturnNode(23)) == ":(return 23)"
@test repr(Core.Compiler.ReturnNode()) == ":(unreachable)"
@test repr(Core.Compiler.GotoIfNot(true, 4)) == ":(goto %4 if not true)"
@test repr(Core.Compiler.PhiNode(Any[2, 3], Any[1, Core.SlotNumber(3)])) == ":(φ (%2 => 1, %3 => _3))"
@test repr(Core.Compiler.UpsilonNode(Core.SlotNumber(3))) == ":(ϒ (_3))"
@test repr(Core.Compiler.PhiCNode(Any[1, Core.SlotNumber(3)])) == ":(φᶜ (1, _3))"
@test sprint(Base.show_unquoted, Core.Compiler.Argument(23)) == "_23"
@test sprint(Base.show_unquoted, Core.Compiler.Argument(-2)) == "_-2"


eval(Meta.parse("""function my_fun28173(x)
    y = if x == 1
            "HI"
        elseif x == 2
            r = 1
            s = try
                r = 2
                "BYE"
            catch
                r = 3
                "CAUGHT!"
            end
            "\$r\$s"
        else
            "three"
        end
    return y
end""")) # use parse to control the line numbers
let src = code_typed(my_fun28173, (Int,))[1][1]
    ir = Core.Compiler.inflate_ir(src)
    source_slotnames = String["my_fun28173", "x"]
    irshow = sprint(show, ir, context = :SOURCE_SLOTNAMES=>source_slotnames)
    @test repr(src) == "CodeInfo(\n" * irshow * ")"
    lines1 = split(repr(ir), '\n')
    @test isempty(pop!(lines1))
    Core.Compiler.insert_node!(ir, 1, Val{1}, QuoteNode(1), false)
    Core.Compiler.insert_node!(ir, 1, Val{2}, QuoteNode(2), true)
    Core.Compiler.insert_node!(ir, length(ir.stmts), Val{3}, QuoteNode(3), false)
    Core.Compiler.insert_node!(ir, length(ir.stmts), Val{4}, QuoteNode(4), true)
    lines2 = split(repr(ir), '\n')
    @test isempty(pop!(lines2))
    @test popfirst!(lines2) == "2  1 ──       $(QuoteNode(1))"
    @test popfirst!(lines2) == "   │          $(QuoteNode(2))" # TODO: this should print after the next statement
    let line1 = popfirst!(lines1)
        line2 = popfirst!(lines2)
        @test startswith(line1, "2  1 ── ")
        @test startswith(line2, "   │    ")
        @test line2[12:end] == line2[12:end]
    end
    let line1 = pop!(lines1)
        line2 = pop!(lines2)
        @test startswith(line1, "17 ")
        @test startswith(line2, "   ")
        @test line1[3:end] == line2[3:end]
    end
    @test pop!(lines2) == "   │          \$(QuoteNode(4))"
    @test pop!(lines2) == "17 │          \$(QuoteNode(3))" # TODO: this should print after the next statement
    @test lines1 == lines2
end

# issue #27352
@test_throws ArgumentError print(nothing)
@test_throws ArgumentError print(stdout, nothing)
@test_throws ArgumentError string(nothing)
@test_throws ArgumentError string(1, "", nothing)
@test_throws ArgumentError let x = nothing; "x = $x" end
@test let x = nothing; "x = $(repr(x))" end == "x = nothing"
@test_throws ArgumentError `/bin/foo $nothing`
@test_throws ArgumentError `$nothing`

struct X28004
    value::Any
end

function Base.show(io::IO, x::X28004)
    print(io, "X(")
    show(io, x.value)
    print(io, ")")
end

@testset """printing "Any" is not skipped with nested arrays""" begin
    @test replstr(Union{X28004,Vector}[X28004(Any[X28004(1)])]) ==
        "1-element Array{Union{X28004, Array{T,1} where T},1}:\n X(Any[X(1)])"
end

# Issue 25589 - Underlines in cmd printing
replstrcolor(x) = sprint((io, x) -> show(IOContext(io, :limit => true, :color => true),
                                         MIME("text/plain"), x), x)
@test occursin("\e[", replstrcolor(`curl abc`))
