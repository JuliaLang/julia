# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base.Docs: meta, @var, DocStr, parsedoc

# Test helpers.
function docstrings_equal(d1, d2)
    io1 = IOBuffer()
    io2 = IOBuffer()
    show(io1, MIME"text/markdown"(), d1)
    show(io2, MIME"text/markdown"(), d2)
    takebuf_string(io1) == takebuf_string(io2)
end
docstrings_equal(d1::DocStr, d2) = docstrings_equal(parsedoc(d1), d2)

function docstring_startswith(d1, d2)
    io1 = IOBuffer()
    io2 = IOBuffer()
    show(io1, MIME"text/markdown"(), d1)
    show(io2, MIME"text/markdown"(), d2)
    startswith(takebuf_string(io1), takebuf_string(io2))
end
docstring_startswith(d1::DocStr, d2) = docstring_startswith(parsedoc(d1), d2)

@doc "Doc abstract type" ->
abstract C74685 <: AbstractArray
@test stringmime("text/plain", Docs.doc(C74685))=="Doc abstract type\n"

macro macro_doctest() end
@doc "Helps test if macros can be documented with `@doc \"...\" -> @...`." ->
:@macro_doctest

@test (@doc @macro_doctest) !== nothing

# issue #11548

module ModuleMacroDoc
macro m() end
end

@doc "I am a module" ModuleMacroDoc
@doc "I am a macro"  :@ModuleMacroDoc.m

@test docstrings_equal(@doc(ModuleMacroDoc), doc"I am a module")
@test docstrings_equal(@doc(ModuleMacroDoc.@m), doc"I am a macro")

# General tests for docstrings.

const LINE_NUMBER = @__LINE__+1
"DocsTest"
module DocsTest

"f-1"
function f(x)
    x
end

"f-2"
f(x, y) = x + y

"s-1"
@generated function s(x)
    :(x)
end

"s-2"
@generated s(x, y) = :(x + y)

"g"
function g end

"AT"
abstract AT

"BT"
bitstype 8 BT

"BT2"
bitstype 8 BT2 <: Integer

"T"
type T <: AT
    "T.x"
    x
    "T.y"
    y :: Int
end

"IT"
immutable IT
    "IT.x"
    x :: Int
    "IT.y"
    y
end

"TA"
typealias TA Union{T, IT}

"@mac()"
macro mac() end

"@mac(x)"
macro mac(x) end

"@mac(x::Int, y::Expr, z = 0)"
macro mac(x::Int, y::Expr, z = 0) end

":@mac"
:@mac

"G"
G = :G

"K"
const K = :K

# Adding docstrings to methods after definition.

t(x::AbstractString) = x
t(x::Int, y) = y
t{S <: Integer}(x::S) = x

"t-1"
t(::AbstractString)
"t-2"
t(::Int, ::Any)
"t-3"
t{S <: Integer}(::S)

"FieldDocs"
type FieldDocs
    "one"
    one
    doc"two"
    two
    three
end

"h/0-3"
h(x = 1, y = 2, z = 3) = x + y + z

# Issue #12700.
module Inner
    macro m() end
end
import .Inner.@m

"Inner.@m"
:@m

type Foo
    x
end

# value with no docs
const val = Foo(1.0)

"doc multiple expressions"
function multidoc  end,
function multidoc! end

"returntype-1"
returntype(x::Float64)::Float64 = x

"returntype-2"
function returntype(x::Int)::Int
    x
end

end

let md = meta(DocsTest)[@var(DocsTest)]
    @test docstrings_equal(md.docs[Union{}], doc"DocsTest")
    # Check that plain docstrings store a module reference.
    # https://github.com/JuliaLang/julia/pull/13017#issuecomment-138618663
    @test md.docs[Union{}].data[:module] == DocsTest
    @test md.docs[Union{}].data[:linenumber] == LINE_NUMBER
end

let f = @var(DocsTest.f)
    md = meta(DocsTest)[f]
    @test docstrings_equal(md.docs[Tuple{Any}], doc"f-1")
    @test docstrings_equal(md.docs[Tuple{Any,Any}], doc"f-2")
    @test md.docs[Tuple{Any}].data[:binding] === f
    @test md.docs[Tuple{Any}].data[:typesig] === Tuple{Any}
    @test md.docs[Tuple{Any,Any}].data[:binding] === f
    @test md.docs[Tuple{Any,Any}].data[:typesig] === Tuple{Any,Any}
end

let s = @var(DocsTest.s)
    md = meta(DocsTest)[s]
    @test docstrings_equal(md.docs[Tuple{Any,}], doc"s-1")
    @test docstrings_equal(md.docs[Tuple{Any,Any}], doc"s-2")
end

let g = @var(DocsTest.g)
    md = meta(DocsTest)[g]
    @test docstrings_equal(md.docs[Union{}], doc"g")
end

let h = @var(DocsTest.h)
    md = meta(DocsTest)[h]
    sig = Union{Tuple{}, Tuple{Any}, Tuple{Any, Any}, Tuple{Any, Any, Any}}
    @test docstrings_equal(md.docs[sig], doc"h/0-3")
end

let AT = @var(DocsTest.AT)
    md = meta(DocsTest)[AT]
    @test docstrings_equal(md.docs[Union{}], doc"AT")
end

let BT = @var(DocsTest.BT)
    md = meta(DocsTest)[BT]
    @test docstrings_equal(md.docs[Union{}], doc"BT")
end

let BT2 = @var(DocsTest.BT2)
    md = meta(DocsTest)[BT2]
    @test docstrings_equal(md.docs[Union{}], doc"BT2")
end

let T = @var(DocsTest.T)
    md = meta(DocsTest)[T]
    d  = md.docs[Union{}]
    @test docstrings_equal(d, doc"T")
    @test d.data[:fields][:x] == "T.x"
    @test d.data[:fields][:y] == "T.y"
end

let IT = @var(DocsTest.IT)
    md = meta(DocsTest)[IT]
    d  = md.docs[Union{}]
    @test docstrings_equal(d, doc"IT")
    @test d.data[:fields][:x] == "IT.x"
    @test d.data[:fields][:y] == "IT.y"
end

let rt = @var(DocsTest.returntype)
    md = meta(DocsTest)[rt]
    @test md.order == [Tuple{Float64}, Tuple{Int}]
end

@test docstrings_equal(@doc(DocsTest.TA), doc"TA")

@test docstrings_equal(@doc(DocsTest.@mac), doc"@mac()")
@test docstrings_equal(@doc(DocsTest.@mac()), doc"@mac()")
@test docstrings_equal(@doc(DocsTest.@mac(x)), doc"@mac(x)")
@test docstrings_equal(@doc(DocsTest.@mac(x::Int, y::Expr)), doc"@mac(x::Int, y::Expr, z = 0)")
@test docstrings_equal(@doc(DocsTest.@mac(x::Int, y::Expr, z)), doc"@mac(x::Int, y::Expr, z = 0)")
let m = doc"""
        @mac()

        @mac(x)

        @mac(x::Int, y::Expr, z = 0)

        :@mac
        """
    @test docstrings_equal(@doc(:@DocsTest.mac), m)
    @test docstrings_equal(@doc(:(DocsTest.@mac)), m)
end

@test docstrings_equal(@doc(DocsTest.G), doc"G")
@test docstrings_equal(@doc(DocsTest.K), doc"K")

let d1 = @doc(DocsTest.t(::AbstractString)),
    d2 = doc"t-1"
    @test docstrings_equal(d1,d2)
end

let d1 = @doc(DocsTest.t(::AbstractString)),
    d2 = doc"t-1"
    @test docstrings_equal(d1,d2)
end

let d1 = @doc(DocsTest.t(::Int, ::Any)),
    d2 = doc"t-2"
    @test docstrings_equal(d1,d2)
end

let d1 = @doc(DocsTest.t{S <: Integer}(::S)),
    d2 = doc"t-3"
    @test docstrings_equal(d1,d2)
end

let fields = meta(DocsTest)[@var(DocsTest.FieldDocs)].docs[Union{}].data[:fields]
    @test haskey(fields, :one) && fields[:one] == "one"
    @test haskey(fields, :two) && fields[:two] == doc"two"
end

let a = @doc(DocsTest.multidoc),
    b = @doc(DocsTest.multidoc!)
    @test docstrings_equal(a, b)
end

"BareModule"
baremodule BareModule

"f/1"
f(x) = x

"g/1"
function g(x) end

"h"
function h end

"@m"
macro m() end

"C"
const C = 1

"A"
abstract A

"T"
type T
    "x"
    x
    "y"
    y
end

end

@test docstrings_equal(@doc(BareModule), doc"BareModule")
@test docstrings_equal(@doc(BareModule.f), doc"f/1")
@test docstrings_equal(@doc(BareModule.g), doc"g/1")
@test docstrings_equal(@doc(BareModule.@m), doc"@m")
@test docstrings_equal(@doc(BareModule.C), doc"C")
@test docstrings_equal(@doc(BareModule.A), doc"A")
@test docstrings_equal(@doc(BareModule.T), doc"T")

@test_throws ErrorException @doc("...", "error")
@test_throws ErrorException @doc("...", @time 0)

# test that when no docs exist, they fallback to
# the docs for the typeof(value)
let d1 = @doc(DocsTest.val)
    @test d1 !== nothing
end

# DocRefs

module DocRefTests

"..."
function f end, function f! end, @enum E a b c

@doc Docs.@ref(f) g() = ()
@doc Docs.@ref(f!) g!() = ()

end

let d_1 = @doc(DocRefTests.f).meta[:results][1],
    d_2 = @doc(DocRefTests.f!).meta[:results][1],
    d_3 = @doc(DocRefTests.g).meta[:results][1],
    d_4 = @doc(DocRefTests.g!).meta[:results][1],
    d_5 = @doc(DocRefTests.E).meta[:results][1]
    @test d_1 === d_2 === d_3 === d_4 === d_5
end

# Document specific expressions generated by macro calls.
module MacroGenerated

import Base.@__doc__

macro example_1(f)
    quote
        $(f)() = 0
        @__doc__ $(f)(x) = x
        $(f)(x, y) = x + y
    end |> esc
end

"f"
@example_1 f

@example_1 _f

macro example_2(f)
    quote
        $(f)() = 0
        @__doc__ $(f)(x) = x
        @__doc__ $(f)(x, y) = x + y
    end |> esc
end

"g"
@example_2 g

@example_2 _g

end

let md = meta(MacroGenerated)[@var(MacroGenerated.f)]
    @test md.order == [Tuple{Any}]
    @test docstrings_equal(md.docs[Tuple{Any}], doc"f")
end

@test isdefined(MacroGenerated, :_f)

let md = meta(MacroGenerated)[@var(MacroGenerated.g)]
    @test md.order == [Tuple{Any}, Tuple{Any, Any}]
    @test docstrings_equal(md.docs[Tuple{Any}], doc"g")
    @test docstrings_equal(md.docs[Tuple{Any, Any}], doc"g")
end

@test isdefined(MacroGenerated, :_g)

module DocVars

immutable __FIELDS__ end

function Docs.formatdoc(buffer, docstr, ::Type{__FIELDS__})
    fields = get(docstr.data, :fields, Dict())
    if !isempty(fields)
        println(buffer, "# Fields")
        for (k, v) in sort!(collect(fields))
            println(buffer, "`", k, "` -- ", v, "\n")
        end
    end
end

"""
    $T

$__FIELDS__
"""
type T
    "x"
    x
    "y"
    y
    z
end

"""
    $S

$__FIELDS__
"""
type S
    x
    y
    z
end

end

let T = meta(DocVars)[@var(DocVars.T)],
    S = meta(DocVars)[@var(DocVars.S)]
    @test docstrings_equal(T.docs[Union{}],
        doc"""
            DocVars.T

        # Fields

        `x` -- x

        `y` -- y
        """
    )
    @test docstrings_equal(S.docs[Union{}],
        doc"""
            DocVars.S

        """
    )
end

# Issues.
# =======

# Issue #16359. Error message for invalid doc syntax.

for each in [ # valid syntax
        :(f()),
        :(f(x)),
        :(f(x::Int)),
        :(f(x...)),
        :(f(x = 1)),
        :(f(; x = 1))
    ]
    @test Meta.isexpr(Docs.docm("...", each), :block)
end
for each in [ # invalid syntax
        :(f("...")),
        :(f(1, 2)),
        :(f(() -> ()))
    ]
    result = Docs.docm("...", each)
    @test Meta.isexpr(result, :call)
    @test result.args[1] === error
end

# Issue #15424. Non-markdown docstrings.

module I15424

immutable LazyHelp
    text
end

function Base.show(io::IO, ::MIME"text/plain", h::LazyHelp)
    print(io, h.text)
end

Base.show(io::IO, h::LazyHelp) = show(io, "text/plain", h)

function Base.Docs.catdoc(hs::LazyHelp...)
    Base.Docs.Text() do io
        for h in hs
            show(io, MIME"text/plain"(), h)
        end
    end
end

Docs.docsearch(haystack::LazyHelp, needle) = Docs.docsearch(haystack.text, needle)

@doc LazyHelp("LazyHelp\n") LazyHelp
@doc LazyHelp("LazyHelp(text)\n") LazyHelp(text)

end

let d = @doc(I15424.LazyHelp)
    @test stringmime("text/plain", d) == "LazyHelp\nLazyHelp(text)\n"
end

# Issue #13385.
@test @doc(I) !== nothing

# Issue #12700.
@test docstrings_equal(@doc(DocsTest.@m), doc"Inner.@m")

# issue 11993
# Check if we are documenting the expansion of the macro
macro m1_11993()
end

macro m2_11993()
    Symbol("@m1_11993")
end

@doc "This should document @m1... since its the result of expansion" @m2_11993
@test (@doc @m1_11993) !== nothing
let d = (@doc :@m2_11993)
    @test docstring_startswith(d, doc"""
    No documentation found.

    `@m2_11993` is a macro.""")
end

@doc "Now @m2... should be documented" :@m2_11993
@test (@doc @m2_11993) !== nothing

"Document inline function"
@inline f1_11993() = nothing

@test (@doc f1_11993) !== nothing

f1_11993()

@doc "Document inline function with old syntax" ->
@inline f2_11993() = nothing

@test (@doc f2_11993) !== nothing

f2_11993()

# issue #11798

module I11798

"read"
read(x) = x

end

let md = Base.Docs.meta(I11798)[@var(I11798.read)],
    d1 = md.docs[md.order[1]],
    d2 = doc"read"
    @test docstrings_equal(d1,d2)
end

module I12515

immutable EmptyType{T} end

"A new method"
Base.collect{T}(::Type{EmptyType{T}}) = "borked"

end

let fd = meta(I12515)[@var(Base.collect)]
    @test fd.order[1] == Tuple{Type{I12515.EmptyType{TypeVar(:T, Any, true)}}}
end

# PR #12593

"$(1 + 1)"
f12593_1() = 1

"$(1 + 1) 2"
f12593_2() = 1

@test (@doc f12593_1) !== nothing
@test (@doc f12593_2) !== nothing

# @test Docs.doc(svdvals, Tuple{Vector{Float64}}) === nothing
@test Docs.doc(svdvals, Tuple{Float64}) !== nothing

# crude test to make sure we sort docstring output by method specificity
@test !docstrings_equal(Docs.doc(getindex, Tuple{Dict{Int,Int},Int}),
                        Docs.doc(getindex, Tuple{Type{Int64},Int}))

# test that macro documentation works
@test (Docs.@repl :@assert) !== nothing

@test (Docs.@repl 0) !== nothing

let t = @doc(DocsTest.t(::Int, ::Int))
    @test docstrings_equal(Docs.@repl(DocsTest.t(0, 0)), t)
    @test docstrings_equal(Docs.@repl(DocsTest.t(::Int, ::Int)), t)
end

# Issue #13467.
@test (Docs.@repl :@r_str) !== nothing

# Simple tests for apropos:
@test contains(sprint(apropos, "pearson"), "cor")
@test contains(sprint(apropos, r"ind(exes|ices)"), "eachindex")
@test contains(sprint(apropos, "print"), "Profile.print")

# Issue #13068.

module I13068

module A

export foo

"""
foo from A
"""
foo(::Int) = 1

end

module B

import ..A: foo

export foo

"""
foo from B
"""
foo(::Float64) = 2

end

end

@test docstrings_equal(
    @doc(I13068.A.foo),
    doc"""
    foo from A

    foo from B
    """
)
@test docstrings_equal(Docs.doc(I13068.A.foo, Tuple{Int}), doc"foo from A")
@test docstrings_equal(Docs.doc(I13068.A.foo, Tuple{Float64}), doc"foo from B")
@test docstrings_equal(Docs.doc(I13068.A.foo, Tuple{Char}),
    doc"""
    foo from A

    foo from B
    """
)

# Issue #13905.
@test macroexpand(:(@doc "" f() = @x)) == Expr(:error, UndefVarError(Symbol("@x")))

# Undocumented DataType Summaries.

module Undocumented

abstract A
abstract B <: A

type C <: A end

immutable D <: B
    one
    two::String
    three::Float64
end

f = () -> nothing

undocumented() = 1
undocumented(x) = 2
undocumented(x,y) = 3

end

@test docstrings_equal(@doc(Undocumented.bindingdoesnotexist), doc"""
No documentation found.

Binding `Undocumented.bindingdoesnotexist` does not exist.
""")

@test docstrings_equal(@doc(Undocumented.A), doc"""
No documentation found.

**Summary:**
```
abstract Undocumented.A <: Any
```

**Subtypes:**
```
Undocumented.B
Undocumented.C
```
""")

@test docstrings_equal(@doc(Undocumented.B), doc"""
No documentation found.

**Summary:**
```
abstract Undocumented.B <: Undocumented.A
```

**Subtypes:**
```
Undocumented.D
```
""")

@test docstrings_equal(@doc(Undocumented.C), doc"""
No documentation found.

**Summary:**
```
type Undocumented.C <: Undocumented.A
```
""")

@test docstrings_equal(@doc(Undocumented.D), doc"""
No documentation found.

**Summary:**
```
immutable Undocumented.D <: Undocumented.B
```

**Fields:**
```
one   :: Any
two   :: String
three :: Float64
```
""")

let d = @doc Undocumented.f
    io = IOBuffer()
    show(io, MIME"text/markdown"(), d)
    @test startswith(takebuf_string(io),"""
    No documentation found.

    `Undocumented.f` is a `Function`.
    """)
end

let d = @doc Undocumented.undocumented
    io = IOBuffer()
    show(io, MIME"text/markdown"(), d)
    @test startswith(takebuf_string(io), """
    No documentation found.

    `Undocumented.undocumented` is a `Function`.
    """)
end

# `@doc` "metadata".

let m = @doc(DocsTest).meta
    @test length(m[:results]) == 1
    @test m[:results][1] === Docs.meta(DocsTest)[@var(DocsTest)].docs[Union{}]
    @test m[:binding] == @var(DocsTest)
    @test m[:typesig] === Union{}
end

let m = @doc(DocsTest.f).meta
    @test length(m[:results]) == 2
    @test m[:results][1] === Docs.meta(DocsTest)[@var(DocsTest.f)].docs[Tuple{Any}]
    @test m[:results][2] === Docs.meta(DocsTest)[@var(DocsTest.f)].docs[Tuple{Any, Any}]
    @test m[:binding] == @var(DocsTest.f)
    @test m[:typesig] === Union{}
end

let m = @doc(DocsTest.f(x)).meta
    @test length(m[:results]) == 1
    @test m[:results][1] === Docs.meta(DocsTest)[@var(DocsTest.f)].docs[Tuple{Any}]
    @test m[:binding] == @var(DocsTest.f)
    @test m[:typesig] == Tuple{Any}
end

let m = @doc(Undocumented.f).meta
    @test isempty(m[:results])
    @test m[:binding] == @var(Undocumented.f)
    @test m[:typesig] === Union{}
end

# Bindings.

import Base.Docs: @var, Binding, defined

let x = Binding(Base, Symbol("@time"))
    @test defined(x) == true
    @test @var(@time) == x
    @test @var(Base.@time) == x
    @test @var(Base.Pkg.@time) == x
end

let x = Binding(Base.LinAlg, :norm)
    @test defined(x) == true
    @test @var(norm) == x
    @test @var(Base.norm) == x
    @test @var(Base.LinAlg.norm) == x
    @test @var(Base.Pkg.Dir.norm) == x
end

let x = Binding(Core, :Int)
    @test defined(x) == true
    @test @var(Int) == x
    @test @var(Base.Int) == x
    @test @var(Core.Int) == x
    @test @var(Base.Pkg.Resolve.Int) == x
end

let x = Binding(Base, :Pkg)
    @test defined(x) == true
    @test @var(Pkg) == x
    @test @var(Base.Pkg) == x
    @test @var(Main.Pkg) == x
end

let x = Binding(Base, :VERSION)
    @test defined(x) == true
    @test @var(VERSION) == x
    @test @var(Base.VERSION) == x
end

let x = Binding(Base, :bindingdoesnotexist)
    @test defined(x) == false
    @test @var(Base.bindingdoesnotexist) == x
end

let x = Binding(Main, :bindingdoesnotexist)
    @test defined(x) == false
    @test @var(bindingdoesnotexist) == x
end

# Docs.helpmode tests: we test whether the correct expressions are being generated here,
# rather than complete integration with Julia's REPL mode system.
for (line, expr) in Pair[
    "sin"          => :sin,
    "Base.sin"     => :(Base.sin),
    "@time(x)"     => :(@time(x)),
    "@time"        => :(:@time),
    ":@time"       => :(:@time),
    "@time()"      => :(@time),
    "Base.@time()" => :(Base.@time),
    "ccall"        => :ccall, # keyword
    "while       " => :while, # keyword, trailing spaces should be stripped.
    "0"            => 0,
    "\"...\""      => "...",
    "r\"...\""     => :(r"..."),
    ]
    @test Docs.helpmode(line) == :(Base.Docs.@repl($expr))
end
