# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base.Docs: meta

# Test helpers.
function docstrings_equal(d1, d2)
    io1 = IOBuffer()
    io2 = IOBuffer()
    writemime(io1, MIME"text/markdown"(), d1)
    writemime(io2, MIME"text/markdown"(), d2)
    takebuf_string(io1) == takebuf_string(io2)
end

function docstring_startswith(d1, d2)
    io1 = IOBuffer()
    io2 = IOBuffer()
    writemime(io1, MIME"text/markdown"(), d1)
    writemime(io2, MIME"text/markdown"(), d2)
    startswith(takebuf_string(io1), takebuf_string(io2))
end

@doc "Doc abstract type" ->
abstract C74685 <: AbstractArray
@test stringmime("text/plain", Docs.doc(C74685))=="Doc abstract type\n"

macro macro_doctest() end
@doc "Helps test if macros can be documented with `@doc \"...\" -> @...`." ->
:@macro_doctest

@test (@doc @macro_doctest) != nothing

# issue #11548

module ModuleMacroDoc
macro m() end
end

@doc "I am a module" ModuleMacroDoc
@doc "I am a macro"  :@ModuleMacroDoc.m

@test docstrings_equal(@doc(ModuleMacroDoc), doc"I am a module")
@test docstrings_equal(@doc(ModuleMacroDoc.@m), doc"I am a macro")

# General tests for docstrings.

module DocsTest

"DocsTest"
DocsTest

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

end

@test meta(DocsTest)[DocsTest] == doc"DocsTest"

# Check that plain docstrings store a module reference.
# https://github.com/JuliaLang/julia/pull/13017#issuecomment-138618663
@test meta(DocsTest)[DocsTest].meta[:module] == DocsTest

let f = DocsTest.f
    md = meta(DocsTest)[f]
    @test docstrings_equal(md.docs[Tuple{Any}], doc"f-1")
    @test docstrings_equal(md.docs[Tuple{Any,Any}], doc"f-2")
end

let s = DocsTest.s
    md = meta(DocsTest)[s]
    @test docstrings_equal(md.docs[Tuple{Any,}], doc"s-1")
    @test docstrings_equal(md.docs[Tuple{Any,Any}], doc"s-2")
end

let g = DocsTest.g
    md = meta(DocsTest)[g]
    @test docstrings_equal(md.docs[Union{}], doc"g")
end

let h = DocsTest.h
    md = meta(DocsTest)[h]
    sig = Union{Tuple{}, Tuple{Any}, Tuple{Any, Any}, Tuple{Any, Any, Any}}
    @test docstrings_equal(md.docs[sig], doc"h/0-3")
end

let AT = DocsTest.AT
    md = meta(DocsTest)[AT]
    @test docstrings_equal(md.docs[Union{}], doc"AT")
end

let BT = DocsTest.BT
    md = meta(DocsTest)[BT]
    @test docstrings_equal(md.docs[Union{}], doc"BT")
end

let BT2 = DocsTest.BT2
    md = meta(DocsTest)[BT2]
    @test docstrings_equal(md.docs[Union{}], doc"BT2")
end

let T = DocsTest.T
    md = meta(DocsTest)[T]
    @test docstrings_equal(md.docs[Union{}], doc"T")
    @test docstrings_equal(md.fields[:x], doc"T.x")
    @test docstrings_equal(md.fields[:y], doc"T.y")
end

let IT = DocsTest.IT
    md = meta(DocsTest)[IT]
    @test docstrings_equal(md.docs[Union{}], doc"IT")
    @test docstrings_equal(md.fields[:x], doc"IT.x")
    @test docstrings_equal(md.fields[:y], doc"IT.y")
end

@test @doc(DocsTest.TA) == doc"TA"

@test docstrings_equal(@doc(DocsTest.@mac), doc"@mac()")
@test docstrings_equal(@doc(DocsTest.@mac()), doc"@mac()")
@test docstrings_equal(@doc(DocsTest.@mac(x)), doc"@mac(x)")
@test docstrings_equal(@doc(DocsTest.@mac(x::Int, y::Expr)), doc"@mac(x::Int, y::Expr, z = 0)")
@test docstrings_equal(@doc(DocsTest.@mac(x::Int, y::Expr, z)), doc"@mac(x::Int, y::Expr, z = 0)")
let m = doc"""
        @mac(x)

        @mac(x::Int, y::Expr, z = 0)

        @mac()

        :@mac
        """
    @test docstrings_equal(@doc(:@DocsTest.mac), m)
    @test docstrings_equal(@doc(:(DocsTest.@mac)), m)
end

@test @doc(DocsTest.G) == doc"G"
@test @doc(DocsTest.K) == doc"K"

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

let fields = meta(DocsTest)[DocsTest.FieldDocs].fields
    @test haskey(fields, :one) && fields[:one] == doc"one"
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

let md = meta(MacroGenerated)[MacroGenerated.f]
    @test md.order == [Tuple{Any}]
    @test md.docs[Tuple{Any}] == doc"f"
end

@test isdefined(MacroGenerated, :_f)

let md = meta(MacroGenerated)[MacroGenerated.g]
    @test md.order == [Tuple{Any}, Tuple{Any, Any}]
    @test md.docs[Tuple{Any}] == doc"g"
    @test md.docs[Tuple{Any, Any}] == doc"g"
end

@test isdefined(MacroGenerated, :_g)

# Issue #13385.
@test @doc(I) !== nothing

# Issue #12700.
@test docstrings_equal(@doc(DocsTest.@m), doc"Inner.@m")

# issue 11993
# Check if we are documenting the expansion of the macro
macro m1_11993()
end

macro m2_11993()
    symbol("@m1_11993")
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

let md = Base.Docs.meta(I11798)[I11798.read],
    d1 = md.docs[md.order[1]],
    d2 = doc"read"
    @test docstrings_equal(d1,d2)
end

module I12515

immutable EmptyType{T} end

"A new method"
Base.collect{T}(::Type{EmptyType{T}}) = "borked"

end

let fd = meta(I12515)[Base.collect]
    @test fd.order[1] == Tuple{Type{I12515.EmptyType{TypeVar(:T, Any, true)}}}
end

# PR #12593

"$(1 + 1)"
f12593_1() = 1

"$(1 + 1) 2"
f12593_2() = 1

@test (@doc f12593_1) !== nothing
@test (@doc f12593_2) !== nothing

@test Docs.doc(svdvals, Tuple{Vector{Float64}}) === nothing
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
@test Docs.doc(I13068.A.foo, Tuple{Char}) === nothing

# Issue #13905.
@test macroexpand(:(@doc "" f() = @x)) == Expr(:error, UndefVarError(symbol("@x")))

# Undocumented DataType Summaries.

module Undocumented

abstract A
abstract B <: A

type C <: A end

immutable D <: B
    one
    two::UTF8String
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
```julia
abstract Undocumented.A <: Any
```

**Subtypes:**
```julia
Undocumented.B
Undocumented.C
```
""")

@test docstrings_equal(@doc(Undocumented.B), doc"""
No documentation found.

**Summary:**
```julia
abstract Undocumented.B <: Undocumented.A
```

**Subtypes:**
```julia
Undocumented.D
```
""")

@test docstrings_equal(@doc(Undocumented.C), doc"""
No documentation found.

**Summary:**
```julia
type Undocumented.C <: Undocumented.A
```
""")

@test docstrings_equal(@doc(Undocumented.D), doc"""
No documentation found.

**Summary:**
```julia
immutable Undocumented.D <: Undocumented.B
```

**Fields:**
```julia
one   :: Any
two   :: UTF8String
three :: Float64
```
""")

let d = @doc Undocumented.f
    io = IOBuffer()
    writemime(io, MIME"text/markdown"(), d)
    @test startswith(takebuf_string(io),"""
    No documentation found.

    `Undocumented.f` is a `Function`.
    """)
end

let d = @doc Undocumented.undocumented
    io = IOBuffer()
    writemime(io, MIME"text/markdown"(), d)
    @test startswith(takebuf_string(io), """
    No documentation found.

    `Undocumented.undocumented` is a `Function`.
    """)
end


# Bindings.

import Base.Docs: @var, Binding

let x = Binding(Base, symbol("@time"))
    @test x.defined == true
    @test @var(@time) == x
    @test @var(Base.@time) == x
    @test @var(Base.Pkg.@time) == x
end

let x = Binding(Base.LinAlg, :norm)
    @test x.defined == true
    @test @var(norm) == x
    @test @var(Base.norm) == x
    @test @var(Base.LinAlg.norm) == x
    @test @var(Base.Pkg.Dir.norm) == x
end

let x = Binding(Core, :Int)
    @test x.defined == true
    @test @var(Int) == x
    @test @var(Base.Int) == x
    @test @var(Core.Int) == x
    @test @var(Base.Pkg.Resolve.Int) == x
end

let x = Binding(Base, :Pkg)
    @test x.defined == true
    @test @var(Pkg) == x
    @test @var(Base.Pkg) == x
    @test @var(Main.Pkg) == x
end

let x = Binding(Base, :VERSION)
    @test x.defined == true
    @test @var(VERSION) == x
    @test @var(Base.VERSION) == x
end

let x = Binding(Base, :bindingdoesnotexist)
    @test x.defined == false
    @test @var(Base.bindingdoesnotexist) == x
end

let x = Binding(Main, :bindingdoesnotexist)
    @test x.defined == false
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
