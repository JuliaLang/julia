# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base.Docs: meta, @var, DocStr, parsedoc

using Markdown
using REPL

using REPL: @repl, repl_latex, _repl, accessible
using InteractiveUtils: apropos

# For curmod_*
include("testenv.jl")

# Test helpers.
function docstrings_equal(d1, d2)
    io1 = IOBuffer()
    io2 = IOBuffer()
    show(io1, MIME"text/markdown"(), d1)
    show(io2, MIME"text/markdown"(), d2)
    String(take!(io1)) == String(take!(io2))
end
docstrings_equal(d1::DocStr, d2) = docstrings_equal(parsedoc(d1), d2)

function docstring_startswith(d1, d2)
    io1 = IOBuffer()
    io2 = IOBuffer()
    show(io1, MIME"text/markdown"(), d1)
    show(io2, MIME"text/markdown"(), d2)
    startswith(String(take!(io1)), String(take!(io2)))
end
docstring_startswith(d1::DocStr, d2) = docstring_startswith(parsedoc(d1), d2)

@doc "Doc abstract type"
abstract type C74685{T,N} <: AbstractArray{T,N} end
@test repr("text/plain", Docs.doc(C74685))=="  Doc abstract type\n"
@test string(Docs.doc(C74685))=="Doc abstract type\n"

macro macro_doctest() end
@doc "Helps test if macros can be documented with `@doc \"...\" @...`."
:@macro_doctest

@test (@doc @macro_doctest) !== nothing

# test that random stuff interpolated into docstrings doesn't break search or other methods here
@doc doc"""
break me:

    code

$:asymbol # a symbol
$1 # a number
$string # a function
$$latex literal$$
### header!
"""
function break_me_docs end

# issue #11548

module ModuleMacroDoc
macro m() end
end

@doc "I am a module" ModuleMacroDoc
@doc "I am a macro"  :@ModuleMacroDoc.m

@test docstrings_equal(@doc(ModuleMacroDoc), doc"I am a module")
@test docstrings_equal(@doc(ModuleMacroDoc.@m), doc"I am a macro")

# General tests for docstrings.

const LINE_NUMBER = @__LINE__() + 1
"DocsTest"
module DocsTest

using Markdown

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
abstract type AT end

"BT"
primitive type BT 8 end

"BT2"
primitive type BT2 <: Integer 8 end

"T"
mutable struct T <: AT
    "T.x"
    x
    "T.y"
    y :: Int
end

"IT"
struct IT
    "IT.x"
    x :: Int
    "IT.y"
    y
end

"TA"
const TA = Union{T, IT}

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
t(x::S) where {S <: Integer} = x

"t-1"
t(::AbstractString)
"t-2"
t(::Int, ::Any)
"t-3"
t{S <: Integer}(::S)

"FieldDocs"
mutable struct FieldDocs
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

mutable struct Foo
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
abstract type A end

"T"
mutable struct T
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

struct __FIELDS__ end

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
mutable struct T
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
mutable struct S
    x
    y
    z
end

end

let T = meta(DocVars)[@var(DocVars.T)],
    S = meta(DocVars)[@var(DocVars.S)],
    Tname = Markdown.parse("```\n$(curmod_prefix)DocVars.T\n```"),
    Sname = Markdown.parse("```\n$(curmod_prefix)DocVars.S\n```")
    # Splicing the expression directly doesn't work
    @test docstrings_equal(T.docs[Union{}],
        doc"""
        $Tname

        # Fields

        `x` -- x

        `y` -- y
        """
    )
    @test docstrings_equal(S.docs[Union{}],
        doc"""
        $Sname

        """
    )
end

# Issues.
# =======

# Issue #16359. Error message for invalid doc syntax.

let __source__ = LineNumberNode(0),
    __module__ = @__MODULE__
    for each in [ # valid syntax
            :(f()),
            :(f(x)),
            :(f(x::Int)),
            :(f(x...)),
            :(f(x = 1)),
            :(f(; x = 1))
        ]
        @test Meta.isexpr(Docs.docm(__source__, __module__, "...", each), :block)
    end
    for each in [ # invalid syntax
            :(f("...")),
            :(f(1, 2)),
            :(f(() -> ()))
        ]
        result = Docs.docm(__source__, __module__, "...", each)
        @test Meta.isexpr(result, :call)
        @test result.args[1] === error
    end
end

# Issue #15424. Non-markdown docstrings.

module I15424

using REPL

struct LazyHelp
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

REPL.docsearch(haystack::LazyHelp, needle) = REPL.docsearch(haystack.text, needle)

@doc LazyHelp("LazyHelp\n") LazyHelp
@doc LazyHelp("LazyHelp(text)\n") LazyHelp(text)

end

let d = @doc(I15424.LazyHelp)
    @test repr("text/plain", d) == "LazyHelp\nLazyHelp(text)\n"
end

# Issue #13385.
struct I13385
    λ
end
"issue #13385"
const i13385 = I13385(true)
@test @doc(i13385) !== nothing

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
let d = (@doc :@m2_11993),
    macro_doc = Markdown.parse("`$(curmod_prefix)@m2_11993` is a macro.")
    @test docstring_startswith(d, doc"""
    No documentation found.

    $macro_doc""")
end

@doc "Now @m2... should be documented" :@m2_11993
@test (@doc @m2_11993) !== nothing

"Document inline function"
@inline f1_11993() = nothing

@test (@doc f1_11993) !== nothing

f1_11993()

@doc "Document inline function with old syntax"
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

struct EmptyType{T} end

"A new method"
Base.collect(::Type{EmptyType{T}}) where {T} = "borked"

end

let fd = meta(I12515)[@var(Base.collect)]
    @test fd.order[1] == (Union{Tuple{Type{I12515.EmptyType{T}}}, Tuple{T}} where T)
end

# PR #12593

"$(1 + 1)"
f12593_1() = 1

"$(1 + 1) 2"
f12593_2() = 1

@test (@doc f12593_1) !== nothing
@test (@doc f12593_2) !== nothing

# @test Docs.doc(svdvals, Tuple{Vector{Float64}}) === nothing
# @test Docs.doc(svdvals, Tuple{Float64}) !== nothing

# crude test to make sure we sort docstring output by method specificity
@test !docstrings_equal(Docs.doc(getindex, Tuple{Dict{Int,Int},Int}),
                        Docs.doc(getindex, Tuple{Type{Int64},Int}))

# test that macro documentation works
@test (@repl :@assert) !== nothing

@test (@repl 0) !== nothing

let t = @doc(DocsTest.t(::Int, ::Int))
    @test docstrings_equal(@repl(DocsTest.t(0, 0)), t)
    @test docstrings_equal(@repl(DocsTest.t(::Int, ::Int)), t)
end

# Issue #13467.
@test (@repl :@r_str) !== nothing

# Simple tests for apropos:
@test occursin("cor", sprint(apropos, "pearson"))
@test occursin("eachindex", sprint(apropos, r"ind(exes|ices)"))
using Profile
@test occursin("Profile.print", sprint(apropos, "print"))

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
let err = try; @macroexpand(@doc "" f() = @x); false; catch ex; ex; end
    __source__ = LineNumberNode(@__LINE__() -  1, Symbol(@__FILE__))
    err::LoadError
    @test err.file === string(__source__.file)
    @test err.line === __source__.line
    err = err.error::LoadError
    @test err.file === string(__source__.file)
    @test err.line === __source__.line
    err = err.error::UndefVarError
    @test err.var == Symbol("@x")
 end


# Undocumented DataType Summaries.

module Undocumented

abstract type A end
abstract type B <: A end

mutable struct C <: A end

struct D <: B
    one
    two::String
    three::Float64
end

f = () -> nothing

undocumented() = 1
undocumented(x) = 2
undocumented(x,y) = 3

end

doc_str = Markdown.parse("""
No documentation found.

Binding `$(curmod_prefix)Undocumented.bindingdoesnotexist` does not exist.
""")
@test docstrings_equal(@doc(Undocumented.bindingdoesnotexist), doc"$doc_str")

doc_str = Markdown.parse("""
No documentation found.

# Summary
```
abstract type $(curmod_prefix)Undocumented.A <: Any
```

# Subtypes
```
$(curmod_prefix)Undocumented.B
$(curmod_prefix)Undocumented.C
```
""")
@test docstrings_equal(@doc(Undocumented.A), doc"$doc_str")

doc_str = Markdown.parse("""
No documentation found.

# Summary
```
abstract type $(curmod_prefix)Undocumented.B <: $(curmod_prefix)Undocumented.A
```

# Subtypes
```
$(curmod_prefix)Undocumented.D
```

# Supertype Hierarchy
```
$(curmod_prefix)Undocumented.B <: $(curmod_prefix)Undocumented.A <: Any
```
""")
@test docstrings_equal(@doc(Undocumented.B), doc"$doc_str")

doc_str = Markdown.parse("""
No documentation found.

# Summary
```
mutable struct $(curmod_prefix)Undocumented.C <: $(curmod_prefix)Undocumented.A
```

# Supertype Hierarchy
```
$(curmod_prefix)Undocumented.C <: $(curmod_prefix)Undocumented.A <: Any
```
""")
@test docstrings_equal(@doc(Undocumented.C), doc"$doc_str")

doc_str = Markdown.parse("""
No documentation found.

# Summary
```
struct $(curmod_prefix)Undocumented.D <: $(curmod_prefix)Undocumented.B
```

# Fields
```
one   :: Any
two   :: String
three :: Float64
```

# Supertype Hierarchy
```
$(curmod_prefix)Undocumented.D <: $(curmod_prefix)Undocumented.B <: $(curmod_prefix)Undocumented.A <: Any
```
""")
@test docstrings_equal(@doc(Undocumented.D), doc"$doc_str")

let d = @doc(Undocumented.f)
    io = IOBuffer()
    show(io, MIME"text/markdown"(), d)
    @test startswith(String(take!(io)),"""
    No documentation found.

    `$(curmod_prefix)Undocumented.f` is a `Function`.
    """)
end

let d = @doc(Undocumented.undocumented)
    io = IOBuffer()
    show(io, MIME"text/markdown"(), d)
    @test startswith(String(take!(io)), """
    No documentation found.

    `$(curmod_prefix)Undocumented.undocumented` is a `Function`.
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
    @test @var(Base.Iterators.@time) == x
end

let x = Binding(Iterators, :enumerate)
    @test defined(x) == true
    @test @var(enumerate) == x
    @test @var(Base.enumerate) == x
    @test @var(Iterators.enumerate) == x
    @test @var(Base.Iterators.enumerate) == x
end

let x = Binding(Core, :Int)
    @test defined(x) == true
    @test @var(Int) == x
    @test @var(Base.Int) == x
    @test @var(Core.Int) == x
end

let x = Binding(Base, :Iterators)
    @test defined(x) == true
    @test @var(Iterators) == x
    @test @var(Base.Iterators) == x
    @test @var(Main.Iterators) == x
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

let x = Binding(curmod, :bindingdoesnotexist)
    @test defined(x) == false
    @test @var(bindingdoesnotexist) == x
end

let x = Binding(Main, :+)
    @test Meta.parse(string(x)) == :(Base.:+)
end

let x = Binding(Meta, :parse)
    @test Meta.parse(string(x)) == :(Base.Meta.parse)
end

let x = Binding(Main, :⊕)
    @test Meta.parse(string(x)) == :(⊕)
end

doc_util_path = Symbol(joinpath("docs", "utils.jl"))

@test sprint(repl_latex, "√") == "\"√\" can be typed by \\sqrt<tab>\n\n"
@test sprint(repl_latex, "x̂₂") == "\"x̂₂\" can be typed by x\\hat<tab>\\_2<tab>\n\n"

# issue #15684
begin
    """
    abc
    """
    f15684(x) = 1
end

@test string(@doc f15684) == "abc\n"

# Dynamic docstrings

mutable struct DynamicDocType
    x
end

Base.Docs.getdoc(d::DynamicDocType, sig) = "$(d.x) $(sig)"

dynamic_test = DynamicDocType("test 1")
@test @doc(dynamic_test) == "test 1 Union{}"
dynamic_test.x = "test 2"
@test @doc(dynamic_test) == "test 2 Union{}"
@test @doc(dynamic_test(::String)) == "test 2 Tuple{String}"

let dt1 = _repl(:(dynamic_test(1.0)))
    @test dt1 isa Expr
    @test dt1.args[1] isa Expr
    @test dt1.args[1].head === :macrocall
    @test dt1.args[1].args[1] == Symbol("@doc")
    @test dt1.args[1].args[3] == :(dynamic_test(::typeof(1.0)))
end
let dt2 = _repl(:(dynamic_test(::String)))
    @test dt2 isa Expr
    @test dt2.args[1] isa Expr
    @test dt2.args[1].head === :macrocall
    @test dt2.args[1].args[1] == Symbol("@doc")
    @test dt2.args[1].args[3] == :(dynamic_test(::String))
end

# Equality testing

@test Text("docstring") == Text("docstring")
@test hash(Text("docstring")) == hash(Text("docstring"))
@test HTML("<b>docstring</b>") == HTML("<b>docstring</b>")
@test Text("docstring1") ≠ Text("docstring2")
@test hash(Text("docstring1")) ≠ hash(Text("docstring2"))
@test hash(Text("docstring")) ≠ hash(HTML("docstring"))

# issue #25172
@test repr(MIME"text/html"(), HTML("a","b")) == "ab"

# issue 21016
module I21016

struct Struct{T}
end

"String 1"
function Struct{T}(arg1) where T<:Float64
end

"String 2"
function Struct{T}(arg1) where T
end

"String 3"
function Struct{T}(arg1) where Integer <: T <: Real
end

"String 4"
function Struct{T}(arg1) where T >: Int
end

end

@test docstrings_equal(
    @doc(I21016.Struct),
    doc"""
    String 1

    String 2

    String 3

    String 4
    """
)

# issue #22105
module I22105
    lineno = @__LINE__
    """foo docs"""
    function foo end
end

let foo_docs = meta(I22105)[@var(I22105.foo)].docs
    @test length(foo_docs) === 1
    @test isa(first(foo_docs), Pair)
    local docstr = first(foo_docs).second
    @test isa(docstr, DocStr)
    @test docstr.data[:path] == Base.source_path()
    @test docstr.data[:linenumber] == I22105.lineno + 1
    @test docstr.data[:module] === I22105
    @test docstr.data[:typesig] === Union{}
    @test docstr.data[:binding] == Binding(I22105, :foo)
end

# issue #23011
@test_nowarn @eval Main begin
    @doc "first" f23011() = 1
    @doc "second" f23011() = 2
end
@test Main.f23011() == 2
@test docstrings_equal(@doc(Main.f23011), doc"second")

# issue 22098
"an empty macro"
macro mdoc22098 end
@test docstrings_equal(@doc(:@mdoc22098), doc"an empty macro")

# issue #24468
let ex = try
    include_string(@__MODULE__, """

    \"\"\"
    an example
    \"\"\"
    function hello(param::Vector{In64_nOt_DeFiNeD__})
    end
    """)
catch e
    e
end
    @test ex.line == 2
end

struct t_docs_abc end
@test "t_docs_abc" in accessible(@__MODULE__)

# Call overloading issue #20087
"""
Docs for `MyFunc` struct.
"""
mutable struct MyFunc
    x
end

"""
Docs for calling `f::MyFunc`.
"""
function (f::MyFunc)(x)
    f.x = x
    return f
end

@test docstrings_equal(@doc(MyFunc(2)),
doc"""
Docs for calling `f::MyFunc`.
""")

struct A_20087 end

"""a"""
(a::A_20087)() = a

@test docstrings_equal(@doc(A_20087()), doc"a")
