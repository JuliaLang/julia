# This file is a part of Julia. License is MIT: http://julialang.org/license

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

@doc ("I am a module";) ModuleMacroDoc
@doc ("I am a macro";)  :@ModuleMacroDoc.m

@test (@doc ModuleMacroDoc)    == "I am a module"
@test (@doc ModuleMacroDoc.@m) == "I am a macro"

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

"@mac"
macro mac() end

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

end

import Base.Docs: meta

function docstrings_equal(d1, d2)
    io1 = IOBuffer()
    io2 = IOBuffer()
    writemime(io1, MIME"text/markdown"(), d1)
    writemime(io2, MIME"text/markdown"(), d2)
    takebuf_string(io1) == takebuf_string(io2)
end

@test meta(DocsTest)[DocsTest] == doc"DocsTest"

let f = DocsTest.f
    funcdoc = meta(DocsTest)[f]
    @test funcdoc.main == nothing
    @test docstrings_equal(funcdoc.meta[Tuple{Any}], doc"f-1")
    @test docstrings_equal(funcdoc.meta[Tuple{Any,Any}], doc"f-2")
end

let s = DocsTest.s
    funcdoc = meta(DocsTest)[s]
    @test funcdoc.main == nothing
    @test docstrings_equal(funcdoc.meta[Tuple{Any,}], doc"s-1")
    @test docstrings_equal(funcdoc.meta[Tuple{Any,Any}], doc"s-2")
end

let g = DocsTest.g
    funcdoc = meta(DocsTest)[g]
    @test docstrings_equal(funcdoc.meta[Union{}], doc"g")
end

let AT = DocsTest.AT
    @test meta(DocsTest)[AT] == doc"AT"
end

let BT = DocsTest.BT
    @test meta(DocsTest)[BT] == doc"BT"
end

let T = DocsTest.T
    typedoc = meta(DocsTest)[T]
    @test docstrings_equal(typedoc.main, doc"T")
    @test docstrings_equal(typedoc.fields[:x], doc"T.x")
    @test docstrings_equal(typedoc.fields[:y], doc"T.y")
end

let IT = DocsTest.IT
    typedoc = meta(DocsTest)[IT]
    @test docstrings_equal(typedoc.main, doc"IT")
    @test docstrings_equal(typedoc.fields[:x], doc"IT.x")
    @test docstrings_equal(typedoc.fields[:y], doc"IT.y")
end

@test @doc(DocsTest.TA) == doc"TA"

@test @doc(DocsTest.@mac) == doc"@mac"

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

# issue 11993
# Check if we are documenting the expansion of the macro
macro m1_11993()
end

macro m2_11993()
    symbol("@m1_11993")
end

@doc "This should document @m1... since its the result of expansion" @m2_11993
@test (@doc @m1_11993) !== nothing
@test (@doc @m2_11993) === nothing

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

let fd = Base.Docs.meta(I11798)[I11798.read],
    d1 = fd.meta[fd.order[1]],
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
