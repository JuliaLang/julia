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
@test (@doc ModuleMacroDoc.@m) == ["I am a macro"]

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
typealias TA Union(T, IT)

"@mac"
macro mac() end

"G"
G = :G

"K"
const K = :K

end

import Base.Docs: meta

@test meta(DocsTest)[DocsTest] == doc"DocsTest"

let f = DocsTest.f
    funcdoc = meta(DocsTest)[f]
    order = [methods(f, sig)[1] for sig in [(Any,), (Any, Any)]]
    @test funcdoc.main == nothing
    @test funcdoc.order == order
    @test funcdoc.meta[order[1]] == doc"f-1"
    @test funcdoc.meta[order[2]] == doc"f-2"
end

let g = DocsTest.g
    funcdoc = meta(DocsTest)[g]
    @test funcdoc.main == doc"g"
end

let AT = DocsTest.AT
    @test meta(DocsTest)[AT] == doc"AT"
end

let BT = DocsTest.BT
    @test meta(DocsTest)[BT] == doc"BT"
end

let T = DocsTest.T
    typedoc = meta(DocsTest)[T]
    @test typedoc.main == doc"T"
    @test typedoc.fields[:x] == doc"T.x"
    @test typedoc.fields[:y] == doc"T.y"
end

let IT = DocsTest.IT
    typedoc = meta(DocsTest)[IT]
    @test typedoc.main == doc"IT"
    @test typedoc.fields[:x] == doc"IT.x"
    @test typedoc.fields[:y] == doc"IT.y"
end

let TA = DocsTest.TA
    @test meta(DocsTest)[TA] == doc"TA"
end

let mac = getfield(DocsTest, symbol("@mac"))
    funcdoc = meta(DocsTest)[mac]
    @test funcdoc.main == doc"@mac"
end

@test meta(DocsTest)[:G] == doc"G"
@test meta(DocsTest)[:K] == doc"K"

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

let fd = meta(I11798)[I11798.read]
    @test fd.order[1] == which(I11798.read, Tuple{Any})
    @test fd.meta[fd.order[1]] == doc"read"
end
