# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, InteractiveUtils

# test methodswith
# `methodswith` relies on exported symbols
export func4union, Base
struct NoMethodHasThisType end
@test isempty(methodswith(NoMethodHasThisType))
@test !isempty(methodswith(Int))
struct Type4Union end
func4union(::Union{Type4Union,Int}) = ()
@test !isempty(methodswith(Type4Union, @__MODULE__))

# PR #19964
@test isempty(subtypes(Float64))

# Issue #20086
abstract type A20086{T,N} end
struct B20086{T,N} <: A20086{T,N} end
@test subtypes(A20086) == [B20086]
@test subtypes(A20086{Int}) == [B20086{Int}]
@test subtypes(A20086{T,3} where T) == [B20086{T,3} where T]
@test subtypes(A20086{Int,3}) == [B20086{Int,3}]

# code_warntype
module WarnType
using Test, Random, InteractiveUtils

function warntype_hastag(f, types, tag)
    iob = IOBuffer()
    code_warntype(iob, f, types)
    str = String(take!(iob))
    return occursin(tag, str)
end

pos_stable(x) = x > 0 ? x : zero(x)
pos_unstable(x) = x > 0 ? x : 0

tag = "UNION"
@test warntype_hastag(pos_unstable, Tuple{Float64}, tag)
@test !warntype_hastag(pos_stable, Tuple{Float64}, tag)

mutable struct Stable{T,N}
    A::Array{T,N}
end
mutable struct Unstable{T}
    A::Array{T}
end
Base.getindex(A::Stable, i) = A.A[i]
Base.getindex(A::Unstable, i) = A.A[i]

tag = "ARRAY{FLOAT64,N}"
@test warntype_hastag(getindex, Tuple{Unstable{Float64},Int}, tag)
@test !warntype_hastag(getindex, Tuple{Stable{Float64,2},Int}, tag)
@test warntype_hastag(getindex, Tuple{Stable{Float64},Int}, tag)

# Make sure emphasis is not used for other functions
tag = "ANY"
iob = IOBuffer()
show(iob, Meta.lower(Main, :(x -> x^2)))
str = String(take!(iob))
@test !occursin(tag, str)

# Make sure non used variables are not emphasized
has_unused() = (a = rand(5))
@test !warntype_hastag(has_unused, Tuple{}, tag)
# No variable information with the new optimizer. Eventually might be able to recover
# some of this info with debug info.
#@test warntype_hastag(has_unused, Tuple{}, "<optimized out>")

# Make sure that "expected" unions are highlighted with warning color instead of error color
iob = IOBuffer()
code_warntype(IOContext(iob, :color => true), x -> (x > 1 ? "foo" : nothing), Tuple{Int64})
str = String(take!(iob))
@test occursin(Base.text_colors[Base.warn_color()], str)

# Make sure getproperty and setproperty! works with @code_... macros
struct T1234321
    t::Int
end
Base.getproperty(t::T1234321, ::Symbol) = "foo"
@test (@code_typed T1234321(1).f).second == String
Base.setproperty!(t::T1234321, ::Symbol, ::Symbol) = "foo"
@test (@code_typed T1234321(1).f = :foo).second == String

module ImportIntrinsics15819
# Make sure changing the lookup path of an intrinsic doesn't break
# the heuristic for type instability warning.
import Core.Intrinsics: sqrt_llvm, bitcast
# Use import
sqrt15819(x::Float64) = bitcast(Float64, sqrt_llvm(x))
# Use fully qualified name
sqrt15819(x::Float32) = bitcast(Float32, Core.Intrinsics.sqrt_llvm(x))
end # module ImportIntrinsics15819

foo11122(x) = @fastmath x - 1.0

# issue #11122, #13568 and #15819
@test !warntype_hastag(+, Tuple{Int,Int}, tag)
@test !warntype_hastag(-, Tuple{Int,Int}, tag)
@test !warntype_hastag(*, Tuple{Int,Int}, tag)
@test !warntype_hastag(/, Tuple{Int,Int}, tag)
@test !warntype_hastag(foo11122, Tuple{Float32}, tag)
@test !warntype_hastag(foo11122, Tuple{Float64}, tag)
@test !warntype_hastag(foo11122, Tuple{Int}, tag)
@test !warntype_hastag(sqrt, Tuple{Int}, tag)
@test !warntype_hastag(sqrt, Tuple{Float64}, tag)
@test !warntype_hastag(^, Tuple{Float64,Int32}, tag)
@test !warntype_hastag(^, Tuple{Float32,Int32}, tag)
@test !warntype_hastag(ImportIntrinsics15819.sqrt15819, Tuple{Float64}, tag)
@test !warntype_hastag(ImportIntrinsics15819.sqrt15819, Tuple{Float32}, tag)

end # module WarnType

# Adds test for PR #17636
let a = @code_typed 1 + 1
    b = @code_lowered 1 + 1
    @test isa(a, Pair{Core.CodeInfo, DataType})
    @test isa(b, Core.CodeInfo)
    @test isa(a[1].code, Array{Any,1})
    @test isa(b.code, Array{Any,1})

    function thing(a::Array, b::Real)
        println("thing")
    end
    function thing(a::AbstractArray, b::Int)
        println("blah")
    end
    @test_throws MethodError thing(rand(10), 1)
    a = @code_typed thing(rand(10), 1)
    b = @code_lowered thing(rand(10), 1)
    @test length(a) == 0
    @test length(b) == 0
end

# Issue #16326
mktemp() do f, io
    OLDSTDOUT = stdout
    redirect_stdout(io)
    @test try @code_native map(abs, rand(3)); true; catch; false; end
    redirect_stdout(OLDSTDOUT)
    nothing
end

module _test_varinfo_
export x
x = 1.0
end
@test repr(varinfo(Main, r"^$")) == """
| name | size | summary |
|:---- | ----:|:------- |
"""
let v = repr(varinfo(_test_varinfo_))
    @test occursin("| x              |   8 bytes | Float64 |", v)
end

# Issue 14173
module Tmp14173
    using Random
    export A
    A = randn(2000, 2000)
end
varinfo(Tmp14173) # warm up
const MEMDEBUG = ccall(:jl_is_memdebug, Bool, ())
@test @allocated(varinfo(Tmp14173)) < (MEMDEBUG ? 60000 : 20000)

# PR #24997: test that `varinfo` doesn't fail when encountering `missing`
module A
    using InteractiveUtils
    export missing
    varinfo(A)
end

# PR #23075
@testset "versioninfo" begin
    # check that versioninfo(io; verbose=true) doesn't error, produces some output
    mktempdir() do dir
        buf = PipeBuffer()
        versioninfo(buf, verbose=true)
        ver = read(buf, String)
        @test startswith(ver, "Julia Version $VERSION")
        @test occursin("Environment:", ver)
    end
    let exename = `$(Base.julia_cmd()) --startup-file=no`
        @test !occursin("Environment:", read(setenv(`$exename -e 'using InteractiveUtils; versioninfo()'`,
                                                    String[]), String))
        @test  occursin("Environment:", read(setenv(`$exename -e 'using InteractiveUtils; versioninfo()'`,
                                                    String["JULIA_CPU_THREADS=1"]), String))
    end
end

const curmod = @__MODULE__
const curmod_name = fullname(curmod)
const curmod_str = curmod === Main ? "Main" : join(curmod_name, ".")

@test_throws ErrorException("\"this_is_not_defined\" is not defined in module $curmod_str") @which this_is_not_defined
# issue #13264
@test (@which vcat(1...)).name == :vcat

# PR #28122, issue #25474
@test (@which [1][1]).name === :getindex
@test (@which [1][1] = 2).name === :setindex!
@test (@which [1]).name === :vect
@test (@which [1 2]).name === :hcat
@test (@which [1; 2]).name === :vcat
@test (@which Int[1 2]).name === :typed_hcat
@test (@which Int[1; 2]).name === :typed_vcat
@test (@which [1 2;3 4]).name === :hvcat
@test (@which Int[1 2;3 4]).name === :typed_hvcat

# issue #13464
try
    @which x = 1
    error("unexpected")
catch err13464
    @test startswith(err13464.msg, "expression is not a function call, or is too complex")
end

module MacroTest
export @macrotest
macro macrotest(x::Int, y::Symbol) end
macro macrotest(x::Int, y::Int)
    nothing #This is here because of #15280
end
end

let
    using .MacroTest
    a = 1
    m = getfield(@__MODULE__, Symbol("@macrotest"))
    @test which(m, Tuple{LineNumberNode, Module, Int, Symbol}) == @which @macrotest 1 a
    @test which(m, Tuple{LineNumberNode, Module, Int, Int}) == @which @macrotest 1 1

    @test first(methods(m, Tuple{LineNumberNode, Module, Int, Int})) == @which MacroTest.@macrotest 1 1
    @test functionloc(@which @macrotest 1 1) == @functionloc @macrotest 1 1
end

mutable struct A18434
end
A18434(x; y=1) = 1

global counter18434 = 0
function get_A18434()
    global counter18434
    counter18434 += 1
    return A18434
end
@which get_A18434()(1; y=2)
@test counter18434 == 1
@which get_A18434()(1, y=2)
@test counter18434 == 2

@eval function f_invalid(x)
    Base.@_noinline_meta
    $(Expr(:loopinfo, 1.0f0)) # some expression that throws an error in codegen
    x
end

let _true = Ref(true), g, h
    @noinline g() = _true[] ? 0 : h()
    @noinline h() = (g(); f_invalid(_true[]))
    @test_throws ErrorException @code_native h() # due to a failure to compile f()
    @test g() == 0
end

let _true = Ref(true), f, g, h
    @noinline f() = ccall((:time, "error_library_doesnt_exist\0"), Cvoid, ()) # should throw error during runtime
    @noinline g() = _true[] ? 0 : h()
    @noinline h() = (g(); f())
    @test g() == 0
    @test_throws ErrorException h()
end

module ReflectionTest
using Test, Random, InteractiveUtils

function test_ast_reflection(freflect, f, types)
    @test !isempty(freflect(f, types))
    nothing
end

function test_bin_reflection(freflect, f, types)
    iob = IOBuffer()
    freflect(iob, f, types)
    str = String(take!(iob))
    @test !isempty(str)
    nothing
end

function test_code_reflection(freflect, f, types, tester)
    tester(freflect, f, types)
    tester(freflect, f, (types.parameters...,))
    nothing
end

function test_code_reflections(tester, freflect)
    test_code_reflection(freflect, occursin,
                         Tuple{Regex, AbstractString}, tester) # abstract type
    test_code_reflection(freflect, +, Tuple{Int, Int}, tester) # leaftype signature
    test_code_reflection(freflect, +,
                         Tuple{Array{Float32}, Array{Float32}}, tester) # incomplete types
    test_code_reflection(freflect, Module, Tuple{}, tester) # Module() constructor (transforms to call)
    test_code_reflection(freflect, Array{Int64}, Tuple{Array{Int32}}, tester) # with incomplete types
    test_code_reflection(freflect, muladd, Tuple{Float64, Float64, Float64}, tester)
end

test_code_reflections(test_bin_reflection, code_llvm)
test_code_reflections(test_bin_reflection, code_native)

end # module ReflectionTest

@test_throws ArgumentError("argument is not a generic function") code_llvm(===, Tuple{Int, Int})
@test_throws ArgumentError("argument is not a generic function") code_native(===, Tuple{Int, Int})

# Issue #18883, code_llvm/code_native for generated functions
@generated f18883() = nothing
@test !isempty(sprint(code_llvm, f18883, Tuple{}))
@test !isempty(sprint(code_native, f18883, Tuple{}))

ix86 = r"i[356]86"

if Sys.ARCH === :x86_64 || occursin(ix86, string(Sys.ARCH))
    function linear_foo()
        x = 4
        y = 5
    end

    rgx = r"%"
    buf = IOBuffer()
    output=""
    #test that the string output is at&t syntax by checking for occurrences of '%'s
    code_native(buf,linear_foo,(), syntax = :att)
    output=String(take!(buf))

    @test occursin(rgx, output)

    #test that the code output is intel syntax by checking it has no occurrences of '%'
    code_native(buf,linear_foo,(), syntax = :intel)
    output=String(take!(buf))

    @test !occursin(rgx, output)

    code_native(buf,linear_foo,())
    output=String(take!(buf))

    @test occursin(rgx, output)
end

@testset "error message" begin
    err = ErrorException("expression is not a function call or symbol")
    @test_throws err @code_lowered ""
    @test_throws err @code_lowered 1
    @test_throws err @code_lowered 1.0
end

using InteractiveUtils: editor

# Issue #13032
withenv("JULIA_EDITOR" => nothing, "VISUAL" => nothing, "EDITOR" => nothing) do
    # Make sure editor doesn't error when no ENV editor is set.
    @test isa(editor(), Cmd)

    # Invalid editor
    ENV["JULIA_EDITOR"] = ""
    @test_throws ErrorException editor()

    # Note: The following testcases should work regardless of whether these editors are
    # installed or not.

    # Editor on the path.
    ENV["JULIA_EDITOR"] = "vim"
    @test editor() == `vim`

    # Absolute path to editor.
    ENV["JULIA_EDITOR"] = "/usr/bin/vim"
    @test editor() == `/usr/bin/vim`

    # Editor on the path using arguments.
    ENV["JULIA_EDITOR"] = "subl -w"
    @test editor() == `subl -w`

    # Absolute path to editor with spaces.
    ENV["JULIA_EDITOR"] = "/Applications/Sublime\\ Text.app/Contents/SharedSupport/bin/subl"
    @test editor() == `'/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl'`

    # Paths with spaces and arguments (#13032).
    ENV["JULIA_EDITOR"] = "/Applications/Sublime\\ Text.app/Contents/SharedSupport/bin/subl -w"
    @test editor() == `'/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl' -w`

    ENV["JULIA_EDITOR"] = "'/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl' -w"
    @test editor() == `'/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl' -w`

    ENV["JULIA_EDITOR"] = "\"/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl\" -w"
    @test editor() == `'/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl' -w`
end

# clipboard functionality
if Sys.isapple()
    let str = "abc\0def"
        clipboard(str)
        @test clipboard() == str
    end
end
if Sys.iswindows() || Sys.isapple()
    for str in ("Hello, world.", "∀ x ∃ y", "")
        clipboard(str)
        @test clipboard() == str
    end
end
@static if Sys.iswindows()
    @test_broken false # CI has trouble with this test
    ## concurrent access error
    #hDesktop = ccall((:GetDesktopWindow, "user32"), stdcall, Ptr{Cvoid}, ())
    #ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Cvoid},), hDesktop) == 0 && Base.windowserror("OpenClipboard")
    #try
    #    @test_throws Base.SystemError("OpenClipboard", 0, Base.WindowsErrorInfo(0x00000005, nothing)) clipboard() # ACCESS_DENIED
    #finally
    #    ccall((:CloseClipboard, "user32"), stdcall, Cint, ()) == 0 && Base.windowserror("CloseClipboard")
    #end
    # empty clipboard failure
    ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Cvoid},), C_NULL) == 0 && Base.windowserror("OpenClipboard")
    try
        ccall((:EmptyClipboard, "user32"), stdcall, Cint, ()) == 0 && Base.windowserror("EmptyClipboard")
    finally
        ccall((:CloseClipboard, "user32"), stdcall, Cint, ()) == 0 && Base.windowserror("CloseClipboard")
    end
    @test clipboard() == ""
    # nul error (unsupported data)
    @test_throws ArgumentError("Windows clipboard strings cannot contain NUL character") clipboard("abc\0")
end

# buildbot path updating
file, ln = functionloc(versioninfo, Tuple{})
@test isfile(file)

@testset "Issue #34434" begin
    io = IOBuffer()
    code_native(io, eltype, Tuple{Int})
    @test occursin("eltype", String(take!(io)))
end
