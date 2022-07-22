# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, InteractiveUtils

@testset "highlighting" begin
    include("highlighting.jl")
end

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

# supertypes
@test supertypes(B20086) == (B20086, A20086, Any)
@test supertypes(B20086{Int}) == (B20086{Int}, A20086{Int}, Any)
@test supertypes(B20086{Int,2}) == (B20086{Int,2}, A20086{Int,2}, Any)
@test supertypes(Any) == (Any,)

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

for u in Any[
    Union{Int, UInt},
    Union{Nothing, Vector{Tuple{String, Tuple{Char, Char}}}},
    Union{Char, UInt8, UInt},
    Union{Tuple{Int, Int}, Tuple{Char, Int}, Nothing},
    Union{Missing, Nothing}
]
    @test InteractiveUtils.is_expected_union(u)
end

for u in Any[
    Union{Nothing, Tuple{Vararg{Char}}},
    Union{Missing, Array},
    Union{Int, Tuple{Any, Int}}
]
    @test !InteractiveUtils.is_expected_union(u)
end
mutable struct Stable{T,N}
    A::Array{T,N}
end
mutable struct Unstable{T}
    A::Array{T}
end
Base.getindex(A::Stable, i) = A.A[i]
Base.getindex(A::Unstable, i) = A.A[i]

tag = "ARRAY"
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


# Make sure getproperty and setproperty! works with @code_... macros
struct T1234321
    t::Int
end
Base.getproperty(t::T1234321, ::Symbol) = "foo"
@test (@code_typed T1234321(1).f).second == String
Base.setproperty!(t::T1234321, ::Symbol, ::Symbol) = "foo"
@test (@code_typed T1234321(1).f = :foo).second == String

# Make sure `do` block works with `@code_...` macros
@test (@code_typed map(1:1) do x; x; end).second == Vector{Int}
@test (@code_typed open(`cat`; read=true) do _; 1; end).second == Int

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
tag = "ANY"
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
module inner_mod
inner_x = 1
end
import Test: @test
export x_exported
x_exported = 1.0
y_not_exp = 1.0
z_larger = Vector{Float64}(undef, 3)
a_smaller = Vector{Float64}(undef, 2)
end

using Test

@test repr(varinfo(Main, r"^$")) == """
| name | size | summary |
|:---- | ----:|:------- |
"""
let v = repr(varinfo(_test_varinfo_))
    @test occursin("| x_exported     |   8 bytes | Float64 |", v)
    @test !occursin("y_not_exp", v)
    @test !occursin("@test", v)
    @test !occursin("inner_x", v)
end
let v = repr(varinfo(_test_varinfo_, all = true))
    @test occursin("x_exported", v)
    @test occursin("y_not_exp", v)
    @test !occursin("@test", v)
    @test findfirst("a_smaller", v)[1] < findfirst("z_larger", v)[1] # check for alphabetical
    @test !occursin("inner_x", v)
end
let v = repr(varinfo(_test_varinfo_, imported = true))
    @test occursin("x_exported", v)
    @test !occursin("y_not_exp", v)
    @test occursin("@test", v)
    @test !occursin("inner_x", v)
end
let v = repr(varinfo(_test_varinfo_, all = true, sortby = :size))
    @test findfirst("z_larger", v)[1] < findfirst("a_smaller", v)[1] # check for size order
end
let v = repr(varinfo(_test_varinfo_, sortby = :summary))
    @test findfirst("Float64", v)[1] < findfirst("Module", v)[1] # check for summary order
end
let v = repr(varinfo(_test_varinfo_, all = true, recursive = true))
    @test occursin("inner_x", v)
end
let v = repr(varinfo(_test_varinfo_, all = true, minsize = 9))
    @test !occursin("x_exported", v) # excluded: 8 bytes
    @test occursin("a_smaller", v)
end

# Issue 14173
module Tmp14173
    using Random
    export A
    A = randn(2000, 2000)
end
varinfo(Tmp14173) # warm up
const MEMDEBUG = ccall(:jl_is_memdebug, Bool, ())
@test @allocated(varinfo(Tmp14173)) < (MEMDEBUG ? 300000 : 100000)

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
@test (@which vcat(1...)).name === :vcat

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
# issue #39426
let x..y = 0
    @test (@which 1..2).name === :..
end

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

let _true = Ref(true), f, g, h
    @noinline f() = ccall((:time, "error_library_doesnt_exist\0"), Cvoid, ()) # should throw error during runtime
    @noinline g() = _true[] ? 0 : h()
    @noinline h() = (g(); f())
    @test g() == 0
    @test_throws ErrorException h()
end

# manually generate a broken function, which will break codegen
# and make sure Julia doesn't crash
@eval @noinline @Base.constprop :none f_broken_code() = 0
let m = which(f_broken_code, ())
   let src = Base.uncompressed_ast(m)
       src.code = Any[
           Expr(:meta, :noinline)
           Core.ReturnNode(Expr(:invalid))
       ]
       m.source = src
   end
end
_true = true
# and show that we can still work around it
@noinline g_broken_code() = _true ? 0 : h_broken_code()
@noinline h_broken_code() = (g_broken_code(); f_broken_code())
let err = tempname(),
    old_stderr = stderr,
    new_stderr = open(err, "w")
    try
        redirect_stderr(new_stderr)
        println(new_stderr, "start")
        flush(new_stderr)
        @test occursin("h_broken_code", sprint(code_native, h_broken_code, ()))
        Libc.flush_cstdio()
        println(new_stderr, "end")
        flush(new_stderr)
        @eval @test g_broken_code() == 0
    finally
        redirect_stderr(old_stderr)
        close(new_stderr)
        let errstr = read(err, String)
            @test startswith(errstr, """start
                end
                Internal error: encountered unexpected error during compilation of f_broken_code:
                ErrorException(\"unsupported or misplaced expression \"invalid\" in function f_broken_code\")
                """) || errstr
            @test !endswith(errstr, "\nend\n") || errstr
        end
        rm(err)
    end
end

# Issue #33163
A33163(x; y) = x + y
B33163(x) = x
@test (@code_typed A33163(1, y=2))[1].inferred
@test !(@code_typed optimize=false A33163(1, y=2))[1].inferred
@test !(@code_typed optimize=false B33163(1))[1].inferred

@test_throws MethodError (@code_lowered wrongkeyword=true 3 + 4)

# Issue #14637
@test (@which Base.Base.Base.nothing) == Core
@test_throws ErrorException (@functionloc Base.nothing)
@test (@code_typed (3//4).num)[2] == Int

struct A14637
    x
end
a14637 = A14637(0)
@test (@which a14637.x).name === :getproperty
@test (@functionloc a14637.x)[2] isa Integer

# Issue #28615
@test_throws ErrorException (@which [1, 2] .+ [3, 4])
@test (@code_typed optimize=true max.([1,7], UInt.([4])))[2] == Vector{UInt}
@test (@code_typed Ref.([1,2])[1].x)[2] == Int
@test (@code_typed max.(Ref(true).x))[2] == Bool
@test !isempty(@code_typed optimize=false max.(Ref.([5, 6])...))

# Issue # 45889
@test !isempty(@code_typed 3 .+ 6)
@test !isempty(@code_typed 3 .+ 6 .+ 7)
@test !isempty(@code_typed optimize=false (.- [3,4]))
@test !isempty(@code_typed optimize=false (6 .- [3,4]))
@test !isempty(@code_typed optimize=false (.- 0.5))

# Issue #36261
@test (@code_typed max.(1 .+ 3, 5 - 7))[2] == Int
f36261(x,y) = 3x + 4y
A36261 = Float64[1.0, 2.0, 3.0]
@test (@code_typed f36261.(A36261, pi))[1].inferred
@test (@code_typed f36261.(A36261, 1 .+ pi))[1].inferred
@test (@code_typed f36261.(A36261, 1 + pi))[1].inferred


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
        return 5
    end

    rgx = r"%"
    buf = IOBuffer()
    #test that the string output is at&t syntax by checking for occurrences of '%'s
    code_native(buf, linear_foo, (), syntax = :att, debuginfo = :none)
    output = replace(String(take!(buf)), r"#[^\r\n]+" => "")
    @test occursin(rgx, output)

    #test that the code output is intel syntax by checking it has no occurrences of '%'
    code_native(buf, linear_foo, (), syntax = :intel, debuginfo = :none)
    output = replace(String(take!(buf)), r"#[^\r\n]+" => "")
    @test !occursin(rgx, output)

    code_native(buf, linear_foo, ())
    output = String(take!(buf))
    @test occursin(rgx, output)

    @testset "binary" begin
        # check the RET instruction (opcode: C3)
        ret = r"^; [0-9a-f]{4}: c3$"m

        # without binary flag (default)
        code_native(buf, linear_foo, (), dump_module=false)
        output = String(take!(buf))
        @test !occursin(ret, output)

        # with binary flag
        for binary in false:true
            code_native(buf, linear_foo, (); binary, dump_module=false)
            output = String(take!(buf))
            @test occursin(ret, output) == binary
        end
    end
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
@test isfile(pathof(InteractiveUtils))
@test isdir(pkgdir(InteractiveUtils))

@testset "buildbot path updating" begin
    file, ln = functionloc(versioninfo, Tuple{})
    @test isfile(file)

    e = try versioninfo("wat")
    catch e
        e
    end
    @test e isa MethodError
    m = @which versioninfo()
    s = sprint(showerror, e)
    m = match(Regex("@ .+ (.*?):$(m.line)"), s)
    @test isfile(expanduser(m.captures[1]))

    g() = x
    e, bt = try code_llvm(g, Tuple{Int})
    catch e
        e, catch_backtrace()
    end
    @test e isa Exception
    s = sprint(showerror, e, bt)
    m = match(r"(\S*InteractiveUtils[\/\\]src\S*):", s)
    @test isfile(expanduser(m.captures[1]))
end

@testset "Issue #34434" begin
    io = IOBuffer()
    code_native(io, eltype, Tuple{Int})
    @test occursin("eltype", String(take!(io)))
end

@testset "Issue #41010" begin
    struct A41010 end

    struct B41010
        a::A41010
    end
    export B41010

    ms = methodswith(A41010, @__MODULE__) |> collect
    @test ms[1].name === :B41010
end

# macro options should accept both literals and variables
let
    opt = false
    @test !(first(@code_typed optimize=opt sum(1:10)).inferred)
end

@testset "@time_imports" begin
    mktempdir() do dir
        cd(dir) do
            try
                pushfirst!(LOAD_PATH, dir)
                foo_file = joinpath(dir, "Foo3242.jl")
                write(foo_file,
                    """
                    module Foo3242
                    foo() = 1
                    end
                    """)

                Base.compilecache(Base.PkgId("Foo3242"))

                fname = tempname()
                f = open(fname, "w")
                redirect_stdout(f) do
                    @eval @time_imports using Foo3242
                end
                close(f)
                buf = read(fname)
                rm(fname)

                @test occursin("ms  Foo3242", String(buf))

            finally
                filter!((≠)(dir), LOAD_PATH)
            end
        end
    end
end

let # `default_tt` should work with any function with one method
    @test (code_warntype(devnull, function ()
        sin(42)
    end); true)
    @test (code_warntype(devnull, function (a::Int)
        sin(a)
    end); true)
    @test (code_llvm(devnull, function ()
        sin(42)
    end); true)
    @test (code_llvm(devnull, function (a::Int)
        sin(a)
    end); true)
    @test (code_native(devnull, function ()
        sin(42)
    end); true)
    @test (code_native(devnull, function (a::Int)
        sin(a)
    end); true)
end

@testset "code_llvm on opaque_closure" begin
    let ci = code_typed(+, (Int, Int))[1][1]
        ir = Core.Compiler.inflate_ir(ci, Any[], Any[Tuple{}, Int, Int])
        oc = Core.OpaqueClosure(ir)
        @test (code_llvm(devnull, oc, Tuple{Int, Int}); true)
        let io = IOBuffer()
            code_llvm(io, oc, Tuple{})
            @test occursin(InteractiveUtils.OC_MISMATCH_WARNING, String(take!(io)))
        end
    end
end
