# This file is a part of Julia. License is MIT: https://julialang.org/license

isdefined(Main, :FakePTYs) || @eval Main include("testhelpers/FakePTYs.jl")

# Tests that do not really go anywhere else

# test @assert macro
@test_throws AssertionError (@assert 1 == 2)
@test_throws AssertionError (@assert false)
@test_throws AssertionError (@assert false "this is a test")
@test_throws AssertionError (@assert false "this is a test" "another test")
@test_throws AssertionError (@assert false :a)
let
    try
        @assert 1 == 2
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test occursin("1 == 2", ex.msg)
    end
end
# test @assert message
let
    try
        @assert 1 == 2 "this is a test"
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "this is a test"
    end
end
# @assert only uses the first message string
let
    try
        @assert 1 == 2 "this is a test" "this is another test"
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "this is a test"
    end
end
# @assert calls string() on second argument
let
    try
        @assert 1 == 2 :random_object
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test !occursin("1 == 2", ex.msg)
        @test occursin("random_object", ex.msg)
    end
end
# if the second argument is an expression, c
let deepthought(x, y) = 42
    try
        @assert 1 == 2 string("the answer to the ultimate question: ",
                              deepthought(6, 9))
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "the answer to the ultimate question: 42"
    end
end

let # test the process title functions, issue #9957
    oldtitle = Sys.get_process_title()
    Sys.set_process_title("julia0x1")
    @test Sys.get_process_title() == "julia0x1"
    Sys.set_process_title(oldtitle)
    @test Sys.get_process_title() == oldtitle
end

# test GC.enable/disable
@test GC.enable(true)
@test GC.enable(false)
@test GC.enable(false) == false
@test GC.enable(true) == false
@test GC.enable(true)

# PR #10984
# Disable on windows because of issue (missing flush) when redirecting stderr.
let
    redir_err = "redirect_stderr(stdout)"
    exename = Base.julia_cmd()
    script = "$redir_err; module A; f() = 1; end; A.f() = 1"
    warning_str = read(`$exename --warn-overwrite=yes --startup-file=no -e $script`, String)
    @test occursin("f()", warning_str)
end

# lock / unlock
let l = ReentrantLock()
    lock(l)
    @test islocked(l)
    success = Ref(false)
    @test trylock(l) do
        @test lock(l) do
            success[] = true
            return :foo
        end === :foo
        return :bar
    end === :bar
    @test success[]
    t = @async begin
        @test trylock(l) do
            @test false
        end === false
    end
    Base.wait(t)
    unlock(l)
    @test_throws ErrorException unlock(l)
end

# task switching

@noinline function f6597(c)
    t = @async nothing
    finalizer(t -> c[] += 1, t)
    Base.wait(t)
    @test c[] == 0
    Base.wait(t)
    nothing
end
let c = Ref(0),
    t2 = @async (wait(); c[] += 99)
    @test c[] == 0
    f6597(c)
    GC.gc() # this should run the finalizer for t
    @test c[] == 1
    yield()
    @test c[] == 1
    yield(t2)
    @test c[] == 100
end

@test_throws ErrorException("deadlock detected: cannot wait on current task") wait(current_task())

# test that @sync is lexical (PR #27164)

const x27164 = Ref(0)
do_something_async_27164() = @async(begin sleep(1); x27164[] = 2; end)

let t = nothing
    @sync begin
        t = do_something_async_27164()
        @async (sleep(0.05); x27164[] = 1)
    end
    @test x27164[] == 1
    fetch(t)
    @test x27164[] == 2
end

# timing macros

# test that they don't introduce global vars
global v11801, t11801, names_before_timing
names_before_timing = names(@__MODULE__, all = true)

let t = @elapsed 1+1
    @test isa(t, Real) && t >= 0
end

let
    stats = @timed sin(1)
    @test stats.value == sin(1)
    @test isa(stats.time, Real) && stats.time >= 0

    # The return type of gcstats was changed in Julia 1.4 (# 34147)
    # Test that the 1.0 API still works
    val, t, bytes, gctime, gcstats = stats
    @test val === stats.value
    @test t === stats.time
    @test bytes === stats.bytes
    @test gctime === stats.gctime
    @test gcstats === stats.gcstats
end

# problem after #11801 - at global scope
t11801 = @elapsed 1+1
@test isa(t11801,Real) && t11801 >= 0
v11801, t11801 = @timed sin(1)
@test v11801 == sin(1)
@test isa(t11801,Real) && t11801 >= 0

@test names(@__MODULE__, all = true) == names_before_timing

# interactive utilities

struct ambigconvert; end # inject a problematic `convert` method to ensure it still works
Base.convert(::Any, v::ambigconvert) = v

import Base.summarysize
@test summarysize(Core) > (summarysize(Core.Compiler) + Base.summarysize(Core.Intrinsics)) > Core.sizeof(Core)
@test summarysize(Base) > 100_000 * sizeof(Ptr)

let R = Ref{Any}(nothing), depth = 10^6
    for i = 1:depth
        R = Ref{Any}(R)
    end
    R = Core.svec(R, R)
    @test summarysize(R) == (depth + 4) * sizeof(Ptr)
end

# issue #25367 - summarysize with reshaped arrays
let A = zeros(1000), B = reshape(A, (1,1000))
    @test summarysize((A,B)) < 2 * sizeof(A)

    # check that object header is accounted for
    @test summarysize(A) > sizeof(A)
end

# issue #32881
mutable struct S32881; end
let s = "abc"
    @test summarysize([s,s]) < summarysize(["abc","xyz"])
end
@test summarysize(Vector{Union{Nothing,Missing}}(undef, 16)) < summarysize(Vector{Union{Nothing,Missing}}(undef, 32))
@test summarysize(Vector{Nothing}(undef, 16)) == summarysize(Vector{Nothing}(undef, 32))
@test summarysize(S32881()) == sizeof(Int)

# issue #33675
let vec = vcat(missing, ones(100000))
    @test length(unique(summarysize(vec) for i = 1:20)) == 1
end

# issue #13021
let ex = try
    Main.x13021 = 0
    nothing
catch ex
    ex
end
    @test isa(ex, ErrorException) && ex.msg == "cannot assign variables in other modules"
end

## test conversion from UTF-8 to UTF-16 (for Windows APIs)

# empty arrays
@test transcode(UInt16, UInt8[]) == UInt16[]
@test transcode(UInt8, UInt16[]) == UInt8[]

# UTF-8-like sequences
V8 = [
    # 1-byte (ASCII)
    ([0x00],[0x0000])
    ([0x0a],[0x000a])
    ([0x7f],[0x007f])
    # 2-byte
    ([0xc0,0x80],[0x0000]) # overlong encoding
    ([0xc1,0xbf],[0x007f]) # overlong encoding
    ([0xc2,0x80],[0x0080])
    ([0xc3,0xbf],[0x00ff])
    ([0xc4,0x80],[0x0100])
    ([0xc4,0xa3],[0x0123])
    ([0xdf,0xbf],[0x07ff])
    # 3-byte
    ([0xe0,0x80,0x80],[0x0000]) # overlong encoding
    ([0xe0,0x81,0xbf],[0x007f]) # overlong encoding
    ([0xe0,0x82,0x80],[0x0080]) # overlong encoding
    ([0xe0,0x9f,0xbf],[0x07ff]) # overlong encoding
    ([0xe0,0xa0,0x80],[0x0800])
    ([0xe0,0xa2,0x9a],[0x089a])
    ([0xe1,0x88,0xb4],[0x1234])
    ([0xea,0xaf,0x8d],[0xabcd])
    ([0xed,0x9f,0xbf],[0xd7ff])
    ([0xed,0xa0,0x80],[0xd800]) # invalid code point – high surrogate
    ([0xed,0xaf,0xbf],[0xdbff]) # invalid code point – high surrogate
    ([0xed,0xb0,0x80],[0xdc00]) # invalid code point – low surrogate
    ([0xed,0xbf,0xbf],[0xdfff]) # invalid code point – low surrogate
    ([0xee,0x80,0x80],[0xe000])
    ([0xef,0xbf,0xbf],[0xffff])
    # 4-byte
    ([0xf0,0x80,0x80,0x80],[0x0000]) # overlong encoding
    ([0xf0,0x80,0x81,0xbf],[0x007f]) # overlong encoding
    ([0xf0,0x80,0x82,0x80],[0x0080]) # overlong encoding
    ([0xf0,0x80,0x9f,0xbf],[0x07ff]) # overlong encoding
    ([0xf0,0x80,0xa0,0x80],[0x0800]) # overlong encoding
    ([0xf0,0x8f,0xbf,0xbf],[0xffff]) # overlong encoding
    ([0xf0,0x90,0x80,0x80],[0xd800,0xdc00]) # U+10000
    ([0xf0,0x90,0x8d,0x88],[0xd800,0xdf48]) # U+10348
    ([0xf0,0x90,0x90,0xb7],[0xd801,0xdc37]) # U+10437
    ([0xf0,0xa4,0xad,0xa2],[0xd852,0xdf62]) # U+24b62
    ([0xf2,0xab,0xb3,0x9e],[0xda6f,0xdcde]) # U+abcde
    ([0xf3,0xbf,0xbf,0xbf],[0xdbbf,0xdfff]) # U+fffff
    ([0xf4,0x80,0x80,0x80],[0xdbc0,0xdc00]) # U+100000
    ([0xf4,0x8a,0xaf,0x8d],[0xdbea,0xdfcd]) # U+10abcd
    ([0xf4,0x8f,0xbf,0xbf],[0xdbff,0xdfff]) # U+10ffff
]

# non UTF-8-like sequences
X8 = Vector{UInt8}[
    # invalid 1-byte sequences
    [0x80], # 1 leading ones
    [0xbf],
    [0xc0], # 2 leading ones
    [0xdf],
    [0xe0], # 3 leading ones
    [0xef],
    [0xf0], # 4 leading ones
    [0xf7],
    [0xf8], # 5 leading ones
    [0xfb],
    [0xfc], # 6 leading ones
    [0xfd],
    [0xfe], # 7 leading ones
    [0xff], # 8 leading ones
    # other invalid sequences
    [0xf4,0x90,0xbf,0xbf],
    [0xf4,0x91,0x80,0x80],
    [0xf7,0x80,0x80,0x80],
    [0xf7,0xbf,0xbf,0xbf],
    [0xf8,0x80,0x80,0x80],
    [0xf8,0xbf,0xbf,0xbf],
    [0xff,0x80,0x80,0x80],
    [0xff,0xbf,0xbf,0xbf],
]

for s in [map(first,V8); X8],
    i = 1:length(s)-1,
    j = i+1:length(s)-(i==1)
    ss = s[i:j]
    ss in X8 || push!(X8, ss)
end
sort!(X8, lt=isless)
sort!(X8, by=length)

I8 = [(s,map(UInt16,s)) for s in X8]

for (X,Y,Z) in ((V8,V8,V8), (I8,V8,I8), (V8,I8,V8), (V8,V8,I8), (I8,V8,V8))
    for (a8, a16) in X
        @test transcode(UInt16, a8) == a16
        for (b8, b16) in Y
            ab8 = [a8; b8]
            ab16 = [a16; b16]
            @test transcode(UInt16, ab8) == ab16
            for (c8, c16) in Z
                abc8 = [ab8; c8]
                abc16 = [ab16; c16]
                @test transcode(UInt16, abc8) == abc16
            end
        end
    end
end

# UTF-16-like sequences
V16 = [
    # 1-unit UTF-16, 1-byte UTF-8 (ASCII)
    ([0x0000],[0x00])
    ([0x000a],[0x0a])
    ([0x007f],[0x7f])
    # 1-unit UTF-16, 2-byte UTF-8
    ([0x0080],[0xc2,0x80])
    ([0x00ff],[0xc3,0xbf])
    ([0x0100],[0xc4,0x80])
    ([0x0123],[0xc4,0xa3])
    ([0x07ff],[0xdf,0xbf])
    # 1-unit UTF-16, 3-byte UTF-8
    ([0x0800],[0xe0,0xa0,0x80])
    ([0x089a],[0xe0,0xa2,0x9a])
    ([0x1234],[0xe1,0x88,0xb4])
    ([0xabcd],[0xea,0xaf,0x8d])
    ([0xd7ff],[0xed,0x9f,0xbf])
    ([0xe000],[0xee,0x80,0x80])
    ([0xffff],[0xef,0xbf,0xbf])
    # 2-unit UTF-16, 4-byte UTF-8
    ([0xd800,0xdc00],[0xf0,0x90,0x80,0x80]) # U+10000
    ([0xd800,0xdf48],[0xf0,0x90,0x8d,0x88]) # U+10348
    ([0xd801,0xdc37],[0xf0,0x90,0x90,0xb7]) # U+10437
    ([0xd852,0xdf62],[0xf0,0xa4,0xad,0xa2]) # U+24b62
    ([0xda6f,0xdcde],[0xf2,0xab,0xb3,0x9e]) # U+abcde
    ([0xdbbf,0xdfff],[0xf3,0xbf,0xbf,0xbf]) # U+fffff
    ([0xdbc0,0xdc00],[0xf4,0x80,0x80,0x80]) # U+100000
    ([0xdbea,0xdfcd],[0xf4,0x8a,0xaf,0x8d]) # U+10abcd
    ([0xdbff,0xdfff],[0xf4,0x8f,0xbf,0xbf]) # U+10ffff
]

I16 = [
    ([0xd800],[0xed,0xa0,0x80]) # high surrogate
    ([0xdbff],[0xed,0xaf,0xbf]) # high surrogate
    ([0xdc00],[0xed,0xb0,0x80]) # low surrogate
    ([0xdfff],[0xed,0xbf,0xbf]) # low surrogate
]

for (X,Y,Z) in ((V16,V16,V16), (I16,V16,I16), (V16,I16,V16), (V16,V16,I16), (I16,V16,V16))
    for (a16, a8) in X
        @test transcode(UInt8, a16) == a8
        @test transcode(UInt16, a8) == a16
        for (b16, b8) in Y
            ab16 = [a16; b16]
            ab8 = [a8; b8]
            @test transcode(UInt8, ab16) == ab8
            @test transcode(UInt16, ab8) == ab16
            for (c16, c8) in Z
                abc16 = [ab16; c16]
                abc8 = [ab8; c8]
                @test transcode(UInt8, abc16) == abc8
                @test transcode(UInt16, abc8) == abc16
            end
        end
    end
end

let s = "abcα🐨\0x\0"
    for T in (UInt8, UInt16, UInt32, Int32)
        @test transcode(T, s) == transcode(T, codeunits(s))
        @test transcode(String, transcode(T, s)) == s
    end
end

let X = UInt8[0x30,0x31,0x32]
    for T in (UInt8, UInt16, UInt32, Int32)
        @test transcode(UInt8,transcode(T, X)) == X
        @test transcode(UInt8,transcode(T, 0x30:0x32)) == X
    end
end

let optstring = repr("text/plain", Base.JLOptions())
    @test startswith(optstring, "JLOptions(\n")
    @test !occursin("Ptr", optstring)
    @test endswith(optstring, "\n)")
    @test occursin(" = \"", optstring)
end
let optstring = repr(Base.JLOptions())
    @test startswith(optstring, "JLOptions(")
    @test endswith(optstring, ")")
    @test !occursin("\n", optstring)
    @test !occursin("Ptr", optstring)
    @test occursin(" = \"", optstring)
end

# Base.securezero! functions (#17579)
import Base: securezero!, unsafe_securezero!
let a = [1,2,3]
    @test securezero!(a) === a == [0,0,0]
    a[:] = 1:3
    @test unsafe_securezero!(pointer(a), length(a)) == pointer(a)
    @test a == [0,0,0]
    a[:] = 1:3
    @test unsafe_securezero!(Ptr{Cvoid}(pointer(a)), sizeof(a)) == Ptr{Cvoid}(pointer(a))
    @test a == [0,0,0]
end

# PR #28038 (prompt/getpass stream args)
@test_throws MethodError Base.getpass(IOBuffer(), stdout, "pass")
let buf = IOBuffer()
    @test Base.prompt(IOBuffer("foo\nbar\n"), buf, "baz") == "foo"
    @test String(take!(buf)) == "baz: "
    @test Base.prompt(IOBuffer("\n"), buf, "baz", default="foobar") == "foobar"
    @test String(take!(buf)) == "baz [foobar]: "
    @test Base.prompt(IOBuffer("blah\n"), buf, "baz", default="foobar") == "blah"
end

# these tests are not in a test block so that they will compile separately
@static if Sys.iswindows()
    SetLastError(code) = ccall(:SetLastError, stdcall, Cvoid, (UInt32,), code)
else
    SetLastError(_) = nothing
end
@test Libc.errno(0xc0ffee) === nothing
@test SetLastError(0xc0def00d) === nothing
let finalized = false
    function closefunc(_)
        Libc.errno(0)
        SetLastError(0)
        finalized = true
    end
    @eval (finalizer($closefunc, zeros()); nothing)
    GC.gc(); GC.gc(); GC.gc(); GC.gc()
    @test finalized
end
@static if Sys.iswindows()
    @test ccall(:GetLastError, stdcall, UInt32, ()) == 0xc0def00d
    @test Libc.GetLastError() == 0xc0def00d
end
@test Libc.errno() == 0xc0ffee

# Test that we can VirtualProtect jitted code to writable
@noinline function WeVirtualProtectThisToRWX(x, y)
    return x + y
end
@static if Sys.iswindows()
    let addr = @cfunction(WeVirtualProtectThisToRWX, UInt64, (UInt64, UInt64))
        addr = addr - (UInt64(addr) % 4096)
        PAGE_EXECUTE_READWRITE = 0x40
        oldPerm = Ref{UInt32}()
        err18083 = ccall(:VirtualProtect, stdcall, Cint,
            (Ptr{Cvoid}, Csize_t, UInt32, Ptr{UInt32}),
            addr, 4096, PAGE_EXECUTE_READWRITE, oldPerm)
        err18083 == 0 && Base.windowserror(:VirtualProtect)
    end
end

let buf = IOBuffer()
    printstyled(IOContext(buf, :color=>true), "foo", color=:red)
    @test startswith(String(take!(buf)), Base.text_colors[:red])
end

# Test that `printstyled` accepts non-string values, just as `print` does
let buf_color = IOBuffer()
    args = (3.2, "foo", :testsym)
    printstyled(IOContext(buf_color, :color=>true), args..., color=:red)
    buf_plain = IOBuffer()
    print(buf_plain, args...)
    expected_str = string(Base.text_colors[:red],
                          String(take!(buf_plain)),
                          Base.text_colors[:default])
    @test expected_str == String(take!(buf_color))
end

# Test that `printstyled` on multiline input prints the ANSI codes
# on each line
let buf_color = IOBuffer()
    str = "Two\nlines"
    printstyled(IOContext(buf_color, :color=>true), str; bold=true, color=:red)
    @test String(take!(buf_color)) == "\e[31m\e[1mTwo\e[22m\e[39m\n\e[31m\e[1mlines\e[22m\e[39m"
end

if stdout isa Base.TTY
    @test haskey(stdout, :color) == true
    @test haskey(stdout, :bar) == false
    @test (:color=>Base.have_color) in stdout
    @test (:color=>!Base.have_color) ∉ stdout
    @test stdout[:color] == get(stdout, :color, nothing) == Base.have_color
    @test get(stdout, :bar, nothing) === nothing
    @test_throws KeyError stdout[:bar]
end

@testset "`displaysize` on closed TTY #34620" begin
    Main.FakePTYs.with_fake_pty() do rawfd, _
        tty = Base.TTY(rawfd)
        @test displaysize(tty) isa Tuple{Integer,Integer}
        close(tty)
        @test_throws Base.IOError displaysize(tty)
    end
end

let
    global c_18711 = 0
    buf = IOContext(IOBuffer(), :hascontext => true)
    Base.with_output_color(:red, buf) do buf
        global c_18711
        get(buf, :hascontext, false) && (c_18711 += 1)
    end
    @test c_18711 == 1
end

let buf = IOBuffer()
    buf_color = IOContext(buf, :color => true)
    printstyled(buf_color, "foo", color=:red)
    # Check that we get back to normal text color in the end
    @test String(take!(buf)) == "\e[31mfoo\e[39m"

    # Check that boldness is turned off
    printstyled(buf_color, "foo"; bold=true, color=:red)
    @test String(take!(buf)) == "\e[31m\e[1mfoo\e[22m\e[39m"
end

abstract type DA_19281{T, N} <: AbstractArray{T, N} end
Base.convert(::Type{Array{S, N}}, ::DA_19281{T, N}) where {S,T,N} = error()
x_19281 = [(), (1,)]
mutable struct Foo_19281
    f::Vector{Tuple}
    Foo_19281() = new(x_19281)
end

@testset "test this does not segfault #19281" begin
    @test Foo_19281().f[1] == ()
    @test Foo_19281().f[2] == (1,)
end

let
    x_notdefined = Ref{String}()
    @test !isassigned(x_notdefined)

    x_defined = Ref{String}("Test")
    @test isassigned(x_defined)
end

mutable struct Demo_20254
    arr::Array{String}
end

# these cause stack overflows and are a little flaky on CI, ref #20256
if Bool(parse(Int,(get(ENV, "JULIA_TESTFULL", "0"))))
    function Demo_20254(arr::AbstractArray=Any[])
        Demo_20254(string.(arr))
    end

    _get_19433(x::NTuple{1}) = (something(x[1]),)
    _get_19433(xs::Vararg) = (something(xs[1]), _get_19433(xs[2:end])...)

    f_19433(f_19433, xs...) = f_19433(_get_19433(xs)...)

    @testset "test this does not crash, issue #19433 and #20254" begin
        @test_throws StackOverflowError Demo_20254()
        @test_throws StackOverflowError f_19433(+, 1, 2)
    end
end

# Test issue #19774 invokelatest fix.

# we define this in a module to allow rewriting
# rather than needing an extra eval.
module Issue19774
f(x) = 1
end

# First test the world issue condition.
let foo() = begin
        @eval Issue19774.f(x::Int) = 2
        return Issue19774.f(0)
    end
    @test foo() == 1    # We should be using the original function.
end

# Now check that invokelatest fixes that issue.
let foo() = begin
        @eval Issue19774.f(x::Int) = 3
        return Base.invokelatest(Issue19774.f, 0)
    end
    @test foo() == 3
end

# Check that the kwargs conditions also works
module Kwargs19774
f(x, y; z=0) = x * y + z
end

@test Kwargs19774.f(2, 3; z=1) == 7

let foo() = begin
        @eval Kwargs19774.f(x::Int, y::Int; z=3) = z
        return Base.invokelatest(Kwargs19774.f, 2, 3; z=1)
    end
    @test foo() == 1
end

# Endian tests
# For now, we only support little endian.
# Add an `Sys.ARCH` test for big endian when/if we add support for that.
# Do **NOT** use `ENDIAN_BOM` to figure out the endianness
# since that's exactly what we want to test.
@test ENDIAN_BOM == 0x04030201
@test ntoh(0x1) == 0x1
@test hton(0x1) == 0x1
@test ltoh(0x1) == 0x1
@test htol(0x1) == 0x1
@test ntoh(0x102) == 0x201
@test hton(0x102) == 0x201
@test ltoh(0x102) == 0x102
@test htol(0x102) == 0x102
@test ntoh(0x1020304) == 0x4030201
@test hton(0x1020304) == 0x4030201
@test ltoh(0x1020304) == 0x1020304
@test htol(0x1020304) == 0x1020304
@test ntoh(0x102030405060708) == 0x807060504030201
@test hton(0x102030405060708) == 0x807060504030201
@test ltoh(0x102030405060708) == 0x102030405060708
@test htol(0x102030405060708) == 0x102030405060708

@testset "inline bug #18735" begin
    @noinline f(n) = n ? error() : Int
    g() = Union{f(true)}
    @test_throws ErrorException g()
end

include("testenv.jl")

let flags = Cmd(filter(a->!occursin("depwarn", a), collect(test_exeflags)))
    local cmd = `$test_exename $flags deprecation_exec.jl`

    if !success(pipeline(cmd; stdout=stdout, stderr=stderr))
        error("Deprecation test failed, cmd : $cmd")
    end
end

# PR #23664, make sure names don't get added to the default `Main` workspace
@test readlines(`$(Base.julia_cmd()) --startup-file=no -e 'foreach(println, names(Main))'`) == ["Base","Core","Main"]

# issue #26310
@test_warn "could not import" Core.eval(@__MODULE__, :(import .notdefined_26310__))
@test_warn "could not import" Core.eval(Main,        :(import ........notdefined_26310__))
@test_nowarn Core.eval(Main, :(import .Main))
@test_nowarn Core.eval(Main, :(import ....Main))

# issue #27239
@testset "strftime tests issue #27239" begin

    # save current locales
    locales = Dict()
    for cat in 0:9999
        cstr = ccall(:setlocale, Cstring, (Cint, Cstring), cat, C_NULL)
        if cstr != C_NULL
            locales[cat] = unsafe_string(cstr)
        end
    end

    # change to non-Unicode Korean
    for (cat, _) in locales
        korloc = ["ko_KR.EUC-KR", "ko_KR.CP949", "ko_KR.949", "Korean_Korea.949"]
        for lc in korloc
            cstr = ccall(:setlocale, Cstring, (Cint, Cstring), cat, lc)
        end
    end

    # system dependent formats
    timestr_c = Libc.strftime(0.0)
    timestr_aAbBpZ = Libc.strftime("%a %A %b %B %p %Z", 0)

    # recover locales
    for (cat, lc) in locales
        cstr = ccall(:setlocale, Cstring, (Cint, Cstring), cat, lc)
    end

    # tests
    @test isvalid(timestr_c)
    @test isvalid(timestr_aAbBpZ)
end


using Base: @kwdef

@kwdef struct Test27970Typed
    a::Int
    b::String = "hi"
end

@kwdef struct Test27970Untyped
    a
end

@kwdef struct Test27970Empty end

@testset "No default values in @kwdef" begin
    @test Test27970Typed(a=1) == Test27970Typed(1, "hi")
    # Implicit type conversion (no assertion on kwarg)
    @test Test27970Typed(a=0x03) == Test27970Typed(3, "hi")
    @test_throws UndefKeywordError Test27970Typed()

    @test Test27970Untyped(a=1) == Test27970Untyped(1)
    @test_throws UndefKeywordError Test27970Untyped()

    # Just checking that this doesn't stack overflow on construction
    @test Test27970Empty() == Test27970Empty()
end

abstract type AbstractTest29307 end
@kwdef struct Test29307{T<:Integer} <: AbstractTest29307
    a::T=2
end

@testset "subtyped @kwdef" begin
    @test Test29307() == Test29307{Int}(2)
    @test Test29307(a=0x03) == Test29307{UInt8}(0x03)
    @test Test29307{UInt32}() == Test29307{UInt32}(2)
    @test Test29307{UInt32}(a=0x03) == Test29307{UInt32}(0x03)
end

@kwdef struct TestInnerConstructor
    a = 1
    TestInnerConstructor(a::Int) = (@assert a>0; new(a))
    function TestInnerConstructor(a::String)
        @assert length(a) > 0
        new(a)
    end
end

@testset "@kwdef inner constructor" begin
    @test TestInnerConstructor() == TestInnerConstructor(1)
    @test TestInnerConstructor(a=2) == TestInnerConstructor(2)
    @test_throws AssertionError TestInnerConstructor(a=0)
    @test TestInnerConstructor(a="2") == TestInnerConstructor("2")
    @test_throws AssertionError TestInnerConstructor(a="")
end

const outsidevar = 7
@kwdef struct TestOutsideVar
    a::Int=outsidevar
end
@test TestOutsideVar() == TestOutsideVar(7)


@testset "exports of modules" begin
    for (_, mod) in Base.loaded_modules
       for v in names(mod)
           @test isdefined(mod, v)
       end
   end
end

@testset "ordering UUIDs" begin
    a = Base.UUID("dbd321ed-e87e-4f33-9511-65b7d01cdd55")
    b = Base.UUID("2832b20a-2ad5-46e9-abb1-2d20c8c31dd3")
    @test isless(b, a)
    @test sort([a, b]) == [b, a]
end

@testset "Libc.rand" begin
    low, high = extrema(Libc.rand(Float64) for i=1:10^4)
    # these fail with probability 2^(-10^4) ≈ 5e-3011
    @test 0 ≤ low < 0.5
    @test 0.5 < high < 1
end

# Pointer 0-arg constructor
@test Ptr{Cvoid}() == C_NULL

# Finalizer with immutable should throw
@test_throws ErrorException finalizer(x->nothing, 1)
@test_throws ErrorException finalizer(C_NULL, 1)


@testset "GC utilities" begin
    GC.gc()
    GC.gc(true); GC.gc(false)

    GC.safepoint()
end
