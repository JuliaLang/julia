# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "misc" begin
# Test info
@test contains(sprint(io->info(io,"test")), "INFO:")
@test contains(sprint(io->info(io,"test")), "INFO: test")
@test contains(sprint(io->info(io,"test ",1,2,3)), "INFO: test 123")
@test contains(sprint(io->info(io,"test", prefix="MYINFO: ")), "MYINFO: test")

# Test warn
@test contains(sprint(io->Base.warn_once(io,"test")), "WARNING: test")
@test isempty(sprint(io->Base.warn_once(io,"test")))

@test contains(sprint(io->warn(io)), "WARNING:")
@test contains(sprint(io->warn(io, "test")), "WARNING: test")
@test contains(sprint(io->warn(io, "test ",1,2,3)), "WARNING: test 123")
@test contains(sprint(io->warn(io, "test", prefix="MYWARNING: ")), "MYWARNING: test")
@test contains(sprint(io->warn(io, "testonce", once=true)), "WARNING: testonce")
@test isempty(sprint(io->warn(io, "testonce", once=true)))
@test !isempty(sprint(io->warn(io, "testonce", once=true, key=hash("testonce",hash("testanother")))))
let bt = backtrace()
    ws = split(chomp(sprint(io->warn(io,"test", bt))), '\n')
    bs = split(chomp(sprint(io->Base.show_backtrace(io,bt))), '\n')
    @test contains(ws[1],"WARNING: test")
    for (l,b) in zip(ws[2:end],bs)
        @test contains(l, b)
    end
end

# test assert() method
@test_throws AssertionError assert(false)
let res = assert(true)
    @test res === nothing
end
let
    try
        assert(false)
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test isempty(ex.msg)
    end
end

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
        @test contains(ex.msg, "1 == 2")
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
        @test !contains(ex.msg,  "1 == 2")
        @test contains(ex.msg, "random_object")
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


# test gc_enable/disable
@test gc_enable(true)
@test gc_enable(false)
@test gc_enable(false) == false
@test gc_enable(true) == false
@test gc_enable(true)

# test methodswith
immutable NoMethodHasThisType end
@test isempty(methodswith(NoMethodHasThisType))
@test !isempty(methodswith(Int))
immutable Type4Union end
func4union(::Union{Type4Union,Int}) = ()
@test !isempty(methodswith(Type4Union))

# PR #10984
# Disable on windows because of issue (missing flush) when redirecting STDERR.
let
    redir_err = "redirect_stderr(STDOUT)"
    exename = Base.julia_cmd()
    script = "$redir_err; module A; f() = 1; end; A.f() = 1"
    warning_str = readstring(`$exename --startup-file=no -e $script`)
    @test contains(warning_str, "f()")
end

# lock / unlock
let l = ReentrantLock()
    lock(l)
    unlock(l)
    @test_throws ErrorException unlock(l)
end

# timing macros

# test that they don't introduce global vars
global v11801, t11801, names_before_timing
names_before_timing = names(current_module(), true)

let t = @elapsed 1+1
    @test isa(t, Real) && t >= 0
end

let
    val, t = @timed sin(1)
    @test val == sin(1)
    @test isa(t, Real) && t >= 0
end

# problem after #11801 - at global scope
t11801 = @elapsed 1+1
@test isa(t11801,Real) && t11801 >= 0
v11801, t11801 = @timed sin(1)
@test v11801 == sin(1)
@test isa(t11801,Real) && t11801 >= 0

@test names(current_module(), true) == names_before_timing

# interactive utilities

import Base.summarysize
@test summarysize(Core) > summarysize(Core.Inference) > Core.sizeof(Core)
@test summarysize(Base) > 10_000*sizeof(Int)
module _test_whos_
export x
x = 1.0
end
@test sprint(whos, Main, r"^$") == ""
let v = sprint(whos, _test_whos_)
    @test contains(v, "x      8 bytes  Float64")
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

@test Base.is_unix(:Darwin)
@test Base.is_unix(:FreeBSD)
@test_throws ArgumentError Base.is_unix(:BeOS)
if !is_windows()
    @test Sys.windows_version() === (0, 0)
else
    @test (Sys.windows_version()::Tuple{Int,Int})[1] > 0
end

# Issue 14173
module Tmp14173
    export A
    A = randn(2000, 2000)
end
whos(IOBuffer(), Tmp14173) # warm up
@test @allocated(whos(IOBuffer(), Tmp14173)) < 10000

## test conversion from UTF-8 to UTF-16 (for Windows APIs)
import Base.Libc: transcode

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
sort!(X8, lt=lexless)
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

# clipboard functionality
if is_windows()
    for str in ("Hello, world.", "∀ x ∃ y", "")
        clipboard(str)
        @test clipboard() == str
    end
end

optstring = sprint(show, Base.JLOptions())
@test startswith(optstring, "JLOptions(")
@test endswith(optstring, ")")

end
