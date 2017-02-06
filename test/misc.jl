# This file is a part of Julia. License is MIT: http://julialang.org/license

# Tests that do not really go anywhere else

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

# PR #16213
module Logging
    function bar(io)
        info(io,"barinfo")
        warn(io,"barwarn")
        Base.display_error(io,"barerror",backtrace())
    end
    function pooh(io)
        info(io,"poohinfo")
        warn(io,"poohwarn")
        Base.display_error(io,"pooherror",backtrace())
    end
end
function foo(io)
    info(io,"fooinfo")
    warn(io,"foowarn")
    Base.display_error(io,"fooerror",backtrace())
end

@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))


logging(DevNull, TestMain_misc.Logging, :bar;  kind=:info)
@test all(contains.(sprint(Logging.bar), ["WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(DevNull, TestMain_misc.Logging;  kind=:info)
@test all(contains.(sprint(Logging.bar), ["WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(DevNull;  kind=:info)
@test all(contains.(sprint(Logging.bar), ["WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(kind=:info)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))


logging(DevNull, TestMain_misc.Logging, :bar;  kind=:warn)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(DevNull, TestMain_misc.Logging;  kind=:warn)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(DevNull;  kind=:warn)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "ERROR: \"fooerror\""]))

logging(kind=:warn)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))


logging(DevNull, TestMain_misc.Logging, :bar;  kind=:error)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "WARNING: barwarn"]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(DevNull, TestMain_misc.Logging;  kind=:error)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "WARNING: barwarn"]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn"]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(DevNull;  kind=:error)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "WARNING: barwarn"]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn"]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn"]))

logging(kind=:error)
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))


logging(DevNull, TestMain_misc.Logging, :bar)
@test sprint(Logging.bar) == ""
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(DevNull, TestMain_misc.Logging)
@test sprint(Logging.bar) == ""
@test sprint(Logging.pooh) == ""
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(DevNull)
@test sprint(Logging.bar) == ""
@test sprint(Logging.pooh) == ""
@test sprint(foo) == ""

logging()
@test all(contains.(sprint(Logging.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(Logging.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

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
# `methodswith` relies on exported symbols
export func4union, Base
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
    wait(t)
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
@test summarysize(Core) > (summarysize(Core.Inference) + Base.summarysize(Core.Intrinsics)) > Core.sizeof(Core)
@test summarysize(Base) > 100_000 * sizeof(Ptr)

let R = Ref{Any}(nothing), depth = 10^6
    for i = 1:depth
        R = Ref{Any}(R)
    end
    R = Core.svec(R, R)
    @test summarysize(R) == (depth + 4) * sizeof(Ptr)
end

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

# Issue 14173
module Tmp14173
    export A
    A = randn(2000, 2000)
end
whos(IOBuffer(), Tmp14173) # warm up
const MEMDEBUG = ccall(:jl_is_memdebug, Bool, ())
@test @allocated(whos(IOBuffer(), Tmp14173)) < (MEMDEBUG ? 30000 : 10000)

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

let s = "abcα🐨\0x\0"
    for T in (UInt8, UInt16, UInt32, Int32)
        @test transcode(T, s) == transcode(T, Vector{UInt8}(s))
        @test transcode(String, transcode(T, s)) == s
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

# Base.securezero! functions (#17579)
import Base: securezero!, unsafe_securezero!
let a = [1,2,3]
    @test securezero!(a) === a == [0,0,0]
    a[:] = 1:3
    @test unsafe_securezero!(pointer(a), length(a)) == pointer(a)
    @test a == [0,0,0]
    a[:] = 1:3
    @test unsafe_securezero!(Ptr{Void}(pointer(a)), sizeof(a)) == Ptr{Void}(pointer(a))
    @test a == [0,0,0]
end
let creds = Base.LibGit2.CachedCredentials()
    LibGit2.get_creds!(creds, "foo", LibGit2.SSHCredentials()).pass = "bar"
    securezero!(creds)
    @test LibGit2.get_creds!(creds, "foo", nothing).pass == "\0\0\0"
end

# Test that we can VirtualProtect jitted code to writable
if is_windows()
    @noinline function WeVirtualProtectThisToRWX(x, y)
        x+y
    end

    let addr = cfunction(WeVirtualProtectThisToRWX, UInt64, (UInt64, UInt64))
        addr = addr-(UInt64(addr)%4096)
        const PAGE_EXECUTE_READWRITE = 0x40
        oldPerm = Ref{UInt32}()
        err18083 = ccall(:VirtualProtect,stdcall,Cint,
            (Ptr{Void}, Csize_t, UInt32, Ptr{UInt32}),
            addr, 4096, PAGE_EXECUTE_READWRITE, oldPerm)
        err18083 == 0 && error(Libc.GetLastError())
    end
end

# CRC32c checksum (test data generated from @andrewcooke's CRC.jl package)
for force_software_crc in (1,0)
    ccall(:jl_crc32c_init, Void, (Cint,), force_software_crc)
    for (n,crc) in [(0,0x00000000),(1,0xa016d052),(2,0x03f89f52),(3,0xf130f21e),(4,0x29308cf4),(5,0x53518fab),(6,0x4f4dfbab),(7,0xbd3a64dc),(8,0x46891f81),(9,0x5a14b9f9),(10,0xb219db69),(11,0xd232a91f),(12,0x51a15563),(13,0x9f92de41),(14,0x4d8ae017),(15,0xc8b74611),(16,0xa0de6714),(17,0x672c992a),(18,0xe8206eb6),(19,0xc52fd285),(20,0x327b0397),(21,0x318263dd),(22,0x08485ccd),(23,0xea44d29e),(24,0xf6c0cb13),(25,0x3969bba2),(26,0x6a8810ec),(27,0x75b3d0df),(28,0x82d535b1),(29,0xbdf7fc12),(30,0x1f836b7d),(31,0xd29f33af),(32,0x8e4acb3e),(33,0x1cbee2d1),(34,0xb25f7132),(35,0xb0fa484c),(36,0xb9d262b4),(37,0x3207fe27),(38,0xa024d7ac),(39,0x49a2e7c5),(40,0x0e2c157f),(41,0x25f7427f),(42,0x368c6adc),(43,0x75efd4a5),(44,0xa84c5c31),(45,0x0fc817b2),(46,0x8d99a881),(47,0x5cc3c078),(48,0x9983d5e2),(49,0x9267c2db),(50,0xc96d4745),(51,0x058d8df3),(52,0x453f9cf3),(53,0xb714ade1),(54,0x55d3c2bc),(55,0x495710d0),(56,0x3bddf494),(57,0x4f2577d0),(58,0xdae0f604),(59,0x3c57c632),(60,0xfe39bbb0),(61,0x6f5d1d41),(62,0x7d996665),(63,0x68c738dc),(64,0x8dfea7ae)]
        @test Base.crc32c(UInt8[1:n;]) == crc
    end
end

let
    old_have_color = Base.have_color
    try
        @eval Base have_color = true
        buf = IOBuffer()
        print_with_color(:red, buf, "foo")
        @test startswith(String(take!(buf)), Base.text_colors[:red])
    finally
        @eval Base have_color = $(old_have_color)
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

let
    old_have_color = Base.have_color
    try
        @eval Base have_color = true
        buf = IOBuffer()
        print_with_color(:red, buf, "foo")
        # Check that we get back to normal text color in the end
        @test String(take!(buf)) == "\e[31mfoo\e[39m"

        # Check that boldness is turned off
        print_with_color(:red, buf, "foo"; bold = true)
        @test String(take!(buf)) == "\e[1m\e[31mfoo\e[39m\e[22m"
    finally
        @eval Base have_color = $(old_have_color)
    end
end

abstract DA_19281{T, N} <: AbstractArray{T, N}
Base.convert{S,T,N}(::Type{Array{S, N}}, ::DA_19281{T, N}) = error()
x_19281 = [(), (1,)]
type Foo_19281
    f::Vector{Tuple}
    Foo_19281() = new(x_19281)
end

@testset "test this does not segfault #19281" begin
    @test Foo_19281().f[1] == ()
    @test Foo_19281().f[2] == (1, )
end

let
    x_notdefined = Ref{String}()
    @test !isassigned(x_notdefined)

    x_defined = Ref{String}("Test")
    @test isassigned(x_defined)
end

type Demo_20254
    arr::Array{String}
end

# these cause stack overflows and are a little flaky on CI, ref #20256
if Bool(parse(Int,(get(ENV, "JULIA_TESTFULL", "0"))))
    function Demo_20254(arr::AbstractArray=Any[])
        Demo_20254(string.(arr))
    end

    _unsafe_get_19433(x::NTuple{1}) = (unsafe_get(x[1]),)
    _unsafe_get_19433(xs::Vararg) = (unsafe_get(xs[1]), _unsafe_get_19433(xs[2:end])...)

    f_19433(f_19433, xs...) = f_19433(_unsafe_get_19433(xs)...)

    @testset "test this does not crash, issue #19433 and #20254" begin
        @test_throws StackOverflowError Demo_20254()
        @test_throws StackOverflowError f_19433(+, 1, 2)
    end
end
