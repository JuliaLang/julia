using Compat
import Compat.String
import Compat.view
@compat import Base.show
using Base.Test

v = 1
@test_throws AssertionError @assert(v < 1)

eval(Expr(:type, true, :TestCustomShowType, quote end))
@compat function show(io::IO, ::MIME"text/plain", ::TestCustomShowType)
    print(io, "MyTestCustomShowType")
end
myio = IOBuffer()
display(TextDisplay(myio), MIME"text/plain"(), TestCustomShowType())
@test @compat String(myio) == "MyTestCustomShowType"

eval(Expr(:type, true, :TestCustomShowType2, quote end))
@compat Base.show(io::IO, ::MIME"text/plain", ::TestCustomShowType2) = print(io, "MyTestCustomShowType2")
myio = IOBuffer()
display(TextDisplay(myio), MIME"text/plain"(), TestCustomShowType2())
@test @compat String(myio) == "MyTestCustomShowType2"

eval(Expr(:type, true, :TestCustomShowType3, quote end))
@compat show(io::IO, ::TestCustomShowType3) = print(io, "2-Argument-show")
myio = IOBuffer()
display(TextDisplay(myio), TestCustomShowType3())
@test @compat String(myio) == "2-Argument-show"

eval(Expr(:type, false, :(ParameterizedShowType{T}), quote
    _::T
end))
myio = IOBuffer()
@compat show{T}(io::IO, ::MIME"text/html", ::ParameterizedShowType{T}) =
    print(io, "<code>::", T, "</code>")
@compat show(myio, MIME("text/html"), ParameterizedShowType(0.0))
@test @compat String(myio) == "<code>::Float64</code>"

d = Dict(zip([1, 2], [3, 4]))
ns = length(d.slots)
@test length(sizehint!(d, ns + 1).slots) > ns

@test @compat split("a,b,,c", ',', limit=2) == ["a", "b,,c"]
@test @compat split("a,b,,c", ',', limit=2,keep=true) == ["a", "b,,c"]
@test @compat split("a,b,,c", ',', keep=false) == ["a", "b", "c"]
@test @compat split("a,b,,c", ',', keep=true) == ["a", "b", "", "c"]

@test @compat rsplit("a,b,,c", ',', limit=2) == ["a,b,", "c"]
@test @compat rsplit("a,b,,c", ',', limit=2,keep=true) == ["a,b,", "c"]
@test @compat rsplit("a,b,,c", ',', keep=false) == ["a", "b", "c"]
@test @compat rsplit("a,b,,c", ',', keep=true) == ["a", "b", "", "c"]

if VERSION < v"0.4.0-dev+1387"
    @test isdefined(Main, :AbstractString)
end

@test round(Int, 3//4) == 1
@test round(Int, 1) == 1
@test round(Int, 1.1) == 1
@test ceil(Int, 1) == 1
@test ceil(Int, 1.1) == 2
@test floor(Int, 1) == 1
@test floor(Int, 1.1) == 1
@test trunc(Int, 1) == 1
@test trunc(Int, 1.1) == 1

if VERSION < v"0.6.0-dev.1825"
    @test round(Int, [1, 1]) == [1, 1]
    @test round(Int, [1.1, 1.1]) == [1, 1]
    @test round(Int, [1 1]) == [1 1]
    @test round(Int, [1.1 1.1]) == [1 1]
    @test round(Int, fill(1.1, 2, 3, 4)) == fill(1, 2, 3, 4)
    @test ceil(Int, [1, 1]) == [1, 1]
    @test ceil(Int, [1.1, 1.1]) == [2, 2]
    @test ceil(Int, [1 1]) == [1 1]
    @test ceil(Int, [1.1 1.1]) == [2 2]
    @test ceil(Int, fill(1.1, 2, 3, 4)) == fill(2, 2, 3, 4)
    @test floor(Int, [1, 1]) == [1, 1]
    @test floor(Int, [1.1, 1.1]) == [1, 1]
    @test floor(Int, [1 1]) == [1 1]
    @test floor(Int, [1.1 1.1]) == [1 1]
    @test floor(Int, fill(1.1, 2, 3, 4)) == fill(1, 2, 3, 4)
    @test trunc(Int, [1, 1]) == [1, 1]
    @test trunc(Int, [1.1, 1.1]) == [1, 1]
    @test trunc(Int, [1 1]) == [1 1]
    @test trunc(Int, [1.1 1.1]) == [1 1]
    @test trunc(Int, fill(1.1, 2, 3, 4)) == fill(1, 2, 3, 4)
end

# n % Type
for T in Any[Int16, Int32, UInt32, Int64, UInt64]
    if !(T <: Unsigned)
        @test convert(T, -200) %  Int8 === @compat Int8(56)
        @test convert(T, -200) % UInt8 === 0x38
        @test convert(T, -300) %  Int8 === @compat Int8(-44)
        @test convert(T, -300) % UInt8 === 0xd4
        @test convert(T, -128) %  Int8 === @compat Int8(-128)
        @test convert(T, -128) % UInt8 === 0x80
    end
    @test convert(T,  127) %  Int8 === @compat Int8(127)
    @test convert(T,  127) % UInt8 === 0x7f
    @test convert(T,  128) %  Int8 === @compat Int8(-128)
    @test convert(T,  128) % UInt8 === 0x80
    @test convert(T,  200) %  Int8 === @compat Int8(-56)
    @test convert(T,  300) % UInt8 === 0x2c
end

@test IPv4("1.2.3.4") == ip"1.2.3.4"
@test IPv6("2001:1:2:3::1") == ip"2001:1:2:3::1"
@test isless(ip"1.2.3.4", ip"1.2.3.5")

@test startswith("abcdef","abc") == true
@test startswith("abcdef","def") == false

@test size(bitrand(3, 4)) == (3, 4)
@test size(bitrand((3, 4))) == (3, 4)
@test size(bitrand(MersenneTwister(0), 3, 4)) == (3, 4)
@test size(bitrand(MersenneTwister(0), (3, 4))) == (3, 4)
@test rand(Bool) in [false, true]

rng = MersenneTwister(0)
srand()
srand(rng, UInt32[0,0])
srand(rng)
for Tr in (Int8,UInt8,Int32,UInt32,Int64,UInt64,Int128,UInt128,Float16,Float32,Float64)
    for T in (Tr, Complex{Tr})
        @test isa(rand(rng, T), T)
        let x = rand(rng, T, 3,4)
            @test isa(x, Array{T,2})
            @test size(x) == (3,4)
            rand!(rng, x)
        end
    end
end
srand(rng, 0)
let x = rand(rng, Int64, 3,4)
    srand(rng, 0)
    @test x == rand(rng, Int64, (3,4))
end

extrapath = Compat.Sys.iswindows() ? joinpath(JULIA_HOME,"..","Git","usr","bin")*";" : ""
@compat withenv("PATH" => extrapath * ENV["PATH"]) do
    cmd1 = pipeline(`echo hello`, `sort`)
    cmd2 = pipeline(`true`, `true`)
    if Compat.Sys.iswindows()
        try # use busybox-w32
            success(`busybox`)
            cmd1 = pipeline(`busybox echo hello`, `busybox sort`)
            cmd2 = pipeline(`busybox true`, `busybox true`)
        end
    end
    @test readstring(cmd1) == "hello\n"
    @test success(cmd2)
end

let convert_funcs_and_types =
    ((:integer, :Integer), (:signed, :Signed), (:unsigned, :Unsigned),
     (:int, :Int), (:int8, :Int8), (:int16, :Int16), (:int32, :Int32),
     (:int64, :Int64), (:int128, :Int128), (:uint, :UInt),
     (:uint8, :UInt8), (:uint16, :UInt16), (:uint32, :UInt32),
     (:uint64, :UInt64), (:uint128, :UInt128),
     (:float16, :Float16), (:float32, :Float32), (:float64, :Float64),
     (:complex32,:Complex32), (:complex64,:Complex64),(:complex128,:Complex128),
     (:char,:Char))

    for (df,t) in convert_funcs_and_types
        x = @compat UInt8(10)
        r1 = eval(:(@compat($t($x))))
        ty = eval(t)
        if ty.abstract
            @test issubtype(typeof(r1),ty)
        else
            @test typeof(r1) === ty
        end
        if VERSION <  v"0.4.0-dev+3732"
            r2 = eval(df)(x)
            @test r1 === r2
            if t === :Signed || t === :Complex32
                continue
            end
            x = fill(x, 10)
            r1 = eval(:(@compat map($t, $x)))
            r2 = eval(df)(x)
            @test r1 == r2
            @test typeof(r1) === typeof(r2)
        end
    end
    @test (@compat Bool(1))
    @test !(@compat Bool(0))

    # issue #54
    c = @compat Complex128(1)
    @test c.re == 1
    @test c.im == 0

    c = @compat Complex128(1,2)
    @test c.re == 1
    @test c.im == 2

    c = @compat Complex128(1+2im)
    @test c.re == 1
    @test c.im == 2
end

eval(Expr(:type, true, :Test3609, quote
    a
    b
end))

if VERSION < v"0.4.0-dev+3609"
    let v = Test3609(1,2)
        @test fieldnames(Test3609) == fieldnames(v) == Symbol[:a, :b]
    end
end

@test fieldoffset(Complex{Float32}, 2) === @compat UInt(4)

@test parse(Int8, '9') == convert(Int8, 9)
@test parse(Int, 'a', 16) == 10
@test parse(Int, "200") == 200
@test parse(Int16, "1101", 2) == convert(Int16, 13)
@test parse(Float64, "222") == 222.0
@test parse(Float32, "1.1") == convert(Float32, 1.1)
@test parse(BigFloat, "1.125") == convert(BigFloat, 1.125)
@test isa(tryparse(Float32, "1.1"), Nullable)
@test get(tryparse(Float32, "1.1")) == 1.1f0
@test isa(tryparse(Float32, "a"), Nullable{Float32})
@test isa(tryparse(Float64, "1.1"), Nullable)
@test get(tryparse(Float64, "1.1")) == 1.1
@test isa(tryparse(Float64, "a"), Nullable{Float64})
@test get(tryparse(Int32, "1")) == 1
@test isa(tryparse(Int32, "a"), Nullable{Int32})
@test get(tryparse(Int64, "1")) == 1
@test isa(tryparse(Int64, "a"), Nullable{Int64})

@test_throws ArgumentError tryparse(Int32, "0", 1)
@test tryparse(Int32, "12345", 16) === Nullable{Int32}(@compat Int32(74565))
@test tryparse(Int64, "12345", 10) === Nullable{Int64}(@compat Int64(12345))
@test tryparse(Int64, "12345", 6) === Nullable{Int64}(@compat Int64(1865))
@test isnull(tryparse(Int64, "nonsense", 10))
@test tryparse(Int64, "nonsense", 36) === Nullable{Int64}(@compat Int64(1856056985582))

# Make sure exports from Libc and Libdl are defined
for x in [:strftime,:systemsleep,:getpid,:FILE,:malloc,:flush_cstdio,:realloc,:strptime,:Libc,:errno,:TmStruct,:calloc,:time,:strerror,:gethostname,:free]
    getfield(Libc, x)
end
for x in [:RTLD_LOCAL,:RTLD_GLOBAL,:find_library,:dlsym,:RTLD_LAZY,:RTLD_NODELETE,:DL_LOAD_PATH,:RTLD_NOW,:Libdl,:dlext,:dlsym_e,:RTLD_FIRST,:dlopen,:dllist,:dlpath,:RTLD_NOLOAD,:dlclose,:dlopen_e,:RTLD_DEEPBIND]
    getfield(Libdl, x)
end

# Test unsafe_convert
eval(Expr(:type, true, :Au_c, quote end))
x = "abc"
@test @compat String(unsafe_string(Compat.unsafe_convert(Ptr{UInt8}, x))) == x
Compat.unsafe_convert(::Ptr{Au_c}, x) = x
@test Compat.unsafe_convert(pointer([Au_c()]), 1) == 1

# Test Ptr{T}(0)
@test @compat(Ptr{Int}(0)) == C_NULL

# Test Tuple{} syntax
if VERSION < v"0.4.0-dev+4319"
    @test @compat Tuple{1} == (1,)
    @test @compat Tuple{:a, :b} == (:a, :b)
    @test @compat Tuple{:a, Tuple{:b}} == (:a, (:b,))
    @test @compat Tuple{:a, Tuple{:b, :c}} == (:a, (:b, :c))
    @test @compat Tuple{Int} == (Int,)
    @test @compat Tuple{Int, Float64} == (Int, Float64)
    @test @compat Tuple{Int, Tuple{Float64}} == (Int, (Float64,))
    @test @compat Tuple{Int, Tuple{Float64, Char}} == (Int, (Float64, Char))
    @test @compat Tuple{Int, Vararg{Float64}} == (Int, Float64...)
    # Issue 81
    a81 = [Int, Int]
    b81 = (Int, Int)
    @test @compat Tuple{a81..., Vararg{Float64}} == (Int, Int, Float64...)
    @test @compat Tuple{b81..., Vararg{Float64}} == (Int, Int, Float64...)
end

# Ensure eachindex iterates over the whole array
let A, B, s
    A = reshape(1:20,4,5)
    s = 0
    for i in eachindex(A)
        s += A[i]
    end
    @test s == 210
    B = sparse(A)
    s = 0
    for i in eachindex(B)
        s += B[i]
    end
    @test s == 210
end

let
    d = @compat Dict(1=>2, 3=>4)
    d[5] = 6
    val = 0
    for I in eachindex(d)
        val += d[I]
    end
    @test val == 12
    empty!(d)
    for I in eachindex(d)
        val += d[I]
    end
    @test val == 12
end

# findlast, findprev
let a = [0,1,2,3,0,1,2,3]
    @test findlast(a) == 8
    @test findlast(a.==0) == 5
    @test findlast(a.==5) == 0
    @test findlast([1,2,4,1,2,3,4], 3) == 6
    @test findlast(isodd, [2,4,6,3,9,2,0]) == 5
    @test findlast(isodd, [2,4,6,2,0]) == 0
    @test findprev(a,4) == 4
    @test findprev(a,5) == 4
    @test findprev(a,1) == 0
    @test findprev(a,1,4) == 2
    @test findprev(a,1,8) == 6
    @test findprev(isodd, [2,4,5,3,9,2,0], 7) == 5
    @test findprev(isodd, [2,4,5,3,9,2,0], 2) == 0
end

# isdiag
@test isdiag(diagm([1,2,3,4]))
@test !isdiag([1 2; 3 4])
@test isdiag(5)

# keytype & valtype
if VERSION < v"0.4.0-dev+4502"
    @test keytype(@compat(Dict(1 => 1.))) == Int
    @test valtype(@compat(Dict(1 => 1.))) == Float64
end

# Val
begin
    local firstlast
    firstlast(::Val{true}) = "First"
    firstlast(::Val{false}) = "Last"

    @test firstlast(Val(true)) == "First"
    @test firstlast(Val(false)) == "Last"
end

# The constructors for some linear algebra stuff changed to take Val{x}
# instead of Type{Val{x}}
const valtrue = VERSION < v"0.7.0-DEV.843" ? Val{true} : Val(true)
const valfalse = VERSION < v"0.7.0-DEV.843" ? Val{false} : Val(false)

# qr, qrfact, qrfact!
let A = [1.0 2.0; 3.0 4.0]
    Q, R = qr(A, valfalse)
    @test Q*R ≈ A
    Q, R, p = qr(A, valtrue)
    @test Q*R ≈ A[:,p]
    F = qrfact(A, valfalse)
    @test F[:Q]*F[:R] ≈ A
    F = qrfact(A, valtrue)
    @test F[:Q]*F[:R] ≈ A[:,F[:p]]
    A_copy = copy(A)
    F = qrfact!(A_copy, valfalse)
    @test F[:Q]*F[:R] ≈ A
    A_copy = copy(A)
    F = qrfact!(A_copy, valtrue)
    @test F[:Q]*F[:R] ≈ A[:,F[:p]]
end

# Cstring
let s = "foo"
    # note: need cconvert in 0.5 because of JuliaLang/julia#16893
    @test reinterpret(Ptr{Cchar}, Compat.unsafe_convert(Cstring, VERSION < v"0.4" ? s : Base.cconvert(Cstring, s))) == pointer(s)
    if VERSION < v"0.5.0-dev+4859"
        let w = wstring("foo")
            @test reinterpret(Ptr{Cwchar_t}, Compat.unsafe_convert(Cwstring, w)) == pointer(w)
        end
    end
end

# fma and muladd
@test fma(3,4,5) == 3*4+5 == muladd(3,4,5)

if VERSION < v"0.5.0-dev+5271"
    # is_valid_utf32
    s = utf32("abc")
    @test isvalid(s)
    s = utf32(UInt32[65,0x110000])
    @test !isvalid(s)

    # isvalid
    let s = "abcdef", u8 = "abcdef\uff", u16 = utf16(u8), u32 = utf32(u8),
        bad32 = utf32(UInt32[65,0x110000]), badch = Char[0x110000][1]

        @test !isvalid(bad32)
        @test !isvalid(badch)
        @test isvalid(s)
        @test isvalid(u8)
        @test isvalid(u16)
        @test isvalid(u32)
        @test isvalid(Compat.ASCIIString, s)
        @test isvalid(Compat.UTF8String,  u8)
        @test isvalid(UTF16String, u16)
        @test isvalid(UTF32String, u32)
    end
end

if VERSION < v"0.5.0-dev+907"
    # chol
    let A = rand(2,2)
        B = A'*A
        U = @compat chol(B, Val{:U})
        @test U'*U ≈ B
    end
end

# @generated
if VERSION > v"0.3.99"
    let
        @compat @generated function foo(x)
            T = x
            :(return $T)
        end
        @test foo(5) == Int
    end
end

# Timer
let c = 0, f, g, t
    @compat f(t::Timer) = (c += 1)
    @compat g(t::Base.Timer) = (c += 1)
    t = Timer(f, 0.0, 0.05)
    sleep(0.05)
    @test c >= 1
    sleep(0.1)
    @test c >= 3
    close(t)
    sleep(0.1)
    val = c
    sleep(0.1)
    @test val == c
    t = Timer(g, 0.0, 0.05)
    sleep(0.05)
    @test c >= 2
    close(t)
end

# MathConst -> Irrational
f(::Irrational) = true
@test f(π)
Compat.@irrational mathconst_one 1.0 big(1.)
@test f(mathconst_one)

# gc_enable(::Bool)
@test gc_enable(false)
@test !gc_enable(true)

# Vector{Int}(), Array{Int}

@test @compat typeof(Vector{Int}()) == Array{Int,1}
@test @compat length(Vector{Int}()) == 0

@test @compat typeof(Vector{Int8}(10)) == Array{Int8,1}
@test @compat length(Vector{Int8}(10)) == 10

@test @compat typeof(Array{UInt16}()) == Array{UInt16,0}
@test @compat length(Array{UInt16}()) == 1

@test @compat typeof(Array{UInt16}(0)) == Array{UInt16,1}
@test @compat length(Array{UInt16}(0)) == 0

@test @compat typeof(Array{Float16}(5)) == Array{Float16,1}
@test @compat length(Array{Float16}(5)) == 5

@test @compat typeof(Array{AbstractString}(2,2)) == Array{AbstractString,2}
@test @compat size(Array{AbstractString}(2,2)) == (2,2)

@test @compat typeof(Array{Rational{Int}}(2,2,2,2,2)) == Array{Rational{Int},5}
@test @compat size(Array{Rational{Int}}(2,2,2,2,2)) == (2,2,2,2,2)

@compat String(Mmap.mmap(@__FILE__(),Vector{UInt8},11,1)) == "sing Compat"

@test base64encode("hello world") == "aGVsbG8gd29ybGQ="

@test nothing === __precompile__(false) # tests should never be precompiled
@test nothing === __precompile__()
@test nothing === include_dependency("foo")

@test real(Int) == real(Complex{Int}) == Int

@test isa(1.2, AbstractFloat)

@test [1,2,3] ≈ [1,2,3+1e-9]
@test [0,1] ≈ [1e-9, 1]
@test [0,1] ≉ [1e-3, 1]

# linspace (some of the julia 0.4 tests)
@test [Compat.linspace(0.1,0.3,3);]     == [1:3;]./10
@test [Compat.linspace(0.0,0.3,4);]     == [0:3;]./10
@test [Compat.linspace(0.3,-0.1,5);]    == [3:-1:-1;]./10
@test [Compat.linspace(0.1,-0.3,5);]    == [1:-1:-3;]./10
@test [Compat.linspace(0.0,1.0,11);]    == [0:10;]./10
@test [Compat.linspace(0.0,1.0,0);]     == []
@test [Compat.linspace(0.0,-1.0,0);]    == []
@test [Compat.linspace(0.0,-1.0,11);]   == [0:-1:-10;]./10
@test [Compat.linspace(1.0,27.0,1275);] == [49:1323;]./49
@test [Compat.linspace(0.0,2.1,4);]     == [0:7:21;]./10
@test [Compat.linspace(0.0,3.3,4);]     == [0:11:33;]./10
@test [Compat.linspace(0.1,3.4,4);]     == [1:11:34;]./10
@test [Compat.linspace(0.0,3.9,4);]     == [0:13:39;]./10
@test [Compat.linspace(0.1,4.0,4);]     == [1:13:40;]./10
@test [Compat.linspace(1.1,3.3,3);]     == [11:11:33;]./10
@test [Compat.linspace(0.3,1.1,9);]     == [3:1:11;]./10
@test [Compat.linspace(0.0,0.0,1);]     == [0.0]
@test [Compat.linspace(0.0,0.0,1);]     == [0.0]

for T = (Float32, Float64)
    z = zero(T)
    u = eps(z)
    @test first(Compat.linspace(u,u,0)) == u
    @test last(Compat.linspace(u,u,0)) == u
    @test first(Compat.linspace(-u,u,0)) == -u
    @test last(Compat.linspace(-u,u,0)) == u
    @test [Compat.linspace(-u,u,0);] == []
    @test [Compat.linspace(-u,-u,1);] == [-u]
    @test [Compat.linspace(-u,u,2);] == [-u,u]
    @test [Compat.linspace(-u,u,3);] == [-u,0,u]
    @test first(Compat.linspace(-u,-u,0)) == -u
    @test last(Compat.linspace(-u,-u,0)) == -u
    @test first(Compat.linspace(u,-u,0)) == u
    @test last(Compat.linspace(u,-u,0)) == -u
    @test [Compat.linspace(u,-u,0);] == []
    @test [Compat.linspace(u,u,1);] == [u]
    @test [Compat.linspace(u,-u,2);] == [u,-u]
    @test [Compat.linspace(u,-u,3);] == [u,0,-u]
    v = [Compat.linspace(-u,u,12);]
    @test length(v) == 12
    @test [-3u:u:3u;] == [Compat.linspace(-3u,3u,7);] == [-3:3;].*u
    @test [3u:-u:-3u;] == [Compat.linspace(3u,-3u,7);] == [3:-1:-3;].*u
end

if VERSION < v"0.4.0-dev+768"
    @test @compat(Void) === Nothing
else
    @test @compat(Void) === Void
end
@test Ptr{Void} == @compat(Ptr{Void})

# MemoryError -> OutOfMemoryError
# Base64Pipe -> Base64EncodePipe
# UdpSocket -> UDPSocket
f141(::Type{OutOfMemoryError}) = true
f141(::Type{Base64EncodePipe}) = true
f141(::Type{UDPSocket}) = true
f141(::Type{TCPSocket}) = true

@test f141(OutOfMemoryError)
@test f141(Base64EncodePipe)
@test f141(UDPSocket)
@test f141(TCPSocket)

# Union syntax
if VERSION < v"0.4.0-dev+5379"
    @test @compat(Union{}) == None
    @test @compat(Union{Int,Float64}) == Union(Int,Float64)
    @test @compat(:(Union{})) == :(Union())
end

@test fetch(remotecall(() -> true, 1))
@test remotecall_fetch(() -> true, 1)
@test fetch(remotecall_wait(() -> true, 1))
Base.remote_do(() -> true, 1) # Doesn't return anything so cannot be `@test`ed but should print some output if it fails

# JuliaLang/julia#13440
@test isa(SparseArrays, Module)

# JuliaLang/julia#12819
@test Compat.Filesystem.JL_O_RDWR == Compat.Filesystem.JL_O_RDWR

# JuliaLang/julia#14338
@test supertype(Int) == Signed

# withenv
@test "1234" == @compat withenv(() -> ENV["_TESTVAR"], "_TESTVAR" => 1234)
@test "1234" == @compat withenv("_TESTVAR" => 1234) do
    return ENV["_TESTVAR"]
end

# Test functional form of mktemp and mktempdir
let
    tmp_path = mktemp() do p, io
        @test isfile(p)
        print(io, "鴨かも？")
        p
    end
    @test tmp_path != ""
    @test !isfile(tmp_path)
end

let
    tmpdir = mktempdir() do d
        @test isdir(d)
        d
    end
    @test tmpdir != ""
    @test !isdir(tmpdir)
end

# https://github.com/JuliaLang/julia/pull/14660
mktempdir() do dir

    verbose = false

    # Create test file...
    filename = joinpath(dir, "file.txt")
    text = "C1,C2\n1,2\na,b\n"

    # List of IO producers...
    l = [
        ("IOStream", (text) -> begin
            write(filename, text)
            open(filename)
        end),
        ("IOBuffer", (text)->IOBuffer(text))
    ]

    open_streams = Any[]
    function cleanup()
        for s in open_streams
            try close(s) end
        end
    end

    for (name, f) in l

        io = ()->(s=f(text); push!(open_streams, s); s)

        write(filename, text)

        verbose && println("$name read...")
        @test read(io(), UInt8) == read(IOBuffer(text), UInt8)
        @test read(io(), UInt8) == read(filename, UInt8)
        @test read(io(), Int) == read(IOBuffer(text), Int)
        @test read(io(), Int) == read(filename,Int)
        s1 = io()
        s2 = IOBuffer(text)
        @test read(s1, UInt32, 2) == read(s2, UInt32, 2)
        @test !eof(s1)
        @test read(s1, UInt8, 5) == read(s2, UInt8, 5)
        @test !eof(s1)
        @test read(s1, UInt8, 1) == read(s2, UInt8, 1)
        @test eof(s1)
        @test_throws EOFError read(s1, UInt8)
        @test eof(s1)
        close(s1)
        close(s2)

        verbose && println("$name eof...")
        n = length(text) - 1
        @test @compat read!(io(), Vector{UInt8}(n)) ==
              read!(IOBuffer(text), Vector{UInt8}(n))
        @test @compat (s = io(); read!(s, Vector{UInt8}(n)); !eof(s))
        n = length(text)
        @test @compat read!(io(), Vector{UInt8}(n)) ==
              read!(IOBuffer(text), Vector{UInt8}(n))
        @test @compat (s = io(); read!(s, Vector{UInt8}(n)); eof(s))
        n = length(text) + 1
        @test_throws EOFError @compat read!(io(), Vector{UInt8}(n))
        @test_throws EOFError @compat read!(io(), Vector{UInt8}(n))

        old_text = text
        cleanup()
        const SZ_UNBUFFERED_IO = 65536

        for text in [
            old_text,
            convert(String, Char['A' + i % 52 for i in 1:(div(SZ_UNBUFFERED_IO,2))]),
            convert(String, Char['A' + i % 52 for i in 1:(    SZ_UNBUFFERED_IO -1)]),
            convert(String, Char['A' + i % 52 for i in 1:(    SZ_UNBUFFERED_IO   )]),
            convert(String, Char['A' + i % 52 for i in 1:(    SZ_UNBUFFERED_IO +1)])
        ]

            write(filename, text)

            verbose && println("$name readstring...")
            @test readstring(io()) == text

            @test readstring(io()) == readstring(filename)


            verbose && println("$name read...")
            @test @compat read(io()) == UInt8[convert(UInt8, x) for x in text]

            @test read(io()) == read(filename)

            cleanup()


            verbose && println("$name readbytes!...")
            l = length(text)
            for n = [1, 2, l-2, l-1, l, l+1, l+2]
                @compat a1 = Vector{UInt8}(n);
                @compat a2 = Vector{UInt8}(n)
                s1 = io()
                s2 = IOBuffer(text)
                n1 = readbytes!(s1, a1)
                n2 = readbytes!(s2, a2)
                @test n1 == n2
                @test length(a1) == length(a2)
                @test a1[1:n1] == a2[1:n2]
                @test n <= length(text) || eof(s1)
                @test n <= length(text) || eof(s2)

                cleanup()
            end

            verbose && println("$name read!...")
            l = length(text)
            for n = [1, 2, l-2, l-1, l]
                @test @compat  read!(io(), Vector{UInt8}(n)) ==
                      read!(IOBuffer(text), Vector{UInt8}(n))
                @test @compat read!(io(), Vector{UInt8}(n)) ==
                      read!(filename, Vector{UInt8}(n))

                cleanup()
            end
            @test_throws EOFError @compat read!(io(), Vector{UInt8}(length(text)+1))


            verbose && println("$name readuntil...")
            @test readuntil(io(), '\n') == readuntil(IOBuffer(text),'\n')
            @test readuntil(io(), '\n') == readuntil(filename,'\n')
            @test readuntil(io(), "\n") == readuntil(IOBuffer(text),"\n")
            @test readuntil(io(), "\n") == readuntil(filename,"\n")
            @test readuntil(io(), ',')  == readuntil(IOBuffer(text),',')
            @test readuntil(io(), ',')  == readuntil(filename,',')

            cleanup()

            verbose && println("$name readline...")
            @test readline(io()) == readline(IOBuffer(text))
            @test readline(io()) == readline(filename)

            verbose && println("$name readlines...")
            @test readlines(io()) == readlines(IOBuffer(text))
            @test readlines(io()) == readlines(filename)
            @test collect(eachline(io())) == collect(eachline(IOBuffer(text)))
            @test collect(eachline(io())) == collect(eachline(filename))

            cleanup()

            verbose && println("$name countlines...")
            @test countlines(io()) == countlines(IOBuffer(text))

            # verbose && println("$name readcsv...")
            # @test readcsv(io()) == readcsv(IOBuffer(text))
            # @test readcsv(io()) == readcsv(filename)

            cleanup()
        end

        text = old_text
        write(filename, text)

        verbose && println("$name position...")
        @test @compat (s = io(); read!(s, Vector{UInt8}(4)); position(s))  == 4

        verbose && println("$name seek...")
        for n = 0:length(text)-1
            @test readlines(seek(io(), n)) == readlines(seek(IOBuffer(text), n))
            cleanup()
        end
        verbose && println("$name skip...")
        for n = 0:length(text)-1
            @test readlines(seek(io(), n)) == readlines(seek(IOBuffer(text), n))
            @test readlines(skip(io(), n)) == readlines(skip(IOBuffer(text), n))
            cleanup()
        end
        verbose && println("$name seekend...")
        @test readstring(seekend(io())) == ""

        verbose && println("$name write(::IOStream, ...)")
        to = open("$filename.to", "w")
        write(to, io())
        close(to)
        @test readstring("$filename.to") == text

        verbose && println("$name write(filename, ...)")
        write("$filename.to", io())
        @test readstring("$filename.to") == text

        verbose && println("$name write(::IOBuffer, ...)")
        @compat to = IOBuffer(UInt8[convert(UInt8, x) for x in text], false, true)
        write(to, io())
        @test String(take!(to)) == text

        cleanup()
    end


# https://github.com/JuliaLang/julia/pull/13232

setprecision(BigFloat, 100)
@test precision(BigFloat) == 100
setprecision(256)
@test precision(BigFloat) == 256

setprecision(BigFloat, 100) do
    a = big(pi)
    @test precision(a) == 100
end

setprecision(130) do
    a = big(pi)
    @test precision(a) == 130
end

for T in (BigFloat, Float64)
    setrounding(T, RoundDown)
    @test rounding(T) == RoundDown
    setrounding(T, RoundNearest)

    setrounding(T, RoundUp) do
        @test rounding(T) == RoundUp
    end
end

end

@test typeof(displaysize()) == @compat(Tuple{Int, Int})

@test Compat.LinAlg.checksquare(randn(4,4)) == 4
@test Compat.LinAlg.checksquare(randn(4,4), randn(3,3)) == [4,3]
@test_throws DimensionMismatch Compat.LinAlg.checksquare(randn(4,3))

@test issymmetric([1 2 3; 2 2 3; 3 3 2])
@test !issymmetric([1 3 3; 2 2 3; 3 3 2])

let X = randn(10,2), Y = randn(10,2), x = randn(10), y = randn(10)
    for b in (true, false)
        if VERSION < v"0.5.0-dev+679"
            @test cov(x, b) == cov(x, corrected=b)
        end
        for d in (1, 2)
            @test size(cov(X, d), 1) == 8*d - 6
            @test size(cov(X, d, b), 1) == 8*d - 6
            @test size(cov(X, Y, d), 1) == 8*d - 6
            @test size(cov(X, Y, d, b), 1) == 8*d - 6

            @test size(cor(X, d), 1) == 8*d - 6
            @test size(cor(X, Y, d), 1) == 8*d - 6
        end
    end
end

# foreach
let
    a = Any[]
    foreach(()->push!(a,0))
    @test a == [0]
    a = Any[]
    foreach(x->push!(a,x), [1,5,10])
    @test a == [1,5,10]
    a = Any[]
    foreach((args...)->push!(a,args), [2,4,6], [10,20,30])
    @test a == [(2,10),(4,20),(6,30)]
end

@test istextmime("text/html") && !istextmime("image/png")

module CallTest

using Base.Test, Compat

eval(Expr(:type, false, :A, quote
    a
end))

eval(Expr(:type, false, :(B{T}), quote
    b::T
end))

if VERSION >= v"0.4"
    @compat (::Type{A})() = A(1)
    @compat (::Type{B})() = B{Int}()
    @compat (::Type{B{T}}){T}() = B{T}(zero(T))

    @compat (a::A)() = a.a
    @compat (a::A)(b) = (a.a, b)
    @compat (a::A)(b, c; d=2) = (a.a, b, c, d)
    @compat (b::B{T}){T}() = b.b, T
    @compat (b::B{T}){T}(c::T) = 1
    @compat (b::B{T}){T,T2}(c::T2) = 0
    @compat (b::B{T}){T}(c::T, d; f=1) = (c, d, f)

    @test A() === A(1)
    @test B() === B(0)
    @test B{Float64}() === B(0.0)

    @test A(1)() === 1
    @test A(1)(2) === (1, 2)
    @test A(1)(2, 3; d=10) === (1, 2, 3, 10)
    @test B(0)() === (0, Int)
    @test B(0)(1) === 1
    @test B(0)(1, 2; f=100) === (1, 2, 100)
    @test B(0)(1.0) === 0
end

end

# walkdir

dirwalk = mktempdir()
cd(dirwalk) do
    for i=1:2
        mkdir("sub_dir$i")
        open("file$i", "w") do f end

        mkdir(joinpath("sub_dir1", "subsub_dir$i"))
        touch(joinpath("sub_dir1", "file$i"))
    end
    touch(joinpath("sub_dir2", "file_dir2"))
    has_symlinks = Compat.Sys.isunix() ? true : (isdefined(Base, :WINDOWS_VISTA_VER) && Base.windows_version() >= Base.WINDOWS_VISTA_VER)
    follow_symlink_vec = has_symlinks ? [true, false] : [false]
    has_symlinks && symlink(abspath("sub_dir2"), joinpath("sub_dir1", "link"))
    for follow_symlinks in follow_symlink_vec
        chnl = walkdir(".", follow_symlinks=follow_symlinks)
        root, dirs, files = take!(chnl)
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]

        root, dirs, files = take!(chnl)
        @test root == joinpath(".", "sub_dir1")
        @test dirs == (has_symlinks ? ["link", "subsub_dir1", "subsub_dir2"] : ["subsub_dir1", "subsub_dir2"])
        @test files == ["file1", "file2"]

        root, dirs, files = take!(chnl)
        if follow_symlinks
            @test root == joinpath(".", "sub_dir1", "link")
            @test dirs == []
            @test files == ["file_dir2"]
            root, dirs, files = take!(chnl)
        end
        for i=1:2
            @test root == joinpath(".", "sub_dir1", "subsub_dir$i")
            @test dirs == []
            @test files == []
            root, dirs, files = take!(chnl)
        end

        @test root == joinpath(".", "sub_dir2")
        @test dirs == []
        @test files == ["file_dir2"]
    end

    for follow_symlinks in follow_symlink_vec
        chnl = walkdir(".", follow_symlinks=follow_symlinks, topdown=false)
        root, dirs, files = take!(chnl)
        if follow_symlinks
            @test root == joinpath(".", "sub_dir1", "link")
            @test dirs == []
            @test files == ["file_dir2"]
            root, dirs, files = take!(chnl)
        end
        for i=1:2
            @test root == joinpath(".", "sub_dir1", "subsub_dir$i")
            @test dirs == []
            @test files == []
            root, dirs, files = take!(chnl)
        end
        @test root == joinpath(".", "sub_dir1")
        @test dirs ==  (has_symlinks ? ["link", "subsub_dir1", "subsub_dir2"] : ["subsub_dir1", "subsub_dir2"])
        @test files == ["file1", "file2"]

        root, dirs, files = take!(chnl)
        @test root == joinpath(".", "sub_dir2")
        @test dirs == []
        @test files == ["file_dir2"]

        root, dirs, files = take!(chnl)
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]
    end
    #test of error handling
    chnl_error = walkdir(".")
    chnl_noerror = walkdir(".", onerror=x->x)
    root, dirs, files = take!(chnl_error)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    rm(joinpath("sub_dir1"), recursive=true)
    @test_throws SystemError take!(chnl_error) # throws an error because sub_dir1 do not exist

    root, dirs, files = take!(chnl_noerror)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    root, dirs, files = take!(chnl_noerror) # skips sub_dir1 as it no longer exist
    @test root == joinpath(".", "sub_dir2")
    @test dirs == []
    @test files == ["file_dir2"]

end
rm(dirwalk, recursive=true)

# RemoteChannel/Future
r = remotecall(sin, 1, pi/3)
@compat foo(r::Future) = 7
@test foo(r) == 7

@compat rc = RemoteChannel()
@compat rc = RemoteChannel(myid())

@compat rc = Future()
@compat rc = Future(myid())

# @functorize
function checkfunc(Fun, func)
    if VERSION >= v"0.5.0-dev+3701"
        @eval @test @functorize($(func)) === Base.$(func)
    else
        if isdefined(Base, Fun)
            @eval @test isa(@functorize($(func)), Base.$(Fun))
        else
            @eval @test isa(@functorize($(func)), Function)
            @eval @test @functorize($(func)) === Base.$(func)
        end
    end
end

for (Fun, func) in [(:IdFun,                   :identity),
                    (:AbsFun,                  :abs),
                    (:Abs2Fun,                 :abs2),
                    (:ExpFun,                  :exp),
                    (:LogFun,                  :log),
                    (:ConjFun,                 :conj)]
    begin
        if isdefined(Base, func)
            checkfunc(Fun, func)
            a = rand(1:10, 10)
            @eval @test mapreduce($(func), +, $(a)) == mapreduce(@functorize($(func)), +, $(a))
        end
    end
end

dotfunctors = [(:DotAddFun,           :.+),
               (:DotSubFun,           :.-),
               (:DotMulFun,           :.*),
               (:DotRDivFun,          :./),
               (:DotIDivFun,          @compat(Symbol(".÷"))),
               (:DotRemFun,           :.%),
               (:DotLSFun,            :.<<),
               (:DotRSFun,            :.>>)]

functors    = [(:AndFun,              :&),
               (:OrFun,               :|),
               (:XorFun,              :⊻),
               (:AddFun,              :+),
               (:SubFun,              :-),
               (:MulFun,              :*),
               (:RDivFun,             :/),
               (:LDivFun,             :\ ),
               (:IDivFun,             :div),
               (:ModFun,              :mod),
               (:RemFun,              :rem),
               (:PowFun,              :^),
               (:MaxFun,              :scalarmax),
               (:MinFun,              :scalarmin),
               (:LessFun,             :<),
               (:MoreFun,             :>),
               (:ElementwiseMaxFun,   :max),
               (:ElementwiseMinFun,   :min)]

# since #17623, dot functions are no longer function objects
if VERSION < v"0.6.0-dev.1614"
    append!(functors, dotfunctors)
end

for (Fun, func) in functors
    begin
        if isdefined(Base, func) && (func !== :.>> || VERSION >= v"0.4.0-dev+553") && (func !== :.% || VERSION >= v"0.5.0-dev+1472")
            checkfunc(Fun, func)
            a = rand(1:10, 10)
            @eval @test mapreduce(identity, Base.$(func), $(a)) == mapreduce(identity, @functorize($(func)), $(a))
        end
    end
end

if VERSION >= v"0.5.0-dev+3701"
    @test @functorize(complex) === complex
    @test @functorize(dot) === dot
else
    if isdefined(Base, :SparseArrays) && isdefined(Base.SparseArrays, :ComplexFun)
        @test isa(@functorize(complex), Base.SparseArrays.ComplexFun)
        @test isa(@functorize(dot), Base.SparseArrays.DotFun)
    else
        @test isa(@functorize(complex), Function)
        @test isa(@functorize(dot), Function)
        @test @functorize(complex) === complex
        @test @functorize(dot) === dot
    end
end
let a = rand(1:10, 10)
    @test mapreduce(identity, dot, a) == mapreduce(identity, @functorize(dot), a)
end

if VERSION < v"0.6.0-dev.2521"
    @test isa(@functorize(centralizedabs2fun)(1), @functorize(centralizedabs2fun))
    @test isa(@functorize(centralizedabs2fun)(1.0), @functorize(centralizedabs2fun))
    let a = rand(1:10, 10)
        @eval @test mapreduce(x -> abs2(x - 1), +, $(a)) == mapreduce(@functorize(centralizedabs2fun)(1), +, $(a))
    end
end

# Threads.@threads
using Compat.Threads
@threads for i=1:10
    @test true
end

# Issue #223
@test 1 == threadid() <= nthreads()

@test @compat(Symbol("foo")) === :foo
@test @compat(Symbol("foo", "bar")) === :foobar
@test @compat(Symbol("a_", 2)) === :a_2
@test @compat(Symbol('c')) === :c
@test @compat(Symbol(1)) === @compat(Symbol("1"))

@test @compat(Base.:+) == +
let x = rand(3), y = rand(3)
    @test @compat(sin.(cos.(x))) == map(x -> sin(cos(x)), x)
    @test @compat(atan2.(sin.(y),x)) == broadcast(atan2,map(sin,y),x)
end
let x0 = Array{Float64}(), v, v0
    x0[1] = rand()
    v0 = @compat sin.(x0)
    @test isa(v0, Array{Float64,0})
    v = @compat sin.(x0[1])
    @test isa(v, Float64)
    @test v == v0[1] == sin(x0[1])
end
let x = rand(2, 2), v
    v = @compat sin.(x)
    @test isa(v, Array{Float64,2})
    @test v == [sin(x[1, 1]) sin(x[1, 2]);
                sin(x[2, 1]) sin(x[2, 2])]
end
let x1 = [1, 2, 3], x2 = ([3, 4, 5],), v
    v = @compat atan2.(x1, x2...)
    @test isa(v, Vector{Float64})
    @test v == [atan2(1, 3), atan2(2, 4), atan2(3, 5)]
end
# Do the following in global scope to make sure inference is able to handle it
@test @compat(sin.([1, 2])) == [sin(1), sin(2)]
@test isa(@compat(sin.([1, 2])), Vector{Float64})
@test @compat(atan2.(1, [2, 3])) == [atan2(1, 2), atan2(1, 3)]
@test isa(@compat(atan2.(1, [2, 3])), Vector{Float64})
@test @compat(atan2.([1, 2], [2, 3])) == [atan2(1, 2), atan2(2, 3)]
@test isa(@compat(atan2.([1, 2], [2, 3])), Vector{Float64})
# And make sure it is actually inferrable
f15032(a) = @compat sin.(a)
@inferred f15032([1, 2, 3])
@inferred f15032([1.0, 2.0, 3.0])

# Issue #291
if VERSION ≥ v"0.5.0-dev+4002"
    @test (1, 2) == @compat abs.((1, -2))
    @test broadcast(+, (1.0, 1.0), (0, -2.0)) == (1.0,-1.0)
end

if VERSION ≥ v"0.4.0-dev+3732"
    @test Symbol("foo") === :foo
    @test Symbol("foo", "bar") === :foobar
    @test Symbol("a_", 2) === :a_2
    @test Symbol('c') === :c
    @test Symbol(1) === Symbol("1")
end

let async, c = false
    run = Condition()
    async = Compat.AsyncCondition(x->(c = true; notify(run)))
    ccall(:uv_async_send, Void, (Ptr{Void},), async.handle)
    wait(run)
    @test c
end

let async, c = false
    async = Compat.AsyncCondition()
    started = Condition()
    task = @schedule begin
        notify(started)
        wait(async)
        true
    end
    wait(started)
    ccall(:uv_async_send, Void, (Ptr{Void},), async.handle)
    @test wait(task)
end

let io = IOBuffer(), s = "hello"
    unsafe_write(io, pointer(s), length(s))
    @test String(take!(io)) == s
end

@static if VERSION ≥ v"0.4"
    @test VERSION ≥ v"0.4"
else
    @test VERSION < v"0.4"
end

let io = IOBuffer(), s = "hello"
    @test @compat String(Vector{UInt8}(s)) == s
    write(io, s)
    @test @compat String(io) == s
    @test unsafe_string(pointer(s)) == s
    @test unsafe_string(pointer(s),sizeof(s)) == s
    @test string(s, s, s) == "hellohellohello"
    @test @compat(String(s)) == s
end

@test Compat.repeat(1:2, inner=2) == [1, 1, 2, 2]
@test Compat.repeat(1:2, outer=[2]) == [1, 2, 1, 2]
@test Compat.repeat([1,2], inner=(2,)) == [1, 1, 2, 2]

for os in [:apple, :bsd, :linux, :unix, :windows]
    from_base = if VERSION >= v"0.7.0-DEV.914"
        Expr(:., Expr(:., :Base, Base.Meta.quot(:Sys)), Base.Meta.quot(Symbol("is", os)))
    else # VERSION >= v"0.5.0-dev+4267"
        Expr(:., :Base, Base.Meta.quot(Symbol("is_", os)))
    end
    @eval @test Compat.Sys.$(Symbol("is", os))() == $from_base()
end

io = IOBuffer()
@test @compat(get(io, :limit, false)) == false
@test @compat(get(io, :compact, false)) == false
@test @compat(get(io, :multiline, false)) == false
@test @compat(get(Nullable(1))) == 1

let
    test_str = "test"
    ptr = pointer(test_str)
    wrapped_str = if VERSION < v"0.6.0-dev.1988"
        unsafe_wrap(Compat.String, ptr)
    else
        unsafe_string(ptr)
    end
    new_str = unsafe_string(ptr)
    cstr = convert(Cstring, ptr)
    new_str2 = unsafe_string(cstr)
    @test wrapped_str == "test"
    @test new_str == "test"
    @test new_str2 == "test"
    if VERSION < v"0.6.0-dev.1988"
        # Test proper pointer aliasing behavior, which is not possible in 0.6
        # with the new String representation
        @test ptr == pointer(wrapped_str)
    end
    @test ptr ≠ pointer(new_str)
    @test ptr ≠ pointer(new_str2)
    @test unsafe_string(convert(Ptr{Int8}, ptr)) == "test"
    if VERSION < v"0.6.0-dev.1988"
        @test unsafe_wrap(Compat.String, convert(Ptr{Int8}, ptr)) == "test"
    end
    x = [1, 2]
    @test unsafe_wrap(Array, pointer(x), 2) == [1, 2]
end

@test allunique([1, 2, 3])
@test !allunique([1, 2, 1])
@test allunique(1:3)
if VERSION < v"0.6.0-dev.2390"
    @test allunique(FloatRange(0.0, 0.0, 0.0, 1.0))
    @test !allunique(FloatRange(0.0, 0.0, 2.0, 1.0))
end

# Add test for Base.view
let a = rand(10,10)
    @test view(a, :, 1) == a[:,1]
end


# 0.5 style single argument `@boundscheck`
@inline function do_boundscheck()
    # A bit ugly since `@boundscheck` returns `nothing`.
    checked = false
    @compat @boundscheck (checked = true;)
    checked
end
@test do_boundscheck() == true

if VERSION < v"0.6.0-dev.1886"
    # `promote_eltype_op` is deprecated
    @test Compat.promote_eltype_op(@functorize(+), ones(2,2), 1) === Float64
    @test Compat.promote_eltype_op(@functorize(*), ones(Int, 2), zeros(Int16,2)) === Int
end

#Add test for Base.normalize and Base.normalize!
let
    vr = [3.0, 4.0]
    for Tr in (Float32, Float64)
        for T in (Tr, Complex{Tr})
            v = convert(Vector{T}, vr)
            @test norm(v) == 5.0
            w = normalize(v)
            @test norm(w - [0.6, 0.8], Inf) < eps(Tr)
            @test isapprox(norm(w), 1.0)
            @test norm(normalize!(copy(v)) - w, Inf) < eps(Tr)
            @test isempty(normalize!(T[]))
        end
    end
end

#Test potential overflow in normalize!
let
    δ = inv(prevfloat(typemax(Float64)))
    v = [δ, -δ]
    if VERSION > v"0.4.0-pre+7164"
        @test norm(v) === 7.866824069956793e-309
    end
    w = normalize(v)
    if VERSION > v"0.4.0-pre+7164"
        @test w ≈ [1/√2, -1/√2]
        @test isapprox(norm(w), 1.0)
    end
    @test norm(normalize!(v) - w, Inf) < eps()
end

# JuliaLang/julia#16603
@test sprint(join, [1, 2, 3]) == "123"
@test sprint(join, [1, 2, 3], ',') == "1,2,3"
@test sprint(join, [1, 2, 3], ", ", ", and ") == "1, 2, and 3"
@test sprint(escape_string, "xyz\n", "z") == "xy\\z\\n"
@test sprint(unescape_string, "xyz\\n") == "xyz\n"

# three-argument show from JuliaLang/julia#16563
@test sprint(show, "text/plain", 1) == stringmime("text/plain", 1)

let n=5, a=rand(n), incx=1, b=rand(n), incy=1
    ccall((Compat.@blasfunc(dcopy_), Base.LinAlg.BLAS.libblas), Void,
          (Ptr{Base.LinAlg.BLAS.BlasInt}, Ptr{Float64}, Ptr{Base.LinAlg.BLAS.BlasInt}, Ptr{Float64}, Ptr{Base.LinAlg.BLAS.BlasInt}),
          &n, a, &incx, b, &incy)
    @test a == b
end

# do-block redirect_std*
let filename = tempname()
    ret = open(filename, "w") do f
        redirect_stdout(f) do
            println("hello")
            [1,3]
        end
    end
    @test ret == [1,3]
    @test chomp(readstring(filename)) == "hello"
    ret = open(filename, "w") do f
        redirect_stderr(f) do
            println(STDERR, "WARNING: hello")
            [2]
        end
    end
    @test ret == [2]
    @test contains(readstring(filename), "WARNING: hello")
    ret = open(filename) do f
        redirect_stdin(f) do
            readline()
        end
    end
    @test contains(ret, "WARNING: hello")
    rm(filename)
end

@test @__DIR__() == dirname(@__FILE__)

# PR #17302
# To be removed when 0.5/0.6 support is dropped.
f17302(a::Number) = a
f17302(a::Number, b::Number) = a + b
Compat.@dep_vectorize_1arg Real f17302
Compat.@dep_vectorize_2arg Real f17302
@test_throws MethodError f17302([1im])
@test_throws MethodError f17302([1im], [1im])
mktemp() do fname, f
    redirect_stderr(f) do
        @test f17302([1.0]) == [1.0]
        @test f17302(1.0, [1]) == [2.0]
        @test f17302([1.0], 1) == [2.0]
        @test f17302([1.0], [1]) == [2.0]
    end
end

# 0.5.0-dev+4677
for A in (Hermitian(randn(5,5) + 10I),
          Symmetric(randn(5,5) + 10I),
          Symmetric(randn(5,5) + 10I, :L))
    F = cholfact(A)
    @test F[:U]'F[:U]  ≈ A
    @test F[:L]*F[:L]' ≈ A

    Ac = copy(A)
    F = cholfact!(Ac)
    @test F[:U]'F[:U]  ≈ A
    @test F[:L]*F[:L]' ≈ A

    @test istriu(chol(A))
    @test chol(A) ≈ F[:U]

    F = cholfact(A, valtrue)
    @test F[:U]'F[:U]  ≈ A[F[:p], F[:p]]
    @test F[:L]*F[:L]' ≈ A[F[:p], F[:p]]
    Ac = copy(A)
    F = cholfact!(Ac, valtrue)
    @test F[:U]'F[:U]  ≈ A[F[:p], F[:p]]
    @test F[:L]*F[:L]' ≈ A[F[:p], F[:p]]
end

types = [
    Bool,
    Float16,
    Float32,
    Float64,
    Int128,
    Int16,
    Int32,
    Int64,
    Int8,
    UInt16,
    UInt32,
    UInt64,
    UInt8,
]
for T in types
    # julia#18510, Nullable constructors
    x = @compat Nullable(one(T), true)
    @test isnull(x) === false
    @test isa(x.value, T)
    @test eltype(x) === T

    x = @compat Nullable{T}(one(T), true)
    y = @compat Nullable{Any}(one(T), true)
    @test isnull(x) === false
    @test isnull(y) === false
    @test isa(x.value, T)
    @test eltype(x) === T
    @test eltype(y) === Any

    x = @compat Nullable{T}(one(T), false)
    y = @compat Nullable{Any}(one(T), false)
    @test isnull(x) === true
    @test isnull(y) === true
    @test eltype(x) === T
    @test eltype(y) === Any

    x = @compat Nullable(one(T), false)
    @test isnull(x) === true
    @test eltype(x) === T

    x = @compat Nullable{T}()
    @test isnull(x) === true
    @test eltype(x) === T

    # julia#18484, generic isnull, unsafe_get
    a = one(T)
    x = @compat Nullable(a, true)
    @test isequal(unsafe_get(x), a)

    x = @compat Nullable{Array{T}}()
    @test_throws UndefRefError unsafe_get(x)
end

@test xor(1,5) == 4
@test 1 ⊻ 5 == 4

# julia#20414
@compat let T = Array{<:Real}, f(x::AbstractVector{<:Real}) = 1
    @test isa([3,4],T)
    @test !isa([3,4im],T)
    @test f(1:3) == f([1,2]) == 1
end
@compat let T = Array{>:Integer}, f(x::AbstractVector{>:Integer}) = 1
    @test isa(Integer[1,2],T)
    @test !isa([3,4],T)
    @test !isa([3.0,4.0],T)
    @test f(Integer[1,2]) == f([1,'a',:sym]) == 1
end

# supertype operator
@test !(Int >: Integer)
@test Integer >: Int

# julia#19246
@test numerator(1//2) === 1
@test denominator(1//2) === 2

# julia#19088
let io = IOBuffer()
    write(io, "aaa")
    @test take!(io) == UInt8['a', 'a', 'a']
    write(io, "bbb")
    @test String(take!(io)) == "bbb"
end

# julia#17510
let x = [1,2,3]
    @compat x .= [3,4,5]
    @test x == [3,4,5]
    @compat x .= x .== 4
    @test x == [0,1,0]
    @compat x .= 7
    @test x == [7,7,7]
end

let s = "Koala test: 🐨"
    @test transcode(UInt16, s) == UInt16[75,111,97,108,97,32,116,101,115,116,58,32,55357,56360]
    @test transcode(UInt32, s) == UInt32[75,111,97,108,97,32,116,101,115,116,58,32,128040]
    for T in (UInt8,UInt16,UInt32,Cwchar_t)
        @test transcode(Compat.String, transcode(T, s)) == s
        @test transcode(UInt8, transcode(T, s)) == Vector{UInt8}(s)
        @test transcode(T, s) == transcode(T, Vector{UInt8}(s)) == transcode(T, transcode(T, s))
    end
end

# julia#17155, tests from Base Julia
@test (uppercase∘hex)(239487) == "3A77F"
let str = randstring(20)
    @test filter(!isupper, str) == replace(str, r"[A-Z]", "")
    @test filter(!islower, str) == replace(str, r"[a-z]", "")
end

# julia#19950, tests from Base (#20028)
for T in (Float16, Float32, Float64, BigFloat, Int8, Int16, Int32, Int64, Int128,
          BigInt, UInt8, UInt16, UInt32, UInt64, UInt128)
    @test iszero(T(0))
    @test iszero(Complex{T}(0))
    if T<:Integer
        @test iszero(Rational{T}(0))
    end
    if T<:AbstractFloat
        @test iszero(T(-0.0))
        @test iszero(Complex{T}(-0.0))
    end
end
@test !iszero([0, 1, 2, 3])
@test iszero([0, 0, 0, 0])

x = view(1:10, 2:4)
D = Diagonal(x)
@test D[1,1] == 2
@test D[3,3] == 4
A = view(rand(5,5), 1:3, 1:3)
@test D*A == Diagonal(copy(x)) * copy(A)
@test A*D == copy(A) * Diagonal(copy(x))

# julia#17623
if VERSION >= v"0.5.0-dev+5509"
# Use include_string to work around unsupported syntax on Julia 0.4
include_string(@__MODULE__, """
    @test [true, false] .& [true, true] == [true, false]
    @test [true, false] .| [true, true] == [true, true]
""")
end

# julia#20022
@test !Compat.isapprox(NaN, NaN)
@test Compat.isapprox(NaN, NaN, nans=true)

# julia#13998
for x in (3.1, -17, 3//4, big(111.1), Inf)
    @test min(x) == max(x) == x
    @test minmax(x) == (x, x)
end

# julia#20006
@compat abstract type AbstractFoo20006 end
eval(Expr(
    :type, false,
    Expr(:(<:), :(ConcreteFoo20006{T<:Int}), :AbstractFoo20006),
    quote end))
eval(Expr(
    :type, false,
    Expr(:(<:), :(ConcreteFoo20006N{T<:Int,N}), :AbstractFoo20006),
    quote end))
@compat ConcreteFoo200061{T<:Int} = ConcreteFoo20006N{T,1}
@test Compat.TypeUtils.isabstract(AbstractFoo20006)
@test !Compat.TypeUtils.isabstract(ConcreteFoo20006)
@test !Compat.TypeUtils.isabstract(ConcreteFoo20006N)
@test !Compat.TypeUtils.isabstract(ConcreteFoo200061)
@test !Compat.TypeUtils.isabstract(StridedArray)
@test Compat.TypeUtils.parameter_upper_bound(ConcreteFoo20006, 1) == Int
@test isa(Compat.TypeUtils.typename(Array), TypeName)

# @view and @views tests copied from Base
let X = reshape(1:24,2,3,4), Y = 4:-1:1
    @test isa(@view(X[1:3]), SubArray)

    @test X[1:end] == @dotcompat (@view X[1:end]) # test compatibility of @. and @view
    @test X[1:end-3] == @view X[1:end-3]
    @test X[1:end,2,2] == @view X[1:end,2,2]
    @test reshape(X[1,2,1:end-2],2) == @view X[1,2,1:end-2]
    @test reshape(X[1,2,Y[2:end]],3) == @view X[1,2,Y[2:end]]
    @test reshape(X[1:end,2,Y[2:end]],2,3) == @view X[1:end,2,Y[2:end]]

    u = (1,2:3)
    @test reshape(X[u...,2:end],2,3) == @view X[u...,2:end]
    @test reshape(X[(1,)...,(2,)...,2:end],3) == @view X[(1,)...,(2,)...,2:end]

    # the following tests fail on 0.5 because of bugs in the 0.5 Base.@view
    # macro (a bugfix is scheduled to be backported from 0.6: julia#20247)
    if !isdefined(Base, Symbol("@view")) || VERSION ≥ v"0.6.0-dev.2406"
        # test macro hygiene
        let size=(x,y)-> error("should not happen"), Base=nothing
            @test X[1:end,2,2] == @view X[1:end,2,2]
        end

        # test that side effects occur only once
        let foo = typeof(X)[X]
            @test X[2:end-1] == @view (push!(foo,X)[1])[2:end-1]
            @test foo == typeof(X)[X, X]
        end
    end

    # test @views macro
    @views @compat let f!(x) = x[1:end-1] .+= x[2:end].^2
        x = [1,2,3,4]
        f!(x)
        @test x == [5,11,19,4]
        @test isa(x[1:3],SubArray)
        @test x[2] === 11
        @test Dict((1:3) => 4)[1:3] === 4
        x[1:2] = 0
        @test x == [0,0,19,4]
        x[1:2] .= 5:6
        @test x == [5,6,19,4]
        f!(x[3:end])
        @test x == [5,6,35,4]
        x[Y[2:3]] .= 7:8
        @test x == [5,8,7,4]
        @dotcompat x[(3,)..., ()...] += 3 # @. should convert to .+=, test compatibility with @views
        @test x == [5,8,10,4]
        i = Int[]
        # test that lhs expressions in update operations are evaluated only once:
        x[push!(i,4)[1]] += 5
        @test x == [5,8,10,9] && i == [4]
        x[push!(i,3)[end]] += 2
        @test x == [5,8,12,9] && i == [4,3]
        @dotcompat x[3:end] = 0       # make sure @. works with end expressions in @views
        @test x == [5,8,0,0]
    end
    # same tests, but make sure we can switch the order of @compat and @views
    @compat @views let f!(x) = x[1:end-1] .+= x[2:end].^2
        x = [1,2,3,4]
        f!(x)
        @test x == [5,11,19,4]
        @test isa(x[1:3],SubArray)
        @test x[2] === 11
        @test Dict((1:3) => 4)[1:3] === 4
        x[1:2] = 0
        @test x == [0,0,19,4]
        x[1:2] .= 5:6
        @test x == [5,6,19,4]
        f!(x[3:end])
        @test x == [5,6,35,4]
        x[Y[2:3]] .= 7:8
        @test x == [5,8,7,4]
        @dotcompat x[(3,)..., ()...] += 3 # @. should convert to .+=, test compatibility with @views
        @test x == [5,8,10,4]
        i = Int[]
        # test that lhs expressions in update operations are evaluated only once:
        x[push!(i,4)[1]] += 5
        @test x == [5,8,10,9] && i == [4]
        x[push!(i,3)[end]] += 2
        @test x == [5,8,12,9] && i == [4,3]
        @dotcompat x[3:end] = 0       # make sure @. works with end expressions in @views
        @test x == [5,8,0,0]
    end
    @views @test isa(X[1:3], SubArray)
    @test X[1:end] == @views X[1:end]
    @test X[1:end-3] == @views X[1:end-3]
    @test X[1:end,2,2] == @views X[1:end,2,2]
    @test reshape(X[1,2,1:end-2],2) == @views X[1,2,1:end-2]
    @test reshape(X[1,2,Y[2:end]],3) == @views X[1,2,Y[2:end]]
    @test reshape(X[1:end,2,Y[2:end]],2,3) == @views X[1:end,2,Y[2:end]]
    @test reshape(X[u...,2:end],2,3) == @views X[u...,2:end]
    @test reshape(X[(1,)...,(2,)...,2:end],3) == @views X[(1,)...,(2,)...,2:end]

    # test macro hygiene
    let size=(x,y)-> error("should not happen"), Base=nothing
        @test X[1:end,2,2] == @views X[1:end,2,2]
    end
end

# @. (@__dot__) tests, from base:
let x = [4, -9, 1, -16]
    @test [2, 3, 4, 5] == @dotcompat(1 + sqrt($sort(abs(x))))
    @test @dotcompat(x^2) == x.^2
    @dotcompat x = 2
    @test x == [2,2,2,2]
end
@test [1,4,9] == @dotcompat let x = [1,2,3]; x^2; end
let x = [1,2,3], y = x
    @dotcompat for i = 1:3
        y = y^2 # should convert to y .= y.^2
    end
    @test x == [1,256,6561]
end
let x = [1,2,3]
    @dotcompat f(x) = x^2
    @test f(x) == [1,4,9]
end

# PR #20418
@compat abstract type Abstract20418{T} <: Ref{T} end
@test Compat.TypeUtils.isabstract(Abstract20418)
@compat primitive type Primitive20418{T} <: Ref{T} 16 end
@test !Compat.TypeUtils.isabstract(Primitive20418)
@test isbits(Primitive20418{Int})
@test sizeof(Primitive20418{Int}) == 2

# julia #18839
module Test18839

using Compat
using Compat.Iterators
using Base.Test

@test collect(take(countfrom(2), 3)) == [2, 3, 4]
@test collect(take(cycle(5:8), 9)) == [5:8; 5:8; 5]
@test collect(drop([1, 2, 3], 2)) == [3]
@test collect(enumerate([4, 5, 6])) == [(1,4), (2,5), (3,6)]
@test collect(flatten(Any[1:2, 4:5, Any[-1, 4]])) == [1,2,4,5,-1,4]
@test vec(collect(product([1, 2], [3, 4]))) == [(1,3), (2,3), (1,4), (2,4)]
@test vec(collect(product(1:2, 1:2, 1:2))) == [
    (1,1,1), (2,1,1), (1,2,1), (2,2,1),
    (1,1,2), (2,1,2), (1,2,2), (2,2,2)]
@test collect(take(repeated(10), 5)) == [10,10,10,10,10]
@test collect(rest(1:10, 5)) == [5,6,7,8,9,10]
@test collect(take([1, 2, 3], 2)) == [1, 2]
@test collect(zip([1,2], [3,4])) == [(1,3), (2,4)]
@test collect(partition(1:5, 2)) == Any[[1,2],[3,4],[5]]

end

# PR #20500
@compat A20500{T<:Integer} = Array{T,20500}
@compat const A20500_2{T<:Union{Int,Float32}} = Pair{T,T}
f20500() = A20500
f20500_2() = A20500_2
@inferred f20500()
@inferred f20500_2()

module CompatArray
using Compat
eval(Expr(
    :type, false,
    Expr(:(<:), :(CartesianArray{T,N}), :(AbstractArray{T,N})),
    quote
        parent::Array{T,N}
    end))
eval(Expr(
    :type, false,
    Expr(:(<:), :(LinearArray{T,N}), :(AbstractArray{T,N})),
    quote
        parent::Array{T,N}
    end))
@compat Base.IndexStyle(::Type{<:LinearArray}) = IndexLinear()
end
@test IndexStyle(Array{Float32,2}) === IndexLinear()
@test IndexStyle(CompatArray.CartesianArray{Float32,2}) === IndexCartesian()
@test IndexStyle(CompatArray.LinearArray{Float32,2}) === IndexLinear()
let a = CompatArray.CartesianArray(rand(2,3)), b = CompatArray.LinearArray(rand(2,3))
    @test IndexStyle(a) === IndexCartesian()
    @test IndexStyle(b) === IndexLinear()
end

for (A,val) in ((zeros(1:5, Float32, 3, 2), 0),
                (ones(1:5, Float32, 3, 2), 1),
                (zeros(1:5, Float32, (3, 2)), 0),
                (ones(1:5, Float32, (3, 2)), 1))
    @test isa(A, Matrix{Float32}) && size(A) == (3,2) && all(x->x==val, A)
end
for (A,val) in ((zeros(1:5, Float32), 0),
                (ones(1:5, Float32), 1))
    @test isa(A, Vector{Float32}) && size(A) == (5,) && all(x->x==val, A)
end

# PR 20203
@test Compat.readline(IOBuffer("Hello, World!\n")) == "Hello, World!"
@test Compat.readline(IOBuffer("x\n"), chomp=true) == "x"
@test Compat.readline(IOBuffer("x\n"), chomp=false) == "x\n"

# PR 18727
let
    iset = Set([17, 4711])
    cfset = convert(Set{Float64}, iset)
    @test typeof(cfset) == Set{Float64}
    @test cfset == iset
    fset = Set([17.0, 4711.0])
    ciset = convert(Set{Int}, fset)
    @test typeof(ciset) == Set{Int}
    @test ciset == fset
    ssset = Set(split("foo bar"))
    cssset = convert(Set{String}, ssset)
    @test typeof(cssset) == Set{String}
    @test cssset == Set(["foo", "bar"])
end

# PR 18082
@test !isassigned(Ref{String}())
@test isassigned(Ref{String}("Test"))

@test unsafe_trunc(Int8, 128) === Int8(-128)
@test_throws InexactError trunc(Int8, 128)

# PR 21346
let zbuf = IOBuffer([0xbf, 0xc0, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
                     0x40, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                     0xc0, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
    z1 = read(zbuf, Complex64)
    z2 = read(zbuf, Complex128)
    @test bswap(z1) === -1.5f0 + 2.5f0im
    @test bswap(z2) ===  3.5 - 4.5im
end

# PR 19449
using Compat: StringVector
@test length(StringVector(5)) == 5
@test String(fill!(StringVector(5), 0x61)) == "aaaaa"

# collect
if VERSION >= v"0.5.0-rc1+46"
    using OffsetArrays
    a = OffsetArray(1:3, -1:1)
    b = Compat.collect(a)
    @test indices(b) === (Base.OneTo(3),)
    @test b == [1,2,3]
end

# PR 22064
module Test22064
using Base.Test, Compat
@test (@__MODULE__) === Test22064
end

# invokelatest
issue19774(x) = 1
let foo() = begin
        eval(:(issue19774(x::Int) = 2))
        return Compat.invokelatest(issue19774, 0)
    end
    @test foo() == 2
end
cm359() = @__MODULE__
@test Compat.invokelatest(cm359) === @__MODULE__

# PR 21378
let
    # https://en.wikipedia.org/wiki/Swatch_Internet_Time
    eval(Expr(
        :type, false,
        Expr(:(<:), :Beat, :(Dates.Period)),
        quote
            value::Int64
        end))

    Dates.value(b::Beat) = b.value
    Dates.toms(b::Beat) = Dates.value(b) * 86400
    Dates._units(b::Beat) = " beat" * (abs(Dates.value(b)) == 1 ? "" : "s")
    Base.promote_rule(::Type{Dates.Day}, ::Type{Beat}) = Dates.Millisecond
    Base.convert{T<:Dates.Millisecond}(::Type{T}, b::Beat) = T(Dates.toms(b))

    @test Beat(1000) == Dates.Day(1)
    @test Beat(1) < Dates.Day(1)
    @test_throws MethodError Dates.Day(30) == Dates.Month(1)
    @test_throws MethodError Dates.Month(1) == Dates.Day(30)
    @test_throws MethodError Dates.Day(1) < Dates.Month(1)
    @test_throws MethodError Dates.Month(1) < Dates.Day(1)
end

# PR 22629
@test logdet(0.5) == log(det(0.5))

# PR 22633
for T in (Float64, Complex64, BigFloat, Int)
    λ = T(4)
    @test chol(λ*I).λ ≈ √λ
    @test_throws Union{ArgumentError,LinAlg.PosDefException} chol(-λ*I)
end

let
    @compat cr(::CartesianRange{2}) = 2
    @test cr(CartesianRange((5, 3))) == 2
    @test_throws MethodError cr(CartesianRange((5, 3, 2)))
end
if VERSION < v"0.7.0-DEV.880"
    # ensure we don't bork any non-updated expressions
    let
        @compat cr(::CartesianRange{CartesianIndex{2}}) = 2
        @test cr(CartesianRange((5, 3))) == 2
        @test_throws MethodError cr(CartesianRange((5, 3, 2)))
    end
end

include("deprecated.jl")

nothing
