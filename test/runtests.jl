using Compat
import Compat.String
import Compat.view
@compat import Base.show
using Base.Test

v = 1
@test_throws AssertionError @assert(v < 1)

type TestCustomShowType end
@compat function show(io::IO, ::MIME"text/plain", ::TestCustomShowType)
    print(io, "MyTestCustomShowType")
end
myio = IOBuffer()
display(TextDisplay(myio), MIME"text/plain"(), TestCustomShowType())
@test @compat String(myio) == "MyTestCustomShowType"

type TestCustomShowType2 end
@compat Base.show(io::IO, ::MIME"text/plain", ::TestCustomShowType2) = print(io, "MyTestCustomShowType2")
myio = IOBuffer()
display(TextDisplay(myio), MIME"text/plain"(), TestCustomShowType2())
@test @compat String(myio) == "MyTestCustomShowType2"

type TestCustomShowType3 end
@compat show(io::IO, ::TestCustomShowType3) = print(io, "2-Argument-show")
myio = IOBuffer()
display(TextDisplay(myio), TestCustomShowType3())
@test @compat String(myio) == "2-Argument-show"

immutable ParameterizedShowType{T}
    _::T
end
myio = IOBuffer()
@compat show{T}(io::IO, ::MIME"text/html", ::ParameterizedShowType{T}) =
    print(io, "<code>::", T, "</code>")
@compat show(myio, MIME("text/html"), ParameterizedShowType(0.0))
@test @compat String(myio) == "<code>::Float64</code>"

d = Dict{Int,Int}()
d[1] = 1
@test Compat.@Dict(1 => 1) == d
@test Compat.@Dict(1 => v) == d

@test typeof(@compat(Dict(1 => 1))) == Dict{Int,Int}
@test @compat(Dict(1 => 1)) == d
@test @compat(Dict(1 => v)) == d

ad = Dict{Any,Any}()
ad[1] = 1
@test Compat.@AnyDict(1 => 1) == ad
@test Compat.@AnyDict(1 => v) == ad

@test typeof(@compat(Dict{Any,Any}(1 => 1))) == Dict{Any,Any}
@test @compat(Dict{Any,Any}(1 => 1)) == ad
@test @compat(Dict{Any,Any}(1 => v)) == ad

td = Dict{Int,Float64}()
td[1] = 1.0

@test typeof(@compat(Dict{Int,Float64}(1 => 1))) == Dict{Int,Float64}
@test @compat(Dict{Int,Float64}(1 => 1)) == td
@test @compat(Dict{Int,Float64}(1 => v)) == td

@test @compat(Dict()) == Dict()
@test @compat(Dict{Any,Any}()) == Dict{Any,Any}()
@test @compat(Dict([(1, 1)])) == d
@test @compat(Dict(:Void => :Nothing)) == Dict([(:Void, :Nothing)])

d2 = Dict{Symbol,Dict{Symbol,Int}}()
d2[:a] = Dict{Symbol,Int}()
d2[:a][:b] = 1
@test @compat(Dict(:a => Dict(:b => 1))) == d2

d = Dict(zip([1, 2], [3, 4]))
@test d == @compat Dict(1=>3, 2=>4)

@compat function f()
    a = :a
    b = Dict(:b => 1)
    Dict(a => b)
end
@test f() == d2

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
@test round(Int, [1, 1]) == [1, 1]
@test round(Int, [1.1, 1.1]) == [1, 1]
@test round(Int, [1 1]) == [1 1]
@test round(Int, [1.1 1.1]) == [1 1]
@test round(Int, fill(1.1, 2, 3, 4)) == fill(1, 2, 3, 4)
@test ceil(Int, 1) == 1
@test ceil(Int, 1.1) == 2
@test ceil(Int, [1, 1]) == [1, 1]
@test ceil(Int, [1.1, 1.1]) == [2, 2]
@test ceil(Int, [1 1]) == [1 1]
@test ceil(Int, [1.1 1.1]) == [2 2]
@test ceil(Int, fill(1.1, 2, 3, 4)) == fill(2, 2, 3, 4)
@test floor(Int, 1) == 1
@test floor(Int, 1.1) == 1
@test floor(Int, [1, 1]) == [1, 1]
@test floor(Int, [1.1, 1.1]) == [1, 1]
@test floor(Int, [1 1]) == [1 1]
@test floor(Int, [1.1 1.1]) == [1 1]
@test floor(Int, fill(1.1, 2, 3, 4)) == fill(1, 2, 3, 4)
@test trunc(Int, 1) == 1
@test trunc(Int, 1.1) == 1
@test trunc(Int, [1, 1]) == [1, 1]
@test trunc(Int, [1.1, 1.1]) == [1, 1]
@test trunc(Int, [1 1]) == [1 1]
@test trunc(Int, [1.1 1.1]) == [1 1]
@test trunc(Int, fill(1.1, 2, 3, 4)) == fill(1, 2, 3, 4)

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
@test size(bitrand(MersenneTwister(), 3, 4)) == (3, 4)
@test size(bitrand(MersenneTwister(), (3, 4))) == (3, 4)
@test rand(Bool) in [false, true]

rng = MersenneTwister()
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

module CartesianTest
    using Base.Cartesian, Compat
    @ngenerate N NTuple{N,Int} function f(X::NTuple{N,Int}...)
        @ncall N tuple X
    end
end

@test CartesianTest.f(1) == (1,)
@test CartesianTest.f(1,2) == (1,2)
@test CartesianTest.f(1,2,3) == (1,2,3)
@test CartesianTest.f(1,2,3,4) == (1,2,3,4)
@test CartesianTest.f(1,2,3,4,5) == (1,2,3,4,5)

extrapath = is_windows() ? joinpath(JULIA_HOME,"..","Git","usr","bin")*";" : ""
@compat withenv("PATH" => extrapath * ENV["PATH"]) do
    cmd1 = pipeline(`echo hello`, `sort`)
    cmd2 = pipeline(`true`, `true`)
    if is_windows()
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

type Test3609
    a
    b
end

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
type A; end
x = "abc"
@test @compat String(unsafe_string(Compat.unsafe_convert(Ptr{UInt8}, x))) == x
Compat.unsafe_convert(::Ptr{A}, x) = x
@test Compat.unsafe_convert(pointer([A()]), 1) == 1

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
    firstlast(::Type{Val{true}}) = "First"
    firstlast(::Type{Val{false}}) = "Last"

    @test firstlast(Val{true}) == "First"
    @test firstlast(Val{false}) == "Last"
end

# qr, qrfact, qrfact!
let A = [1.0 2.0; 3.0 4.0]
    Q, R = qr(A, Val{false})
    @test_approx_eq Q*R A
    Q, R, p = qr(A, Val{true})
    @test_approx_eq Q*R A[:,p]
    F = qrfact(A, Val{false})
    @test_approx_eq F[:Q]*F[:R] A
    F = qrfact(A, Val{true})
    @test_approx_eq F[:Q]*F[:R] A[:,F[:p]]
    A_copy = copy(A)
    F = qrfact!(A_copy, Val{false})
    @test_approx_eq F[:Q]*F[:R] A
    A_copy = copy(A)
    F = qrfact!(A_copy, Val{true})
    @test_approx_eq F[:Q]*F[:R] A[:,F[:p]]
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
        @test_approx_eq U'*U B
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
    @test [Compat.linspace(-u,u,4);] == [-u,0,0,u]
    @test [Compat.linspace(-u,u,4);][2] === -z
    @test [Compat.linspace(-u,u,4);][3] === z
    @test first(Compat.linspace(-u,-u,0)) == -u
    @test last(Compat.linspace(-u,-u,0)) == -u
    @test first(Compat.linspace(u,-u,0)) == u
    @test last(Compat.linspace(u,-u,0)) == -u
    @test [Compat.linspace(u,-u,0);] == []
    @test [Compat.linspace(u,u,1);] == [u]
    @test [Compat.linspace(u,-u,2);] == [u,-u]
    @test [Compat.linspace(u,-u,3);] == [u,0,-u]
    @test [Compat.linspace(u,-u,4);] == [u,0,0,-u]
    @test [Compat.linspace(u,-u,4);][2] === z
    @test [Compat.linspace(u,-u,4);][3] === -z
    v = [Compat.linspace(-u,u,12);]
    @test length(v) == 12
    @test issorted(v) && unique(v) == [-u,0,0,u]
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
            convert(Compat.UTF8String, Char['A' + i % 52 for i in 1:(div(SZ_UNBUFFERED_IO,2))]),
            convert(Compat.UTF8String, Char['A' + i % 52 for i in 1:(    SZ_UNBUFFERED_IO -1)]),
            convert(Compat.UTF8String, Char['A' + i % 52 for i in 1:(    SZ_UNBUFFERED_IO   )]),
            convert(Compat.UTF8String, Char['A' + i % 52 for i in 1:(    SZ_UNBUFFERED_IO +1)])
        ]

            write(filename, text)

            verbose && println("$name readstring...")
            @test readstring(io()) == text

            @test readstring(io()) == readstring(filename)


            verbose && println("$name read...")
            @test @compat read(io()) == UInt8[convert(UInt8, _) for _ in text]

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
        @compat to = IOBuffer(UInt8[convert(UInt8, _) for _ in text], false, true)
        write(to, io())
        @test takebuf_string(to) == text

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

immutable A
    a
end

immutable B{T}
    b::T
end

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
    has_symlinks = is_unix() ? true : (isdefined(Base, :WINDOWS_VISTA_VER) && Base.windows_version() >= Base.WINDOWS_VISTA_VER)
    follow_symlink_vec = has_symlinks ? [true, false] : [false]
    has_symlinks && symlink(abspath("sub_dir2"), joinpath("sub_dir1", "link"))
    for follow_symlinks in follow_symlink_vec
        task = walkdir(".", follow_symlinks=follow_symlinks)
        root, dirs, files = consume(task)
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]

        root, dirs, files = consume(task)
        @test root == joinpath(".", "sub_dir1")
        @test dirs == (has_symlinks ? ["link", "subsub_dir1", "subsub_dir2"] : ["subsub_dir1", "subsub_dir2"])
        @test files == ["file1", "file2"]

        root, dirs, files = consume(task)
        if follow_symlinks
            @test root == joinpath(".", "sub_dir1", "link")
            @test dirs == []
            @test files == ["file_dir2"]
            root, dirs, files = consume(task)
        end
        for i=1:2
            @test root == joinpath(".", "sub_dir1", "subsub_dir$i")
            @test dirs == []
            @test files == []
            root, dirs, files = consume(task)
        end

        @test root == joinpath(".", "sub_dir2")
        @test dirs == []
        @test files == ["file_dir2"]
    end

    for follow_symlinks in follow_symlink_vec
        task = walkdir(".", follow_symlinks=follow_symlinks, topdown=false)
        root, dirs, files = consume(task)
        if follow_symlinks
            @test root == joinpath(".", "sub_dir1", "link")
            @test dirs == []
            @test files == ["file_dir2"]
            root, dirs, files = consume(task)
        end
        for i=1:2
            @test root == joinpath(".", "sub_dir1", "subsub_dir$i")
            @test dirs == []
            @test files == []
            root, dirs, files = consume(task)
        end
        @test root == joinpath(".", "sub_dir1")
        @test dirs ==  (has_symlinks ? ["link", "subsub_dir1", "subsub_dir2"] : ["subsub_dir1", "subsub_dir2"])
        @test files == ["file1", "file2"]

        root, dirs, files = consume(task)
        @test root == joinpath(".", "sub_dir2")
        @test dirs == []
        @test files == ["file_dir2"]

        root, dirs, files = consume(task)
        @test root == "."
        @test dirs == ["sub_dir1", "sub_dir2"]
        @test files == ["file1", "file2"]
    end
    #test of error handling
    task_error = walkdir(".")
    task_noerror = walkdir(".", onerror=x->x)
    root, dirs, files = consume(task_error)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    rm(joinpath("sub_dir1"), recursive=true)
    @test_throws SystemError consume(task_error) # throws an error because sub_dir1 do not exist

    root, dirs, files = consume(task_noerror)
    @test root == "."
    @test dirs == ["sub_dir1", "sub_dir2"]
    @test files == ["file1", "file2"]

    root, dirs, files = consume(task_noerror) # skips sub_dir1 as it no longer exist
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

for (Fun, func) in [(:AndFun,              :&),
                    (:OrFun,               :|),
                    (:XorFun,              :$),
                    (:AddFun,              :+),
                    (:DotAddFun,           :.+),
                    (:SubFun,              :-),
                    (:DotSubFun,           :.-),
                    (:MulFun,              :*),
                    (:DotMulFun,           :.*),
                    (:RDivFun,             :/),
                    (:DotRDivFun,          :./),
                    (:LDivFun,             :\),
                    (:IDivFun,             :div),
                    (:DotIDivFun,          @compat(Symbol(".÷"))),
                    (:ModFun,              :mod),
                    (:RemFun,              :rem),
                    (:DotRemFun,           :.%),
                    (:PowFun,              :^),
                    (:MaxFun,              :scalarmax),
                    (:MinFun,              :scalarmin),
                    (:LessFun,             :<),
                    (:MoreFun,             :>),
                    (:DotLSFun,            :.<<),
                    (:DotRSFun,            :.>>),
                    (:ElementwiseMaxFun,   :max),
                    (:ElementwiseMinFun,   :min)]
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
@test isa(@functorize(centralizedabs2fun)(1), @functorize(centralizedabs2fun))
@test isa(@functorize(centralizedabs2fun)(1.0), @functorize(centralizedabs2fun))
let a = rand(1:10, 10)
    @eval @test mapreduce(x -> abs2(x - 1), +, $(a)) == mapreduce(@functorize(centralizedabs2fun)(1), +, $(a))
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

if VERSION ≥ v"0.4.0-dev+3732"
    @test Symbol("foo") === :foo
    @test Symbol("foo", "bar") === :foobar
    @test Symbol("a_", 2) === :a_2
    @test Symbol('c') === :c
    @test Symbol(1) === Symbol("1")
end

foostring(::String) = 1
@test foostring("hello") == 1
@test foostring("λ") == 1
@test isa("hello", Compat.ASCIIString)
@test isa("λ", Compat.UTF8String)

let async, c = false
    async = Compat.AsyncCondition(x->(c = true))
    ccall(:uv_async_send, Void, (Ptr{Void},), async.handle)
    sleep(0.1)
    @test c
end

let io = IOBuffer(), s = "hello"
    unsafe_write(io, pointer(s), length(s))
    @test takebuf_string(io) == s
end

@static if VERSION ≥ v"0.4"
    @test VERSION ≥ v"0.4"
else
    @test VERSION < v"0.4"
end

let io = IOBuffer(), s = "hello"
    @test @compat String(s.data) == s
    write(io, s)
    @test @compat String(io) == s
    @test unsafe_string(pointer(s.data)) == s
    @test unsafe_string(pointer(s.data),length(s.data)) == s
    @test string(s, s, s) == "hellohellohello"
    @test @compat(String(s)) == s
    @test String == @compat(Union{Compat.UTF8String,Compat.ASCIIString})
end

@test Compat.repeat(1:2, inner=2) == [1, 1, 2, 2]
@test Compat.repeat(1:2, outer=[2]) == [1, 2, 1, 2]
@test Compat.repeat([1,2], inner=(2,)) == [1, 1, 2, 2]

if VERSION < v"0.5.0-dev+4267"
    if OS_NAME == :Windows
        @test is_windows()
    elseif OS_NAME == :Darwin
        @test is_apple() && is_bsd() && is_unix()
    elseif OS_NAME == :FreeBSD
        @test is_bsd() && is_unix()
    elseif OS_NAME == :Linux
        @test is_linux() && is_unix()
    end
else
    @test Compat.KERNEL == Sys.KERNEL
end

io = IOBuffer()
@test @compat(get(io, :limit, false)) == false
@test @compat(get(io, :compact, false)) == false
@test @compat(get(io, :multiline, false)) == false
@test @compat(get(Nullable(1))) == 1

let
    test_str = "test"
    ptr = pointer(test_str.data)
    wrapped_str = unsafe_wrap(Compat.String, ptr)
    new_str = unsafe_string(ptr)
    cstr = convert(Cstring, ptr)
    new_str2 = unsafe_string(cstr)
    @test wrapped_str == "test"
    @test new_str == "test"
    @test new_str2 == "test"
    @test ptr == pointer(wrapped_str)  # Test proper pointer aliasing behavior
    @test ptr ≠ pointer(new_str)
    @test ptr ≠ pointer(new_str2)
    @test unsafe_string(convert(Ptr{Int8}, ptr)) == "test"
    @test unsafe_wrap(Compat.String, convert(Ptr{Int8}, ptr)) == "test"
    x = [1, 2]
    @test unsafe_wrap(Array, pointer(x), 2) == [1, 2]
end

@test allunique([1, 2, 3])
@test !allunique([1, 2, 1])
@test allunique(1:3)
@test allunique(FloatRange(0.0, 0.0, 0.0, 1.0))
@test !allunique(FloatRange(0.0, 0.0, 2.0, 1.0))

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

@test Compat.promote_eltype_op(@functorize(+), ones(2,2), 1) === Float64
@test Compat.promote_eltype_op(@functorize(*), ones(Int, 2), zeros(Int16,2)) === Int

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
