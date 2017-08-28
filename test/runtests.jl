using Compat
using Base.Test

# Issue #291
# 0.6
@test (1, 2) == @compat abs.((1, -2))
@test broadcast(+, (1.0, 1.0), (0, -2.0)) == (1.0,-1.0)

# Test for `take!(::Task)`/`take!(::Channel)`
# 0.6
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

for os in [:apple, :bsd, :linux, :unix, :windows]
    from_base = if VERSION >= v"0.7.0-DEV.914"
        Expr(:., Expr(:., :Base, Base.Meta.quot(:Sys)), Base.Meta.quot(Symbol("is", os)))
    else
        Expr(:., :Base, Base.Meta.quot(Symbol("is_", os)))
    end
    @eval @test Compat.Sys.$(Symbol("is", os))() == $from_base()
end

# do-block redirect_std*
# 0.6
let filename = tempname()
    ret = open(filename, "w") do f
        redirect_stdout(f) do
            println("hello")
            [1,3]
        end
    end
    @test ret == [1,3]
    @test chomp(read(filename, String)) == "hello"
    ret = open(filename, "w") do f
        redirect_stderr(f) do
            println(STDERR, "WARNING: hello")
            [2]
        end
    end
    @test ret == [2]
    @test contains(read(filename, String), "WARNING: hello")
    ret = open(filename) do f
        redirect_stdin(f) do
            readline()
        end
    end
    @test contains(ret, "WARNING: hello")
    rm(filename)
end

# 0.6
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
# 0.6
@test [true, false] .& [true, true] == [true, false]
@test [true, false] .| [true, true] == [true, true]

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
using OffsetArrays
a = OffsetArray(1:3, -1:1)
b = Compat.collect(a)
@test indices(b) === (Base.OneTo(3),)
@test b == [1,2,3]

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

# PR #21197
let c = `ls -l "foo bar"`
    @test collect(c) == ["ls", "-l", "foo bar"]
    @test first(c) == "ls" == c[1]
    @test last(c) == "foo bar" == c[3] == c[end]
    @test c[1:2] == ["ls", "-l"]
    @test eltype(c) == String
    @test length(c) == 3
    @test eachindex(c) == 1:3
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

# PR 22350
eval(Expr(:type, false, :TestType, Expr(:block, :(a::Int), :b)))
@test fieldcount(TestType) == 2
@test fieldcount(Int) == 0

# PR 20005
@test_throws InexactError throw(InexactError(:func, Int, 3.2))

# PR 22751
@test_throws DomainError throw(DomainError(-2))
@test_throws DomainError throw(DomainError(-2, "negative"))

# PR 22761
@test_throws OverflowError throw(OverflowError("overflow"))

let x = fill!(StringVector(5), 0x61)
    # 0.7
    @test pointer(x) == pointer(String(x))
end

# Val(x)
# 0.7
begin
    local firstlast
    firstlast(::Val{true}) = "First"
    firstlast(::Val{false}) = "Last"

    @test firstlast(Val(true)) == "First"
    @test firstlast(Val(false)) == "Last"
end

# Reshape to a given number of dimensions using Val(N)
# 0.7
let
    for A in (rand(()), rand(2), rand(2,3), rand(2,3,5), rand(2,3,5,7)), N in (1,2,3,4,5,6)
        B = @inferred reshape(A, Val(N))
        @test ndims(B) == N
        if N < ndims(A)
            new_sz = (size(A)[1:N-1]..., prod(size(A)[N:end]))
        elseif N == ndims(A)
            new_sz = size(A)
        else
            new_sz = (size(A)..., ntuple(x->1, N-ndims(A))...)
        end
        @test size(B) == new_sz
        @test B == reshape(A, new_sz)
    end
end

# ntuple with Val(N)
# 0.7
@test @inferred(ntuple(x->1, Val(3))) == (1,1,1)
@test @inferred(ntuple(x->x, Val(0))) == ()
@test @inferred(ntuple(x->x, Val(5))) == (1,2,3,4,5)

# @nospecialize
# 0.7
no_specialize(@nospecialize(x)) = sin(1)
no_specialize(@nospecialize(x::Integer)) = sin(2)
@test no_specialize(1.0) == sin(1)
@test no_specialize(1) == sin(2)

# 0.7
@test read(IOBuffer("aaaa"), String) == "aaaa"
@test contains(read(@__FILE__, String), "read(@__FILE__, String)")
@test read(`$(Base.julia_cmd()) --startup-file=no -e "println(:aaaa)"`, String) == "aaaa\n"

if VERSION < v"0.6.0"
    include("deprecated.jl")
end

nothing
