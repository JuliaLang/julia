# parse various syntax into all tree types
let code = raw"""
    module M
    export f, g
    public h
    import Base: +, show
    using Base.Iterators: take
    import A.B as C

    \"""
        f(x)

    Docstring
    \"""
    function f(x::Int, y::T=2, z...; k=1, kw...)::Int where {T<:Real}
        local a = x + y * z[1] - k ÷ 2 % 3 ^ 2 // 1
        global g_
        a += 1; a .+= 1; a |= 0x01
        b = a > 1 ? a : -a
        c = a < b <= x == y != z[1]
        d = a && b || !c
        e = ([1, 2], [1 2; 3 4], [1;; 2], Int[], (1, 2), (a=1, b=2), (; a, b))
        e2 = ([i^2 for i in 1:10 if isodd(i)], (i for i in 1:3, j in 4:5), Dict(k => v for (k, v) in e))
        s = ("s $a $(a+1) \n\t\\ \x41", \"""
            triple $x
            \""", raw"r\s", 'c', '\n', `cmd $x`, r"[a-z]+"i, b"bytes")
        n = (1, 1.0, 1f0, 0x1f, 0b101, 0o17, 1e10, 1.5e-3, 0x1p3, 1_000, 123456789012345678901234567890,
             0xffffffffffffffffffffffffffffffffff, 1im, 2x, 2(x+1), true)
        t = (x isa Int, x::Int, Int <: Real, x -> x + 1, (x, y) -> x * y, function (x) x end)
        u = (x', x.y.z, x[1, end], x[begin:end-1], f.(x) .+ 1, x |> f, f ∘ g, x ≥ y ≠ z, √x, -x)
        @show x
        @inline f(x) = x
        if x > 0
            return 1
        elseif x < 0
            return -1
        else
            nothing
        end
        for i in 1:10, j = 1:2
            i == 5 && continue
            break
        end
        while x < 10
            x += 1
        end
        try
            error("e")
        catch err
            rethrow()
        else
            nothing
        finally
            nothing
        end
        let y = 1, z
            y
        end
        quote
            $x + $(y)
        end
        f(x) do y
            y + 1
        end
        (a, b) = (1, 2); (; a, b) = e
        x::Int = 3
        ccall(:jl_foo, Cvoid, (Ptr{Cvoid},), C_NULL)
        f(x...; y...)
        T{S} where S; Vector{<:Real}
        var"weird name" = 1
        @label lbl
        @goto lbl
    end
    g(x) = 2x
    h(::Type{T}) where T = T
    (::Foo)(x) = x
    Base.show(io::IO, x::Foo) = print(io, "Foo")
    struct P{T} <: AbstractP
        x::T
        "doc"
        y::Int
        P(x) = new{typeof(x)}(x, 1)
    end
    mutable struct Q
        const a::Int
        b
    end
    abstract type AbstractP end
    primitive type Prim 8 end
    macro m(ex, args...)
        esc(:($ex + 1))
    end
    baremodule BM end
    end
    """
    function parse_all_ways(::Type{T}, code, version) where {T}
        parseall(T, code; version=version)
        parseall(T, code; filename="none", version=version)
        parsestmt(T, "f(x) = x + 1"; version=version)
        parsestmt(T, SubString("f(x) = x + 1"); version=version)
        parseatom(T, ":x"; version=version)
        parsestmt(T, "x"; version=version)
        parseall(T, "if x; y ? z end\nf(x"; ignore_errors=true, version=version)
        nothing
    end
    # requires 1.11 for `public`
    version = v"1.11"
    trees = isdefined(Base, :Syntax) ? (Expr, SyntaxNode, GreenNode, Base.Syntax) :
                                       (Expr, SyntaxNode, GreenNode)
    for T in trees
        parse_all_ways(T, code, version)
    end
    try parseall(Expr, "f(x") catch end
end
