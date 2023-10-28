module Stubs

module Random
    let Random_PkgID = Base.PkgId(Base.UUID(0x9a3f8284_a2c9_5f02_9a11_845980a1fd5c), "Random")
        RANDOM_MODULE_REF = Ref{Module}()

        global delay_initialize
        function delay_initialize()
            if !isassigned(RANDOM_MODULE_REF)
                RANDOM_MODULE_REF[] = Base.require(Random_PkgID)
            end
            return ccall(:jl_module_world, Csize_t, (Any,), RANDOM_MODULE_REF[])
        end
    end

    import Base: rand, randn
    function rand(args...)
        Base.invoke_in_world(delay_initialize(), rand, args...)
    end

    function randn(args...)
        Base.invoke_in_world(delay_initialize(), randn, args...)
    end
end

Base.Docs.getdoc(::typeof(Base.rand)) = (Random.delay_initialize(); nothing)
Base.Docs.getdoc(::typeof(Base.randn)) = (Random.delay_initialize(); nothing)

module LinearAlgebra
    let LinearAlgebra_PkgID = Base.PkgId(Base.UUID(0x37e2e46d_f89d_539d_b4ee_838fcccc9c8e), "LinearAlgebra")
        LINALG_MODULE_REF = Ref{Module}()

        global delay_initialize
        function delay_initialize()
            if !isassigned(LINALG_MODULE_REF)
                LINALG_MODULE_REF[] = Base.require(LinearAlgebra_PkgID)
            end
            return ccall(:jl_module_world, Csize_t, (Any,), LINALG_MODULE_REF[])
        end
    end

    import Base: adjoint, *, \, ^, acos, acosh, acot, acoth, acsc, acsch, asec, asech, asin, asinh, atan, atanh, cis,
                 cos, cosh, cot, coth, csc, csch, exp, inv, isone, kron, kron!, log, sec, sech, sin, sinh,
                 sincos, sqrt, tan, tanh, transpose, sqrt, isapprox

    adjoint(B::Union{BitMatrix, BitVector}) = Base.invoke_in_world(delay_initialize(), adjoint, B)
    adjoint(a::AbstractArray) = Base.invoke_in_world(delay_initialize(), adjoint, B)

    *(A::AbstractMatrix, B::AbstractMatrix, C::AbstractMatrix, D::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), *, A, B, C, D)
    *(A::AbstractMatrix, B::AbstractMatrix, C::AbstractMatrix, x::AbstractVector) = Base.invoke_in_world(delay_initialize(), *, A, B, C, x)
    *(A::AbstractMatrix, B::AbstractMatrix, x::AbstractVector) = Base.invoke_in_world(delay_initialize(), *, A, B, x)
    *(A::AbstractMatrix, B::AbstractMatrix, C::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), *, A, B, C)
    *(a::AbstractVector, B::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), *, a, B)
    *(A::AbstractMatrix{T}, x::AbstractVector{S}) where {T, S} = Base.invoke_in_world(delay_initialize(), *, A, x)
    *(A::AbstractMatrix, B::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), *, A, B)
    \(a::AbstractVector, b::AbstractArray) = Base.invoke_in_world(delay_initialize(), \, a, b)
    ^(i::Irrational{:ℯ}, A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), ^, i, A)

    acos(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), acos, A)
    acosh(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), acosh, A)
    acot(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), acot, A)
    acoth(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), acoth, A)
    acsc(A::AbstractMatrix{T}) where T  = Base.invoke_in_world(delay_initialize(), acsc, A)
    acsch(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), acsch, A)
    asec(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), asec, A)
    asech(A::AbstractMatrix{T}) where T= Base.invoke_in_world(delay_initialize(), asech, A)
    asin(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), asin, A)
    asinh(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), asinh, A)
    atan(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), atan, A)
    atanh(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), atanh, A)
    # cis(A::AbstractMatrix{<:Union{Float32, Float64, Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8, Complex{<:Union{Float32, Float64, Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8}}, Rational{<:Union{Float32, Float64, Int16, Int32, Int64, Int8, UInt16, UInt32, UInt64, UInt8}}}}) @ LinearAlgebra ~/.julia/juliaup/julia-1.10.0-beta2+0.x64.linux.gnu/share/julia/stdlib/v1.10/LinearAlgebra/src/dense.jl:616
    cis(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), cis, A)
    cos(A::AbstractMatrix{<:Complex}) = Base.invoke_in_world(delay_initialize(), cos, A)
    cos(A::AbstractMatrix{<:Real}) = Base.invoke_in_world(delay_initialize(), cos, A)
    cosh(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), cosh, A)
    cot(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), cot, A)
    coth(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), coth, A)
    csc(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), csc, A)
    csch(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), csch, A)


    exp(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), exp, A)
    inv(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), inv, A)
    isone(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), isone, A)
    kron(a::BitMatrix, b::BitMatrix) = Base.invoke_in_world(delay_initialize(), kron, a, b)
    kron(a::BitVector, b::BitVector) = Base.invoke_in_world(delay_initialize(), kron, a, b)
    kron(a::AbstractVector{T}, b::AbstractVector{S}) where {T, S} = Base.invoke_in_world(delay_initialize(), kron, a, b)
    kron!(R::BitMatrix, a::BitMatrix, b::BitMatrix) = Base.invoke_in_world(delay_initialize(), kron!, R, a, B)
    kron!(R::BitVector, a::BitVector, b::BitVector) = Base.invoke_in_world(delay_initialize(), kron!, R, a, b)
    kron!(c::AbstractVector, a::AbstractVector, b::AbstractVector) = Base.invoke_in_world(delay_initialize(), kron!, c, a, b)
    log(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), log, A)
    sec(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), sec, A)
    sech(A::AbstractMatrix{T}) where T = Base.invoke_in_world(delay_initialize(), sech, A)
    sin(A::AbstractMatrix{<:Complex}) = Base.invoke_in_world(delay_initialize(), sin, A)
    sin(A::AbstractMatrix{<:Real}) = Base.invoke_in_world(delay_initialize(), sin, A)
    sincos(A::AbstractMatrix{<:Real}) = Base.invoke_in_world(delay_initialize(), sincos, A)
    sincos(A::AbstractMatrix{<:Complex}) = Base.invoke_in_world(delay_initialize(), sincos, A)
    sinh(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), sinh, A)
    sqrt(A::AbstractMatrix{T}) where T<:Union{Real, Complex} = Base.invoke_in_world(delay_initialize(), sqrt, A)
    tan(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), tan, A)
    tanh(A::AbstractMatrix) = Base.invoke_in_world(delay_initialize(), tanh, A)
    transpose(B::Union{BitMatrix, BitVector}) = Base.invoke_in_world(delay_initialize(), transpose, B)
    transpose(a::AbstractArray) = Base.invoke_in_world(delay_initialize(), transpose, a)
    # isapprox(x::AbstractArray, y::AbstractArray; atol, rtol, nans, norm) = Base.invoke_in_world(delay_initialize(), isapprox, x, y; atol, rtol, nans, norm)
    isapprox(x::AbstractArray, y::AbstractArray; kwargs...) = Base.invoke_in_world(delay_initialize(), isapprox, x, y; kwargs...)

end

Base.Docs.getdoc(::typeof(Base.adjoint)) = (LinearAlgebra.delay_initialize(); nothing)

function delete_stubs(mod)
    for name in names(mod, imported=true)
        if name == :delay_initialize
            continue
        end
        obj = getglobal(mod, name)
        if obj isa Function
            ms = Base.methods(obj, mod)
            for m in ms
                ccall(:jl_push_newly_deleted, Cvoid, (Any,), m)
                ccall(:jl_method_table_disable_incremental, Cvoid, (Any, Any), Base.get_methodtable(m), m)
            end
        end
    end
end

end
