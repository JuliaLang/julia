# This file is a part of Julia. License is MIT: https://julialang.org/license

# givensAlgorithm functions are derived from LAPACK, see below

abstract type AbstractRotation{T} end

transpose(R::AbstractRotation) = error("transpose not implemented for $(typeof(R)). Consider using adjoint instead of transpose.")

function (*)(R::AbstractRotation{T}, A::AbstractVecOrMat{S}) where {T,S}
    TS = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    lmul!(convert(AbstractRotation{TS}, R), TS == S ? copy(A) : convert(AbstractArray{TS}, A))
end
(*)(A::AbstractVector, adjR::Adjoint{<:Any,<:AbstractRotation}) = _absvecormat_mul_adjrot(A, adjR)
(*)(A::AbstractMatrix, adjR::Adjoint{<:Any,<:AbstractRotation}) = _absvecormat_mul_adjrot(A, adjR)
function _absvecormat_mul_adjrot(A::AbstractVecOrMat{T}, adjR::Adjoint{<:Any,<:AbstractRotation{S}}) where {T,S}
    R = adjR.parent
    TS = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    rmul!(TS.(A), convert(AbstractRotation{TS}, R)')
end
function(*)(A::AbstractMatrix{T}, R::AbstractRotation{S}) where {T,S}
    TS = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    rmul!(TS.(A), convert(AbstractRotation{TS}, R))
end

"""
    LinearAlgebra.Givens(i1,i2,c,s) -> G

A Givens rotation linear operator. The fields `c` and `s` represent the cosine and sine of
the rotation angle, respectively. The `Givens` type supports left multiplication `G*A` and
conjugated transpose right multiplication `A*G'`. The type doesn't have a `size` and can
therefore be multiplied with matrices of arbitrary size as long as `i2<=size(A,2)` for
`G*A` or `i2<=size(A,1)` for `A*G'`.

See also: [`givens`](@ref)
"""
struct Givens{T} <: AbstractRotation{T}
    i1::Int
    i2::Int
    c::T
    s::T
end
struct Rotation{T} <: AbstractRotation{T}
    rotations::Vector{Givens{T}}
end

convert(::Type{T}, r::T) where {T<:AbstractRotation} = r
convert(::Type{T}, r::AbstractRotation) where {T<:AbstractRotation} = T(r)

Givens{T}(G::Givens{T}) where {T} = G
Givens{T}(G::Givens) where {T} = Givens(G.i1, G.i2, convert(T, G.c), convert(T, G.s))
Rotation{T}(R::Rotation{T}) where {T} = R
Rotation{T}(R::Rotation) where {T} = Rotation{T}([Givens{T}(g) for g in R.rotations])
AbstractRotation{T}(G::Givens) where {T} = Givens{T}(G)
AbstractRotation{T}(R::Rotation) where {T} = Rotation{T}(R)

adjoint(G::Givens) = Givens(G.i1, G.i2, G.c', -G.s)
adjoint(R::Rotation) = Adjoint(R)
function Base.copy(aG::Adjoint{<:Any,<:Givens})
    G = aG.parent
    return Givens(G.i1, G.i2, conj(G.c), -G.s)
end
Base.copy(aR::Adjoint{<:Any,Rotation{T}}) where {T} = Rotation{T}(reverse!([r' for r in aR.parent.rotations]))

floatmin2(::Type{Float32}) = reinterpret(Float32, 0x26000000)
floatmin2(::Type{Float64}) = reinterpret(Float64, 0x21a0000000000000)
floatmin2(::Type{T}) where {T} = (twopar = 2one(T); twopar^trunc(Integer,log(floatmin(T)/eps(T))/log(twopar)/twopar))

# derived from LAPACK's dlartg
# Copyright:
# Univ. of Tennessee
# Univ. of California Berkeley
# Univ. of Colorado Denver
# NAG Ltd.
function givensAlgorithm(f::T, g::T) where T<:AbstractFloat
    onepar = one(T)
    twopar = 2one(T)
    T0 = typeof(onepar) # dimensionless
    zeropar = T0(zero(T)) # must be dimensionless

    # need both dimensionful and dimensionless versions of these:
    safmn2 = floatmin2(T0)
    safmn2u = floatmin2(T)
    safmx2 = one(T)/safmn2
    safmx2u = oneunit(T)/safmn2

    if g == 0
        cs = onepar
        sn = zeropar
        r = f
    elseif f == 0
        cs = zeropar
        sn = onepar
        r = g
    else
        f1 = f
        g1 = g
        scalepar = max(abs(f1), abs(g1))
        if scalepar >= safmx2u
            count = 0
            while true
                count += 1
                f1 *= safmn2
                g1 *= safmn2
                scalepar = max(abs(f1), abs(g1))
                if scalepar < safmx2u break end
            end
            r = sqrt(f1*f1 + g1*g1)
            cs = f1/r
            sn = g1/r
            for i = 1:count
                r *= safmx2
            end
        elseif scalepar <= safmn2u
            count = 0
            while true
                count += 1
                f1 *= safmx2
                g1 *= safmx2
                scalepar = max(abs(f1), abs(g1))
                if scalepar > safmn2u break end
            end
            r = sqrt(f1*f1 + g1*g1)
            cs = f1/r
            sn = g1/r
            for i = 1:count
                r *= safmn2
            end
        else
            r = sqrt(f1*f1 + g1*g1)
            cs = f1/r
            sn = g1/r
        end
        if abs(f) > abs(g) && cs < 0
            cs = -cs
            sn = -sn
            r = -r
        end
    end
    return cs, sn, r
end

# derived from LAPACK's zlartg
# Copyright:
# Univ. of Tennessee
# Univ. of California Berkeley
# Univ. of Colorado Denver
# NAG Ltd.
function givensAlgorithm(f::Complex{T}, g::Complex{T}) where T<:AbstractFloat
    twopar, onepar = 2one(T), one(T)
    T0 = typeof(onepar) # dimensionless
    zeropar = T0(zero(T)) # must be dimensionless
    czero = complex(zeropar)

    abs1(ff) = max(abs(real(ff)), abs(imag(ff)))
    safmin = floatmin(T0)
    safmn2 = floatmin2(T0)
    safmn2u = floatmin2(T)
    safmx2 = one(T)/safmn2
    safmx2u = oneunit(T)/safmn2
    scalepar = max(abs1(f), abs1(g))
    fs = f
    gs = g
    count = 0
    if scalepar >= safmx2u
        while true
            count += 1
            fs *= safmn2
            gs *= safmn2
            scalepar *= safmn2
            if scalepar < safmx2u break end
        end
    elseif scalepar <= safmn2u
        if g == 0
            cs = onepar
            sn = czero
            r = f
            return cs, sn, r
        end
        while true
            count -= 1
            fs *= safmx2
            gs *= safmx2
            scalepar *= safmx2
            if scalepar > safmn2u break end
        end
    end
    f2 = abs2(fs)
    g2 = abs2(gs)
    if f2 <= max(g2, oneunit(T))*safmin
        # This is a rare case: F is very small.
        if f == 0
            cs = zero(T)
            r = complex(hypot(real(g), imag(g)))
            # do complex/real division explicitly with two real divisions
            d = hypot(real(gs), imag(gs))
            sn = complex(real(gs)/d, -imag(gs)/d)
            return cs, sn, r
        end
        f2s = hypot(real(fs), imag(fs))
        # g2 and g2s are accurate
        # g2 is at least safmin, and g2s is at least safmn2
        g2s = sqrt(g2)
        # error in cs from underflow in f2s is at most
        # unfl / safmn2 .lt. sqrt(unfl*eps) .lt. eps
        # if max(g2,one)=g2, then f2 .lt. g2*safmin,
        # and so cs .lt. sqrt(safmin)
        # if max(g2,one)=one, then f2 .lt. safmin
        # and so cs .lt. sqrt(safmin)/safmn2 = sqrt(eps)
        # therefore, cs = f2s/g2s / sqrt( 1 + (f2s/g2s)**2 ) = f2s/g2s
        cs = f2s/g2s
        # make sure abs(ff) = 1
        # do complex/real division explicitly with 2 real divisions
        if abs1(f) > 1
            d = hypot(real(f), imag(f))
            ff = complex(real(f)/d, imag(f)/d)
        else
            dr = safmx2*real(f)
            di = safmx2*imag(f)
            d = hypot(dr, di)
            ff = complex(dr/d, di/d)
        end
        sn = ff*complex(real(gs)/g2s, -imag(gs)/g2s)
        r = cs*f + sn*g
    else
        # This is the most common case.
        # Neither F2 nor F2/G2 are less than SAFMIN
        # F2S cannot overflow, and it is accurate
        f2s = sqrt(onepar + g2/f2)
        # do the f2s(real)*fs(complex) multiply with two real multiplies
        r = complex(f2s*real(fs), f2s*imag(fs))
        cs = onepar/f2s
        d = f2 + g2
        # do complex/real division explicitly with two real divisions
        sn = complex(real(r)/d, imag(r)/d)
        sn *= conj(gs)
        if count != 0
            if count > 0
                for i = 1:count
                    r *= safmx2
                end
            else
                for i = 1:-count
                    r *= safmn2
                end
            end
        end
    end
    return cs, sn, r
end

givensAlgorithm(f, g) = givensAlgorithm(promote(float(f), float(g))...)

"""

    givens(f::T, g::T, i1::Integer, i2::Integer) where {T} -> (G::Givens, r::T)

Computes the Givens rotation `G` and scalar `r` such that for any vector `x` where
```
x[i1] = f
x[i2] = g
```
the result of the multiplication
```
y = G*x
```
has the property that
```
y[i1] = r
y[i2] = 0
```

See also: [`LinearAlgebra.Givens`](@ref)
"""
function givens(f::T, g::T, i1::Integer, i2::Integer) where T
    if i1 == i2
        throw(ArgumentError("Indices must be distinct."))
    end
    c, s, r = givensAlgorithm(f, g)
    if i1 > i2
        s = -conj(s)
        i1,i2 = i2,i1
    end
    Givens(i1, i2, convert(T, c), convert(T, s)), r
end
"""
    givens(A::AbstractArray, i1::Integer, i2::Integer, j::Integer) -> (G::Givens, r)

Computes the Givens rotation `G` and scalar `r` such that the result of the multiplication
```
B = G*A
```
has the property that
```
B[i1,j] = r
B[i2,j] = 0
```

See also: [`LinearAlgebra.Givens`](@ref)
"""
givens(A::AbstractMatrix, i1::Integer, i2::Integer, j::Integer) =
    givens(A[i1,j], A[i2,j],i1,i2)


"""
    givens(x::AbstractVector, i1::Integer, i2::Integer) -> (G::Givens, r)

Computes the Givens rotation `G` and scalar `r` such that the result of the multiplication
```
B = G*x
```
has the property that
```
B[i1] = r
B[i2] = 0
```

See also: [`LinearAlgebra.Givens`](@ref)
"""
givens(x::AbstractVector, i1::Integer, i2::Integer) =
    givens(x[i1], x[i2], i1, i2)


function getindex(G::Givens, i::Integer, j::Integer)
    if i == j
        if i == G.i1 || i == G.i2
            G.c
        else
            oneunit(G.c)
        end
    elseif i == G.i1 && j == G.i2
        G.s
    elseif i == G.i2 && j == G.i1
        -conj(G.s)
    else
        zero(G.s)
    end
end

@inline function lmul!(G::Givens, A::AbstractVecOrMat)
    require_one_based_indexing(A)
    m, n = size(A, 1), size(A, 2)
    if G.i2 > m
        throw(DimensionMismatch("column indices for rotation are outside the matrix"))
    end
    @inbounds for i = 1:n
        a1, a2 = A[G.i1,i], A[G.i2,i]
        A[G.i1,i] =       G.c *a1 + G.s*a2
        A[G.i2,i] = -conj(G.s)*a1 + G.c*a2
    end
    return A
end
@inline function rmul!(A::AbstractMatrix, G::Givens)
    require_one_based_indexing(A)
    m, n = size(A, 1), size(A, 2)
    if G.i2 > n
        throw(DimensionMismatch("column indices for rotation are outside the matrix"))
    end
    @inbounds for i = 1:m
        a1, a2 = A[i,G.i1], A[i,G.i2]
        A[i,G.i1] = a1*G.c - a2*G.s'
        A[i,G.i2] = a1*G.s + a2*G.c
    end
    return A
end

function lmul!(G::Givens, R::Rotation)
    push!(R.rotations, G)
    return R
end
function lmul!(R::Rotation, A::AbstractMatrix)
    @inbounds for i = 1:length(R.rotations)
        lmul!(R.rotations[i], A)
    end
    return A
end
function rmul!(A::AbstractMatrix, adjR::Adjoint{<:Any,<:Rotation})
    R = adjR.parent
    @inbounds for i = 1:length(R.rotations)
        rmul!(A, adjoint(R.rotations[i]))
    end
    return A
end
*(G1::Givens{T}, G2::Givens{T}) where {T} = Rotation(push!(push!(Givens{T}[], G2), G1))

# TODO: None of the following disambiguation methods are great. They should perhaps
# instead be MethodErrors, or revised.
#
# disambiguation methods: *(Adj/Trans of AbsVec or AbsMat, Adj of AbstractRotation)
*(A::Adjoint{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:AbstractRotation}) = copy(A) * B
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractRotation}) = copy(A) * B
*(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:AbstractRotation}) = copy(A) * B
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractRotation}) = copy(A) * B
# disambiguation methods: *(Adj/Trans of AbsTri or RealHermSymComplex{Herm|Sym}, Adj of AbstractRotation)
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractRotation}) = copy(A) * B
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:AbstractRotation}) = copy(A) * B
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Adjoint{<:Any,<:AbstractRotation}) = copy(A) * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Adjoint{<:Any,<:AbstractRotation}) = copy(A) * B
# disambiguation methods: *(Diag/AbsTri, Adj of AbstractRotation)
*(A::Diagonal, B::Adjoint{<:Any,<:AbstractRotation}) = A * copy(B)
*(A::AbstractTriangular, B::Adjoint{<:Any,<:AbstractRotation}) = A * copy(B)
