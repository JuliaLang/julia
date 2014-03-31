immutable Givens{T} <: AbstractMatrix{T}
    size::Int
    i1::Int
    i2::Int
    c::T
    s::T
    r::T
end
type Rotation{T}
    rotations::Vector{T}
end
typealias AbstractRotation Union(Givens, Rotation)

function givensAlgorithm{T<:FloatingPoint}(f::T, g::T)
    zeropar = zero(T)
    onepar = one(T)
    twopar = 2one(T)

    safmin = realmin(T)
    epspar = eps(T)
    safmn2 = twopar^itrunc(log(safmin/epspar)/log(twopar)/twopar)
    safmx2 = onepar/safmn2

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
        if scalepar >= safmx2
            count = 0
            while true
                count += 1
                f1 *= safmn2
                g1 *= safmn2
                scalepar = max(abs(f1), abs(g1))
                if scalepar < safmx2 break end
            end
            r = hypot(f1, g1)
            cs = f1/r
            sn = g1/r
            for i = 1:count
                r *= safmx2
            end
        elseif scalepar <= safmn2
            count = 0
            while true
                count += 1
                f1 *= safmx2
                g1 *= safmx2
                scalepar = max(abs(f1), abs(g1))
                if scalepar > safmn2 break end
            end
            r = hypot(f1, g1)
            cs = f1/r
            sn = g1/r
            for i = 1:count
                r *= safmn2
            end
        else
            r = hypot(f1, g1)
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

function givensAlgorithm{T<:FloatingPoint}(f::Complex{T}, g::Complex{T})
    twopar, onepar, zeropar = 2one(T), one(T), zero(T)
    czero = zero(Complex{T})

    abs1(ff) = max(abs(real(ff)), abs(imag(ff)))
    safmin = realmin(T)
    epspar = eps(T)
    safmn2 = twopar^itrunc(log(safmin/epspar)/log(twopar)/twopar)
    safmx2 = onepar/safmn2
    scalepar = max(abs1(f), abs1(g))
    fs = f
    gs = g
    count = 0
    if scalepar >= safmx2
        while true
            count += 1
            fs *= safmn2
            gs *= safmn2
            scalepar *= safmn2
            if scalepar < safmx2 break end
        end
    elseif scalepar <= safmn2
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
            if scalepar > safmn2 break end
        end
    end
    f2 = abs2(fs)
    g2 = abs2(gs)
    if f2 <= max(g2, onepar)*safmin

     # This is a rare case: F is very small.

        if f == 0
            cs = zero
            r = hypot(real(g), imag(g))
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

function givens{T}(f::T, g::T, i1::Integer, i2::Integer, size::Integer)
    i2 <= size || error("indices cannot be larger than size Givens rotation matrix")
    i1 < i2 || error("second index must be larger than the first")
    h = givensAlgorithm(f, g)
    Givens(size, i1, i2, convert(T, h[1]), convert(T, h[2]), convert(T, h[3]))
end

function givens{T}(A::AbstractMatrix{T}, i1::Integer, i2::Integer, col::Integer)
    i1 < i2 || error("second index must be larger than the first")
    h = givensAlgorithm(A[i1,col], A[i2,col])
    Givens(size(A, 1), i1, i2, convert(T, h[1]), convert(T, h[2]), convert(T, h[3]))
end

*{T}(G1::Givens{T}, G2::Givens{T}) = Rotation(push!(push!(Givens{T}[], G2), G1))
*(G::Givens, B::BitArray{2}) = error("method not defined")
*{TBf,TBi}(G::Givens, B::SparseMatrixCSC{TBf,TBi}) = error("method not defined")
*(R::AbstractRotation, A::AbstractMatrix) = A_mul_B!(R, copy(A))

A_mul_Bc(A::AbstractMatrix, R::Rotation) = A_mul_Bc!(copy(A), R)

getindex(G::Givens, i::Integer, j::Integer) = i == j ? (i == G.i1 || i == G.i2 ? G.c : one(G.c)) : (i == G.i1 && j == G.i2 ? G.s : (i == G.i2 && j == G.i1 ? -G.s : zero(G.s)))
size(G::Givens) = (G.size, G.size)
size(G::Givens, i::Integer) = 1 <= i <= 2 ? G.size : 1
A_mul_B!(G1::Givens, G2::Givens) = error("Operation not supported. Consider *")
function A_mul_B!(G::Givens, A::AbstractMatrix)
    m, n = size(A)
    for i = 1:n
        tmp = G.c*A[G.i1,i] + G.s*A[G.i2,i]
        A[G.i2,i] = G.c*A[G.i2,i] - conj(G.s)*A[G.i1,i]
        A[G.i1,i] = tmp
    end
    return A
end
function A_mul_Bc!(A::AbstractMatrix, G::Givens)
    m, n = size(A)
    for i = 1:m
        tmp = G.c*A[i,G.i1] + conj(G.s)*A[i,G.i2]
        A[i,G.i2] = G.c*A[i,G.i2] - G.s*A[i,G.i1]
        A[i,G.i1] = tmp
    end
    return A
end
function A_mul_B!(G::Givens, R::Rotation)
    push!(R.rotations, G)
    return R
end
function A_mul_B!(R::Rotation, A::AbstractMatrix)
    for i = 1:length(R.rotations)
        A_mul_B!(R.rotations[i], A)
    end
    return A
end
function A_mul_Bc!(A::AbstractMatrix, R::Rotation)
    for i = 1:length(R.rotations)
        A_mul_Bc!(A, R.rotations[i])
    end
    return A
end
