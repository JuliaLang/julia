## linalg.jl: Some generic Linear Algebra definitions

scale(X::AbstractArray, s::Number) = scale!(copy(X), s)
scale(s::Number, X::AbstractArray) = scale!(copy(X), s)

function scale!(X::AbstractArray, s::Number)
    for i in 1:length(X)
        @inbounds X[i] *= s
    end
    X
end
scale!(s::Number, X::AbstractArray) = scale!(X, s)

cross(a::AbstractVector, b::AbstractVector) = [a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1]]

triu(M::AbstractMatrix) = triu(M,0)
tril(M::AbstractMatrix) = tril(M,0)
#triu{T}(M::AbstractMatrix{T}, k::Integer)
#tril{T}(M::AbstractMatrix{T}, k::Integer)
triu!(M::AbstractMatrix) = triu!(M,0)
tril!(M::AbstractMatrix) = tril!(M,0)

#diff(a::AbstractVector)
#diff(a::AbstractMatrix, dim::Integer)
diff(a::AbstractMatrix) = diff(a, 1)
diff(a::AbstractVector) = [ a[i+1] - a[i] for i=1:length(a)-1 ]

function diff(A::AbstractMatrix, dim::Integer)
    if dim == 1
        [A[i+1,j] - A[i,j] for i=1:size(A,1)-1, j=1:size(A,2)]
    else
        [A[i,j+1] - A[i,j] for i=1:size(A,1), j=1:size(A,2)-1]
    end
end


gradient(F::AbstractVector) = gradient(F, [1:length(F)])
gradient(F::AbstractVector, h::Real) = gradient(F, [h*(1:length(F))])
#gradient(F::AbstractVector, h::AbstractVector)

diag(A::AbstractVector) = error("use diagm instead of diag to construct a diagonal matrix")
#diag(A::AbstractMatrix)

#diagm{T}(v::AbstractVecOrMat{T})

function norm{T}(x::AbstractVector{T}, p::Number)
    if length(x) == 0
        a = zero(T)
    elseif p == Inf
        a = maximum(abs(x))
    elseif p == -Inf
        a = minimum(abs(x))
    else
        absx = abs(x)
        dx = maximum(absx)
        if dx != zero(T)
            scale!(absx, 1/dx)
            a = dx * (sum(absx.^p).^(1/p))
        else
            a = sum(absx.^p).^(1/p)
        end
    end
    float(a)
end
norm{T<:Integer}(x::AbstractVector{T}, p::Number) = norm(float(x), p)
norm(x::AbstractVector) = norm(x, 2)

function norm(A::AbstractMatrix, p::Number=2)
    m, n = size(A)
    if m == 0 || n == 0
        a = zero(eltype(A))
    elseif m == 1 || n == 1
        a = norm(reshape(A, length(A)), p)
    elseif p == 1
        a = maximum(sum(abs(A),1))
    elseif p == 2
        a = maximum(svdvals(A))
    elseif p == Inf
        a = maximum(sum(abs(A),2))
    else
        throw(ArgumentError("invalid p-norm p=$p. Valid: 1, 2, Inf"))
    end
    float(a)
end

norm(x::Number, p=nothing) = abs(x)

normfro(A::AbstractMatrix) = norm(reshape(A, length(A)))
normfro(x::Number) = abs(x)

rank(A::AbstractMatrix, tol::Real) = sum(svdvals(A) .> tol)
function rank(A::AbstractMatrix)
    m,n = size(A)
    (m == 0 || n == 0) && return 0
    sv = svdvals(A)
    return sum(sv .> maximum(size(A))*eps(sv[1]))
end
rank(x::Number) = x==0 ? 0 : 1

function trace(A::AbstractMatrix)
    chksquare(A)
    sum(diag(A))
end
trace(x::Number) = x

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)

inv(a::AbstractVector) = error("argument must be a square matrix")

function \{TA<:Number,TB<:Number}(A::AbstractMatrix{TA}, B::AbstractVecOrMat{TB})
    TC = typeof(one(TA)/one(TB))
    return TB == TC ? A_ldiv_B!(A, copy(B)) : A_ldiv_B!(A, convert(Array{TC}, B))
end
\(a::AbstractVector, b::AbstractArray) = reshape(a, length(a), 1) \ b
/(A::AbstractVecOrMat, B::AbstractVecOrMat) = (B' \ A')'

cond(x::Number) = x == 0 ? Inf : 1.0
cond(x::Number, p) = cond(x)

function issym(A::AbstractMatrix)
    m, n = size(A)
    m==n || return false
    for i = 1:(n-1), j = (i+1):n
        if A[i,j] != A[j,i]
            return false
        end
    end
    return true
end

issym(x::Number) = true

function ishermitian(A::AbstractMatrix)
    m, n = size(A)
    m==n || return false
    for i = 1:n, j = i:n
        if A[i,j] != conj(A[j,i])
            return false
        end
    end
    return true
end

ishermitian(x::Number) = (x == conj(x))

function istriu(A::AbstractMatrix)
    m, n = size(A)
    for j = 1:min(n,m-1), i = j+1:m
        if A[i,j] != 0
            return false
        end
    end
    return true
end

function istril(A::AbstractMatrix)
    m, n = size(A)
    for j = 2:n, i = 1:min(j-1,m)
        if A[i,j] != 0
            return false
        end
    end
    return true
end

istriu(x::Number) = true
istril(x::Number) = true

linreg{T<:Number}(X::StridedVecOrMat{T}, y::Vector{T}) = [ones(T, size(X,1)) X] \ y

# weighted least squares
function linreg(x::AbstractVector, y::AbstractVector, w::AbstractVector)
    sw = sqrt(w)
    [sw sw.*x] \ (sw.*y)
end

# multiply by diagonal matrix as vector
#diagmm!(C::AbstractMatrix, A::AbstractMatrix, b::AbstractVector)

#diagmm!(C::AbstractMatrix, b::AbstractVector, A::AbstractMatrix)

scale!(A::AbstractMatrix, b::AbstractVector) = scale!(A,A,b)
scale!(b::AbstractVector, A::AbstractMatrix) = scale!(A,b,A)

#diagmm(A::AbstractMatrix, b::AbstractVector)
#diagmm(b::AbstractVector, A::AbstractMatrix)

#^(A::AbstractMatrix, p::Number)

#findmax(a::AbstractArray)
#findmin(a::AbstractArray)

#rref{T}(A::AbstractMatrix{T})

function peakflops(n::Integer=2000; parallel::Bool=false)
    a = rand(100,100)
    t = @elapsed a*a
    a = rand(n,n)
    t = @elapsed a*a
    parallel ? sum(pmap(peakflops, [ n for i in 1:nworkers()])) : (2*n^3/t)
end

# Givens rotations
function givens{T<:FloatingPoint}(f::T, g::T)
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

function givens{T<:FloatingPoint}(f::Complex{T}, g::Complex{T})
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
givens(f::Number,g::Number) = givens(float(f), float(g))