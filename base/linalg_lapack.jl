# linear algebra functions that use the Lapack module

function eig{T<:LapackType}(A::StridedMatrix{T}, vecs::Bool)
    n = size(A, 2)
    if n == 0; return vecs ? (zeros(T, 0), zeros(T, 0, 0)) : zeros(T, 0, 0); end

    if ishermitian(A); return Lapack.syev!(vecs ? 'V' : 'N', 'U', copy(A)); end

    if iscomplex(A)
        W, VR = Lapack.geev!('N', vecs ? 'V' : 'N', copy(A))[2:3]
        if vecs; return W, VR; end
        return W
    end

    VL, WR, WI, VR = Lapack.geev!('N', vecs ? 'V' : 'N', copy(A))
    if all(WI .== 0.) 
        if vecs; return WR, VR; end
        return WR
    end
    if vecs    
        evec = complex(zeros(T, n, n))
        j = 1
        while j <= n
            if WI[j] == 0.0
                evec[:,j] = VR[:,j]
            else
                evec[:,j]   = VR[:,j] + im*VR[:,j+1]
                evec[:,j+1] = VR[:,j] - im*VR[:,j+1]
                j += 1
            end
            j += 1
        end
        return complex(WR, WI), evec
    end
    complex(WR, WI)
end

eig{T<:Integer}(x::StridedMatrix{T}, vecs::Bool) = eig(float64(x), vecs)
eig(x::StridedMatrix) = eig(x, true)
eigvals(x::StridedMatrix) = eig(x, false)

function svd{T<:LapackType}(A::StridedMatrix{T},vecs::Bool,thin::Bool)
    m,n = size(A)
    if m == 0 || n == 0
        if vecs; return (eye(m, thin ? n : m), zeros(0), eye(n,n)); end
        return (zeros(T, 0, 0), zeros(T, 0), zeros(T, 0, 0))
    end
    if vecs; return Lapack.gesvd!(thin ? 'S' : 'A', thin ? 'S' : 'A', copy(A)); end
    Lapack.gesvd!('N', 'N', copy(A))
end

svd{T<:Integer}(x::StridedMatrix{T},vecs,thin) = svd(float64(x),vecs,thin)
svd(A::StridedMatrix) = svd(A,true,false)
svd(A::StridedMatrix, thin::Bool) = svd(A,true,thin)
svdvals(A) = svd(A,false,true)[2]

function sdd{T<:LapackType}(A::StridedMatrix{T},vecs::Bool,thin::Bool)
    m,n = size(A)
    if m == 0 || n == 0
        if vecs; return (eye(m, thin ? n : m), zeros(0), eye(n,n)); end
        return (zeros(T, 0, 0), zeros(T, 0), zeros(T, 0, 0))
    end
    if vecs; return Lapack.gesdd!(thin ? 'S' : 'A', copy(A)); end
    Lapack.gesdd!('N', copy(A))
end

sdd{T<:Integer}(x::StridedMatrix{T},vecs,thin) = sdd(float64(x),vecs,thin)
sdd(A::StridedMatrix) = sdd(A,true,false)
sdd(A::StridedMatrix, thin::Bool) = sdd(A,true,thin)
sddvals(A) = sdd(A,false,true)[2]

function (\){T<:LapackType}(A::StridedMatrix{T}, B::StridedVecOrMat{T})
    Acopy = copy(A)
    m, n  = size(Acopy)
    X     = copy(B)

    if m == n # Square
        if istriu(A) return Lapack.trtrs!('U', 'N', 'N', Acopy, X) end
        if istril(A) return Lapack.trtrs!('L', 'N', 'N', Acopy, X) end
        if ishermitian(A) return Lapack.sysv!('U', Acopy, X)[1] end
        return Lapack.gesv!(Acopy, X)[3]
    end
    Lapack.gels!('N', Acopy, X)[2]
end

(\){T1<:Real, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(float64(A), float64(B))

# TODO: use *gels transpose argument
(/)(A::StridedVecOrMat, B::StridedVecOrMat) = (B' \ A')'

## Destructive matrix exponential using algorithm from Higham, 2008,
## "Functions of Matrices: Theory and Computation", SIAM
function expm!{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    m, n = size(A)
    if m != n error("expm!: Matrix A must be square") end
    if m < 2 return exp(A) end
    ilo, ihi, scale = Lapack.gebal!('B', A)    # modifies A
    nA   = norm(A, 1)
    I    = convert(Array{T,2}, eye(n))
    ## For sufficiently small nA, use lower order Padé-Approximations
    if (nA <= 2.1)
        if nA > 0.95
            C = [17643225600.,8821612800.,2075673600.,302702400.,
                    30270240.,   2162160.,    110880.,     3960.,
                          90.,         1.]
        elseif nA > 0.25
            C = [17297280.,8648640.,1995840.,277200.,
                    25200.,   1512.,     56.,     1.]
        elseif nA > 0.015
            C = [30240.,15120.,3360.,
                   420.,   30.,   1.]
        else
            C = [120.,60.,12.,1.]
        end
        A2 = A * A
        P  = copy(I)
        U  = C[2] * P
        V  = C[1] * P
        for k in 1:(div(size(C, 1), 2) - 1)
            k2 = 2 * k
            P *= A2
            U += C[k2 + 2] * P
            V += C[k2 + 1] * P
        end
        U  = A * U
        X  = (V - U)\(V + U)
    else
        s  = log2(nA/5.4)               # power of 2 later reversed by squaring
        if s > 0
            si = iceil(s)
            A /= 2^si
        end
        CC = [64764752532480000.,32382376266240000.,7771770303897600.,
               1187353796428800.,  129060195264000.,  10559470521600.,
                   670442572800.,      33522128640.,      1323241920.,
                       40840800.,           960960.,           16380.,
                            182.,                1.]
        A2 = A * A
        A4 = A2 * A2
        A6 = A2 * A4
        U  = A * (A6 * (CC[14]*A6 + CC[12]*A4 + CC[10]*A2) +
                  CC[8]*A6 + CC[6]*A4 + CC[4]*A2 + CC[2]*I)
        V  = A6 * (CC[13]*A6 + CC[11]*A4 + CC[9]*A2) +
                  CC[7]*A6 + CC[5]*A4 + CC[3]*A2 + CC[1]*I
        X  = (V-U)\(V+U)
                         
        if s > 0            # squaring to reverse dividing by power of 2
            for t in 1:si X *= X end
        end
    end
                                        # Undo the balancing
    doscale = false                     # check if rescaling is needed
    for i = ilo:ihi
        if scale[i] != 1.
            doscale = true
            break
        end
    end
    if doscale
        for j = ilo:ihi
            scj = scale[j]
            if scj != 1.                # is this overkill?
                for i = ilo:ihi
                    X[i,j] *= scale[i]/scj
                end
            else
                for i = ilo:ihi
                    X[i,j] *= scale[i]
                end
            end
        end
    end
    if ilo > 1       # apply lower permutations in reverse order
        for j in (ilo-1):1:-1 rcswap!(j, int(scale[j]), X) end
    end
    if ihi < n       # apply upper permutations in forward order
        for j in (ihi+1):n    rcswap!(j, int(scale[j]), X) end
    end
    X
end

## Swap rows j and jp and columns j and jp in X
function rcswap!{T<:Number}(j::Int, jp::Int, X::StridedMatrix{T})
    for k in 1:size(X, 2)
        tmp     = X[k,j]
        X[k,j]  = X[k,jp]
        X[k,jp] = tmp
        tmp     = X[j,k]
        X[j,k]  = X[jp,k]
        X[jp,k] = tmp
    end
end

# Matrix exponential
expm{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T}) = expm!(copy(A))
expm{T<:Integer}(A::StridedMatrix{T}) = expm!(float(A))

