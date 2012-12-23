## BIDIAG ##

bidiag(A) = bidiag(A, max(1,div(min(size(A)),2)), 2)

function bidiag(A::Matrix{Float64}, nb, nargout)
    (m,n) = size(A)
    s=min(m,n)
    w=max(m,n)
    D=zeros(s)
    E=zeros(s-1)
    tauq=zeros(s); taup=zeros(s)
    A0 = A = copy(A)

    k = 1-nb
    for k=1:nb:s-nb
        Dblk = zeros(nb)
        Eblk = zeros(nb)
        tauqblk = zeros(nb)
        taupblk = zeros(nb)
        X = zeros(m-k+1, nb)
        Y = zeros(n-k+1, nb)

        ccall(dlsym(libLAPACK, :dlabrd_), Void,
              (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
               Ptr{Float64},
               Ptr{Int32}, Ptr{Float64}, Ptr{Int32}),
              m-k+1, n-k+1, nb, A, m-k+1, Dblk, Eblk, tauqblk, taupblk,
              X, m-k+1, Y, n-k+1)
        b = k+nb-1  # end of block

        # accumulate reduced leading portion
        A0[k:b, k:end] = A[1:nb, :]
        A0[k:end, k:b] = A[:, 1:nb]

        D[k:b]    = Dblk
        E[k:b]    = Eblk
        tauq[k:b] = tauqblk
        taup[k:b] = taupblk

        # Update the trailing submatrix A[k+nb:m,k+nb:n],
        # using an update of the form  A := A - V*Y' - X*U'
        l = nb+1
        A = A[l:end,l:end]-A[l:end,1:nb]*Y[l:end,:]'-X[l:end,:]*A[1:nb,l:end]
        A0[k+nb:m, k+nb:n] = A
    end

    k = k+nb

    tauqblk = zeros(s-k+1)
    taupblk = zeros(s-k+1)

    ccall(dlsym(libLAPACK,:dgebd2_), Void,
          (Ptr{Int32}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32},
           Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
           Ptr{Float64}, Ptr{Int32}),
          m-k+1, n-k+1, A, m-k+1, zeros(s-k+1), zeros(s-k),
          tauqblk, taupblk, zeros(w-k+1), 0)
    A0[k:m,k:n] = A
    tauq[k:end] = tauqblk
    taup[k:end] = taupblk

    for j=k:s-1
        l = j-k+1
        D[j] = A[l,l]
        if m >= n
            E[j] = A[l,l+1]
        else
            E[j] = A[l+1,l]
        end
    end
    D[s] = A[s-k+1,s-k+1]

    if nargout == 2
        return (D, E)
    end

    if m >= n
        Q = copy(A0)
        ccall(dlsym(libLAPACK,:dorgbr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64},
               Ptr{Int32}, Ptr{Int32}),
              "Q", m, n, n, Q, m, tauq, zeros(s), s, 0)
        PT = A0
        ccall(dlsym(libLAPACK,:dorgbr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64},
               Ptr{Int32}, Ptr{Int32}),
              "P", n, n, n, PT, m, taup, zeros(s), s, 0)
        PT = PT[1:n,:]
    else
        Q = copy(A0)
        ccall(dlsym(libLAPACK,:dorgbr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64},
               Ptr{Int32}, Ptr{Int32}),
              "Q", m, m, n, Q, m, tauq, zeros(s), s, 0)
        Q = Q[:,1:m]
        PT = A0
        ccall(dlsym(libLAPACK,:dorgbr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64},
               Ptr{Int32}, Ptr{Int32}),
              "P", m, n, m, PT, m, taup, zeros(s), s, 0)
    end

    return (Q, D, E, PT)
end

blksvd(A) = blksvd(A, 1)

function blksvd(A::Matrix{Float64}, nargout)
    (m, n) = size(A)
    nb = 3

    (Q, D, E, PT) = bidiag(A, nb, 4)

    if m >= n
        ccall(dlsym(libLAPACK, :dbdsqr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}),
              "U",
              n, n, m, 0, D, E, PT, n, Q, m, zeros(1,1), 1, zeros(4*n,1), 0)
    else
        ccall(dlsym(libLAPACK, :dbdsqr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}),
              "L",
              m, n, m, 0, D, E, PT, m, Q, m, zeros(1,1), 1, zeros(4*m,1), 0)
    end

    S = D
    if nargout == 1
        return S
    end
    U = Q
    VT = PT
    V = VT'
    S = diagm(S)
    return (U, S, V)
end
