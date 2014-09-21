## Based on "Multi-Threading and One-Sided Communication in Parallel LU Factorization"
## http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.138.4361&rank=7

function hpl_seq(A::Matrix, b::Vector)

    blocksize = 5

    n = size(A,1)
    A = [A b]

    B_rows = linspace(0, n, div(n,blocksize)+1)
    B_rows[end] = n
    B_cols = [B_rows, [n+1]]
    nB = length(B_rows)
    depend = zeros(Bool, nB, nB) # In parallel, depend needs to be able to hold futures

    ## Small matrix case
    if nB <= 1
        x = A[1:n, 1:n] \ A[:,n+1]
        return x
    end

    ## Add a ghost row of dependencies to boostrap the computation
    for j=1:nB; depend[1,j] = true; end

    for i=1:(nB-1)
        ## Threads for panel factorizations
        I = (B_rows[i]+1):B_rows[i+1]
        #(depend[i+1,i], panel_p) = spawn(panel_factor_seq, I, depend[i,i])
        (depend[i+1,i], panel_p) = panel_factor_seq(A, I, depend[i,i])

        ## Threads for trailing updates
        for j=(i+1):nB
            J = (B_cols[j]+1):B_cols[j+1]
            #depend[i+1,j] = spawn(trailing_update_seq, I, J, panel_p, depend[i+1,i],depend[i,j])
            depend[i+1,j] = trailing_update_seq(A, I, J, panel_p, depend[i+1,i],depend[i,j])
        end
    end

    ## Completion of the last diagonal block signals termination
    #wait(depend[nB, nB])

    ## Solve the triangular system
    x = triu(A[1:n,1:n]) \ A[:,n+1]

    return x

end ## hpl()


### Panel factorization ###

function panel_factor_seq(A, I, col_dep)
    n = size (A, 1)

    ## Enforce dependencies
    #wait(col_dep)

    ## Factorize a panel
    K = I[1]:n
    panel_p = lufact!(sub(A, K, I))[:p] # Economy mode

    ## Panel permutation
    panel_p = K[panel_p]

    return (true, panel_p)

end ## panel_factor_seq()


### Trailing update ###

function trailing_update_seq(A, I, J, panel_p, row_dep, col_dep)

    n = size (A, 1)

    ## Enforce dependencies
    #wait(row_dep, col_dep)

    ## Apply permutation from pivoting
    K = (I[end]+1):n
    A[I[1]:n, J] = A[panel_p, J]

    ## Compute blocks of U
    L = tril(A[I,I],-1) + eye(length(I))
    A[I, J] = L \ A[I, J]

    ## Trailing submatrix update
    if !isempty(K)
        A[K,J] = A[K,J] - A[K,I]*A[I,J]
    end

    return true

end ## trailing_update_seq()

# This version is written for a shared memory implementation.
# The matrix A is local to the first Worker, which allocates work to other Workers
# All updates to A are carried out by the first Worker. Thus A is not distributed

hpl_par(A::Matrix, b::Vector) = hpl_par(A, b, max(1, div(max(size(A)),4)), true)

hpl_par(A::Matrix, b::Vector, bsize::Integer) = hpl_par(A, b, bsize, true)

function hpl_par(A::Matrix, b::Vector, blocksize::Integer, run_parallel::Bool)

    n = size(A,1)
    A = [A b]

    if blocksize < 1
       throw(ArgumentError("hpl_par: invalid blocksize: $blocksize < 1"))
    end

    B_rows = linspace(0, n, div(n,blocksize)+1)
    B_rows[end] = n
    B_cols = [B_rows, [n+1]]
    nB = length(B_rows)
    depend = cell(nB, nB)

    ## Small matrix case
    if nB <= 1
        x = A[1:n, 1:n] \ A[:,n+1]
        return x
    end

    ## Add a ghost row of dependencies to boostrap the computation
    for j=1:nB; depend[1,j] = true; end
    for i=2:nB, j=1:nB; depend[i,j] = false; end

    for i=1:(nB-1)
        #println("A=$A") #####
        ## Threads for panel factorizations
        I = (B_rows[i]+1):B_rows[i+1]
        K = I[1]:n
        (A_KI, panel_p) = panel_factor_par(A[K,I], depend[i,i])

        ## Write the factorized panel back to A
        A[K,I] = A_KI

        ## Panel permutation
        panel_p = K[panel_p]
        depend[i+1,i] = true

        ## Apply permutation from pivoting
        J = (B_cols[i+1]+1):B_cols[nB+1]
        A[K, J] = A[panel_p, J]
        ## Threads for trailing updates
        #L_II = tril(A[I,I], -1) + eye(length(I))
        L_II = tril(sub(A,I,I), -1) + eye(length(I))
        K = (I[length(I)]+1):n
        A_KI = A[K,I]

        for j=(i+1):nB
            J = (B_cols[j]+1):B_cols[j+1]

            ## Do the trailing update (Compute U, and DGEMM - all flops are here)
            if run_parallel
                A_IJ = A[I,J]
                #A_KI = A[K,I]
                A_KJ = A[K,J]
                depend[i+1,j] = @spawn trailing_update_par(L_II, A_IJ, A_KI, A_KJ, depend[i+1,i], depend[i,j])
            else
                depend[i+1,j] = trailing_update_par(L_II, A[I,J], A[K,I], A[K,J], depend[i+1,i], depend[i,j])
            end
        end

        # Wait for all trailing updates to complete, and write back to A
        for j=(i+1):nB
            J = (B_cols[j]+1):B_cols[j+1]
            if run_parallel
                (A_IJ, A_KJ) = fetch(depend[i+1,j])
            else
                (A_IJ, A_KJ) = depend[i+1,j]
            end
            A[I,J] = A_IJ
            A[K,J] = A_KJ
            depend[i+1,j] = true
        end

    end

    ## Completion of the last diagonal block signals termination
    @assert depend[nB, nB]

    ## Solve the triangular system
    x = triu(A[1:n,1:n]) \ A[:,n+1]

    return x

end ## hpl()


### Panel factorization ###

function panel_factor_par(A_KI, col_dep)

    @assert col_dep

    ## Factorize a panel
    panel_p = lufact!(A_KI)[:p] # Economy mode

    return (A_KI, panel_p)

end ## panel_factor_par()


### Trailing update ###

function trailing_update_par(L_II, A_IJ, A_KI, A_KJ, row_dep, col_dep)

    @assert row_dep
    @assert col_dep

    ## Compute blocks of U
    A_IJ = L_II \ A_IJ

    ## Trailing submatrix update - All flops are here
    if !isempty(A_KJ)
        m, k = size(A_KI)
        n = size(A_IJ,2)
        blas_gemm('N','N',m,n,k,-1.0,A_KI,m,A_IJ,k,1.0,A_KJ,m)
        #A_KJ = A_KJ - A_KI*A_IJ
    end

    return (A_IJ, A_KJ)

end ## trailing_update_par()


### using DArrays ###

function hpl_par2(A::Matrix, b::Vector)
    n = size(A,1)
    A = [A b]

    C = distribute(A, 2)
    nB = length(C.pmap)

    ## case if only one processor
    if nB <= 1
        x = A[1:n, 1:n] \ A[:,n+1]
        return x
    end

    depend = Array(RemoteRef, nB, nB)

    #pmap[i] is where block i's stuff is
    #block i is dist[i] to dist[i+1]-1
    for i = 1:nB
        #println("C=$(convert(Array, C))") #####
        ##panel factorization
        panel_p = remotecall_fetch(C.pmap[i], panel_factor_par2, C, i, n)

        ## Apply permutation from pivoting
        for j = (i+1):nB
           depend[i,j] = remotecall(C.pmap[j], permute, C, i, j, panel_p, n, false)
        end
        ## Special case for last column
        if i == nB
           depend[nB,nB] = remotecall(C.pmap[nB], permute, C, i, nB+1, panel_p, n, true)
        end

        ##Trailing updates
        (i == nB) ? (I = (C.dist[i]):n) :
                    (I = (C.dist[i]):(C.dist[i+1]-1))
        C_II = C[I,I]
        L_II = tril(C_II, -1) + eye(length(I))
        K = (I[length(I)]+1):n
        if length(K) > 0
            C_KI = C[K,I]
        else
            C_KI = zeros(0)
        end

        for j=(i+1):nB
            dep = depend[i,j]
            depend[j,i] = remotecall(C.pmap[j], trailing_update_par2, C, L_II, C_KI, i, j, n, false, dep)
        end

        ## Special case for last column
        if i == nB
            dep = depend[nB,nB]
            remotecall_fetch(C.pmap[nB], trailing_update_par2, C, L_II, C_KI, i, nB+1, n, true, dep)
        else
            #enforce dependencies for nonspecial case
            for j=(i+1):nB
                wait(depend[j,i])
            end
        end
    end

    A = convert(Array, C)
    x = triu(A[1:n,1:n]) \ A[:,n+1]
end ## hpl_par2()

function panel_factor_par2(C, i, n)
    (C.dist[i+1] == n+2) ? (I = (C.dist[i]):n) :
                           (I = (C.dist[i]):(C.dist[i+1]-1))
    K = I[1]:n
    C_KI = C[K,I]
    #(C_KI, panel_p) = lu!(C_KI) #economy mode
    panel_p = lu!(C_KI)[2]
    C[K,I] = C_KI

    return panel_p
end ##panel_factor_par2()

function permute(C, i, j, panel_p, n, flag)
    if flag
        K = (C.dist[i]):n
        J = (n+1):(n+1)
        C_KJ = C[K,J]

        C_KJ = C_KJ[panel_p,:]
        C[K,J] = C_KJ
    else
        K = (C.dist[i]):n
        J = (C.dist[j]):(C.dist[j+1]-1)
        C_KJ = C[K,J]

        C_KJ = C_KJ[panel_p,:]
        C[K,J] = C_KJ
    end
end ##permute()

function trailing_update_par2(C, L_II, C_KI, i, j, n, flag, dep)
    if isa(dep, RemoteRef); wait(dep); end
    if flag
        #(C.dist[i+1] == n+2) ? (I = (C.dist[i]):n) :
        #                       (I = (C.dist[i]):(C.dist[i+1]-1))
        I = C.dist[i]:n
        J = (n+1):(n+1)
        K = (I[length(I)]+1):n
        C_IJ = C[I,J]
        if length(K) > 0
            C_KJ = C[K,J]
        else
            C_KJ = zeros(0)
        end
        ## Compute blocks of U
        C_IJ = L_II \ C_IJ
        C[I,J] = C_IJ
    else
        #(C.dist[i+1] == n+2) ? (I = (C.dist[i]):n) :
        #                       (I = (C.dist[i]):(C.dist[i+1]-1))

        I = (C.dist[i]):(C.dist[i+1]-1)
        J = (C.dist[j]):(C.dist[j+1]-1)
        K = (I[length(I)]+1):n
        C_IJ = C[I,J]
        if length(K) > 0
            C_KJ = C[K,J]
        else
            C_KJ = zeros(0)
        end

        ## Compute blocks of U
        C_IJ = L_II \ C_IJ
        C[I,J] = C_IJ
        ## Trailing submatrix update - All flops are here
        if !isempty(C_KJ)
            cm, ck = size(C_KI)
            cn = size(C_IJ,2)
            blas_gemm('N','N',cm,cn,ck,-1.0,C_KI,cm,C_IJ,ck,1.0,C_KJ,cm)
            #C_KJ = C_KJ - C_KI*C_IJ
            C[K,J] = C_KJ
        end
    end
end ## trailing_update_par2()

## Test n*n matrix on np processors
## Prints 5 numbers that should be close to zero
function test(n, np)
    A = rand(n,n); b = rand(n);
    X = (@elapsed x = A \ b);
    Y = (@elapsed y = hpl_par(A,b, max(1,div(n,np))));
    Z = (@elapsed z = hpl_par2(A,b));
    for i=1:(min(5,n))
        print(z[i]-y[i], " ")
    end
    println()
    return (X,Y,Z)
end

## test k times and collect average
function test(n,np,k)
    sum1 = 0; sum2 = 0; sum3 = 0;
    for i = 1:k
        (X,Y,Z) = test(n,np)
        sum1 += X
        sum2 += Y
        sum3 += Z
    end
    return (sum1/k, sum2/k, sum3/k)
end
