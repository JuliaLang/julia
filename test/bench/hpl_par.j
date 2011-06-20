## Based on "Multi-Threading and One-Sided Communication in Parallel LU Factorization"
## http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.138.4361&rank=7

# This version is written for a shared memory implementation.
# The matrix A is local to the first Worker, which allocates work to other Workers
# All updates to A are carried out by the first Worker. Thus A is not distributed

function hpl_par(A::Matrix, b::Vector)

    blocksize = 5

    n = size(A,1)
    A = [A b]
    
    B_rows = linspace(0, n, blocksize)
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
        ## Threads for panel factorizations
        I = (B_rows[i]+1):B_rows[i+1]
        K = I[1]:n
        (A_KI, panel_p) = panel_factor(A[K,I], depend[i,i])

        ## Write the factorized panel back to A
        A[K,I] = A_KI

        ## Panel permutation 
        panel_p = K[panel_p]
        depend[i+1,i] = true

        ## Apply permutation from pivoting
        J = (B_cols[i+1]+1):B_cols[nB+1]
        A[K, J] = A[panel_p, J]

        ## Threads for trailing updates
        L_II = tril(A[I,I], -1) + eye(length(I))
        K = (I[length(I)]+1):n

        for j=(i+1):nB
            J = (B_cols[j]+1):B_cols[j+1]
            
            ## Do the trailing update (Compute U, and DGEMM - all flops are here)
            depend[i+1,j] = @spawn trailing_update(L_II, A[I,J], A[K,I], A[K,J],
                                                   depend[i+1,i], depend[i,j], J)
        end

        for j=(i+1):nB
            (A_IJ, A_KJ, J) = fetch(depend[i+1,j])
            ## Write updates back to A
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

function panel_factor(A_KI, col_dep)

    @assert col_dep
    
    ## Factorize a panel
    (A_KI, panel_p) = lu(A_KI, true) # Economy mode
    
    return (A_KI, panel_p)
    
end ## panel_factor()


### Trailing update ###

function trailing_update(L_II, A_IJ, A_KI, A_KJ, row_dep, col_dep, J)
    
    @assert row_dep
    @assert col_dep

    ## Compute blocks of U 
    A_IJ = L_II \ A_IJ
    
    ## Trailing submatrix update - All flops are here
    if !isempty(A_KJ)
        A_KJ = A_KJ - A_KI*A_IJ
    end
    
    return (A_IJ, A_KJ, J)
    
end ## trailing_update()
