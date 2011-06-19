## Based on "Multi-Threading and One-Sided Communication in Parallel LU Factorization"
## http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.138.4361&rank=7

function hpl (A::Matrix, b::Vector)

    blocksize = 5

    n = size(A,1)
    A = [A b]
    
    B_rows = linspace(0, n, blocksize)
    B_rows[end] = n 
    B_cols = [B_rows, [n+1]]
    nB = length(B_rows)
    #depend = zeros(Bool, nB, nB) # In parallel, depend needs to be able to hold futures
    depend = Array(RemoteRef, nB, nB)

    ## Add a ghost row of dependencies to boostrap the computation
    for j=1:nB; depend[1,j] = true; end
    
    for i=1:(nB-1)
        ## Threads for panel factorizations
        K = (B_rows[i]+1):n
        I = (B_rows[i]+1):B_rows[i+1]
        #(depend[i+1,i], panel_p) = panel_factor(A, I, depend[i,i])        
        rref_pf = @spawn panel_factor(A[K,I], depend[i,i]) 
        depend[i+1,i] = @spawnlocal spawn_trailing_updates ()
    end
    
    ## Completion of the last diagonal block signals termination
    #wait(depend[nB, nB])
    
    ## Solve the triangular system
    x = triu(A[1:n,1:n]) \ A[:,n+1]
    
    return x

end ## hpl()


### Panel factorization ###

function panel_factor(A, col_dep)

    ## Enforce dependencies
    wait(col_dep)

    ## Factorize a panel
    panel_p = lu(A, true) # Economy mode. A is overwritten with factors.
    
    return (A, panel_p)
    
end ## panel_factor()

### spawn trailing updates ###

function spawn_trailing_updates(A, depend, rref_pf)

    (A[K,I], p) = fetch(rref_pf)
    panel_p = K[p]
    
    for j=(i+1):nB
        J = (B_cols[j]+1):B_cols[j+1]
        #depend[i+1,j] = trailing_update(A, I, J, panel_p, depend[i+1,i],depend[i,j])
        rref_tu = @spawn trailing_update(A, I, J, panel_p, depend[i+1,i],depend[i,j])
        depend[i+1,j] = @spawnlocal (A[I,J], A[K,J]) = fetch(rref_tu)
    end

end

### One block column trailing update ###

function trailing_update(A_II, A_IJ, A_KI, A_IJ, panel_p, row_dep, col_dep)

    ## Enforce dependencies
    wait(row_dep)
    wait(col_dep)
    
    ## Apply permutation from pivoting 
    K = (I[end]+1):n
    A[I[1]:n, J] = A[panel_p, J]
    
    ## Apply pivot sequence to Compute blocks of U 
    L = tril(A_II, -1) + eye(length(I))
    A_IJ = L \ A_IJ 
    
    ## Trailing submatrix update
    if !isempty(K)
        A_KJ = A_KJ - A_KI*A_IJ  
    end
    
    return (A_IJ, A_KJ)
    
end ## trailing_update()
