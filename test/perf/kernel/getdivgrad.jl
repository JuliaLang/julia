# This file is a part of Julia. License is MIT: http://julialang.org/license

# https://github.com/JuliaLang/julia/issues/4707

#----------------- Get the A matrix
function getDivGrad(n1,n2,n3)
    # the Divergence
    D1 = kron(speye(n3),kron(speye(n2),ddx(n1)))
    D2 = kron(speye(n3),kron(ddx(n2),speye(n1)))
    D3 = kron(ddx(n3),kron(speye(n2),speye(n1)))
    # DIV from faces to cell-centers
    Div = [D1 D2 D3]

    return Div*Div'
end

#----------------- 1D finite difference on staggered grid
function ddx(n)
# generate 1D derivatives
    return d = spdiags(ones(n)*[-1 1],[0,1],n,n+1)
end

#------------- Build a diagonal matrix
function spdiags(B,d,m,n)
#   spdiags(B,d,m,n)
# creates a sparse matrix from its diagonals
    d = d[:]
    p = length(d)

    len = zeros(Int, p+1, 1)
    for k = 1:p
        len[k+1] = len[k] + length(max(1,1-d[k]):min(m,n-d[k]))
    end
    a = zeros(Int, len[p+1], 3)
    for k = 1:p
        # Append new d[k]-th diagonal to compact form
        i = max(1,1-d[k]):min(m,n-d[k])
        a[(len[k]+1):len[k+1],:] = [i i+d[k] B[i+(m>=n)*d[k],k]]
    end

    A = sparse(a[:,1],a[:,2],a[:,3],m,n)

    return A
end
