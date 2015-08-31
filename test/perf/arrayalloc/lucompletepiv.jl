# This file is a part of Julia. License is MIT: http://julialang.org/license

function lucompletepivCopy!(A)
    n = size(A, 1)
    rowpiv=zeros(Int, n-1)
    colpiv=zeros(Int, n-1)
    for k = 1:n-1
        As = abs(A[k:n, k:n])
        μ, λ = ind2sub(size(As), indmax(As))
        μ += k-1; λ += k-1
        rowpiv[k] = μ
        A[[k,μ], 1:n] = A[[μ,k], 1:n]
        colpiv[k] = λ
        A[1:n, [k,λ]] = A[1:n, [λ,k]]
        if A[k,k] ≠ 0
            ρ = k+1:n
            A[ρ, k] = A[ρ, k]/A[k, k]
            A[ρ, ρ] = A[ρ, ρ] - A[ρ, k] * A[k, ρ]
        end
    end
    return (A, rowpiv, colpiv)
end

function lucompletepivSub!(A)
    n = size(A, 1)
    rowpiv=zeros(Int, n-1)
    colpiv=zeros(Int, n-1)
    for k = 1:n-1
        As = abs(sub(A, k:n, k:n))
        μ, λ = ind2sub(size(As), indmax(As))
        μ += k-1; λ += k-1
        rowpiv[k] = μ
        A[[k,μ], 1:n] = sub(A, [μ,k], 1:n)
        colpiv[k] = λ
        A[1:n, [k,λ]] = sub(A, 1:n, [λ,k])
        if A[k,k] ≠ 0
            ρ = k+1:n
            A[ρ, k] = sub(A, ρ, k)/A[k, k]
            A[ρ, ρ] = sub(A, ρ, ρ) - sub(A, ρ, k) * sub(A, k, ρ)
        end
    end
    return (A, rowpiv, colpiv)
end

println("Julia version with copy slices")
for n = [100, 250, 500, 1000]
    A = randn(n,n)
    @printf("size %4d matrix factorized in %6.3f seconds\n", n, mean([@elapsed lucompletepivCopy!(copy(A)) for i = 1:3]))
end

println("\nJulia version with view slices")
for n = [100, 250, 500, 1000]
    A = randn(n,n)
    @printf("size %4d matrix factorized in %6.3f seconds\n", n, mean([@elapsed lucompletepivSub!(copy(A)) for i = 1:3]))
end
