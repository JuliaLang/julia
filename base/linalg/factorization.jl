# This file is a part of Julia. License is MIT: https://julialang.org/license

## Matrix factorizations and decompositions

abstract type Factorization{T} end

eltype(::Type{Factorization{T}}) where {T} = T
transpose(F::Factorization) = error("transpose not implemented for $(typeof(F))")
adjoint(F::Factorization) = error("adjoint not implemented for $(typeof(F))")

macro assertposdef(A, info)
   :($(esc(info)) == 0 ? $(esc(A)) : throw(PosDefException($(esc(info)))))
end

macro assertnonsingular(A, info)
   :($(esc(info)) == 0 ? $(esc(A)) : throw(SingularException($(esc(info)))))
end

"""
    issuccess(F::Factorization)

Test that a factorization of a matrix succeeded.

```jldoctest
julia> F = cholfact([1 0; 0 1]);

julia> LinAlg.issuccess(F)
true

julia> F = lufact([1 0; 0 0]);

julia> LinAlg.issuccess(F)
false
```
"""
issuccess(F::Factorization)

function logdet(F::Factorization)
    d, s = logabsdet(F)
    return d + log(s)
end

function det(F::Factorization)
    d, s = logabsdet(F)
    return exp(d)*s
end

### General promotion rules
convert(::Type{Factorization{T}}, F::Factorization{T}) where {T} = F
inv(F::Factorization{T}) where {T} = A_ldiv_B!(F, eye(T, size(F,1)))

Base.hash(F::Factorization, h::UInt) = mapreduce(f -> hash(getfield(F, f)), hash, h, 1:nfields(F))
Base.:(==)(  F::T, G::T) where {T<:Factorization} = all(f -> getfield(F, f) == getfield(G, f), 1:nfields(F))
Base.isequal(F::T, G::T) where {T<:Factorization} = all(f -> isequal(getfield(F, f), getfield(G, f)), 1:nfields(F))

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
function (\)(F::Factorization{T}, B::VecOrMat{Complex{T}}) where T<:BlasReal
    c2r = reshape(transpose(reinterpret(T, reshape(B, (1, length(B))))), size(B, 1), 2*size(B, 2))
    x = A_ldiv_B!(F, c2r)
    return reshape(collect(reinterpret(Complex{T}, transpose(reshape(x, div(length(x), 2), 2)))), _ret_size(F, B))
end

for (f1, f2) in ((:\, :A_ldiv_B!),
                 (:Ac_ldiv_B, :Ac_ldiv_B!))
    @eval begin
        function $f1(F::Factorization, B::AbstractVecOrMat)
            TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
            BB = similar(B, TFB, size(B))
            copy!(BB, B)
            $f2(F, BB)
        end
    end
end

# support the same 3-arg idiom as in our other in-place A_*_B functions:
for f in (:A_ldiv_B!, :Ac_ldiv_B!, :At_ldiv_B!)
    @eval $f(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat) =
        $f(A, copy!(Y, B))
end

# fallback methods for transposed solves
At_ldiv_B(F::Factorization{<:Real}, B::AbstractVecOrMat) = Ac_ldiv_B(F, B)
At_ldiv_B(F::Factorization, B) = conj.(Ac_ldiv_B(F, conj.(B)))

"""
    A_ldiv_B!([Y,] A, B) -> Y

Compute `A \\ B` in-place and store the result in `Y`, returning the result.
If only two arguments are passed, then `A_ldiv_B!(A, B)` overwrites `B` with
the result.

The argument `A` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholfact`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lufact!`](@ref)),
and performance-critical situations requiring `A_ldiv_B!` usually also require fine-grained
control over the factorization of `A`.
"""
A_ldiv_B!

"""
    Ac_ldiv_B!([Y,] A, B) -> Y

Similar to [`A_ldiv_B!`](@ref), but return ``Aᴴ`` \\ ``B``,
computing the result in-place in `Y` (or overwriting `B` if `Y` is not supplied).
"""
Ac_ldiv_B!

"""
    At_ldiv_B!([Y,] A, B) -> Y

Similar to [`A_ldiv_B!`](@ref), but return ``Aᵀ`` \\ ``B``,
computing the result in-place in `Y` (or overwriting `B` if `Y` is not supplied).
"""
At_ldiv_B!
