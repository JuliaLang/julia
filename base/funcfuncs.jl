# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    ^(f::Function, n::Integer)

Compose `f` with itself `n` times. `f^0` evaluates to `identity`

# Examples
```jldoctest
julia> map(f -> f(Int), supertype.^(0:5))
6-element Vector{DataType}:
 Int64
 Signed
 Integer
 Real
 Number
 Any
```
"""
^(f::Function, n::Integer) = n == 0 ? identity : foldl((s,_) -> f ∘ s, 1:n-1; init=f)
