# This file is a part of Julia. License is MIT: https://julialang.org/license

issorted(r::AbstractUnitRange) = true
issorted(r::AbstractRange) = length(r) <= 1 || step(r) >= zero(step(r))

sort(r::AbstractUnitRange) = r
sort!(r::AbstractUnitRange) = r

sort(r::AbstractRange) = issorted(r) ? r : reverse(r)

sortperm(r::AbstractUnitRange) = 1:length(r)
sortperm(r::AbstractRange) = issorted(r) ? (1:1:length(r)) : (length(r):-1:1)
