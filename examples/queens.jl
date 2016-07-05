# This file is a part of Julia. License is MIT: http://julialang.org/license

addqueen(queens::Array{Vector{Int}}, queen::Vector{Int}) = push!(copy(queens), queen)

hitsany(queen::Vector{Int}, queens::Array{Vector{Int}}) = any(x->hits(queen, x), queens)
hits(a::Array{Int}, b::Array{Int}) = any(a .== b) || abs.(a-b)[1] == abs.(a-b)[2]

function solve(x, y, n, d=Array{Vector{Int}}(0))
  if n == 0
    return d
  end
  for px = 1:x
    for py = 1:y
      if !hitsany([px, py], d)
        s = solve(x, y, n-1, addqueen(d, [px, py]))
        s !== nothing && return s
      end
    end
  end
  return nothing
end
