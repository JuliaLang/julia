addqueen(queens::Array{Vector{Int}}, queen::Vector{Int}) = push(copy(queens), queen)

hitsany(queen::Vector{Int}, queens::Array{Vector{Int}}) = any(map((x) -> hits(queen, x), queens))
hits(a::Array{Int}, b::Array{Int}) = any(a .== b) || abs(a-b)[1] == abs(a-b)[2]

function solve(x::Int, y::Int, n::Int, d::Array{Vector{Int}})
  if n == 0
    return d
  end
  for px = 1:x
    for py = 1:y
      if !hitsany([px, py], d)
        s = solve(x, y, n-1, addqueen(d, [px, py]))
        if !isequal(s, None)
          return s
        end
      end
    end
  end
  return None
end

solve(x, y, n) = solve(x, y, n, Array(Vector{Int}, 0))

for i = 1:8
  print("Solve for $i\n")
  print(solve(8, 8, i))
  print("\n")
end
