using Pkg

Pkg.activate(dirname(@__DIR__)) do
  Pkg.instantiate()
  include("runtests.jl")
end
