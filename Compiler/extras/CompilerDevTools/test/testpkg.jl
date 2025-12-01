using Pkg

Pkg.activate(dirname(@__DIR__)) do
  include("runtests.jl")
end
