@pkginit(false)

@test isdir(Base._pkg_home)
Pkg = include(joinpath(Base._pkg_home(), "src", "Pkg.jl"))

Pkg.add("Pkg")
Pkg.test("Pkg")

# From here we could run Pkg.test(...) for other parts of Base.
