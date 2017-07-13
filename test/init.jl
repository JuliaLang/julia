# Initialize the default packages
@initialize(false)

# @test isdir(Base._pkg_home)

# Interally we could load Pkg from the pkg home path, but `using` will also work.
# Pkg = include(joinpath(Base._pkg_home(), "src", "Pkg.jl"))

# Ideally we should be able to use the ./usr/share/julia/site/v0.7/Pkg.jl to
# install and test a different version of Pkg
# Pkg.add("Pkg")
# Pkg.test("Pkg")

# From here we could run Pkg.test(...) for other parts of Base.
