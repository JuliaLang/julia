# This file is a part of Julia. License is MIT: https://julialang.org/license

# Build an application image: a system image that holds this program on top of
# the one this Julia runs. The five lines before the `require` are what an
# application needs of its environment.
Base.reinit_stdio()
@eval Sys BINDIR = ccall(:jl_get_julia_bindir, Any, ())::String
@eval Sys STDLIB = $(abspath(Sys.BINDIR, "../share/julia/stdlib", string('v', VERSION.major, '.', VERSION.minor)))
copy!(LOAD_PATH, [ARGS[1], "@stdlib"])
Base.init_depot_path()
let mod = Base.require(Base.PkgId(Base.UUID("b0f2e1a4-9d3c-4f6b-8a17-2c5d0e9b7431"), "Prelinked"))
    # The launcher looks the program up in `Main`, so bind it there.
    Core.eval(Main, Expr(:const, Expr(:(=), :Prelinked, mod)))
end
