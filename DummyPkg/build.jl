# julia --startup-file=no --output-o sys.o -J"usr/lib/julia/sys.so" DummyPkg/build.jl
Base.reinit_stdio()
Base.init_depot_path()
Base.init_load_path()

push!(LOAD_PATH, abspath("DummyPkg"))

using DummyPkg

empty!(LOAD_PATH)
empty!(DEPOT_PATH)