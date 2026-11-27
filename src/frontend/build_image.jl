# This file is a part of Julia. License is MIT: https://julialang.org/license

# Script run in the process that generates the image object of
# libjulia-frontend (`julia --output-o ... build_image.jl <script>`), where
# `julia` is the release that the frontend is built with (see the Makefile).
# This is the part of JuliaC.jl's build script that a library with
# `@ccallable` entry points and no trimming needs: initialize what output
# mode does not, load the script, compile its `@ccallable`s, and reset the
# session state that must not be saved into the image.

# Initialize some things not usually initialized when output is requested
Sys.__init__()
Base.reinit_stdio()
Base.init_depot_path()
Base.init_load_path()
Base.init_active_project()
Base.JuliaSyntax.enable_in_core!()
let task = current_task()
    task.rngState0 = 0x5156087469e170ab
    task.rngState1 = 0x7431eaead385992c
    task.rngState2 = 0x503e1d32781c2608
    task.rngState3 = 0x3a77f7189200c20b
    task.rngState4 = 0x5502376d099035ae
end
ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__,
      (UInt64(0), UInt64(0)))

Base.include(Main, ARGS[1])
Core.@latestworld

if isdefined(Base.Compiler, :add_ccallable_entrypoints!)
    Base.Compiler.add_ccallable_entrypoints!()
else
    ccall(:jl_add_ccallable_entrypoints, Cvoid, ())
end

empty!(Core.ARGS)
empty!(Base.ARGS)
empty!(LOAD_PATH)
empty!(DEPOT_PATH)
empty!(Base.TOML_CACHE.d)
Base.TOML.reinit!(Base.TOML_CACHE.p, "")
Base.ACTIVE_PROJECT[] = nothing
@eval Base begin
    PROGRAM_FILE = ""
end
@eval Sys begin
    BINDIR = ""
    STDLIB = ""
end
