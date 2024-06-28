# Script to run in the process that generates juliac's object file output

inputfile = ARGS[1]
output_type = ARGS[2]
add_ccallables = ARGS[3] == "true"

# Initialize some things not usually initialized when output is request
Sys.__init__()
Base.init_depot_path()
Base.init_load_path()
Base.init_active_project()
task = current_task()
task.rngState0 = 0x5156087469e170ab
task.rngState1 = 0x7431eaead385992c
task.rngState2 = 0x503e1d32781c2608
task.rngState3 = 0x3a77f7189200c20b
task.rngState4 = 0x5502376d099035ae
uuid_tuple = (UInt64(0), UInt64(0))
ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__, uuid_tuple)
ccall(:jl_set_newly_inferred, Cvoid, (Any,), Core.Compiler.newly_inferred)

include("patches.jl")

# Patch methods in Core and Base
for (mod, patch) in base_patches
    Core.eval(mod, patch)
end

# Load user code

import Base.Experimental.entrypoint

let mod = Base.include(Base.__toplevel__, inputfile)
    if !isa(mod, Module)
        mod = Main
    end
    if output_type == "--output-exe" && isdefined(mod, :main) && !add_ccallables
        entrypoint(mod.main, ())
    end
    #entrypoint(join, (Base.GenericIOBuffer{Memory{UInt8}}, Array{Base.SubString{String}, 1}, String))
    #entrypoint(join, (Base.GenericIOBuffer{Memory{UInt8}}, Array{String, 1}, Char))
    if add_ccallables
        ccall(:jl_add_ccallable_entrypoints, Cvoid, ())
    end
end

# Additional method patches depending on whether user code loads certain stdlibs
# TODO find a better way to do this
let loaded = Base.loaded_modules_array(),
    loaded_names = Set(Symbol.(loaded))
    for (pkgname, patch) in extra_patches
        if pkgname in loaded_names
            mod = loaded[findfirst(m::Module->nameof(m)===pkgname, loaded)::Int]
            Core.eval(mod, patch)
        end
    end
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
