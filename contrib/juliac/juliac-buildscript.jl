# This file is a part of Julia. License is MIT: https://julialang.org/license

# Script to run in the process that generates juliac's object file output

# Run the verifier in the current world (before modifications), so that error
# messages and types print in their usual way.
Core.Compiler._verify_trim_world_age[] = Base.get_world_counter()

# Initialize some things not usually initialized when output is requested
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
if Base.get_bool_env("JULIA_USE_FLISP_PARSER", false) === false
    Base.JuliaSyntax.enable_in_core!()
end

if Base.JLOptions().trim != 0
    include(joinpath(@__DIR__, "juliac-trim-base.jl"))
end

const C_friendly_types = Union{    # a few of these are redundant to make it easier to maintain
    Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Float32, Float64, Bool,
    Cvoid, Cint, Cshort, Clong, Cuint, Cushort, Culong, Cssize_t, Csize_t,
    Cchar, Cwchar_t, Cstring, Cwstring,
    RawFD,
}

function is_c_friendly(@nospecialize(T::DataType))
    T <: Ptr && return is_c_friendly(T.parameters[1])
    return T <: C_friendly_types
end

function recursively_add_types!(types::Base.IdSet{DataType}, @nospecialize(T::DataType))
    if !is_c_friendly(T)
        T.name.module === Core && error("invalid type for juliac: ", T) # exclude internals (they may change)
        push!(types, T)
    end
    for list in (T.parameters, fieldtypes(T))
        for S in list
            recursively_add_types!(types, S)
        end
    end
end

function mangle_name(@nospecialize(T::DataType))
    is_c_friendly(T) && return string(T)
    pname = isempty(T.parameters) ? String(nameof(T)) :
                                    join(pushfirst!(map(mangle_name, T.parameters), String(nameof(T)), "_"))
    return "_" * pname * "_"
end

# Load user code

import Base.Experimental.entrypoint

# for use as C main if needed
function _main(argc::Cint, argv::Ptr{Ptr{Cchar}})::Cint
    args = ccall(:jl_set_ARGS, Any, (Cint, Ptr{Ptr{Cchar}}), argc, argv)::Vector{String}
    return Main.main(args)
end

let mod = Base.include(Main, ARGS[1])
    Core.@latestworld
    if ARGS[2] == "--output-exe"
        have_cmain = false
        if isdefined(Main, :main)
            for m in methods(Main.main)
                if isdefined(m, :ccallable)
                    # TODO: possibly check signature and return type
                    have_cmain = true
                    break
                end
            end
        end
        if !have_cmain
            if Base.should_use_main_entrypoint()
                if hasmethod(Main.main, Tuple{Vector{String}})
                    entrypoint(_main, (Cint, Ptr{Ptr{Cchar}}))
                    Base._ccallable("main", Cint, Tuple{typeof(_main), Cint, Ptr{Ptr{Cchar}}})
                else
                    error("`@main` must accept a `Vector{String}` argument.")
                end
            else
                error("To generate an executable a `@main` function must be defined.")
            end
        end
    end
    #entrypoint(join, (Base.GenericIOBuffer{Memory{UInt8}}, Array{Base.SubString{String}, 1}, String))
    #entrypoint(join, (Base.GenericIOBuffer{Memory{UInt8}}, Array{String, 1}, Char))
    entrypoint(Base.task_done_hook, (Task,))
    entrypoint(Base.wait, ())
    entrypoint(Base.wait_forever, ())
    entrypoint(Base.trypoptask, (Base.StickyWorkqueue,))
    entrypoint(Base.checktaskempty, ())
    if ARGS[3] == "true"
        ccall(:jl_add_ccallable_entrypoints, Cvoid, ())
    end

    # Export info about entrypoints and structs needed to create header files
    if length(ARGS) >= 4
        logfile = ARGS[4]
        open(logfile, "w") do io
            types = Base.IdSet{DataType}()
            Base.visit(Core.GlobalMethods) do method
                if isdefined(method, :ccallable)
                    rt, sig = method.ccallable
                    name = length(method.ccallable) > 2 ? Symbol(method.ccallable[3]) : method.name
                    Base.show_tuple_as_call(io, name, sig)
                    println(io, "::", rt)
                    for T in sig.parameters[2:end]
                        recursively_add_types!(types, T)
                    end
                end
            end
            println(io)
            for T in types
                println(io, mangle_name(T))
                dtfd = Base.DataTypeFieldDesc(T)
                local fd
                for i = 1:Base.datatype_nfields(T)
                    fd = dtfd[i]
                    fn = fieldname(T, i)
                    ft = fieldtype(T, i)
                    println(io, "  ", fn, "::", mangle_name(ft), "[", fd.offset, "]")
                end
                println(io, fd.offset + fd.size, " bytes")
            end
        end
    end
end

if Base.JLOptions().trim != 0
    include(joinpath(@__DIR__, "juliac-trim-stdlib.jl"))
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
