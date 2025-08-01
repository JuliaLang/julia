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

const C_friendly_types = Base.IdSet{Any}([    # a few of these are redundant to make it easier to maintain
    Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Float32, Float64, Bool,
    Cvoid, Cint, Cshort, Clong, Cuint, Cushort, Culong, Cssize_t, Csize_t,
    Cchar, Cwchar_t, Cstring, Cwstring,
    RawFD,
])

function recursively_add_types!(types::Base.IdSet{DataType}, @nospecialize(T::DataType))
    T in types && return types
    while T.name === Ptr.body.name
        push!(types, T)
        T = T.parameters[1] # unwrap Ptr{...}
        T in types && return types
    end
    if T.name.module === Core && T ∉ C_friendly_types
        error("invalid type for juliac: ", T) # exclude internals (they may change)
    end
    push!(types, T)
    for list in (T.parameters, fieldtypes(T))
        for S in list
            recursively_add_types!(types, S)
        end
    end
    return types
end

struct TypeEmitter
    io::IO
    type_ids::IdDict{Any,Int}
end

function escape_string_json(s::AbstractString)
    iob = IOBuffer()
    print(iob, '"')
    for c in s
        if c == '"'
            print(iob, "\\\"")
        elseif c == '\\'
            print(iob, "\\\\")
        elseif c == '\b'
            print(iob, "\\b")
        elseif c == '\f'
            print(iob, "\\f")
        elseif c == '\n'
            print(iob, "\\n")
        elseif c == '\r'
            print(iob, "\\r")
        elseif c == '\t'
            print(iob, "\\t")
        elseif '\x00' <= c <= '\x1f'
            print(iob, "\\u", lpad(string(UInt16(c), base=16), 4, '0'))
        else
            @assert isvalid(c) "invalid unicode character"
            print(iob, c)
        end
    end
    print(iob, '"')
    return String(take!(iob))
end

function type_name_json(@nospecialize(dt::DataType))
    return escape_string_json(repr(dt))
end

function field_name_json(@nospecialize(dt::DataType), field::Int)
    name = String(fieldname(dt, field))
    return escape_string_json(name)
end

function emit_pointer_info!(ctx::TypeEmitter, @nospecialize(dt::DataType); indent::Int = 0)
    pointee_type_id = ctx.type_ids[dt.parameters[1]]
    let indented_println(args...) = println(ctx.io, " " ^ indent, args...)
        indented_println("{")
        indented_println("  \"id\": ", ctx.type_ids[dt], ",")
        indented_println("  \"kind\": \"pointer\",")
        indented_println("  \"name\": ", type_name_json(dt), ",")
        indented_println("  \"pointee\": ", pointee_type_id)
        print(ctx.io, " " ^ indent, "}")
    end
end

function emit_field_info!(ctx::TypeEmitter, @nospecialize(dt::DataType), field::Int; indent::Int = 0)
    desc = Base.DataTypeFieldDesc(dt)[field]
    type_id = ctx.type_ids[fieldtype(dt, field)]
    let indented_println(args...) = println(ctx.io, " " ^ indent, args...)
        indented_println("{")
        indented_println("  \"name\": ", field_name_json(dt, field), ",")
        indented_println("  \"offset\": ", desc.offset, ",")
        indented_println("  \"type\": ", type_id)
        print(ctx.io, " " ^ indent, "}")
    end
end

function emit_struct_info!(ctx::TypeEmitter, @nospecialize(dt::DataType); indent::Int = 0)
    type_id = ctx.type_ids[dt]
    let indented_println(args...) = println(ctx.io, " " ^ indent, args...)
        indented_println("{")
        indented_println("  \"id\": ", type_id, ",")
        indented_println("  \"kind\": \"struct\",")
        indented_println("  \"name\": ", type_name_json(dt), ",")
        indented_println("  \"size\": ", Core.sizeof(dt), ",")
        indented_println("  \"alignment\": ", Base.datatype_alignment(dt), ",")
        indented_println("  \"fields\": [")
        for i = 1:Base.datatype_nfields(dt)
            emit_field_info!(ctx, dt, i; indent = indent + 4)
            println(ctx.io, i == Base.datatype_nfields(dt) ? "" : ",")
        end
        indented_println("  ]")
        print(ctx.io, " " ^ indent, "}")
    end
end

function emit_primitive_type!(ctx::TypeEmitter, @nospecialize(dt::DataType); indent::Int = 0)
    type_id = ctx.type_ids[dt]
    let indented_println(args...) = println(ctx.io, " " ^ indent, args...)
        indented_println("{")
        indented_println("  \"id\": ", type_id, ",")
        indented_println("  \"kind\": \"primitive\",")
        indented_println("  \"name\": ", type_name_json(dt), ",")
        indented_println("  \"size\": ", Core.sizeof(dt), ",")
        indented_println("  \"alignment\": ", Base.datatype_alignment(dt))
        print(ctx.io, " " ^ indent, "}")
    end
end

function emit_type_info!(ctx::TypeEmitter, @nospecialize(dt::DataType); indent::Int = 0)
    if dt.name === Ptr.body.name
        emit_pointer_info!(ctx, dt; indent)
    elseif Base.isprimitivetype(dt)
        emit_primitive_type!(ctx, dt; indent)
    else
        emit_struct_info!(ctx, dt; indent)
    end
end

function emit_method_info!(ctx::TypeEmitter, method::Core.Method; indent::Int = 0)
    (rt, sig) = method.ccallable
    (name, symbol) = let
        symbol = length(method.ccallable) > 2 ? Symbol(method.ccallable[3]) : method.name
        iob = IOBuffer()
        print(IOContext(iob, :print_method_signature_only => true), method)
        str = String(take!(iob))
        if symbol !== method.name && startswith(str, String(method.name))
            # Make a best-effort attempt to use the exported name
            #
            # Note: the `startswith` check is to make sure we support 'functor's in arg0,
            # which Base.@ccallable supports as long as they are singletons.
            str = replace(str, String(method.name) => String(symbol); count = 1)
        end
        (str, String(symbol))
    end

    argnames = String.(Base.method_argnames(method))
    let indented_println(args...) = println(ctx.io, " " ^ indent, args...)
        indented_println("{")
        indented_println("  \"symbol\": ", escape_string_json(symbol), ",")
        indented_println("  \"name\": ", escape_string_json(name), ",")
        indented_println("  \"arguments\": [")
        for i in 2:length(sig.parameters)
            indented_println("    {")
            indented_println("      \"name\": ", escape_string_json(argnames[i]), ",")
            indented_println("      \"type\": ", ctx.type_ids[sig.parameters[i]])
            indented_println("    }", i == length(sig.parameters) ? "" : ",")
        end
        indented_println("  ],")
        indented_println("  \"returns\": {")
        indented_println("    \"type\": ", ctx.type_ids[rt])
        indented_println("  }")
        print(ctx.io, " " ^ indent, "}")
    end
end

function emit_abi_info!(ctx::TypeEmitter, exported::Vector{Core.Method}, types::IdSet{DataType})
    println(ctx.io, "{")

    # assign an ID to each type, so that we can refer to them
    for (i, T) in enumerate(types)
        ctx.type_ids[T] = i
    end

    # print exported functions
    println(ctx.io, "  \"functions\": [")
    for (i, method) in enumerate(exported)
        emit_method_info!(ctx, method; indent = 4)
        println(ctx.io, i == length(exported) ? "" : ",")
    end
    println(ctx.io, "  ],")

    # print type / structure information
    println(ctx.io, "  \"types\": [")
    for (i, T) in enumerate(types)
        emit_type_info!(ctx, T; indent = 4)
        println(ctx.io, i == length(types) ? "" : ",")
    end
    println(ctx.io, "  ]")

    println(ctx.io, "}")
end

function write_logfile(io::IO)
    types = Base.IdSet{DataType}()

    # discover all exported methods + any types they reference
    exported = Core.Method[]
    Base.visit(Core.GlobalMethods) do method
        if isdefined(method, :ccallable)
            push!(exported, method)
            (rt, sig) = method.ccallable
            for T in sig.parameters[2:end]
                recursively_add_types!(types, T)
            end
            recursively_add_types!(types, rt)
        end
    end

    # print the discovered ABI info
    ctx = TypeEmitter(io, IdDict{Any,Int}())
    emit_abi_info!(ctx, exported, types)
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
            write_logfile(io)
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
