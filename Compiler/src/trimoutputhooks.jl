# This file is a part of Julia. License is MIT: https://julialang.org/license

# Functions for AOT build scripts to register with `add_codegen_input_hook!`

using Base, Core.IR
using Base: isexpr
using ..Compiler: Const, foreignsymbol_type, widenconst

# The artifact (`nothing` if bundled) of the library `lib` and its path there, if static
function library_location(@nospecialize(lib))
    Libdl = Base.Libc.Libdl
    lib isa Libdl.LazyLibrary || return nothing
    path = lib.path
    path isa Libdl.LazyLibraryPath || return nothing
    pieces = path.pieces
    length(pieces) >= 2 || return nothing
    subpath = Base.tail(pieces)
    all(@nospecialize(p) -> p isa String, subpath) || return nothing
    subpath = join(subpath, '/')
    dir = pieces[1]
    dir isa Libdl.PrivateShlibdirGetter && return (nothing, subpath)
    dir isa Libdl.LazyArtifactDir && return (dir.artifact_hash, subpath)
    return nothing
end

# Write the foreign symbols of an AOT image to `path` as JSON (see `add_codegen_input_hook!`)
function record_foreign_symbols(path::String, codeinfos::Vector{Any})
    records = Any[]
    for i = 1:length(codeinfos)
        item = codeinfos[i]
        item isa CodeInstance || continue
        src = codeinfos[i + 1]::CodeInfo
        for stmt in src.code
            kind = isexpr(stmt, :foreigncall) ? "ccall" :
                   isexpr(stmt, :foreignglobal) ? "cglobal" : nothing
            kind === nothing && continue
            types = foreignsymbol_type(stmt.args[1], src)
            types === nothing && continue # a function pointer
            name_type, library_type = types
            name = name_type isa Const ? name_type.val : nothing
            name isa Union{String,Symbol} || (name = nothing)
            record = (kind, name, library_type)
            any(@nospecialize(r) -> r === record, records) || push!(records, record)
        end
    end
    write_foreign_symbol_record(path, records)
    return nothing
end

function print_json_string(io::IO, @nospecialize(s))
    print(io, '"', escape_string(string(s)), '"')
end

function print_foreign_library_record(io::IO, @nospecialize(library_type))
    if library_type === nothing
        print(io, "null")
    elseif library_type isa Const
        lib = library_type.val
        location = library_location(lib)
        if lib isa Union{String,Symbol}
            print(io, "{\"name\": "); print_json_string(io, lib); print(io, "}")
        elseif location === nothing
            print(io, "{\"unresolved\": "); print_json_string(io, typeof(lib)); print(io, "}")
        elseif location[1] === nothing
            print(io, "{\"bundled\": "); print_json_string(io, location[2]); print(io, "}")
        else
            print(io, "{\"artifact\": {\"hash\": "); print_json_string(io, location[1])
            print(io, ", \"path\": "); print_json_string(io, location[2]); print(io, "}}")
        end
    elseif widenconst(library_type) <: Union{String,Symbol}
        print(io, "{\"name\": null}")
    else
        print(io, "{\"unresolved\": "); print_json_string(io, widenconst(library_type)); print(io, "}")
    end
end

function write_foreign_symbol_record(path::String, records::Vector{Any})
    open(path, "w") do io
        print(io, "[")
        for i = 1:length(records)
            (kind, name, library_type) = records[i]
            print(io, i == 1 ? "\n  " : ",\n  ")
            print(io, "{\"kind\": \"", kind, "\", \"name\": ")
            name === nothing ? print(io, "null") : print_json_string(io, name)
            print(io, ", \"library\": ")
            print_foreign_library_record(io, library_type)
            print(io, "}")
        end
        print(io, "\n]\n")
    end
    return nothing
end
