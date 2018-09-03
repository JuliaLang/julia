# This file is a part of Julia. License is MIT: https://julialang.org/license

using Serialization: serialize_cycle, deserialize_cycle, writetag,
                     serialize_typename, deserialize_typename,
                     TYPENAME_TAG, reset_state, serialize_type
using Serialization.__deserialized_types__

import Serialization: object_number, lookup_object_number, remember_object

mutable struct ClusterSerializer{I<:IO} <: AbstractSerializer
    io::I
    counter::Int
    table::IdDict{Any,Any}
    pending_refs::Vector{Int}

    pid::Int                                     # Worker we are connected to.
    tn_obj_sent::Set{UInt64}                     # TypeName objects sent
    glbs_sent::Dict{UInt64, UInt64}              # (key,value) -> (objectid, hash_value)
    glbs_in_tnobj::Dict{UInt64, Vector{Symbol}}  # Track globals referenced in
                                                 # anonymous functions.
    anonfunc_id::UInt64

    function ClusterSerializer{I}(io::I) where I<:IO
        new(io, 0, IdDict(), Int[], worker_id_from_socket(io),
            Set{UInt64}(), Dict{UInt64, UInt64}(), Dict{UInt64, Vector{Symbol}}(), 0)
    end
end
ClusterSerializer(io::IO) = ClusterSerializer{typeof(io)}(io)

const object_numbers = WeakKeyDict()
const obj_number_salt = Ref(0)
function object_number(s::ClusterSerializer, @nospecialize(l))
    global obj_number_salt, object_numbers
    if haskey(object_numbers, l)
        return object_numbers[l]
    end
    # a hash function that always gives the same number to the same
    # object on the same machine, and is unique over all machines.
    ln = obj_number_salt[]+(UInt64(myid())<<44)
    obj_number_salt[] += 1
    object_numbers[l] = ln
    return ln::UInt64
end

const known_object_data = Dict{UInt64,Any}()

function lookup_object_number(s::ClusterSerializer, n::UInt64)
    return get(known_object_data, n, nothing)
end

function remember_object(s::ClusterSerializer, @nospecialize(o), n::UInt64)
    known_object_data[n] = o
    if isa(o, Core.TypeName) && !haskey(object_numbers, o)
        # set up reverse mapping for serialize
        object_numbers[o] = n
    end
    return nothing
end

function deserialize(s::ClusterSerializer, ::Type{Core.TypeName})
    full_body_sent = deserialize(s)
    number = read(s.io, UInt64)
    if !full_body_sent
        tn = lookup_object_number(s, number)::Core.TypeName
        remember_object(s, tn, number)
        deserialize_cycle(s, tn)
    else
        tn = deserialize_typename(s, number)
    end

    # retrieve arrays of global syms sent if any and deserialize them all.
    foreach(sym->deserialize_global_from_main(s, sym), deserialize(s))
    return tn
end

function serialize(s::ClusterSerializer, t::Core.TypeName)
    serialize_cycle(s, t) && return
    writetag(s.io, TYPENAME_TAG)

    identifier = object_number(s, t)
    send_whole = !(identifier in s.tn_obj_sent)
    serialize(s, send_whole)
    write(s.io, identifier)
    if send_whole
        # Track globals referenced in this anonymous function.
        # This information is used to resend modified globals when we
        # only send the identifier.
        prev = s.anonfunc_id
        s.anonfunc_id = identifier
        serialize_typename(s, t)
        s.anonfunc_id = prev
        push!(s.tn_obj_sent, identifier)
        finalizer(t) do x
            cleanup_tname_glbs(s, identifier)
        end
    end

    # Send global refs if required.
    syms = syms_2b_sent(s, identifier)
    serialize(s, syms)
    foreach(sym->serialize_global_from_main(s, sym), syms)
    nothing
end

function serialize(s::ClusterSerializer, g::GlobalRef)
    # Record if required and then invoke the default GlobalRef serializer.
    sym = g.name
    if g.mod === Main && isdefined(g.mod, sym)
        if (binding_module(Main, sym) === Main) && (s.anonfunc_id != 0) &&
            !startswith(string(sym), "#") # Anonymous functions are handled via FULL_GLOBALREF_TAG

            push!(get!(s.glbs_in_tnobj, s.anonfunc_id, []), sym)
        end
    end

    invoke(serialize, Tuple{AbstractSerializer, GlobalRef}, s, g)
end

# Send/resend a global object if
# a) has not been sent previously, i.e., we are seeing this objectid for the first time, or,
# b) hash value has changed or
# c) is a bits type
function syms_2b_sent(s::ClusterSerializer, identifier)
    lst = Symbol[]
    check_syms = get(s.glbs_in_tnobj, identifier, [])
    for sym in check_syms
        v = getfield(Main, sym)

        if isbits(v)
            push!(lst, sym)
        else
            oid = objectid(v)
            if haskey(s.glbs_sent, oid)
                # We have sent this object before, see if it has changed.
                s.glbs_sent[oid] != hash(sym, hash(v)) && push!(lst, sym)
            else
                push!(lst, sym)
            end
        end
    end
    return unique(lst)
end

function serialize_global_from_main(s::ClusterSerializer, sym)
    v = getfield(Main, sym)

    oid = objectid(v)
    record_v = true
    if isbits(v)
        record_v = false
    elseif !haskey(s.glbs_sent, oid)
        # set up a finalizer the first time this object is sent
        try
            finalizer(v) do x
                delete_global_tracker(s,x)
            end
        catch ex
            # Do not track objects that cannot be finalized.
            if isa(ex, ErrorException)
                record_v = false
            else
                rethrow(ex)
            end
        end
    end
    record_v && (s.glbs_sent[oid] = hash(sym, hash(v)))

    serialize(s, isconst(Main, sym))
    serialize(s, v)
end

function deserialize_global_from_main(s::ClusterSerializer, sym)
    sym_isconst = deserialize(s)
    v = deserialize(s)
    if sym_isconst
        ccall(:jl_set_const, Cvoid, (Any, Any, Any), Main, sym, v)
    else
        ccall(:jl_set_global, Cvoid, (Any, Any, Any), Main, sym, v)
    end
    return nothing
end

function delete_global_tracker(s::ClusterSerializer, v)
    oid = objectid(v)
    if haskey(s.glbs_sent, oid)
        delete!(s.glbs_sent, oid)
    end

    # TODO: A global binding is released and gc'ed here but it continues
    # to occupy memory on the remote node. Would be nice to release memory
    # if possible.
end

function cleanup_tname_glbs(s::ClusterSerializer, identifier)
    delete!(s.glbs_in_tnobj, identifier)
end

# TODO: cleanup from s.tn_obj_sent


# Specialized serialize-deserialize implementations for CapturedException to partially
# recover from any deserialization errors in `CapturedException.ex`

function serialize(s::ClusterSerializer, ex::CapturedException)
    serialize_type(s, typeof(ex))
    serialize(s, string(typeof(ex.ex))) # String type should not result in a deser error
    serialize(s, ex.processed_bt)       # Currently should not result in a deser error
    serialize(s, ex.ex)                 # can result in a UndefVarError on the remote node
                                        # if a type used in ex.ex is undefined on the remote node.
end

function original_ex(s::ClusterSerializer, ex_str, remote_stktrace)
    local pid_str = ""
    try
        pid_str = string(" from worker ", worker_id_from_socket(s.io))
    catch
    end

    stk_str = remote_stktrace ? "Remote" : "Local"
    ErrorException(string("Error deserializing a remote exception", pid_str, "\n",
                          "Remote(original) exception of type ", ex_str, "\n",
                          stk_str,  " stacktrace : "))
end

function deserialize(s::ClusterSerializer, t::Type{<:CapturedException})
    ex_str = deserialize(s)
    local bt
    local capex
    try
        bt = deserialize(s)
    catch e
        throw(CompositeException([
            original_ex(s, ex_str, false),
            CapturedException(e, catch_backtrace())
        ]))
    end

    try
        capex = deserialize(s)
    catch e
        throw(CompositeException([
            CapturedException(original_ex(s, ex_str, true), bt),
            CapturedException(e, catch_backtrace())
        ]))
    end

    return CapturedException(capex, bt)
end

"""
    clear!(syms, pids=workers(); mod=Main)

Clears global bindings in modules by initializing them to `nothing`.
`syms` should be of type `Symbol` or a collection of `Symbol`s . `pids` and `mod`
identify the processes and the module in which global variables are to be
reinitialized. Only those names found to be defined under `mod` are cleared.

An exception is raised if a global constant is requested to be cleared.
"""
function clear!(syms, pids=workers(); mod=Main)
    @sync for p in pids
        @async remotecall_wait(clear_impl!, p, syms, mod)
    end
end
clear!(sym::Symbol, pid::Int; mod=Main) = clear!([sym], [pid]; mod=mod)
clear!(sym::Symbol, pids=workers(); mod=Main) = clear!([sym], pids; mod=mod)
clear!(syms, pid::Int; mod=Main) = clear!(syms, [pid]; mod=mod)

clear_impl!(syms, mod::Module) = foreach(x->clear_impl!(x,mod), syms)
clear_impl!(sym::Symbol, mod::Module) = isdefined(mod, sym) && @eval(mod, global $sym = nothing)
