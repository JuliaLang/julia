# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Serializer: known_object_data, object_number, serialize_cycle, deserialize_cycle, writetag,
                      __deserialized_types__, serialize_typename_body, deserialize_typename_body,
                      TYPENAME_TAG, object_numbers

type ClusterSerializer{I<:IO} <: AbstractSerializer
    io::I
    counter::Int
    table::ObjectIdDict

    sent_objects::Dict{UInt64, Bool} # used by serialize (track objects sent)

    ClusterSerializer(io::I) = new(io, 0, ObjectIdDict(), Dict())
end
ClusterSerializer(io::IO) = ClusterSerializer{typeof(io)}(io)

function deserialize(s::ClusterSerializer, ::Type{TypeName})
    number, full_body_sent = deserialize(s)
    makenew = false
    known = haskey(known_object_data, number)
    if !full_body_sent
        if !known
            error("Expected object in cache. Not found.")
        else
            tn = known_object_data[number]::TypeName
        end
    else
        name = deserialize(s)
        mod = deserialize(s)
        if known
            tn = known_object_data[number]::TypeName
        elseif mod !== __deserialized_types__ && isdefined(mod, name)
            tn = getfield(mod, name).name
            # TODO: confirm somehow that the types match
            #warn(mod, ".", name, " isdefined, need not have been serialized")
            name = tn.name
            mod = tn.module
        else
            name = gensym()
            mod = __deserialized_types__
            tn = ccall(:jl_new_typename_in, Ref{TypeName}, (Any, Any), name, mod)
            makenew = true
        end
    end
    deserialize_cycle(s, tn)
    full_body_sent && deserialize_typename_body(s, tn, number, name, mod, makenew)
    !known && (known_object_data[number] = tn)
    if !haskey(object_numbers, tn)
        object_numbers[tn] = number
    end
    return tn
end

function serialize(s::ClusterSerializer, t::TypeName)
    serialize_cycle(s, t) && return
    writetag(s.io, TYPENAME_TAG)

    identifier = object_number(t)
    if !haskey(s.sent_objects, identifier)
        serialize(s, (identifier, true))
        serialize(s, t.name)
        serialize(s, t.module)
        serialize_typename_body(s, t)
        s.sent_objects[identifier] = true
#        println(t.module, ":", t.name, ", id:", identifier, " sent")
    else
        serialize(s, (identifier, false))
#        println(t.module, ":", t.name, ", id:", identifier, " NOT sent")
    end
end
