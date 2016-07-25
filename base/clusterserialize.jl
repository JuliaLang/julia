# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Serializer: known_object_data, object_number, serialize_cycle, deserialize_cycle, writetag,
                      __deserialized_types__, serialize_typename_body, deserialize_typename_body,
                      TYPENAME_TAG, object_numbers

type ClusterSerializer{I<:IO} <: AbstractSerializer
    io::I
    counter::Int
    table::ObjectIdDict

    sent_objects::Set{UInt64} # used by serialize (track objects sent)

    ClusterSerializer(io::I) = new(io, 0, ObjectIdDict(), Set{UInt64}())
end
ClusterSerializer(io::IO) = ClusterSerializer{typeof(io)}(io)

function deserialize(s::ClusterSerializer, ::Type{TypeName})
    full_body_sent = deserialize(s)
    if !full_body_sent
        number = read(s.io, UInt64)
        tn = get(known_object_data, number, nothing)::TypeName
        if !haskey(object_numbers, tn)
            # setup reverse mapping for serialize
            object_numbers[tn] = number
        end
        deserialize_cycle(s, tn)
    else
        tn = invoke(deserialize, (AbstractSerializer, Type{TypeName}), s, TypeName)
    end
    return tn
end

function serialize(s::ClusterSerializer, t::TypeName)
    serialize_cycle(s, t) && return
    writetag(s.io, TYPENAME_TAG)

    identifier = object_number(t)
    if !(identifier in s.sent_objects)
        serialize(s, true)
        write(s.io, identifier)
        serialize(s, t.name)
        serialize(s, t.module)
        serialize_typename_body(s, t)
        push!(s.sent_objects, identifier)
#        println(t.module, ":", t.name, ", id:", identifier, " sent")
    else
        serialize(s, false)
        write(s.io, identifier)
#        println(t.module, ":", t.name, ", id:", identifier, " NOT sent")
    end
    nothing
end
