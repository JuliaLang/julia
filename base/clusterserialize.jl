# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Serializer: known_object_data, object_number, serialize_cycle, deserialize_cycle, writetag,
                      __deserialized_types__, serialize_typename, deserialize_typename,
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
    number = read(s.io, UInt64)
    if !full_body_sent
        tn = get(known_object_data, number, nothing)::TypeName
        if !haskey(object_numbers, tn)
            # set up reverse mapping for serialize
            object_numbers[tn] = number
        end
        deserialize_cycle(s, tn)
    else
        tn = deserialize_typename(s, number)
    end
    return tn
end

function serialize(s::ClusterSerializer, t::TypeName)
    serialize_cycle(s, t) && return
    writetag(s.io, TYPENAME_TAG)

    identifier = object_number(t)
    send_whole = !(identifier in s.sent_objects)
    serialize(s, send_whole)
    write(s.io, identifier)
    if send_whole
        serialize_typename(s, t)
        push!(s.sent_objects, identifier)
    end
#   println(t.module, ":", t.name, ", id:", identifier, send_whole ? " sent" : " NOT sent")
    nothing
end
