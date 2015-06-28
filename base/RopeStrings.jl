# This file is a part of Julia. License is MIT: http://julialang.org/license

## ropes for efficient concatenation, etc. ##

immutable RopeString <: AbstractString
    head::AbstractString
    tail::AbstractString
    depth::Int32
    endof::Int

    RopeString(h::RopeString, t::RopeString) =
        strdepth(h.tail) + strdepth(t) < strdepth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h, t, max(h.depth,t.depth)+1, endof(h)+endof(t))

    RopeString(h::RopeString, t::AbstractString) =
        strdepth(h.tail) < strdepth(h.head) ?
            RopeString(h.head, RopeString(h.tail, t)) :
            new(h, t, h.depth+1, endof(h)+endof(t))

    RopeString(h::AbstractString, t::RopeString) =
        strdepth(t.head) < strdepth(t.tail) ?
            RopeString(RopeString(h, t.head), t.tail) :
            new(h, t, t.depth+1, endof(h)+endof(t))

    RopeString(h::AbstractString, t::AbstractString) =
        new(h, t, 1, endof(h)+endof(t))
end
RopeString(s::AbstractString) = RopeString(s,"")

strdepth(s::AbstractString) = 0
strdepth(s::RopeString) = s.depth

function next(s::RopeString, i::Int)
    eh = endof(s.head)
    if i <= eh
        return next(s.head, i)
    else
        c, j = next(s.tail, i-eh)
        return c, j+eh
    end
end

endof(s::RopeString) = s.endof
length(s::RopeString) = length(s.head) + length(s.tail)
write(io::IO, s::RopeString) = (write(io, s.head); write(io, s.tail))
sizeof(s::RopeString) = sizeof(s.head) + sizeof(s.tail)
