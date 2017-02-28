# This file is a part of Julia. License is MIT: http://julialang.org/license

mutable struct Backtrack{T}
    target::Vector{T}
    pos::Int
    len::Int
    cache::SparseVector{Int,Int}
    max_pos::Int
end

function Backtrack(target::AbstractString)
    t = collect(target)
    len = length(t)
    # Setting max_pos to 1 means that backtrack[1] == 0
    Backtrack(t, 0, len, spzeros(Int, len), 1)
end

function Backtrack(target::String)
    t = Vector{UInt8}(target)  # convert String to a utf8-byte-iterator
    len = length(t)
    Backtrack(t, 0, len, spzeros(Int, len), 1)
end

function backtrack(bt::Backtrack, index::Integer)
    for i = (bt.max_pos + 1):index
        b = bt.cache[i - 1] + 1
        # Requires that i > b. We can avoid this check since we start max_pos at 1.
        if bt.target[i] == bt.target[b]
            bt.cache[i] = b
        end
    end
    bt.max_pos = index
    return bt.cache[index]
end

@inline function record{T}(bt::Backtrack{T}, c::T)
    i = bt.pos
    # Backtrack until the next target character matches what was found
    while i > 0 && c != bt.target[i + 1]
        i = backtrack(bt, i)
    end
    if c == bt.target[i + 1]
        i += 1
    end
    bt.pos = i
    return i == bt.len
end

function readuntil(io::IO, target::AbstractString)
    i = start(target)
    if done(target, i)
        return ""
    end
    c, i = next(target, i)
    if done(target, i) && c < Char(0x80)
        return readuntil_string(s, c % UInt8)
    end
    backtrack = Backtrack(target)
    out = IOBuffer()
    while !eof(io)
        c = read(io, Char)
        write(out, c)
        record(backtrack, c) && break
    end
    return String(take!(out))
end

function readuntil(s::IO, target::String)
    i = start(target)
    if done(target, i)
        return ""
    end
    c, i = next(target, i)
    if done(target, i) && c < Char(0x80)
        return readuntil_string(s, c % UInt8)
    end
    backtrack = Backtrack(target)
    out = Base.StringVector(0)
    while !eof(s)
        byte = read(s, UInt8)
        push!(out, byte)
        record(backtrack, byte) && break
    end
    return String(out)
end
