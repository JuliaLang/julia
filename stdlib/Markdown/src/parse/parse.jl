# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct MD
    content::Vector{Any}
    meta::Dict{Any, Any}

    MD(content::AbstractVector, meta::Dict = Dict()) =
        new(content, meta)
end

MD(xs...) = MD(vcat(xs...))

function MD(cfg::Config, xs...)
    md = MD(xs...)
    md.meta[:config] = cfg
    return md
end

config(md::MD) = md.meta[:config]::Config

# Forward some array methods

Base.push!(md::MD, x) = push!(md.content, x)
Base.getindex(md::MD, args...) = md.content[args...]
Base.setindex!(md::MD, args...) = setindex!(md.content, args...)
Base.lastindex(md::MD) = lastindex(md.content)
Base.firstindex(md::MD) = firstindex(md.content)
Base.length(md::MD) = length(md.content)
Base.isempty(md::MD) = isempty(md.content)
Base.copy(md::MD) = MD(copy(md.content), copy(md.meta))

==(a::MD, b::MD) = (html(a) == html(b))

# Parser functions:
#   md – should be modified appropriately
#   return – basically, true if parse was successful
#     false uses the next parser in the queue, true
#     goes back to the beginning
#
# Inner parsers:
#   return – element to use or nothing

# Inner parsing

function parseinline(stream::IO, md::MD, parsers::Vector{Function})
    for parser in parsers
        inner = parser(stream, md)
        inner ≡ nothing || return inner
    end
end

function parseinline(stream::IO, md::MD, config::Config)
    content = []
    buffer = IOBuffer()
    while !eof(stream)
        char = peek(stream, Char)
        if haskey(config.inner, char) &&
                (inner = parseinline(stream, md, config.inner[char])) !== nothing
            c = String(take!(buffer))
            !isempty(c) && push!(content, c)
            buffer = IOBuffer()
            push!(content, inner)
        else
            write(buffer, read(stream, Char))
        end
    end
    c = String(take!(buffer))
    !isempty(c) && push!(content, c)
    return content
end

parseinline(s::AbstractString, md::MD, c::Config) =
    parseinline(IOBuffer(s), md, c)

parseinline(s, md::MD) = parseinline(s, md, config(md))

# Block parsing

function parse(stream::IO, block::MD, config::Config; breaking = false)
    skipblank(stream)
    eof(stream) && return false
    for parser in (breaking ? config.breaking : [config.breaking; config.regular])
        parser(stream, block) && return true
    end
    return false
end

parse(stream::IO, block::MD; breaking = false) =
  parse(stream, block, config(block), breaking = breaking)

function parse(stream::IO; flavor = julia)
    isa(flavor, Symbol) && (flavor = flavors[flavor])
    markdown = MD(flavor)
    while parse(stream, markdown, flavor) end
    return markdown
end
