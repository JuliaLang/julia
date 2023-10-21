# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    AnnotatedString{S <: AbstractString} <: AbstractString

A string with metadata, in the form of annotated regions.

More specifically, this is a simple wrapper around any other
[`AbstractString`](@ref) that allows for regions of the wrapped string to be
annotated with labeled values.

```text
                           C
                    ┌──────┸─────────┐
  "this is an example annotated string"
  └──┰────────┼─────┘         │
     A        └─────┰─────────┘
                    B
```

The above diagram represents a `AnnotatedString` where three ranges have been
annotated (labeled `A`, `B`, and `C`). Each annotation holds a label (`Symbol`)
and a value (`Any`), paired together as a `Pair{Symbol, <:Any}`.

Labels do not need to be unique, the same region can hold multiple annotations
with the same label.

See also [`AnnotatedChar`](@ref), [`annotatedstring`](@ref),
[`annotations`](@ref), and [`annotate!`](@ref).

!!! warning
    While the constructors are part of the Base public API, the fields
    of `AnnotatedString` are not. This is to allow for potential future
    changes in the implementation of this type. Instead use the
    [`annotations`](@ref), and [`annotate!`](@ref) getter/setter
    functions.

# Constructors

```julia
AnnotatedString(s::S<:AbstractString) -> AnnotatedString{S}
AnnotatedString(s::S<:AbstractString, annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, <:Any}}})
```

A AnnotatedString can also be created with [`annotatedstring`](@ref), which acts much
like [`string`](@ref) but preserves any annotations present in the arguments.

# Example

```julia-repl
julia> AnnotatedString("this is an example annotated string",
                    [(1:18, :A => 1), (12:28, :B => 2), (18:35, :C => 3)])
"this is an example annotated string"
```
"""
struct AnnotatedString{S <: AbstractString} <: AbstractString
    string::S
    annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}
end

"""
    AnnotatedChar{S <: AbstractChar} <: AbstractChar

A Char with annotations.

More specifically, this is a simple wrapper around any other
[`AbstractChar`](@ref), which holds a list of arbitrary labeled annotations
(`Pair{Symbol, <:Any}`) with the wrapped character.

See also: [`AnnotatedString`](@ref), [`annotatedstring`](@ref), `annotations`,
and `annotate!`.

!!! warning
    While the constructors are part of the Base public API, the fields
    of `AnnotatedChar` are not. This it to allow for potential future
    changes in the implementation of this type. Instead use the
    [`annotations`](@ref), and [`annotate!`](@ref) getter/setter
    functions.

# Constructors

```julia
AnnotatedChar(s::S) -> AnnotatedChar{S}
AnnotatedChar(s::S, annotations::Vector{Pair{Symbol, <:Any}})
```

# Examples

```julia-repl
julia> AnnotatedChar('j', :label => 1)
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)
```
"""
struct AnnotatedChar{C <: AbstractChar} <: AbstractChar
    char::C
    annotations::Vector{Pair{Symbol, Any}}
end

## Constructors ##

# When called with overly-specialised arguments

AnnotatedString(s::AbstractString, annots::Vector{<:Tuple{UnitRange{Int}, <:Pair{Symbol, <:Any}}}) =
    AnnotatedString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}(annots))

AnnotatedChar(c::AbstractChar, annots::Vector{<:Pair{Symbol, <:Any}}) =
    AnnotatedChar(c, Vector{Pair{Symbol, Any}}(annots))

# Constructors to avoid recursive wrapping

AnnotatedString(s::AnnotatedString, annots::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}) =
    AnnotatedString(s.string, vcat(s.annotations, annots))

AnnotatedChar(c::AnnotatedChar, annots::Vector{Pair{Symbol, Any}}) =
    AnnotatedChar(c.char, vcat(s.annotations, annots))

String(s::AnnotatedString{String}) = s.string # To avoid pointless overhead

## Conversion/promotion ##

convert(::Type{AnnotatedString}, s::AnnotatedString) = s
convert(::Type{AnnotatedString{S}}, s::S) where {S <: AbstractString} =
    AnnotatedString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}())
convert(::Type{AnnotatedString}, s::S) where {S <: AbstractString} =
    convert(AnnotatedString{S}, s)
AnnotatedString(s::S) where {S <: AbstractString} = convert(AnnotatedString{S}, s)

convert(::Type{AnnotatedChar}, c::AnnotatedChar) = c
convert(::Type{AnnotatedChar{C}}, c::C) where { C <: AbstractChar } =
    AnnotatedChar{C}(c, Vector{Pair{Symbol, Any}}())
convert(::Type{AnnotatedChar}, c::C) where { C <: AbstractChar } =
    convert(AnnotatedChar{C}, c)

AnnotatedChar(c::AbstractChar) = convert(AnnotatedChar, c)
AnnotatedChar(c::UInt32) = convert(AnnotatedChar, Char(c))
AnnotatedChar{C}(c::UInt32) where {C <: AbstractChar} = convert(AnnotatedChar, C(c))

promote_rule(::Type{<:AnnotatedString}, ::Type{<:AbstractString}) = AnnotatedString

## AbstractString interface ##

ncodeunits(s::AnnotatedString) = ncodeunits(s.string)
codeunits(s::AnnotatedString) = codeunits(s.string)
codeunit(s::AnnotatedString) = codeunit(s.string)
codeunit(s::AnnotatedString, i::Integer) = codeunit(s.string, i)
isvalid(s::AnnotatedString, i::Integer) = isvalid(s.string, i)
@propagate_inbounds iterate(s::AnnotatedString, i::Integer=firstindex(s)) =
    if i <= lastindex(s.string); (s[i], nextind(s, i)) end
eltype(::Type{<:AnnotatedString{S}}) where {S} = AnnotatedChar{eltype(S)}
firstindex(s::AnnotatedString) = firstindex(s.string)
lastindex(s::AnnotatedString) = lastindex(s.string)

function getindex(s::AnnotatedString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds if isvalid(s, i)
        AnnotatedChar(s.string[i], annotations(s, i))
    else
        string_index_err(s, i)
    end
end

## AbstractChar interface ##

ncodeunits(c::AnnotatedChar) = ncodeunits(c.char)
codepoint(c::AnnotatedChar) = codepoint(c.char)

# Avoid the iteration fallback with comparison
cmp(a::AnnotatedString, b::AbstractString) = cmp(a.string, b)
cmp(a::AbstractString, b::AnnotatedString) = cmp(a, b.string)
# To avoid method ambiguity
cmp(a::AnnotatedString, b::AnnotatedString) = cmp(a.string, b.string)

==(a::AnnotatedString, b::AnnotatedString) =
    a.string == b.string && a.annotations == b.annotations

==(a::AnnotatedString, b::AbstractString) = isempty(a.annotations) && a.string == b
==(a::AbstractString, b::AnnotatedString) = isempty(b.annotations) && a == b.string

"""
    annotatedstring(values...)

Create a `AnnotatedString` from any number of `values` using their
[`print`](@ref)ed representation.

This acts like [`string`](@ref), but takes care to preserve any annotations
present (in the form of [`AnnotatedString`](@ref) or [`AnnotatedChar`](@ref) values).

See also [`AnnotatedString`](@ref) and [`AnnotatedChar`](@ref).

## Examples

```julia-repl
julia> annotatedstring("now a AnnotatedString")
"now a AnnotatedString"

julia> annotatedstring(AnnotatedString("annotated", [(1:9, :label => 1)]), ", and unannotated")
"annotated, and unannotated"
```
"""
function annotatedstring(xs...)
    isempty(xs) && return AnnotatedString("")
    size = mapreduce(_str_sizehint, +, xs)
    s = IOContext(IOBuffer(sizehint=size), :color => true)
    annotations = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    for x in xs
        if x isa AnnotatedString
            for (region, annot) in x.annotations
                push!(annotations, (s.io.size .+ (region), annot))
            end
            print(s, x.string)
        elseif x isa SubString{<:AnnotatedString}
            for (region, annot) in x.string.annotations
                start, stop = first(region), last(region)
                if start <= x.offset + x.ncodeunits && stop > x.offset
                    rstart = s.io.size + max(0, start - x.offset - 1) + 1
                    rstop = s.io.size + min(stop, x.offset + x.ncodeunits) - x.offset
                    push!(annotations, (rstart:rstop, annot))
                end
            end
            print(s, SubString(x.string.string, x.offset, x.ncodeunits, Val(:noshift)))
        elseif x isa AnnotatedChar
            for annot in x.annotations
                push!(annotations, (1+s.io.size:1+s.io.size, annot))
            end
            print(s, x.char)
        else
            print(s, x)
        end
    end
    str = String(resize!(s.io.data, s.io.size))
    AnnotatedString(str, annotations)
end

annotatedstring(s::AnnotatedString) = s
annotatedstring(c::AnnotatedChar) =
    AnnotatedString(string(c.char), [(1:ncodeunits(c), annot) for annot in c.annotations])

AnnotatedString(s::SubString{<:AnnotatedString}) = annotatedstring(s)

"""
    annotatedstring_optimize!(str::AnnotatedString)

Merge contiguous identical annotations in `str`.
"""
function annotatedstring_optimize!(s::AnnotatedString)
    last_seen = Dict{Pair{Symbol, Any}, Int}()
    i = 1
    while i <= length(s.annotations)
        region, keyval = s.annotations[i]
        prev = get(last_seen, keyval, 0)
        if prev > 0
            lregion, _ = s.annotations[prev]
            if last(lregion) + 1 == first(region)
                s.annotations[prev] =
                    setindex(s.annotations[prev],
                             first(lregion):last(region),
                             1)
                deleteat!(s.annotations, i)
            else
                delete!(last_seen, keyval)
            end
        else
            last_seen[keyval] = i
            i += 1
        end
    end
    s
end

function repeat(str::AnnotatedString, r::Integer)
    r == 0 && return one(AnnotatedString)
    r == 1 && return str
    unannot = repeat(str.string, r)
    annotations = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    len = ncodeunits(str)
    fullregion = firstindex(str):lastindex(str)
    for (region, annot) in str.annotations
        if region == fullregion
            push!(annotations, (firstindex(unannot):lastindex(unannot), annot))
        end
    end
    for offset in 0:len:(r-1)*len
        for (region, annot) in str.annotations
            if region != fullregion
                push!(annotations, (region .+ offset, annot))
            end
        end
    end
    AnnotatedString(unannot, annotations) |> annotatedstring_optimize!
end

repeat(str::SubString{<:AnnotatedString}, r::Integer) =
    repeat(AnnotatedString(str), r)

function repeat(c::AnnotatedChar, r::Integer)
    str = repeat(c.char, r)
    fullregion = firstindex(str):lastindex(str)
    AnnotatedString(str, [(fullregion, annot) for annot in c.annotations])
end

function reverse(s::AnnotatedString)
    lastind = lastindex(s)
    AnnotatedString(reverse(s.string),
                 [(UnitRange(1 + lastind - last(region),
                             1 + lastind - first(region)),
                   annot)
                  for (region, annot) in s.annotations])
end

# TODO optimise?
reverse(s::SubString{<:AnnotatedString}) = reverse(AnnotatedString(s))

# TODO implement `replace(::AnnotatedString, ...)`

## End AbstractString interface ##

"""
    annotate!(str::AnnotatedString, [range::UnitRange{Int}], label::Symbol => value)
    annotate!(str::SubString{AnnotatedString}, [range::UnitRange{Int}], label::Symbol => value)

Annotate a `range` of `str` (or the entire string) with a labeled value (`label` => `value`).
To remove existing `label` annotations, use a value of `nothing`.
"""
function annotate!(s::AnnotatedString, range::UnitRange{Int}, @nospecialize(labelval::Pair{Symbol, <:Any}))
    label, val = labelval
    indices = searchsorted(s.annotations, (range,), by=first)
    if val === nothing
        labelindex = filter(i -> first(s.annotations[i][2]) === label, indices)
        for index in Iterators.reverse(labelindex)
            deleteat!(s.annotations, index)
        end
    else
        splice!(s.annotations, indices, [(range, Pair{Symbol, Any}(label, val))])
    end
    s
end

annotate!(ss::AnnotatedString, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    annotate!(ss, firstindex(ss):lastindex(ss), labelval)

annotate!(s::SubString{<:AnnotatedString}, range::UnitRange{Int}, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    (annotate!(s.string, s.offset .+ (range), labelval); s)

annotate!(s::SubString{<:AnnotatedString}, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    (annotate!(s.string, s.offset .+ (1:s.ncodeunits), labelval); s)

"""
    annotate!(char::AnnotatedChar, label::Symbol => value)

Annotate `char` with the pair `label => value`.
"""
annotate!(c::AnnotatedChar, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    (push!(c.annotations, labelval); c)

"""
    annotations(str::AnnotatedString, [position::Union{Integer, UnitRange}])
    annotations(str::SubString{AnnotatedString}, [position::Union{Integer, UnitRange}])

Get all annotations that apply to `str`. Should `position` be provided, only
annotations that overlap with `position` will be returned.

See also: `annotate!`.
"""
annotations(s::AnnotatedString) = s.annotations

annotations(s::SubString{<:AnnotatedString}) =
    annotations(s, s.offset+1:s.offset+s.ncodeunits)

function annotations(s::AnnotatedString, pos::UnitRange{<:Integer})
    # TODO optimise
    annots = filter(label -> !isempty(intersect(pos, first(label))),
                    s.annotations)
    last.(annots)
end

annotations(s::AnnotatedString, pos::Integer) = annotations(s, pos:pos)

annotations(s::SubString{<:AnnotatedString}, pos::Integer) =
    annotations(s.string, s.offset + pos)
annotations(s::SubString{<:AnnotatedString}, pos::UnitRange{<:Integer}) =
    annotations(s.string, first(pos)+s.offset:last(pos)+s.offset)

"""
    annotations(chr::AnnotatedChar)

Get all annotations of `chr`.
"""
annotations(c::AnnotatedChar) = c.annotations
