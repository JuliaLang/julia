# This file is a part of Julia. License is MIT: https://julialang.org/license

const RGBTuple = NamedTuple{(:r, :g, :b), NTuple{3, UInt8}}

"""
    struct SimpleColor

A basic representation of a color, intended for string styling purposes.
It can either contain a named color (like `:red`), or an `RGBTuple` which
is a NamedTuple specifying an `r`, `g`, `b` color with a bit-depth of 8.

# Constructors

```julia
SimpleColor(name::Symbol)  # e.g. :red
SimpleColor(rgb::RGBTuple) # e.g. (r=1, b=2, g=3)
SimpleColor(r::Integer, b::Integer, b::Integer)
SimpleColor(rgb::UInt32)   # e.g. 0x123456
```

Also see `tryparse(SimpleColor, rgb::String)`.
"""
struct SimpleColor
    value::Union{Symbol, RGBTuple}
end

SimpleColor(r::Integer, g::Integer, b::Integer) = SimpleColor((; r=UInt8(r), g=UInt8(g), b=UInt8(b)))
SimpleColor(rgb::UInt32) = SimpleColor(reverse(reinterpret(UInt8, [rgb]))[2:end]...)

"""
    tryparse(::Type{SimpleColor}, rgb::String)

Attempt to parse `rgb` as a `SimpleColor`. If `rgb` starts with
`#` and has a length of 7, it is converted into a `RGBTuple`-backed `SimpleColor`.
If `rgb` starts with `a`-`z`, `rgb` is interpreted as a color name
and converted to a `Symbol`-backed `SimpleColor`.

Otherwise, `nothing` is returned.

# Examples

```jldoctest
julia> tryparse(SimpleColor, "blue")
SimpleColor(:blue)

julia> tryparse(SimpleColor, "#9558b2")
SimpleColor((r = 0x95, g = 0x58, b = 0xb2))

julia> tryparse(SimpleColor, "#nocolor")
```
"""
function tryparse(::Type{SimpleColor}, rgb::String)
    if ncodeunits(rgb) == 7 && first(rgb) == '#' &&
        all(∈(('#',) ∪ ('0':'9') ∪ ('a':'f') ∪ ('A':'F')), rgb)
        SimpleColor(parse(UInt8, rgb[2:3], base=16),
                    parse(UInt8, rgb[4:5], base=16),
                    parse(UInt8, rgb[6:7], base=16))
    elseif startswith(rgb, 'a':'z') || startswith(rgb, 'A':'Z')
        SimpleColor(Symbol(rgb))
    else
        nothing
    end
end

"""
    parse(::Type{SimpleColor}, rgb::String)

An analogue of `tryparse(SimpleColor, rgb::String)` (which see),
that raises an error instead of returning `nothing`.
"""
function parse(::Type{SimpleColor}, rgb::String)
    color = tryparse(SimpleColor, rgb)
    !isnothing(color) ||
        throw(ArgumentError("invalid color \"$rgb\""))
    color
end

"""
A [`Face`](@ref) is a collection of graphical attributes for displaying text.
Faces control how text is displayed in the terminal, and possibly other
places too.

Most of the time, a [`Face`](@ref) will be stored in the global faces dicts as a
unique association with a *face name* Symbol, and will be most often referred to
by this name instead of the [`Face`](@ref) object itself.

# Attributes

All attributes can be set via the keyword constructor, and default to `nothing`.

- `height` (an `Int` or `Float64`): The height in either deci-pt (when an `Int`),
  or as a factor of the base size (when a `Float64`).
- `weight` (a `Symbol`): One of the symbols (from faintest to densest)
  `:thin`, `:extralight`, `:light`, `:semilight`, `:normal`,
  `:medium`, `:semibold`, `:bold`, `:extrabold`, or `:black`.
  In terminals any weight greater than `:normal` is displayed as bold,
  and in terminals that support variable-brightness text, any weight
  less than `:normal` is displayed as faint.
- `slant` (a `Symbol`): One of the symbols `:italic`, `:oblique`, or `:normal`.
- `foreground` (a `SimpleColor`): The text foreground color.
- `background` (a `SimpleColor`): The text background color.
- `underline`, the text underline, which takes one of the following forms:
  - a `Bool`: Whether the text should be underlined or not.\\
  - a `SimpleColor`: The text should be underlined with this color.\\
  - a `Tuple{Nothing, Symbol}`: The text should be underlined using the style
    set by the Symbol, one of `:straight`, `:double`, `:curly`, `:dotted`,
    or `:dashed`.\\
  - a `Tuple{SimpleColor, Symbol}`: The text should be underlined in the specified
    SimpleColor, and using the style specified by the Symbol, as before.
- `strikethrough` (a `Bool`): Whether the text should be struck through.
- `inverse` (a `Bool`): Whether the foreground and background colors should be
  inverted.
- `inherit` (a `Vector{Symbol}`): Names of faces to inherit from,
  with earlier faces taking priority. All faces inherit from the `:default` face.
"""
struct Face
    font::Union{Nothing, String}
    height::Union{Nothing, Int, Float64}
    weight::Union{Nothing, Symbol}
    slant::Union{Nothing, Symbol}
    foreground::Union{Nothing, SimpleColor}
    background::Union{Nothing, SimpleColor}
    underline::Union{Nothing, Bool, SimpleColor,
                     Tuple{<:Union{Nothing, SimpleColor}, Symbol}}
    strikethrough::Union{Nothing, Bool}
    inverse::Union{Nothing, Bool}
    inherit::Vector{Symbol}
end

function Face(; font::Union{Nothing, String} = nothing,
              height::Union{Nothing, Int, Float64} = nothing,
              weight::Union{Nothing, Symbol} = nothing,
              slant::Union{Nothing, Symbol} = nothing,
              foreground::Union{Nothing, SimpleColor, Symbol, RGBTuple, UInt32} = nothing,
              background::Union{Nothing, SimpleColor, Symbol, RGBTuple, UInt32} = nothing,
              underline::Union{Nothing, Bool, SimpleColor,
                               Symbol, RGBTuple, UInt32,
                               Tuple{<:Union{Nothing, SimpleColor, Symbol, RGBTuple, UInt32}, Symbol}
                               } = nothing,
              strikethrough::Union{Nothing, Bool} = nothing,
              inverse::Union{Nothing, Bool} = nothing,
              inherit::Union{Symbol, Vector{Symbol}} = Symbol[],
              _...) # Simply ignore unrecognised keyword arguments.
    ascolor(::Nothing) = nothing
    ascolor(c::SimpleColor) = c
    ascolor(c::Union{Symbol, RGBTuple, UInt32}) = SimpleColor(c)
    Face(font, height, weight, slant,
         ascolor(foreground), ascolor(background),
         if underline isa Tuple
             (ascolor(underline[1]), underline[2])
         elseif underline isa Symbol || underline isa RGBTuple || underline isa UInt32
             ascolor(underline)
         else
             underline
         end,
         strikethrough,
         inverse,
         if inherit isa Symbol
             [inherit]
         else inherit end)
end

==(a::Face, b::Face) =
    getfield.(Ref(a), fieldnames(Face)) ==
    getfield.(Ref(b), fieldnames(Face))

function show(io::IO, ::MIME"text/plain", color::SimpleColor)
    skiptype = get(io, :typeinfo, nothing) === SimpleColor
    skiptype || show(io, SimpleColor)
    skiptype || print(io, '(')
    if get(io, :color, false)::Bool
        print(io, StyledString("■", :face => Face(foreground=color)), ' ')
    end
    if color.value isa Symbol
        print(io, color.value)
    else # rgb tuple
        print(io, '#', join(lpad.(string.(values(color.value), base=16), 2, '0')))
    end
    skiptype || print(io, ')')
    nothing
end

function show(io::IO, ::MIME"text/plain", face::Face)
    if get(io, :compact, false)::Bool
        show(io, Face)
        if get(io, :color, false)::Bool
            # Could do S"({$face:sample})", but S_str isn't defined yet
            print(io, StyledString("(sample)", [(2:7, :face => face)]))
        else
            print(io, '(')
            isfirst = true
            for field in setdiff(fieldnames(Face), (:inherit,))
                if !isnothing(getfield(face, field))
                    if isfirst; isfirst = false else print(io, ", ") end
                    print(io, field, '=')
                    show(io, getfield(face, field))
                end
            end
            if !isempty(face.inherit)
                if isfirst; isfirst = false else print(io, ", ") end
                print(io, "inherit=")
                show(IOContext(io, :typeinfo => Vector{Symbol}), face.inherit)
            end
            print(io, ')')
        end
    else
        show(io, Face)
        print(io, StyledString(" (sample)", [(3:8, :face => face)]))
        showcolor(io, color) = show(IOContext(io, :typeinfo => SimpleColor),
                                    MIME("text/plain"), color)
        setfields = Pair{Symbol, Any}[]
        isempty(setfields) || print(io, ":")
        fieldnamepad = 14
        for field in (:font, :height, :weight, :slant)
            if !isnothing(getfield(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ",
                      getfield(face, field))
            end
        end
        for field in (:foreground, :background)
            if !isnothing(getfield(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ")
                showcolor(io, getfield(face, field))
            end
        end
        if !isnothing(face.underline)
            print(io, '\n', lpad("underline", fieldnamepad, ' '), ": ")
            if face.underline isa Bool
                print(io, face.underline)
            elseif face.underline isa SimpleColor
                showcolor(io, face.underline)
            elseif face.underline isa Tuple{Nothing, Symbol}
                print(io, last(face.underline))
            elseif face.underline isa Tuple{SimpleColor, Symbol}
                showcolor(io, first(face.underline))
                print(io, ", ", last(face.underline))
            end
        end
        for field in (:strikethrough, :inverse)
            if !isnothing(getfield(face, field))
                print(io, '\n', lpad(String(field), fieldnamepad, ' '), ": ",
                      getfield(face, field))
            end
        end
        if !isempty(face.inherit)
            print(io, '\n', lpad("inherit", fieldnamepad, ' '), ": ")
            isfirst = true
            for iface in face.inherit
                if isfirst; isfirst = false else print(io, ", ") end
                print(io, iface, '(', StyledString("*", :face => iface), ')')
            end
        end
    end
end

function show(io::IO, face::Face)
    show(IOContext(io, :compact => true), MIME("text/plain"), face)
end

"""
Globally named [`Face`](@ref)s.

`default` gives the initial values of the faces, and `current` holds the active
(potentially modified) set of faces. This two-set system allows for any
modifications to the active faces to be undone.
"""
const FACES = let default = Dict{Symbol, Face}(
    # Default is special, it must be completely specified
    # and everything inherits from it.
    :default => Face(
        "monospace", 120,      # font, height
        :normal, :normal,      # weight, slant
        SimpleColor(:default), # foreground
        SimpleColor(:default), # background
        false, false, false,   # underline, strikethrough, overline
        Symbol[]),              # inherit
    # Property faces
    :bold => Face(weight=:bold),
    :italic => Face(slant=:italic),
    :underline => Face(underline=true),
    :strikethrough => Face(strikethrough=true),
    :inverse => Face(inverse=true),
    # Basic color faces
    :black => Face(foreground=:black),
    :red => Face(foreground=:red),
    :green => Face(foreground=:green),
    :yellow => Face(foreground=:yellow),
    :blue => Face(foreground=:blue),
    :magenta => Face(foreground=:magenta),
    :cyan => Face(foreground=:cyan),
    :white => Face(foreground=:white),
    :bright_black => Face(foreground=:bright_black),
    :grey => Face(foreground=:bright_black),
    :gray => Face(foreground=:bright_black),
    :bright_red => Face(foreground=:bright_red),
    :bright_green => Face(foreground=:bright_green),
    :bright_yellow => Face(foreground=:bright_yellow),
    :bright_blue => Face(foreground=:bright_blue),
    :bright_magenta => Face(foreground=:bright_magenta),
    :bright_cyan => Face(foreground=:bright_cyan),
    :bright_white => Face(foreground=:bright_white),
    # Useful common faces
    :shadow => Face(foreground=:bright_black),
    :region => Face(background=0x3a3a3a),
    :emphasis => Face(foreground=:blue),
    :highlight => Face(inherit=:emphasis, inverse=true),
    :code => Face(foreground=:cyan),
    # Styles of generic content categories
    :error => Face(foreground=:bright_red),
    :warning => Face(foreground=:yellow),
    :success => Face(foreground=:green),
    :info => Face(foreground=:bright_cyan),
    :note => Face(foreground=:grey),
    :tip => Face(foreground=:bright_green))
    (; default, current=Ref(copy(default)))
end

## Adding and resetting faces ##

"""
    addface!(name::Symbol => default::Face)

Create a new face by the name `name`. So long as no face already exists by this
name, `default` is added to both `FACES``.default` and (a copy of) to
`FACES`.`current`, with the current value returned.

Should the face `name` already exist, `nothing` is returned.

# Examples

```jldoctest
julia> addface!(:mypkg_myface => Face(slant=:italic, underline=true))
Face (sample)
         slant: italic
     underline: true
```
"""
function addface!((name, default)::Pair{Symbol, Face})
    if !haskey(FACES.default, name)
        FACES.default[name] = default
        FACES.current[][name] = if haskey(FACES.current[], name)
            merge(deepcopy(default), FACES.current[][name])
        else
            deepcopy(default)
        end
    end
end

"""
    resetfaces!()

Reset the current global face dictionary to the default value.
"""
function resetfaces!()
    FACES.current[] = copy(FACES.default)
end

"""
    resetfaces!(name::Symbol)

Reset the face `name` to its default value, which is returned.

If the face `name` does not exist, nothing is done and `nothing` returned.
In the unlikely event that the face `name` does not have a default value,
it is deleted, a warning message is printed, and `nothing` returned.
"""
function resetfaces!(name::Symbol)
    if !haskey(FACES.current[], name)
    elseif haskey(FACES.current[], name)
        FACES.current[][name] = copy(FACES.default[name])
    else # This shouldn't happen
        delete!(FACES.current[], name)
        println(stderr,
                """! The face $name was reset, but it had no default value, and so has been deleted instead!",
                     This should not have happened, perhaps the face was added without using `addface!`?""")
    end
end

"""
    withfaces(f, kv::Pair...)

Execute `f` with `FACES``.current` temporarily modified by zero or more
`:name => val` arguments `kv`. `withfaces` is generally used via the
`withfaces(kv...) do ... end` syntax. A value of `nothing` can be used to
temporarily unset an face (if if has been set). When `withfaces` returns, the
original `FACES``.current` has been restored.

    !!! warning
    Changing faces is not thread-safe.

# Examples

```jldoctest
julia> withfaces(:yellow => Face(foreground=:red), :green => :blue) do
           println(S"{yellow:red} and {green:blue} mixed make {magenta:purple}")
       end
"red and blue mixed make purple"
```
"""
function withfaces(f, keyvals::Pair{Symbol, <:Union{Face, Symbol, Nothing}}...)
    old = Dict{Symbol, Union{Face, Nothing}}()
    for (name, face) in keyvals
        old[name] = get(FACES.current[], name, nothing)
        if face isa Face
            FACES.current[][name] = face
        elseif face isa Symbol
            FACES.current[][name] =
                @something(get(old, face, nothing), get(FACES.current[], face, Face()))
        elseif haskey(FACES.current[], name)
            delete!(FACES.current[], name)
        end
    end
    try f()
    finally
        for (name, face) in old
            if isnothing(face)
                delete!(FACES.current[], name)
            else
                FACES.current[][name] = face
            end
        end
    end
end

"""
    withfaces(f, altfaces::Dict{Symbol, Face})

Execute `f` with `FACES``.current` temporarily swapped out with `altfaces`
When `withfaces` returns, the original `FACES``.current` has been restored.

    !!! warning
    Changing faces is not thread-safe.
"""
function withfaces(f, altfaces::Dict{Symbol, Face})
    oldfaces, FACES.current[] = FACES.current[], altfaces
    try f()
    finally
        FACES.current[] = oldfaces
    end
end

withfaces(f) = f()

## Face combination and inheritance ##

"""
    merge(initial::Face, others::Face...)

Merge the properties of the `initial` face and `others`, with
later faces taking priority.
"""
function merge(a::Face, b::Face)
    if isempty(a.inherit)
        Face(ifelse(isnothing(b.font),          a.font,          b.font),
             if isnothing(b.height)      a.height
             elseif isnothing(a.height)  b.height
             elseif b.height isa Int     b.height
             elseif a.height isa Int     round(Int, a.height * b.height)
             else a.height * b.height end,
             ifelse(isnothing(b.weight),        a.weight,        b.weight),
             ifelse(isnothing(b.slant),         a.slant,         b.slant),
             ifelse(isnothing(b.foreground),    a.foreground,    b.foreground),
             ifelse(isnothing(b.background),    a.background,    b.background),
             ifelse(isnothing(b.underline),     a.underline,     b.underline),
             ifelse(isnothing(b.strikethrough), a.strikethrough, b.strikethrough),
             ifelse(isnothing(b.inverse),       a.inverse,       b.inverse),
             b.inherit)
    else
        a_noinherit = Face(
            a.font, a.height, a.weight, a.slant, a.foreground, a.background,
            a.underline, a.strikethrough, a.inverse, Symbol[])
        a_inheritance = map(fname -> get(FACES.current[], fname, Face()), Iterators.reverse(a.inherit))
        a_resolved = merge(foldl(merge, a_inheritance), a_noinherit)
        merge(a_resolved, b)
    end
end

merge(a::Face, b::Face, others::Face...) = merge(merge(a, b), others...)

## Getting the combined face from a set of properties ##

"""
    getface(faces)

Obtain the final merged face from `faces`, an iterator of
[`Face`](@ref)s, face name `Symbol`s, and lists thereof.
"""
function getface(faces)
    isempty(faces) && return FACES.current[][:default]
    mergedface(face::Face) = face
    mergedface(face::Symbol) = get(FACES.current[], face, Face())
    mergedface(faces::Vector) = mapfoldl(mergedface, merge, Iterators.reverse(faces))
    combined = mapfoldl(mergedface, merge, Iterators.reverse(faces))::Face
    if !isempty(combined.inherit)
        combined = merge(combined, Face())
    end
    merge(FACES.current[][:default], combined)
end

"""
    getface(styles::Vector{Pair{Symbol, Any}})

Combine all of the `:face` styles with `getfaces`.
"""
function getface(styles::Vector{Pair{Symbol, Any}})
    faces = (last(prop) for prop in styles if first(prop) === :face)
    getface(faces)
end

getface(face::Face) = merge(FACES.current[][:default], merge(face, Face()))
getface(face::Symbol) = getface(get(FACES.current[], face, Face()))

"""
    getface()

Obtain the default face.
"""
getface() = FACES.current[][:default]

## Face/StyledString integration ##

"""
    getface(s::StyledString, i::Integer)

Get the merged [`Face`](@ref) that applies to `s` at index `i`.
"""
getface(s::StyledString, i::Integer) =
    getface(textproperties(s, i))

"""
    getface(c::StyledChar)

Get the merged [`Face`](@ref) that applies to `c`.
"""
getface(c::StyledChar) = getface(c.properties)

"""
    face!(s::Union{<:StyledString, <:SubString{<:StyledString}},
          [range::UnitRange{Int},] face::Union{Symbol, Face})

Apply `face` to `s`, along `range` if specified, or the whole of `s`.
"""
face!(s::Union{<:StyledString, <:SubString{<:StyledString}},
      range::UnitRange{Int}, face::Union{Symbol, Face}) =
          textproperty!(s, range, :face, face)

face!(s::Union{<:StyledString, <:SubString{<:StyledString}},
      face::Union{Symbol, Face}) =
          textproperty!(s, firstindex(s):lastindex(s), :face, face)

## Reading face definitions from a dictionary ##

"""
    loadfaces!(name::Symbol => update::Face)

Merge the face `name` in `FACES``.current` with `update`. If the face `name` does
not already exist in `FACES``.current`, then it is set to `update.`

# Examples

```jldoctest
julia> loadfaces!(:red => Face(foreground=0xff0000))
Face (sample)
    foreground: ■ #ff0000
```
"""
function loadfaces!((name, update)::Pair{Symbol, Face})
    if haskey(FACES.current[], name)
        FACES.current[][name] = merge(FACES.current[][name], update)
    else
        FACES.current[][name] = update
    end
end

function loadfaces!((name, _)::Pair{Symbol, Nothing})
    if haskey(FACES.current[], name)
        resetfaces!(name)
    end
end

"""
    loadfaces!(faces::Dict{String, Any})

For each face specified in `Dict`, load it to `FACES``.current`.
"""
function loadfaces!(faces::Dict{String, Any}, prefix::Union{String, Nothing}=nothing)
    for (name, spec) in faces
        fullname = if isnothing(prefix)
            name
        else
            string(prefix, '_', name)
        end
        fspec = filter((_, v)::Pair -> !(v isa Dict), spec)
        fnest = filter((_, v)::Pair -> v isa Dict, spec)
        !isempty(fspec) &&
            loadfaces!(Symbol(fullname) => convert(Face, fspec))
        !isempty(fnest) &&
            loadfaces!(fnest, fullname)
    end
end

function convert(::Type{Face}, spec::Dict)
    Face(if haskey(spec, "font") && spec["font"] isa String
             spec["font"] end,
         if haskey(spec, "height") && (spec["height"] isa Int || spec["height"] isa Float64)
             spec["height"]
         end,
         if haskey(spec, "weight") && spec["weight"] isa String
             Symbol(spec["weight"])
         elseif haskey(spec, "bold") && spec["bold"] isa Bool
             ifelse(spec["bold"], :bold, :normal)
         end,
         if haskey(spec, "slant") && spec["slant"] isa String
             Symbol(spec["slant"])
         elseif haskey(spec, "italic") && spec["italic"] isa Bool
             ifelse(spec["italic"], :italic, :normal)
         end,
         if haskey(spec, "foreground") && spec["foreground"] isa String
             tryparse(SimpleColor, spec["foreground"])
         elseif haskey(spec, "fg") && spec["fg"] isa String
             tryparse(SimpleColor, spec["fg"])
         end,
         if haskey(spec, "background") && spec["background"] isa String
             tryparse(SimpleColor, spec["background"])
         elseif haskey(spec, "bg") && spec["bg"] isa String
             tryparse(SimpleColor, spec["bg"])
         end,
         if !haskey(spec, "underline")
         elseif spec["underline"] isa Bool
             spec["underline"]
         elseif spec["underline"] isa String
             tryparse(SimpleColor, spec["underline"])
         elseif spec["underline"] isa Vector && length(spec["underline"]) == 2
             color = tryparse(SimpleColor, spec["underline"][1])
             (color, Symbol(spec["underline"][2]))
         end,
         if !haskey(spec, "strikethrough")
         elseif spec["strikethrough"] isa Bool
             spec["strikethrough"]
         elseif spec["strikethrough"] isa String
             tryparse(SimpleColor, spec["strikethrough"])
         end,
         if haskey(spec, "inverse") && spec["inverse"] isa Bool
             spec["inverse"] end,
         if !haskey(spec, "inherit")
             Symbol[]
         elseif spec["inherit"] isa String
             [Symbol(spec["inherit"])]
         elseif spec["inherit"] isa Vector{String}
             Symbol.(spec["inherit"])
         else
             Symbol[]
         end)
end

## Style macro ##

"""
    @S_str -> StyledString

Construct a styled string. Within the string, `{<specs>:<content>}` structures
apply the formatting to `<content>`, according to the list of comma-separated
specifications `<specs>`. Each spec can either take the form of a face name,
an inline face specification, or a `key=value` pair. The value must be wrapped
by `{...}` should it contain any of the characters `,=:{}`.

String interpolation with `\$` functions in the same way as regular strings,
except quotes need to be escaped. Faces, keys, and values can also be
interpolated with `\$`.

# Example

```julia
S"The {bold:{italic:quick} {(foreground=#cd853f):brown} fox} jumped over \
the {link={https://en.wikipedia.org/wiki/Laziness}:lazy} dog"
```
"""
macro S_str(raw_content::String)
    parts = Any[]
    content = unescape_string(raw_content, ('{', '}', '$', '\n'))
    content_bytes = Vector{UInt8}(content)
    s = Iterators.Stateful(zip(eachindex(content), content))
    offset = 0
    point = 1
    escape = false
    active_styles = Vector{Vector{Tuple{Int, Union{Symbol, Expr, Pair{Symbol, Any}}}}}()
    pending_styles = Vector{Tuple{UnitRange{Int}, Union{Symbol, Expr, Pair{Symbol, Any}}}}()
    interpolated = false
    function addpart(stop::Int)
        str = String(content_bytes[point:stop+offset+ncodeunits(content[stop])-1])
        push!(parts,
              if isempty(pending_styles) && isempty(active_styles)
                  str
              else
                  styles = Expr[]
                  relevant_styles = Iterators.filter(
                      (start, _)::Tuple -> start <= stop + offset + 1,
                      Iterators.flatten(active_styles))
                  for (start, prop) in relevant_styles
                      range = (start - point):(stop - point + offset + 1)
                      push!(styles, Expr(:tuple, range, prop))
                  end
                  for (range, prop) in pending_styles
                      if !isempty(range)
                          push!(styles, Expr(:tuple, range .- point, prop))
                      end
                  end
                  empty!(pending_styles)
                  if isempty(styles)
                      str
                  else
                      :(StyledString($str, $(Expr(:vect, styles...))))
                  end
              end)
        point = nextind(content, stop) + offset
    end
    function addpart(start::Int, expr, stop::Int)
        if point < start
            addpart(start)
        end
        if isempty(active_styles)
            push!(parts, expr)
        else
            push!(parts,
                  :(StyledString(string($expr),
                                 $(last.(Iterators.flatten(active_styles))...))))
            map!.((_, prop)::Tuple -> (nextind(content, stop + offset), prop), active_styles, active_styles)
        end
    end
    for (i, char) in s
        if char == '\\'
            escape = true
        elseif escape
            if char in ('{', '}', '$')
                deleteat!(content_bytes, i + offset - 1)
                offset -= 1
            elseif char == '\n'
                deleteat!(content_bytes, i+offset-1:i+offset)
                offset -= 2
            end
            escape = false
        elseif char == '$'
            # Interpolation
            expr, nexti = Meta.parseatom(content, i + 1)
            deleteat!(content_bytes, i + offset)
            offset -= 1
            nchars = length(content[i:prevind(content, nexti)])
            for _ in 1:min(length(s), nchars-1)
                popfirst!(s)
            end
            addpart(i, expr, nexti)
            point = nexti + offset
            interpolated = true
        elseif char == '{'
            # Property declaration parsing and application
            properties = true
            hasvalue = false
            newstyles = Vector{Tuple{Int, Union{Symbol, Expr, Pair{Symbol, Any}}}}()
            while properties
                if !isnothing(peek(s)) && last(peek(s)) == '('
                    # Inline face
                    popfirst!(s)
                    specstr = Iterators.takewhile(c -> last(c) != ')', s) |>
                        collect .|> last |> String
                    spec = map(split(specstr, ',')) do spec
                        spec = rstrip(spec)
                        kv = split(spec, '=', limit=2)
                        if length(kv) == 2
                            kv[1] => @something(tryparse(Bool, kv[2]),
                                                String(kv[2]))
                        else "" => "" end
                    end |> Dict
                    push!(newstyles,
                          (nextind(content, i + offset),
                           Pair{Symbol, Any}(:face, convert(Face, spec))))
                    if isnothing(peek(s)) || last(popfirst!(s)) != ','
                        properties = false
                    end
                else
                    # Face symbol or key=value pair
                    key = if isempty(s)
                        break
                    elseif last(peek(s)) == '$'
                        interpolated = true
                        j, _ = popfirst!(s)
                        expr, nextj = Meta.parseatom(content, j + 1)
                        nchars = length(content[j:prevind(content, nextj)])
                        for _ in 1:min(length(s), nchars-1)
                            popfirst!(s)
                        end
                        if !isempty(s)
                            _, c = popfirst!(s)
                            if c == ':'
                                properties = false
                            elseif c == '='
                                hasvalue = true
                            end
                        end
                        expr
                    else
                        Iterators.takewhile(
                            function(c)
                                if last(c) == ':' # Start of content
                                    properties = false
                                elseif last(c) == '=' # Start of value
                                    hasvalue = true
                                    false
                                elseif last(c) == ',' # Next key
                                    false
                                else true end
                            end, s) |> collect .|> last |> String
                    end
                    if hasvalue
                        hasvalue = false
                        value = if !isnothing(peek(s))
                            if last(peek(s)) == '{'
                                # Grab {}-wrapped value
                                popfirst!(s)
                                isescaped = false
                                val = Vector{Char}()
                                while (next = popfirst!(s)) |> !isnothing
                                    (_, c) = next
                                    if isescaped && c ∈ ('\\', '}')
                                        push!(val, c)
                                    elseif isescaped
                                        push!(val, '\\', c)
                                    elseif c == '}'
                                        break
                                    else
                                        push!(val, c)
                                    end
                                end
                                String(val)
                            elseif last(peek(s)) == '$'
                                j, _ = popfirst!(s)
                                expr, nextj = Meta.parseatom(content, j + 1)
                                nchars = length(content[j:prevind(content, nextj)])
                                for _ in 1:min(length(s), nchars-1)
                                    popfirst!(s)
                                end
                                interpolated = true
                                expr
                            else
                                # Grab up to next value, or start of content.
                                Iterators.takewhile(
                                    function (c)
                                        if last(c) == ':'
                                            properties = false
                                        elseif last(c) == ','
                                            false
                                        else true end
                                    end, s) |> collect .|> last |> String
                            end
                        end
                        push!(newstyles,
                              (nextind(content, i + offset),
                               if key isa String && !(value isa Symbol || value isa Expr)
                                   Pair{Symbol, Any}(Symbol(key), value)
                               elseif key isa Expr || key isa Symbol
                                   :(Pair{Symbol, Any}($key, $value))
                               else
                                   :(Pair{Symbol, Any}(
                                       $(QuoteNode(Symbol(key))), $value))
                               end))
                    elseif key !== "" # No value, hence a Face property
                        push!(newstyles,
                              (nextind(content, i + offset),
                               if key isa Symbol || key isa Expr
                                   :(Pair{Symbol, Any}(:face, $key))
                               else # Face symbol
                                   Pair{Symbol, Any}(:face, Symbol(key))
                               end))
                    end
                end
            end
            push!(active_styles, newstyles)
            # Adjust content_bytes/offset based on how much the index
            # has been incremented in the processing of the
            # style declaration(s).
            if !isnothing(peek(s))
                nexti = first(peek(s))
                deleteat!(content_bytes, i+offset:nexti+offset-1)
                offset -= nexti - i
            end
        elseif char == '}' && !isempty(active_styles)
            # Close off most recent active style
            for (start, prop) in pop!(active_styles)
                push!(pending_styles, (start:i+offset, prop))
            end
            deleteat!(content_bytes, i + offset)
            offset -= 1
        end
    end
    # Ensure that any trailing unstyled content is added
    if point <= lastindex(content) + offset
        addpart(lastindex(content))
    end
    if !isempty(active_styles)
        println(stderr, "WARNING: Styled string macro in module ", __module__,
                " at ", something(__source__.file, ""), ':', string(__source__.line),
                " contains unterminated styled constructs.")
    end
    if interpolated
        :(styledstring($(parts...))) |> esc
    else
        styledstring(map(eval, parts)...)
    end
end
