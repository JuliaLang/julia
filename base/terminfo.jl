# This file is a part of Julia. License is MIT: https://julialang.org/license

include("terminfo_data.jl")

"""
    struct TermInfoRaw

A structured representation of a terminfo file, without any knowledge of
particular capabilities, solely based on `term(5)`.

!!! warning
  This is not part of the public API, and thus subject to change without notice.

# Fields

- `names::Vector{String}`: The names this terminal is known by.
- `flags::BitVector`: A list of 0–$(length(TERM_FLAGS)) flag values.
- `numbers::Union{Vector{UInt16}, Vector{UInt32}}`: A list of 0–$(length(TERM_NUMBERS))
  number values. A value of `typemax(eltype(numbers))` is used to skip over
  unspecified capabilities while ensuring value indices are correct.
- `strings::Vector{Union{String, Nothing}}`: A list of 0–$(length(TERM_STRINGS))
  string values. A value of `nothing` is used to skip over unspecified
  capabilities while ensuring value indices are correct.
- `extended::Union{Nothing, Dict{Symbol, Union{Bool, Int, String}}}`: Should an
  extended info section exist, this gives the entire extended info as a
  dictionary. Otherwise `nothing`.

See also: `TermInfo` and `TermCapability`.
"""
struct TermInfoRaw
    names::Vector{String}
    flags::BitVector
    numbers::Union{Vector{UInt16}, Vector{UInt32}}
    strings::Vector{Union{String, Nothing}}
    extended::Union{Nothing, Dict{Symbol, Union{Bool, Int, String}}}
end

"""
    struct TermInfo

A parsed terminfo paired with capability information.

!!! warning
  This is not part of the public API, and thus subject to change without notice.

# Fields

- `names::Vector{String}`: The names this terminal is known by.
- `flags::Int`: The number of flags specified.
- `numbers::BitVector`: A mask indicating which of `TERM_NUMBERS` have been
  specified.
- `strings::BitVector`: A mask indicating which of `TERM_STRINGS` have been
  specified.
- `extensions::Vector{Symbol}`: A list of extended capability variable names.
- `capabilities::Dict{Symbol, Union{Bool, Int, String}}`: The capability values
  themselves.

See also: `TermInfoRaw` and `TermCapability`.
"""
struct TermInfo
    names::Vector{String}
    flags::Int
    numbers::BitVector
    strings::BitVector
    extensions::Vector{Symbol}
    capabilities::Dict{Symbol, Union{Bool, Int, String}}
end

TermInfo() = TermInfo([], 0, [], [], [], Dict())

function read(data::IO, ::Type{TermInfoRaw})
    # Parse according to `term(5)`
    # Header
    magic = read(data, UInt16) |> ltoh
    NumInt = if magic == 0o0432
        UInt16
    elseif magic == 0o01036
        UInt32
    else
        throw(ArgumentError("Terminfo data did not start with the magic number 0o0432 or 0o01036"))
    end
    name_bytes = read(data, UInt16) |> ltoh
    flag_bytes = read(data, UInt16) |> ltoh
    numbers_count = read(data, UInt16) |> ltoh
    string_count = read(data, UInt16) |> ltoh
    table_bytes = read(data, UInt16) |> ltoh
    # Terminal Names
    term_names = split(String(read(data, name_bytes - 1)), '|') .|> String
    0x00 == read(data, UInt8) ||
        throw(ArgumentError("Terminfo data did not contain a null byte after the terminal names section"))
    # Boolean Flags
    flags = read(data, flag_bytes) .== 0x01
    if position(data) % 2 != 0
        0x00 == read(data, UInt8) ||
            throw(ArgumentError("Terminfo did not contain a null byte after the flag section, expected to position the start of the numbers section on an even byte"))
    end
    # Numbers, Strings, Table
    numbers = reinterpret(NumInt, read(data, numbers_count * sizeof(NumInt))) .|> ltoh
    string_indices = reinterpret(UInt16, read(data, string_count * sizeof(UInt16))) .|> ltoh
    strings_table = read(data, table_bytes)
    strings = map(string_indices) do idx
        if idx ∉ (0xffff, 0xfffe)
            len = findfirst(==(0x00), view(strings_table, 1+idx:length(strings_table)))
            !isnothing(len) ||
                throw(ArgumentError("Terminfo string table entry does not terminate with a null byte"))
            String(strings_table[1+idx:idx+len-1])
        end
    end
    TermInfoRaw(term_names, flags, numbers, strings,
                if !eof(data) extendedterminfo(data; NumInt) end)
end

"""
    extendedterminfo(data::IO; NumInt::Union{Type{UInt16}, Type{UInt32}})

Read an extended terminfo section from `data`, with `NumInt` as the numbers type.

This will accept any terminfo content that conforms with `term(5)`.

See also: `read(::IO, ::Type{TermInfoRaw})`
"""
function extendedterminfo(data::IO; NumInt::Union{Type{UInt16}, Type{UInt32}})
    # Extended info
    if position(data) % 2 != 0
        0x00 == read(data, UInt8) ||
            throw(ArgumentError("Terminfo did not contain a null byte before the extended section, expected to position the start on an even byte"))
    end
    # Extended header
    flag_bytes = read(data, UInt16) |> ltoh
    numbers_count = read(data, UInt16) |> ltoh
    string_count = read(data, UInt16) |> ltoh
    table_count = read(data, UInt16) |> ltoh
    table_bytes = read(data, UInt16) |> ltoh
    # Extended flags/numbers/strings
    flags = read(data, flag_bytes) .== 0x01
    if flag_bytes % 2 != 0
        0x00 == read(data, UInt8) ||
            throw(ArgumentError("Terminfo did not contain a null byte after the extended flag section, expected to position the start of the numbers section on an even byte"))
    end
    numbers = map(n -> Int(ltoh(n)), reinterpret(NumInt, read(data, numbers_count * sizeof(NumInt))))
    table_indices = reinterpret(UInt16, read(data, table_count * sizeof(UInt16))) .|> ltoh
    table_strings = [String(readuntil(data, 0x00)) for _ in 1:length(table_indices)]
    strings = table_strings[1:string_count]
    labels = Symbol.(table_strings[string_count+1:end])
    Dict{Symbol, Union{Bool, Int, String}}(
        labels .=> vcat(flags, numbers, strings))
end

"""
    TermInfo(raw::TermInfoRaw)

Construct a `TermInfo` from `raw`, using known terminal capabilities (as of
NCurses 6.3, see `TERM_FLAGS`, `TERM_NUMBERS`, and `TERM_STRINGS`).
"""
function TermInfo(raw::TermInfoRaw)
    capabilities = Dict{Symbol, Union{Bool, Int, String}}()
    sizehint!(capabilities, 2 * (length(raw.flags) + length(raw.numbers) + length(raw.strings)))
    for (flag, value) in zip(TERM_FLAGS, raw.flags)
        capabilities[flag.short] = value
        capabilities[flag.long] = value
    end
    for (num, value) in zip(TERM_NUMBERS, raw.numbers)
        if value != typemax(eltype(raw.numbers))
            capabilities[num.short] = Int(value)
            capabilities[num.long] = Int(value)
        end
    end
    for (str, value) in zip(TERM_STRINGS, raw.strings)
        if !isnothing(value)
            capabilities[str.short] = value
            capabilities[str.long] = value
        end
    end
    extensions = if !isnothing(raw.extended)
        capabilities = merge(capabilities, raw.extended)
        keys(raw.extended) |> collect
    else
        Symbol[]
    end
    TermInfo(raw.names, length(raw.flags),
             raw.numbers .!= typemax(eltype(raw.numbers)),
             map(!isnothing, raw.strings),
             extensions, capabilities)
end

getindex(ti::TermInfo, key::Symbol) = ti.capabilities[key]
get(ti::TermInfo, key::Symbol, default::D) where D<:Union{Bool, Int, String} =
    get(ti.capabilities, key, default)::D
get(ti::TermInfo, key::Symbol, default) = get(ti.capabilities, key, default)
keys(ti::TermInfo) = keys(ti.capabilities)
haskey(ti::TermInfo, key::Symbol) = haskey(ti.capabilities, key)

function show(io::IO, ::MIME"text/plain", ti::TermInfo)
    print(io, "TermInfo(", ti.names, "; ", ti.flags, " flags, ",
          sum(ti.numbers), " numbers, ", sum(ti.strings), " strings")
    !isempty(ti.extensions) > 0 &&
        print(io, ", ", length(ti.extensions), " extended capabilities")
    print(io, ')')
end

"""
    find_terminfo_file(term::String)

Locate the terminfo file for `term`, return `nothing` if none could be found.

The lookup policy is described in `terminfo(5)` "Fetching Compiled
Descriptions".
"""
function find_terminfo_file(term::String)
    isempty(term) && return
    chr, chrcode = string(first(term)), string(Int(first(term)), base=16)
    terminfo_dirs = if haskey(ENV, "TERMINFO")
        [ENV["TERMINFO"]]
    elseif isdir(joinpath(homedir(), ".terminfo"))
        [joinpath(homedir(), ".terminfo")]
    elseif haskey(ENV, "TERMINFO_DIRS")
        split(ENV["TERMINFO_DIRS"], ':')
    elseif Sys.isunix()
        ["/usr/share/terminfo"]
    else
        String[]
    end
    for dir in terminfo_dirs
        if isfile(joinpath(dir, chr, term))
            return joinpath(dir, chr, term)
        elseif isfile(joinpath(dir, chrcode, term))
            return joinpath(dir, chrcode, term)
        end
    end
end

"""
    load_terminfo(term::String)

Load the `TermInfo` for `term`, falling back on a blank `TermInfo`.
"""
function load_terminfo(term::String)
    file = find_terminfo_file(term)
    isnothing(file) && return TermInfo()
    try
        TermInfo(read(file, TermInfoRaw))
    catch err
        if err isa ArgumentError || err isa IOError
            TermInfo()
        else
            rethrow()
        end
    end
end

"""
The terminfo of the current terminal.
"""
current_terminfo::TermInfo = TermInfo()

# Legacy/TTY methods and the `:color` parameter

if Sys.iswindows()
    ttyhascolor(term_type = nothing) = true
else
    function ttyhascolor(term_type = get(ENV, "TERM", ""))
        startswith(term_type, "xterm") ||
            haskey(current_terminfo, :setaf)
    end
end

"""
    ttyhastruecolor()

Return a boolean signifying whether the current terminal supports 24-bit colors.

This uses the `COLORTERM` environment variable if possible, returning true if it
is set to either `"truecolor"` or `"24bit"`.

As a fallback, first on unix systems the `colors` terminal capability is checked
— should more than 256 colors be reported, this is taken to signify 24-bit
support.
"""
function ttyhastruecolor()
    get(ENV, "COLORTERM", "") ∈ ("truecolor", "24bit") ||
        @static if Sys.isunix()
            get(current_terminfo, :colors, 0) > 256
        else
            false
        end
end

function get_have_color()
    global have_color
    have_color === nothing && (have_color = ttyhascolor())
    return have_color::Bool
end

function get_have_truecolor()
    global have_truecolor
    have_truecolor === nothing && (have_truecolor = ttyhastruecolor())
    return have_truecolor::Bool
end

in(key_value::Pair{Symbol,Bool}, ::TTY) = key_value.first === :color && key_value.second === get_have_color()
haskey(::TTY, key::Symbol) = key === :color
getindex(::TTY, key::Symbol) = key === :color ? get_have_color() : throw(KeyError(key))
get(::TTY, key::Symbol, default) = key === :color ? get_have_color() : default
