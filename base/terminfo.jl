# This file is a part of Julia. License is MIT: https://julialang.org/license

# Since this code is in the startup-path, we go to some effort to
# be easier on the compiler, such as using `map` over broadcasting.

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
- `numbers::Union{Vector{Int16}, Vector{Int32}}`: A list of 0–$(length(TERM_NUMBERS))
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
    numbers::Vector{Int}
    strings::Vector{Union{String, Nothing}}
    extended::Union{Nothing, Dict{Symbol, Union{Bool, Int, String, Nothing}}}
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
    flags::Dict{Symbol, Bool}
    numbers::Dict{Symbol, Int}
    strings::Dict{Symbol, String}
    extensions::Union{Nothing, Set{Symbol}}
    aliases::Dict{Symbol, Symbol}
end

TermInfo() = TermInfo([], Dict(), Dict(), Dict(), nothing, Dict())

function read(data::IO, ::Type{TermInfoRaw})
    # Parse according to `term(5)`
    # Header
    magic = read(data, UInt16) |> ltoh
    NumInt = if magic == 0o0432
        Int16
    elseif magic == 0o01036
        Int32
    else
        throw(ArgumentError("Terminfo data did not start with the magic number 0o0432 or 0o01036"))
    end
    name_bytes, flag_bytes, numbers_count, string_count, table_bytes =
        @ntuple 5 _->read(data, Int16) |> ltoh
    # Terminal Names
    term_names = map(String, split(String(read(data, name_bytes - 1)), '|'))
    0x00 == read(data, UInt8) ||
        throw(ArgumentError("Terminfo data did not contain a null byte after the terminal names section"))
    # Boolean Flags
    flags = map(==(0x01), read(data, flag_bytes))
    if position(data) % 2 != 0
        0x00 == read(data, UInt8) ||
            throw(ArgumentError("Terminfo did not contain a null byte after the flag section, expected to position the start of the numbers section on an even byte"))
    end
    # Numbers, Strings, Table
    numbers = map(Int ∘ ltoh, reinterpret(NumInt, read(data, numbers_count * sizeof(NumInt))))
    string_indices = map(ltoh, reinterpret(Int16, read(data, string_count * sizeof(Int16))))
    strings_table = read(data, table_bytes)
    strings = _terminfo_read_strings(strings_table, string_indices)
    TermInfoRaw(term_names, flags, numbers, strings,
                if !eof(data) extendedterminfo(data, NumInt) end)
end

"""
    extendedterminfo(data::IO; NumInt::Union{Type{Int16}, Type{Int32}})

Read an extended terminfo section from `data`, with `NumInt` as the numbers type.

This will accept any terminfo content that conforms with `term(5)`.

See also: `read(::IO, ::Type{TermInfoRaw})`
"""
function extendedterminfo(data::IO, NumInt::Union{Type{Int16}, Type{Int32}})
    # Extended info
    if position(data) % 2 != 0
        0x00 == read(data, UInt8) ||
            throw(ArgumentError("Terminfo did not contain a null byte before the extended section; expected to position the start on an even byte"))
    end
    # Extended header
    flag_bytes, numbers_count, string_count, table_count, table_bytes =
        @ntuple 5 _->read(data, Int16) |> ltoh
    # Extended flags/numbers/strings
    flags = map(==(0x01), read(data, flag_bytes))
    if flag_bytes % 2 != 0
        0x00 == read(data, UInt8) ||
            throw(ArgumentError("Terminfo did not contain a null byte after the extended flag section; expected to position the start of the numbers section on an even byte"))
    end
    numbers = map(Int ∘ ltoh, reinterpret(NumInt, read(data, numbers_count * sizeof(NumInt))))
    table_indices = map(ltoh, reinterpret(Int16, read(data, table_count * sizeof(Int16))))
    table_data = read(data, table_bytes)
    strings = _terminfo_read_strings(table_data, table_indices[1:string_count])
    table_halfoffset = Int16(get(table_indices, string_count, 0) +
        ncodeunits(something(get(strings, length(strings), ""), "")) + 1)
    for index in string_count+1:lastindex(table_indices)
        table_indices[index] += table_halfoffset
    end
    labels = map(Symbol, _terminfo_read_strings(table_data, table_indices[string_count+1:end]))
    Dict{Symbol, Union{Bool, Int, String, Nothing}}(
        zip(labels, Iterators.flatten((flags, numbers, strings))))
end

"""
    _terminfo_read_strings(table::Vector{UInt8}, indices::Vector{Int16})

From `table`, read a string starting at each position in `indices`. Each string
must be null-terminated. Should an index be -1 or -2, `nothing` is given instead
of a string.
"""
function _terminfo_read_strings(table::Vector{UInt8}, indices::Vector{Int16})
    strings = Vector{Union{Nothing, String}}(undef, length(indices))
    map!(strings, indices) do idx
        if idx >= 0
            len = findfirst(==(0x00), view(table, 1+idx:length(table)))
            !isnothing(len) ||
                throw(ArgumentError("Terminfo table entry @$idx does not terminate with a null byte"))
            String(table[1+idx:idx+len-1])
        elseif idx ∈ (-1, -2)
        else
            throw(ArgumentError("Terminfo table index is invalid: -2 ≰ $idx"))
        end
    end
    strings
end

"""
    TermInfo(raw::TermInfoRaw)

Construct a `TermInfo` from `raw`, using known terminal capabilities (as of
NCurses 6.3, see `TERM_FLAGS`, `TERM_NUMBERS`, and `TERM_STRINGS`).
"""
function TermInfo(raw::TermInfoRaw)
    capabilities = Dict{Symbol, Union{Bool, Int, String}}()
    sizehint!(capabilities, 2 * (length(raw.flags) + length(raw.numbers) + length(raw.strings)))
    flags = Dict{Symbol, Bool}()
    numbers = Dict{Symbol, Int}()
    strings = Dict{Symbol, String}()
    aliases = Dict{Symbol, Symbol}()
    extensions = nothing
    for (flag, value) in zip(TERM_FLAGS, raw.flags)
        flags[flag.name] = value
        aliases[flag.capname] = flag.name
    end
    for (num, value) in zip(TERM_NUMBERS, raw.numbers)
        numbers[num.name] = Int(value)
        aliases[num.capname] = num.name
    end
    for (str, value) in zip(TERM_STRINGS, raw.strings)
        if !isnothing(value)
            strings[str.name] = value
            aliases[str.capname] = str.name
        end
    end
    if !isnothing(raw.extended)
        extensions = Set{Symbol}()
        longalias(key, value) = first(get(TERM_USER, (typeof(value), key), (nothing, "")))
        for (short, value) in raw.extended
            long = longalias(short, value)
            key = something(long, short)
            push!(extensions, key)
            if value isa Bool
                flags[key] = value
            elseif value isa Int
                numbers[key] = value
            elseif value isa String
                strings[key] = value
            end
            if !isnothing(long)
                aliases[short] = long
            end
        end
    end
    TermInfo(raw.names, flags, numbers, strings, extensions, aliases)
end

get(ti::TermInfo, key::Symbol, default::Bool)   = get(ti.flags,   get(ti.aliases, key, key), default)
get(ti::TermInfo, key::Symbol, default::Int)    = get(ti.numbers, get(ti.aliases, key, key), default)
get(ti::TermInfo, key::Symbol, default::String) = get(ti.strings, get(ti.aliases, key, key), default)

haskey(ti::TermInfo, key::Symbol) =
    haskey(ti.flags, key) || haskey(ti.numbers, key) || haskey(ti.strings, key) || haskey(ti.aliases, key)

function getindex(ti::TermInfo, key::Symbol)
    haskey(ti.flags, key) && return ti.flags[key]
    haskey(ti.numbers, key) && return ti.numbers[key]
    haskey(ti.strings, key) && return ti.strings[key]
    haskey(ti.aliases, key) && return getindex(ti, ti.aliases[key])
    throw(KeyError(key))
end

keys(ti::TermInfo) = keys(ti.flags) ∪ keys(ti.numbers) ∪ keys(ti.strings) ∪ keys(ti.aliases)

function show(io::IO, ::MIME"text/plain", ti::TermInfo)
    print(io, "TermInfo(", ti.names, "; ", length(ti.flags), " flags, ",
          length(ti.numbers), " numbers, ", length(ti.strings), " strings")
    !isnothing(ti.extensions) &&
        print(io, ", ", length(ti.extensions), " extended capabilities")
    print(io, ')')
end

"""
    find_terminfo_file(term::String)

Locate the terminfo file for `term`, return `nothing` if none could be found.

The lookup policy is described in `terminfo(5)` "Fetching Compiled
Descriptions". A terminfo database is included by default with Julia and is
taken to be the first entry of `@TERMINFO_DIRS@`.
"""
function find_terminfo_file(term::String)
    isempty(term) && return
    chr, chrcode = string(first(term)), string(Int(first(term)), base=16)
    terminfo_dirs = if haskey(ENV, "TERMINFO")
        [ENV["TERMINFO"]]
    elseif isdir(joinpath(homedir(), ".terminfo"))
        [joinpath(homedir(), ".terminfo")]
    else
        String[]
    end
    haskey(ENV, "TERMINFO_DIRS") &&
        append!(terminfo_dirs,
                replace(split(ENV["TERMINFO_DIRS"], ':'),
                        "" => "/usr/share/terminfo"))
    push!(terminfo_dirs, normpath(Sys.BINDIR, DATAROOTDIR, "julia", "terminfo"))
    Sys.isunix() &&
        push!(terminfo_dirs, "/etc/terminfo", "/lib/terminfo", "/usr/share/terminfo")
    for dir in terminfo_dirs
        if isfile(joinpath(dir, chr, term))
            return joinpath(dir, chr, term)
        elseif isfile(joinpath(dir, chrcode, term))
            return joinpath(dir, chrcode, term)
        elseif isfile(joinpath(dir, lowercase(chr), lowercase(term)))
            # The vendored terminfo database is fully lowercase to avoid issues on
            # case-sensitive filesystems. On Unix-like systems, terminfo files with
            # different cases are hard links to one another, so this is still
            # correct for non-vendored terminfo, just redundant.
            return joinpath(dir, lowercase(chr), lowercase(term))
        end
    end
    return nothing
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
const current_terminfo = OncePerProcess{TermInfo}() do
    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    terminfo = load_terminfo(term_env)
    # Ensure setaf is set for xterm terminals
    if !haskey(terminfo, :setaf) && startswith(term_env, "xterm")
        # For xterm-like terminals without setaf, add a reasonable default
        terminfo.strings[:setaf] = "\e[3%p1%dm"
    end
    return terminfo
end

# Legacy/TTY methods and the `:color` parameter

if Sys.iswindows()
    ttyhascolor() = true
else
    function ttyhascolor()
        haskey(current_terminfo(), :setaf)
    end
end

"""
    ttyhastruecolor()

Return a boolean signifying whether the current terminal supports 24-bit colors.

Multiple conditions are taken as signifying truecolor support, specifically any of the following:
- The `COLORTERM` environment variable is set to `"truecolor"` or `"24bit"`
- The current terminfo sets the [`RGB`[^1]
  capability](https://invisible-island.net/ncurses/man/user_caps.5.html#h3-Recognized-Capabilities)
  (or the legacy `Tc` capability[^2]) flag
- The current terminfo provides `setrgbf` and `setrgbb` strings[^3]
- The current terminfo has a `colors` number greater that `256`, on a unix system
- The VTE version is at least 3600 (detected via the `VTE_VERSION` environment variable)
- The current terminal has the `XTERM_VERSION` environment variable set
- The current terminal appears to be iTerm according to the `TERMINAL_PROGRAM` environment variable
- The `TERM` environment variable corresponds to: linuxvt, rxvt, or st

[^1]: Added to Ncurses 6.1, and used in `TERM=*-direct` terminfos.
[^2]: Convention [added to tmux in 2016](https://github.com/tmux/tmux/commit/427b8204268af5548d09b830e101c59daa095df9),
      superseded by `RGB`.
[^3]: Proposed by [Rüdiger Sonderfeld in 2013](https://lists.gnu.org/archive/html/bug-ncurses/2013-10/msg00007.html),
      adopted by a few terminal emulators.

!!! note
    The set of conditions is messy, because the situation is a mess, and there's
    no resolution in sight. `COLORTERM` is widely accepted, but an imperfect
    solution because only `TERM` is passed across `ssh` sessions. Terminfo is
    the obvious place for a terminal to declare capabilities, but it's taken
    enough years for ncurses/terminfo to declare a standard capability (`RGB`)
    that a number of other approaches have taken root. Furthermore, the official
    `RGB` capability is *incompatible* with 256-color operation, and so is
    unable to resolve the fragmentation in the terminal ecosystem.
"""
function ttyhastruecolor()
    # Lasciate ogne speranza, voi ch'intrate
    get(ENV, "COLORTERM", "") ∈ ("truecolor", "24bit") ||
        get(current_terminfo(), :RGB, false) || get(current_terminfo(), :Tc, false) ||
        (haskey(current_terminfo(), :setrgbf) && haskey(current_terminfo(), :setrgbb)) ||
        @static if Sys.isunix() get(current_terminfo(), :colors, 0) > 256 else false end ||
        (Sys.iswindows() && Sys.windows_version() ≥ v"10.0.14931") || # See <https://devblogs.microsoft.com/commandline/24-bit-color-in-the-windows-console/>
        something(tryparse(Int, get(ENV, "VTE_VERSION", "")), 0) >= 3600 || # Per GNOME bug #685759 <https://bugzilla.gnome.org/show_bug.cgi?id=685759>
        haskey(ENV, "XTERM_VERSION") ||
        get(ENV, "TERMINAL_PROGRAM", "") == "iTerm.app" || # Why does Apple need to be special?
        haskey(ENV, "KONSOLE_PROFILE_NAME") || # Per commentary in VT102Emulation.cpp
        haskey(ENV, "KONSOLE_DBUS_SESSION") ||
        let term = get(ENV, "TERM", "")
            startswith(term, "linux") || # Linux 4.8+ supports true-colour SGR.
                startswith(term, "rxvt") || # See <http://lists.schmorp.de/pipermail/rxvt-unicode/2016q2/002261.html>
                startswith(term, "st") # From experimentation
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
