module TOML

using Base: IdSet

# In case we do not have the Dates stdlib available
# we parse DateTime into these internal structs,
# note that these do not do any argument checking
struct Date
    year::Int
    month::Int
    day::Int
end
struct Time
    hour::Int
    minute::Int
    second::Int
    ms::Int
end
struct DateTime
    date::Date
    time::Time
end
DateTime(y, m, d, h, mi, s, ms) =
    DateTime(Date(y,m,d), Time(h, mi, s, ms))

const EOF_CHAR = typemax(Char)

const TOMLDict  = Dict{String, Any}

##########
# Parser #
##########

mutable struct Parser
    str::String
    # 1 character look ahead
    current_char::Char
    pos::Int
    # prevpos equals the startbyte of the look ahead character
    # prevpos-1 is therefore the end byte of the character we last ate
    prevpos::Int

    # File info
    column::Int
    line::Int

    # The function `take_substring` takes the substring from `marker` up
    # to `prevpos-1`.
    marker::Int

    # The current table that `key = value` entries are inserted into
    active_table::TOMLDict

    # As we parse dotted keys we store each part of the key in this cache
    # A future improvement would be to also store the spans of the keys
    # so that in error messages we could also show the previous key
    # definition in case of duplicated keys
    dotted_keys::Vector{String}

    # Strings in TOML can have line continuations ('\' as the last character
    # on a line. We store the byte ranges for each of these "chunks" in here
    chunks::Vector{UnitRange{Int}}

    # We need to keep track of those tables / arrays that are defined
    # inline since we are not allowed to add keys to those
    inline_tables::IdSet{TOMLDict}
    static_arrays::IdSet{Any}

    # [a.b.c.d] doesn't "define" the table [a]
    # so keys can later be added to [a], therefore
    # we need to keep track of what tables are
    # actually defined
    defined_tables::IdSet{TOMLDict}

    # The table we will finally return to the user
    root::TOMLDict

    # Filled in in case we are parsing a file to improve error messages
    filepath::Union{String, Nothing}

    # Get's populated with the Dates stdlib if it exists
    Dates::Union{Module, Nothing}
end

const DATES_PKGID = Base.PkgId(Base.UUID("ade2ca70-3891-5945-98fb-dc099432e06a"), "Dates")

function Parser(str::String; filepath=nothing)
    root = TOMLDict()
    l = Parser(
            str,                  # str
            EOF_CHAR,             # current_char
            firstindex(str),      # pos
            0,                    # prevpos
            0,                    # column
            1,                    # line
            0,                    # marker
            root,                 # active_table
            String[],             # dotted_keys
            UnitRange{Int}[],     # chunks
            IdSet{TOMLDict}(),    # inline_tables
            IdSet{Any}(),         # static_arrays
            IdSet{TOMLDict}(),    # defined_tables
            root,
            filepath,
            get(Base.loaded_modules, DATES_PKGID, nothing),
        )
    startup(l)
    return l
end
function startup(l::Parser)
    # Populate our one character look-ahead
    c = eat_char(l)
    # Skip BOM
    if c === '\ufeff'
        l.column -= 1
        eat_char(l)
    end
end

Parser() = Parser("")
Parser(io::IO) = Parser(read(io, String))

function reinit!(p::Parser, str::String; filepath::Union{Nothing, String}=nothing)
    p.str = str
    p.current_char = EOF_CHAR
    p.pos = firstindex(str)
    p.prevpos = 0
    p.column = 0
    p.line = 1
    p.marker = 0
    p.root = TOMLDict()
    p.active_table = p.root
    empty!(p.dotted_keys)
    empty!(p.chunks)
    empty!(p.inline_tables)
    empty!(p.static_arrays)
    empty!(p.defined_tables)
    p.filepath = filepath
    startup(p)
    return p
end

##########
# Errors #
##########

throw_internal_error(msg) = error("internal TOML parser error: $msg")

# Many functions return a ParserError. We want this to bubble up
# all the way and have this error be returned to the user
# if the parse is called with `raise=false`. This macro
# makes that easier
@eval macro $(:var"try")(expr)
    return quote
        v = $(esc(expr))
        v isa ParserError && return v
        v
    end
end

# TODO: Check all of these are used
@enum ErrorType begin

    # Toplevel #
    ############
    ErrRedefineTableArray
    ErrExpectedNewLineKeyValue
    ErrAddKeyToInlineTable
    ErrAddArrayToStaticArray
    ErrArrayTreatedAsDictionary
    ErrExpectedEndOfTable
    ErrExpectedEndArrayOfTable

    # Keys #
    ########
    ErrExpectedEqualAfterKey
    # Check, are these the same?
    ErrDuplicatedKey
    ErrKeyAlreadyHasValue
    ErrInvalidBareKeyCharacter
    ErrEmptyBareKey

    # Values #
    ##########
    ErrUnexpectedEofExpectedValue
    ErrUnexpectedStartOfValue
    ErrGenericValueError

    # Arrays
    ErrExpectedCommaBetweenItemsArray

    # Inline tables
    ErrExpectedCommaBetweenItemsInlineTable
    ErrTrailingCommaInlineTable

    # Numbers
    ErrUnderscoreNotSurroundedByDigits
    ErrLeadingZeroNotAllowedInteger
    ErrOverflowError
    ErrLeadingDot
    ErrNoTrailingDigitAfterDot
    ErrTrailingUnderscoreNumber

    # DateTime
    ErrParsingDateTime
    ErrOffsetDateNotSupported

    # Strings
    ErrNewLineInString
    ErrUnexpectedEndString
    ErrInvalidEscapeCharacter
    ErrInvalidUnicodeScalar
end

const err_message = Dict(
    ErrTrailingCommaInlineTable             => "trailing comma not allowed in inline table",
    ErrExpectedCommaBetweenItemsArray       => "expected comma between items in array",
    ErrExpectedCommaBetweenItemsInlineTable => "expected comma between items in inline table",
    ErrExpectedEndArrayOfTable              => "expected array of table to end with ']]'",
    ErrInvalidBareKeyCharacter              => "invalid bare key character",
    ErrRedefineTableArray                   => "tried to redefine an existing table as an array",
    ErrDuplicatedKey                        => "key already defined",
    ErrKeyAlreadyHasValue                   => "key already has a value",
    ErrEmptyBareKey                         => "bare key cannot be empty",
    ErrExpectedNewLineKeyValue              => "expected newline after key value pair",
    ErrNewLineInString                      => "newline character in single quoted string",
    ErrUnexpectedEndString                  => "string literal ened unexpectedly",
    ErrExpectedEndOfTable                   => "expected end of table ']'",
    ErrAddKeyToInlineTable                  => "tried to add a new key to an inline table",
    ErrArrayTreatedAsDictionary             => "tried to add a key to an array",
    ErrAddArrayToStaticArray                => "tried to append to a statically defined array",
    ErrGenericValueError                    => "failed to parse value",
    ErrLeadingZeroNotAllowedInteger         => "leading zero in integer not allowed",
    ErrUnderscoreNotSurroundedByDigits      => "underscore is not surrounded by digits",
    ErrUnexpectedStartOfValue               => "unexpected start of value",
    ErrOffsetDateNotSupported               => "offset date-time is not supported",
    ErrParsingDateTime                      => "parsing date/time value failed",
    ErrTrailingUnderscoreNumber             => "trailing underscore in number",
    ErrLeadingDot                           => "floats require a leading zero",
    ErrExpectedEqualAfterKey                => "expected equal sign after key",
    ErrNoTrailingDigitAfterDot              => "expected digit after dot",
    ErrOverflowError                        => "overflowed when parsing integer",
    ErrInvalidUnicodeScalar                 => "invalid uncidode scalar",
    ErrInvalidEscapeCharacter               => "invalid escape character",
    ErrUnexpectedEofExpectedValue           => "unexpected end of file, expected a value"
)

for err in instances(ErrorType)
    @assert haskey(err_message, err) "$err does not have an error message"
end

mutable struct ParserError <: Exception
    type::ErrorType

    # Arbitrary data to store at the
    # call site to be used when formatting
    # the error
    data

    # These are filled in before returning from parse function
    str       ::Union{String,   Nothing}
    filepath  ::Union{String,   Nothing}
    line      ::Union{Int,      Nothing}
    column    ::Union{Int,      Nothing}
    pos       ::Union{Int,      Nothing} # position of parser when
    table     ::Union{TOMLDict, Nothing} # result parsed until error
end
ParserError(type, data) = ParserError(type, data, nothing, nothing, nothing, nothing, nothing, nothing)
ParserError(type) = ParserError(type, nothing)
# Defining these below can be useful when debugging code that erroneously returns a
# ParserError because you get a stacktrace to where the ParserError was created
#ParserError(type) = error(type)
#ParserError(type, data) = error(type,data)

# Many functions return either a T or a ParserError
const Err{T} = Union{T, ParserError}

function format_error_message_for_err_type(error::ParserError)
    msg = err_message[error.type]
    if error.type == ErrInvalidBareKeyCharacter
        c_escaped = escape_string(string(error.data))
        msg *= ": '$c_escaped'"
    end
    return msg
end

# This is used in error formatting, for example,
# point_to_line("aa\nfoobar\n\bb", 4, 6) would return the strings:
# str1 = "foobar"
# str2 = "^^^"
# used to show the interval where an error happened
# Right now, it is only called with a == b
function point_to_line(str::AbstractString, a::Int, b::Int, context)
    @assert b >= a
    a = thisind(str, a)
    b = thisind(str, b)
    pos = something(findprev('\n', str, prevind(str, a)), 0) + 1
    io1 = IOContext(IOBuffer(), context)
    io2 = IOContext(IOBuffer(), context)
    while true
        if a <= pos <= b
            printstyled(io2, "^"; color=:light_green)
        else
            print(io2, " ")
        end
        it = iterate(str, pos)
        it === nothing && break
        c, pos = it
        c == '\n' && break
        print(io1, c)
    end
    return String(take!(io1.io)), String(take!(io2.io))
end

function Base.showerror(io::IO, err::ParserError)
    printstyled(io, "TOML Parser error:\n"; color=Base.error_color())
    f = something(err.filepath, "none")
    printstyled(io, f, ':', err.line, ':', err.column; bold=true)
    printstyled(io, " error: "; color=Base.error_color())
    println(io, format_error_message_for_err_type(err))
    # In this case we want the arrow to point one character
    pos = err.pos
    err.type == ErrUnexpectedEofExpectedValue && (pos += 1)
    str1, err1 = point_to_line(err.str, pos, pos, io)
    @static if VERSION <= v"1.6.0-DEV.121"
        # See https://github.com/JuliaLang/julia/issues/36015
        format_fixer = get(io, :color, false) == true ? "\e[0m" : ""
        println(io, "$format_fixer  ", str1)
        print(io, "$format_fixer  ", err1)
    else
        println(io, "  ", str1)
        print(io, "  ", err1)
    end
end


################
# Parser utils #
################

@inline function next_char(l::Parser)::Char
    state = iterate(l.str, l.pos)
    l.prevpos = l.pos
    l.column += 1
    state === nothing && return EOF_CHAR
    c, pos = state
    l.pos = pos
    if c == '\n'
        l.line += 1
        l.column = 0
    end
    return c
end

@inline function eat_char(l::Parser)::Char
    c = l.current_char
    l.current_char = next_char(l)
    return c
end

@inline peek(l::Parser) = l.current_char

# Return true if the character was accepted. When a character
# is accepted it get's eaten and we move to the next character
@inline function accept(l::Parser, f::Union{Function, Char})::Bool
    c = peek(l)
    c == EOF_CHAR && return false
    ok = false
    if isa(f, Function)
        ok = f(c)
    elseif isa(f, Char)
        ok = c === f
    end
    ok && eat_char(l)
    return ok
end

# Return true if any character was accepted
function accept_batch(l::Parser, f::F)::Bool where {F}
    ok = false
    while accept(l, f)
        ok = true
    end
    return ok
end

# Return true if `f` was accepted `n` times
@inline function accept_n(l::Parser, n, f::F)::Bool where {F}
    for i in 1:n
        if !accept(l, f)
            return false
        end
    end
    return true
end

@inline iswhitespace(c::Char) = c == ' ' || c == '\t'
@inline isnewline(c::Char) = c == '\n' || c == '\r'

skip_ws(l::Parser) = accept_batch(l, iswhitespace)

skip_ws_nl_no_comment(l::Parser)::Bool = accept_batch(l, x -> iswhitespace(x) || isnewline(x))

function skip_ws_nl(l::Parser)::Bool
    skipped = false
    while true
        skipped_ws = accept_batch(l, x -> iswhitespace(x) || isnewline(x))
        skipped_comment = skip_comment(l)
        if !skipped_ws && !skipped_comment
            break
        end
        skipped = true
    end
    return skipped
end

# Returns true if a comment was skipped
function skip_comment(l::Parser)::Bool
    found_comment = accept(l, '#')
    if found_comment
        accept_batch(l, !isnewline)
    end
    return found_comment
end

skip_ws_comment(l::Parser) = skip_ws(l) && skip_comment(l)

@inline set_marker!(l::Parser) = l.marker = l.prevpos
take_substring(l::Parser) = SubString(l.str, l.marker:(l.prevpos-1))

############
# Toplevel #
############

# Driver, keeps parsing toplevel until we either get
# a `ParserError` or eof.
function parse(l::Parser)::TOMLDict
    v = tryparse(l)
    v isa ParserError && throw(v)
    return v
end

function tryparse(l::Parser)::Err{TOMLDict}
    while true
        skip_ws_nl(l)
        peek(l) == EOF_CHAR && break
        v = parse_toplevel(l)
        if v isa ParserError
            v.str      = l.str
            v.pos      = l.prevpos-1
            v.table    = l.root
            v.filepath = l.filepath
            v.line     = l.line
            v.column   = l.column-1
            return v
        end
    end
    return l.root
end

# Top level can be either a table key, an array of table statement
# or a key/value entry.
function parse_toplevel(l::Parser)::Err{Nothing}
    if accept(l, '[')
        l.active_table = l.root
        @try parse_table(l)
        skip_ws_comment(l)
        if !(peek(l) == '\n' || peek(l) == '\r' || peek(l) == EOF_CHAR)
            eat_char(l)
            return ParserError(ErrExpectedNewLineKeyValue)
        end
    else
        @try parse_entry(l, l.active_table)
        skip_ws_comment(l)
        # SPEC: "There must be a newline (or EOF) after a key/value pair."
        if !(peek(l) == '\n' || peek(l) == '\r' || peek(l) == EOF_CHAR)
            c = eat_char(l)
            return ParserError(ErrExpectedNewLineKeyValue)
        end
    end
end

function recurse_dict!(l::Parser, d::Dict, dotted_keys::AbstractVector{String}, check=true)::Err{TOMLDict}
    for i in 1:length(dotted_keys)
        key = dotted_keys[i]
        d = get!(TOMLDict, d, key)
        if d isa Vector
            d = d[end]
        end
        check && @try check_allowed_add_key(l, d, i == length(dotted_keys))
    end
    return d
end

function check_allowed_add_key(l::Parser, d, check_defined=true)::Err{Nothing}
    if !(d isa Dict)
        return ParserError(ErrKeyAlreadyHasValue)
    elseif d isa Dict && d in l.inline_tables
        return ParserError(ErrAddKeyToInlineTable)
    elseif check_defined && d in l.defined_tables
        return ParserError(ErrDuplicatedKey)
    end
    return nothing
end

# Can only enter here from toplevel
function parse_table(l)
    if accept(l, '[')
        return parse_array_table(l)
    end
    table_key = @try parse_key(l)
    skip_ws(l)
    if !accept(l, ']')
        return ParserError(ErrExpectedEndOfTable)
    end
    l.active_table = @try recurse_dict!(l, l.root, table_key)
    push!(l.defined_tables, l.active_table)
    return
end

function parse_array_table(l)::Union{Nothing, ParserError}
    table_key = @try parse_key(l)
    skip_ws(l)
    if !(accept(l, ']') && accept(l, ']'))
        return ParserError(ErrExpectedEndArrayOfTable)
    end
    d = @try recurse_dict!(l, l.root, @view(table_key[1:end-1]), false)
    k = table_key[end]
    old = get!(() -> [], d, k)
    if old isa Vector
        if old in l.static_arrays
            return ParserError(ErrAddArrayToStaticArray)
        end
    else
        return ParserError(ErrArrayTreatedAsDictionary)
    end
    d_new = TOMLDict()
    push!(old, d_new)
    push!(l.defined_tables, d_new)
    l.active_table = d_new

    return
end

function parse_entry(l::Parser, d)::Union{Nothing, ParserError}
    key = @try parse_key(l)
    skip_ws(l)
    if !accept(l, '=')
        return ParserError(ErrExpectedEqualAfterKey)
    end
    if length(key) > 1
        d = @try recurse_dict!(l, d, @view(key[1:end-1]))
    end
    last_key_part = l.dotted_keys[end]

    v = get(d, last_key_part, nothing)
    if v !== nothing
        @try check_allowed_add_key(l, v)
    end

    skip_ws(l)
    value = @try parse_value(l)
    # TODO: Performance, hashing `last_key_part` again here
    d[last_key_part] = value
    return
end


########
# Keys #
########

# SPEC: "Bare keys may only contain ASCII letters, ASCII digits, underscores,
# and dashes (A-Za-z0-9_-).
# Note that bare keys are allowed to be composed of only ASCII digits, e.g. 1234,
# but are always interpreted as strings."
@inline isvalid_barekey_char(c::Char) =
    'a' <= c <= 'z' ||
    'A' <= c <= 'Z' ||
    isdigit(c) ||
    c == '-' || c == '_'

# Current key...
function parse_key(l::Parser)
    empty!(l.dotted_keys)
    _parse_key(l)
end

# Recursively add dotted keys to `l.dotted_key`
function _parse_key(l::Parser)
    skip_ws(l)
    # SPEC: "A bare key must be non-empty,"
    if isempty(l.dotted_keys) && accept(l, '=')
        return ParserError(ErrEmptyBareKey)
    end
    keyval = if accept(l, '"')
        @try parse_string_start(l, false)
    elseif accept(l, '\'')
        @try parse_string_start(l, true)
    else
        set_marker!(l)
        if accept_batch(l, isvalid_barekey_char)
            if !(peek(l) == '.' || peek(l) == ' ' || peek(l) == ']' || peek(l) == '=')
                c = eat_char(l)
                return ParserError(ErrInvalidBareKeyCharacter, c)
            end
            String(take_substring(l))
        else
            c = eat_char(l)
            return ParserError(ErrInvalidBareKeyCharacter, c)
        end
    end
    new_key = keyval
    push!(l.dotted_keys, new_key)
    # SPEC: "Whitespace around dot-separated parts is ignored."
    skip_ws(l)
    if accept(l, '.')
        skip_ws(l)
        @try _parse_key(l)
    end
    return l.dotted_keys
end


##########
# Values #
##########

function parse_value(l::Parser)
    val = if accept(l, '[')
        parse_array(l)
    elseif accept(l, '{')
        parse_inline_table(l)
    elseif accept(l, '"')
        parse_string_start(l, false)
    elseif accept(l, '\'')
        parse_string_start(l, true)
    elseif accept(l, 't')
        parse_bool(l, true)
    elseif accept(l, 'f')
        parse_bool(l, false)
    else
        parse_number_or_date_start(l)
    end
    if val === nothing
        return ParserError(ErrGenericValueError)
    end
    return val
end


#########
# Array #
#########

function push!!(v::Vector, el)
    T = eltype(v)
    if el isa T || typeof(el) === T
        push!(v, el::T)
        return v
    else
        if typeof(T) === Union
            newT = Base.typejoin(T, typeof(el))
        else
            newT = Union{T, typeof(el)}
        end
        new = Array{newT}(undef, length(v))
        copy!(new, v)
        return push!!(new, el)
    end
end

function parse_array(l::Parser)::Err{Vector}
    skip_ws_nl(l)
    array = Union{}[]
    empty_array = accept(l, ']')
    while !empty_array
        v = @try parse_value(l)
        # TODO: Worth to function barrier this?
        array = push!!(array, v)
        # There can be an arbitrary number of newlines and comments before a value and before the closing bracket.
        skip_ws_nl(l)
        comma = accept(l, ',')
        skip_ws_nl(l)
        accept(l, ']') && break
        if !comma
            return ParserError(ErrExpectedCommaBetweenItemsArray)
        end
    end
    push!(l.static_arrays, array)
    return array
end


################
# Inline table #
################

function parse_inline_table(l::Parser)::Err{TOMLDict}
    dict = TOMLDict()
    push!(l.inline_tables, dict)
    skip_ws(l)
    accept(l, '}') && return dict
    while true
        @try parse_entry(l, dict)
        # SPEC: No newlines are allowed between the curly braces unless they are valid within a value.
        skip_ws(l)
        accept(l, '}') && return dict
        if accept(l, ',')
            skip_ws(l)
            if accept(l, '}')
                return ParserError(ErrTrailingCommaInlineTable)
            end
        else
            return ParserError(ErrExpectedCommaBetweenItemsInlineTable)
        end
    end
end


###########
# Numbers #
###########

parse_inf(l::Parser, sgn::Int) = accept(l, 'n') && accept(l, 'f') ? sgn * Inf : nothing
parse_nan(l::Parser) = accept(l, 'a') && accept(l, 'n') ? NaN : nothing

function parse_bool(l::Parser, v::Bool)::Union{Bool, Nothing}
    # Have eaten a 't' if `v` is true, otherwise have eaten a `f`.
    v ? (accept(l, 'r') && accept(l, 'u') && accept(l, 'e') && return true) :
        (accept(l, 'a') && accept(l, 'l') && accept(l, 's') && accept(l, 'e') && return false)
    return nothing
end

isvalid_hex(c::Char) = isdigit(c) || ('a' <= c <= 'f') || ('A' <= c <= 'F')
isvalid_oct(c::Char) = '0' <= c <= '7'
isvalid_binary(c::Char) = '0' <= c <= '1'

const ValidSigs = Union{typeof.([isvalid_hex, isvalid_oct, isvalid_binary, isdigit])...}
# This function eats things accepted by `f` but also allows eating `_` in between
# digits. Retruns if it ate at lest one character and if it ate an underscore
function accept_batch_underscore(l::Parser, f::ValidSigs, fail_if_underscore=true)::Err{Tuple{Bool, Bool}}
    contains_underscore = false
    at_least_one = false
    last_underscore = false
    while true
        c = peek(l)
        if c == '_'
            contains_underscore = true
            if fail_if_underscore
                return ParserError(ErrUnderscoreNotSurroundedByDigits)
            end
            eat_char(l)
            fail_if_underscore = true
            last_underscore = true
        else
            # SPEC:  "Each underscore must be surrounded by at least one digit on each side."
            fail_if_underscore = false
            if f(c)
                at_least_one = true
                eat_char(l)
            else
                if last_underscore
                    return ParserError(ErrTrailingUnderscoreNumber)
                end
                return at_least_one, contains_underscore
            end
            last_underscore = false
        end
    end
end

function parse_number_or_date_start(l::Parser)
    integer = true
    read_dot = false

    set_marker!(l)
    sgn = 1
    if accept(l, '+')
        # do nothing
    elseif accept(l, '-')
        sgn = -1
    end
    if accept(l, 'i')
        return parse_inf(l, sgn)
    elseif accept(l, 'n')
        return parse_nan(l)
    end

    if accept(l, '.')
        return ParserError(ErrLeadingDot)
    end

    # Zero is allowed to follow by a end value char, a base x, o, b or a dot
    readed_zero = false
    if accept(l, '0')
        readed_zero = true # Intentional bad grammar to remove the ambiguity in "read"...
        if ok_end_value(peek(l))
            return Int64(0)
        elseif accept(l, 'x')
            ate, contains_underscore = @try accept_batch_underscore(l, isvalid_hex)
            ate && return parse_int(l, contains_underscore)
        elseif accept(l, 'o')
            ate, contains_underscore = @try accept_batch_underscore(l, isvalid_oct)
            ate && return parse_int(l, contains_underscore)
        elseif accept(l, 'b')
            ate, contains_underscore = @try accept_batch_underscore(l, isvalid_binary)
            ate && return parse_int(l, contains_underscore)
        elseif accept(l, isdigit)
            return parse_local_time(l)
        elseif peek(l) !== '.'
            return ParserError(ErrLeadingZeroNotAllowedInteger)
        end
    end

    read_underscore = false
    read_digit = accept(l, isdigit)
    if !readed_zero && !read_digit
        if peek(l) == EOF_CHAR
            return ParserError(ErrUnexpectedEofExpectedValue)
        else
            return ParserError(ErrUnexpectedStartOfValue)
        end
    end
    ate, contains_underscore = @try accept_batch_underscore(l, isdigit, readed_zero)
    read_underscore |= contains_underscore
    if (read_digit || ate) && ok_end_value(peek(l))
        return parse_int(l, contains_underscore)
    end
    # Done with integers here

    if !read_underscore
        # No underscores in date / times
        if peek(l) == '-'
            return parse_datetime(l)
        elseif peek(l) == ':'
            return parse_local_time(l)
        end
    end
    # Done with datetime / localtime here

    # can optionally read a . + digits and then exponent
    ate_dot = accept(l, '.')
    ate, contains_underscore = @try accept_batch_underscore(l, isdigit, true)
    if ate_dot && !ate
        return ParserError(ErrNoTrailingDigitAfterDot)
    end
    read_underscore |= contains_underscore
    if accept(l, x -> x == 'e' || x == 'E')
        accept(l, x-> x == '+' || x == '-')
        # SPEC: (which follows the same rules as decimal integer values but may include leading zeros)
        read_digit = accept_batch(l, isdigit)
        ate, read_underscore = @try accept_batch_underscore(l, isdigit, !read_digit)
        contains_underscore |= read_underscore
    end
    if !ok_end_value(peek(l))
        eat_char(l)
        return ParserError(ErrGenericValueError)
    end
    return parse_float(l, read_underscore)
end


function take_string_or_substring(l, contains_underscore)::SubString
    subs = take_substring(l)
    # Need to pass a AbstractString to `parse` so materialize it in case it
    # contains underscore.
    return contains_underscore ? SubString(filter(!=('_'), subs)) : subs
end

function parse_float(l::Parser, contains_underscore)::Err{Float64}
    s = take_string_or_substring(l, contains_underscore)
    v = Base.tryparse(Float64, s)
    v === nothing && return(ParserError(ErrGenericValueError))
    return v
end

function parse_int(l::Parser, contains_underscore, base=nothing)::Err{Int64}
    s = take_string_or_substring(l, contains_underscore)
    v = try
        Base.parse(Int64, s; base=base)
    catch e
        e isa Base.OverflowError && return(ParserError(ErrOverflowError))
        error("internal parser error: did not correctly discredit $(repr(s)) as an int")
    end
    return v
end


##########################
# Date / Time / DateTime #
##########################

ok_end_value(c::Char) = iswhitespace(c) || c == '#' || c == EOF_CHAR || c == ']' ||
                               c == '}' || c == ',' || c == '\n'     || c == '\r'

#=
# https://tools.ietf.org/html/rfc3339

# Internet Protocols MUST generate four digit years in dates.

   date-fullyear   = 4DIGIT
   date-month      = 2DIGIT  ; 01-12
   date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
                             ; month/year
   time-hour       = 2DIGIT  ; 00-23
   time-minute     = 2DIGIT  ; 00-59
   time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second
                             ; rules
   time-secfrac    = "." 1*DIGIT
   time-numoffset  = ("+" / "-") time-hour ":" time-minute
   time-offset     = "Z" / time-numoffset

   partial-time    = time-hour ":" time-minute ":" time-second
                     [time-secfrac]
   full-date       = date-fullyear "-" date-month "-" date-mday
   full-time       = partial-time time-offset

   date-time       = full-date "T" full-time
=#

accept_two(l, f::F) where {F} = accept_n(l, 2, f) || return(ParserError(ErrParsingDateTime))
function parse_datetime(l)
    # Year has already been eaten when we reach here
    year = parse_int(l, false)::Int64
    year in 0:9999 || return ParserError(ErrParsingDateTime)

    # Month
    accept(l, '-') || return ParserError(ErrParsingDateTime)
    set_marker!(l)
    @try accept_two(l, isdigit)
    month = parse_int(l, false)
    month in 1:12 || return ParserError(ErrParsingDateTime)
    accept(l, '-') || return ParserError(ErrParsingDateTime)

    # Day
    set_marker!(l)
    @try accept_two(l, isdigit)
    day = parse_int(l, false)
    # Verify the real range in the constructor below
    day in 1:31 || return ParserError(ErrParsingDateTime)

    # We might have a local date now
    read_space = false
    if ok_end_value(peek(l))
        if (read_space = accept(l, ' '))
            if !isdigit(peek(l))
                return try_return_date(l, year, month, day)
            end
        else
            return try_return_date(l, year, month, day)
        end
    end
    if !read_space
        accept(l, 'T') || accept(l, 't') || return ParserError(ErrParsingDateTime)
    end

    h, m, s, ms = @try _parse_local_time(l)

    # Julia doesn't support offset times
    if !accept(l, 'Z')
        if accept(l, '+') || accept(l, '-')
            return ParserError(ErrOffsetDateNotSupported)
        end
    end

    if !ok_end_value(peek(l))
        return ParserError(ErrParsingDateTime)
    end

    # The DateTime parser verifies things like leap year for us
    return try_return_datetime(l, year, month, day, h, m, s, ms)
end

function try_return_datetime(p, year, month, day, h, m, s, ms)
    if p.Dates !== nothing
        try
            return p.Dates.DateTime(year, month, day, h, m, s, ms)
        catch
            return ParserError(ErrParsingDateTime)
        end
    else
        return DateTime(year, month, day, h, m, s, ms)
    end
end

function try_return_date(p, year, month, day)
    if p.Dates !== nothing
        try
            return p.Dates.Date(year, month, day)
        catch
            return ParserError(ErrParsingDateTime)
        end
    else
        return Date(year, month, day)
    end
end

function parse_local_time(l::Parser)
    h = parse_int(l, false)
    h in 0:23 || return ParserError(ErrParsingDateTime)
    _, m, s, ms = @try _parse_local_time(l, true)
    # TODO: Could potentially parse greater accuracy for the
    # fractional seconds here.
    return try_return_time(l, h, m, s, ms)
end

function try_return_time(p, h, m, s, ms)
    if p.Dates !== nothing
        try
            return p.Dates.Time(h, m, s, ms)
        catch
            return ParserError(ErrParsingDateTime)
        end
    else
        return Time(h, m, s, ms)
    end
end

function _parse_local_time(l::Parser, skip_hour=false)::Err{NTuple{4, Int64}}
    # Hour has potentially been already parsed in
    # `parse_number_or_date_start` already
    if skip_hour
        hour = Int64(0)
    else
        set_marker!(l)
        @try accept_two(l, isdigit)
        hour = parse_int(l, false)
        hour in 0:23 || return ParserError(ErrParsingDateTime)
    end

    accept(l, ':') || return ParserError(ErrParsingDateTime)

    # minute
    set_marker!(l)
    @try accept_two(l, isdigit)
    minute = parse_int(l, false)
    minute in 0:59 || return ParserError(ErrParsingDateTime)

    accept(l, ':') || return ParserError(ErrParsingDateTime)

    # second
    set_marker!(l)
    @try accept_two(l, isdigit)
    second = parse_int(l, false)
    second in 0:59 || return ParserError(ErrParsingDateTime)

    # optional fractional second
    fractional_second = Int64(0)
    if accept(l, '.')
        set_marker!(l)
        found_fractional_digit = false
        for i in 1:3
            found_fractional_digit |= accept(l, isdigit)
        end
        if !found_fractional_digit
            return ParserError(ErrParsingDateTime)
        end
        # DateTime in base only manages 3 significant digits in fractional
        # second
        fractional_second = parse_int(l, false)
        # Truncate off the rest eventual digits
        accept_batch(l, isdigit)
    end
    return hour, minute, second, fractional_second
end


##########
# String #
##########

function parse_string_start(l::Parser, quoted::Bool)::Err{String}
    # Have eaten a `'` if `quoted` is true, otherwise have eaten a `"`
    multiline = false
    c = quoted ? '\'' : '"'
    if accept(l, c) # Eat second quote
        if !accept(l, c)
            return ""
        end
        accept(l, '\r') # Eat third quote
        accept(l, '\n') # Eat third quote
        multiline = true
    end
    return parse_string_continue(l, multiline, quoted)
end

@inline stop_candidates_multiline(x)         = x != '"'  &&  x != '\\'
@inline stop_candidates_singleline(x)        = x != '"'  &&  x != '\\' && x != '\n'
@inline stop_candidates_multiline_quoted(x)  = x != '\'' &&  x != '\\'
@inline stop_candidates_singleline_quoted(x) = x != '\'' &&  x != '\\' && x != '\n'

function parse_string_continue(l::Parser, multiline::Bool, quoted::Bool)::Err{String}
    start_chunk = l.prevpos
    q = quoted ? '\'' : '"'
    contains_backslash = false
    offset = multiline ? 3 : 1
    while true
        if peek(l) == EOF_CHAR
            return ParserError(ErrUnexpectedEndString)
        end
        if quoted
            accept_batch(l, multiline ? stop_candidates_multiline_quoted : stop_candidates_singleline_quoted)
        else
            accept_batch(l, multiline ? stop_candidates_multiline : stop_candidates_singleline)
        end
        if !multiline && peek(l) == '\n'
            return ParserError(ErrNewLineInString)
        end
        next_slash = peek(l) == '\\'
        if !next_slash
            # TODO: Doesn't handle values with e.g. format `""""str""""`
            if accept(l, q) && (!multiline || (accept(l, q) && accept(l, q)))
                push!(l.chunks, start_chunk:(l.prevpos-offset-1))
                return take_chunks(l, contains_backslash)
            end
        end
        c = eat_char(l) # eat the character we stopped at
        next_slash = c == '\\'
        if next_slash && !quoted
            if peek(l) == '\n' || peek(l) == '\r'
                push!(l.chunks, start_chunk:(l.prevpos-1-1)) # -1 due to eating the slash
                skip_ws_nl_no_comment(l)
                start_chunk = l.prevpos
            else
                c = eat_char(l) # eat the escaped character
                if c == 'u'  || c == 'U'
                    n = c == 'u' ? 4 : 6
                    set_marker!(l)
                    if !accept_n(l, n, isvalid_hex)
                        return ParserError(ErrInvalidUnicodeScalar)
                    end
                    codepoint = parse_int(l, false, 16)
                    #=
                    Unicode Scalar Value
                    ---------------------
                    Any Unicode code point except high-surrogate and
                    low-surrogate code points.  In other words, the ranges of
                    integers 0 to D7FF16 and E00016 to 10FFFF16 inclusive.
                    =#
                    if !(codepoint <= 0xD7FF || 0xE000 <= codepoint <= 0x10FFFF)
                        return ParserError(ErrInvalidUnicodeScalar)
                    end
                elseif c != 'b' && c != 't' && c != 'n' && c != 'f' && c != 'r' && c != '"' && c!= '\\'
                    return ParserError(ErrInvalidEscapeCharacter)
                end
                contains_backslash = true
            end
        end
    end
end

function take_chunks(l::Parser, unescape::Bool)::String
    nbytes = sum(length, l.chunks)
    str = Base._string_n(nbytes)
    offset = 1
    for chunk in l.chunks
        # The SubString constructor takes as an index the first byte of the
        # last character but we have the last byte.
        n = length(chunk)
        GC.@preserve str begin
            unsafe_copyto!(pointer(str, offset), pointer(l.str, first(chunk)), n)
        end
        offset += n
    end
    empty!(l.chunks)
    return unescape ? unescape_string(str) : str
end

end
