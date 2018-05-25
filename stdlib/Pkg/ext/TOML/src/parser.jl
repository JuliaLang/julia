"TOML Table"
struct Table
    values::Dict{String,Any}
    defined::Bool
end

Table(defined::Bool) = Table(Dict{String,Any}(), defined)
function Base.show(io::IO, tbl::Table, level::Int=1)
    Base.print(io, "T($(tbl.defined)){\n")
    for (k,v) in tbl.values
        Base.print(io, "\t"^level, k, " => ")
        if isa(v, Table)
            Base.show(io, v, level+1)
            Base.print(io, "\n")
        elseif isa(v, Vector{Table})
            Base.print(io, "[\n")
            for i in 1:length(v)
                Base.print(io, "\t"^level, "  ")
                Base.show(io, v[i], level+1)
                i != length(v) && Base.print(io, ",\n")
            end
            Base.print("\t"^level, "]\n")
        else
            Base.print(io, " $v\n")
        end
    end
    Base.print(io, "\t"^(level-1), "}\n")
end
Base.getindex(tbl::Table, key::AbstractString) = tbl.values[key]
Base.haskey(tbl::Table, key::AbstractString) = haskey(tbl.values ,key)

"Parser error exception"
struct ParserError <: Exception
    lo::Int
    hi::Int
    msg::String
end

"TOML Parser"
mutable struct Parser{IO_T <: IO}
    input::IO_T
    errors::Vector{ParserError}
    charbuffer::Base.GenericIOBuffer{Array{UInt8,1}}
    currentchar::Char

    Parser(input::IO_T) where {IO_T <: IO}  = new{IO_T}(input, ParserError[], IOBuffer(), ' ')
end
Parser(input::String) = Parser(IOBuffer(input))
Base.error(p::Parser, l, h, msg) = push!(p.errors, ParserError(l, h, msg))
Base.eof(p::Parser) = eof(p.input)
Base.position(p::Parser) = Int(position(p.input))+1
Base.write(p::Parser, x) = write(p.charbuffer, x)
Base.read(p::Parser) = p.currentchar = read(p.input, Char)

NONE() = nothing
NONE(::Type{T}) where {T} = nothing
SOME(v::T) where {T} = v
NONE(::Type{String}, p::Parser) = (take!(p.charbuffer); nothing)
SOME(::Type{String}, p::Parser) = String(take!(p.charbuffer))
isnull(x) = x === nothing
get(x) = (@assert !isnull(x); x)

"Rewind parser input on `n` characters."
function rewind(p::Parser, n=1)
    pos = position(p.input)
    pos == 0 && return 0
    skip(p.input, -n)
    return Int(position(p.input))
end

"Converts an offset to a line and a column in the original source."
function linecol(p::Parser, offset::Int)
    pos = position(p)
    seekstart(p.input)
    line = 0
    cur = 1
    for (i,l) in enumerate(eachline(p.input, chomp=false))
        if cur + length(l) > offset
            return (i, offset - cur + 1)
        end
        cur += length(l)
        line+=1
    end
    seek(p.input, pos)
    return (line, 0)
end

"Determine next position in parser input"
function nextpos(p::Parser)
    pos = position(p)
    return pos + 1
end

"Peeks ahead `n` characters"
function peek(p::Parser) #, i::Int=0
    eof(p) && return nothing
    res = Base.peek(p.input)
    res == -1 && return nothing
    return Char(res)
end

"Returns `true` and consumes the next character if it matches `ch`, otherwise do nothing and return `false`"
function consume(p::Parser, ch::AbstractChar)
    eof(p) && return false
    c = peek(p)
    if get(c) == ch
        read(p)
        return true
    else
        return false
    end
end

function expect(p::Parser, ch::AbstractChar)
    consume(p, ch) && return true
    lo = position(p)
    if eof(p)
        error(p, lo, lo, "expected `$ch`, but found EOF")
    else
        v = peek(p)
        mark(p.input)
        c = read(p)
        error(p, lo, lo+1, "expected `$ch`, but found `$c`")
        reset(p.input)
    end
    return false
end

"Consumes whitespace ('\t' and ' ') until another character (or EOF) is reached. Returns `true` if any whitespace was consumed"
function whitespace(p::Parser)
    ret = false
    while !eof(p)
        c = read(p)
        if c == '\t' || c == ' '
            ret = true
        else
            rewind(p)
            break
        end
    end
    return ret
end

"Consumes the rest of the line after a comment character"
function comment(p::Parser)
    !consume(p, '#') && return false
    while !eof(p)
        ch = read(p)
        ch == '\n' && break
    end
    return true
end

"Consumes a newline if one is next"
function newline(p::Parser)
    if !eof(p)
        n = 1
        ch = read(p.input, UInt8)
        ch == 0x0a && return true
        if ch == 0x0d
            if !eof(p)
                nch = read(p.input, UInt8)
                nch == 0x0a  && return true
                n+=1
            end
        end
        rewind(p, n)
    end
    return false
end

"Consume ignored symbols"
function ignore(p::Parser)
    while true
        whitespace(p)
        !newline(p) && !comment(p) && break
    end
end

"Parse a single key name starting at next position"
function keyname(p)
    s = nextpos(p)
    key = if consume(p, '"')
        basicstring(p, s, false)
    elseif consume(p, '\'')
        literalstring(p, s, false)
    else
        while !eof(p)
            ch = read(p)
            if  'a' <= ch <= 'z' ||
                'A' <= ch <= 'Z' ||
                isdigit(ch)      ||
                ch == '_'        ||
                ch == '-'
                write(p, ch)
            else
                rewind(p)
                break
            end
        end
        SOME(String, p)
    end

    if !isnull(key) && isempty(get(key))
        error(p, s, s, "expected a key but found an empty string")
        key = NONE(String, p)
    end
    return key
end

"Parse a path into a vector of paths"
function lookup(p::Parser)
    ks = String[]
    eof(p) && return SOME(ks)
    while true
        whitespace(p)
        s = keyname(p)
        if !isnull(s)
            push!(ks, get(s))
        else
            s = integer(p, 0)
            if !isnull(s)
                push!(ks, get(s))
            else
                return NONE()
            end
        end
        whitespace(p)
        !expect(p, '.') && return SOME(ks)
    end
end

"Parses a key-value separator"
function separator(p::Parser)
    whitespace(p)
    !expect(p, '=') && return false
    whitespace(p)
    return true
end

"Insert key-value pair to table, if duplicate key found reports error"
function insertpair(p::Parser, tbl::Table, k, v, st)
    if haskey(tbl, k)
        error(p, st, st+length(k), "duplicate key `$k`")
    else
        tbl.values[k] = v
    end
end

"Parses integer with leading zeros and sign"
function integer(p::Parser, st::Int, allow_leading_zeros::Bool=false, allow_sign::Bool=false)
    if allow_sign
        if consume(p, '-')
            write(p, '-')
        elseif consume(p, '+')
            write(p, '+')
        end
    end
    nch = peek(p)
    if isnull(nch)
        pos = nextpos(p)
        error(p, pos, pos, "expected start of a numeric literal")
        return NONE(String, p)
    else
        ch = get(nch)
        if isdigit(ch)
            c = read(p)
            if c == '0' && !allow_leading_zeros
                write(p, '0')
                nch = peek(p)
                if !isnull(nch)
                    ch = get(nch)
                    if isdigit(ch)
                        error(p, st, position(p), "leading zeroes are not allowed")
                        return NONE(String, p)
                    end
                end
            elseif isdigit(c)
                write(p, c)
            end
        else
            # non-digit
            error(p, st, position(p), "expected a digit, found `$ch`")
            return NONE(String, p)
        end
    end

    underscore = false
    while !eof(p)
        ch = read(p)
        if isdigit(ch)
            write(p, ch)
            underscore = false
        elseif ch == '_' && !underscore
            underscore = true
        else
            rewind(p)
            break
        end
    end
    if underscore
        pos = nextpos(p)
        error(p, pos, pos, "numeral cannot end with an underscore")
        NONE(String, p)
    else
        p.charbuffer.ptr == 1 ? NONE(String, p) : SOME(String, p)
    end
end

"Parses boolean"
function boolean(p::Parser, st::Int)
    ch = '\x00'

    i = 0
    word = "true"
    while !eof(p) && i<length(word)
        ch = read(p)
        i+=1
        ch != word[i] && break
    end
    if i == length(word)
        return SOME(true)
    else
        rewind(p, i)
    end
    ti = i
    tch = ch

    i = 0
    word = "false"
    while !eof(p) && i<length(word)
        ch = read(p)
        i+=1
        ch != word[i] && break
    end
    if i == length(word)
        return SOME(false)
    else
        rewind(p, i)
        if i < ti
            i = ti
            ch = tch
        end
        error(p, st, st+i-1, "unexpected character: `$ch`")
    end

    return NONE(Bool)
end

"Parse number or datetime"
function numdatetime(p::Parser, st::Int)
    isfloat = false

    nprefix = integer(p, st, false, true)
    isnull(nprefix) && return NONE()
    prefix = get(nprefix)

    decimal = if consume(p, '.')
        isfloat = true
        ndecimal = integer(p, st, true, false)
        isnull(ndecimal) && return NONE()
        SOME(get(ndecimal))
    else
        NONE(String, p)
    end

    exponent = if consume(p,'e') || consume(p,'E')
        isfloat = true;
        nexponent = integer(p, st, false, true)
        isnull(nexponent) && return NONE()
        SOME(get(nexponent))
    else
        NONE(String, p)
    end

    pend = nextpos(p)
    nch = peek(p)
    ret = if isnull(decimal) &&
             isnull(exponent) &&
             nch != '+' &&
             nch != '+' &&
             st + 4 == pend &&
             consume(p, '-')
        datetime(p, prefix, st)
    else
        input = if isnull(decimal) && isnull(exponent) #!isfloat
            prefix
        elseif !isnull(decimal) && isnull(exponent)
            "$(prefix)."*get(decimal,"")
        elseif isnull(decimal) && !isnull(exponent)
            "$(prefix)E"*get(exponent,"")
        elseif !isnull(decimal) && !isnull(exponent)
            "$(prefix)."*get(decimal,"")*"E"*get(exponent,"")
        end
        input = lstrip(input, '+')
        try
            SOME(Base.parse(isfloat ? Float64 : Int, input))
        catch
            NONE()
        end
    end
    isnull(ret) && error(p, st, pend, "invalid numeric literal")
    return ret
end

"Parses a datetime value"
function datetime(p::Parser, syear::String, st::Int)

    valid = true

    function parsetwodigits(p, valid)
        eof(p) && return 0, false
        ch1 = read(p)
        valid = valid && isdigit(ch1)
        eof(p) && return 0, false
        ch2 = read(p)
        valid = valid && isdigit(ch2)
        (valid ? Base.parse(Int, String([ch1, ch2])) : 0), valid
    end

    year = Base.parse(Int, syear)
    month,  valid = parsetwodigits(p, valid)
    valid = valid && consume(p, '-')
    day,    valid = parsetwodigits(p, valid)
    valid = valid && consume(p, 'T')
    hour,   valid = parsetwodigits(p, valid)
    valid = valid && consume(p, ':')
    minute, valid = parsetwodigits(p, valid)
    valid = valid && consume(p, ':')
    second, valid = parsetwodigits(p, valid)

    # fractional seconds
    msec = 0
    if consume(p, '.')
        fsec = Char[]
        ch = peek(p)
        valid = valid && !isnull(ch) && isdigit(get(ch))
        while true
            ch = peek(p)
            if !isnull(ch) && isdigit(get(ch))
                push!(fsec, read(p))
            else
                break
            end
        end
        if length(fsec)>0
            msec = Base.parse(Int, String(fsec))
        end
    end

    # time zone
    tzhour=0
    tzminute=0
    tzplus = true
    tzminus = false
    tzsign = true
    if valid && !consume(p, 'Z')
        tzplus = consume(p, '+')
        if !tzplus
            tzminus = consume(p, '-')
        end
        valid = valid && (tzplus || tzminus)
        tzhour,   valid = parsetwodigits(p, valid)
        valid =   valid && consume(p, ':')
        tzminute, valid = parsetwodigits(p, valid)

        tzsign = tzplus
    end

    if valid
        dt = DateTime(year, month, day,
                hour + (tzsign ? tzhour : -tzhour),
                minute + (tzsign ? tzminute : -tzminute),
                second, msec)
        return SOME(dt)
    else
        error(p, st, position(p), "malformed date literal")
        return NONE()
    end
end

"Parses a single or multi-line string"
function basicstring(p::Parser, st::Int)
    !expect(p, '"') && return NONE(String, p)

    multiline = false

    if consume(p, '"')
        if consume(p, '"')
            multiline = true
            newline(p)
        else
            return SOME("")
        end
    end

    basicstring(p, st, multiline)
end

"Finish parsing a basic string after the opening quote has been seen"
function basicstring(p::Parser, st::Int, multiline::Bool)
    while true
        while multiline && newline(p)
            write(p, '\n')
        end
        if eof(p)
            pos = position(p)
            error(p, st, pos, "unterminated string literal")
            return NONE(String, p)
        else
            ch = read(p)
            if ch == '"'
                if multiline
                    if !consume(p, '"')
                        write(p, '"')
                        continue
                    end
                    if !consume(p, '"')
                        write(p, '"')
                        write(p, '"')
                        continue
                    end
                end
                return SOME(String, p)
            elseif ch == '\\'
                pos = position(p)
                ec = escape(p, pos, multiline)
                !isnull(ec) && write(p, get(ec))
            elseif ch < '\x1f'
                pos = position(p)
                error(p, st, pos, "control character `$ch` must be escaped")
            else
                write(p, ch)
            end
        end
    end
end

"Reads character(s) after `\\` and transforms them into proper character"
function escape(p::Parser, st::Int, multiline::Bool)
    if multiline && newline(p)
        while whitespace(p) || newline(p) end
        return NONE()
    end
    pos = position(p)
    if eof(p)
        error(p, st, pos, "unterminated escape sequence")
        return NONE()
    else
        ch = read(p)
        if ch == 'b'
            SOME('\b')
        elseif ch == 't'
            SOME('\t')
        elseif ch == 'n'
            SOME('\n')
        elseif ch == 'f'
            SOME('\f')
        elseif ch == 'r'
            SOME('\r')
        elseif ch == '"'
            SOME('\"')
        elseif ch == '\\'
            SOME('\\')
        elseif ch == 'u' || ch == 'U'
            len = ch == 'u' ? 4 : 8
            ucstr = ch == 'u' ? "\\u" : "\\U"
            snum = String(read(p.input, len))
            try
                if length(snum) < len
                    error(p, st, st+len, "expected $len hex digits after a `$ch` escape")
                    NONE()
                end
                if !all(isxdigit, snum)
                    error(p, st, st+len, "unknown string escape: `$snum`")
                    NONE()
                end
                c = unescape_string(ucstr * snum)[1]
                SOME(c)
            catch
                error(p, st, st+len, "codepoint `$snum` is not a valid unicode codepoint")
                rewind(p, len)
                NONE()
            end
        else
            error(p, st, position(p), "unknown string escape: `$("\\x"*hex(ch,2))`")
            NONE()
        end
    end
end

"Parses a single or multi-line literal string"
function literalstring(p::Parser, st::Int)
    !expect(p, '\'') && return NONE(String, p)

    multiline = false

    if consume(p, '\'')
        if consume(p, '\'')
            multiline = true
            newline(p)
        else
            return SOME("")
        end
    end

    literalstring(p, st, multiline)
end

function literalstring(p::Parser, st::Int, multiline::Bool)
    while true
        if !multiline && newline(p)
            npos = nextpos(p)
            error(p, st, npos, "literal strings cannot contain newlines")
            return NONE(String, p)
        end
        if eof(p)
            error(p, st, position(p), "unterminated string literal")
            return NONE(String, p)
        else
            ch = read(p.input, UInt8)
            if ch == 0x27
                if multiline
                    if !consume(p, '\'')
                        write(p, 0x27)
                        continue
                    end
                    if !consume(p, '\'')
                        write(p, 0x27)
                        write(p, 0x27)
                        continue
                    end
                end
                return SOME(String, p)
            else
                write(p, ch)
            end
        end
    end
end

function array(p::Parser, st::Int)
    !expect(p, '[') && return NONE()
    ret = Any[]
    rettype = Any
    expected = Any
    while true

        # Break out early if we see the closing bracket
        ignore(p)
        consume(p, ']') && return SOME(ret)

        # Attempt to parse a value, triggering an error if it's the wrong type.
        pstart = nextpos(p)
        npvalue = value(p)
        isnull(npvalue) && return NONE()
        pvalue = get(npvalue)

        pend = nextpos(p)
        valtype = isa(pvalue, Array) ? Array : typeof(pvalue)
        expected = rettype === Any ? valtype : expected
        if valtype != expected
            error(p, pstart, pend, "expected type `$expected`, found type `$valtype`")
        else
            rettype = expected
            push!(ret, pvalue)
        end

        # Look for a comma. If we don't find one we're done
        ignore(p)
        !consume(p, ',') && break
    end
    ignore(p)
    !expect(p, ']') && return NONE()
    return SOME(convert(Vector{rettype}, ret))
end

function inlinetable(p::Parser, st::Int)
    !expect(p, '{') && return NONE()
    whitespace(p)

    ret = Table(true)
    consume(p, '}') && return SOME(ret)

    while true
        npos = nextpos(p)
        k = keyname(p)
        isnull(k) && return NONE()
        !separator(p) && return NONE()
        v = value(p)
        isnull(v) && return NONE()
        insertpair(p, ret, get(k), get(v), npos)

        whitespace(p)
        consume(p, '}') && break
        !expect(p, ',') && return NONE()
        whitespace(p)
    end

    return SOME(ret)
end

"Parses a value"
function value(p::Parser)
    whitespace(p)
    c = peek(p)
    isnull(c) && return NONE()
    ch = get(c)
    pos = position(p)+1
    if ch == '"'
        return basicstring(p, pos)
    elseif ch == '\''
        return literalstring(p, pos)
    elseif ch == 't' || ch == 'f'
        return boolean(p, pos)
    elseif ch == '['
        return array(p, pos)
    elseif ch == '{'
        return inlinetable(p, pos)
    elseif ch == '-' || ch == '+' || isdigit(ch)
        return numdatetime(p, pos)
    else
        error(p, pos, pos+1, "expected a value")
        return NONE()
    end
end

"Parses the key-values and fills the given dictionary. Returns true in case of success and false in case of error."
function keyvalues(p::Parser, tbl::Table)
    while true
        whitespace(p)
        newline(p) && continue
        comment(p) && continue
        nc = peek(p)
        isnull(nc) && break
        get(nc) == '[' && break

        # get key
        klo = nextpos(p)
        k = keyname(p)
        isnull(k) && return false

        # skip kv separator
        !separator(p) && return false

        # get value
        v = value(p)
        isnull(v) && return false
        vend = nextpos(p)

        # insert kv into result table
        insertpair(p, tbl, get(k), get(v), klo)

        whitespace(p)
        if !comment(p) && !newline(p)
            pc = peek(p)
            isnull(pc) && return true
            error(p, klo, vend, "expected a newline after a key")
            return false
        end
    end
    return true
end

"Construct nested structures from keys"
function nested(p, into, ks, kstart)
    cnode = into
    kend = 0
    for k in ks[1:end-1]
        kend += length(k)+1
        if !haskey(cnode, k)
            cnode.values[k] = Table(false)
            cnode = cnode[k]
        else
            tmp = cnode[k]
            if isa(tmp, Table)
                cnode = tmp
            elseif isa(tmp, Array)
                if length(tmp)>0 && typeof(tmp[end]) === Table
                    cnode = tmp[end]
                else
                    error(p, kstart, kstart+kend, "array `$k` does not contain tables")
                    return NONE(), kend
                end
            else
                error(p, kstart, kstart+kend, "key `$k` was not previously a table")
                return NONE(), kend
            end
        end
    end
    return SOME(cnode), kend
end

function addtable(p::Parser, into::Table, ks::Vector{String}, tbl::Table, kstart)

    cnode, kend = nested(p, into, ks, kstart)
    isnull(cnode) && return
    cur = get(cnode)

    # fill last level with values
    tkey = ks[end]
    if haskey(cur, tkey)
        ctbl = cur[tkey]
        if isa(ctbl, Table)
            ctbl.defined && error(p, kstart, kstart+kend+length(tkey), "redefinition of table `$tkey`")

            for (k,v) in tbl.values
                insertpair(p, ctbl, k, v, kstart)
            end
        else
            error(p, kstart, kstart+kend+length(tkey), "key `$tkey` was not previously a table")
        end
    else
        cur.values[tkey] = tbl
    end

end

function addarray(p::Parser, into::Table, ks::Vector{String}, val, kstart)

    cnode, kend = nested(p, into, ks, kstart)
    isnull(cnode) && return
    cur = get(cnode)

    akey = ks[end]
    if haskey(cur, akey)
        vec = cur[akey]
        if isa(vec, Array)
            if isa(val, eltype(vec))
                push!(vec, val)
            else
                error(p, kstart, kstart+kend+length(akey), "expected type `$(typeof(val))`, found type `$(eltype(vec))`")
            end
        else
            error(p, kstart, kstart+kend+length(akey), "key `$akey` was previously not an array")
        end
    else
        cur.values[akey] = Any[val]
    end

end


"""Executes the parser, parsing the string contained within.

This function will return the `Table` instance if parsing is successful, or it will return `nothing` if any parse error or invalid TOML error occurs.

If an error occurs, the `errors` field of this parser can be consulted to determine the cause of the parse failure.
"""
function parse(p::Parser)
    ret = Table(false)
    while !eof(p)
        whitespace(p)
        newline(p) && continue
        comment(p) && continue

        # parse section
        if consume(p, '[')
            arr = consume(p, '[')
            npos = nextpos(p)

            # parse the name of the section
            ks = String[]
            while true
                whitespace(p)
                s = keyname(p)
                !isnull(s) && push!(ks, get(s))
                whitespace(p)
                if consume(p, ']')
                    arr && !expect(p, ']') && return NONE(Table)
                    break
                end
                !expect(p, '.') && return NONE()
            end
            isempty(ks) && return NONE(Table)

            # build the section
            section = Table(true)
            !keyvalues(p, section) && return NONE(Table)
            if arr
                addarray(p, ret, ks, section, npos)
            else
                addtable(p, ret, ks, section, npos)
            end
        else
            !keyvalues(p, ret) && return NONE(Table)
        end
    end

    length(p.errors) != 0 && return NONE(Table)

    return SOME(ret)
end
