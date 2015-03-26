# ––––––––––
# Paragraphs
# ––––––––––

type Paragraph
    content
end

Paragraph() = Paragraph([])

function paragraph(stream::IO, md::MD, config::Config)
    buffer = IOBuffer()
    p = Paragraph()
    push!(md, p)
    skipwhitespace(stream)
    while !eof(stream)
        char = read(stream, Char)
        if char == '\n' || char == '\r'
            if blankline(stream) || parse(stream, md, config, breaking = true)
                break
            else
                write(buffer, ' ')
            end
        else
            write(buffer, char)
        end
    end
    p.content = parseinline(seek(buffer, 0), config)
    return true
end

# –––––––
# Headers
# –––––––

type Header{level}
    text
end

Header(s, level::Int) = Header{level}(s)
Header(s) = Header(s, 1)

@breaking true ->
function hashheader(stream::IO, md::MD, config::Config)
    withstream(stream) do
        startswith(stream, r"^ {0,3}#") != "" || return false
        level = 1
        while startswith(stream, "#")
            level += 1
        end

        if level > 6
            return false
        elseif !eof(stream) && (c = read(stream, Char); !(c in " \n"))
            return false
        end

        h = readline(stream) |> chomp
        h = strip(h)
        h = match(r"(.*?)( +#+)?$", h).captures[1]
        buffer = IOBuffer()
        print(buffer, h)
        push!(md.content, Header(parseinline(seek(buffer, 0), config), level))
        return true
    end
end

function setextheader(stream::IO, md::MD, config::Config)
    withstream(stream) do
        startswith(stream, r"^ {0,3}")
        startswith(stream, " ") && return false
        header = readline(stream) |> strip
        header == "" && return false

        startswith(stream, r"^ {0,3}")
        startswith(stream, " ") && return false
        underline = readline(stream) |> strip
        length(underline) < 3 && return false
        u = underline[1]
        u in "-=" || return false
        for c in underline[2:end]
            c != u && return false
        end
        level = (u == '=') ? 1 : 2

        push!(md.content, Header(parseinline(header, config), level))
        return true
    end
end

# ––––
# Code
# ––––

type Code
    language::UTF8String
    code::UTF8String
end

Code(code) = Code("", code)

function indentcode(stream::IO, block::MD, config::Config)
    withstream(stream) do
        buffer = IOBuffer()
        while !eof(stream)
            if startswith(stream, "    ") || startswith(stream, "\t")
                write(buffer, readline(stream))
            elseif blankline(stream)
                write(buffer, '\n')
            else
                break
            end
        end
        code = takebuf_string(buffer)
        !isempty(code) && (push!(block, Code(chomp(code))); return true)
        return false
    end
end

# ––––––
# Quotes
# ––––––

type BlockQuote
    content
end

BlockQuote() = BlockQuote([])

# TODO: Laziness
@breaking true ->
function blockquote(stream::IO, block::MD, config::Config)
    withstream(stream) do
        buffer = IOBuffer()
        empty = true
        while startswith(stream, r"^ {0,3}>") != ""
            startswith(stream, " ")
            write(buffer, readline(stream))
            empty = false
        end
        empty && return false

        md = takebuf_string(buffer)
        push!(block, BlockQuote(parse(md, flavor = config).content))
        return true
    end
end

# –––––
# Lists
# –––––

type List
    items::Vector{Any}
    ordered::Bool

    List(x::AbstractVector, b::Bool) = new(x, b)
    List(x::AbstractVector) = new(x, false)
    List(b::Bool) = new(Any[], b)
end

List(xs...) = List(vcat(xs...))

const bullets = "*•+-"
const num_or_bullets = r"^(\*|•|\+|-|\d+(\.|\))) "

# Todo: ordered lists, inline formatting
function list(stream::IO, block::MD, config::Config)
    withstream(stream) do
        startswith(stream, r"^ {0,3}")
        b = startswith(stream, num_or_bullets)
        (b == nothing || b == "") && return false
        ordered = !(b[1] in bullets)
        if ordered
            b = b[end - 1] == '.' ? r"^\d+\. " : r"^\d+\) "
            # TODO start value
        end
        the_list = List(ordered)

        buffer = IOBuffer()
        fresh_line = false
        has_hr = false
        while !eof(stream)
            if fresh_line
                # TODO should config affect this?
                ishorizontalrule(stream) && (has_hr = true; break)

                sp = startswith(stream, r"^ {0,3}")
                if !(startswith(stream, b) in [false, ""])
                    push!(the_list.items, parseinline(takebuf_string(buffer), config))
                    buffer = IOBuffer()
                else
                    # TODO write a newline here, and deal with nested
                    write(buffer, ' ', sp)
                end
                fresh_line = false
            else
                c = read(stream, Char)
                if c == '\n'
                    eof(stream) && break
                    next = peek(stream)
                    if next == '\n'
                        break
                    else
                        fresh_line = true
                    end
                else
                    write(buffer, c)
                end
            end
        end
        push!(the_list.items, parseinline(takebuf_string(buffer), config))
        push!(block, the_list)
        has_hr && push!(block, HorizontalRule())
        return true
    end
end

# ––––––––––––––
# HorizontalRule
# ––––––––––––––

type HorizontalRule
end

function horizontalrule(stream::IO, block::MD, config::Config)
    is_hr = ishorizontalrule(stream)
    is_hr && push!(block, HorizontalRule())
    return is_hr
end

const _hrules = "*-_"

function ishorizontalrule(stream::IO)
    withstream(stream) do
        startswith(stream, r"^ {0,3}")
        eof(stream) && return false
        rule = read(stream, Char)
        rule in _hrules || return false

        n = 1
        while !eof(stream)
            ch = read(stream, Char)
            ch == '\n' && break
            isspace(ch) && continue
            if ch == rule
                n += 1
            else
                return false
            end
        end
        n ≥ 3
    end
end
