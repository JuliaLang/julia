# This file is a part of Julia. License is MIT: http://julialang.org/license

# ––––––––––
# Paragraphs
# ––––––––––

type Paragraph
    content
end

Paragraph() = Paragraph([])

function paragraph(stream::IO, md::MD)
    buffer = IOBuffer()
    p = Paragraph()
    push!(md, p)
    skipwhitespace(stream)
    prev_char = '\n'
    while !eof(stream)
        char = read(stream, Char)
        if char == '\n' || char == '\r'
            char == '\r' && peek(stream) == '\n' && read(stream, Char)
            if prev_char == '\\'
                write(buffer, '\n')
            elseif blankline(stream) || parse(stream, md, breaking = true)
                break
            else
                write(buffer, ' ')
            end
        else
            write(buffer, char)
        end
        prev_char = char
    end
    p.content = parseinline(seek(buffer, 0), md)
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
function hashheader(stream::IO, md::MD)
    withstream(stream) do
        eatindent(stream) || return false
        level = 0
        while startswith(stream, '#') level += 1 end
        level < 1 || level > 6 && return false

        c = ' '
        # Allow empty headers, but require a space
        !eof(stream) && (c = read(stream, Char); !(c in " \n")) &&
            return false

        if c != '\n' # Empty header
            h = readline(stream) |> strip
            h = match(r"(.*?)( +#+)?$", h).captures[1]
            buffer = IOBuffer()
            print(buffer, h)
            push!(md.content, Header(parseinline(seek(buffer, 0), md), level))
        else
            push!(md.content, Header("", level))
        end
        return true
    end
end

function setextheader(stream::IO, md::MD)
    withstream(stream) do
        eatindent(stream) || return false
        header = readline(stream) |> strip
        header == "" && return false

        eatindent(stream) || return false
        underline = readline(stream) |> strip
        length(underline) < 3 && return false
        u = underline[1]
        u in "-=" || return false
        all(c -> c == u, underline) || return false
        level = (u == '=') ? 1 : 2

        push!(md.content, Header(parseinline(header, md), level))
        return true
    end
end

# ––––
# Code
# ––––

type Code
    language::String
    code::String
end

Code(code) = Code("", code)

function indentcode(stream::IO, block::MD)
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
        !isempty(code) && (push!(block, Code(rstrip(code))); return true)
        return false
    end
end

# ––––––
# Quotes
# ––––––

type BlockQuote
    content
end

BlockQuote() = BlockQuote([])

# TODO: Laziness
@breaking true ->
function blockquote(stream::IO, block::MD)
    withstream(stream) do
        buffer = IOBuffer()
        empty = true
        while eatindent(stream) && startswith(stream, '>')
            startswith(stream, " ")
            write(buffer, readline(stream))
            empty = false
        end
        empty && return false

        md = takebuf_string(buffer)
        push!(block, BlockQuote(parse(md, flavor = config(block)).content))
        return true
    end
end

# –––––
# Lists
# –––––

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
function list(stream::IO, block::MD)
    withstream(stream) do
        eatindent(stream) || return false
        b = startswith(stream, num_or_bullets)
        (b === nothing || b == "") && return false
        ordered = !(b[1] in bullets)
        if ordered
            b = b[end - 1] == '.' ? r"^\d+\. " : r"^\d+\) "
            # TODO start value
        end
        the_list = List(ordered)

        buffer = IOBuffer()
        fresh_line = false
        while !eof(stream)
            if fresh_line
                sp = startswith(stream, r"^ {0,3}")
                if !(startswith(stream, b) in [false, ""])
                    push!(the_list.items, parseinline(takebuf_string(buffer), block))
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
        push!(the_list.items, parseinline(takebuf_string(buffer), block))
        push!(block, the_list)
        return true
    end
end

# ––––––––––––––
# HorizontalRule
# ––––––––––––––

type HorizontalRule
end

function horizontalrule(stream::IO, block::MD)
   withstream(stream) do
       n, rule = 0, ' '
       while !eof(stream)
           char = read(stream, Char)
           char == '\n' && break
           isspace(char) && continue
           if n==0 || char==rule
               rule = char
               n += 1
           else
               return false
           end
       end
       is_hr = (n ≥ 3 && rule in "*-")
       is_hr && push!(block, HorizontalRule())
       return is_hr
   end
end
