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
    startswith(stream, "#") || return false
    level = 1
    while startswith(stream, "#")
        level += 1
    end

    if level > 6
        skip(stream, -level)
        return false
    elseif !eof(stream) && (c = read(stream, Char); !(c in " \n"))
        skip(stream, -level-length(string(c).data))
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
        while startswith(stream, "    ") || startswith(stream, "\t")
            write(buffer, readline(stream))
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
        skipwhitespace(stream)
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
        while !eof(stream)
            if fresh_line
                skipwhitespace(stream)
                if startswith(stream, b) != ""
                    push!(the_list.items, parseinline(takebuf_string(buffer), config))
                    buffer = IOBuffer()
                else
                    write(buffer, ' ')
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
        return true
    end
end

# ––––––––––––––
# HorizontalRule
# ––––––––––––––

type HorizontalRule
end

function horizontalrule(stream::IO, block::MD, config::Config)
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
