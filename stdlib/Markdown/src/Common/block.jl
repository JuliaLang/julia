# This file is a part of Julia. License is MIT: https://julialang.org/license

# ––––––––––
# Paragraphs
# ––––––––––

mutable struct Paragraph
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
            char == '\r' && !eof(stream) && Char(peek(stream)) == '\n' && read(stream, Char)
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

mutable struct Header{level}
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
        (level < 1 || level > 6) && return false

        c = ' '
        # Allow empty headers, but require a space
        !eof(stream) && (c = read(stream, Char); !(c in " \n")) &&
            return false

        if c != '\n' # Empty header
            h = strip(readline(stream))
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
        header = strip(readline(stream))
        isempty(header) && return false

        eatindent(stream) || return false
        underline = strip(readline(stream))
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

mutable struct Code
    language::String
    code::String
end

Code(code) = Code("", code)

function indentcode(stream::IO, block::MD)
    withstream(stream) do
        buffer = IOBuffer()
        while !eof(stream)
            if startswith(stream, "    ") || startswith(stream, "\t")
                write(buffer, readline(stream, keep=true))
            elseif blankline(stream)
                write(buffer, '\n')
            else
                break
            end
        end
        code = String(take!(buffer))
        !isempty(code) && (push!(block, Code(rstrip(code))); return true)
        return false
    end
end

# --------
# Footnote
# --------

mutable struct Footnote
    id::String
    text
end

function footnote(stream::IO, block::MD)
    withstream(stream) do
        regex = r"^\[\^(\w+)\]:"
        str = startswith(stream, regex)
        if isempty(str)
            return false
        else
            ref = match(regex, str).captures[1]
            buffer = IOBuffer()
            write(buffer, readline(stream, keep=true))
            while !eof(stream)
                if startswith(stream, "    ")
                    write(buffer, readline(stream, keep=true))
                elseif blankline(stream)
                    write(buffer, '\n')
                else
                    break
                end
            end
            content = parse(seekstart(buffer)).content
            push!(block, Footnote(ref, content))
            return true
        end
    end
end

# ––––––
# Quotes
# ––––––

mutable struct BlockQuote
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
            write(buffer, readline(stream, keep=true))
            empty = false
        end
        empty && return false

        md = String(take!(buffer))
        push!(block, BlockQuote(parse(md, flavor = config(block)).content))
        return true
    end
end

# -----------
# Admonitions
# -----------

mutable struct Admonition
    category::String
    title::String
    content::Vector
end

@breaking true ->
function admonition(stream::IO, block::MD)
    withstream(stream) do
        # Admonition syntax:
        #
        # !!! category "optional explicit title within double quotes"
        #     Any number of other indented markdown elements.
        #
        #     This is the second paragraph.
        #
        startswith(stream, "!!! ") || return false
        # Extract the category of admonition and its title:
        category, title =
            let untitled = r"^([a-z]+)$",          # !!! <CATEGORY_NAME>
                titled   = r"^([a-z]+) \"(.*)\"$", # !!! <CATEGORY_NAME> "<TITLE>"
                line     = strip(readline(stream))
                if occursin(untitled, line)
                    m = match(untitled, line)
                    # When no title is provided we use CATEGORY_NAME, capitalising it.
                    m.captures[1], uppercasefirst(m.captures[1])
                elseif occursin(titled, line)
                    m = match(titled, line)
                    # To have a blank TITLE provide an explicit empty string as TITLE.
                    m.captures[1], m.captures[2]
                else
                    # Admonition header is invalid so we give up parsing here and move
                    # on to the next parser.
                    return false
                end
            end
        # Consume the following indented (4 spaces) block.
        buffer = IOBuffer()
        while !eof(stream)
            if startswith(stream, "    ")
                write(buffer, readline(stream, keep=true))
            elseif blankline(stream)
                write(buffer, '\n')
            else
                break
            end
        end
        # Parse the nested block as markdown and create a new Admonition block.
        nested = parse(String(take!(buffer)), flavor = config(block))
        push!(block, Admonition(category, title, nested.content))
        return true
    end
end

# –––––
# Lists
# –––––

mutable struct List
    items::Vector{Any}
    ordered::Int # `-1` is unordered, `>= 0` is ordered.
    loose::Bool # TODO: Renderers should use this field
end
List(x::AbstractVector, b::Integer) = List(x, b, false)
List(x::AbstractVector) = List(x, -1)
List(b::Integer) = List(Any[], b)
List(xs...) = List(vcat(xs...))

isordered(list::List) = list.ordered >= 0

const BULLETS = r"^ {0,3}(\*|\+|-)( |$)"
const NUM_OR_BULLETS = r"^ {0,3}(\*|\+|-|\d+(\.|\)))( |$)"

@breaking true ->
function list(stream::IO, block::MD)
    withstream(stream) do
        bullet = startswith(stream, NUM_OR_BULLETS; eat = false)
        indent = isempty(bullet) ? (return false) : length(bullet)
        # Calculate the starting number and regex to use for bullet matching.
        initial, regex =
            if occursin(BULLETS, bullet)
                # An unordered list. Use `-1` to flag the list as unordered.
                -1, BULLETS
            elseif occursin(r"^ {0,3}\d+(\.|\))( |$)", bullet)
                # An ordered list. Either with `1. ` or `1) ` style numbering.
                r = occursin(".", bullet) ? r"^ {0,3}(\d+)\.( |$)" : r"^ {0,3}(\d+)\)( |$)"
                Base.parse(Int, match(r, bullet).captures[1]), r
            else
                # Failed to match any bullets. This branch shouldn't actually be needed
                # since the `NUM_OR_BULLETS` regex should cover this, but we include it
                # simply for thoroughness.
                return false
            end

        # Initialise the empty list object: either ordered or unordered.
        list = List(initial)

        buffer = IOBuffer() # For capturing nested text for recursive parsing.
        newline = false     # For checking if we have two consecutive newlines: end of list.
        count = 0           # Count of list items. Used to check if we need to push remaining
                            # content in `buffer` after leaving the `while` loop.
        while !eof(stream)
            if startswith(stream, "\n")
                if newline
                    # Double newline ends the current list.
                    pushitem!(list, buffer)
                    break
                else
                    newline = true
                    println(buffer)
                end
            else
                if startswith(stream, " "^indent)
                    newline && (list.loose = true)
                    # Indented text that is part of the current list item.
                    print(buffer, readline(stream, keep=true))
                else
                    matched = startswith(stream, regex)
                    if isempty(matched)
                        # Unindented text meaning we have left the current list.
                        pushitem!(list, buffer)
                        break
                    else
                        # Start of a new list item.
                        newline && (list.loose = true)
                        count += 1
                        count > 1 && pushitem!(list, buffer)
                        print(buffer, readline(stream, keep=true))
                    end
                end
                newline = false
            end
        end
        count == length(list.items) || pushitem!(list, buffer)
        push!(block, list)
        return true
    end
end
pushitem!(list, buffer) = push!(list.items, parse(String(take!(buffer))).content)

# ––––––––––––––
# HorizontalRule
# ––––––––––––––

mutable struct HorizontalRule
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
