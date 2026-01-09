# This file is a part of Julia. License is MIT: https://julialang.org/license

# ––––––––––
# Paragraphs
# ––––––––––

mutable struct Paragraph <: MarkdownElement
    content
end

Paragraph() = Paragraph([])

function paragraph(stream::IO, md::MD)
    buffer = IOBuffer()
    p = Paragraph()
    push!(md, p)
    skipwhitespace(stream)
    prev_char = '\n'
    for char in readeach(stream, Char)
        if char == '\n' || char == '\r'
            char == '\r' && !eof(stream) && peek(stream, Char) == '\n' && read(stream, Char)
            if prev_char == '\\'
                write(buffer, '\n')
            elseif blankline(stream) || _parse(stream, md, breaking = true)
                break
            else
                write(buffer, '\n')
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

mutable struct Header{level} <: MarkdownElement
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
        !eof(stream) && (c = read(stream, Char); !(c in " \t\n")) &&
            return false

        if c != '\n' # Empty header
            h = strip(readline(stream))
            h = (match(r"(.*?)( +#+)?$", h)::AbstractMatch).captures[1]
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

mutable struct Code <: MarkdownElement
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
        code = takestring!(buffer)
        !isempty(code) && (push!(block, Code(rstrip(code))); return true)
        return false
    end
end

@breaking true ->
function fencedcode(stream::IO, block::MD)
    withstream(stream) do
        startswith(stream, "~~~", padding = true) || startswith(stream, "```", padding = true) || return false
        skip(stream, -1)
        ch = read(stream, Char)
        trailing = strip(readline(stream))
        flavor = lstrip(trailing, ch)
        n = 3 + length(trailing) - length(flavor)

        # inline code block
        ch in flavor && return false

        buffer = IOBuffer()
        while !eof(stream)
            line_start = position(stream)
            if startswith(stream, string(ch) ^ n)
                if !startswith(stream, string(ch))
                    if flavor == "math"
                        push!(block, LaTeX(takestring!(buffer) |> chomp))
                    else
                        push!(block, Code(flavor, takestring!(buffer) |> chomp))
                    end
                    return true
                else
                    seek(stream, line_start)
                end
            end
            write(buffer, readline(stream, keep=true))
        end
        return false
    end
end

# --------
# Footnote
# --------

mutable struct Footnote <: MarkdownElement
    id::String
    text
end

function footnote(stream::IO, block::MD)
    withstream(stream) do
        regex = r"^\[\^(\w+)\]:"
        m = matchstart(stream, regex)
        if m === nothing
            return false
        else
            ref = m.captures[1]
            buffer = IOBuffer()
            write(buffer, readline(stream, keep=true))
            while !eof(stream)
                if startswith(stream, "    ") || startswith(stream, "\t")
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

mutable struct BlockQuote <: MarkdownElement
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

        md = takestring!(buffer)
        push!(block, BlockQuote(parse(md, flavor = config(block)).content))
        return true
    end
end

# -----------
# Admonitions
# -----------

mutable struct Admonition <: MarkdownElement
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
                    m = match(untitled, line)::AbstractMatch
                    # When no title is provided we use CATEGORY_NAME, capitalising it.
                    m.captures[1], uppercasefirst(m.captures[1])
                elseif occursin(titled, line)
                    m = match(titled, line)::AbstractMatch
                    # To have a blank TITLE provide an explicit empty string as TITLE.
                    m.captures[1], m.captures[2]
                else
                    # Admonition header is invalid so we give up parsing here and move
                    # on to the next parser.
                    return false
                end
            end
        # Consume the following indented (4 spaces or tab) block.
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
        # Parse the nested block as markdown and create a new Admonition block.
        nested = parse(takestring!(buffer), flavor = config(block))
        push!(block, Admonition(category, title, nested.content))
        return true
    end
end

# –––––
# Lists
# –––––

mutable struct List <: MarkdownElement
    items::Vector{Any}
    ordered::Int # `-1` is unordered, `>= 0` is ordered and indicates the start index.
    loose::Bool # TODO: Renderers should use this field
end
List(x::AbstractVector, b::Integer) = List(x, b, false)
List(x::AbstractVector) = List(x, -1)
List(b::Integer) = List(Any[], b)
List(xs...) = List(vcat(xs...))

isordered(list::List) = list.ordered >= 0

const BULLETS = r"^ {0,3}(\*|\+|-)( |$)"
const NUM_OR_BULLETS = r"^ {0,3}(\*|\+|-|(\d+)(\.|\)))( |$)"

@breaking true ->
function list(stream::IO, block::MD)
    withstream(stream) do
        m = matchstart(stream, NUM_OR_BULLETS; eat = false)
        isnothing(m) && return false
        indent = length(m.match)
        # Calculate the starting number and regex to use for bullet matching.
        initial, regex =
            if m.captures[3] == nothing
                # An unordered list. Use `-1` to flag the list as unordered.
                -1, BULLETS
            elseif m.captures[3] == "."
                # An ordered list with `1. ` style numbering.
                Base.parse(Int, m.captures[2]), r"^ {0,3}(\d+)\.( |$)"
            elseif m.captures[3] == ")"
                # An ordered list with `1) ` style numbering.
                Base.parse(Int, m.captures[2]), r"^ {0,3}(\d+)\)( |$)"
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
                    # We have a double newline. Peek ahead to see if the next non-blank
                    # line is indented. If so, continue the list item.
                    saved_pos = position(stream)

                    # Skip any additional blank lines and check indentation
                    still_indented = false
                    while !eof(stream)
                        if startswith(stream, "\n"; eat = true)
                            continue
                        elseif startswith(stream, " "^indent; eat = false)
                            still_indented = true
                            break
                        else
                            break
                        end
                    end
                    # Reset stream to position after the double newline we detected
                    seek(stream, saved_pos)
                    if still_indented
                        # Multiple blank lines within indented content - allow it
                        println(buffer)
                    else
                        # Double newline ends the current list
                        pushitem!(list, buffer)
                        break
                    end
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
                    matched = matchstart(stream, regex)
                    if matched === nothing
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
pushitem!(list, buffer) = push!(list.items, parse(takestring!(buffer)).content)

# ––––––––––––––
# HorizontalRule
# ––––––––––––––

mutable struct HorizontalRule <: MarkdownElement
end

@breaking true ->
function horizontalrule(stream::IO, block::MD)
   withstream(stream) do
       n, rule = 0, ' '
       for char in readeach(stream, Char)
           char == '\n' && break
           isspace(char) && continue
           if n == 0
               rule = char
           elseif char != rule
               return false
           end
           n += 1
       end
       is_hr = (n ≥ 3 && rule in "*-_")
       is_hr && push!(block, HorizontalRule())
       return is_hr
   end
end
