# This file is a part of Julia. License is MIT: https://julialang.org/license

include("formatting.jl")

cols(io) = displaysize(io)[2]

function term(io::IO, content::Vector, cols)
    isempty(content) && return
    for md in content[1:end-1]
        term(io, md, cols)
        print(io, '\n', '\n')
    end
    term(io, content[end], cols)
end

function term(io::IO, md::MD, columns = cols(io))
    md = insert_hlines(md)
    return term(io, md.content, columns)
end

function term(io::IO, md::Paragraph, columns)
    lines = wraplines(annotprint(terminline, md.content), columns-2margin)
    for (i, line) in enumerate(lines)
        print(io, ' '^margin, line)
        i < length(lines) && println(io)
    end
end

function term(io::IO, md::BlockQuote, columns)
    content = annotprint(term, md.content, columns - 10)
    lines = wraplines(rstrip(content), columns - 10)
    for (i, line) in enumerate(lines)
        print(io, ' '^margin, '│', line)
        i < length(lines) && println(io)
    end
end

function term(io::IO, md::Admonition, columns)
    accent = if md.category == "danger"
        :error
    elseif md.category in ("warning", "info", "note", "tip")
        Symbol(md.category)
    elseif md.category == "compat"
        :bright_cyan
    elseif md.category == "todo"
        :magenta
    else
        :default
    end
    title = if isempty(md.title) md.category else md.title end
    print(io, ' '^margin, styled"{$accent,markdown_admonition:│ $title}",
          '\n', ' '^margin, styled"{$accent,markdown_admonition:│}", '\n')
    content = annotprint(term, md.content, columns - 10)
    lines = split(rstrip(content), '\n')
    for (i, line) in enumerate(lines)
        print(io, ' '^margin, styled"{$accent,markdown_admonition:│}", line)
        i < length(lines) && println(io)
    end
end

function term(io::IO, f::Footnote, columns)
    print(io, ' '^margin, "│ ")
    print(io, styled"{markdown_footnote:[^$(f.id)]}")
    println(io, '\n', ' '^margin, '│')
    content = annotprint(term, f.text, columns - 10)
    lines = split(rstrip(content), '\n')
    for (i, line) in enumerate(lines)
        print(io, ' '^margin, '│', line)
        i < length(lines) && println(io)
    end
end

function term(io::IO, md::List, columns)
    for (i, point) in enumerate(md.items)
        bullet = isordered(md) ? "$(i + md.ordered - 1)." : "• "
        print(io, ' '^2margin, styled"{markdown_list:$bullet} ")
        content = annotprint(term, point, columns - 10)
        lines = split(rstrip(content), '\n')
        for (l, line) in enumerate(lines)
            l > 1 && print(io, ' '^(2margin+3))
            print(io, lstrip(line))
            l < length(lines) && println(io)
        end
        i < length(md.items) && print(io, '\n'^(1 + md.loose))
    end
end

const _header_underlines = collect("≡=–-⋅ ")
# TODO settle on another option with unicode e.g. "≡=≃–∼⋅" ?

function term(io::AnnotIO, md::Header{l}, columns) where l
    face = Symbol("markdown_h$l")
    underline = _header_underlines[l]
    pre = ' '^margin
    local line_width
    with_output_annotations(io, :face => face) do io
        headline = annotprint(terminline, md.text)
        lines = wraplines(headline, columns - 4margin)
        for (i, line) in enumerate(lines)
            print(io, pre, line)
            i < length(lines) && println(io)
        end
        line_width = if length(lines) == 1
            min(textwidth(lines[end]), columns)
        elseif length(lines) > 1
            max(textwidth(lines[end]), div(columns, 3)+length(pre))
        else
            0
        end
    end
    header_width = max(0, line_width)
    if underline != ' ' && header_width > 0
        print(io, '\n', ' '^(margin))
        with_output_annotations(io -> print(io, underline^header_width), io, :face => face)
    end
end

function term(io::IO, md::Code, columns)
    code = if md.language == "julia"
        hl = AnnotatedString(md.code)
        StyledStrings.face!(hl, :markdown_code)
        highlight!(hl)
    elseif md.language == "julia-repl" || Base.startswith(md.language, "jldoctest")
        hl = AnnotatedString(md.code)
        StyledStrings.face!(hl, :markdown_code)
        for (; match) in eachmatch(r"(?:^|\n)julia>", hl)
            StyledStrings.face!(match, :markdown_julia_prompt)
            afterprompt = match.offset + ncodeunits(match) + 1
            _, exprend = Meta.parse(md.code, afterprompt, raise = false)
            highlight!(hl[afterprompt:prevind(md.code, exprend)])
            if (nextspace = findnext(' ', md.code, exprend)) |> !isnothing
                nextword = hl[exprend:prevind(hl, nextspace)]
                if nextword == "ERROR:"
                    StyledStrings.face!(nextword, :error)
                end
            end
        end
        hl
    elseif md.language == "styled"
        styled(md.code)
    else
        styled"{markdown_code:$(md.code)}"
    end
    lines = split(code, '\n')
    for (i, line) in enumerate(lines)
        print(io, ' '^margin, line)
        i < length(lines) && println(io)
    end
end

function term(io::IO, tex::LaTeX, columns)
    print(io, ' '^margin, styled"{markdown_latex:$(tex.formula)}")
end

term(io::IO, br::LineBreak, columns) = nothing # line breaks already printed between subsequent elements

function term(io::IO, br::HorizontalRule, columns)
    print(io, ' '^margin, styled"{markdown_hrule:$('─'^(columns - 2margin))}")
end

function term(io::IO, md::MarkdownElement, columns)
    a = IOContext(AnnotatedIOBuffer(), io)
    term(a, md, columns)
    print(io, read(seekstart(a.io), AnnotatedString))
end

term(io::IO, x, _) = show(io, MIME"text/plain"(), x)

# Inline Content

terminline(io::IO, content...) = terminline(io, collect(content))

function terminline(io::IO, content::Vector)
    for md in content
        terminline(io, md)
    end
end

function terminline(io::IO, md::AbstractString)
    print(io, replace(md, r"[\s\t\n]+" => ' '))
end

function terminline(io::AnnotIO, md::Bold)
    with_output_annotations(io -> terminline(io, md.text), io, :face => :bold)
end

function terminline(io::AnnotIO, md::Italic)
    with_output_annotations(io -> terminline(io, md.text), io, :face => :italic)
end

function terminline(io::IO, md::LineBreak)
    println(io)
end

function terminline(io::IO, md::Image)
    terminline(io, "(Image: $(md.alt))")
end

function terminline(io::IO, f::Footnote)
    print(io, styled"{markdown_footnote:[^$(f.id)]}")
end

function terminline(io::AnnotIO, md::Link)
    annots = if occursin(r"^(https?|file)://", md.url)
        (:face => :markdown_link, :link => md.url)
    else
        (:face => :markdown_link,)
    end
    with_output_annotations(io -> terminline(io, md.text), io, annots...)
end

function terminline(io::IO, code::Code)
    body = if code.language == "styled"
        styled(code.code)
    else
        code.code
    end
    print(io, styled"{markdown_inlinecode:$body}")
end

function terminline(io::IO, tex::LaTeX)
    print(io, styled"{markdown_latex:$(tex.formula)}")
end

function terminline(io::IO, md::MarkdownElement)
    a = IOContext(AnnotatedIOBuffer(), io)
    terminline(a, md)
    print(io, read(seekstart(a.io), AnnotatedString))
end

terminline(io::IO, x) = show(io, MIME"text/plain"(), x)

# Show in terminal
Base.show(io::IO, ::MIME"text/plain", md::MD) = (term(io, md); nothing)
