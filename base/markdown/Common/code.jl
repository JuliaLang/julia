# This file is a part of Julia. License is MIT: http://julialang.org/license

type Code
    language::UTF8String
    code::UTF8String
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

@trigger '`' ->
function inline_code(stream::IO, md::MD)
    result = parse_inline_wrapper(stream, "`"; rep=true)
    return result == nothing ? nothing : Code(result)
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
                    push!(block, Code(flavor, takebuf_string(buffer) |> chomp))
                    return true
                else
                    seek(stream, line_start)
                end
            end
            write(buffer, readline(stream))
        end
        return false
    end
end

function term(io::IO, md::Code, columns)
    with_output_format(:cyan, io) do io
        for line in lines(md.code)
            print(io, " "^margin)
            println(io, line)
        end
    end
end

function terminline(io::IO, code::Code)
    print_with_format(:cyan, io, code.code)
end

function html(io::IO, code::Code)
    withtag(io, :pre) do
        maybe_lang = code.language != "" ? Any[:class=>"language-$(code.language)"] : []
        withtag(io, :code, maybe_lang...) do
            htmlesc(io, code.code)
            # TODO should print newline if this is longer than one line ?
        end
    end
end

function htmlinline(io::IO, code::Code)
    withtag(io, :code) do
        htmlesc(io, code.code)
    end
end

function latex(io::IO, code::Code)
    wrapblock(io, "verbatim") do
        # TODO latex escape
        println(io, code.code)
    end
end

function latexinline(io::IO, code::Code)
    wrapinline(io, "texttt") do
        print(io, code.code)
    end
end

function plain(io::IO, code::Code)
    println(io, "```", code.language)
    println(io, code.code)
    println(io, "```")
end

plaininline(io::IO, md::Code) = print(io, "`", md.code, "`")
