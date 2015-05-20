# This file is a part of Julia. License is MIT: http://julialang.org/license

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

function html(io::IO, md::BlockQuote)
    withtag(io, :blockquote) do
        println(io)
        html(io, md.content)
    end
end

function latex(io::IO, md::BlockQuote)
    wrapblock(io, "quote") do
        latex(io, md.content)
    end
end

function plain(io::IO, md::BlockQuote)
    print(io, "> ")
    plain(io, md.content)
end

function term(io::IO, md::BlockQuote, columns)
    s = sprint(io->term(io, md.content, columns - 10))
    for line in split(rstrip(s), "\n")
        println(io, " "^margin, "|", line)
    end
    println(io)
end
