# This file is a part of Julia. License is MIT: https://julialang.org/license

# Wrapping

function ansi_length(s)
    replace(s, r"\e\[[0-9]+m" => "") |> length
end

words(s) = split(s, " ")
lines(s) = split(s, "\n")

function wrapped_line(io::IO, s::AbstractString, width, i)
    ws = words(s)
    lines = String[]
    for word in ws
        word_length = ansi_length(word)
        word_length == 0 && continue
        if isempty(lines) || i + word_length + 1 > width
            i = word_length
            push!(lines, word)
        else
            i += word_length + 1
            lines[end] *= " " * word   # this could be more efficient
        end
    end
    return i, lines
end

function wrapped_lines(io::IO, s::AbstractString; width = 80, i = 0)
    ls = String[]
    for ss in lines(s)
        i, line = wrapped_line(io, ss, width, i)
        append!(ls, line)
    end
    return ls
end

wrapped_lines(io::IO, f::Function, args...; width = 80, i = 0) =
    wrapped_lines(io, sprint(f, args...; context=io), width = width, i = 0)

function print_wrapped(io::IO, s...; width = 80, pre = "", i = 0)
    lines = wrapped_lines(io, s..., width = width, i = i)
    isempty(lines) && return 0, 0
    print(io, lines[1])
    for line in lines[2:end]
        print(io, '\n', pre, line)
    end
    length(lines), length(pre) + ansi_length(lines[end])
end

print_wrapped(f::Function, io::IO, args...; kws...) = print_wrapped(io, f, args...; kws...)
