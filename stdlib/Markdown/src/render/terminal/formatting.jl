# This file is a part of Julia. License is MIT: https://julialang.org/license

# Wrapping

function ansi_length(s)
    replace(s, r"\e\[[0-9]+m" => "") |> length
end

words(s) = split(s, " ")
lines(s) = split(s, "\n")

# This could really be more efficient
function wrapped_lines(io::IO, s::AbstractString; width = 80, i = 0)
    if occursin(r"\n", s)
        return vcat(map(s->wrapped_lines(io, s, width = width, i = i), split(s, "\n"))...)
    end
    ws = words(s)
    lines = AbstractString[ws[1]]
    i += ws[1] |> ansi_length
    for word in ws[2:end]
        word_length = ansi_length(word)
        if i + word_length + 1 > width
            i = word_length
            push!(lines, word)
        else
            i += word_length + 1
            lines[end] *= " " * word
        end
    end
    return lines
end

wrapped_lines(io::IO, f::Function, args...; width = 80, i = 0) =
    wrapped_lines(io, sprint(f, args...; context=io), width = width, i = 0)

function print_wrapped(io::IO, s...; width = 80, pre = "", i = 0)
    lines = wrapped_lines(io, s..., width = width, i = i)
    print(io, lines[1])
    for line in lines[2:end]
        print(io, '\n', pre, line)
    end
    length(lines), length(pre) + ansi_length(lines[end])
end

print_wrapped(f::Function, io::IO, args...; kws...) = print_wrapped(io, f, args...; kws...)
