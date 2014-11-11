# Styles

const text_formats = Dict(
  :black   => "\e[30m",
  :red     => "\e[31m",
  :green   => "\e[32m",
  :yellow  => "\e[33m",
  :blue    => "\e[34m",
  :magenta => "\e[35m",
  :cyan    => "\e[36m",
  :white   => "\e[37m",
  :reset   => "\e[0m",
  :bold    => "\e[1m",
  :underline => "\e[4m",
  :blink     => "\e[5m",
  :negative  => "\e[7m")

function with_output_format(f::Function, formats::Vector{Symbol}, io::IO, args...)
  Base.have_color && for format in formats
    print(io, get(text_formats, format, ""))
  end
  try f(io, args...)
  finally
    Base.have_color && print(io, text_formats[:reset])
  end
end

with_output_format(f::Function, format::Symbol, args...) =
  with_output_format(f, [format], args...)

with_output_format(format, f::Function, args...) =
  with_output_format(f, format, args...)

function print_with_format(format, io::IO, x)
  with_output_format(format, io) do io
    print(io, x)
  end
end

function println_with_format(format, io::IO, x)
  print_with_format(format, io, x)
  println(io)
end

# Wrapping

function ansi_length(s)
  replace(s, r"\e\[[0-9]+m", "") |> length
end

words(s) = split(s, " ")
lines(s) = split(s, "\n")

# This could really be more efficient
function wrapped_lines(s::String; width = 80, i = 0)
  if ismatch(r"\n", s)
    return [map(s->wrapped_lines(s, width = width, i = i), split(s, "\n"))...]
  end
  ws = words(s)
  lines = String[ws[1]]
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

wrapped_lines(f::Function, args...; width = 80, i = 0) =
  wrapped_lines(sprint(f, args...), width = width, i = 0)

function print_wrapped(io::IO, s...; width = 80, pre = "", i = 0)
  lines = wrapped_lines(s..., width = width, i = i)
  println(io, lines[1])
  for line in lines[2:end]
    println(io, pre, line)
  end
end

print_wrapped(f::Function, io::IO, args...; kws...) = print_wrapped(io, f, args...; kws...)

function print_centred(io::IO, s...; columns = 80, width = columns)
  lines = wrapped_lines(s..., width = width)
  for line in lines
    print(io, " "^(div(columns-ansi_length(line), 2)))
    println(io, line)
  end
end

function centred(s, columns)
  pad = div(columns - ansi_length(s), 2)
  " "^pad * s
end
