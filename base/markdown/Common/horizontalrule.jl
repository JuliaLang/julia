# This file is a part of Julia. License is MIT: http://julialang.org/license

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

function html(io::IO, md::HorizontalRule)
    tag(io, :hr)
end

function latex(io::IO, md::HorizontalRule)
    println(io, "\\rule{\\textwidth}{1pt}")
end

function plain(io::IO, md::HorizontalRule)
    println(io, "–" ^ 3)
end

function term(io::IO, br::HorizontalRule, columns)
   println(io, " " ^ margin, "-" ^ (columns - 2margin))
end
