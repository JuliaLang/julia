function _replace(s, pairs::Pair...)
    for p in pairs
        s = replace(s, p)
    end
    return s
end

# Convert flisp definitions and comments to psuedo-Julia to reflect the
# structure of the existing flisp parser.
#
# Surrounded with all this compiler technology, but still resorting to a pile
# of regexs? 😂😱
function juliafy_flisp(fl_input, jl_output)
    prev_newline = false
    had_comment = false
    for line in readlines(fl_input)
        if occursin(r"^\(define *\(", line)
            had_comment && println(jl_output, "#")
            println(jl_output, "# flisp: $line")
            m = match(r"\(define *\(([-a-zA-Z?_=0-9*><:!]+) *([^)]*)", replace(line, "-"=>"_"))
            isnothing(m) && @error "no match for line" line
            funcname = m[1]
            funcname = _replace(funcname,
                r"(.*)\?"=>s"is_\1",
                "=" => "equals",
                "*" => "_star",
                ">" => "_gt",
                "<" => "_lt",
                ":" => "_",
            )
            funcargs = _replace(m[2],
                r" *\("                 => ";",
                r" +"                   => ", ",
                "."                     => "_",
                r"([-a-zA-Z?_=]+)\?"    => s"is_\1",
                r", *#t"                => "=true",
                r", *#f"                => "=false",
                ";"                     => "; ",
            )
            if startswith(funcname, "parse_")
                funcargs = "ps::ParseState, "*funcargs
            end
            text = """
            function $funcname($funcargs)
                TODO("$funcname unimplemented")
            end
            """
            ex = Meta.parse(text, raise=false)
            if Meta.isexpr(ex, :error)
                @warn "Generated bad code"  message=ex.args[1] code=Text(text)
            end
            print(jl_output, text)
            prev_newline = false
            had_comment = false
        elseif occursin(r"^;;", line)
            println(jl_output, replace(line, r"^;;" => "#"))
            prev_newline = false
            had_comment = true
        elseif line == ""
            if !prev_newline
                println(jl_output)
            end
            prev_newline = true
            had_comment = false
        end
    end
end

open("/home/chris/dev/julia/src/julia-parser.scm", "r") do fl_input
    open(joinpath(@__DIR__, "julia_parser_scm.jl"), "w") do jl_output
        juliafy_flisp(fl_input, jl_output)
    end
end

