using .LineNumbers: SourceFile, compute_line
using CSTParser
using CSTParser: Error

function display_diagnostic(io::IO, code, diagnostics::Vector{Error}; filename = "none")
    file = SourceFile(code)
    for (i,message) in enumerate(diagnostics)
        if isempty(message.loc)
            i != 1 && (printstyled(io, "ERROR", color=:red, bold=true); print(io, ": "))
            println(io, message.description)
            continue
        end
        offset = first(message.loc)
        line = compute_line(file, offset)
        str  = String(file[line])
        lineoffset = offset - file.offsets[line]
        col  = (lineoffset == 0 || isempty(str)) ? 1 :
               lineoffset > sizeof(str) ? textwidth(str) + 1 :
                textwidth(str[1:lineoffset])
        if false #message.severity == :fixit
            print(io, " "^(col-1))
            printstyled(io, message.text, color=:green)
            println(io)
        else
            print(io, "$filename:$line:$col " )
            i != 1 && (printstyled(io, "ERROR", color=:red, bold=true); print(io, ": "))
            println(io, message.description)
            println(io, rstrip(str))
            print(io, " "^(col-1))
            printstyled(io, string('^',"~"^(max(0,length(message.loc)-1))); color=:green, bold = true)
            println(io)
        end
    end
end
