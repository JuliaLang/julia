
"""
    Like printstyled, but allows providing RGB colors for true color terminals
"""
function _printstyled(io::IO, text; color)
    if length(color) != 3 || !all(0 .<= color .< 256)
        error("Invalid ansi color $color")
    end
    colcode = "\e[48;2;$(color[1]);$(color[2]);$(color[3])m"
    colreset = "\e[0;0m"
    first = true
    for linepart in split(text, '\n')
        first || print('\n')
        print(colcode, linepart, colreset)
        first = false
    end
end

