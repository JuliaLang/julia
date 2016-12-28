# Unicode Input

!!! warning

    This table may appear to contain missing characters in the second column, or even
    show characters that are inconsistent with the characters as they are rendered in
    the Julia REPL. In these cases, users are strongly advised to check their choice
    of fonts in their browser and REPL environment, as there are known issues with
    glyphs in many fonts.

```@eval
#
# Generate a table containing all LaTeX and Emoji tab completions available in the REPL.
#

function tab_completions(symbols...)
    completions = Dict{String, Vector{String}}()
    for each in symbols, (k, v) in each
        completions[v] = push!(get!(completions, v, String[]), k)
    end
    return completions
end

function unicode_data()
    file = normpath(JULIA_HOME, "..", "..", "doc", "UnicodeData.txt")
    names = Dict{UInt32, String}()
    open(file) do unidata
        for line in readlines(unidata)
            id, name, desc = split(line, ";")[[1, 2, 11]]
            codepoint = parse(UInt32, "0x$id")
            names[codepoint] = name == "" ? desc : desc == "" ? name : "$name / $desc"
        end
    end
    return names
end

function table_entries(completions, unicode_dict)
    entries = [[
        "Code point(s)", "Character(s)",
        "Tab completion sequence(s)", "Unicode name(s)"
    ]]
    for (chars, inputs) in sort!(collect(completions), by = first)
        code_points, unicode_names, characters = String[], String[], String[]
        for char in chars
            push!(code_points, "U+$(uppercase(hex(char, 5)))")
            push!(unicode_names, get(unicode_dict, UInt32(char), "(No Unicode name)"))
            push!(characters, Base.UTF8proc.category_code(char) == 6 ? "◌$char" : "$char")
        end
        push!(entries, [
            join(code_points, " + "), join(chars),
            join(inputs, ", "), join(unicode_names, " + ")
        ])
    end
    return Markdown.Table(entries, [:l, :l, :l, :l])
end

table_entries(
    tab_completions(
        Base.REPLCompletions.latex_symbols,
        Base.REPLCompletions.emoji_symbols
    ),
    unicode_data()
)
```
