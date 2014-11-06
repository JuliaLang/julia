#Generate list of LaTeX tab-completions supported by the Julia REPL
#as documented in doc/manual/unicode-input-table.rst
#The output will be rendered as a reStructuredText document to STDOUT
#Note: This script will download a file called UnicodeData.txt from the Unicode
#Consortium

include("../base/latex_symbols.jl")

#Create list of different tab-completions for a given character
#Sometimes there is more than one way...
latexvals = Dict()
for (key, val) in latex_symbols
    latexvals[val] = push!(get(latexvals, val, AbstractString[]), "\\"*key)
end

#Join with Unicode names to aid in lookup
isfile("UnicodeData.txt") || download(
    "http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt", "UnicodeData.txt")
unicodenames = Dict()
open("UnicodeData.txt") do unidata
    for line in readlines(unidata)
        tokens = split(line, ";")
        codepoint = uint32("0x"tokens[1])
        name = tokens[ 2]=="" ? tokens[11] :
               tokens[11]=="" ? tokens[ 2] :
               tokens[ 2]*" / "*tokens[11]
        unicodenames[codepoint] = name
    end
end

#Render list
#Need to do this in two passes since ReST complains if the tables aren't exactly aligned
#Pass 1. Generate strings
entries = Any[("Code point(s)", "Character(s)", "Tab completion sequence(s)", "Unicode name(s)")]
maxlen = [map(length, entries[1])...]
for (chars, inputs) in sort!([x for x in latexvals])
    #Find all keys with this value
    entry = (
            join(map(c->"U+"*uppercase(hex(c, 5)), collect(chars)), " + "),
            chars,
            join(inputs, ", "),
            join(map(c->unicodenames[c], collect(chars)), " + ")
        )

    currentlength = map(length, entry)
    for i=1:length(entry)
        maxlen[i] = max(maxlen[i], currentlength[i])
    end

    push!(entries, entry)
end

#Pass 2. Print table in ReST simple table format
function underline(str, maxlen)
    join(map(n->str^n, maxlen), " ")
end

isheader = true
println(underline("=", maxlen))
for entry in entries
    for (i, col) in enumerate(entry)
        print(rpad(col, maxlen[i], " "), " ")
    end
    println()
    if isheader
        println(underline("-", maxlen))
        isheader = false
    end
end
println(underline("=", maxlen))
