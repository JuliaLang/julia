include("../base/latex_symbols.jl")

#Create list of different tab-completions for a given character
#Sometimes there is more than one way...
latexvals = Dict()
for (key, val) in latex_symbols
	latexvals[val] = push!(get(latexvals, val, String[]), "\\"*key)
end

#Render list
#Need to do this in two passes since ReST complains if the tables aren't exactly aligned
#Pass 1. Generate strings
entries = Any[("Code point(s)", "Character(s)", "Tab completion sequence(s)")]
maxlen = [map(length, entries[1])...]
for (chars, inputs) in sort!([x for x in latexvals])
	#Find all keys with this value
	entry = (
			join(map(c->"U+"*uppercase(hex(c, 5)), collect(chars))),
			chars,
			join(inputs, ", ")
		)

	currentlength = map(length, entry)
	for i=1:3
		maxlen[i] = max(maxlen[i], currentlength[i])
	end

	push!(entries, entry)
end

#Pass 2. Print table in ReST simple table format
function underline(str, maxlen)
	join(map(n->str^n, maxlen), " ")
end

println(":orphan:\n")
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
#	break
end
println(underline("=", maxlen))
