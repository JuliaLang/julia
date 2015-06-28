const d = Dict()

open("base/docs/helpdb.jl") do fh
	while !eof(fh)
		a = readline(fh)
		if startswith(a, "doc")
			b = ""
			while true
				c = readline(fh)
				startswith(c, "\"\"\"") && break
				b *= c
			end
			k = chomp(readline(fh))
			push!(get!(()->Any[], d, k), b)
		end
	end
end

open("base/docs/helpdb.jl", "w") do fh
	for k in sort!(collect(keys(d)))
		print(fh, """
		doc\"\"\"
		$(join(d[k], "\n"))
		\"\"\"
		$k

		""")
	end
end
