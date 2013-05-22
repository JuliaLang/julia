module Write

using ..Types, ..Reqs

function update_file(f::Function, file::String, args...)
	tmp = "$file.$(randstring()).tmp"
	ispath(tmp) && error("tempfile $tmp already exists!?")
	try
		x = open(file) do input
			open(tmp,"w") do output
				f(input, output, args...)
			end
		end
		run(`mv -f $tmp $file`)
		return x
	catch
		ispath(tmp) && rm(tmp)
		rethrow()
	end
end

end # module
