module Write

using ..Types, ..Reqs

function update_file(f::Function, file::String, args...)
    tmp = "$file.$(randstring()).tmp"
    ispath(tmp) && error("tempfile $tmp already exists!?")
    try
        replace = open(file) do input
            open(tmp,"w") do output
                f(input, output, args...)
            end
        end
        replace && run(`mv -f $tmp $file`)
        return replace
    catch
        ispath(tmp) && rm(tmp)
        rethrow()
    end
end

end # module
