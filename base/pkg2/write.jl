module Write

using Base.Git, ..Cache

function edit(f::Function, file::String, args...)
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
    finally
        ispath(tmp) && rm(tmp)
    end
end
edit(file::String, f::Function, args...) = edit(f, file, args...)

function install(pkg::String, sha1::String)
    ispath(pkg) && error("path $pkg already exists! please remove to allow installation.")
    Git.run(`clone -q $(Cache.path(pkg))`)
    Git.run(`config remote.origin.url $(Cache.origin(pkg))`, dir=pkg)
    Git.run(`checkout -q $sha1`, dir=pkg)
end

function update(pkg::String, sha1::String)
    Git.run(`config remote.origin.url $(Cache.origin(pkg))`, dir=pkg)
    Git.run(`fetch -q $(Cache.path(pkg))`, dir=pkg)
    Git.run(`checkout -q $sha1`, dir=pkg)
end

function remove(pkg::String)
    run(`rm -rf -- $pkg`)
end

end # module
