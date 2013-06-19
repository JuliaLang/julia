module Write

using ..Types, ..Reqs, ..Read, ..Cache

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
    url = Cache.origin(pkg)
    cache = abspath(Cache.path(pkg))
    run(`git clone -q $cache`)
    cd(pkg) do
        run(`git config remote.origin.url $url`)
        run(`git checkout -q $sha1`)
    end
end

function update(pkg::String, sha1::String)
    url = Cache.origin(pkg)
    cache = abspath(Cache.path(pkg))
    cd(pkg) do
        run(`git config remote.origin.url $url`)
        run(`git fetch -q --tags $cache`)
        run(`git checkout -q $sha1`)
    end
end

function remove(pkg::String)
    run(`rm -rf -- $pkg`)
end

end # module
