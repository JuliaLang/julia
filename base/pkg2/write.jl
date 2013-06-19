module Write

using ..Types, ..Reqs

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

function install(pkg::String, ver::VersionNumber)
    info("Installing $pkg v$ver")
    if ispath(pkg)
        error("Path $pkg already exists! Please remove to allow installation.")
    end
    run(`git clone --reference . $url $pkg`)
    cd(pkg) do
        if !success(`git checkout -q $ver`)
            run(`git fetch -q`)
            try run(`git checkout -q $ver`)
            catch
                error("An invalid SHA1 hash seems to be registered for $pkg. Please contact the package maintainer.")
            end
        end
    end
end

function update(pkg::String, A::VersionNumber, B::VersionNumber)
    info("$(A <= B ? "Up" : "Down")grading $pkg: v$A => v$B")
    cd(pkg) do
        Git.transact() do
            run(`git checkout -q $B`)
        end
    end
end

function remove(pkg::String)
    info("Removing $pkg v$ver")
    run(`rm -rf -- $pkg`)
end

end # module
