module Write

using Base.Git, ..Cache

function install(pkg::String, sha1::String)
    ispath(pkg) && error("path $pkg already exists! please remove to allow installation.")
    try
        Git.run(`clone -q $(Cache.path(pkg)) $pkg`)
        Git.run(`config remote.origin.url $(Cache.origin(pkg))`, dir=pkg)
        Git.run(`checkout -q $sha1`, dir=pkg)
    catch
        run(`rm -rf $pkg`)
        rethrow()
    end
end

function update(pkg::String, sha1::String)
    prev = Git.head(dir=pkg)
    try
        Git.run(`config remote.origin.url $(Cache.origin(pkg))`, dir=pkg)
        Git.run(`fetch -q $(Cache.path(pkg))`, dir=pkg)
        Git.run(`checkout -q $sha1`, dir=pkg)
    catch
        Git.run(`checkout -q $prev`, dir=pkg)
        rethrow()
    end
end

function remove(pkg::String)
    run(`rm -rf -- $pkg`)
end

end # module
