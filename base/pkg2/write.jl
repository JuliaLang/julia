module Write

using Base.Git, ..Cache

function install(pkg::String, sha1::String)
    if !isdir(".trash/$pkg")
        Git.run(`clone -q $(Cache.path(pkg)) $pkg`)
    else
        run(`mv .trash/$pkg ./`)
        Git.run(`fetch -q $(Cache.path(pkg))`, dir=pkg)
    end
    Git.run(`config remote.origin.url $(Cache.origin(pkg))`, dir=pkg)
    Git.run(`checkout -q $sha1`, dir=pkg)
end

function update(pkg::String, sha1::String)
    Git.run(`fetch -q $(Cache.path(pkg))`, dir=pkg)
    Git.run(`config remote.origin.url $(Cache.origin(pkg))`, dir=pkg)
    Git.run(`checkout -q $sha1`, dir=pkg)
end

function remove(pkg::String)
    isdir(".trash") || mkdir(".trash")
    ispath(".trash/$pkg") && run(`rm -rf .trash/$pkg`)
    run(`mv $pkg .trash/`)
end

end # module
