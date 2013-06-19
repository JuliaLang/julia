module Pkg2

include("pkg2/dir.jl")
include("pkg2/types.jl")
include("pkg2/reqs.jl")
include("pkg2/read.jl")
include("pkg2/query.jl")
include("pkg2/resolve.jl")
include("pkg2/cache.jl")
include("pkg2/write.jl")

using .Types

add(pkg::String, vers::VersionSet) = Dir.cd() do
    avail = Read.available()
    haskey(avail,pkg) || error("unknown package $pkg")
    Write.edit("REQUIRE", Reqs.add, pkg, vers)
    resolve(avail)
    # TODO: roll back REQUIRE on failure
end
add(pkg::String, vers::VersionNumber...) = add(pkg, VersionSet(vers...))

rm(pkg::String) = Dir.cd() do
    avail = Read.available()
    Write.edit("REQUIRE") do input, output
        altered = Reqs.rm(input, output, pkg)
        altered || haskey(avail,pkg) || error("unknown package $pkg")
        altered
    end
    resolve(avail)
    # TODO: roll back REQUIRE on failure
end

macro recover(ex)
    quote
        try $(esc(ex))
        catch err
            show(err)
        end
    end
end

resolve(avail::Dict=Dir.cd(Read.available)) = Dir.cd() do
    # figure out what should be installed
    reqs  = Reqs.parse("REQUIRE")
    fixed = Read.fixed(avail)
    reqs  = Query.requirements(reqs,fixed)
    deps  = Query.dependencies(avail,fixed)
    want  = Resolve.resolve(reqs,deps)
    have  = Read.free(avail)

    # compare what is installed with what should be
    install, update, remove = Query.diff(have, want)

    # prefetch phase isolates network activity, nothing to roll back
    for (pkg,ver) in install
        Cache.prefetch(pkg, Read.url(pkg), ver, Read.sha1(pkg,ver))
    end
    for (pkg,(_,ver)) in update
        Cache.prefetch(pkg, Read.url(pkg), ver, Read.sha1(pkg,ver))
    end

    # try applying changes, roll back everything if anything fails
    try
        for (pkg,ver) in install
            info("Installing $pkg v$ver")
            Write.install(pkg, Read.sha1(pkg,ver))
        end
        for (pkg,(v1,v2)) in update
            up = v1 <= v2 ? "Up" : "Down"
            info("$(up)grading $pkg: v$v1 => v$v2")
            Write.update(pkg, Read.sha1(pkg,v2))
        end
        for (pkg,ver) in remove
            info("Removing $pkg v$ver")
            Write.remove(pkg)
        end
    catch
        for (pkg,ver) in remove
            info("Rolling back $pkg to v$ver")
            @recover Write.install(pkg, Read.sha1(pkg,ver))
        end
        for (pkg,(v1,v2)) in update
            info("Rolling back $pkg to v$v1")
            @recover Write.update(pkg, Read.sha1(pkg,v1))
        end
        for (pkg,ver) in install
            info("Rolling back install of $pkg")
            @recover Write.remove(pkg)
        end
        rethrow()
    end
end

end # module
