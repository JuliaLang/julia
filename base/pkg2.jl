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

edit(f::Function, pkg, args...) = Dir.cd() do
    r = Reqs.read("REQUIRE")
    reqs = Reqs.parse(r)
    avail = Read.available()
    if !haskey(avail,pkg) && !haskey(reqs,pkg)
        error("unknown package $pkg")
    end
    r_ = f(r,pkg,args...)
    r_ == r && return
    reqs_ = Reqs.parse(r_)
    reqs_ != reqs && resolve(reqs_,avail)
    Reqs.write("REQUIRE",r_)
    return
end

rm(pkg::String) = edit(Reqs.rm, pkg)
add(pkg::String, vers::VersionSet) = edit(Reqs.add, pkg, vers)
add(pkg::String, vers::VersionNumber...) = add(pkg, VersionSet(vers...))

macro recover(ex)
    quote
        try $(esc(ex))
        catch err
            show(err)
        end
    end
end

resolve(reqs::Dict, avail::Dict=Dir.cd(Read.available)) = Dir.cd() do
    # figure out what should be installed
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
resolve() = Dir.cd() do
    resolve(Reqs.parse("REQUIRE"))
end

end # module
