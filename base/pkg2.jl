module Pkg2

include("pkg2/dir.jl")
include("pkg2/types.jl")
include("pkg2/reqs.jl")
include("pkg2/cache.jl")
include("pkg2/read.jl")
include("pkg2/query.jl")
include("pkg2/resolve.jl")
include("pkg2/write.jl")

using .Types, Base.Git

rm(pkg::String) = edit(Reqs.rm, pkg)
add(pkg::String, vers::VersionSet) = edit(Reqs.add, pkg, vers)
add(pkg::String, vers::VersionNumber...) = add(pkg, VersionSet(vers...))

edit(f::Function, pkg, args...) = Dir.cd() do
    r = Reqs.read("REQUIRE")
    reqs = Reqs.parse(r)
    avail = Read.available()
    if !haskey(avail,pkg) && !haskey(reqs,pkg)
        error("unknown package $pkg")
    end
    r_ = f(r,pkg,args...)
    r_ == r && return info("nothing to be done.")
    reqs_ = Reqs.parse(r_)
    reqs_ != reqs && resolve(reqs_,avail)
    Reqs.write("REQUIRE",r_)
    return
end

update(avail::Dict=Dir.cd(Read.available)) = Dir.cd() do
    info("Updating metadata...")
    cd("METADATA") do
        Git.run(`fetch -q --all`)
        Git.run(`checkout -q HEAD^0`)
        Git.run(`branch -f devel refs/remotes/origin/devel`)
        Git.run(`checkout -q devel`)
        Git.run(`pull -q`)
    end
    fixed = Read.fixed(avail)
    for (pkg,ver) in fixed
        ispath(pkg,".git") || continue
        if Git.attached(dir=pkg) && !Git.isdirty(dir=pkg)
            @recover begin
                Git.run(`fetch -q --all`)
                Git.run(`pull -q`)
            end
        end
        haskey(avail,pkg) &&
            Cache.prefetch(pkg, Read.url(pkg), avail[pkg])
    end
    free = Read.free(avail)
    for (pkg,ver) in free
        Cache.prefetch(pkg, Read.url(pkg), avail[pkg])
    end
    resolve(Reqs.parse("REQUIRE"), avail, fixed, free)
end

resolve(reqs::Dict, avail::Dict=Dir.cd(Read.available),
                    fixed::Dict=Dir.cd(()->Read.fixed(avail)),
                    have::Dict=Dir.cd(()->Read.free(avail))) = Dir.cd() do
    # figure out what should be installed
    reqs  = Query.requirements(reqs,fixed)
    deps  = Query.dependencies(avail,fixed)
    want  = Resolve.resolve(reqs,deps)

    # compare what is installed with what should be
    install, update, remove = Query.diff(have, want)
    if isempty(install) && isempty(update) && isempty(remove)
        return info("no packages to install, update or remove.")
    end

    # prefetch phase isolates network activity, nothing to roll back
    for (pkg,ver) in install
        Cache.prefetch(pkg, Read.url(pkg), Read.sha1(pkg,ver), ver)
    end
    for (pkg,(_,ver)) in update
        Cache.prefetch(pkg, Read.url(pkg), Read.sha1(pkg,ver), ver)
    end

    # try applying changes, roll back everything if anything fails
    installed, updated, removed = {}, {}, {}
    try
        for (pkg,ver) in install
            info("Installing $pkg v$ver")
            Write.install(pkg, Read.sha1(pkg,ver))
            push!(installed,(pkg,ver))
        end
        for (pkg,(v1,v2)) in update
            up = v1 <= v2 ? "Up" : "Down"
            info("$(up)grading $pkg: v$v1 => v$v2")
            Write.update(pkg, Read.sha1(pkg,v2))
            push!(updated,(pkg,(v1,v2)))
        end
        for (pkg,ver) in remove
            info("Removing $pkg v$ver")
            Write.remove(pkg)
            push!(removed,(pkg,ver))
        end
    catch
        for (pkg,ver) in reverse!(removed)
            info("Rolling back deleted $pkg to v$ver")
            @recover Write.install(pkg, Read.sha1(pkg,ver))
        end
        for (pkg,(v1,v2)) in reverse!(updated)
            info("Rolling back $pkg from v$v2 to v$v1")
            @recover Write.update(pkg, Read.sha1(pkg,v1))
        end
        for (pkg,ver) in reverse!(installed)
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
