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
    Write.edit("REQUIRE", Reqs.add, pkg, vers)
end
add(pkg::String, vers::VersionNumber...) = add(pkg, VersionSet(vers...))

rm(pkg::String) = Dir.cd() do
    Write.edit("REQUIRE", Reqs.rm, pkg)
end

resolve() = Dir.cd() do
    # figure out what should be installed
    reqs  = Reqs.parse("REQUIRE")
    avail = Read.available()
    fixed = Read.fixed(avail)
    reqs  = Query.requirements(reqs,fixed)
    deps  = Query.dependencies(avail,fixed)
    want  = Resolve.resolve(reqs,deps)
    have  = Read.free(avail)

    # compare what is installed with what should be
    install, update, remove = Query.diff(have, want)

    # prefetch phase isolates network activity, nothing to roll back
    for (pkg,ver) in install
        Cache.fetch(pkg, Read.url(pkg), ver, Read.sha1(pkg,ver))
    end
    for (pkg,(_,ver)) in update
        Cache.fetch(pkg, Read.url(pkg), ver, Read.sha1(pkg,ver))
    end

    # try applying changes, roll back everything if anything fails
    try
        for (pkg,ver) in install
            Write.install(pkg,ver)
        end
        for (pkg,(v1,v2)) in update
            Write.update(pkg,v1,v2)
        end
        for (pkg,ver) in remove
            Write.remove(pkg)
        end
    catch
        for (pkg,ver) in remove
            Write.install!(pkg,ver)
        end
        for (pkg,(v1,v2)) in update
            Write.update!(pkg,v2,v1)
        end
        for (pkg,ver) in install
            Write.remove!(pkg)
        end
        rethrow()
    end
end

end # module
