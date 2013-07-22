module Pkg2

include("pkg2/dir.jl")
include("pkg2/types.jl")
include("pkg2/reqs.jl")
include("pkg2/cache.jl")
include("pkg2/read.jl")
include("pkg2/query.jl")
include("pkg2/resolve.jl")
include("pkg2/write.jl")
include("pkg2/extdeps.jl")

using Base.Git, .Types

rm(pkg::String) = edit(Reqs.rm, pkg)
add(pkg::String, vers::VersionSet) = edit(Reqs.add, pkg, vers)
add(pkg::String, vers::VersionNumber...) = add(pkg, VersionSet(vers...))
init(meta::String=Dir.DEFAULT_META) = Dir.init(meta)

edit(f::Function, pkg, args...) = Dir.cd() do
    r = Reqs.read("REQUIRE")
    reqs = Reqs.parse(r)
    avail = Read.available()
    if !haskey(avail,pkg) && !haskey(reqs,pkg)
        error("unknown package $pkg")
    end
    r_ = f(r,pkg,args...)
    r_ == r && return info("Nothing to be done.")
    reqs_ = Reqs.parse(r_)
    reqs_ != reqs && resolve(reqs_,avail)
    Reqs.write("REQUIRE",r_)
    info("REQUIRE updated.")
end

available() = sort!([keys(Pkg2.Dir.cd(Pkg2.Read.available))...], by=lowercase)

status() = Dir.cd() do
    reqs = Reqs.parse("REQUIRE")
    instd = Read.installed()
    println("Required:")
    for pkg in sort!([keys(reqs)...])
        ver,fix = delete!(instd,pkg)
        status(pkg,ver,fix)
    end
    println("Additional:")
    for pkg in sort!([keys(instd)...])
        ver,fix = instd[pkg]
        status(pkg,ver,fix)
    end
end
function status(pkg::String, ver::VersionNumber, fix::Bool)
    @printf " - %-29s " pkg
    fix || return println(ver)
    @printf "%-10s" ver
    print(" fixed: ")
    if ispath(Dir.path(pkg,".git"))
        print(Git.attached(dir=pkg) ? Git.branch(dir=pkg) : Git.head(dir=pkg)[1:8])
        Git.dirty(dir=pkg) && print("*")
    else
        print("non-repo")
    end
    println()
end

urlpkg(url::String) = match(r"/(\w+?)(?:\.jl)?(?:\.git)?$", url).captures[1]

clone(url::String, pkg::String=urlpkg(url); opts::Cmd=``) = Dir.cd() do
    ispath(pkg) && error("$pkg already exists")
    try Git.run(`clone $opts $url $pkg`)
    catch
        run(`rm -rf $pkg`)
        rethrow()
    end
    isempty(Reqs.parse("$pkg/REQUIRE")) && return
    info("Computing changes...")
    resolve()
end

update() = Dir.cd() do
    info("Updating METADATA...")
    cd("METADATA") do
        if Git.branch() != "devel"
            Git.run(`fetch -q --all`)
            Git.run(`checkout -q HEAD^0`)
            Git.run(`branch -f devel refs/remotes/origin/devel`)
            Git.run(`checkout -q devel`)
        end
        Git.run(`pull -q`)
    end
    avail = Read.available()
    # this has to happen before computing free/fixed
    for pkg in filter!(Read.isinstalled,[keys(avail)...])
        Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
    end
    instd = Read.installed(avail)
    free = Read.free(instd)
    for (pkg,ver) in free
        Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
    end
    fixed = Read.fixed(avail,instd)
    for (pkg,ver) in fixed
        ispath(pkg,".git") || continue
        if Git.attached(dir=pkg) && !Git.dirty(dir=pkg)
            info("Updating $pkg...")
            @recover begin
                Git.run(`fetch -q --all`, dir=pkg)
                Git.success(`pull -q --ff-only`, dir=pkg) # suppress output
            end
        end
        if haskey(avail,pkg)
            Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
        end
    end
    info("Computing changes...")
    resolve(Reqs.parse("REQUIRE"), avail, instd, fixed, free)
end

resolve(
    reqs  :: Dict,
    avail :: Dict = Dir.cd(Read.available),
    instd :: Dict = Dir.cd(()->Read.installed(avail)),
    fixed :: Dict = Dir.cd(()->Read.fixed(avail,instd)),
    have  :: Dict = Dir.cd(()->Read.free(instd))
) = Dir.cd() do

    reqs = Query.requirements(reqs,fixed)
    deps = Query.dependencies(avail,fixed)

    for pkg in keys(reqs)
        haskey(deps, pkg) ||
            error("$pkg has no version compatible with fixed requirements")
    end

    deps = Query.prune_dependencies(reqs,deps)

    want = Resolve.resolve(reqs,deps)

    # compare what is installed with what should be
    changes = Query.diff(have, want, avail, fixed)
    if isempty(changes)
        return info("No packages to install, update or remove.")
    end

    # prefetch phase isolates network activity, nothing to roll back
    missing = {}
    for (pkg,(ver1,ver2)) in changes
        vers = ASCIIString[]
        ver1 !== nothing && push!(vers,Git.head(dir=pkg))
        ver2 !== nothing && push!(vers,Read.sha1(pkg,ver2))
        append!(missing,
            map(sha1->(pkg,(ver1,ver2),sha1),
                Cache.prefetch(pkg, Read.url(pkg), vers)))
    end
    if !isempty(missing)
        msg = "unfound package versions (possible metadata misconfiguration):"
        for (pkg,ver,sha1) in missing
            msg *= "  $pkg v$ver [$sha1[1:10]]\n"
        end
        error(msg)
    end

    # try applying changes, roll back everything if anything fails
    changed = {}
    try
        for (pkg,(ver1,ver2)) in changes
            if ver1 === nothing
                info("Installing $pkg v$ver2")
                Write.install(pkg, Read.sha1(pkg,ver2))
            elseif ver2 == nothing
                info("Removing $pkg v$ver1")
                Write.remove(pkg)
            else
                up = ver1 <= ver2 ? "Up" : "Down"
                info("$(up)grading $pkg: v$ver1 => v$ver2")
                Write.update(pkg, Read.sha1(pkg,ver2))
            end
            push!(changed,(pkg,(ver1,ver2)))
        end
    catch
        for (pkg,(ver1,ver2)) in reverse!(changed)
            if ver1 == nothing
                info("Rolling back install of $pkg")
                @recover Write.remove(pkg)
            elseif ver2 == nothing
                info("Rolling back deleted $pkg to v$ver1")
                @recover Write.install(pkg, Read.sha1(pkg,ver1))
            else
                info("Rolling back $pkg from v$ver2 to v$ver1")
                @recover Write.update(pkg, Read.sha1(pkg,ver1))
            end
        end
        rethrow()
    end

    # run build scripts
    lines = ExtDeps.read("WORKING")
    for (pkg,_) in changes  
        contains(lines,ExtDeps.Record(pkg)) && ExtDeps.rm(lines,pkg)
    end
    Reqs.write("WORKING",lines)
    for (pkg,_) in changes
        if !runbuildscript(pkg)
            return
        end
    end
end

function runbuildscript(pkg,args=[])
    try 
        path = Pkg2.Dir.path(pkg,"deps","build.jl")
        if isfile(path)
            info("Running build script for package $pkg")
            Dir.cd(joinpath(Dir.path(pkg),"deps")) do
                m = Module(:__anon__)
                body = Expr(:toplevel,:(ARGS=$args),:(include($path)))
                eval(m,body)
            end
        end
    catch
        warn("""
             An exception occured while building binary dependencies.
             You may have to take manual steps to complete the installation, see the error message below.
             To reattempt the installation, run Pkg.fixup("$pkg").
             """)
        rethrow()
    end
    true
end

resolve() = Dir.cd() do
    resolve(Reqs.parse("REQUIRE"))
end

# Metadata sanity check
check_metadata(julia_version::VersionNumber=VERSION) = Dir.cd() do
    avail = Read.available()
    instd = Read.installed(avail)
    fixed = Read.fixed(avail,instd,julia_version)
    deps  = Query.dependencies(avail,fixed)

    problematic = Resolve.sanity_check(deps)
    if !isempty(problematic)
        warning = "Packages with unsatisfiable requirements found:\n"
        for (p, vn, rp) in problematic
            warning *= "    $p v$vn : no valid versions exist for package $rp\n"
        end
        warn(warning)
        return false
    end
    return true
end
check_metadata(julia_version::String) = check_metadata(convert(VersionNumber, julia_version))

function _fixup(instlist;exclude=[])
    for (p,_) in instlist
        if contains(exclude,p)
            continue
        end
        if !runbuildscript(p,["fixup"])
            return
        end
    end
    info("SUCCESS!")
end

function fixup(
        lines::Vector{Reqs.Line}=Dir.cd(()->ExtDeps.read("WORKING")),
        avail=Dir.cd(Read.available),
        inst::Dict=Dir.cd(()->Read.installed(avail)),
        free::Dict=Dir.cd(()->Read.free(inst)),
        fixed::Dict=Dir.cd(()->Read.fixed(avail,inst));exclude = []) 
    # TODO: Replace by proper toposorts
    instlist = [(k,v) for (k,v) in inst]
    sort!(instlist, lt=function(a,b)
        ((a,vera),(b,verb)) = (a,b)
        c = contains(Pkg2.Read.alldependencies(a,avail,free,fixed),b) 
        nonordered = (!c && !contains(Pkg2.Read.alldependencies(b,avail,free,fixed),a))
        nonordered ? a < b : c
    end)
    _fixup(instlist,exclude=exclude)
end

end # module
