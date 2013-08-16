module Pkg

include("pkg/dir.jl")
include("pkg/types.jl")
include("pkg/reqs.jl")
include("pkg/cache.jl")
include("pkg/read.jl")
include("pkg/query.jl")
include("pkg/resolve.jl")
include("pkg/write.jl")

using Base.Git, .Types

const dir = Dir.path

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
    reqs_ != reqs && _resolve(reqs_,avail)
    Reqs.write("REQUIRE",r_)
    info("REQUIRE updated.")
end

available() = sort!([keys(Dir.cd(Read.available))...], by=lowercase)

function installed()
    pkgs = Dict{String,VersionNumber}()
    for (pkg,(ver,fix)) in Dir.cd(Read.installed)
        pkgs[pkg] = ver
    end
    return pkgs
end
installed(pkg::String) = Dir.cd() do
    avail = Read.available()
    Read.isinstalled(pkg) ? Read.installed_version(pkg,avail[pkg]) :
        haskey(avail,pkg) ? nothing :
            error("$pkg is neither installed nor registered")
end

status(io::IO=STDOUT) = Dir.cd() do
    reqs = Reqs.parse("REQUIRE")
    instd = Read.installed()
    println(io, "Required packages:")
    for pkg in sort!([keys(reqs)...])
        ver,fix = pop!(instd,pkg)
        status(io,pkg,ver,fix)
    end
    println(io, "Additional packages:")
    for pkg in sort!([keys(instd)...])
        ver,fix = instd[pkg]
        status(io,pkg,ver,fix)
    end
end
function status(io::IO, pkg::String, ver::VersionNumber, fix::Bool)
    @printf io " - %-29s " pkg
    fix || return println(io,ver)
    @printf io "%-19s" ver
    if ispath(Dir.path(pkg,".git"))
        print(io, Git.attached(dir=pkg) ? Git.branch(dir=pkg) : Git.head(dir=pkg)[1:8])
        Git.dirty(dir=pkg) && print(io, " (dirty)")
    else
        print(io, "non-repo")
    end
    println(io)
end

urlpkg(url::String) = match(r"/(\w+?)(?:\.jl)?(?:\.git)?$", url).captures[1]

clone(url::String, pkg::String=urlpkg(url); opts::Cmd=``) = Dir.cd() do
    info("Cloning $pkg from $url")
    ispath(pkg) && error("$pkg already exists")
    try Git.run(`clone $opts $url $pkg`)
    catch
        run(`rm -rf $pkg`)
        rethrow()
    end
    isempty(Reqs.parse("$pkg/REQUIRE")) && return info("Nothing to be done.")
    info("Computing changes...")
    _resolve()
end

function _checkout(pkg::String, what::String, force::Bool)
    Git.transact(dir=pkg) do
        if force
            Git.run(`checkout -q -f $what`, dir=pkg)
        else
            Git.dirty(dir=pkg) && error("$pkg is dirty, bailing")
            Git.run(`checkout -q $what`, dir=pkg)
        end
        _resolve()
    end
end

checkout(pkg::String, branch::String="master"; force::Bool=false) = Dir.cd() do
    ispath(pkg,".git") || error("$pkg is not a git repo")
    info("Checking out $pkg $branch...")
    _checkout(pkg,branch,force)
end

release(pkg::String; force::Bool=false) = Dir.cd() do
    ispath(pkg,".git") || error("$pkg is not a git repo")
    avail = Dir.cd(Read.available)
    haskey(avail,pkg) || error("$pkg is not registered")
    force || Git.dirty(dir=pkg) && error("$pkg is dirty, bailing")
    info("Releasing $pkg...")
    avail = avail[pkg]
    vers = sort!([keys(avail)...], rev=true)
    for ver in vers
        sha1 = avail[ver].sha1
        Git.iscommit(sha1, dir=pkg) || continue
        return _checkout(pkg,sha1,force)
    end
    Write.update(pkg,vers[1])
    _resolve()
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
    _resolve(Reqs.parse("REQUIRE"), avail, instd, fixed, free)
end

function _resolve(
    reqs  :: Dict = Reqs.parse("REQUIRE"),
    avail :: Dict = Read.available(),
    instd :: Dict = Read.installed(avail),
    fixed :: Dict = Read.fixed(avail,instd),
    have  :: Dict = Read.free(instd),
)
    reqs = Query.requirements(reqs,fixed)
    deps = Query.dependencies(avail,fixed)

    incompatible = {}
    for pkg in keys(reqs)
        haskey(deps,pkg) || push!(incompatible,pkg)
    end
    isempty(incompatible) ||
        error("The following packages are incompatible with fixed requirements: ",
              join(incompatible, ", ", " and "))

    deps = Query.prune_dependencies(reqs,deps)
    want = Resolve.resolve(reqs,deps)

    # compare what is installed with what should be
    changes = Query.diff(have, want, avail, fixed)
    isempty(changes) && return info("No packages to install, update or remove.")

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
        msg = "Missing package versions (possible metadata misconfiguration):"
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

    installed
    # Since we just changed a lot of things, it's probably better to reread
    # the state, so only pass avail
    _fixup(String[pkg for (pkg,_) in filter(x->x[2][2]!=nothing,changes)], avail)
end

resolve() = Dir.cd(_resolve)

function build(pkg::String, args=[])
    try 
        path = Dir.path(pkg,"deps","build.jl")
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

function __fixup(
    instlist,
    avail :: Dict = Read.available(),
    inst  :: Dict = Read.installed(avail),
    free  :: Dict = Read.free(inst),
    fixed :: Dict = Read.fixed(avail,inst);
    exclude = []
)
    sort!(instlist, lt=function(a,b)
        c = contains(Read.alldependencies(a,avail,free,fixed),b) 
        nonordered = (!c && !contains(Read.alldependencies(b,avail,free,fixed),a))
        nonordered ? a < b : c
    end)
    for p in instlist
        contains(exclude,p) && continue
        build(p,["fixup"]) || return
    end
end

function _fixup{T<:String}(
    pkg::Vector{T},
    avail :: Dict = Read.available(),
    inst  :: Dict = Read.installed(avail),
    free  :: Dict = Read.free(inst),
    fixed :: Dict = Read.fixed(avail,inst);
    exclude = []
)
    tofixup = copy(pkg)
    oldlength = length(tofixup)
    while true
        for (p,_) in inst
            contains(tofixup,p) && continue
            for pf in tofixup
                if contains(Read.alldependencies(p,avail,free,fixed),pf)
                    push!(tofixup,p)
                    break
                end
            end
        end
        oldlength == length(tofixup) && break
        oldlength = length(tofixup)
    end
    __fixup(tofixup, avail, inst, free, fixed; exclude=exclude)
end

fixup(
    pkg::String,
    avail :: Dict = Read.available(),
    inst  :: Dict = Read.installed(avail),
    free  :: Dict = Read.free(inst),
    fixed :: Dict = Read.fixed(avail,inst);
    exclude = []
) = fixup([pkg], avail, inst, free, fixed; exclude=exclude)

function _fixup(
    avail :: Dict = Read.available(),
    inst  :: Dict = Read.installed(avail),
    free  :: Dict = Read.free(inst),
    fixed :: Dict = Read.fixed(avail,inst);
    exclude = []
)
    # TODO: Replace by proper toposorts
    instlist = [k for (k,v) in inst]
    _fixup(instlist, avail, inst, free, fixed; exclude=exclude)
end

fixup() = Dir.cd(_fixup)

end # module
