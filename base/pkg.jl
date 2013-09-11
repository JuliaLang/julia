module Pkg

include("pkg/dir.jl")
include("pkg/types.jl")
include("pkg/reqs.jl")
include("pkg/cache.jl")
include("pkg/read.jl")
include("pkg/query.jl")
include("pkg/resolve.jl")
include("pkg/write.jl")
include("pkg/scaffold.jl")

using Base.Git, .Types

const dir = Dir.path
const scaffold = Scaffold.scaffold

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
    #4082 TODO: some call to fixup should go here
end

available() = sort!([keys(Dir.cd(Read.available))...], by=lowercase)

available(pkg::String) = Dir.cd() do
    avail = Read.available(pkg)
    if !isempty(avail) || Read.isinstalled(pkg)
        return sort!([keys(avail)...])
    end
    error("$pkg is not a package (not registered or installed)")
end

function installed()
    pkgs = Dict{String,VersionNumber}()
    for (pkg,(ver,fix)) in Dir.cd(Read.installed)
        pkgs[pkg] = ver
    end
    return pkgs
end

installed(pkg::String) = Dir.cd() do
    avail = Read.available(pkg)
    Read.isinstalled(pkg) && return Read.installed_version(pkg,avail)
    isempty(avail) && error("$pkg is not a package (not registered or installed)")
    return nothing # registered but not installed
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
        attrs = String[]
        isfile("METADATA",pkg,"url") || push!(attrs,"unregistered")
        Git.dirty(dir=pkg) && push!(attrs,"dirty")
        isempty(attrs) || print(io, " (",join(attrs,", "),")")
    else
        print(io, "non-repo (unregistered)")
    end
    println(io)
end

url2pkg(url::String) = match(r"/(\w+?)(?:\.jl)?(?:\.git)?$", url).captures[1]

clone(url::String, pkg::String=url2pkg(url); opts::Cmd=``) = Dir.cd() do
    info("Cloning $pkg from $url")
    ispath(pkg) && error("$pkg already exists")
    try
        Git.run(`clone $opts $url $pkg`)
        Git.set_remote_url(url, dir=pkg)
    catch
        run(`rm -rf $pkg`)
        rethrow()
    end
    isempty(Reqs.parse("$pkg/REQUIRE")) && return
    info("Computing changes...")
    _resolve()
    #4082 TODO: some call to fixup should go here
end

function _checkout(pkg::String, what::String, merge::Bool=false)
    Git.transact(dir=pkg) do
        Git.dirty(dir=pkg) && error("$pkg is dirty, bailing")
        Git.run(`checkout -q $what`, dir=pkg)
        merge && Git.run(`merge -q --ff-only $what`, dir=pkg)
        _resolve()
        #4082 TODO: some call to fixup should go here
    end
end

checkout(pkg::String, branch::String="master"; merge::Bool=true) = Dir.cd() do
    ispath(pkg,".git") || error("$pkg is not a git repo")
    info("Checking out $pkg $branch...")
    _checkout(pkg,branch,merge)
end

release(pkg::String) = Dir.cd() do
    Read.isinstalled(pkg) || error("$pkg cannot be released – not an installed package")
    avail = Read.available(pkg)
    isempty(avail) && error("$pkg cannot be released – not a registered package")
    Git.dirty(dir=pkg) && error("$pkg cannot be released – repo is dirty")
    info("Releasing $pkg...")
    vers = sort!([keys(avail)...], rev=true)
    while true
        for ver in vers
            sha1 = avail[ver].sha1
            Git.iscommit(sha1, dir=pkg) || continue
            return _checkout(pkg,sha1)
        end
        isempty(Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail])) && continue
        error("can't find any registered versions of $pkg to checkout")
    end
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
    #4082 TODO: some call to fixup should go here
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

    # Since we just changed a lot of things, it's probably better to reread
    # the state, so only pass avail
    _fixup(String[pkg for (pkg,_) in filter(x->x[2][2]!=nothing,changes)], avail)
    #4082 TODO: this call to fixup should no longer go here
end

resolve() = Dir.cd(_resolve) #4082 TODO: some call to fixup should go here

tag(pkg::String, ver::Union(Symbol,VersionNumber)=:patch;
    msg::String="", commit::String="") = Dir.cd() do
    ispath(pkg,".git") || error("$pkg is not a git repo")
    Git.dirty(dir=pkg) && error("$pkg is dirty – stash changes to tag")
    registered = isfile("METADATA",pkg,"url")
    existing = VersionNumber[(
        registered ? keys(Read.available(pkg)) :
        filter!(v->ismatch(Base.VERSION_REGEX,v), split(Git.readall(`tag -l v*`, dir=pkg)))
    )...]
    sort!(existing)
    if isa(ver,Symbol)
        ver = (ver == :patch) ? Base.nextpatch(max([v"0",existing])) :
              (ver == :minor) ? Base.nextminor(max([v"0",existing])) :
              (ver == :major) ? Base.nextmajor(max([v"0",existing])) :
                                error("invalid version selector: $ver")
    end
    Base.check_new_version(existing,ver)
    isempty(msg) && (msg = "$pkg v$ver")
    isempty(commit) && (commit = Git.head(dir=pkg))
    # TODO: check that SHA1 isn't the same as another version
    info("Tagging $pkg v$ver")
    Git.run(`tag -m $msg v$ver $commit`, dir=pkg)
    registered || return
    try
        reqs = Reqs.parse(joinpath(pkg,"REQUIRE"))
        cd("METADATA") do
            Git.transact() do
                d = joinpath(pkg,"versions",string(ver))
                mkpath(d)
                open(io->println(io,commit), joinpath(d,"sha1"), "w")
                isempty(reqs) || Reqs.write(joinpath(d,"requires"), reqs)
                Git.run(`add $d`)
            end
        end
    catch
        Git.run(`tag -d v$ver`, dir=pkg)
        rethrow()
    end
end

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
        c = in(b,Read.alldependencies(a,avail,free,fixed))
        nonordered = (!c && !in(a,Read.alldependencies(b,avail,free,fixed)))
        nonordered ? a < b : c
    end)
    for p in instlist
        in(p,exclude) && continue
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
            in(p,tofixup) && continue
            for pf in tofixup
                if in(pf,Read.alldependencies(p,avail,free,fixed))
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

_fixup(
    pkg::String,
    avail :: Dict = Read.available(),
    inst  :: Dict = Read.installed(avail),
    free  :: Dict = Read.free(inst),
    fixed :: Dict = Read.fixed(avail,inst);
    exclude = []
) = _fixup([pkg], avail, inst, free, fixed; exclude=exclude)

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
fixup(pkg) = Dir.cd(()->_fixup(pkg))

end # module