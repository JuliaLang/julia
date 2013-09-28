module Entry

using ..Types
import ..Reqs, ..Read, ..Query, ..Resolve, ..Cache, ..Write
import Base: Git, thispatch, nextpatch, nextminor, nextmajor, check_new_version

function edit(f::Function, pkg, args...)
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

available() = sort!([keys(Read.available())...], by=lowercase)

function available(pkg::String)
    avail = Read.available(pkg)
    if !isempty(avail) || Read.isinstalled(pkg)
        return sort!([keys(avail)...])
    end
    error("$pkg is not a package (not registered or installed)")
end

function installed()
    pkgs = Dict{String,VersionNumber}()
    for (pkg,(ver,fix)) in Read.installed()
        pkgs[pkg] = ver
    end
    return pkgs
end

function installed(pkg::String)
    avail = Read.available(pkg)
    Read.isinstalled(pkg) && return Read.installed_version(pkg,avail)
    isempty(avail) && error("$pkg is not a package (not registered or installed)")
    return nothing # registered but not installed
end

function status(io::IO)
    reqs = Reqs.parse("REQUIRE")
    instd = Read.installed()
    required = sort!([keys(reqs)...])
    if !isempty(required)
        println(io, "Required packages:")
        for pkg in required
            ver,fix = pop!(instd,pkg)
            status(io,pkg,ver,fix)
        end
    end
    additional = sort!([keys(instd)...])
    if !isempty(additional)
        println(io, "Additional packages:")
        for pkg in additional
            ver,fix = instd[pkg]
            status(io,pkg,ver,fix)
        end
    end
    if isempty(required) && isempty(additional)
        println(io, "No packages installed.")
    end
end
# TODO: status(io::IO, pkg::String)

function status(io::IO, pkg::String, ver::VersionNumber, fix::Bool)
    @printf io " - %-29s " pkg
    fix || return println(io,ver)
    @printf io "%-19s" ver
    if ispath(pkg,".git")
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

function clone(url::String, pkg::String, opts::Cmd=``)
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

function _checkout(pkg::String, what::String, merge::Bool=false, pull::Bool=false)
    Git.transact(dir=pkg) do
        Git.dirty(dir=pkg) && error("$pkg is dirty, bailing")
        Git.run(`checkout -q $what`, dir=pkg)
        merge && Git.run(`merge -q --ff-only $what`, dir=pkg)
        if pull
            info("Pulling $pkg latest $what...")
            Git.run(`pull -q --ff-only`, dir=pkg)
        end
        _resolve()
        #4082 TODO: some call to fixup should go here
    end
end

function checkout(pkg::String, branch::String, merge::Bool, pull::Bool)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    info("Checking out $pkg $branch...")
    _checkout(pkg,branch,merge,pull)
end

function release(pkg::String)
    ispath(pkg,".git") || error("$pkg is not a git repo")
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

function fix(pkg::String, head::String)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    branch = "fixed-$(head[1:8])"
    rslv = (head != Git.head(dir=pkg))
    info("Creating $pkg branch $branch...")
    Git.run(`checkout -q -B $branch $head`, dir=pkg)
    rslv ? _resolve() : nothing
end
fix(pkg::String) = fix(pkg,Git.head(dir=pkg))

function fix(pkg::String, ver::VersionNumber)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    Read.isinstalled(pkg) || error("$pkg cannot be fixed – not an installed package")
    avail = Read.available(pkg)
    isempty(avail) && error("$pkg cannot be fixed – not a registered package")
    haskey(avail,ver) || error("$pkg – $ver is not a registered version")
    fix(pkg,avail[ver].sha1)
end

function update()
    info("Updating METADATA...")
    cd("METADATA") do
        if Git.branch() != "devel"
            Git.run(`fetch -q --all`)
            Git.run(`checkout -q HEAD^0`)
            Git.run(`branch -f devel refs/remotes/origin/devel`)
            Git.run(`checkout -q devel`)
        end
        Git.run(`pull -q -m`)
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

function write_tag_metadata(pkg::String, ver::VersionNumber, commit::String)
    info("Writing METADATA for $pkg v$ver")
    cmd = Git.cmd(`cat-file blob $commit:REQUIRE`, dir=pkg)
    reqs = success(cmd) ? Reqs.parse(cmd) : Requires()
    cd("METADATA") do
        d = joinpath(pkg,"versions",string(ver))
        mkpath(d)
        sha1file = joinpath(d,"sha1")
        open(io->println(io,commit), sha1file, "w")
        Git.run(`add $sha1file`)
        reqsfile = joinpath(d,"requires")
        if isempty(reqs)
            ispath(reqsfile) && Git.run(`rm -f -q $reqsfile`)
        else
            Reqs.write(reqsfile,reqs)
            Git.run(`add $reqsfile`)
        end
    end
    return nothing
end

function register(pkg::String, url::String)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    isfile("METADATA",pkg,"url") && error("$pkg already registered")
    tags = split(Git.readall(`tag -l v*`, dir=pkg))
    filter!(tag->ismatch(Base.VERSION_REGEX,tag), tags)
    versions = [
        convert(VersionNumber,tag) =>
        Git.readchomp(`rev-parse --verify $tag^{commit}`, dir=pkg)
        for tag in tags
    ]
    Git.transact(dir="METADATA") do
        cd("METADATA") do
            info("Registering $pkg at $url")
            mkdir(pkg)
            path = joinpath(pkg,"url")
            open(io->println(io,url), path, "w")
            Git.run(`add $path`)
        end
        for (ver,commit) in versions
            write_tag_metadata(pkg,ver,commit)
        end
        info("Checking METADATA sanity")
        check_metadata()
    end
end

function register(pkg::String)
    Git.success(`config remote.origin.url`, dir=pkg) ||
        error("$pkg: no URL configured")
    url = Git.readchomp(`config remote.origin.url`, dir=pkg)
    register(pkg,url)
end

function isrewritable(v::VersionNumber)
    thispatch(v)==v"0" ||
    length(v.prerelease)==1 && isempty(v.prerelease[1]) ||
    length(v.build)==1 && isempty(v.build[1])
end

nextbump(v::VersionNumber) = isrewritable(v) ? v : nextpatch(v)

function tag(pkg::String, ver::Union(Symbol,VersionNumber), commit::String, msg::String)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    Git.dirty(dir=pkg) && error("$pkg is dirty – stash changes to tag")
    commit = isempty(commit) ? Git.head(dir=pkg) :
        Git.readchomp(`rev-parse $commit`, dir=pkg)
    registered = isfile("METADATA",pkg,"url")
    if registered
        avail = Read.available(pkg)
        existing = [keys(Read.available(pkg))...]
        ancestors = filter(v->Git.is_ancestor_of(avail[v].sha1,commit,dir=pkg), existing)
    else
        tags = split(Git.readall(`tag -l v*`, dir=pkg))
        filter!(tag->ismatch(Base.VERSION_REGEX,tag), tags)
        existing = VersionNumber[tags...]
        filter!(tags) do tag
            sha1 = Git.readchomp(`rev-parse --verify $tag^{commit}`, dir=pkg)
            Git.is_ancestor_of(sha1,commit,dir=pkg)
        end
        ancestors = VersionNumber[tags...]
    end
    sort!(existing)
    if isa(ver,Symbol)
        prv = isempty(existing) ? v"0" :
              isempty(ancestors) ? max(existing) : max(ancestors)
        ver = (ver == :bump ) ? nextbump(prv)  :
              (ver == :patch) ? nextpatch(prv) :
              (ver == :minor) ? nextminor(prv) :
              (ver == :major) ? nextmajor(prv) :
                                error("invalid version selector: $ver")
    end
    rewritable = isrewritable(ver)
    rewritable && filter!(v->v!=ver,existing)
    check_new_version(existing,ver)
    isempty(msg) && (msg = "$pkg v$ver [$(commit[1:10])]")
    # TODO: check that SHA1 isn't the same as another version
    opts = rewritable ? `--force` : `--annotate --message $msg`
    info("Tagging $pkg v$ver")
    Git.run(`tag $opts v$ver $commit`, dir=pkg, out=DevNull)
    registered || return
    try
        Git.transact(dir="METADATA") do
            write_tag_metadata(pkg,ver,commit)
            info("Checking METADATA sanity")
            check_metadata()
        end
    catch
        Git.run(`tag -d v$ver`, dir=pkg)
        rethrow()
    end
end

function check_metadata()
    avail = Read.available()
    instd = Read.installed(avail)
    fixed = Read.fixed(avail,instd,VERSION)
    deps  = Query.dependencies(avail,fixed)

    problematic = Resolve.sanity_check(deps)
    if !isempty(problematic)
        msg = "Packages with unsatisfiable requirements found:\n"
        for (p, vn, rp) in problematic
            msg *= "    $p v$vn : no valid versions exist for package $rp\n"
        end
        error(msg)
    end
    return
end

function build(pkg::String, args=[])
    try 
        path = abspath(pkg,"deps","build.jl")
        if isfile(path)
            info("Running build script for package $pkg")
            cd(dirname(path)) do
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

end # module
