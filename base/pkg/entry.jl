module Entry

using ..Types
import ..Reqs, ..Read, ..Query, ..Resolve, ..Cache, ..Write
import Base: Git, thispatch, nextpatch, nextminor, nextmajor, check_new_version

function edit(f::Function, pkg::String, args...)
    r = Reqs.read("REQUIRE")
    reqs = Reqs.parse(r)
    avail = Read.available()
    !haskey(avail,pkg) && !haskey(reqs,pkg) && return false
    r_ = f(r,pkg,args...)
    r_ == r && return false
    reqs_ = Reqs.parse(r_)
    reqs_ != reqs && resolve(reqs_,avail)
    Reqs.write("REQUIRE",r_)
    info("REQUIRE updated.")
    return true
end

function add(pkg::String, vers::VersionSet)
    edit(Reqs.add,pkg,vers) && return
    ispath(pkg) || error("unknown package $pkg")
    info("Nothing to be done.")
end
add(pkg::String, vers::VersionNumber...) = add(pkg,VersionSet(vers...))

function rm(pkg::String)
    edit(Reqs.rm,pkg) && return
    ispath(pkg) || return info("Nothing to be done.")
    info("Removing $pkg (unregistered)")
    Write.remove(pkg)
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

function clone(url::String, pkg::String)
    info("Cloning $pkg from $url")
    ispath(pkg) && error("$pkg already exists")
    try
        Git.run(`clone $url $pkg`)
        Git.set_remote_url(url, dir=pkg)
    catch
        run(`rm -rf $pkg`)
        rethrow()
    end
    isempty(Reqs.parse("$pkg/REQUIRE")) && return
    info("Computing changes...")
    resolve()
end

function clone(url_or_pkg::String)
    urlpath = joinpath("METADATA",url_or_pkg,"url")
    if isfile(urlpath)
        pkg = url_or_pkg
        url = readchomp(urlpath)
        # TODO: Cache.prefetch(pkg,url)
    else
        url = url_or_pkg
        m = match(r"/(\w+?)(?:\.jl)?(?:\.git)?$", url)
        m != nothing || error("can't determine package name from URL: $url")
        pkg = m.captures[1]
    end
    clone(url,pkg)
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
        resolve()
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
    info("Releasing $pkg")
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

function pin(pkg::String, head::String)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    branch = "pinned.$(head[1:8]).tmp"
    rslv = (head != Git.head(dir=pkg))
    info("Creating $pkg branch $branch")
    Git.run(`checkout -q -B $branch $head`, dir=pkg)
    rslv ? resolve() : nothing
end
pin(pkg::String) = pin(pkg,Git.head(dir=pkg))

function pin(pkg::String, ver::VersionNumber)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    Read.isinstalled(pkg) || error("$pkg cannot be pinned – not an installed package".tmp)
    avail = Read.available(pkg)
    isempty(avail) && error("$pkg cannot be pinned – not a registered package".tmp)
    haskey(avail,ver) || error("$pkg – $ver is not a registered version")
    pin(pkg,avail[ver].sha1)
end

function update(branch::String)
    info("Updating METADATA...")
    cd("METADATA") do
        if Git.branch() != branch
            Git.dirty() && error("METADATA is dirty and not on $branch, bailing")
            Git.attached() || error("METADATA is detached not on $branch, bailing")
            Git.run(`fetch -q --all`)
            Git.run(`checkout -q HEAD^0`)
            Git.run(`branch -f $branch refs/remotes/origin/$branch`)
            Git.run(`checkout -q $branch`)
        end
        # TODO: handle merge conflicts
        Base.with_env("GIT_MERGE_AUTOEDIT","no") do
            Git.run(`pull -q`, out=DevNull)
        end
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

function publish(branch::String)
    Git.branch(dir="METADATA") == branch ||
        error("METADATA must be on $branch to publish changes")
    Git.success(`push -q -n`, dir="METADATA") ||
        error("METADATA is behind origin/$branch – run Pkg.update() before publishing")
    Git.run(`fetch -q`, dir="METADATA")
    info("Validating METADATA")
    check_metadata()
    cmd = Git.cmd(`diff --name-only --diff-filter=AMR origin/$branch HEAD --`, dir="METADATA")
    tags = Dict{ASCIIString,Vector{ASCIIString}}()
    for line in eachline(cmd)
        m = match(r"^(.+?)/versions/([^/]+)/sha1$", line)
        m != nothing && ismatch(Base.VERSION_REGEX, m.captures[2]) || continue
        pkg, ver = m.captures; ver = convert(VersionNumber,ver)
        sha1 = readchomp(joinpath("METADATA",chomp(line)))
        any(split(Git.readall(`tag --points-at $sha1`, dir=pkg))) do tag
            ver == convert(VersionNumber,tag) || return false
            haskey(tags,pkg) || (tags[pkg] = ASCIIString[])
            push!(tags[pkg], tag)
            return true
        end || error("$pkg v$ver is incorrectly tagged – $sha1 expected")
    end
    isempty(tags) && info("No new package versions to publish.")
    for pkg in sort!([keys(tags)...])
        forced = ASCIIString[]
        unforced = ASCIIString[]
        for tag in tags[pkg]
            ver = convert(VersionNumber,tag)
            push!(isrewritable(ver) ? forced : unforced, tag)
        end
        if !isempty(forced)
            info("Pushing $pkg temporary tags: ", join(forced,", "))
            refspecs = ["refs/tags/$tag:refs/tags/$tag" for tag in forced]
            Git.run(`push -q --force origin $refspecs`, dir=pkg)
        end
        if !isempty(unforced)
            info("Pushing $pkg permanent tags: ", join(unforced,", "))
            refspecs = ["refs/tags/$tag:refs/tags/$tag" for tag in unforced]
            Git.run(`push -q origin $refspecs`, dir=pkg)
        end
    end
    info("Pushing METADATA changes")
    Git.run(`push -q`, dir="METADATA")
end

function resolve(
    reqs  :: Dict = Reqs.parse("REQUIRE"),
    avail :: Dict = Read.available(),
    instd :: Dict = Read.installed(avail),
    fixed :: Dict = Read.fixed(avail,instd),
    have  :: Dict = Read.free(instd),
)
    reqs = Query.requirements(reqs,fixed,avail)
    deps, conflicts = Query.dependencies(avail,fixed)

    for pkg in keys(reqs)
        if !haskey(deps,pkg)
            error("$pkg's requirements can't be satisfied because of the following fixed packages: ",
                   join(conflicts[pkg], ", ", " and "))
        end
    end

    Query.check_requirements(reqs,deps,fixed)

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
    # re/build all updated/installed packages
    build(map(x->x[1],filter(x->x[2][2]!=nothing,changes)))
end

function write_tag_metadata(pkg::String, ver::VersionNumber, commit::String)
    cmd = Git.cmd(`cat-file blob $commit:REQUIRE`, dir=pkg)
    reqs = success(cmd) ? Reqs.read(cmd) : Reqs.Line[]
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
            info("Tagging $pkg v$ver")
            write_tag_metadata(pkg,ver,commit)
        end
        info("Committing METADATA for $pkg")
        msg = "Register $pkg"
        if !isempty(versions)
            vers = map(v->"v$v", sort!([keys(versions)...]))
            msg *= ": $(join(vers,", "))"
        end
        Git.run(`commit -q -m $msg -- $pkg`, dir="METADATA")
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
    Git.dirty(dir=pkg) &&
        error("$pkg is dirty – commit or stash changes to tag")
    Git.dirty(pkg, dir="METADATA") &&
        error("METADATA/$pkg is dirty – commit or stash changes to tag")
    commit = isempty(commit) ? Git.head(dir=pkg) :
        Git.readchomp(`rev-parse $commit`, dir=pkg)
    registered = isfile("METADATA",pkg,"url")
    if registered
        avail = Read.available(pkg)
        existing = VersionNumber[keys(Read.available(pkg))...]
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
              isempty(ancestors) ? maximum(existing) : maximum(ancestors)
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
            info("Committing METADATA for $pkg")
            Git.run(`commit -q -m "Tag $pkg v$ver" -- $pkg`, dir="METADATA")
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
        msg = "packages with unsatisfiable requirements found:\n"
        for (p, vn, rp) in problematic
            msg *= "    $p v$vn – no valid versions exist for package $rp\n"
        end
        error(msg)
    end
    return
end

function warnbanner(msg...; label="[ WARNING ]", prefix="")
    warn(prefix="", Base.cpad(label,Base.tty_cols(),"="))
    println(STDERR)
    warn(prefix=prefix, msg...)
    println(STDERR)
    warn(prefix="", "="^Base.tty_cols())
end

function build!(pkgs::Vector, errs::Dict, seen::Set=Set())
    for pkg in pkgs
        pkg in seen && continue
        build!(Read.requires_list(pkg),errs,push!(seen,pkg))
        path = abspath(pkg,"deps","build.jl")
        isfile(path) || continue
        info("Building $pkg")
        cd(dirname(path)) do
            try eval(Module(),Expr(:toplevel,:(ARGS={}),:(include($path))))
            catch err
                warnbanner(err, label="[ ERROR: $pkg ]")
                errs[pkg] = err
            end
        end
    end
end

function build(pkgs::Vector)
    errs = Dict()
    build!(pkgs,errs)
    isempty(errs) && return
    println(STDERR)
    warnbanner(label="[ BUILD ERRORS ]", """
    WARNING: $(join(map(x->x[1],errs),", "," and ")) had build errors.

     - packages with build errors remain installed in $(pwd())
     - build a package and all its dependencies with `Pkg.build(pkg)`
     - build a single package by running its `deps/build.jl` script
    """)
end
build() = build(sort!([keys(installed())...]))

end # module
