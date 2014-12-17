module Entry

import Base: thispatch, nextpatch, nextminor, nextmajor, check_new_version
import ..Git, ..Reqs, ..Read, ..Query, ..Resolve, ..Cache, ..Write, ..GitHub, ..Dir
using ..Types

macro recover(ex)
    quote
        try $(esc(ex))
        catch err
            show(err)
        end
    end
end

function edit(f::Function, pkg::AbstractString, args...)
    r = Reqs.read("REQUIRE")
    reqs = Reqs.parse(r)
    avail = Read.available()
    !haskey(avail,pkg) && !haskey(reqs,pkg) && return false
    rʹ = f(r,pkg,args...)
    rʹ == r && return false
    reqsʹ = Reqs.parse(rʹ)
    reqsʹ != reqs && resolve(reqsʹ,avail)
    Reqs.write("REQUIRE",rʹ)
    info("Package database updated")
    return true
end

function edit()
    editor = get(ENV,"VISUAL",get(ENV,"EDITOR",nothing))
    editor != nothing ||
        error("set the EDITOR environment variable to an edit command")
    editor = Base.shell_split(editor)
    reqs = Reqs.parse("REQUIRE")
    run(`$editor REQUIRE`)
    reqsʹ = Reqs.parse("REQUIRE")
    reqs == reqsʹ && return info("Nothing to be done")
    info("Computing changes...")
    resolve(reqsʹ)
end

function add(pkg::AbstractString, vers::VersionSet)
    outdated = :maybe
    @sync begin
        @async if !edit(Reqs.add,pkg,vers)
            ispath(pkg) || error("unknown package $pkg")
            info("Nothing to be done")
        end
        branch = Dir.getmetabranch()
        if Git.branch(dir="METADATA") == branch
            if !Git.success(`diff --quiet origin/$branch`, dir="METADATA")
                outdated = :yes
            else
                try
                    run(Git.cmd(`fetch -q --all`, dir="METADATA") |>DevNull .>DevNull)
                    outdated = Git.success(`diff --quiet origin/$branch`, dir="METADATA") ?
                        (:no) : (:yes)
                end
            end
        else
            outdated = :no # user is doing something funky with METADATA
        end
    end
    if outdated != :no
        is = outdated == :yes ? "is" : "might be"
        info("METADATA $is out-of-date — you may not have the latest version of $pkg")
        info("Use `Pkg.update()` to get the latest versions of your packages")
    end
end
add(pkg::AbstractString, vers::VersionNumber...) = add(pkg,VersionSet(vers...))

function rm(pkg::AbstractString)
    edit(Reqs.rm,pkg) && return
    ispath(pkg) || return info("Nothing to be done")
    info("Removing $pkg (unregistered)")
    Write.remove(pkg)
end

available() = sort!(ASCIIString[keys(Read.available())...], by=lowercase)

function available(pkg::AbstractString)
    avail = Read.available(pkg)
    if !isempty(avail) || Read.isinstalled(pkg)
        return sort!([keys(avail)...])
    end
    error("$pkg is not a package (not registered or installed)")
end

function installed()
    pkgs = Dict{ASCIIString,VersionNumber}()
    for (pkg,(ver,fix)) in Read.installed()
        pkgs[pkg] = ver
    end
    return pkgs
end

function installed(pkg::AbstractString)
    avail = Read.available(pkg)
    Read.isinstalled(pkg) && return Read.installed_version(pkg,avail)
    isempty(avail) && error("$pkg is not a package (not registered or installed)")
    return nothing # registered but not installed
end

function status(io::IO; pkgname::AbstractString = "")
    showpkg(pkg) = (pkgname == "") ? (true) : (pkg == pkgname)
    reqs = Reqs.parse("REQUIRE")
    instd = Read.installed()
    required = sort!([keys(reqs)...])
    if !isempty(required)
        showpkg("") && println(io, "$(length(required)) required packages:")
        for pkg in required
            ver,fix = pop!(instd,pkg)
            showpkg(pkg) && status(io,pkg,ver,fix)
        end
    end
    additional = sort!([keys(instd)...])
    if !isempty(additional)
        showpkg("") && println(io, "$(length(additional)) additional packages:")
        for pkg in additional
            ver,fix = instd[pkg]
            showpkg(pkg) && status(io,pkg,ver,fix)
        end
    end
    if isempty(required) && isempty(additional)
        println(io, "No packages installed")
    end
end

status(io::IO, pkg::AbstractString) = status(io, pkgname = pkg)

function status(io::IO, pkg::AbstractString, ver::VersionNumber, fix::Bool)
    @printf io " - %-29s " pkg
    fix || return println(io,ver)
    @printf io "%-19s" ver
    if ispath(pkg,".git")
        print(io, Git.attached(dir=pkg) ? Git.branch(dir=pkg) : Git.head(dir=pkg)[1:8])
        attrs = AbstractString[]
        isfile("METADATA",pkg,"url") || push!(attrs,"unregistered")
        Git.dirty(dir=pkg) && push!(attrs,"dirty")
        isempty(attrs) || print(io, " (",join(attrs,", "),")")
    else
        print(io, "non-repo (unregistered)")
    end
    println(io)
end

function clone(url::AbstractString, pkg::AbstractString)
    info("Cloning $pkg from $url")
    ispath(pkg) && error("$pkg already exists")
    try
        Git.run(`clone -q $url $pkg`)
        Git.set_remote_url(url, dir=pkg)
    catch
        Base.rm(pkg, recursive=true)
        rethrow()
    end
    info("Computing changes...")
    if !edit(Reqs.add, pkg)
        isempty(Reqs.parse("$pkg/REQUIRE")) && return
        resolve()
    end
end

function clone(url_or_pkg::AbstractString)
    urlpath = joinpath("METADATA",url_or_pkg,"url")
    if isfile(urlpath)
        pkg = url_or_pkg
        url = readchomp(urlpath)
        # TODO: Cache.prefetch(pkg,url)
    else
        url = url_or_pkg
        m = match(r"(?:^|[/\\])(\w+?)(?:\.jl)?(?:\.git)?$", url)
        m != nothing || error("can't determine package name from URL: $url")
        pkg = m.captures[1]
    end
    clone(url,pkg)
end

function _checkout(pkg::AbstractString, what::AbstractString, merge::Bool=false, pull::Bool=false, branch::Bool=false)
    Git.transact(dir=pkg) do
        Git.dirty(dir=pkg) && error("$pkg is dirty, bailing")
        branch ? Git.run(`checkout -q -B $what -t origin/$what`, dir=pkg) : Git.run(`checkout -q $what`, dir=pkg)
        merge && Git.run(`merge -q --ff-only $what`, dir=pkg)
        if pull
            info("Pulling $pkg latest $what...")
            Git.run(`pull -q --ff-only`, dir=pkg)
        end
        resolve()
    end
end

function checkout(pkg::AbstractString, branch::AbstractString, merge::Bool, pull::Bool)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    info("Checking out $pkg $branch...")
    _checkout(pkg,branch,merge,pull,true)
end

function free(pkg::AbstractString)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    Read.isinstalled(pkg) || error("$pkg cannot be freed – not an installed package")
    avail = Read.available(pkg)
    isempty(avail) && error("$pkg cannot be freed – not a registered package")
    Git.dirty(dir=pkg) && error("$pkg cannot be freed – repo is dirty")
    info("Freeing $pkg")
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

function pin(pkg::AbstractString, head::AbstractString)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    branch = "pinned.$(head[1:8]).tmp"
    rslv = (head != Git.head(dir=pkg))
    info("Creating $pkg branch $branch")
    Git.run(`checkout -q -B $branch $head`, dir=pkg)
    rslv ? resolve() : nothing
end
pin(pkg::AbstractString) = pin(pkg,Git.head(dir=pkg))

function pin(pkg::AbstractString, ver::VersionNumber)
    ispath(pkg,".git") || error("$pkg is not a git repo")
    Read.isinstalled(pkg) || error("$pkg cannot be pinned – not an installed package".tmp)
    avail = Read.available(pkg)
    isempty(avail) && error("$pkg cannot be pinned – not a registered package".tmp)
    haskey(avail,ver) || error("$pkg – $ver is not a registered version")
    pin(pkg,avail[ver].sha1)
end

function update(branch::AbstractString)
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
    @sync for pkg in filter!(Read.isinstalled,[keys(avail)...])
        @async Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
    end
    instd = Read.installed(avail)
    free = Read.free(instd)
    @sync for (pkg,ver) in free
        @async Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
    end
    fixed = Read.fixed(avail,instd)
    @sync for (pkg,ver) in fixed
        ispath(pkg,".git") || continue
        @async begin
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
    end
    info("Computing changes...")
    resolve(Reqs.parse("REQUIRE"), avail, instd, fixed, free)
    # Don't use instd here since it may have changed
    updatehook(sort!([keys(installed())...]))
end

function pull_request(dir::AbstractString, commit::AbstractString="", url::AbstractString="")
    commit = isempty(commit) ? Git.head(dir=dir) :
        Git.readchomp(`rev-parse --verify $commit`, dir=dir)
    isempty(url) && (url = Git.readchomp(`config remote.origin.url`, dir=dir))
    m = match(Git.GITHUB_REGEX, url)
    m == nothing && error("not a GitHub repo URL, can't make a pull request: $url")
    owner, repo = m.captures[2:3]
    user = GitHub.user()
    info("Forking $owner/$repo to $user")
    response = GitHub.fork(owner,repo)
    fork = response["ssh_url"]
    branch = "pull-request/$(commit[1:8])"
    info("Pushing changes as branch $branch")
    Git.run(`push -q $fork $commit:refs/heads/$branch`, dir=dir)
    pr_url = "$(response["html_url"])/compare/$branch"
    @osx? run(`open $pr_url`) : info("To create a pull-request, open:\n\n  $pr_url\n")
end

function submit(pkg::AbstractString, commit::AbstractString="")
    urlpath = joinpath("METADATA",pkg,"url")
    url = ispath(urlpath) ? readchomp(urlpath) : ""
    pull_request(pkg, commit, url)
end

function publish(branch::AbstractString)
    Git.branch(dir="METADATA") == branch ||
        error("METADATA must be on $branch to publish changes")
    Git.run(`fetch -q`, dir="METADATA")
    ahead_remote, ahead_local = map(int,split(Git.readchomp(`rev-list --count --left-right origin/$branch...$branch`, dir="METADATA"),'\t'))
    ahead_remote > 0 && error("METADATA is behind origin/$branch – run `Pkg.update()` before publishing")
    ahead_local == 0 && error("There are no METADATA changes to publish")

    info("Validating METADATA")
    check_metadata()
    tags = Dict{ASCIIString,Vector{ASCIIString}}()
    Git.run(`update-index -q --really-refresh`, dir="METADATA")
    cmd = `diff --name-only --diff-filter=AMR origin/$branch HEAD --`
    for line in eachline(Git.cmd(cmd, dir="METADATA"))
        path = chomp(line)
        m = match(r"^(.+?)/versions/([^/]+)/sha1$", path)
        m != nothing && ismatch(Base.VERSION_REGEX, m.captures[2]) || continue
        pkg, ver = m.captures; ver = convert(VersionNumber,ver)
        sha1 = readchomp(joinpath("METADATA",path))
        if Git.success(`cat-file -e origin/$branch:$path`, dir="METADATA")
            old = Git.readchomp(`cat-file blob origin/$branch:$path`, dir="METADATA")
            old == sha1 || error("$pkg v$ver SHA1 changed in METADATA – refusing to publish")
        end
        any(split(Git.readall(`tag --contains $sha1`, dir=pkg))) do tag
            ver == convert(VersionNumber,tag) || return false
            haskey(tags,pkg) || (tags[pkg] = ASCIIString[])
            push!(tags[pkg], tag)
            return true
        end || error("$pkg v$ver is incorrectly tagged – $sha1 expected")
    end
    isempty(tags) && info("No new package versions to publish")
    @sync for pkg in sort!([keys(tags)...])
        @async begin
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
    end
    info("Submitting METADATA changes")
    pull_request("METADATA")
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
            if "julia" in conflicts[pkg]
                error("$pkg can't be installed because it has no versions that support ", VERSION, " of julia. " *
                   "You may need to update METADATA by running `Pkg.update()`")
            else
                error("$pkg's requirements can't be satisfied because of the following fixed packages: ",
                   join(conflicts[pkg], ", ", " and "))
            end
        end
    end

    Query.check_requirements(reqs,deps,fixed)

    deps = Query.prune_dependencies(reqs,deps)
    want = Resolve.resolve(reqs,deps)

    # compare what is installed with what should be
    changes = Query.diff(have, want, avail, fixed)
    isempty(changes) && return info("No packages to install, update or remove")

    # prefetch phase isolates network activity, nothing to roll back
    missing = []
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
    changed = []
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

function write_tag_metadata(pkg::AbstractString, ver::VersionNumber, commit::AbstractString, force::Bool=false)
    cmd = Git.cmd(`cat-file blob $commit:REQUIRE`, dir=pkg)
    reqs = success(cmd) ? Reqs.read(cmd) : Reqs.Line[]
    cd("METADATA") do
        d = joinpath(pkg,"versions",string(ver))
        mkpath(d)
        sha1file = joinpath(d,"sha1")
        if !force && ispath(sha1file)
            current = readchomp(sha1file)
            current == commit ||
                error("$pkg v$ver is already registered as $current, bailing")
        end
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

function register(pkg::AbstractString, url::AbstractString)
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
        vers = sort!([keys(versions)...])
        for ver in vers
            info("Tagging $pkg v$ver")
            write_tag_metadata(pkg,ver,versions[ver])
        end
        if Git.staged(dir="METADATA")
            info("Committing METADATA for $pkg")
            msg = "Register $pkg"
            if !isempty(versions)
                msg *= ": $(join(map(v->"v$v", vers),", "))"
            end
            Git.run(`commit -q -m $msg -- $pkg`, dir="METADATA")
        else
            info("No METADATA changes to commit")
        end
    end
end

function register(pkg::AbstractString)
    Git.success(`config remote.origin.url`, dir=pkg) ||
        error("$pkg: no URL configured")
    url = Git.readchomp(`config remote.origin.url`, dir=pkg)
    register(pkg,Git.normalize_url(url))
end

function isrewritable(v::VersionNumber)
    thispatch(v)==v"0" ||
    length(v.prerelease)==1 && isempty(v.prerelease[1]) ||
    length(v.build)==1 && isempty(v.build[1])
end

nextbump(v::VersionNumber) = isrewritable(v) ? v : nextpatch(v)

function tag(pkg::AbstractString, ver::Union(Symbol,VersionNumber), force::Bool=false, commit::AbstractString="HEAD")
    ispath(pkg,".git") || error("$pkg is not a git repo")
    Git.dirty(dir=pkg) &&
        error("$pkg is dirty – commit or stash changes to tag")
    Git.dirty(pkg, dir="METADATA") &&
        error("METADATA/$pkg is dirty – commit or stash changes to tag")
    commit = Git.readchomp(`rev-parse $commit`, dir=pkg)
    registered = isfile("METADATA",pkg,"url")
    if !force
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
        isrewritable(ver) && filter!(v->v!=ver,existing)
        check_new_version(existing,ver)
    end
    # TODO: check that SHA1 isn't the same as another version
    info("Tagging $pkg v$ver")
    opts = ``
    if force || isrewritable(ver)
        opts = `$opts --force`
    end
    if !isrewritable(ver)
        opts = `$opts --annotate --message "$pkg v$ver [$(commit[1:10])]"`
    end
    Git.run(`tag $opts v$ver $commit`, dir=pkg, out=DevNull)
    registered || return
    try
        Git.transact(dir="METADATA") do
            write_tag_metadata(pkg,ver,commit,force)
            if Git.staged(dir="METADATA")
                info("Committing METADATA for $pkg")
                Git.run(`commit -q -m "Tag $pkg v$ver" -- $pkg`, dir="METADATA")
            else
                info("No METADATA changes to commit")
            end
        end
    catch
        Git.run(`tag -d v$ver`, dir=pkg)
        rethrow()
    end
end

function check_metadata()
    avail = Read.available()
    deps, conflicts = Query.dependencies(avail)

    for (dp,dv) in deps, (v,a) in dv, p in keys(a.requires)
        haskey(deps, p) || error("package $dp v$v requires a non-registered package: $p")
    end

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
    cols = Base.tty_size()[2]
    warn(prefix="", Base.cpad(label,cols,"="))
    println(STDERR)
    warn(prefix=prefix, msg...)
    println(STDERR)
    warn(prefix="", "="^cols)
end

function build!(pkgs::Vector, errs::Dict, seen::Set=Set())
    for pkg in pkgs
        pkg in seen && continue
        build!(Read.requires_list(pkg),errs,push!(seen,pkg))
        path = abspath(pkg,"deps","build.jl")
        isfile(path) || continue
        info("Building $pkg")
        cd(dirname(path)) do
            try evalfile(path)
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
     - build the package(s) and all dependencies with `Pkg.build("$(join(map(x->x[1],errs),"\", \""))")`
     - build a single package by running its `deps/build.jl` script
    """)
end
build() = build(sort!([keys(installed())...]))

function updatehook!(pkgs::Vector, errs::Dict, seen::Set=Set())
    for pkg in pkgs
        pkg in seen && continue
        updatehook!(Read.requires_list(pkg),errs,push!(seen,pkg))
        path = abspath(pkg,"deps","update.jl")
        isfile(path) || continue
        info("Running update script for $pkg")
        cd(dirname(path)) do
            try evalfile(path)
            catch err
                warnbanner(err, label="[ ERROR: $pkg ]")
                errs[pkg] = err
            end
        end
    end
end

function updatehook(pkgs::Vector)
    errs = Dict()
    updatehook!(pkgs,errs)
    isempty(errs) && return
    println(STDERR)
    warnbanner(label="[ UPDATE ERRORS ]", """
    WARNING: $(join(map(x->x[1],errs),", "," and ")) had update errors.

     - Unrelated packages are unaffected
     - To retry, run Pkg.update() again
    """)
end

function test!(pkg::AbstractString, errs::Vector{AbstractString}, notests::Vector{AbstractString}; coverage::Bool=false)
    reqs_path = abspath(pkg,"test","REQUIRE")
    if isfile(reqs_path)
        tests_require = Reqs.parse(reqs_path)
        if (!isempty(tests_require))
            info("Computing test dependencies for $pkg...")
            resolve(merge(Reqs.parse("REQUIRE"), tests_require))
        end
    end
    test_path = abspath(pkg,"test","runtests.jl")
    if isfile(test_path)
        info("Testing $pkg")
        cd(dirname(test_path)) do
            try
                color = Base.have_color? "--color=yes" : "--color=no"
                codecov = coverage? "--code-coverage=user" : "--code-coverage=none"
                run(`$JULIA_HOME/julia $codecov $color $test_path`)
                info("$pkg tests passed")
            catch err
                warnbanner(err, label="[ ERROR: $pkg ]")
                push!(errs,pkg)
            end
        end
    else
        push!(notests,pkg)
    end
    resolve()
end

function test(pkgs::Vector{AbstractString}; coverage::Bool=false)
    errs = AbstractString[]
    notests = AbstractString[]
    for pkg in pkgs
        test!(pkg,errs,notests; coverage=coverage)
    end
    if !isempty(errs) || !isempty(notests)
        messages = AbstractString[]
        isempty(errs) || push!(messages, "$(join(errs,", "," and ")) had test errors")
        isempty(notests) || push!(messages, "$(join(notests,", "," and ")) did not provide a test/runtests.jl file")
        error(join(messages, "and"))
    end
end

test(;coverage::Bool=false) = test(sort!(AbstractString[keys(installed())...]); coverage=coverage)

end # module
