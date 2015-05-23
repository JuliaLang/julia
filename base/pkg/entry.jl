# This file is a part of Julia. License is MIT: http://julialang.org/license

module Entry

import Base: thispatch, nextpatch, nextminor, nextmajor, check_new_version
import ..LibGit2, ..Git, ..Reqs, ..Read, ..Query, ..Resolve, ..Cache, ..Write, ..GitHub, ..Dir
importall ..LibGit2
import ...Pkg.PkgError
using ..Types

macro recover(ex)
    quote
        try $(esc(ex))
        catch err
            show(err)
            print('\n')
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
        throw(PkgError("set the EDITOR environment variable to an edit command"))
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
            ispath(pkg) || throw(PkgError("unknown package $pkg"))
            info("Nothing to be done")
        end
        branch = Dir.getmetabranch()
        outdated = with(GitRepo, "METADATA") do repo
            current_barnch = LibGit2.branch(repo)
            if current_barnch == branch
                if LibGit2.isdiff(repo, "origin/$branch")
                    outdated = :yes
                else
                    try
                        LibGit2.fetch(repo)
                        outdated = LibGit2.isdiff(repo, "origin/$branch") ? (:yes) : (:no)
                    end
                end
            else
                :no # user is doing something funky with METADATA
            end
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

function available()
    all_avail = Read.available()
    avail = AbstractString[]
    for (pkg, vers) in all_avail
        any(x->Types.satisfies("julia", VERSION, x[2].requires), vers) && push!(avail, pkg)
    end
    sort!(avail, by=lowercase)
end

function available(pkg::AbstractString)
    avail = Read.available(pkg)
    if !isempty(avail) || Read.isinstalled(pkg)
        return sort!(collect(keys(avail)))
    end
    throw(PkgError("$pkg is not a package (not registered or installed)"))
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
    if Read.isinstalled(pkg)
        res = typemin(VersionNumber)
        repo = GitRepo(pkg)
        try
            res = Read.installed_version(pkg, repo, avail)
        finally
            finalize(repo)
        end
        return res
    end
    isempty(avail) && throw(PkgError("$pkg is not a package (not registered or installed)"))
    return nothing # registered but not installed
end

function status(io::IO; pkgname::AbstractString = "")
    showpkg(pkg) = (pkgname == "") ? (true) : (pkg == pkgname)
    reqs = Reqs.parse("REQUIRE")
    instd = Read.installed()
    required = sort!(collect(keys(reqs)))
    if !isempty(required)
        showpkg("") && println(io, "$(length(required)) required packages:")
        for pkg in required
            ver,fix = pop!(instd,pkg)
            showpkg(pkg) && status(io,pkg,ver,fix)
        end
    end
    additional = sort!(collect(keys(instd)))
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
        prepo = GitRepo(pkg)
        try
            with(LibGit2.head(prepo)) do phead
                if LibGit2.isattached(prepo)
                    print(io, LibGit2.shortname(phead))
                else
                    print(io, string(LibGit2.Oid(phead))[1:8])
                end
            end
            attrs = AbstractString[]
            isfile("METADATA",pkg,"url") || push!(attrs,"unregistered")
            LibGit2.isdirty(prepo) && push!(attrs,"dirty")
            isempty(attrs) || print(io, " (",join(attrs,", "),")")
        catch
            print(io, "broken-repo (unregistered)")
        finally
            finalize(prepo)
        end
    else
        print(io, "non-repo (unregistered)")
    end
    println(io)
end

function clone(url::AbstractString, pkg::AbstractString)
    info("Cloning $pkg from $url")
    ispath(pkg) && throw(PkgError("$pkg already exists"))
    try
        LibGit2.with(LibGit2.clone(url, pkg)) do repo
            LibGit2.set_remote_url(repo, url)
        end
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
        m != nothing || throw(PkgError("can't determine package name from URL: $url"))
        pkg = m.captures[1]
    end
    clone(url,pkg)
end

function checkout(pkg::AbstractString, branch::AbstractString, merge::Bool, pull::Bool)
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    info("Checking out $pkg $branch...")
    with(GitRepo, pkg) do r
        LibGit2.transact(r) do repo
            LibGit2.isdirty(repo) && throw(PkgError("$pkg is dirty, bailing"))
            LibGit2.branch!(repo, branch, track=LibGit2.GitConst.REMOTE_ORIGIN)
            merge && LibGit2.merge!(repo, fast_forward=true) # merge changes
            if pull
                info("Pulling $pkg latest $branch...")
                LibGit2.fetch(repo)
                LibGit2.merge!(repo, fast_forward=true)
            end
            resolve()
        end
    end
end

function free(pkg::AbstractString)
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    Read.isinstalled(pkg) || throw(PkgError("$pkg cannot be freed – not an installed package"))
    avail = Read.available(pkg)
    isempty(avail) && throw(PkgError("$pkg cannot be freed – not a registered package"))
    with(GitRepo, pkg) do repo
        LibGit2.isdirty(repo) && throw(PkgError("$pkg cannot be freed – repo is dirty"))
        info("Freeing $pkg")
        vers = sort!(collect(keys(avail)), rev=true)
        while true
            for ver in vers
                sha1 = avail[ver].sha1
                LibGit2.iscommit(sha1, repo) || continue
                return LibGit2.transact(repo) do r
                    LibGit2.isdirty(repo) && throw(PkgError("$pkg is dirty, bailing"))
                    LibGit2.checkout!(repo, sha1)
                    resolve()
                end
            end
            isempty(Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail])) && continue
            throw(PkgError("can't find any registered versions of $pkg to checkout"))
        end
    end
end

# function free(pkgs)
#     try
#         for pkg in pkgs
#             ispath(pkg,".git") || error("$pkg is not a git repo")
#             Read.isinstalled(pkg) || error("$pkg cannot be freed – not an installed package")
#             avail = Read.available(pkg)
#             isempty(avail) && error("$pkg cannot be freed – not a registered package")
#             Git.dirty(dir=pkg) && error("$pkg cannot be freed – repo is dirty")
#             info("Freeing $pkg")
#             vers = sort!(collect(keys(avail)), rev=true)
#             for ver in vers
#                 sha1 = avail[ver].sha1
#                 Git.iscommit(sha1, dir=pkg) || continue
#                 Git.run(`checkout -q $sha1`, dir=pkg)
#                 break
#             end
#             isempty(Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail])) && continue
#             error("can't find any registered versions of $pkg to checkout")
#         end
#     finally
#         resolve()
#     end
# end

function pin(pkg::AbstractString, head::AbstractString)
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    rslv = !isempty(head) # no need to resolve, branch will be from HEAD
    with(GitRepo, pkg) do repo
        if isempty(head) # get HEAD oid
            head = head_oid(repo)
        end
        branch = "pinned.$(head[1:8]).tmp"
        info("Creating $pkg branch $branch")
        LibGit2.create_branch(repo, branch, head)
    end
    rslv ? resolve() : nothing
end
pin(pkg::AbstractString) = pin(pkg, "")

function pin(pkg::AbstractString, ver::VersionNumber)
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    Read.isinstalled(pkg) || throw(PkgError("$pkg cannot be pinned – not an installed package".tmp))
    avail = Read.available(pkg)
    isempty(avail) && throw(PkgError("$pkg cannot be pinned – not a registered package".tmp))
    haskey(avail,ver) || throw(PkgError("$pkg – $ver is not a registered version"))
    pin(pkg, avail[ver].sha1)
end

function update(branch::AbstractString)
    info("Updating METADATA...")
    with(GitRepo, "METADATA") do repo
        with(LibGit2.head(repo)) do h
            if LibGit2.branch(h) != branch
                LibGit2.isdirty(repo) && throw(PkgError("METADATA is dirty and not on $branch, bailing"))
                LibGit2.isattached(repo) || throw(PkgError("METADATA is detached not on $branch, bailing"))
                LibGit2.fetch(repo)
                LibGit2.checkout_head(repo)
                LibGit2.branch!(repo, branch, track="refs/remotes/origin/$branch")
                LibGit2.merge!(repo)
            end
        end
        LibGit2.fetch(repo)
        LibGit2.merge!(repo, fast_forward=true) || LibGit2.rebase!(repo, "origin/$branch")
    end
    avail = Read.available()
    # this has to happen before computing free/fixed
    for pkg in filter!(Read.isinstalled,collect(keys(avail)))
        try
            Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
        catch err
            warn("Package $pkg: unable to update cache\n$(err.msg)")
        end
    end
    instd = Read.installed(avail)
    free = Read.free(instd)
    for (pkg,ver) in free
        Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
    end
    fixed = Read.fixed(avail,instd)
    for (pkg,ver) in fixed
        ispath(pkg,".git") || continue
        with(GitRepo, pkg) do repo
            if LibGit2.isattached(repo) && !LibGit2.isdirty(repo)
                info("Updating $pkg...")
                @recover begin
                    LibGit2.fetch(repo)
                    LibGit2.merge!(repo, fast_forward=true)
                end
            end
        end
        if haskey(avail,pkg)
            try
                Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
            catch err
                warn("Package $pkg: unable to update cache\n$(err.msg)")
            end
        end
    end
    info("Computing changes...")
    resolve(Reqs.parse("REQUIRE"), avail, instd, fixed, free)
    # Don't use instd here since it may have changed
    updatehook(sort!(collect(keys(installed()))))
end

function pull_request(dir::AbstractString, commit::AbstractString="", url::AbstractString="")
    with(GitRepo, dir) do repo
        if isempty(commit)
            commit = string(LibGit2.head_oid(repo))
        else
            !LibGit2.iscommit(commit, repo) && throw(PkgError("Cannot find pull commit: $commit"))
        end
        if isempty(url)
            url = LibGit2.getconfig(repo, "remote.origin.url", "")
        end

        m = match(LibGit2.GITHUB_REGEX, url)
        m == nothing && throw(PkgError("not a GitHub repo URL, can't make a pull request: $url"))
        owner, repo = m.captures[2:3]
        user = GitHub.user()
        info("Forking $owner/$repo to $user")
        response = GitHub.fork(owner,repo)
        fork = response["ssh_url"]
        branch = "pull-request/$(commit[1:8])"
        info("Pushing changes as branch $branch")
        LibGit2.push(repo, url=fork, refspecs=["$commit:refs/heads/$branch"])
        pr_url = "$(response["html_url"])/compare/$branch"
        info("To create a pull-request, open:\n\n  $pr_url\n")
    end
end

function submit(pkg::AbstractString, commit::AbstractString="")
    urlpath = joinpath("METADATA",pkg,"url")
    url = ispath(urlpath) ? readchomp(urlpath) : ""
    pull_request(pkg, commit, url)
end

function publish(branch::AbstractString)
    with(GitRepo, "METADATA") do repo
        LibGit2.branch(repo) == branch ||
            throw(PkgError("METADATA must be on $branch to publish changes"))
        LibGit2.fetch(repo)

        ahead_remote, ahead_local = LibGit2.revcount(repo, "origin/$branch", branch)
        ahead_remote > 0 && throw(PkgError("METADATA is behind origin/$branch – run `Pkg.update()` before publishing"))
        ahead_local == 0 && throw(PkgError("There are no METADATA changes to publish"))

        tags = Dict{ByteString,Vector{ASCIIString}}()

        # get changed files
        for path in LibGit2.diff_files(repo, "origin/metadata-v2", LibGit2.GitConst.HEAD_FILE)
            println(path)
            m = match(r"^(.+?)/versions/([^/]+)/sha1$", path)
            m != nothing && ismatch(Base.VERSION_REGEX, m.captures[2]) || continue
            pkg, ver = m.captures; ver = convert(VersionNumber,ver)
            sha1 = readchomp(joinpath("METADATA",path))
            old = LibGit2.cat(repo, LibGit2.GitBlob, "origin/$branch:$path")
            old != nothing && old != sha1 && throw(PkgError("$pkg v$ver SHA1 changed in METADATA – refusing to publish"))
            with(GitRepo, pkg) do pkg_repo
                tag_name = "v$ver"
                tag_commit = LibGit2.revparseid(pkg_repo, "$(tag_name)^{commit}")
                LibGit2.iszero(tag_commit) || string(tag_commit) == sha1 || return false
                haskey(tags,pkg) || (tags[pkg] = ASCIIString[])
                push!(tags[pkg], tag_name)
                return true
            end || throw(PkgError("$pkg v$ver is incorrectly tagged – $sha1 expected"))
        end
        isempty(tags) && info("No new package versions to publish")
        info("Validating METADATA")
        check_metadata(Set(keys(tags)))
    end

    for pkg in sort!(collect(keys(tags)))
        with(GitRepo, pkg) do pkg_repo
            forced = ASCIIString[]
            unforced = ASCIIString[]
            for tag in tags[pkg]
                ver = convert(VersionNumber,tag)
                push!(isrewritable(ver) ? forced : unforced, tag)
            end
            if !isempty(forced)
                info("Pushing $pkg temporary tags: ", join(forced,", "))
                LibGit2.push(pkg_repo, remote="origin", force=true,
                             refspecs=["refs/tags/$tag:refs/tags/$tag" for tag in forced])
            end
            if !isempty(unforced)
                info("Pushing $pkg permanent tags: ", join(unforced,", "))
                LibGit2.push(pkg_repo, remote="origin",
                             refspecs=["refs/tags/$tag:refs/tags/$tag" for tag in unforced])
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
                throw(PkgError("$pkg can't be installed because it has no versions that support ", VERSION, " of julia. " *
                   "You may need to update METADATA by running `Pkg.update()`"))
            else
                throw(PkgError("$pkg's requirements can't be satisfied because of the following fixed packages: ",
                   join(conflicts[pkg], ", ", " and ")))
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
        ver1 !== nothing && push!(vers,LibGit2.head(pkg))
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
        throw(PkgError(msg))
    end

    # try applying changes, roll back everything if anything fails
    changed = []
    try
        for (pkg,(ver1,ver2)) in changes
            if ver1 === nothing
                info("Installing $pkg v$ver2")
                Write.install(pkg, Read.sha1(pkg,ver2))
            elseif ver2 === nothing
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
            if ver1 === nothing
                info("Rolling back install of $pkg")
                @recover Write.remove(pkg)
            elseif ver2 === nothing
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
    build(map(x->x[1], filter(x -> x[2][2] !== nothing, changes)))
end

function write_tag_metadata(repo::GitRepo, pkg::AbstractString, ver::VersionNumber, commit::AbstractString, force::Bool=false)
    content = LibGit2.cat(repo, LibGit2.GitBlob, "$commit:REQUIRE")
    reqs = content != nothing ? Reqs.read(split(content, '\n', keep=false)) : Reqs.Line[]
    cd("METADATA") do
        d = joinpath(pkg,"versions",string(ver))
        mkpath(d)
        sha1file = joinpath(d,"sha1")
        if !force && ispath(sha1file)
            current = readchomp(sha1file)
            current == commit ||
                throw(PkgError("$pkg v$ver is already registered as $current, bailing"))
        end
        open(io->println(io,commit), sha1file, "w")
        LibGit2.add!(repo, sha1file)
        reqsfile = joinpath(d,"requires")
        if isempty(reqs)
            ispath(reqsfile) && LibGit2.remove!(repo, reqsfile)
        else
            Reqs.write(reqsfile,reqs)
            LibGit2.add!(repo, reqsfile)
        end
    end
    return nothing
end

function register(pkg::AbstractString, url::AbstractString)
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    isfile("METADATA",pkg,"url") && throw(PkgError("$pkg already registered"))
    LibGit2.transact(GitRepo("METADATA")) do repo
        # Get versions from package repo
        versions = with(GitRepo, pkg) do pkg_repo
            tags = filter(t->startswith(t,"v"), LibGit2.tag_list(pkg_repo))
            filter!(tag->ismatch(Base.VERSION_REGEX,tag), tags)
            [
                convert(VersionNumber,tag) => string(LibGit2.revparseid(pkg_repo, "$tag^{commit}"))
                for tag in tags
            ]
        end
        # Register package url in METADATA
        cd("METADATA") do
            info("Registering $pkg at $url")
            mkdir(pkg)
            path = joinpath(pkg,"url")
            open(io->println(io,url), path, "w")
            LibGit2.add!(repo, path)
        end
        # Register package version in METADATA
        vers = sort!(collect(keys(versions)))
        for ver in vers
            info("Tagging $pkg v$ver")
            write_tag_metadata(repo, pkg,ver,versions[ver])
        end
        # Commit changes in METADATA
        if LibGit2.isdirty(repo)
            info("Committing METADATA for $pkg")
            msg = "Register $pkg"
            if !isempty(versions)
                msg *= ": $(join(map(v->"v$v", vers),", "))"
            end
            LibGit2.commit(repo, msg)
        else
            info("No METADATA changes to commit")
        end
    end
    return
end

function register(pkg::AbstractString)
    url = ""
    try
        url = LibGit2.getconfig(pkg, "remote.origin.url", "")
    catch err
        throw(PkgError("$pkg: $err"))
    end
    !isempty(url) || throw(PkgError("$pkg: no URL configured"))
    register(pkg, LibGit2.normalize_url(url))
end

function isrewritable(v::VersionNumber)
    thispatch(v)==v"0" ||
    length(v.prerelease)==1 && isempty(v.prerelease[1]) ||
    length(v.build)==1 && isempty(v.build[1])
end

nextbump(v::VersionNumber) = isrewritable(v) ? v : nextpatch(v)

function tag(pkg::AbstractString, ver::Union(Symbol,VersionNumber), force::Bool=false, commitish::AbstractString="HEAD")
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    with(GitRepo,"METADATA") do repo
        LibGit2.isdirty(repo, pkg) && throw(PkgError("METADATA/$pkg is dirty – commit or stash changes to tag"))
    end
    with(GitRepo,pkg) do repo
        LibGit2.isdirty(repo) && throw(PkgError("$pkg is dirty – commit or stash changes to tag"))
        commit = string(LibGit2.revparseid(repo, commitish))
        registered = isfile("METADATA",pkg,"url")

        if !force
            if registered
                avail = Read.available(pkg)
                existing = VersionNumber[keys(Read.available(pkg))...]
                ancestors = filter(v->LibGit2.is_ancestor_of(avail[v].sha1, commit, repo), existing)
            else
                tags = filter(t->startswith(t,"v"), Pkg.LibGit2.tag_list(repo))
                filter!(tag->ismatch(Base.VERSION_REGEX,tag), tags)
                existing = VersionNumber[tags...]
                filter!(tags) do tag
                    sha1 = LibGit2.revparseid(repo, "$tag^{commit}")
                    LibGit2.is_ancestor_of(sha1, commit, repo)
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
                                        throw(PkgError("invalid version selector: $ver"))
            end
            isrewritable(ver) && filter!(v->v!=ver,existing)
            check_new_version(existing,ver)
        end
        # TODO: check that SHA1 isn't the same as another version
        info("Tagging $pkg v$ver")
        LibGit2.tag_create(repo, "v$ver", commit,
                           msg=(!isrewritable(ver) ? "$pkg v$ver [$(commit[1:10])]" : ""),
                           force=(force || isrewritable(ver)) )
        registered || return
        try
            LibGit2.transact(GitRepo("METADATA")) do repo
                write_tag_metadata(repo, pkg, ver, commit, force)
                if LibGit2.isdirty(repo)
                    info("Committing METADATA for $pkg")
                    LibGit2.commit(repo, "Tag $pkg v$ver")
                    #run(`commit -q -m "Tag $pkg v$ver" -- $pkg`, dir="METADATA")
                else
                    info("No METADATA changes to commit")
                end
            end
        catch
            LibGit2.tag_delete(repo, "v$ver")
            rethrow()
        end
    end
    return
end

function check_metadata(pkgs::Set{ByteString} = Set{ByteString}())
    avail = Read.available()
    deps, conflicts = Query.dependencies(avail)

    for (dp,dv) in deps, (v,a) in dv, p in keys(a.requires)
        haskey(deps, p) || throw(PkgError("package $dp v$v requires a non-registered package: $p"))
    end

    problematic = Resolve.sanity_check(deps, pkgs)
    if !isempty(problematic)
        msg = "packages with unsatisfiable requirements found:\n"
        for (p, vn, rp) in problematic
            msg *= "    $p v$vn – no valid versions exist for package $rp\n"
        end
        throw(PkgError(msg))
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
        pkg == "julia" && continue
        pkg in seen && continue
        build!(Read.requires_list(pkg),errs,push!(seen,pkg))
        Read.isinstalled(pkg) || throw(PkgError("$pkg is not an installed package"))
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
build() = build(sort!(collect(keys(installed()))))

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
                codecov = coverage? ["--code-coverage=user", "--inline=no"] : ["--code-coverage=none"]
                julia_exe = joinpath(JULIA_HOME, Base.julia_exename())
                run(`$julia_exe --check-bounds=yes $codecov $color $test_path`)
                info("$pkg tests passed")
            catch err
                warnbanner(err, label="[ ERROR: $pkg ]")
                push!(errs,pkg)
            end
        end
    else
        push!(notests,pkg)
    end
    isfile(reqs_path) && resolve()
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
        throw(PkgError(join(messages, "and")))
    end
end

test(;coverage::Bool=false) = test(sort!(AbstractString[keys(installed())...]); coverage=coverage)

end # module
