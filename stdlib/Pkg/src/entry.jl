# This file is a part of Julia. License is MIT: https://julialang.org/license

module Entry

import Base: thispatch, nextpatch, nextminor, nextmajor, check_new_version
import Pkg
import ..Reqs, ..Read, ..Query, ..Resolve, ..Cache, ..Write, ..Dir
using LibGit2
import ..PkgError
using ..Types
using Base.Printf: @printf

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
    @info "Package database updated"
    return true
end

function edit()
    editor = get(ENV,"VISUAL",get(ENV,"EDITOR",nothing))
    editor !== nothing ||
        throw(PkgError("set the EDITOR environment variable to an edit command"))
    editor = Base.shell_split(editor)
    reqs = Reqs.parse("REQUIRE")
    run(`$editor REQUIRE`)
    reqsʹ = Reqs.parse("REQUIRE")
    reqs == reqsʹ && return @info "Nothing to be done"
    @info "Computing changes..."
    resolve(reqsʹ)
end

function add(pkg::AbstractString, vers::VersionSet)
    outdated = :maybe
    @sync begin
        @async if !edit(Reqs.add,pkg,vers)
            ispath(pkg) || throw(PkgError("unknown package $pkg"))
            @info "Package $pkg is already installed"
        end
        branch = Dir.getmetabranch()
        outdated = with(GitRepo, "METADATA") do repo
            if LibGit2.branch(repo) == branch
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
        @info """
            METADATA $is out-of-date — you may not have the latest version of $pkg
            Use `Pkg.update()` to get the latest versions of your packages
            """
    end
end
add(pkg::AbstractString, vers::VersionNumber...) = add(pkg,VersionSet(vers...))

function rm(pkg::AbstractString)
    edit(Reqs.rm,pkg) && return
    ispath(pkg) || return @info "Package $pkg is not installed"
    @info "Removing $pkg (unregistered)"
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
    pkgs = Dict{String,VersionNumber}()
    for (pkg,(ver,fix)) in Read.installed()
        pkgs[pkg] = ver
    end
    return pkgs
end

function installed(pkg::AbstractString)
    avail = Read.available(pkg)
    if Read.isinstalled(pkg)
        res = typemin(VersionNumber)
        if ispath(joinpath(pkg,".git"))
            LibGit2.with(GitRepo, pkg) do repo
                res = Read.installed_version(pkg, repo, avail)
            end
        end
        return res
    end
    isempty(avail) && throw(PkgError("$pkg is not a package (not registered or installed)"))
    return nothing # registered but not installed
end

function status(io::IO; pkgname::AbstractString = "")
    if !isempty(pkgname) && !ispath(pkgname)
        throw(PkgError("Package $pkgname does not exist"))
    end
    showpkg(pkg) = isempty(pkgname) ? true : (pkg == pkgname)
    reqs = Reqs.parse("REQUIRE")
    instd = Read.installed()
    required = sort!(collect(keys(reqs)))
    if !isempty(required)
        showpkg("") && println(io, "$(length(required)) required packages:")
        for pkg in required
            if !haskey(instd, pkg)
                showpkg(pkg) && status(io,pkg,"not found")
            else
                ver,fix = pop!(instd,pkg)
                showpkg(pkg) && status(io,pkg,ver,fix)
            end
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
    if !isempty(pkg) && !ispath(pkg)
        throw(PkgError("Package $pkg does not exist"))
    end
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
                    print(io, string(LibGit2.GitHash(phead))[1:8])
                end
            end
            attrs = AbstractString[]
            isfile("METADATA",pkg,"url") || push!(attrs,"unregistered")
            LibGit2.isdirty(prepo) && push!(attrs,"dirty")
            isempty(attrs) || print(io, " (",join(attrs,", "),")")
        catch err
            printstyled(io, " broken-repo (unregistered)", color=Base.error_color())
        finally
            close(prepo)
        end
    else
        printstyled(io, "non-repo (unregistered)", color=Base.warn_color())
    end
    println(io)
end

function status(io::IO, pkg::AbstractString, msg::AbstractString)
    @printf io " - %-29s %-19s\n" pkg msg
end

function clone(url::AbstractString, pkg::AbstractString)
    @info "Cloning $pkg from $url"
    ispath(pkg) && throw(PkgError("$pkg already exists"))
    try
        LibGit2.with(LibGit2.clone(url, pkg)) do repo
            LibGit2.set_remote_url(repo, "origin", url)
        end
    catch err
        isdir(pkg) && Base.rm(pkg, recursive=true)
        rethrow(err)
    end
    @info "Computing changes..."
    if !edit(Reqs.add, pkg)
        isempty(Reqs.parse("$pkg/REQUIRE")) && return
        resolve()
    end
end

function url_and_pkg(url_or_pkg::AbstractString)
    if !(':' in url_or_pkg)
        # no colon, could be a package name
        url_file = joinpath("METADATA", url_or_pkg, "url")
        isfile(url_file) && return readchomp(url_file), url_or_pkg
    end
    # try to parse as URL or local path
    m = match(r"(?:^|[/\\])(\w+?)(?:\.jl)?(?:\.git)?$", url_or_pkg)
    m === nothing && throw(PkgError("can't determine package name from URL: $url_or_pkg"))
    return url_or_pkg, m.captures[1]
end

clone(url_or_pkg::AbstractString) = clone(url_and_pkg(url_or_pkg)...)

function checkout(pkg::AbstractString, branch::AbstractString, do_merge::Bool, do_pull::Bool)
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    @info "Checking out $pkg $branch..."
    with(GitRepo, pkg) do r
        LibGit2.transact(r) do repo
            LibGit2.isdirty(repo) && throw(PkgError("$pkg is dirty, bailing"))
            LibGit2.branch!(repo, branch, track=LibGit2.Consts.REMOTE_ORIGIN)
            do_merge && LibGit2.merge!(repo, fastforward=true) # merge changes
            if do_pull
                @info "Pulling $pkg latest $branch..."
                LibGit2.fetch(repo)
                LibGit2.merge!(repo, fastforward=true)
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
        @info "Freeing $pkg"
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

function free(pkgs)
    try
        for pkg in pkgs
            ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
            Read.isinstalled(pkg) || throw(PkgError("$pkg cannot be freed – not an installed package"))
            avail = Read.available(pkg)
            isempty(avail) && throw(PkgError("$pkg cannot be freed – not a registered package"))
            with(GitRepo, pkg) do repo
                LibGit2.isdirty(repo) && throw(PkgError("$pkg cannot be freed – repo is dirty"))
                @info "Freeing $pkg"
                vers = sort!(collect(keys(avail)), rev=true)
                for ver in vers
                    sha1 = avail[ver].sha1
                    LibGit2.iscommit(sha1, repo) || continue
                    LibGit2.checkout!(repo, sha1)
                    break
                end
            end
            isempty(Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail])) && continue
            throw(PkgError("Can't find any registered versions of $pkg to checkout"))
        end
    finally
        resolve()
    end
end

function pin(pkg::AbstractString, head::AbstractString)
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    should_resolve = true
    with(GitRepo, pkg) do repo
        id = if isempty(head) # get HEAD commit
            # no need to resolve, branch will be from HEAD
            should_resolve = false
            LibGit2.head_oid(repo)
        else
            LibGit2.revparseid(repo, head)
        end
        commit = LibGit2.GitCommit(repo, id)
        try
            # note: changing the following naming scheme requires a corresponding change in Read.ispinned()
            branch = "pinned.$(string(id)[1:8]).tmp"
            if LibGit2.isattached(repo) && LibGit2.branch(repo) == branch
                @info "Package $pkg is already pinned" * (isempty(head) ? "" : " to the selected commit")
                should_resolve = false
                return
            end
            ref = LibGit2.lookup_branch(repo, branch)
            try
                if ref !== nothing
                    if LibGit2.revparseid(repo, branch) != id
                        throw(PkgError("Package $pkg: existing branch $branch has " *
                            "been edited and doesn't correspond to its original commit"))
                    end
                    @info "Package $pkg: checking out existing branch $branch"
                else
                    @info "Creating $pkg branch $branch"
                    ref = LibGit2.create_branch(repo, branch, commit)
                end

                # checkout selected branch
                with(LibGit2.peel(LibGit2.GitTree, ref)) do btree
                    LibGit2.checkout_tree(repo, btree)
                end
                # switch head to the branch
                LibGit2.head!(repo, ref)
            finally
                close(ref)
            end
        finally
            close(commit)
        end
    end
    should_resolve && resolve()
    nothing
end
pin(pkg::AbstractString) = pin(pkg, "")

function pin(pkg::AbstractString, ver::VersionNumber)
    ispath(pkg,".git") || throw(PkgError("$pkg is not a git repo"))
    Read.isinstalled(pkg) || throw(PkgError("$pkg cannot be pinned – not an installed package"))
    avail = Read.available(pkg)
    isempty(avail) && throw(PkgError("$pkg cannot be pinned – not a registered package"))
    haskey(avail,ver) || throw(PkgError("$pkg – $ver is not a registered version"))
    pin(pkg, avail[ver].sha1)
end

function update(branch::AbstractString, upkgs::Set{String})
    @info "Updating METADATA..."
    with(GitRepo, "METADATA") do repo
        try
            with(LibGit2.head(repo)) do h
                if LibGit2.branch(h) != branch
                    if LibGit2.isdirty(repo)
                        throw(PkgError("METADATA is dirty and not on $branch, bailing"))
                    end
                    if !LibGit2.isattached(repo)
                        throw(PkgError("METADATA is detached not on $branch, bailing"))
                    end
                    LibGit2.fetch(repo)
                    LibGit2.checkout_head(repo)
                    LibGit2.branch!(repo, branch, track="refs/remotes/origin/$branch")
                    LibGit2.merge!(repo)
                end
            end

            LibGit2.fetch(repo)
            ff_succeeded = LibGit2.merge!(repo, fastforward=true)
            if !ff_succeeded
                LibGit2.rebase!(repo, "origin/$branch")
            end
        catch err
            cex = CapturedException(err, catch_backtrace())
            throw(PkgError("METADATA cannot be updated. Resolve problems manually in " *
                Pkg.dir("METADATA") * ".", cex))
        end
    end
    deferred_errors = CompositeException()
    avail = Read.available()
    # this has to happen before computing free/fixed
    for pkg in filter(Read.isinstalled, collect(keys(avail)))
        try
            Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
        catch err
            cex = CapturedException(err, catch_backtrace())
            push!(deferred_errors, PkgError("Package $pkg: unable to update cache.", cex))
        end
    end
    instd = Read.installed(avail)
    reqs = Reqs.parse("REQUIRE")
    if !isempty(upkgs)
        for (pkg, (v,f)) in instd
            satisfies(pkg, v, reqs) || throw(PkgError("Package $pkg: current " *
                "package status does not satisfy the requirements, cannot do " *
                "a partial update; use `Pkg.update()`"))
        end
    end
    dont_update = Query.partial_update_mask(instd, avail, upkgs)
    free  = Read.free(instd,dont_update)
    for (pkg,ver) in free
        try
            Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
        catch err
            cex = CapturedException(err, catch_backtrace())
            push!(deferred_errors, PkgError("Package $pkg: unable to update cache.", cex))
        end
    end
    fixed = Read.fixed(avail,instd,dont_update)
    creds = LibGit2.CachedCredentials()
    try
        stopupdate = false
        for (pkg,ver) in fixed
            ispath(pkg,".git") || continue
            pkg in dont_update && continue
            with(GitRepo, pkg) do repo
                if LibGit2.isattached(repo)
                    if LibGit2.isdirty(repo)
                        @warn "Package $pkg: skipping update (dirty)..."
                    elseif Read.ispinned(repo)
                        @info "Package $pkg: skipping update (pinned)..."
                    else
                        prev_sha = string(LibGit2.head_oid(repo))
                        success = true
                        try
                            LibGit2.fetch(repo, payload=LibGit2.CredentialPayload(creds))
                            LibGit2.merge!(repo, fastforward=true)
                        catch err
                            cex = CapturedException(err, catch_backtrace())
                            push!(deferred_errors, PkgError("Package $pkg cannot be updated.", cex))
                            success = false
                            stopupdate = isa(err, InterruptException)
                        end
                        if success
                            post_sha = string(LibGit2.head_oid(repo))
                            branch = LibGit2.branch(repo)
                            @info "Updating $pkg $branch..." * (prev_sha != post_sha ?
                                  " $(prev_sha[1:8]) → $(post_sha[1:8])" : "")
                        end
                    end
                end
            end
            stopupdate && break
            if haskey(avail,pkg)
                try
                    Cache.prefetch(pkg, Read.url(pkg), [a.sha1 for (v,a)=avail[pkg]])
                catch err
                    cex = CapturedException(err, catch_backtrace())
                    push!(deferred_errors, PkgError("Package $pkg: unable to update cache.", cex))
                end
            end
        end
    finally
        Base.securezero!(creds)
    end
    @info "Computing changes..."
    resolve(reqs, avail, instd, fixed, free, upkgs)
    # Don't use instd here since it may have changed
    updatehook(sort!(collect(keys(installed()))))

    # Print deferred errors
    length(deferred_errors) > 0 && throw(PkgError("Update finished with errors.", deferred_errors))
    nothing
end


function resolve(
    reqs  :: Dict = Reqs.parse("REQUIRE"),
    avail :: Dict = Read.available(),
    instd :: Dict = Read.installed(avail),
    fixed :: Dict = Read.fixed(avail, instd),
    have  :: Dict = Read.free(instd),
    upkgs :: Set{String} = Set{String}()
)
    bktrc = Query.init_resolve_backtrace(reqs, fixed)
    orig_reqs = deepcopy(reqs)
    Query.check_fixed(reqs, fixed, avail)
    Query.propagate_fixed!(reqs, bktrc, fixed)
    deps, conflicts = Query.dependencies(avail, fixed)

    for pkg in keys(reqs)
        if !haskey(deps,pkg)
            if "julia" in conflicts[pkg]
                throw(PkgError("$pkg can't be installed because it has no versions that support $VERSION " *
                   "of julia. You may need to update METADATA by running `Pkg.update()`"))
            else
                sconflicts = join(conflicts[pkg], ", ", " and ")
                throw(PkgError("$pkg's requirements can't be satisfied because " *
                    "of the following fixed packages: $sconflicts"))
            end
        end
    end

    Query.check_requirements(reqs, deps, fixed)

    deps = Query.prune_dependencies(reqs, deps, bktrc)
    want = Resolve.resolve(reqs, deps)

    if !isempty(upkgs)
        orig_deps, _ = Query.dependencies(avail)
        Query.check_partial_updates(orig_reqs, orig_deps, want, fixed, upkgs)
    end

    # compare what is installed with what should be
    changes = Query.diff(have, want, avail, fixed)
    isempty(changes) && return @info "No packages to install, update or remove"

    # prefetch phase isolates network activity, nothing to roll back
    missing = []
    for (pkg,(ver1,ver2)) in changes
        vers = String[]
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
    imported = String[]
    try
        for (pkg,(ver1,ver2)) in changes
            if ver1 === nothing
                @info "Installing $pkg v$ver2"
                Write.install(pkg, Read.sha1(pkg,ver2))
            elseif ver2 === nothing
                @info "Removing $pkg v$ver1"
                Write.remove(pkg)
            else
                up = ver1 <= ver2 ? "Up" : "Down"
                @info "$(up)grading $pkg: v$ver1 => v$ver2"
                Write.update(pkg, Read.sha1(pkg,ver2))
                if Base.root_module_exists(Base.PkgId(pkg))
                    push!(imported, "- $pkg")
                end
            end
            push!(changed,(pkg,(ver1,ver2)))
        end
    catch err
        for (pkg,(ver1,ver2)) in reverse!(changed)
            if ver1 === nothing
                @info "Rolling back install of $pkg"
                @recover Write.remove(pkg)
            elseif ver2 === nothing
                @info "Rolling back deleted $pkg to v$ver1"
                @recover Write.install(pkg, Read.sha1(pkg,ver1))
            else
                @info "Rolling back $pkg from v$ver2 to v$ver1"
                @recover Write.update(pkg, Read.sha1(pkg,ver1))
            end
        end
        rethrow(err)
    end
    if !isempty(imported)
        @warn join(["The following packages have been updated but were already imported:",
            imported..., "Restart Julia to use the updated versions."], "\n")
    end
    # re/build all updated/installed packages
    build(map(x->x[1], filter(x -> x[2][2] !== nothing, changes)))
end

function build(pkg::AbstractString, build_file::AbstractString, errfile::AbstractString)
    # To isolate the build from the running Julia process, we execute each build.jl file in
    # a separate process. Errors are written to errfile for later reporting.
    code = """
        import Pkg
        empty!(Base.LOAD_PATH)
        append!(Base.LOAD_PATH, $(repr(LOAD_PATH, context=:module=>nothing)))
        empty!(Base.DEPOT_PATH)
        append!(Base.DEPOT_PATH, $(repr(DEPOT_PATH)))
        empty!(Base.DL_LOAD_PATH)
        append!(Base.DL_LOAD_PATH, $(repr(Base.DL_LOAD_PATH)))
        open("$(escape_string(errfile))", "a") do f
            pkg, build_file = "$pkg", "$(escape_string(build_file))"
            try
                @info "Building \$pkg"
                cd(dirname(build_file)) do
                    evalfile(build_file)
                end
            catch err
                @error \"""
                    ------------------------------------------------------------
                    # Build failed for \$pkg
                    \""" exception=err,catch_backtrace()
                write(f, pkg); write(f, 0x00)
                write(f, sprint(showerror, err)); write(f, 0x00)
            end
        end
        """
    cmd = ```
        $(Base.julia_cmd()) -O0
        --color=$(Base.have_color ? "yes" : "no")
        --compiled-modules=$(Bool(Base.JLOptions().use_compiled_modules) ? "yes" : "no")
        --history-file=no
        --startup-file=$(Base.JLOptions().startupfile != 2 ? "yes" : "no")
        --eval $code
        ```

    success(pipeline(cmd, stdout=stdout, stderr=stderr))
end

function build!(pkgs::Vector, seen::Set, errfile::AbstractString)
    for pkg in pkgs
        pkg == "julia" && continue
        pkg in seen ? continue : push!(seen,pkg)
        Read.isinstalled(pkg) || throw(PkgError("$pkg is not an installed package"))
        build!(Read.requires_list(pkg), seen, errfile)
        path = abspath(pkg,"deps","build.jl")
        isfile(path) || continue
        build(pkg, path, errfile) || error("Build process failed.")
    end
end

function build!(pkgs::Vector, errs::Dict, seen::Set=Set())
    mktemp() do errfile, f
        build!(pkgs, seen, errfile)
        while !eof(f)
            pkg = readuntil(f, '\0')
            err = readuntil(f, '\0')
            errs[pkg] = err
        end
    end
end

function build(pkgs::Vector)
    errs = Dict()
    build!(pkgs,errs)
    isempty(errs) && return
    @warn """
        ------------------------------------------------------------
        # Build error summary

        $(join(keys(errs),", "," and ")) had build errors.

         - packages with build errors remain installed in $(pwd())
         - build the package(s) and all dependencies with `Pkg.build("$(join(keys(errs),"\", \""))")`
         - build a single package by running its `deps/build.jl` script
        """
end
build() = build(sort!(collect(keys(installed()))))

function updatehook!(pkgs::Vector, errs::Dict, seen::Set=Set())
    for pkg in pkgs
        pkg in seen && continue
        updatehook!(Read.requires_list(pkg),errs,push!(seen,pkg))
        path = abspath(pkg,"deps","update.jl")
        isfile(path) || continue
        @info "Running update script for $pkg"
        cd(dirname(path)) do
            try evalfile(path)
            catch err
                @error """
                    ------------------------------------------------------------
                    # Update hook failed for $pkg
                    """ exception=err,catch_backtrace()
                errs[pkg] = err
            end
        end
    end
end

function updatehook(pkgs::Vector)
    errs = Dict()
    updatehook!(pkgs,errs)
    isempty(errs) && return
    println(stderr)
    @warn """
        ------------------------------------------------------------
        # Update hook summary

        $(join(keys(errs),", "," and ")) had update errors.

        - Unrelated packages are unaffected
        - To retry, run Pkg.update() again
        """
end

function test!(pkg::AbstractString,
               errs::Vector{AbstractString},
               nopkgs::Vector{AbstractString},
               notests::Vector{AbstractString}; coverage::Bool=false)
    reqs_path = abspath(pkg,"test","REQUIRE")
    if isfile(reqs_path)
        tests_require = Reqs.parse(reqs_path)
        if (!isempty(tests_require))
            @info "Computing test dependencies for $pkg..."
            resolve(merge(Reqs.parse("REQUIRE"), tests_require))
        end
    end
    test_path = abspath(pkg,"test","runtests.jl")
    if !isdir(pkg)
        push!(nopkgs, pkg)
    elseif !isfile(test_path)
        push!(notests, pkg)
    else
        @info "Testing $pkg"
        cd(dirname(test_path)) do
            try
                cmd = ```
                    $(Base.julia_cmd())
                    --code-coverage=$(coverage ? "user" : "none")
                    --color=$(Base.have_color ? "yes" : "no")
                    --compiled-modules=$(Bool(Base.JLOptions().use_compiled_modules) ? "yes" : "no")
                    --check-bounds=yes
                    --warn-overwrite=yes
                    --startup-file=$(Base.JLOptions().startupfile != 2 ? "yes" : "no")
                    $test_path
                    ```
                run(cmd)
                @info "$pkg tests passed"
            catch err
                @error """
                    ------------------------------------------------------------
                    # Testing failed for $pkg
                    """ exception=err,catch_backtrace()
                push!(errs,pkg)
            end
        end
    end
    isfile(reqs_path) && resolve()
end

struct PkgTestError <: Exception
    msg::AbstractString
end

function Base.showerror(io::IO, ex::PkgTestError, bt; backtrace=true)
    printstyled(io, ex.msg, color=Base.error_color())
end

function test(pkgs::Vector{AbstractString}; coverage::Bool=false)
    errs = AbstractString[]
    nopkgs = AbstractString[]
    notests = AbstractString[]
    for pkg in pkgs
        test!(pkg,errs,nopkgs,notests; coverage=coverage)
    end
    if !all(isempty, (errs, nopkgs, notests))
        messages = AbstractString[]
        if !isempty(errs)
            push!(messages, "$(join(errs,", "," and ")) had test errors")
        end
        if !isempty(nopkgs)
            msg = length(nopkgs) > 1 ? " are not installed packages" :
                                       " is not an installed package"
            push!(messages, string(join(nopkgs,", ", " and "), msg))
        end
        if !isempty(notests)
            push!(messages, "$(join(notests,", "," and ")) did not provide a test/runtests.jl file")
        end
        throw(PkgTestError(join(messages, "and")))
    end
end

test(;coverage::Bool=false) = test(sort!(AbstractString[keys(installed())...]); coverage=coverage)

end # module
