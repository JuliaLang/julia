# This file is a part of Julia. License is MIT: https://julialang.org/license

module Operations

using UUIDs
using Random: randstring
import LibGit2

import REPL
using REPL.TerminalMenus
using ..Types, ..GraphType, ..Resolve, ..Pkg2, ..BinaryProvider, ..GitTools, ..Display
import ..depots, ..depots1, ..devdir, ..Types.uuid_julia

function find_installed(name::String, uuid::UUID, sha1::SHA1)
    slug_default = Base.version_slug(uuid, sha1)
    # 4 used to be the default so look there first
    for slug in (Base.version_slug(uuid, sha1, 4), slug_default)
        for depot in depots()
            path = abspath(depot, "packages", name, slug)
            ispath(path) && return path
        end
    end
    return abspath(depots1(), "packages", name, slug_default)
end

function load_versions(path::String)
    toml = parse_toml(path, "Versions.toml")
    return Dict{VersionNumber, SHA1}(VersionNumber(ver) => SHA1(info["git-tree-sha1"]) for (ver, info) in toml)
end

function load_package_data(f::Base.Callable, path::String, versions)
    toml = parse_toml(path, fakeit=true)
    data = Dict{VersionNumber,Dict{String,Any}}()
    for ver in versions
        ver::VersionNumber
        for (v, d) in toml, (key, value) in d
            vr = VersionRange(v)
            ver in vr || continue
            dict = get!(data, ver, Dict{String,Any}())
            haskey(dict, key) && pkgerror("$ver/$key is duplicated in $path")
            dict[key] = f(value)
        end
    end
    return data
end

load_package_data(f::Base.Callable, path::String, version::VersionNumber) =
    get(load_package_data(f, path, [version]), version, nothing)

function load_package_data_raw(T::Type, path::String)
    toml = parse_toml(path, fakeit=true)
    data = Dict{VersionRange,Dict{String,T}}()
    for (v, d) in toml, (key, value) in d
        vr = VersionRange(v)
        dict = get!(data, vr, Dict{String,T}())
        haskey(dict, key) && pkgerror("$vr/$key is duplicated in $path")
        dict[key] = T(value)
    end
    return data
end

#######################################
# Dependency gathering and resolution #
#######################################

function set_maximum_version_registry!(env::EnvCache, pkg::PackageSpec)
    pkgversions = Set{VersionNumber}()
    for path in registered_paths(env, pkg.uuid)
        pathvers = keys(load_versions(path))
        union!(pkgversions, pathvers)
    end
    if length(pkgversions) == 0
        pkg.version = VersionNumber(0)
    else
        max_version = maximum(pkgversions)
        pkg.version = VersionNumber(max_version.major, max_version.minor, max_version.patch, max_version.prerelease, ("",))
    end
end

# This also sets the .path field for fixed packages in `pkgs`
function collect_fixed!(ctx::Context, pkgs::Vector{PackageSpec}, uuid_to_name::Dict{UUID, String})
    fixed_pkgs = PackageSpec[]
    fix_deps_map = Dict{UUID,Vector{PackageSpec}}()
    uuid_to_pkg = Dict{UUID,PackageSpec}()
    for pkg in pkgs
        local path
        info = manifest_info(ctx.env, pkg.uuid)
        if pkg.special_action == PKGSPEC_FREED && !haskey(info, "pinned")
            continue
        elseif pkg.special_action == PKGSPEC_DEVELOPED
            @assert pkg.path !== nothing
            path = pkg.path
        elseif pkg.special_action == PKGSPEC_REPO_ADDED
            @assert pkg.repo !== nothing && pkg.repo.git_tree_sha1 !== nothing
            path = find_installed(pkg.name, pkg.uuid, pkg.repo.git_tree_sha1)
        elseif info !== nothing && haskey(info, "path")
            pkg.path = info["path"]
            path = pkg.path
        elseif info !== nothing && haskey(info, "repo-url")
            path = find_installed(pkg.name, pkg.uuid, SHA1(info["git-tree-sha1"]))
            pkg.repo = Types.GitRepo(info["repo-url"], info["repo-rev"], SHA1(info["git-tree-sha1"]))
        else
            continue
        end

        path = project_rel_path(ctx, path)
        if !isdir(path)
            pkgerror("path $(path) for package $(pkg.name) no longer exists. Remove the package or `develop` it at a new path")
        end

        uuid_to_pkg[pkg.uuid] = pkg
        uuid_to_name[pkg.uuid] = pkg.name
        found_project = collect_project!(ctx, pkg, path, fix_deps_map)
        if !found_project
            collect_require!(ctx, pkg, path, fix_deps_map)
        end
    end

    fixed = Dict{UUID,Fixed}()
    # Collect the dependencies for the fixed packages
    for (uuid, fixed_pkgs) in fix_deps_map
        fix_pkg = uuid_to_pkg[uuid]
        v = Dict{VersionNumber,Dict{UUID,VersionSpec}}()
        q = Dict{UUID, VersionSpec}()
        for deppkg in fixed_pkgs
            uuid_to_name[deppkg.uuid] = deppkg.name
            q[deppkg.uuid] = deppkg.version
        end
        fixed[uuid] = Fixed(fix_pkg.version, q)
    end
    return fixed
end

function collect_project!(ctx::Context, pkg::PackageSpec, path::String, fix_deps_map::Dict{UUID,Vector{PackageSpec}})
    project_file = projectfile_path(path)
    fix_deps_map[pkg.uuid] = valtype(fix_deps_map)()
    (project_file === nothing) && return false
    project = read_package(project_file)
    compat = get(project, "compat", Dict())
    if haskey(compat, "julia")
        if !(VERSION in Types.semver_spec(compat["julia"]))
            @warn("julia version requirement for package $(pkg.name) not satisfied")
        end
    end
    for (deppkg_name, uuid) in project["deps"]
        vspec = haskey(compat, deppkg_name) ? Types.semver_spec(compat[deppkg_name]) : VersionSpec()
        deppkg = PackageSpec(deppkg_name, UUID(uuid), vspec)
        push!(fix_deps_map[pkg.uuid], deppkg)
    end
    if haskey(project, "version")
        pkg.version = VersionNumber(project["version"])
    else
        # @warn "project file for $(pkg.name) is missing a `version` entry"
        set_maximum_version_registry!(ctx.env, pkg)
    end
    return true
end

# Backwards compatibility with Pkg2 REQUIRE format
function collect_require!(ctx::Context, pkg::PackageSpec, path::String, fix_deps_map::Dict{UUID,Vector{PackageSpec}})
    fix_deps = PackageSpec[]
    reqfile = joinpath(path, "REQUIRE")
    # Checked out "old-school" packages have by definition a version higher than all registered.
    set_maximum_version_registry!(ctx.env, pkg)
    !haskey(fix_deps_map, pkg.uuid) && (fix_deps_map[pkg.uuid] = valtype(fix_deps_map)())
    if isfile(reqfile)
        for r in Pkg2.Reqs.read(reqfile)
            r isa Pkg2.Reqs.Requirement || continue
            pkg_name, vspec = r.package, VersionSpec(VersionRange[r.versions.intervals...])
            if pkg_name == "julia"
                if !(VERSION in vspec)
                    @warn("julia version requirement for package $(pkg.name) not satisfied")
                end
            else
                deppkg = PackageSpec(pkg_name, vspec)
                push!(fix_deps_map[pkg.uuid], deppkg)
                push!(fix_deps, deppkg)
            end
        end

        # Packages from REQUIRE files need to get their UUID from the registry
        registry_resolve!(ctx.env, fix_deps)
        project_deps_resolve!(ctx.env, fix_deps)
        ensure_resolved(ctx.env, fix_deps; registry=true)
    end

    # And collect the stdlibs
    stdlibs = find_stdlib_deps(ctx, path)
    for (uuid, stdlib) in stdlibs
        deppkg = PackageSpec(stdlib, uuid, VersionSpec())
        push!(fix_deps_map[pkg.uuid], deppkg)
        push!(fix_deps, deppkg)
    end

    return
end


get_or_make(::Type{T}, d::Dict{K}, k::K) where {T,K} =
    haskey(d, k) ? convert(T, d[k]) : T()

get_or_make!(d::Dict{K,V}, k::K) where {K,V} = get!(d, k) do; V() end

function deps_graph(ctx::Context, uuid_to_name::Dict{UUID,String}, reqs::Requires, fixed::Dict{UUID,Fixed})
    uuids = collect(union(keys(reqs), keys(fixed), map(fx->keys(fx.requires), values(fixed))...))
    seen = UUID[]

    all_versions = Dict{UUID,Set{VersionNumber}}()
    all_deps     = Dict{UUID,Dict{VersionRange,Dict{String,UUID}}}()
    all_compat   = Dict{UUID,Dict{VersionRange,Dict{String,VersionSpec}}}()

    for (fp, fx) in fixed
        all_versions[fp] = Set([fx.version])
        all_deps[fp]     = Dict(VersionRange(fx.version) => Dict())
        all_compat[fp]   = Dict(VersionRange(fx.version) => Dict())
    end

    while true
        unseen = setdiff(uuids, seen)
        isempty(unseen) && break
        for uuid in unseen
            push!(seen, uuid)
            uuid in keys(fixed) && continue
            all_versions_u = get_or_make!(all_versions, uuid)
            all_deps_u     = get_or_make!(all_deps,     uuid)
            all_compat_u   = get_or_make!(all_compat,   uuid)
            # make sure all versions of all packages know about julia uuid
            if uuid ≠ uuid_julia
                deps_u_allvers = get_or_make!(all_deps_u, VersionRange())
                deps_u_allvers["julia"] = uuid_julia
            end

            # Collect deps + compat for stdlib
            if uuid in keys(ctx.stdlibs)
                path = Types.stdlib_path(ctx.stdlibs[uuid])
                proj_file = projectfile_path(path)
                @assert proj_file != nothing
                proj = Types.read_package(proj_file)

                v = haskey(proj, "version") ? (VersionNumber(proj["version"])) : VERSION
                push!(all_versions_u, v)
                vr = VersionRange(v)

                all_deps_u_vr = get_or_make!(all_deps_u, vr)
                for (name, _other_uuid) in proj["deps"]
                    other_uuid = UUID(_other_uuid)
                    all_deps_u_vr[name] = other_uuid
                    other_uuid in uuids || push!(uuids, other_uuid)
                end

                # TODO look at compat section for stdlibs?
                all_compat_u_vr = get_or_make!(all_compat_u, vr)
                for (name, other_uuid) in proj["deps"]
                    all_compat_u_vr[name] = VersionSpec()
                end
            else
                for path in registered_paths(ctx.env, uuid)
                    version_info = load_versions(path)
                    versions = sort!(collect(keys(version_info)))
                    deps_data = load_package_data_raw(UUID, joinpath(path, "Deps.toml"))
                    compat_data = load_package_data_raw(VersionSpec, joinpath(path, "Compat.toml"))

                    union!(all_versions_u, versions)

                    for (vr, dd) in deps_data
                        all_deps_u_vr = get_or_make!(all_deps_u, vr)
                        for (name,other_uuid) in dd
                            # check conflicts??
                            all_deps_u_vr[name] = other_uuid
                            other_uuid in uuids || push!(uuids, other_uuid)
                        end
                    end
                    for (vr, cd) in compat_data
                        all_compat_u_vr = get_or_make!(all_compat_u, vr)
                        for (name,vs) in cd
                            # check conflicts??
                            all_compat_u_vr[name] = vs
                        end
                    end
                end
            end
        end
        find_registered!(ctx.env, uuids)
    end

    for uuid in uuids
        uuid == uuid_julia && continue
        if !haskey(uuid_to_name, uuid)
            uuid_to_name[uuid] = registered_name(ctx.env, uuid)
            info = manifest_info(ctx.env, uuid)
            info ≡ nothing && continue
            uuid_to_name[UUID(info["uuid"])] = info["name"]
        end
    end

    return Graph(all_versions, all_deps, all_compat, uuid_to_name, reqs, fixed, #=verbose=# ctx.graph_verbose)
end

# Resolve a set of versions given package version specs
function resolve_versions!(ctx::Context, pkgs::Vector{PackageSpec})::Dict{UUID,VersionNumber}
    printpkgstyle(ctx, :Resolving, "package versions...")
    # anything not mentioned is fixed
    uuids = UUID[pkg.uuid for pkg in pkgs]
    uuid_to_name = Dict{UUID, String}(uuid => stdlib for (uuid, stdlib) in ctx.stdlibs)
    uuid_to_name[uuid_julia] = "julia"

    for (name::String, uuidstr::String) in ctx.env.project["deps"]
        uuid = UUID(uuidstr)
        uuid_to_name[uuid] = name

        uuid_idx = findfirst(isequal(uuid), uuids)
        info = manifest_info(ctx.env, uuid)
        if info !== nothing && haskey(info, "version") # stdlibs might not have a version
            ver = VersionSpec(VersionNumber(info["version"]))
        else
            ver = VersionSpec()
        end
        if uuid_idx != nothing
            pkg = pkgs[uuid_idx]
            if info !== nothing && pkg.special_action != PKGSPEC_FREED && get(info, "pinned", false)
                # This is a pinned package, fix its version
                pkg.version = ver
            end
        else
            pkg = PackageSpec(name, uuid, ver)
            push!(pkgs, pkg)
        end
    end

    # construct data structures for resolver and call it
    # this also sets pkg.version for fixed packages
    fixed = collect_fixed!(ctx, pkgs, uuid_to_name)

    # compatibility
    proj_compat = Types.project_compatibility(ctx, "julia")
    v = intersect(VERSION, proj_compat)
    if isempty(v)
        @warn "julia version requirement for project not satisfied" _module=nothing _file=nothing
    end

    for pkg in pkgs
        proj_compat = Types.project_compatibility(ctx, pkg.name)
        v = intersect(pkg.version, proj_compat)
        if isempty(v)
            pkgerror(string("empty intersection between $(pkg.name)@$(pkg.version) and project ",
                            "compatibility $(proj_compat)"))
        end
        # Work around not clobbering 0.x.y+ for checked out old type of packages
        if !(pkg.version isa VersionNumber)
            pkg.version = v
        end
    end

    reqs = Requires(pkg.uuid => VersionSpec(pkg.version) for pkg in pkgs if pkg.uuid ≠ uuid_julia)
    fixed[uuid_julia] = Fixed(VERSION)
    graph = deps_graph(ctx, uuid_to_name, reqs, fixed)

    simplify_graph!(graph)
    vers = resolve(graph)
    find_registered!(ctx.env, collect(keys(vers)))
    # update vector of package versions
    for pkg in pkgs
        # Fixed packages are not returned by resolve (they already have their version set)
        haskey(vers, pkg.uuid) && (pkg.version = vers[pkg.uuid])
    end
    uuids = UUID[pkg.uuid for pkg in pkgs]
    for (uuid, ver) in vers
        uuid in uuids && continue
        name = registered_name(ctx.env, uuid)
        push!(pkgs, PackageSpec(name, uuid, ver))
    end
    return vers
end

# Find names, repos and hashes for each package UUID & version
function version_data!(ctx::Context, pkgs::Vector{PackageSpec})
    names = Dict{UUID,String}()
    hashes = Dict{UUID,SHA1}()
    clones = Dict{UUID,Vector{String}}()
    for pkg in pkgs
        if pkg.uuid in keys(ctx.stdlibs)
            names[pkg.uuid] = ctx.stdlibs[pkg.uuid]
            continue
        elseif pkg.repo != nothing
            continue
        end
        pkg.path == nothing || continue
        uuid = pkg.uuid
        ver = pkg.version::VersionNumber
        clones[uuid] = String[]
        for path in registered_paths(ctx.env, uuid)
            info = parse_toml(path, "Package.toml")
            if haskey(names, uuid)
                names[uuid] == info["name"] ||
                    pkgerror("$uuid: name mismatch between registries: ",
                             "$(names[uuid]) vs. $(info["name"])")
            else
                names[uuid] = info["name"]
            end
            repo = info["repo"]
            repo in clones[uuid] || push!(clones[uuid], repo)
            vers = load_versions(path)
            if haskey(vers, ver)
                h = vers[ver]
                if haskey(hashes, uuid)
                    h == hashes[uuid] ||
                        @warn "$uuid: hash mismatch for version $ver!"
                else
                    hashes[uuid] = h
                end
            end
        end
        @assert haskey(hashes, uuid)
    end
    for pkg in pkgs
        haskey(names, pkg.uuid) && (pkg.name = names[pkg.uuid])
    end
    foreach(sort!, values(clones))
    return hashes, clones
end

########################
# Package installation #
########################
function get_archive_url_for_version(url::String, ref)
    if (m = match(r"https://github.com/(.*?)/(.*?).git", url)) != nothing
        return "https://api.github.com/repos/$(m.captures[1])/$(m.captures[2])/tarball/$(ref)"
    end
    return nothing
end

# can be removed after https://github.com/JuliaLang/julia/pull/27036
get_archive_url_for_version(url::String, hash::SHA1) = get_archive_url_for_version(url::String, string(hash))

# Returns if archive successfully installed
function install_archive(
    urls::Vector{String},
    hash::SHA1,
    version_path::String
)::Bool
    for url in urls
        archive_url = get_archive_url_for_version(url, hash)
        if archive_url != nothing
            path = tempname() * randstring(6) * ".tar.gz"
            url_success = true
            cmd = BinaryProvider.gen_download_cmd(archive_url, path);
            try
                run(cmd, (devnull, devnull, devnull))
            catch e
                e isa InterruptException && rethrow(e)
                url_success = false
            end
            url_success || continue
            dir = joinpath(tempdir(), randstring(12))
            mkpath(dir)
            cmd = BinaryProvider.gen_unpack_cmd(path, dir);
            # Might fail to extract an archive (Pkg#190)
            try
                run(cmd, (devnull, devnull, devnull))
            catch e
                e isa InterruptException && rethrow(e)
                @warn "failed to extract archive downloaded from $(archive_url)"
                url_success = false
            end
            url_success || continue
            dirs = readdir(dir)
            # 7z on Win might create this spurious file
            filter!(x -> x != "pax_global_header", dirs)
            @assert length(dirs) == 1
            !isdir(version_path) && mkpath(version_path)
            mv(joinpath(dir, dirs[1]), version_path; force=true)
            Base.rm(path; force = true)
            return true
        end
    end
    return false
end

const refspecs = ["+refs/*:refs/remotes/cache/*"]
function install_git(
    ctx::Context,
    uuid::UUID,
    name::String,
    hash::SHA1,
    urls::Vector{String},
    version::Union{VersionNumber,Nothing},
    version_path::String
)::Nothing
    repo = nothing
    tree = nothing
    try
        repo, git_hash = Base.shred!(LibGit2.CachedCredentials()) do creds
            clones_dir = joinpath(depots1(), "clones")
            ispath(clones_dir) || mkpath(clones_dir)
            repo_path = joinpath(clones_dir, string(uuid))
            repo = ispath(repo_path) ? LibGit2.GitRepo(repo_path) : begin
                GitTools.clone(urls[1], repo_path; isbare=true, header = "[$uuid] $name from $(urls[1])", credentials=creds)
            end
            git_hash = LibGit2.GitHash(hash.bytes)
            for url in urls
                try LibGit2.with(LibGit2.GitObject, repo, git_hash) do g
                    end
                    break # object was found, we can stop
                catch err
                    err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
                end
                GitTools.fetch(repo, url, refspecs=refspecs, credentials=creds)
            end
            return repo, git_hash
        end
        tree = try
            LibGit2.GitObject(repo, git_hash)
        catch err
            err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
            error("$name: git object $(string(hash)) could not be found")
        end
        tree isa LibGit2.GitTree ||
            error("$name: git object $(string(hash)) should be a tree, not $(typeof(tree))")
        mkpath(version_path)
        opts = LibGit2.CheckoutOptions(
            checkout_strategy = LibGit2.Consts.CHECKOUT_FORCE,
            target_directory = Base.unsafe_convert(Cstring, version_path)
        )
        LibGit2.checkout_tree(repo, tree, options=opts)
        return
    finally
        repo !== nothing && LibGit2.close(repo)
        tree !== nothing && LibGit2.close(tree)
    end
end

# install & update manifest
function apply_versions(ctx::Context, pkgs::Vector{PackageSpec})::Vector{UUID}
    hashes, urls = version_data!(ctx, pkgs)
    apply_versions(ctx, pkgs, hashes, urls)
end

function apply_versions(ctx::Context, pkgs::Vector{PackageSpec}, hashes::Dict{UUID,SHA1}, urls::Dict{UUID,Vector{String}})
    BinaryProvider.probe_platform_engines!()
    new_versions = UUID[]

    pkgs_to_install = Tuple{PackageSpec, String}[]
    for pkg in pkgs
        pkg.uuid in keys(ctx.stdlibs) && continue
        pkg.path == nothing || continue
        pkg.repo == nothing || continue
        path = find_installed(pkg.name, pkg.uuid, hashes[pkg.uuid])
        if !ispath(path)
            push!(pkgs_to_install, (pkg, path))
            push!(new_versions, pkg.uuid)
        end
    end

    widths = [textwidth(pkg.name) for (pkg, _) in pkgs_to_install]
    max_name = length(widths) == 0 ? 0 : maximum(widths)

    ########################################
    # Install from archives asynchronously #
    ########################################
    jobs = Channel(ctx.num_concurrent_downloads);
    results = Channel(ctx.num_concurrent_downloads);
    @async begin
        for pkg in pkgs_to_install
            put!(jobs, pkg)
        end
    end

    for i in 1:ctx.num_concurrent_downloads
        @async begin
            for (pkg, path) in jobs
                if ctx.preview
                    put!(results, (pkg, true, path))
                    continue
                end
                if ctx.use_libgit2_for_all_downloads
                    put!(results, (pkg, false, path))
                    continue
                end
                try
                    success = install_archive(urls[pkg.uuid], hashes[pkg.uuid], path)
                    if ctx.use_only_tarballs_for_downloads && !success
                        pkgerror("failed to get tarball from $(urls[pkg.uuid])")
                    end
                    put!(results, (pkg, success, path))
                catch err
                    put!(results, (pkg, err, catch_backtrace()))
                end
            end
        end
    end

    missed_packages = Tuple{PackageSpec, String}[]
    for i in 1:length(pkgs_to_install)
        pkg, exc_or_success, bt_or_path = take!(results)
        exc_or_success isa Exception && pkgerror("Error when installing packages:\n", sprint(Base.showerror, exc_or_success, bt_or_path))
        success, path = exc_or_success, bt_or_path
        if success
            vstr = pkg.version != nothing ? "v$(pkg.version)" : "[$h]"
            printpkgstyle(ctx, :Installed, string(rpad(pkg.name * " ", max_name + 2, "─"), " ", vstr))
        else
            push!(missed_packages, (pkg, path))
        end
    end

    ##################################################
    # Use LibGit2 to download any remaining packages #
    ##################################################
    for (pkg, path) in missed_packages
        uuid = pkg.uuid
        if !ctx.preview
            install_git(ctx, pkg.uuid, pkg.name, hashes[uuid], urls[uuid], pkg.version::VersionNumber, path)
        end
        vstr = pkg.version != nothing ? "v$(pkg.version)" : "[$h]"
        @info "Installed $(rpad(pkg.name * " ", max_name + 2, "─")) $vstr"
    end

    ##########################################
    # Installation done, update the manifest #
    ##########################################
    for pkg in pkgs
        uuid = pkg.uuid
        if pkg.path !== nothing || uuid in keys(ctx.stdlibs)
            hash = nothing
        elseif pkg.repo != nothing
            hash = pkg.repo.git_tree_sha1
        else
            hash = hashes[uuid]
        end
        update_manifest(ctx, pkg, hash)
    end

    prune_manifest(ctx.env)
    return new_versions
end

################################
# Manifest update and pruning #
################################
function find_stdlib_deps(ctx::Context, path::String)
    stdlib_deps = Dict{UUID, String}()
    regexps = [Regex("\\b(import|using)\\s+((\\w|\\.)+\\s*,\\s*)*$lib\\b") for lib in values(ctx.stdlibs)]
    for (root, dirs, files) in walkdir(path; onerror = x->nothing)
        for file in files
            endswith(file, ".jl") || continue
            filecontent = try read(joinpath(root, file), String)
                catch e
                    e isa SystemError || rethrow(e)
                    ""
                end
            for ((uuid, stdlib), r) in zip(ctx.stdlibs, regexps)
                if occursin(r, filecontent)
                    stdlib_deps[uuid] = stdlib
                end
            end
        end
    end
    return stdlib_deps
end

project_rel_path(ctx::Context, path::String) =
    normpath(joinpath(dirname(ctx.env.project_file), path))

function update_manifest(ctx::Context, pkg::PackageSpec, hash::Union{SHA1, Nothing})
    env = ctx.env
    uuid, name, version, path, special_action, repo = pkg.uuid, pkg.name, pkg.version, pkg.path, pkg.special_action, pkg.repo
    hash == nothing && @assert (path != nothing || pkg.uuid in keys(ctx.stdlibs) || pkg.repo != nothing)
    infos = get!(env.manifest, name, Dict{String,Any}[])
    info = nothing
    for i in infos
        UUID(i["uuid"]) == uuid || continue
        info = i
        break
    end
    if info == nothing
        info = Dict{String,Any}("uuid" => string(uuid))
        push!(infos, info)
    end
    is_stdlib = uuid in keys(ctx.stdlibs)
    if !is_stdlib
        info["version"] = string(version)
        hash == nothing ? delete!(info, "git-tree-sha1")  : (info["git-tree-sha1"] = string(hash))
        path == nothing ? delete!(info, "path")           : (info["path"]          = path)
        if special_action == PKGSPEC_DEVELOPED
            delete!(info, "pinned")
            delete!(info, "repo-url")
            delete!(info, "repo-rev")
        elseif special_action == PKGSPEC_FREED
            if get(info, "pinned", false)
                delete!(info, "pinned")
            else
                delete!(info, "repo-url")
                delete!(info, "repo-rev")
            end
        elseif special_action == PKGSPEC_PINNED
            info["pinned"] = true
        elseif special_action == PKGSPEC_REPO_ADDED
            info["repo-url"] = repo.url
            info["repo-rev"] = repo.rev
            path = find_installed(name, uuid, hash)
        end
        if haskey(info, "repo-url")
            path = find_installed(name, uuid, hash)
        end
    end

    delete!(info, "deps")
    if path != nothing || is_stdlib
        if is_stdlib
            path = Types.stdlib_path(name)
        else
            path = joinpath(dirname(ctx.env.project_file), path)
        end

        deps = Dict{String,String}()

        # Check for deps in project file
        project_file = projectfile_path(path)
        if nothing !== project_file
            project = read_project(project_file)
            deps = project["deps"]
        else
            # Check in REQUIRE file
            # Remove when packages uses Project files properly
            dep_pkgs = PackageSpec[]
            stdlib_deps = find_stdlib_deps(ctx, path)
            for (stdlib_uuid, stdlib) in stdlib_deps
                push!(dep_pkgs, PackageSpec(stdlib, stdlib_uuid))
            end
            reqfile = joinpath(path, "REQUIRE")
            if isfile(reqfile)
                for r in Pkg2.Reqs.read(reqfile)
                    r isa Pkg2.Reqs.Requirement || continue
                    push!(dep_pkgs, PackageSpec(r.package))
                end
                registry_resolve!(env, dep_pkgs)
                project_deps_resolve!(ctx.env, dep_pkgs)
                ensure_resolved(env, dep_pkgs; registry=true)
            end
            for dep_pkg in dep_pkgs
                dep_pkg.name == "julia" && continue
                deps[dep_pkg.name] = string(dep_pkg.uuid)
            end
        end
        if !isempty(deps)
            info["deps"] = deps
        end
    else
        for path in registered_paths(env, uuid)
            data = load_package_data(UUID, joinpath(path, "Deps.toml"), version)
            data == nothing && continue
            info["deps"] = Dict(string(k) => string(v) for (k,v) in data)
            break
        end
    end
    return
end

function prune_manifest(env::EnvCache)
    keep = map(UUID, values(env.project["deps"]))
    while !isempty(keep)
        clean = true
        for (name, infos) in env.manifest, info in infos
            haskey(info, "uuid") && haskey(info, "deps") || continue
            UUID(info["uuid"]) ∈ keep || continue
            for dep in map(UUID, values(info["deps"]))
                dep ∈ keep && continue
                push!(keep, dep)
                clean = false
            end
        end
        clean && break
    end
    filter!(env.manifest) do (_, infos)
        filter!(infos) do info
            haskey(info, "uuid") && UUID(info["uuid"]) ∈ keep
        end
        !isempty(infos)
    end
end

# When testing or building a dependency, we want that dependency to be able to load its own dependencies
# at top level. Therefore we would like to execute the build or testing of a dependency using its own Project file as
# the current environment. Being backwards compatible with REQUIRE file complicates the story a bit since these packages
# do not have any Project files.
function with_dependencies_loadable_at_toplevel(f, mainctx::Context, pkg::PackageSpec; might_need_to_resolve=false)
    # localctx is the context for the temporary environment we run the testing / building in
    localctx = deepcopy(mainctx)
    localctx.currently_running_target = true
    # If pkg or its dependencies are checked out, we will need to resolve
    # unless we already have resolved for the current environment, which the calleer indicates
    # with `might_need_to_resolve`
    need_to_resolve = false
    is_project = Types.is_project(localctx.env, pkg)

    # Only put `pkg` and its deps + target deps (recursively) in the temp project
    collect_deps!(seen, pkg) = begin
        pkg.uuid in seen && return
        push!(seen, pkg.uuid)
        info = manifest_info(localctx.env, pkg.uuid)
        info === nothing && return
        need_to_resolve |= haskey(info, "path")
        localctx.env.project["deps"][pkg.name] = string(pkg.uuid)
        for (dpkg, duuid) in get(info, "deps", [])
            collect_deps!(seen, PackageSpec(dpkg, UUID(duuid)))
        end
    end

    if is_project # testing the project itself
        # the project might have changes made to it so need to resolve
        need_to_resolve = true
        # Since we will create a temp environment in another place we need to extract the project
        # and put it in the Project as a normal `deps` entry and in the Manifest with a path.
        foreach(k->delete!(localctx.env.project, k), ("name", "uuid", "version"))
        localctx.env.pkg = nothing
        localctx.env.project["deps"][pkg.name] = string(pkg.uuid)
        localctx.env.manifest[pkg.name] = [Dict(
            "deps" => mainctx.env.project["deps"],
            "uuid" => string(pkg.uuid),
            "path" => dirname(localctx.env.project_file),
            "version" => string(pkg.version)
        )]
    else
        # Only put `pkg` and its deps (recursively) in the temp project
        empty!(localctx.env.project["deps"])
        localctx.env.project["deps"][pkg.name] = string(pkg.uuid)
        seen_uuids = Set{UUID}()
        # Only put `pkg` and its deps (recursively) in the temp project
        collect_deps!(seen_uuids, pkg)
    end

    pkgs = PackageSpec[]
    target = ""
    if pkg.special_action == PKGSPEC_TESTED
        target = "test"
    end
    if !isempty(target)
        collect_target_deps!(localctx, pkgs, pkg, target)
        seen_uuids = Set{UUID}()
        for dpkg in pkgs
            # Also put eventual deps of target deps in new manifest
            collect_deps!(seen_uuids, dpkg)
        end
    end

    mktempdir() do tmpdir
        localctx.env.project_file = joinpath(tmpdir, "Project.toml")
        localctx.env.manifest_file = joinpath(tmpdir, "Manifest.toml")

        function rewrite_manifests(manifest)
            # Rewrite paths in Manifest since relative paths won't work here due to the temporary environment
            for (name, infos) in manifest
                for iinfo in infos
                    # Is stdlib
                    if UUID(iinfo["uuid"]) in keys(localctx.stdlibs)
                        iinfo["path"] = Types.stdlib_path(name)
                    end
                    if haskey(iinfo, "path")
                        iinfo["path"] = project_rel_path(mainctx, iinfo["path"])
                    end
                end
            end
        end

        rewrite_manifests(localctx.env.manifest)

        # Add target deps to deps (https://github.com/JuliaLang/Pkg.jl/issues/427)
        if !isempty(pkgs)
            target_deps = deepcopy(pkgs)
            add_or_develop(localctx, pkgs)
            need_to_resolve = false # add resolves
            info = manifest_info(localctx.env, pkg.uuid)
            !haskey(info, "deps") && (info["deps"] = Dict{String, Any}())
            deps = info["deps"]
            for deppkg in target_deps
                deps[deppkg.name] = string(deppkg.uuid)
            end
        end

        # Might have added stdlibs in `add` above
        rewrite_manifests(localctx.env.manifest)

        local new
        will_resolve = might_need_to_resolve && need_to_resolve
        if will_resolve
            resolve_versions!(localctx, pkgs)
            new = apply_versions(localctx, pkgs)
        else
            prune_manifest(localctx.env)
        end
        write_env(localctx, display_diff = false)
        will_resolve && build_versions(localctx, new)

        sep = Sys.iswindows() ? ';' : ':'
        withenv("JULIA_LOAD_PATH" => "@$sep$tmpdir", "JULIA_PROJECT"=>nothing) do
            f(localctx)
        end
    end
end

function collect_target_deps!(ctx::Context, pkgs::Vector{PackageSpec}, pkg::PackageSpec, target::String)
    # Find the path to the package
    if pkg.uuid in keys(ctx.stdlibs)
        path = Types.stdlib_path(pkg.name)
    elseif Types.is_project_uuid(ctx.env, pkg.uuid)
        path = dirname(ctx.env.project_file)
    else
        info = manifest_info(ctx.env, pkg.uuid)
        path = haskey(info, "path") ? project_rel_path(ctx, info["path"]) : find_installed(pkg.name, pkg.uuid, SHA1(info["git-tree-sha1"]))
    end

    project_path = nothing
    for project_name in Base.project_names
        project_path_cand = joinpath(path, project_name)
        if isfile(project_path_cand)
            project_path = project_path_cand
            break
        end
    end
    project = nothing
    if project_path !== nothing
        project = read_package(project_path)
    end

    # Pkg2 compatibiity with test/REQUIRE
    has_project_test_target = false
    if project !== nothing
        if haskey(project, "targets")
            has_project_test_target = true
        end
    end
    if target == "test" && !has_project_test_target
        pkg2_test_target_compatibility!(ctx, path, pkgs)
        return
    end

    # Collect target deps from Project
    if project !== nothing
        targets = project["targets"]
        haskey(targets, target) || return
        for pkg in targets[target]
            uuid = UUID(project["extras"][pkg])
            push!(pkgs, PackageSpec(pkg, uuid))
        end
    end
    return nothing
end

# Pkg2 test/REQUIRE compatibility
function pkg2_test_target_compatibility!(ctx, path, pkgs)
    test_reqfile = joinpath(path, "test", "REQUIRE")
    if isfile(test_reqfile)
        for r in Pkg2.Reqs.read(test_reqfile)
            r isa Pkg2.Reqs.Requirement || continue
            pkg_name, vspec = r.package, VersionSpec(VersionRange[r.versions.intervals...])
            pkg_name == "julia" && continue
            push!(pkgs, PackageSpec(pkg_name, vspec))
        end
        registry_resolve!(ctx.env, pkgs)
        project_deps_resolve!(ctx.env, pkgs)
        ensure_resolved(ctx.env, pkgs; registry=true)
    end
    return nothing
end

function any_package_not_installed(ctx)
    for (pkg, infos) in ctx.env.manifest
        for info in infos
            if Base.locate_package(Base.PkgId(UUID(info["uuid"]), pkg)) === nothing
                return true
            end
        end
    end
    return false
end

#########
# Build #
#########
function dependency_order_uuids(ctx::Context, uuids::Vector{UUID})::Dict{UUID,Int}
    order = Dict{UUID,Int}()
    seen = UUID[]
    k = 0
    function visit(uuid::UUID)
        uuid in keys(ctx.stdlibs) && return
        uuid in seen &&
            return @warn("Dependency graph not a DAG, linearizing anyway")
        haskey(order, uuid) && return
        push!(seen, uuid)
        if Types.is_project_uuid(ctx.env, uuid)
            deps = values(ctx.env.project["deps"])
        else
            info = manifest_info(ctx.env, uuid)
            deps = values(get(info, "deps", Dict()))
        end
        foreach(visit, deps)
        pop!(seen)
        order[uuid] = k += 1
    end
    visit(uuid::String) = visit(UUID(uuid))
    foreach(visit, uuids)
    return order
end

function build_versions(ctx::Context, uuids::Vector{UUID}; might_need_to_resolve=false)
    # collect builds for UUIDs with `deps/build.jl` files
    ctx.preview && (printpkgstyle(ctx, :Building, "skipping building in preview mode"); return)
    builds = Tuple{UUID,String,Union{String,SHA1},String, VersionNumber}[]
    for uuid in uuids
        uuid in keys(ctx.stdlibs) && continue
        if Types.is_project_uuid(ctx.env, uuid)
            path = dirname(ctx.env.project_file)
            hash_or_path = path
            name = ctx.env.pkg.name
            version = ctx.env.pkg.version
        else
            info = manifest_info(ctx.env, uuid)
            name = info["name"]
            if haskey(info, "git-tree-sha1")
                hash_or_path = SHA1(info["git-tree-sha1"])
                path = find_installed(name, uuid, hash_or_path)
            elseif haskey(info, "path")
                path = project_rel_path(ctx, info["path"])
                hash_or_path = path
            else
                pkgerror("Could not find either `git-tree-sha1` or `path` for package $(pkg.name)")
            end
            version = v"0.0"
        end
        ispath(path) || error("Build path for $name does not exist: $path")
        build_file = joinpath(path, "deps", "build.jl")
        ispath(build_file) && push!(builds, (uuid, name, hash_or_path, build_file, version))
    end
    # toposort builds by dependencies
    order = dependency_order_uuids(ctx, map(first, builds))
    sort!(builds, by = build -> order[first(build)])
    max_name = isempty(builds) ? 0 : maximum(textwidth.([build[2] for build in builds]))
    # build each package versions in a child process
    for (uuid, name, hash_or_path, build_file, version) in builds
        log_file = splitext(build_file)[1] * ".log"
        printpkgstyle(ctx, :Building,
            rpad(name * " ", max_name + 1, "─") * "→ " * Types.pathrepr(log_file))
        code = """
            $(Base.load_path_setup_code(false))
            cd($(repr(dirname(build_file))))
            include($(repr(build_file)))
            """
        cmd = ```
            $(Base.julia_cmd()) -O0 --color=no --history-file=no
            --startup-file=$(Base.JLOptions().startupfile == 1 ? "yes" : "no")
            --compiled-modules=$(Bool(Base.JLOptions().use_compiled_modules) ? "yes" : "no")
            --eval $code
            ```
        run_build = () -> begin
            ok = open(log_file, "w") do log
                success(pipeline(cmd, stdout=log, stderr=log))
            end
            if !ok
                n_lines = isinteractive() ? 100 : 5000
                # TODO: Extract last n  lines more efficiently
                log_lines = readlines(log_file)
                log_show = join(log_lines[max(1, length(log_lines) - n_lines):end], '\n')
                full_log_at, last_lines =
                if length(log_lines) > n_lines
                    "\n\nFull log at $log_file",
                    ", showing the last $n_lines of log"
                else
                    "", ""
                end
                @error "Error building `$name`$last_lines: \n$log_show$full_log_at"
            end
        end
        with_dependencies_loadable_at_toplevel(ctx, PackageSpec(name, uuid, version);
                                               might_need_to_resolve=might_need_to_resolve) do localctx
            run_build()
        end
    end
    return
end

##############
# Operations #
##############
function rm(ctx::Context, pkgs::Vector{PackageSpec})
    drop = UUID[]
    # find manifest-mode drops
    for pkg in pkgs
        pkg.mode == PKGMODE_MANIFEST || continue
        info = manifest_info(ctx.env, pkg.uuid)
        if info != nothing
            pkg.uuid in drop || push!(drop, pkg.uuid)
        else
            str = has_name(pkg) ? pkg.name : string(pkg.uuid)
            @warn("`$str` not in manifest, ignoring")
        end
    end
    # drop reverse dependencies
    while !isempty(drop)
        clean = true
        for (name, infos) in ctx.env.manifest, info in infos
            haskey(info, "uuid") && haskey(info, "deps") || continue
            deps = map(UUID, values(info["deps"]))
            isempty(drop ∩ deps) && continue
            uuid = UUID(info["uuid"])
            uuid ∉ drop || continue
            push!(drop, uuid)
            clean = false
        end
        clean && break
    end
    # find project-mode drops
    for pkg in pkgs
        pkg.mode == PKGMODE_PROJECT || continue
        found = false
        for (name::String, uuidstr::String) in ctx.env.project["deps"]
            uuid = UUID(uuidstr)
            has_name(pkg) && pkg.name == name ||
            has_uuid(pkg) && pkg.uuid == uuid || continue
            !has_name(pkg) || pkg.name == name ||
                error("project file name mismatch for `$uuid`: $(pkg.name) ≠ $name")
            !has_uuid(pkg) || pkg.uuid == uuid ||
                error("project file UUID mismatch for `$name`: $(pkg.uuid) ≠ $uuid")
            uuid in drop || push!(drop, uuid)
            found = true
            break
        end
        found && continue
        str = has_name(pkg) ? pkg.name : string(pkg.uuid)
        @warn("`$str` not in project, ignoring")
    end
    # delete drops from project
    n = length(ctx.env.project["deps"])
    filter!(ctx.env.project["deps"]) do (_, uuid)
        UUID(uuid) ∉ drop
    end
    if length(ctx.env.project["deps"]) == n
        @info "No changes"
        return
    end
    # only keep reachable manifest entires
    prune_manifest(ctx.env)
    # update project & manifest
    write_env(ctx)
end

function add_or_develop(ctx::Context, pkgs::Vector{PackageSpec}; new_git = UUID[])
    # copy added name/UUIDs into project
    for pkg in pkgs
        ctx.env.project["deps"][pkg.name] = string(pkg.uuid)
    end
    # if a package is in the project file and
    # the manifest version in the specified version set
    # then leave the package as is at the installed version
    for (name::String, uuidstr::String) in ctx.env.project["deps"]
        uuid = UUID(uuidstr)
        info = manifest_info(ctx.env, uuid)
        info != nothing && haskey(info, "version") || continue
        version = VersionNumber(info["version"])
        for pkg in pkgs
            pkg.uuid == uuid && version ∈ pkg.version || continue
            pkg.version = version
        end
    end
    # resolve & apply package versions
    resolve_versions!(ctx, pkgs)
    new_apply = apply_versions(ctx, pkgs)
    write_env(ctx) # write env before building
    build_versions(ctx, union(new_apply, new_git))
end

function up(ctx::Context, pkgs::Vector{PackageSpec})
    # resolve upgrade levels to version specs
    new_git = UUID[]
    Base.shred!(LibGit2.CachedCredentials()) do creds
        for pkg in pkgs
            if pkg.uuid in keys(ctx.stdlibs)
                pkg.version = VersionSpec()
                continue
            end
            pkg.version isa UpgradeLevel || continue
            level = pkg.version
            info = manifest_info(ctx.env, pkg.uuid)
            if info !== nothing && haskey(info, "repo-url")
                pkg.repo = Types.GitRepo(info["repo-url"], info["repo-rev"])
                new = handle_repos_add!(ctx, [pkg]; credentials=creds,
                                        upgrade_or_add = (level == UPLEVEL_MAJOR))
                append!(new_git, new)
            else
                if info !== nothing
                    pkg.uuid in keys(ctx.stdlibs) && continue
                    ver = VersionNumber(info["version"])
                    if level == UPLEVEL_FIXED
                        pkg.version = VersionNumber(info["version"])
                    else
                        r = level == UPLEVEL_PATCH ? VersionRange(ver.major, ver.minor) :
                            level == UPLEVEL_MINOR ? VersionRange(ver.major) :
                            level == UPLEVEL_MAJOR ? VersionRange() :
                                error("unexpected upgrade level: $level")
                        pkg.version = VersionSpec(r)
                    end
                else
                    pkg.version = VersionSpec()
                end
            end
        end
    end
    # resolve & apply package versions
    resolve_versions!(ctx, pkgs)
    new_apply = apply_versions(ctx, pkgs)
    write_env(ctx) # write env before building
    build_versions(ctx, union(new_apply, new_git))
end

function pin(ctx::Context, pkgs::Vector{PackageSpec})
    for pkg in pkgs
        info = manifest_info(ctx.env, pkg.uuid)
        if pkg.version == VersionSpec()
            pkg.version = VersionNumber(info["version"])
        end
        pkg.special_action = PKGSPEC_PINNED
    end
    # resolve & apply package versions
    resolve_versions!(ctx, pkgs)
    new = apply_versions(ctx, pkgs)
    write_env(ctx) # write env before building
    build_versions(ctx, new)
end

function free(ctx::Context, pkgs::Vector{PackageSpec})
    need_to_resolve = false
    for pkg in pkgs
        pkg.special_action = PKGSPEC_FREED
        info = manifest_info(ctx.env, pkg.uuid)
        if haskey(info, "path") || haskey(info, "repo-url")
            need_to_resolve = true
        else
            pkg.version = VersionNumber(info["version"])
        end
    end
    need_to_resolve && resolve_versions!(ctx, pkgs)
    new = apply_versions(ctx, pkgs)
    write_env(ctx) # write env before building
    need_to_resolve && build_versions(ctx, new)
end

function test(ctx::Context, pkgs::Vector{PackageSpec}; coverage=false)
    # See if we can find the test files for all packages
    missing_runtests = String[]
    testfiles        = String[]
    version_paths    = String[]
    for pkg in pkgs
        pkg.special_action = PKGSPEC_TESTED
        if Types.is_project_uuid(ctx.env, pkg.uuid)
            pkg.version = ctx.env.pkg.version
            version_path = dirname(ctx.env.project_file)
        else
            info = manifest_info(ctx.env, pkg.uuid)
            if haskey(info, "git-tree-sha1")
                version_path = find_installed(pkg.name, pkg.uuid, SHA1(info["git-tree-sha1"]))
            elseif haskey(info, "path")
                version_path =  project_rel_path(ctx, info["path"])
            elseif pkg.uuid in keys(ctx.stdlibs)
                version_path = Types.stdlib_path(pkg.name)
            else
                pkgerror("Could not find either `git-tree-sha1` or `path` for package $(pkg.name)")
            end
        end
        testfile = joinpath(version_path, "test", "runtests.jl")
        if !isfile(testfile)
            push!(missing_runtests, pkg.name)
        end
        push!(version_paths, version_path)
        push!(testfiles, testfile)
    end
    if !isempty(missing_runtests)
        pkgerror(length(missing_runtests) == 1 ? "Package " : "Packages ",
                join(missing_runtests, ", "),
                " did not provide a `test/runtests.jl` file")
    end

    pkgs_errored = []
    for (pkg, testfile, version_path) in zip(pkgs, testfiles, version_paths)
        printpkgstyle(ctx, :Testing, pkg.name)
        if ctx.preview
            @info("In preview mode, skipping tests for $(pkg.name)")
            continue
        end
        code = """
            $(Base.load_path_setup_code(false))
            cd($(repr(dirname(testfile))))
            include($(repr(testfile)))
            """
        cmd = ```
            $(Base.julia_cmd())
            --code-coverage=$(coverage ? "user" : "none")
            --color=$(Base.have_color ? "yes" : "no")
            --compiled-modules=$(Bool(Base.JLOptions().use_compiled_modules) ? "yes" : "no")
            --check-bounds=yes
            --startup-file=$(Base.JLOptions().startupfile == 1 ? "yes" : "no")
            --track-allocation=$(("none", "user", "all")[Base.JLOptions().malloc_log + 1])
            --eval $code
        ```
        run_test = () -> begin
            try
                run(cmd)
                printpkgstyle(ctx, :Testing, pkg.name * " tests passed ")
            catch err
                push!(pkgs_errored, pkg.name)
            end
        end
        with_dependencies_loadable_at_toplevel(ctx, pkg; might_need_to_resolve=true) do localctx
            if !Types.is_project_uuid(ctx.env, pkg.uuid)
                Display.status(localctx, PKGMODE_MANIFEST)
            end

            run_test()
        end
    end

    if !isempty(pkgs_errored)
        pkgerror(length(pkgs_errored) == 1 ? "Package " : "Packages ",
                 join(pkgs_errored, ", "),
                 " errored during testing")
    end
end

end # module
