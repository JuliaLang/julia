module Operations

using UUIDs
using Random: randstring
import LibGit2

import REPL
using REPL.TerminalMenus
using Pkg3.Types
using Pkg3.GraphType
using Pkg3.Resolve
import Pkg3.Pkg2
import Pkg3.BinaryProvider

import Pkg3: depots, devdir
import Pkg3.Types: uuid_julia

function find_installed(name::String, uuid::UUID, sha1::SHA1)
    slug = Base.version_slug(uuid, sha1)
    for depot in depots()
        path = abspath(depot, "packages", name, slug)
        ispath(path) && return path
    end
    return abspath(depots()[1], "packages", name, slug)
end

function load_versions(path::String)
    toml = parse_toml(path, "versions.toml")
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
            haskey(dict, key) && cmderror("$ver/$key is duplicated in $path")
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
        haskey(dict, key) && cmderror("$vr/$key is duplicated in $path")
        dict[key] = T(value)
    end
    return data
end

#######################################
# Dependency gathering and resolution #
#######################################

# This also sets the .path field for fixed packages in `pkgs`
function collect_fixed!(ctx::Context, pkgs::Vector{PackageSpec}, uuid_to_name::Dict{UUID, String})
    fix_deps = PackageSpec[]
    fixed_pkgs = PackageSpec[]
    fix_deps_map = Dict{UUID,Vector{PackageSpec}}()
    uuid_to_pkg = Dict{UUID,PackageSpec}()
    for pkg in pkgs
        info = manifest_info(ctx.env, pkg.uuid)
        if pkg.special_action == PKGSPEC_FREED
            continue
        elseif pkg.special_action == PKGSPEC_CHECKED_OUT
            @assert pkg.path != nothing
        elseif info !== nothing && get(info, "path", false) != false
            pkg.path = info["path"]
        else
            continue
        end

        # Checked out package has by definition a version higher than all registered.
        pkgversions = Set{VersionNumber}()
        for path in registered_paths(ctx.env, pkg.uuid)
            pathvers = keys(load_versions(path))
            union!(pkgversions, pathvers)
        end
        max_version = maximum(pkgversions)
        pkg.version = VersionNumber(max_version.major, max_version.minor, max_version.patch, max_version.prerelease, ("",))

        uuid_to_pkg[pkg.uuid] = pkg
        uuid_to_name[pkg.uuid] = pkg.name

        # Backwards compatability with Pkg2 REQUIRE format
        reqfile = joinpath(pkg.path, "REQUIRE")
        fix_deps_map[pkg.uuid] = valtype(fix_deps_map)()
        !isfile(reqfile) && continue
        for r in filter!(r->r isa Pkg2.Reqs.Requirement, Pkg2.Reqs.read(reqfile))
            pkg_name, vspec = r.package, VersionSpec(VersionRange[r.versions.intervals...])
            if pkg_name == "julia"
                if !(VERSION in vspec)
                    error("julia version requirement for package $pkg not satisfied")
                end
            else
                deppkg = PackageSpec(pkg_name, vspec)
                push!(fix_deps_map[pkg.uuid], deppkg)
                push!(fix_deps, deppkg)
            end
        end
    end

    # Look up the UUIDS for all the fixed dependencies in the registry in one shot
    registry_resolve!(ctx.env, fix_deps)
    ensure_resolved(ctx.env, fix_deps)
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
            all_versions_u = get_or_make!(all_versions, uuid)
            all_deps_u     = get_or_make!(all_deps,     uuid)
            all_compat_u   = get_or_make!(all_compat,   uuid)
            # make sure all versions of all packages know about julia uuid
            if uuid ≠ uuid_julia
                deps_u_allvers = get_or_make!(all_deps_u, VersionRange())
                deps_u_allvers["julia"] = uuid_julia
            end

            if uuid in ctx.stdlib_uuids
                push!(all_versions_u, VERSION)
                continue
            end

            for path in registered_paths(ctx.env, uuid)
                version_info = load_versions(path)
                versions = sort!(collect(keys(version_info)))
                deps_data = load_package_data_raw(UUID, joinpath(path, "dependencies.toml"))
                compatibility_data = load_package_data_raw(VersionSpec, joinpath(path, "compatibility.toml"))

                union!(all_versions_u, versions)

                for (vr, dd) in deps_data
                    all_deps_u_vr = get_or_make!(all_deps_u, vr)
                    for (name,other_uuid) in dd
                        # check conflicts??
                        all_deps_u_vr[name] = other_uuid
                        other_uuid in uuids || push!(uuids, other_uuid)
                    end
                end
                for (vr, cd) in compatibility_data
                    all_compat_u_vr = get_or_make!(all_compat_u, vr)
                    for (name,vs) in cd
                        # check conflicts??
                        all_compat_u_vr[name] = vs
                    end
                end
            end
        end
        find_registered!(ctx.env, uuids)
    end

    for uuid in uuids
        uuid == uuid_julia && continue
        uuid_to_name[uuid] = registered_name(ctx.env, uuid)
        info = manifest_info(ctx.env, uuid)
        info ≡ nothing && continue
        uuid_to_name[UUID(info["uuid"])] = info["name"]
    end

    return Graph(all_versions, all_deps, all_compat, uuid_to_name, reqs, fixed; verbose=ctx.graph_verbose)
end

# Resolve a set of versions given package version specs
function resolve_versions!(ctx::Context, pkgs::Vector{PackageSpec})::Dict{UUID,VersionNumber}
    @info("Resolving package versions")
    # anything not mentioned is fixed
    uuids = UUID[pkg.uuid for pkg in pkgs]
    uuid_to_name = Dict{UUID,String}(uuid_julia => "julia")
    for (name::String, uuidstr::String) in ctx.env.project["deps"]
        uuid = UUID(uuidstr)
        uuid_to_name[uuid] = name
        info = manifest_info(ctx.env, uuid)
        info == nothing && continue
        haskey(info, "version") || continue
        ver = VersionNumber(info["version"])
        uuid_idx = findfirst(equalto(uuid), uuids)
        if uuid_idx != nothing
            if get(info, "pinned", false)
                # This is a pinned package, fix its version
                pkg = pkgs[uuid_idx]
                @info ("Package $name [$(string(uuid)[1:8])] is pinned, keeping it at current version: $ver")
                pkg.version = ver
            end
        else
            push!(pkgs, PackageSpec(name, uuid, ver))
        end
    end
    # construct data structures for resolver and call it
    reqs = Requires(pkg.uuid => VersionSpec(pkg.version) for pkg in pkgs if pkg.uuid ≠ uuid_julia)
    fixed = collect_fixed!(ctx, pkgs, uuid_to_name)
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
function version_data(ctx::Context, pkgs::Vector{PackageSpec})
    names = Dict{UUID,String}()
    hashes = Dict{UUID,SHA1}()
    upstreams = Dict{UUID,Vector{String}}()
    for pkg in pkgs
        pkg.uuid in ctx.stdlib_uuids && continue
        pkg.path == nothing || continue
        uuid = pkg.uuid
        ver = pkg.version::VersionNumber
        upstreams[uuid] = String[]
        for path in registered_paths(ctx.env, uuid)
            info = parse_toml(path, "package.toml")
            if haskey(names, uuid)
                names[uuid] == info["name"] ||
                    cmderror("$uuid: name mismatch between registries: ",
                             "$(names[uuid]) vs. $(info["name"])")
            else
                names[uuid] = info["name"]
            end
            repo = info["repo"]
            repo in upstreams[uuid] || push!(upstreams[uuid], repo)
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
    foreach(sort!, values(upstreams))
    return names, hashes, upstreams
end

########################
# Package installation #
########################
function get_archive_url_for_version(url::String, version)
    if (m = match(r"https://github.com/(.*?)/(.*?).git", url)) != nothing
        return "https://github.com/$(m.captures[1])/$(m.captures[2])/archive/v$(version).tar.gz"
    end
    return nothing
end

# Returns if archive successfully installed
function install_archive(
    urls::Vector{String},
    version::Union{VersionNumber,Nothing},
    version_path::String
)::Bool
    for url in urls
        archive_url = get_archive_url_for_version(url, version)
        if archive_url != nothing
            path = tempname() * randstring(6) * ".tar.gz"
            url_success = true
            try
                cmd = BinaryProvider.gen_download_cmd(archive_url, path);
                run(cmd, (DevNull, DevNull, DevNull))
            catch e
                e isa InterruptException && rethrow(e)
                url_success = false
            end
            url_success || continue
            dir = joinpath(tempdir(), randstring(12))
            mkpath(dir)
            cmd = BinaryProvider.gen_unpack_cmd(path, dir);
            run(cmd, (DevNull, DevNull, DevNull))
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
    uuid::UUID,
    name::String,
    hash::SHA1,
    urls::Vector{String},
    version::Union{VersionNumber,Nothing},
    version_path::String
)::Nothing
    upstream_dir = joinpath(depots()[1], "upstream")
    ispath(upstream_dir) || mkpath(upstream_dir)
    repo_path = joinpath(upstream_dir, string(uuid))
    repo = ispath(repo_path) ? LibGit2.GitRepo(repo_path) : begin
        @info("Cloning [$uuid] $name from $(urls[1])")
        LibGit2.clone(urls[1], repo_path, isbare=true)
    end
    git_hash = LibGit2.GitHash(hash.bytes)
    for i = 2:length(urls)
        try LibGit2.GitObject(repo, git_hash)
            break # object was found, we can stop
        catch err
            err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
        end
        url = urls[i]
        LibGit2.fetch(repo, remoteurl=url, refspecs=refspecs)
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
    h = string(hash)[1:16]
    LibGit2.checkout_tree(repo, tree, options=opts)
    return
end

# install & update manifest
function apply_versions(ctx::Context, pkgs::Vector{PackageSpec})::Vector{UUID}
    BinaryProvider.probe_platform_engines!()
    names, hashes, urls = version_data(ctx, pkgs)
    new_versions = UUID[]

    pkgs_to_install = Tuple{PackageSpec, String}[]
    for pkg in pkgs
        pkg.uuid in ctx.stdlib_uuids && continue
        pkg.path == nothing || continue
        path = find_installed(pkg.name, pkg.uuid, hashes[pkg.uuid])
        if !ispath(path)
            push!(pkgs_to_install, (pkg, path))
            push!(new_versions, pkg.uuid)
        end
    end

    widths = [textwidth(names[pkg.uuid]) for (pkg, _) in pkgs_to_install if haskey(names, pkg.uuid)]
    max_name = length(widths) == 0 ? 0 : maximum(widths)

    ########################################
    # Install from archives asynchronously #
    ########################################
    jobs = Channel(ctx.num_concurrent_downloads);
    results = Channel(ctx.num_concurrent_downloads);
    @schedule begin
        for pkg in pkgs_to_install
            put!(jobs, pkg)
        end
    end

    for i in 1:ctx.num_concurrent_downloads
        @schedule begin
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
                    success = install_archive(urls[pkg.uuid], pkg.version::VersionNumber, path)
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
        exc_or_success isa Exception && cmderror("Error when installing packages:\n", sprint(Base.showerror, exc_or_success, bt_or_path))
        success, path = exc_or_success, bt_or_path
        if success
            vstr = pkg.version != nothing ? "v$(pkg.version)" : "[$h]"
            @info "Installed $(rpad(names[pkg.uuid] * " ", max_name + 2, "─")) $vstr"
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
            install_git(pkg.uuid, names[uuid], hashes[uuid], urls[uuid], pkg.version::VersionNumber, path)
        end
        vstr = pkg.version != nothing ? "v$(pkg.version)" : "[$h]"
        @info "Installed $(rpad(names[pkg.uuid] * " ", max_name + 2, "─")) $vstr"
    end

    ##########################################
    # Installation done, update the manifest #
    ##########################################
    for pkg in pkgs
        uuid = pkg.uuid
        uuid in ctx.stdlib_uuids && continue
        if pkg.path == nothing
            pkg.name = names[uuid]
            hash = hashes[uuid]
        else
            hash = nothing
        end
        update_manifest(ctx.env, pkg, hash)
    end

    prune_manifest(ctx.env)
    return new_versions
end

################################
# Manifest update and pruning #
################################
function update_manifest(env::EnvCache, pkg::PackageSpec, hash::Union{SHA1, Nothing})
    uuid, name, version, path, special_action = pkg.uuid, pkg.name, pkg.version, pkg.path, pkg.special_action
    hash == nothing && @assert path != nothing
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
    info["version"] = string(version)
    hash == nothing ? delete!(info, "git-tree-sha1") : (info["git-tree-sha1"] = string(hash))
    path == nothing ? delete!(info, "path")          : (info["path"]          = path)
    if special_action == PKGSPEC_FREED
        delete!(info, "pinned")
    elseif special_action == PKGSPEC_PINNED
        info["pinned"] = true
    end

    delete!(info, "deps")
    if path != nothing
        reqfile = joinpath(path, "REQUIRE")
        !isfile(reqfile) && return
        deps = Dict{String,String}()
        dep_pkgs = PackageSpec[]
        for r in filter!(r->r isa Pkg2.Reqs.Requirement, Pkg2.Reqs.read(reqfile))
            push!(dep_pkgs, PackageSpec(r.package))
        end
        registry_resolve!(env, dep_pkgs)
        ensure_resolved(env, dep_pkgs)
        for dep_pkg in dep_pkgs
            dep_pkg.name == "julia" && continue
            deps[dep_pkg.name] = string(dep_pkg.uuid)
        end
        if !isempty(deps)
            info["deps"] = deps
        end
    else
        for path in registered_paths(env, uuid)
            data = load_package_data(UUID, joinpath(path, "dependencies.toml"), version)
            data == nothing && continue
            info["deps"] = Dict(string(k) => string(v) for (k,v) in data)
            break
        end
    end
    return info
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

#########
# Build #
#########
function dependency_order_uuids(ctx::Context, uuids::Vector{UUID})::Dict{UUID,Int}
    order = Dict{UUID,Int}()
    seen = UUID[]
    k = 0
    function visit(uuid::UUID)
        uuid in ctx.stdlib_uuids && return
        uuid in seen &&
            return @warn("Dependency graph not a DAG, linearizing anyway")
        haskey(order, uuid) && return
        push!(seen, uuid)
        info = manifest_info(ctx.env, uuid)
        haskey(info, "deps") &&
            foreach(visit, values(info["deps"]))
        pop!(seen)
        order[uuid] = k += 1
    end
    visit(uuid::String) = visit(UUID(uuid))
    foreach(visit, uuids)
    return order
end

function build_versions(ctx::Context, uuids::Vector{UUID})
    # collect builds for UUIDs with `deps/build.jl` files
    ctx.preview && (@info "Skipping building in preview mode"; return)
    builds = Tuple{UUID,String,Union{String,SHA1},String}[]
    for uuid in uuids
        uuid in ctx.stdlib_uuids && continue
        info = manifest_info(ctx.env, uuid)
        name = info["name"]
        if haskey(info, "git-tree-sha1")
            hash_or_path = SHA1(info["git-tree-sha1"])
            path = find_installed(name, uuid, hash_or_path)
        elseif haskey(info, "path")
            path = info["path"]
            hash_or_path = path
        else
            cmderror("Could not find either `git-tree-sha1` or `path` for package $(pkg.name)")
        end
        ispath(path) || error("Build path for $name does not exist: $path")
        build_file = joinpath(path, "deps", "build.jl")
        ispath(build_file) && push!(builds, (uuid, name, hash_or_path, build_file))
    end
    # toposort builds by dependencies
    order = dependency_order_uuids(ctx, map(first, builds))
    sort!(builds, by = build -> order[first(build)])
    # build each package verions in a child process
    for (uuid, name, hash_or_path, build_file) in builds
        log_file = splitext(build_file)[1] * ".log"
        if hash_or_path isa SHA1
            @info "Building $name [$(string(hash_or_path)[1:8])]..."
        else
            @info "Building $name [$(string(hash_or_path))]..."
        end
        @info " → $log_file"
        code = """
            empty!(Base.LOAD_PATH)
            append!(Base.LOAD_PATH, $(repr(Base.load_path())))
            empty!(Base.DEPOT_PATH)
            append!(Base.DEPOT_PATH, $(repr(map(abspath, DEPOT_PATH))))
            empty!(Base.LOAD_CACHE_PATH)
            append!(Base.LOAD_CACHE_PATH, $(repr(map(abspath, Base.LOAD_CACHE_PATH))))
            empty!(Base.DL_LOAD_PATH)
            append!(Base.DL_LOAD_PATH, $(repr(map(abspath, Base.DL_LOAD_PATH))))
            m = Base.require(Base.PkgId(Base.UUID($(repr(string(uuid)))), $(repr(name))))
            eval(m, :(module __build__ end))
            cd($(repr(dirname(build_file))))
            Base.include_relative(m.__build__, $(repr(build_file)))
            """
        cmd = ```
            $(Base.julia_cmd()) -O0 --color=no --history-file=no
            --startup-file=$(Base.JLOptions().startupfile != 2 ? "yes" : "no")
            --compiled-modules=$(Bool(Base.JLOptions().use_compiled_modules) ? "yes" : "no")
            --eval $code
            ```
        open(log_file, "w") do log
            success(pipeline(cmd, stdout=log, stderr=log))
        end ? Base.rm(log_file, force=true) :
        @error("Error building `$name`; see log file for further info")
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

function add(ctx::Context, pkgs::Vector{PackageSpec})
    # if julia is passed as a package the solver gets tricked;
    # this catches the error early on
    any(pkg->(pkg.uuid == uuid_julia), pkgs) &&
        cmderror("Trying to add julia as a package")
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
    new = apply_versions(ctx, pkgs)
    write_env(ctx) # write env before building
    build_versions(ctx, new)
end

function up(ctx::Context, pkgs::Vector{PackageSpec})
    # resolve upgrade levels to version specs
    for pkg in pkgs
        pkg.version isa UpgradeLevel || continue
        level = pkg.version
        info = manifest_info(ctx.env, pkg.uuid)
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
    end
    # resolve & apply package versions
    resolve_versions!(ctx, pkgs)
    new = apply_versions(ctx, pkgs)
    write_env(ctx) # write env before building
    build_versions(ctx, new)
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
        if haskey(info, "path")
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

function checkout(ctx::Context, pkgs_branches::Vector; path = devdir())
    pkgs = PackageSpec[]
    for (pkg, branch) in pkgs_branches
        push!(pkgs, pkg)
        ctx.env.project["deps"][pkg.name] = string(pkg.uuid)
        pkg.special_action = PKGSPEC_CHECKED_OUT
        @info "Checking out $(pkg.name) [$(string(pkg.uuid)[1:8])]"

        pkgpath = joinpath(path, pkg.name)
        pkg.path = pkgpath
        if ispath(pkgpath)
            if !isfile(joinpath(pkgpath, "src", pkg.name * ".jl"))
                cmderror("Path `$(pkgpath)` exists but it does not contain `src/$(pkg.name).jl")
            else
                @info "Path `$(pkgpath)` exists and looks like the correct package, using existing path instead of cloning"
            end
        else
            successfully_cloned = false
            for regpath in Types.registered_paths(ctx.env, pkg.uuid)
                repos = Types.registered_info(ctx.env, pkg.uuid, "repo")
                for (_, repo) in repos
                    @info "Cloning $(pkg.name) from $(repo) to $path"
                    try
                        LibGit2.clone(repo, pkgpath; branch = branch == nothing ? "" : branch)
                        successfully_cloned = true
                        break
                    catch err
                        err isa LibGit2.GitError || rethrow(err)
                        @error "Failed clone from $repo" exception = err
                    end
                end
            end
            if !successfully_cloned
                cmderror("Failed to checkout $(pkg.name) [$(string(pkg.uuid)[1:8])] to $(pkgpath)")
            end
        end
    end
    resolve_versions!(ctx, pkgs)
    new = apply_versions(ctx, pkgs)
    write_env(ctx) # write env before building
    build_versions(ctx, new)
end

function test(ctx::Context, pkgs::Vector{PackageSpec}; coverage=false)
    # See if we can find the test files for all packages
    missing_runtests = String[]
    testfiles        = String[]
    version_paths    = String[]
    for pkg in pkgs
        info = manifest_info(ctx.env, pkg.uuid)
        if haskey(info, "git-tree-sha1")
            version_path = find_installed(pkg.name, pkg.uuid, SHA1(info["git-tree-sha1"]))
        elseif haskey(info, "path")
            version_path = info["path"]
        else
            cmderror("Could not find either `git-tree-sha1` or `path` for package $(pkg.name)")
        end
        testfile = joinpath(version_path, "test", "runtests.jl")
        if !isfile(testfile)
            push!(missing_runtests, pkg.name)
        end
        push!(version_paths, version_path)
        push!(testfiles, testfile)
    end
    if !isempty(missing_runtests)
        cmderror(length(missing_runtests) == 1 ? "Package " : "Packages ",
                join(missing_runtests, ", "),
                " did not provide a `test/runtests.jl` file")
    end

    pkgs_errored = []
    for (pkg, testfile, version_path) in zip(pkgs, testfiles, version_paths)
        @info("Testing $(pkg.name) located at $version_path")
        if ctx.preview
            @info("In preview mode, skipping tests for $(pkg.name)")
            continue
        end
        # TODO, cd to test folder (need to be careful with getting the same EnvCache
        # as for this session in that case
        code = """
            empty!(Base.LOAD_PATH)
            append!(Base.LOAD_PATH, $(repr(Base.load_path())))
            empty!(Base.DEPOT_PATH)
            append!(Base.DEPOT_PATH, $(repr(map(abspath, DEPOT_PATH))))
            empty!(Base.LOAD_CACHE_PATH)
            append!(Base.LOAD_CACHE_PATH, $(repr(map(abspath, Base.LOAD_CACHE_PATH))))
            empty!(Base.DL_LOAD_PATH)
            append!(Base.DL_LOAD_PATH, $(repr(map(abspath, Base.DL_LOAD_PATH))))
            m = Base.require(Base.PkgId(Base.UUID($(repr(string(pkg.uuid)))), $(repr(pkg.name))))
            eval(m, :(module __test__ end))
            cd($(repr(dirname(testfile))))
            Base.include_relative(m.__test__, $(repr(testfile)))
            """
        cmd = ```
            $(Base.julia_cmd())
            --code-coverage=$(coverage ? "user" : "none")
            --color=$(Base.have_color ? "yes" : "no")
            --compiled-modules=$(Bool(Base.JLOptions().use_compiled_modules) ? "yes" : "no")
            --check-bounds=yes
            --startup-file=$(Base.JLOptions().startupfile != 2 ? "yes" : "no")
            --eval $code
        ```
        try
            run(cmd)
            @info("$(pkg.name) tests passed")
        catch err
            push!(pkgs_errored, pkg.name)
        end

    end

    if !isempty(pkgs_errored)
        cmderror(length(pkgs_errored) == 1 ? "Package " : "Packages ",
                 join(pkgs_errored, ", "),
                 " errored during testing")
    end
end

function init(ctx::Context)
    project_file = ctx.env.project_file
    isfile(project_file) &&
        cmderror("Environment already initialized at $project_file")
    mkpath(dirname(project_file))
    touch(project_file)
    @info "Initialized environment by creating $project_file"
end

end # module

