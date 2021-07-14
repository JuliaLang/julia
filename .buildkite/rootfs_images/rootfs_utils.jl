#!/usr/bin/env julia

# This is an example invocation of `debootstrap` to generate a Debian/Ubuntu-based rootfs
using Scratch, Pkg, Pkg.Artifacts, ghr_jll, SHA, Dates

# Utility functions
getuid() = ccall(:getuid, Cint, ())
getgid() = ccall(:getgid, Cint, ())
chroot(rootfs, cmds...; uid=getuid(), gid=getgid()) = run(`sudo chroot --userspec=$(uid):$(gid) $(rootfs) $(cmds)`)

# Sometimes rootfs images have absolute symlinks within them; this
# is very bad for us as it breaks our ability to look at a rootfs
# without `chroot`'ing into it; so we fix up all the links to be
# relative here.
function force_relative(link, rootfs)
    target = readlink(link)
    if !isabspath(target)
        return
    end
    target = joinpath(rootfs, target[2:end])
    rm(link; force=true)
    symlink(relpath(target, dirname(link)), link)
end
function force_relative(rootfs)
    for (root, dirs, files) in walkdir(rootfs)
        for f in files
            f = joinpath(root, f)
            if islink(f)
                force_relative(f, rootfs)
            end
        end
        for d in dirs
            d = joinpath(root, d)
            if islink(d)
                force_relative(d, rootfs)
            end
        end
    end
end

function cleanup_rootfs(rootfs; rootfs_info=nothing)
    # Remove special `dev` files
    @info("Cleaning up `/dev`")
    for f in readdir(joinpath(rootfs, "dev"); join=true)
        # Keep the symlinks around (such as `/dev/fd`), as they're useful
        if !islink(f)
            run(`sudo rm -rf "$(f)"`)
        end
    end

    # take ownership of the entire rootfs
    @info("Chown'ing rootfs")
    run(`sudo chown $(getuid()):$(getgid()) -R "$(rootfs)"`)

    # Write out a reasonable default resolv.conf
    open(joinpath(rootfs, "etc", "resolv.conf"), write=true) do io
        write(io, """
        nameserver 1.1.1.1
        nameserver 8.8.8.8
        """)
    end

    # If we've been given a `rootfs_info` parameter, write it out as /etc/rootfs-info
    if rootfs_info !== nothing
        open(io -> write(io, rootfs_info),
             joinpath(rootfs, "etc", "rootfs-info"),
             write=true,
        )
    end
    
    @info("Forcing all symlinks to be relative")
    force_relative(rootfs)
end

function create_rootfs(f::Function, name::String; force::Bool=false)
    tarball_path = joinpath(@get_scratch!("rootfs-images"), "$(name).tar.gz")
    if !force && isfile(tarball_path)
        @error("Refusing to overwrite tarball without `force` set", tarball_path)
        error()
    end

    artifact_hash = create_artifact(f)

    # Archive it into a `.tar.gz` file
    @info("Archiving", tarball_path, artifact_hash)
    archive_artifact(artifact_hash, tarball_path)
    return tarball_path
end

function debootstrap(f::Function, name::String; release::String="buster", variant::String="minbase",
                     packages::Vector{String}=String[], force::Bool=false)
    if Sys.which("debootstrap") === nothing
        error("Must install `debootstrap`!")
    end

    return create_rootfs(name; force) do rootfs
        packages_string = join(push!(packages, "locales"), ",")
        @info("Running debootstrap", release, variant, packages)
        run(`sudo debootstrap --variant=$(variant) --include=$(packages_string) $(release) "$(rootfs)"`)

        # Call user callback, if requested
        f(rootfs)

        # Remove special `dev` files, take ownership, force symlinks to be relative, etc...
        rootfs_info="""
                    rootfs_type=debootstrap
                    release=$(release)
                    variant=$(variant)
                    packages=$(packages_string)
                    build_date=$(Dates.now())
                    """
        cleanup_rootfs(rootfs; rootfs_info)

        # Remove `_apt` user so that `apt` doesn't try to `setgroups()`
        @info("Removing `_apt` user")
        open(joinpath(rootfs, "etc", "passwd"), write=true, read=true) do io
            filtered_lines = filter(l -> !startswith(l, "_apt:"), readlines(io))
            truncate(io, 0)
            seek(io, 0)
            for l in filtered_lines
                println(io, l)
            end
        end

        # Set up the one true locale
        @info("Setting up UTF-8 locale")
        open(joinpath(rootfs, "etc", "locale.gen"), "a") do io
            println(io, "en_US.UTF-8 UTF-8")
        end
        chroot(rootfs, "locale-gen")
    end
end
# If no user callback is provided, default to doing nothing
debootstrap(name::String; kwargs...) = debootstrap(p -> nothing, name; kwargs...)



# Helper structure for installing alpine packages that may or may not be part of an older Alpine release
struct AlpinePackage
    name::String
    repo::Union{Nothing,String}

    AlpinePackage(name, repo=nothing) = new(name, repo)
end
function repository_arg(repo)
    if startswith(repo, "https://")
        return "--repository=$(repo)"
    end
    return "--repository=http://dl-cdn.alpinelinux.org/alpine/$(repo)/main"
end

function alpine_bootstrap(f::Function, name::String; release::VersionNumber=v"3.13.5", variant="minirootfs",
                          packages::Vector{AlpinePackage}=AlpinePackage[], force::Bool=false)
    return create_rootfs(name; force) do rootfs
        rootfs_url = "https://github.com/alpinelinux/docker-alpine/raw/v$(release.major).$(release.minor)/x86_64/alpine-$(variant)-$(release)-x86_64.tar.gz"
        @info("Downloading Alpine rootfs", url=rootfs_url)
        rm(rootfs)
        Pkg.Artifacts.download_verify_unpack(rootfs_url, nothing, rootfs; verbose=true)

        # Call user callback, if requested
        f(rootfs)

        # Remove special `dev` files, take ownership, force symlinks to be relative, etc...
        rootfs_info = """
                    rootfs_type=alpine
                    release=$(release)
                    variant=$(variant)
                    packages=$(join([pkg.name for pkg in packages], ","))
                    build_date=$(Dates.now())
                    """
        cleanup_rootfs(rootfs; rootfs_info)

        # Generate one `apk` invocation per repository
        repos = unique([pkg.repo for pkg in packages])
        for repo in repos
            apk_args = ["/sbin/apk", "add", "--no-chown"]
            if repo !== nothing
                push!(apk_args, repository_arg(repo))
            end
            for pkg in filter(pkg -> pkg.repo == repo, packages)
                push!(apk_args, pkg.name)
            end
            chroot(rootfs, apk_args...)
        end
    end
end
# If no user callback is provided, default to doing nothing
alpine_bootstrap(name::String; kwargs...) = alpine_bootstrap(p -> nothing, name; kwargs...)

function upload_rootfs_image(tarball_path::String;
                             github_repo::String="JuliaCI/rootfs-images",
                             tag_name::String,
                             force_overwrite::Bool)
    # Upload it to `github_repo`
    tarball_url = "https://github.com/$(github_repo)/releases/download/$(tag_name)/$(basename(tarball_path))"
    @info("Uploading to $(github_repo)@$(tag_name)", tarball_url)
    replace_flag = force_overwrite ? "-replace" : ""
    run(`$(ghr_jll.ghr()) -u $(dirname(github_repo)) -r $(basename(github_repo)) $(replace_flag) $(tag_name) $(tarball_path)`)
    return tarball_url
end

# process command-line arguments

function get_arguments(args::AbstractVector, script_file::AbstractString)
    usage = "Usage: $(basename(script_file)) <tag_name> [--force-overwrite]"
    length(args) < 1 && throw(ArgumentError(usage))
    length(args) > 2 && throw(ArgumentError(usage))
    tag_name        = get_tag_name(args; usage)
    force_overwrite = get_force_overwrite(args; usage)
    return (; tag_name, force_overwrite)
end

function get_tag_name(args::AbstractVector; usage::AbstractString)
    tag_name = convert(String, strip(args[1]))::String
    isempty(tag_name)          && throw(ArgumentError(usage))
    startswith(tag_name, "--") && throw(ArgumentError(usage))
    return tag_name
end

function get_force_overwrite(args::AbstractVector; usage::AbstractString)
    force_overwrite_string = strip(get(args, 2, ""))
    force_overwrite_string == ""                  && return false
    force_overwrite_string == "--force-overwrite" && return true
    throw(ArgumentError(usage))
end
