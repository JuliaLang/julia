#!/usr/bin/env julia

# This is an example invocation of `debootstrap` to generate a Debian/Ubuntu-based rootfs
using Scratch, Pkg, Pkg.Artifacts, ghr_jll, SHA, Dates

# Utility functions
getuid() = ccall(:getuid, Cint, ())
getgid() = ccall(:getgid, Cint, ())

function debootstrap(name::String; release::String="buster", variant::String="minbase",
                     packages::Vector{String}=String[], force::Bool=false)
    if Sys.which("debootstrap") === nothing
        error("Must install `debootstrap`!")
    end

    tarball_path = joinpath(@get_scratch!("rootfs-images"), "$(name).tar.gz")
    if !force && isfile(tarball_path)
        @error("Refusing to overwrite tarball without `force` set", tarball_path)
        error()
    end

    artifact_hash = create_artifact() do rootfs
        packages_string = join(push!(packages, "locales"), ",")
        @info("Running debootstrap", release, variant, packages)
        run(`sudo debootstrap --variant=$(variant) --include=$(packages_string) $(release) "$(rootfs)"`)

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

        # Write out rootfs-info to contain a minimally-identifying string
        open(joinpath(rootfs, "etc", "rootfs-info"), write=true) do io
            write(io, """
            rootfs_type=debootstrap
            release=$(release)
            variant=$(variant)
            packages=$(packages_string)
            build_date=$(Dates.now())
            """)
        end

        # Write out a reasonable default resolv.conf
        open(joinpath(rootfs, "etc", "resolv.conf"), write=true) do io
            write(io, """
            nameserver 1.1.1.1
            nameserver 8.8.8.8
            """)
        end

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
        run(`sudo chroot --userspec=$(getuid()):$(getgid()) $(rootfs) locale-gen`)
    end

    # Archive it into a `.tar.gz` file
    @info("Archiving", tarball_path, artifact_hash)
    archive_artifact(artifact_hash, tarball_path)

    return tarball_path
end

function upload_rootfs_image(tarball_path::String;
                             github_repo::String="JuliaCI/rootfs-images",
                             tag_name::String)
    # Upload it to `github_repo`
    tarball_url = "https://github.com/$(github_repo)/releases/download/$(tag_name)/$(basename(tarball_path))"
    @info("Uploading to $(github_repo)@$(tag_name)", tarball_url)
    run(`$(ghr_jll.ghr()) -u $(dirname(github_repo)) -r $(basename(github_repo)) -replace $(tag_name) $(tarball_path)`)
    return tarball_url
end
