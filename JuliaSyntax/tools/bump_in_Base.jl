function find_checksum_files(checksum_dir)
    filter(readdir(checksum_dir, join=true)) do path
        occursin(r"^JuliaSyntax-", basename(path))
    end
end

function bump_in_Base(julia_dir, juliasyntax_dir, juliasyntax_branch_or_commit)
    julia_git_dir = joinpath(julia_dir, ".git")
    JuliaSyntax_git_dir = joinpath(juliasyntax_dir, ".git")
    if !isdir(julia_git_dir)
        @error "Julia .git directory not found" julia_git_dir
        return 1
    end
    if !isdir(JuliaSyntax_git_dir)
        @error "JuliaSyntax .git directory not found" JuliaSyntax_git_dir
        return 1
    end

    @info "Vendoring JuliaSyntax into Base" julia_dir juliasyntax_branch_or_commit

    remote_containing_branches = filter(b->occursin(r"^origin/(main|release-.*)$", b),
        strip.(split(
            read(`git --git-dir=$JuliaSyntax_git_dir branch -r --contains $juliasyntax_branch_or_commit`, String),
            '\n', keepempty=false)))
    if isempty(remote_containing_branches)
        @warn "No remote main or release branches contain the given commit. This is ok for testing, but is otherwise an error." juliasyntax_branch_or_commit
    else
        @info "Given commit is accessible on remote branch" remote_containing_branches
    end

    commit_sha = strip(String(read(`git --git-dir=$JuliaSyntax_git_dir show -s --pretty=tformat:%H $juliasyntax_branch_or_commit`)))

    cd(julia_dir) do
        status = read(`git status --porcelain --untracked-files=no`, String)
        if status != ""
            @error "Julia git directory contains uncommitted changes" status=Text(status)
            return 1
        end

        verfile_path = joinpath("deps", "JuliaSyntax.version")
        @info "Updating JuliaSyntax.version" verfile_path
        write(verfile_path, replace(read(verfile_path, String), r"JULIASYNTAX_SHA1.*"=>"JULIASYNTAX_SHA1 = "*commit_sha))
        run(`git add $verfile_path`)

        @info "Updating JuliaSyntax checksums"
        deps_dir = "deps"
        checksum_dir = joinpath(deps_dir, "checksums")
        old_checksum_paths = find_checksum_files(checksum_dir)
        if !isempty(old_checksum_paths)
            run(`git rm -rf $old_checksum_paths`)
        end
        run(`make -C $deps_dir`)
        run(`git add $(find_checksum_files(checksum_dir))`)

        # Force rebuild of Base to include the newly vendored JuliaSyntax next time Julia is built.
        # (TODO: fix the Makefile instead?)
        touch("base/Base.jl")

        @info "JuliaSyntax version updated. You can now test or commit the following changes"
        run(`git diff --cached`)
    end

    return 0
end

if !isinteractive()
    if length(ARGS) != 2
        println("Usage: bump_in_Base.jl \$julia_dir \$juliasyntax_branch_or_commit")
        exit(1)
    else
        juliasyntax_dir = dirname(@__DIR__)
        exit(bump_in_Base(ARGS[1], juliasyntax_dir, ARGS[2]))
    end
end
