#!/usr/bin/env julia

const repo_root = normpath(joinpath(@__DIR__, ".."))
const canonical_root = "doc/src/devdocs/agents/skills"
const mirror_roots = [".agents/skills", ".claude/skills"]
const is_gha = Base.get_bool_env("GITHUB_ACTIONS", false) === true

relpath_unix(path) = replace(relpath(path, repo_root), '\\' => '/')

git_ls_files(path) = readlines(`git -C $repo_root ls-files -- $path`)

tracked_files_under(path) = Set(git_ls_files(path))

function git_file_mode(path)
    lines = readlines(`git -C $repo_root ls-files -s -- $path`)
    isempty(lines) && return nothing
    return first(split(first(lines)))
end

function canonical_skill_names()
    return sort!(filter(readdir(joinpath(repo_root, canonical_root))) do name
        isfile(joinpath(repo_root, canonical_root, name, "SKILL.md"))
    end)
end

function parse_frontmatter(path)
    lines = split(read(path, String), '\n'; keepempty=true)
    if isempty(lines) || strip(lines[1]) != "---"
        error("$path does not start with YAML frontmatter")
    end
    closing = findnext(line -> strip(line) == "---", lines, 2)
    closing === nothing && error("$path has no closing frontmatter delimiter")

    metadata = Dict{String,String}()
    for line in lines[2:closing-1]
        isempty(strip(line)) && continue
        parts = split(line, ':'; limit=2)
        length(parts) == 2 || error("unable to parse frontmatter line in $path: $line")
        metadata[strip(parts[1])] = strip(parts[2])
    end
    return metadata
end

function copy_tracked_skill_files(skill)
    canonical_dir = joinpath(repo_root, canonical_root, skill)
    canonical_rel = relpath_unix(canonical_dir)
    canonical_files = sort!(collect(tracked_files_under(canonical_rel)))

    for mirror_root in mirror_roots
        mirror_dir = joinpath(repo_root, mirror_root, skill)
        rm(mirror_dir; force=true, recursive=true)
        for canonical_file in canonical_files
            subpath = relpath(canonical_file, canonical_rel)
            dest = joinpath(mirror_dir, subpath)
            mkpath(dirname(dest))
            cp(joinpath(repo_root, canonical_file), dest; force=true)
        end
    end
end

function remove_extra_mirror_entries(skills)
    for mirror_root in mirror_roots
        mirror_dir = joinpath(repo_root, mirror_root)
        isdir(mirror_dir) || continue
        for entry in readdir(mirror_dir)
            entry in skills && continue
            rm(joinpath(mirror_dir, entry); force=true, recursive=true)
        end
    end
end

function check_agent_skills(; fix::Bool=false)
    errors = String[]
    skills = canonical_skill_names()
    skill_set = Set(skills)

    if fix
        remove_extra_mirror_entries(skill_set)
        foreach(copy_tracked_skill_files, skills)
    end

    expected_mirror_files = Set{String}()

    for skill in skills
        skill_dir = joinpath(repo_root, canonical_root, skill)
        skill_file = joinpath(skill_dir, "SKILL.md")
        metadata = try
            parse_frontmatter(skill_file)
        catch err
            push!(errors, sprint(showerror, err))
            continue
        end

        name = get(metadata, "name", nothing)
        description = get(metadata, "description", nothing)
        name == skill || push!(errors, "$canonical_root/$skill/SKILL.md has name=$(repr(name)); expected $(repr(skill))")
        if isnothing(description) || isempty(description) || length(description) > 1024
            push!(errors, "$canonical_root/$skill/SKILL.md has an invalid description length")
        end
        occursin(r"^[a-z0-9]+(-[a-z0-9]+)*$", skill) ||
            push!(errors, "$canonical_root/$skill has an invalid skill name")
        length(skill) <= 64 || push!(errors, "$canonical_root/$skill has a skill name longer than 64 characters")

        canonical_rel = "$canonical_root/$skill"
        canonical_files = sort!(collect(tracked_files_under(canonical_rel)))
        isempty(canonical_files) && push!(errors, "$canonical_rel has no tracked files")

        for mirror_root in mirror_roots
            for canonical_file in canonical_files
                subpath = relpath(canonical_file, canonical_rel)
                mirror_file = "$mirror_root/$skill/$subpath"
                push!(expected_mirror_files, mirror_file)

                mode = git_file_mode(mirror_file)
                if isnothing(mode)
                    push!(errors, "$mirror_file is not tracked; run contrib/check-agent-skills.jl --fix and git add the mirror copies")
                    continue
                elseif mode == "120000"
                    push!(errors, "$mirror_file is tracked as a symlink; use a regular mirror copy")
                    continue
                elseif !startswith(mode, "100")
                    push!(errors, "$mirror_file has unexpected git file mode $mode")
                    continue
                end
                if !isfile(joinpath(repo_root, mirror_file))
                    push!(errors, "$mirror_file is missing or is not a regular file")
                    continue
                end
                canonical_content = read(joinpath(repo_root, canonical_file), String)
                mirror_content = read(joinpath(repo_root, mirror_file), String)
                canonical_content == mirror_content ||
                    push!(errors, "$mirror_file does not match $canonical_file; run contrib/check-agent-skills.jl --fix")
            end
        end
    end

    for mirror_root in mirror_roots
        for tracked in tracked_files_under(mirror_root)
            tracked in expected_mirror_files || push!(errors, "$tracked is not expected from canonical Agent Skills")
        end
    end

    if isempty(errors)
        println(stderr, "Agent Skill mirror check found no issues.")
        return 0
    else
        println(stderr, "Agent Skill mirror check found $(length(errors)) issue(s):")
        for error in errors
            println(stderr, "  - ", error)
            if is_gha
                println(stdout, "::error title=Agent Skill mirror check::", error)
            end
        end
        return 1
    end
end

function print_usage(io::IO=stdout)
    print(io, """
    usage: contrib/check-agent-skills.jl [--fix] [--help]

    Check that checked-in Agent Skill mirror copies under `.agents/skills/` and
    `.claude/skills/` match the canonical Agent Skills (https://agentskills.io)
    documented in `doc/src/devdocs/agents/README.md` and stored under
    `doc/src/devdocs/agents/skills/`.

    Options:
      --fix       Refresh mirror copies from the canonical Agent Skills before checking.
      -h, --help  Show this help message.
    """)
end

function (@main)(args::Vector{String})
    allowed_args = Set(["--fix", "--help", "-h"])
    unknown_args = filter(arg -> !(arg in allowed_args), args)
    if !isempty(unknown_args)
        println(stderr, "error: unknown argument(s): ", join(unknown_args, ", "))
        print_usage(stderr)
        return 1
    end

    if "--help" in args || "-h" in args
        print_usage(stdout)
        return 0
    end

    return check_agent_skills(; fix="--fix" in args)
end
