module REPLMode

import Pkg3
using Pkg3.Types
using Pkg3.Operations

import Base: LineEdit, REPL, REPLCompletions
import Base.Random: UUID

const cmds = Dict(
    "help"      => :help,
    "?"         => :help,
    "status"    => :status,
    "st"        => :status,
    "search"    => :search,
    "find"      => :search,
    "info"      => :info,
    "add"       => :add,
    "install"   => :add,
    "rm"        => :rm,
    "remove"    => :rm,
    "uninstall" => :rm,
    "up"        => :up,
    "update"    => :up,
    "upgrade"   => :up,
    "test"      => :test,
    "gc"        => :gc,
    "fsck"      => :fsck,
)

function parse_option(word::AbstractString)
    m = match(r"^--(\w+)(?:\s*=\s*(\S*))?$", word)
    m == nothing && cmderror("invalid option: ", repr(word))
    return m.captures[2] == nothing ?
        (:opt, Symbol(m.captures[1])) :
        (:opt, Symbol(m.captures[1]), String(m.captures[2]))
end

let uuid = raw"(?i)[0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}(?-i)",
    name = raw"(\w+)(?:\.jl)?"
    global name_re = Regex("^$name\$")
    global uuid_re = Regex("^$uuid\$")
    global name_uuid_re = Regex("^$name\\s*=\\s*($uuid)\$")
end

const lex_re = r"[^@\s]+\s*=\s*[^@\s]+ | @\s*[^@\s]* | [^@\s]+"x

function tokenize(cmd::String)::Vector{Tuple{Symbol,Vararg{Any}}}
    tokens = Tuple{Symbol,Vararg{Any}}[]
    # TODO: handle string-quoted values, e.g. path names
    words = map(m->m.match, eachmatch(lex_re, cmd))
    help_mode = false
    while !isempty(words)
        word = shift!(words)
        if word[1] == '-'
            push!(tokens, parse_option(word))
        else
            word in keys(cmds) || cmderror("invalid command: ", repr(word))
            push!(tokens, (:cmd, cmds[word]))
            help_mode || cmds[word] != :help && break
            help_mode = true
        end
    end
    if isempty(tokens) || tokens[end][1] != :cmd
        cmderror("no package command given")
    end
    while !isempty(words)
        word = shift!(words)
        if word[1] == '-'
            push!(tokens, parse_option(word))
        elseif word[1] == '@'
            push!(tokens, (:ver, VersionRange(strip(word[2:end]))))
        elseif ismatch(uuid_re, word)
            push!(tokens, (:pkg, UUID(word)))
        elseif ismatch(name_re, word)
            push!(tokens, (:pkg, String(match(name_re, word).captures[1])))
        elseif ismatch(name_uuid_re, word)
            m = match(name_uuid_re, word)
            push!(tokens, (:pkg, String(m.captures[1]), UUID(m.captures[2])))
        else
            cmderror("invalid argument: ", repr(word))
        end
    end
    return tokens
end

function do_cmd(repl::Base.REPL.AbstractREPL, input::String)
    try
        tokens = tokenize(input)
        local cmd::Symbol
        local env_opt::Union{String,Void} = nothing
        while !isempty(tokens)
            token = shift!(tokens)
            if token[1] == :cmd
                cmd = token[2]
                break
            elseif token[1] == :opt
                if token[2] == :env
                    length(token) == 3 ||
                        cmderror("the `--env` option requires a value")
                    env_opt = token[3]
                else
                    cmderror("unrecognized option: `--$(token[2])`")
                end
            else
                cmderror("misplaced token: ", token)
            end
        end
        env = EnvCache(env_opt)
        cmd == :rm     ?     do_rm!(env, tokens) :
        cmd == :add    ?    do_add!(env, tokens) :
        cmd == :up     ?     do_up!(env, tokens) :
        cmd == :status ? do_status!(env, tokens) :
            cmderror("`$cmd` command not yet implemented")
    catch err
        if err isa CommandError
            Base.display_error(repl.t.err_stream, ErrorException(err.msg), Ptr{Void}[])
        else
            Base.display_error(repl.t.err_stream, err, Base.catch_backtrace())
        end
    end
end

function do_rm!(env::EnvCache, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    # tokens: package names and/or uuids
    isempty(tokens) &&
        cmderror("`rm` – list packages to remove")
    pkgs = Package[]
    while !isempty(tokens)
        token = shift!(tokens)
        token[1] != :pkg &&
            cmderror("`rm` only accepts package names and/or UUIDs")
        push!(pkgs, Package(token[2:end]...))
    end
    project_resolve!(env, pkgs)
    ensure_resolved(env, pkgs)
    Pkg3.Operations.rm(env, pkgs)
end

function do_add!(env::EnvCache, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    # tokens: package names and/or uuids, optionally followed by version specs
    isempty(tokens) &&
    cmderror("`add` – list packages to add")
    tokens[1][1] == :ver &&
        cmderror("package name/uuid must precede version spec `@$(tokens[1][2])`")
    pkgs = PackageVersion[]
    while !isempty(tokens)
        token = shift!(tokens)
        if token[1] == :pkg
            push!(pkgs, PackageVersion(Package(token[2:end]...)))
        elseif token[1] == :ver
            pkgs[end].version = VersionSpec(token[2])
            isempty(tokens) || tokens[1][1] == :pkg ||
                cmderror("package name/uuid must precede version spec `@$(tokens[1][2])`")
        elseif token[1] == :opt
            cmderror("`add` doesn't take options: --$(join(token[2:end], '='))\ninvalid command: $input")
        end
    end
    project_resolve!(env, pkgs)
    registry_resolve!(env, pkgs)
    ensure_resolved(env, pkgs, true)
    Pkg3.Operations.add(env, pkgs)
end

function do_up!(env::EnvCache, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    # tokens:
    #  - upgrade levels as options: --[fixed|patch|minor|major]
    #  - package names and/or uuids, optionally followed by version specs
    !isempty(tokens) && tokens[1][1] == :ver &&
        cmderror("package name/uuid must precede version spec `@$(tokens[1][2])`")
    pkgs = PackageVersion[]
    level = UpgradeLevel(:major)
    while !isempty(tokens)
        token = shift!(tokens)
        if token[1] == :pkg
            push!(pkgs, PackageVersion(Package(token[2:end]...), level))
        elseif token[1] == :ver
            pkgs[end].version = VersionSpec(token[2])
            isempty(tokens) || tokens[1][1] == :pkg ||
                cmderror("package name/uuid must precede version spec `@$(tokens[1][2])`")
        elseif token[1] == :opt
            level = UpgradeLevel(token[2])
            length(token) == 3 &&
                cmderror("the --$(token[2]) option does not take an argument")
        end
    end
    project_resolve!(env, pkgs)
    ensure_resolved(env, pkgs)
    if isempty(pkgs)
        for (name::String, uuid::UUID) in env.project["deps"]
            push!(pkgs, PackageVersion(Package(name, uuid), level))
        end
    end
    Pkg3.Operations.up(env, pkgs)
end

function do_status!(env::EnvCache, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    isempty(tokens) || cmderror("`status` does not take arguments")
    if env.git == nothing
        cmderror("`status` only supported in git-saved environments")
    else
        path = LibGit2.path(env.git)
        project_path = relpath(env.project_file, path)
        manifest_path = relpath(env.manifest_file, path)
        project = read_project(git_file_stream(env.git, "HEAD:$project_path", fakeit=true))
        manifest = read_manifest(git_file_stream(env.git, "HEAD:$manifest_path", fakeit=true))
        print_with_color(:cyan, "Project\n")
        print_project_diff(project["deps"], env.project["deps"])
        print_with_color(:cyan, "Manifest\n")
        print_manifest_diff(manifest, env.manifest)
    end
end

function create_mode(repl, main)
    pkg_mode = LineEdit.Prompt("pkg> ";
        prompt_prefix = Base.text_colors[:blue],
        prompt_suffix = "",
        sticky = true)

    hp = main.hist
    hp.mode_mapping[:pkg] = pkg_mode
    pkg_mode.hist = hp

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    prefix_prompt, prefix_keymap = LineEdit.setup_prefix_keymap(hp, pkg_mode)

    pkg_mode.on_done = (s, buf, ok) -> begin
        ok || return REPL.transition(s, :abort)
        input = String(take!(buf))
        REPL.reset(repl)
        do_cmd(repl, input)
        REPL.prepare_next(repl)
        REPL.reset_state(s)
        s.current_mode.sticky || REPL.transition(s, main)
    end

    mk = REPL.mode_keymap(main)
    # ^C should not exit prompt
    delete!(mk, "^C")

    b = Dict{Any,Any}[
        skeymap, mk, prefix_keymap, LineEdit.history_keymap,
        LineEdit.default_keymap, LineEdit.escape_defaults
    ]
    pkg_mode.keymap_dict = LineEdit.keymap(b)
    return pkg_mode
end

function repl_init(repl)
    main_mode = repl.interface.modes[1]
    pkg_mode = create_mode(repl, main_mode)
    push!(repl.interface.modes, pkg_mode)
    keymap = Dict{Any,Any}(
        ']' => function (s,args...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                LineEdit.transition(s, pkg_mode) do
                    LineEdit.state(s, pkg_mode).input_buffer = buf
                end
            else
                LineEdit.edit_insert(s, ']')
            end
        end
    )
    main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, keymap)
    return
end

end
