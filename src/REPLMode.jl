module REPLMode

using Markdown
using UUIDs

import REPL
import REPL: LineEdit, REPLCompletions

import Pkg3
using Pkg3.Types
using Pkg3.Display
using Pkg3.Operations

const cmds = Dict(
    "help"      => :help,
    "?"         => :help,
    "status"    => :status,
    "st"        => :status,
    "."         => :status,
    "search"    => :search,
    "find"      => :search,
    "/"         => :search,
    "add"       => :add,
    "install"   => :add,
    "+"         => :add,
    "rm"        => :rm,
    "remove"    => :rm,
    "uninstall" => :rm,
    "-"         => :rm,
    "up"        => :up,
    "update"    => :up,
    "upgrade"   => :up,
    "test"      => :test,
    "gc"        => :gc,
    "fsck"      => :fsck,
    "preview"   => :preview,
    "init"      => :init,
    "build"     => :build,
)

const opts = Dict(
    "env"      => :env,
    "project"  => :project,
    "p"        => :project,
    "manifest" => :manifest,
    "m"        => :manifest,
    "major"    => :major,
    "minor"    => :minor,
    "patch"    => :patch,
    "fixed"    => :fixed,
    "coverage" => :coverage,
)

function parse_option(word::AbstractString)
    m = match(r"^(?: -([a-z]) | --([a-z]{2,})(?:\s*=\s*(\S*))? )$"ix, word)
    m == nothing && cmderror("invalid option: ", repr(word))
    k = m.captures[1] != nothing ? m.captures[1] : m.captures[2]
    haskey(opts, k) || cmderror("invalid option: ", repr(word))
    m.captures[3] == nothing ?
        (:opt, opts[k]) : (:opt, opts[k], String(m.captures[3]))
end

let uuid = raw"(?i)[0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}(?-i)",
    name = raw"(\w+)(?:\.jl)?"
    global const name_re = Regex("^$name\$")
    global const uuid_re = Regex("^$uuid\$")
    global const name_uuid_re = Regex("^$name\\s*=\\s*($uuid)\$")
end

const lex_re = r"^[\?\./\+\-](?!\-) | [^@\s]+\s*=\s*\S+ | @\s*[^@\s]* | [^@\s]+"x

function tokenize(cmd::String)::Vector{Tuple{Symbol,Vararg{Any}}}
    tokens = Tuple{Symbol,Vararg{Any}}[]
    # TODO: handle string-quoted values, e.g. path names
    words = map(m->m.match, eachmatch(lex_re, cmd))
    help_mode = false
    while !isempty(words)
        word = popfirst!(words)
        if word[1] == '-' && length(word) > 1
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
        word = popfirst!(words)
        if word[1] == '-'
            push!(tokens, parse_option(word))
        elseif word[1] == '@'
            push!(tokens, (:ver, VersionRange(strip(word[2:end]))))
        elseif contains(word, uuid_re)
            push!(tokens, (:pkg, UUID(word)))
        elseif contains(word, name_re)
            push!(tokens, (:pkg, String(match(name_re, word).captures[1])))
        elseif contains(word, name_uuid_re)
            m = match(name_uuid_re, word)
            push!(tokens, (:pkg, String(m.captures[1]), UUID(m.captures[2])))
        else
            cmderror("invalid argument: ", repr(word))
        end
    end
    return tokens
end

function do_cmd(repl::REPL.AbstractREPL, input::String)
    try
        tokens = tokenize(input)
        do_cmd!(tokens, repl)
    catch err
        if err isa CommandError
            Base.display_error(repl.t.err_stream, ErrorException(err.msg), Ptr{Nothing}[])
        else
            Base.display_error(repl.t.err_stream, err, Base.catch_backtrace())
        end
    end
end

function do_cmd!(tokens, repl; preview = false)
    cmd = env_opt = nothing
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token[1] == :cmd
            cmd = token[2]
            break
        elseif token[1] == :opt
            if token[2] == :env
                length(token) == 3 ||
                    cmderror("the `--env` option requires a value")
                env_opt = Base.parse_env(token[3])
            else
                cmderror("unrecognized option: `--$(token[2])`")
            end
        else
            cmderror("misplaced token: ", token)
        end
    end
    cmd == :preview && return do_preview!(tokens, repl)
    ctx = Context(env = EnvCache(env_opt), preview = preview)
    cmd == :help    ?    do_help!(ctx, tokens, repl) :
    cmd == :init    ?    do_init!(ctx, tokens) :
    cmd == :rm      ?      do_rm!(ctx, tokens) :
    cmd == :add     ?     do_add!(ctx, tokens) :
    cmd == :up      ?      do_up!(ctx, tokens) :
    cmd == :status  ?  do_status!(ctx, tokens) :
    cmd == :test    ?    do_test!(ctx, tokens) :
    cmd == :gc      ?      do_gc!(ctx, tokens) :
    cmd == :build   ?   do_build!(ctx, tokens) :
        cmderror("`$cmd` command not yet implemented")
end

function do_preview!(tokens, repl)
    isempty(tokens) && cmderror("`preview` needs a command")
    word = tokens[1][2]
    word in keys(cmds) || cmderror("invalid command: ", repr(word))
    cmds[word] == :init && cmderror("cannot run `init` in preview mode")
    tokens[1] = (:cmd, cmds[word])
    do_cmd!(tokens, repl, preview = true)
end

const help = Markdown.parse("""
    **Synopsis**

        pkg> [--env=...] cmd [opts] [args]

    **Environment**

    The `--env` meta option determines which project environment to manipulate. By
    default, this looks for a git repo in the parents directories of the current
    working directory, and if it finds one, it uses that as an environment. Otherwise,
    it uses a named environment (typically found in `~/.julia/environments`) looking
    for environments named `v$(VERSION.major).$(VERSION.minor).$(VERSION.patch)`,
    `v$(VERSION.major).$(VERSION.minor)`,  `v$(VERSION.major)` or `default` in order.

    **Commands**

    What action you want the package manager to take:

    `help`: show this message

    `status`: summarize contents of and changes to environment

    `add`: add packages to project

    `rm`: remove packages from project or manifest

    `up`: update packages in manifest

    `preview`: previews a subsequent command without affecting the current state

    `test`: run tests for packages

    `gc`: garbage collect packages not used for a significant time

    `init` initializes an environment in the current, or git base, directory

    `build` run the build script for packages
    """)

const helps = Dict(
    :help => md"""

        help

    Display this message.

        help cmd ...

    Display usage information for commands listed.

    Available commands: `help`, `status`, `add`, `rm`, `up`
    """, :status => md"""

        status
        status [-p|--project]
        status [-m|--manifest]

    Show the status of the current environment. By default, the full contents of
    the project file is summarized, showing what version each package is on and
    how it has changed since the last git commit (if in a git repo), as well as
    any changes to manifest packages not already listed. In `--project` mode, the
    status of the project file is summarized. In `--project` mode, the status of
    the project file is summarized.
    """, :add => md"""

        add pkg[=uuid] [@version] ...

    Add package `pkg` to the current project file. If `pkg` could refer to
    multiple different packages, specifying `uuid` allows you to disambiguate.
    `@version` optionally allows specifying which versions of packages. Versions
    may be specified by `@1`, `@1.2`, `@1.2.3`, allowing any version with a prefix
    that matches, or ranges thereof, such as `@1.2-3.4.5`.
    """, :rm => md"""

        rm [-p|--project] pkg[=uuid] ...

    Remove package `pkg` from the project file. Since the name `pkg` can only
    refer to one package in a project this is unambiguous, but you can specify
    a `uuid` anyway, and the command is ignored, with a warning if package name
    and UUID do not mactch. When a package is removed from the project file, it
    may still remain in the manifest if it is required by some other package in
    the project. Project mode operation is the default, so passing `-p` or
    `--project` is optional unless it is preceded by the `-m` or `--manifest`
    options at some earlier point.

        rm [-m|--manifest] pkg[=uuid] ...

    Remove package `pkg` from the manifest file. If the name `pkg` refers to
    multiple packages in the manifest, `uuid` disambiguates it. Removing a package
    from the manifest forces the removal of all packages that depend on it, as well
    as any no-longer-necessary manifest packages due to project package removals.
    """, :up => md"""

        up [-p|project]  [opts] pkg[=uuid] [@version] ...
        up [-m|manifest] [opts] pkg[=uuid] [@version] ...

        opts: --major | --minor | --patch | --fixed

    Update the indicated package within the constraints of the indicated version
    specifications. Versions may be specified by `@1`, `@1.2`, `@1.2.3`, allowing
    any version with a prefix that matches, or ranges thereof, such as `@1.2-3.4.5`.
    In `--project` mode, package specifications only match project packages, while
    in `manifest` mode they match any manifest package. Bound level options force
    the following packages to be upgraded only within the current major, minor,
    patch version; if the `--fixed` upgrade level is given, then the following
    packages will not be upgraded at all.
    """, :preview => md"""

        preview cmd

    Runs the command `cmd` in preview mode. This is defined such that no side effects
    will take place i.e. no packages are downloaded and neither the project nor manifest
    is modified.
    """, :test => md"""

        test [opts] pkg[=uuid] ...

        opts: --coverage

    Run the tests for package `pkg`. This is done by running the file `test/runtests.jl`
    in the package directory. The option `--coverage` can be used to run the tests with
    coverage enabled.
    """, :gc => md"""

    Deletes packages that are not reached from any environment used within the last 6 weeks.
    """, :init => md"""

        init

    Creates an environment in the current directory, or the git base directory if the current directory
    is in a git repository.
    """, :build =>md"""

        build pkg[=uuid] ...

    Run the build script in deps/build.jl for each package in pkgs and all of their dependencies in depth-first recursive order.
    If no packages are given, runs the build scripts for all packages in the manifest.
    """,
)

function do_help!(
    ctk::Context,
    tokens::Vector{Tuple{Symbol,Vararg{Any}}},
    repl::REPL.AbstractREPL,
)
    disp = REPL.REPLDisplay(repl)
    if isempty(tokens)
        Base.display(disp, help)
        return
    end
    help_md = md""
    for token in tokens
        if token[1] == :cmd
            if haskey(helps, token[2])
                isempty(help_md.content) ||
                push!(help_md.content, md"---")
                push!(help_md.content, helps[token[2]].content)
            else
                cmderror("Sorry, I don't have any help for the `$(token[2])` command.")
            end
        else
            error("This should not happen")
        end
    end
    Base.display(disp, help_md)
end

function do_rm!(ctx::Context, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    # tokens: package names and/or uuids
    mode = :project
    pkgs = PackageSpec[]
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token[1] == :pkg
            push!(pkgs, PackageSpec(token[2:end]...))
            pkgs[end].mode = mode
        elseif token[1] == :ver
            cmderror("`rm` does not take version specs")
        elseif token[1] == :opt
            if token[2] in (:project, :manifest)
                length(token) == 2 ||
                    cmderror("the --$(token[2]) option does not take an argument")
                mode = token[2]
            else
                cmderror("invalid option for `rm`: --$(token[2])")
            end
        end
    end
    isempty(pkgs) &&
        cmderror("`rm` – list packages to remove")
    Pkg3.API.rm(ctx, pkgs)
end

function do_add!(ctx::Context, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    # tokens: package names and/or uuids, optionally followed by version specs
    isempty(tokens) &&
        cmderror("`add` – list packages to add")
    tokens[1][1] == :ver &&
        cmderror("package name/uuid must precede version spec `@$(tokens[1][2])`")
    pkgs = PackageSpec[]
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token[1] == :pkg
            push!(pkgs, PackageSpec(token[2:end]...))
        elseif token[1] == :ver
            pkgs[end].version = VersionSpec(token[2])
            isempty(tokens) || tokens[1][1] == :pkg ||
                cmderror("package name/uuid must precede version spec `@$(tokens[1][2])`")
        elseif token[1] == :opt
            cmderror("`add` doesn't take options: --$(join(token[2:end], '='))")
        end
    end
    Pkg3.API.add(ctx, pkgs)
end

function do_up!(ctx::Context, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    # tokens:
    #  - upgrade levels as options: --[fixed|patch|minor|major]
    #  - package names and/or uuids, optionally followed by version specs
    mode = :project
    pkgs = PackageSpec[]
    level = UpgradeLevel(:major)
    last_token_type = :cmd
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token[1] == :pkg
            push!(pkgs, PackageSpec(token[2:end]..., level))
            pkgs[end].mode = mode
        elseif token[1] == :ver
            pkgs[end].version = VersionSpec(token[2])
            last_token_type == :pkg ||
                cmderror("package name/uuid must precede version spec `@$(token[2])`")
        elseif token[1] == :opt
            if token[2] in (:project, :manifest)
                length(token) == 2 ||
                    cmderror("the --$(token[2]) option does not take an argument")
                mode = token[2]
            elseif token[2] in (:major, :minor, :patch, :fixed)
                length(token) == 2 ||
                    cmderror("the --$(token[2]) option does not take an argument")
                level = UpgradeLevel(token[2])
            else
                cmderror("invalid option for `up`: --$(token[2])")
            end
        end
        last_token_type = token[1]
    end
    Pkg3.API.up(ctx, pkgs; level=level, mode=mode)
end

function do_status!(ctx::Context, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    mode = :combined
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token[1] == :opt
            if token[2] in (:project, :manifest)
                length(token) == 2 ||
                    cmderror("the --$(token[2]) option does not take an argument")
                mode = token[2]
            else
                cmderror("invalid option for `status`: --$(token[2])")
            end
        else
            cmderror("`status` does not take arguments")
        end
    end
    Pkg3.Display.status(ctx, mode)
end

# TODO , test recursive dependencies as on option.
function do_test!(ctx::Context, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    pkgs = PackageSpec[]
    coverage = false
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token[1] == :pkg
            if length(token) == 2
                pkg = PackageSpec(token[2])
                pkg.mode = :manifest
                push!(pkgs, pkg)
            else
                cmderror("`test` only takes a set of packages to test")
            end
        elseif token[1] == :opt
            if token[2] == :coverage
                coverage = true
            else
                cmderror("invalid option for `test`: --$(token[2])")
            end
        else
            # TODO: Better error message
            cmderror("invalid usage for `test`")
        end
    end
    isempty(pkgs) && cmderror("`test` takes a set of packages")
    Pkg3.API.test(ctx, pkgs; coverage = coverage)
end

function do_gc!(ctx::Context, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    !isempty(tokens) && cmderror("`gc` does not take any arguments")
    Pkg3.API.gc(ctx)
end

function do_build!(ctx::Context, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    pkgs = PackageSpec[]
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token[1] == :pkg
            push!(pkgs, PackageSpec(token[2:end]...))
        else
            cmderror("`build` only takes a list of packages")
        end
    end
    Pkg3.API.build(ctx, pkgs)
end

function do_init!(ctx::Context, tokens::Vector{Tuple{Symbol,Vararg{Any}}})
    if !isempty(tokens)
        cmderror("`init` does currently not take any arguments")
    end
    Pkg3.API.init(ctx)
end


function create_mode(repl, main)
    pkg_mode = LineEdit.Prompt("pkg> ";
        prompt_prefix = Base.text_colors[:blue],
        prompt_suffix = "",
        sticky = true)

    pkg_mode.repl = repl
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
