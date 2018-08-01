# This file is a part of Julia. License is MIT: https://julialang.org/license

module REPLMode

using Markdown
using UUIDs

import REPL
import REPL: LineEdit, REPLCompletions

import ..devdir, ..Types.casesensitive_isdir, ..TOML
using ..Types, ..Display, ..Operations, ..API

############
# Commands #
############
@enum(CommandKind, CMD_HELP, CMD_STATUS, CMD_SEARCH, CMD_ADD, CMD_RM, CMD_UP,
                   CMD_TEST, CMD_GC, CMD_PREVIEW, CMD_INIT, CMD_BUILD, CMD_FREE,
                   CMD_PIN, CMD_CHECKOUT, CMD_DEVELOP, CMD_GENERATE, CMD_PRECOMPILE,
                   CMD_INSTANTIATE, CMD_RESOLVE, CMD_ACTIVATE, CMD_DEACTIVATE)

struct Command
    kind::CommandKind
    val::String
end
Base.show(io::IO, cmd::Command) = print(io, cmd.val)

const cmds = Dict(
    "help"        => CMD_HELP,
    "?"           => CMD_HELP,
    "status"      => CMD_STATUS,
    "st"          => CMD_STATUS,
    "add"         => CMD_ADD,
    "rm"          => CMD_RM,
    "remove"      => CMD_RM,
    "up"          => CMD_UP,
    "update"      => CMD_UP,
    "test"        => CMD_TEST,
    "gc"          => CMD_GC,
    "preview"     => CMD_PREVIEW,
    "build"       => CMD_BUILD,
    "pin"         => CMD_PIN,
    "free"        => CMD_FREE,
    "develop"     => CMD_DEVELOP,
    "dev"         => CMD_DEVELOP,
    "generate"    => CMD_GENERATE,
    "precompile"  => CMD_PRECOMPILE,
    "instantiate" => CMD_INSTANTIATE,
    "resolve"     => CMD_RESOLVE,
    "activate"    => CMD_ACTIVATE,
)

#################
# Git revisions #
#################
struct Rev
    rev::String
end

###########
# Options #
###########
@enum(OptionKind, OPT_ENV, OPT_PROJECT, OPT_MANIFEST, OPT_MAJOR, OPT_MINOR,
                  OPT_PATCH, OPT_FIXED, OPT_COVERAGE, OPT_NAME,
                  OPT_LOCAL, OPT_SHARED)

function Types.PackageMode(opt::OptionKind)
    opt == OPT_MANIFEST && return PKGMODE_MANIFEST
    opt == OPT_PROJECT  && return PKGMODE_PROJECT
    throw(ArgumentError("invalid option $opt"))
end

function Types.UpgradeLevel(opt::OptionKind)
    opt == OPT_MAJOR && return UPLEVEL_MAJOR
    opt == OPT_MINOR && return UPLEVEL_MINOR
    opt == OPT_PATCH && return UPLEVEL_PATCH
    opt == OPT_FIXED && return UPLEVEL_FIXED
    throw(ArgumentError("invalid option $opt"))
end

struct Option
    kind::OptionKind
    val::String
    argument::Union{String, Nothing}
    Option(kind::OptionKind, val::String) = new(kind, val, nothing)
    function Option(kind::OptionKind, val::String, argument::Union{String, Nothing})
        if kind in (OPT_PROJECT, OPT_MANIFEST, OPT_MAJOR,
                    OPT_MINOR, OPT_PATCH, OPT_FIXED) &&
                argument !== nothing
            cmderror("the `$val` option does not take an argument")
        elseif kind in (OPT_ENV,) && argument == nothing
            cmderror("the `$val` option requires an argument")
        end
        new(kind, val, argument)
    end
end
Base.show(io::IO, opt::Option) = print(io, "--$(opt.val)", opt.argument == nothing ? "" : "=$(opt.argument)")

const opts = Dict(
    "env"      => OPT_ENV,
    "project"  => OPT_PROJECT,
    "p"        => OPT_PROJECT,
    "manifest" => OPT_MANIFEST,
    "m"        => OPT_MANIFEST,
    "major"    => OPT_MAJOR,
    "minor"    => OPT_MINOR,
    "patch"    => OPT_PATCH,
    "fixed"    => OPT_FIXED,
    "coverage" => OPT_COVERAGE,
    "name"     => OPT_NAME,
    "local"    => OPT_LOCAL,
    "shared"   => OPT_SHARED,
)

function parse_option(word::AbstractString)::Option
    m = match(r"^(?: -([a-z]) | --([a-z]{2,})(?:\s*=\s*(\S*))? )$"ix, word)
    m == nothing && cmderror("invalid option: ", repr(word))
    k = m.captures[1] != nothing ? m.captures[1] : m.captures[2]
    haskey(opts, k) || cmderror("invalid option: ", repr(word))
    return Option(opts[k], String(k), m.captures[3] == nothing ? nothing : String(m.captures[3]))
end

###################
# Package parsing #
###################
let uuid = raw"(?i)[0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}(?-i)",
    name = raw"(\w+)(?:\.jl)?"
    global const name_re = Regex("^$name\$")
    global const uuid_re = Regex("^$uuid\$")
    global const name_uuid_re = Regex("^$name\\s*=\\s*($uuid)\$")
end

# packages can be identified through: uuid, name, or name+uuid
# additionally valid for add/develop are: local path, url
function parse_package(word::AbstractString; add_or_develop=false)::PackageSpec
    word = replace(word, "~" => homedir())
    if add_or_develop && casesensitive_isdir(word)
        return PackageSpec(Types.GitRepo(word))
    elseif occursin(uuid_re, word)
        return PackageSpec(UUID(word))
    elseif occursin(name_re, word)
        return PackageSpec(String(match(name_re, word).captures[1]))
    elseif occursin(name_uuid_re, word)
        m = match(name_uuid_re, word)
        return PackageSpec(String(m.captures[1]), UUID(m.captures[2]))
    elseif add_or_develop
        # Guess it is a url then
        return PackageSpec(Types.GitRepo(word))
    else
        cmderror("`$word` cannot be parsed as a package")
    end
end

################
# REPL parsing #
################
const lex_re = r"^[\?\./\+\-](?!\-) | ((git|ssh|http(s)?)|(git@[\w\-\.]+))(:(//)?)([\w\.@\:/\-~]+)(\.git)(/)? | [^@\#\s;]+\s*=\s*[^@\#\s;]+ | \#\s*[^@\#\s;]* | @\s*[^@\#\s;]* | [^@\#\s;]+|;"x

const Token = Union{Command, Option, VersionRange, String, Rev}

function tokenize(cmd::String)::Vector{Vector{Token}}
    # phase 1: tokenize accoring to whitespace / quotes
    chunks = parse_quotes(cmd)
    # phase 2: tokenzie unquoted tokens according to pkg REPL syntax
    words::Vector{String} = []
    for chunk in chunks
        is_quoted = chunk[1]
        word = chunk[2]
        if is_quoted
            push!(words, word)
        else # break unquoted chunks further according to lexer
            # note: space before `$word` is necessary to keep using current `lex_re`
            #                                                 v
            append!(words, map(m->m.match, eachmatch(lex_re, " $word")))
        end
    end

    commands = Vector{Token}[]
    while !isempty(words)
        push!(commands, tokenize!(words))
    end
    return commands
end

function parse_quotes(cmd::String)
    in_doublequote = false
    in_singlequote = false
    all_tokens::Array = []
    token_in_progress::Array{Char} = []

    push_token!(is_quoted) = begin
        complete_token = String(token_in_progress)
        empty!(token_in_progress)
        push!(all_tokens, (is_quoted, complete_token))
    end

    for c in cmd
        if c == '"'
            if in_singlequote # raw char
                push!(token_in_progress, c)
            else # delimiter
                in_doublequote = !in_doublequote
                push_token!(true)
            end
        elseif c == '\''
            if in_doublequote # raw char
                push!(token_in_progress, c)
            else # delimiter
                in_singlequote = !in_singlequote
                push_token!(true)
            end
        elseif c == ' ' && !(in_doublequote || in_singlequote)
            push_token!(false)
        else
            push!(token_in_progress, c)
        end
    end
    if (in_doublequote || in_singlequote)
        ArgumentError("unterminated quote")
    else
        push_token!(false)
    end
    # to avoid complexity in the main loop, empty tokens are allowed above and
    # filtered out before returning
    isnotempty(x) = !isempty(x[2])
    filter!(isnotempty, all_tokens)
    return all_tokens
end

function tokenize!(words::Vector{<:AbstractString})::Vector{Token}
    tokens = Token[]
    help_mode = false
    preview_mode = false
    # First parse a Command or a modifier (help / preview) + Command
    while !isempty(words)
        word = popfirst!(words)
        if word[1] == '-' && length(word) > 1
            push!(tokens, parse_option(word))
        else
            haskey(cmds, word) || cmderror("invalid command: ", repr(word))
            cmdkind = cmds[word]
            push!(tokens, Command(cmdkind, word))
            # If help / preview and not in help mode we want to eat another cmd
            if !help_mode
                cmdkind == CMD_HELP    && (help_mode    = true; continue)
                cmdkind == CMD_PREVIEW && (preview_mode = true; continue)
            end
            break
        end
    end
    if isempty(tokens) || !(tokens[end] isa Command)
        cmderror("no package command given")
    end
    # Now parse the arguments / options to the command
    while !isempty(words)
        word = popfirst!(words)
        if word == ";"
            return tokens
        elseif first(word) == '-'
            push!(tokens, parse_option(word))
        elseif first(word) == '@'
            push!(tokens, VersionRange(strip(word[2:end])))
        elseif first(word) == '#'
            push!(tokens, Rev(word[2:end]))
        else
            push!(tokens, String(word))
        end
    end
    return tokens
end


#############
# Execution #
#############

function do_cmd(repl::REPL.AbstractREPL, input::String; do_rethrow=false)
    try
        commands = tokenize(input)
        for command in commands
            do_cmd!(command, repl)
        end
    catch err
        if do_rethrow
            rethrow(err)
        end
        if err isa CommandError || err isa ResolverError
            Base.display_error(repl.t.err_stream, ErrorException(sprint(showerror, err)), Ptr{Nothing}[])
        else
            Base.display_error(repl.t.err_stream, err, Base.catch_backtrace())
        end
    end
end

function enforce_argument_order(tokens::Vector{Token})
    prev_token = nothing
    function check_prev_token(valid_type::DataType, error_message::AbstractString)
        prev_token isa valid_type || cmderror(error_message)
    end

    for token in tokens
        if token isa VersionRange
            check_prev_token(String, "package name/uuid must precede version spec `@$token`")
        elseif token isa Rev
            check_prev_token(String, "package name/uuid must precede rev spec `#$(token.rev)`")
        end
        prev_token = token
    end
end

function do_cmd!(tokens::Vector{Token}, repl)
    cmd = env_opt = nothing
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa Command
            cmd = token
            break
        elseif token isa Option
            # Only OPT_ENV is allowed before a command
            if token.kind == OPT_ENV
                env_opt = Base.parse_env(token.argument)
            else
                cmderror("unrecognized command option: `$token`")
            end
        else
            cmderror("misplaced token: ", token)
        end
    end

    if cmd.kind == CMD_ACTIVATE
        return Base.invokelatest(do_activate!, Base.active_project() === nothing ?
            nothing : EnvCache(env_opt), tokens)
    end

    ctx = Context(env = EnvCache(env_opt))
    if cmd.kind == CMD_PREVIEW
        ctx.preview = true
        isempty(tokens) && cmderror("expected a command to preview")
        cmd = popfirst!(tokens)
    end

    enforce_argument_order(tokens)

    # Using invokelatest to hide the functions from inference.
    # Otherwise it would try to infer everything here.
    cmd.kind == CMD_INIT        ? Base.invokelatest(          do_init!, ctx, tokens) :
    cmd.kind == CMD_HELP        ? Base.invokelatest(          do_help!, ctx, tokens, repl) :
    cmd.kind == CMD_RM          ? Base.invokelatest(            do_rm!, ctx, tokens) :
    cmd.kind == CMD_ADD         ? Base.invokelatest(do_add_or_develop!, ctx, tokens, CMD_ADD) :
    cmd.kind == CMD_CHECKOUT    ? Base.invokelatest(do_add_or_develop!, ctx, tokens, CMD_DEVELOP) :
    cmd.kind == CMD_DEVELOP     ? Base.invokelatest(do_add_or_develop!, ctx, tokens, CMD_DEVELOP) :
    cmd.kind == CMD_UP          ? Base.invokelatest(            do_up!, ctx, tokens) :
    cmd.kind == CMD_STATUS      ? Base.invokelatest(        do_status!, ctx, tokens) :
    cmd.kind == CMD_TEST        ? Base.invokelatest(          do_test!, ctx, tokens) :
    cmd.kind == CMD_GC          ? Base.invokelatest(            do_gc!, ctx, tokens) :
    cmd.kind == CMD_BUILD       ? Base.invokelatest(         do_build!, ctx, tokens) :
    cmd.kind == CMD_PIN         ? Base.invokelatest(           do_pin!, ctx, tokens) :
    cmd.kind == CMD_FREE        ? Base.invokelatest(          do_free!, ctx, tokens) :
    cmd.kind == CMD_GENERATE    ? Base.invokelatest(      do_generate!, ctx, tokens) :
    cmd.kind == CMD_RESOLVE     ? Base.invokelatest(       do_resolve!, ctx, tokens) :
    cmd.kind == CMD_PRECOMPILE  ? Base.invokelatest(    do_precompile!, ctx, tokens) :
    cmd.kind == CMD_INSTANTIATE ? Base.invokelatest(   do_instantiate!, ctx, tokens) :
        cmderror("`$cmd` command not yet implemented")
    return
end

const help = md"""

**Welcome to the Pkg REPL-mode**. To return to the `julia>` prompt, either press
backspace when the input line is empty or press Ctrl+C.


**Synopsis**

    pkg> [--env=...] cmd [opts] [args]

Multiple commands can be given on the same line by interleaving a `;` between the commands.

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

`develop`: clone the full package repo locally for development

`rm`: remove packages from project or manifest

`up`: update packages in manifest

`test`: run tests for packages

`build`: run the build script for packages

`pin`: pins the version of packages

`free`: undoes a `pin`, `develop`, or stops tracking a repo.

`instantiate`: downloads all the dependencies for the project

`resolve`: resolves to update the manifest from changes in dependencies of
developed packages

`generate`: generate files for a new project

`preview`: previews a subsequent command without affecting the current state

`precompile`: precompile all the project dependencies

`gc`: garbage collect packages not used for a significant time

`activate`: set the primary environment the package manager manipulates
"""

const helps = Dict(
    CMD_HELP => md"""

        help

    Display this message.

        help cmd ...

    Display usage information for commands listed.

    Available commands: `help`, `status`, `add`, `rm`, `up`, `preview`, `gc`, `test`, `build`, `free`, `pin`, `develop`.
    """, CMD_STATUS => md"""

        status
        status [-p|--project]
        status [-m|--manifest]

    Show the status of the current environment. By default, the full contents of
    the project file is summarized, showing what version each package is on and
    how it has changed since the last git commit (if in a git repo), as well as
    any changes to manifest packages not already listed. In `--project` mode, the
    status of the project file is summarized. In `--manifest` mode the output also
    includes the dependencies of explicitly added packages.
    """, CMD_GENERATE => md"""

        generate pkgname

    Create a project called `pkgname` in the current folder.
    """,
    CMD_ADD => md"""

        add pkg[=uuid] [@version] [#rev] ...

    Add package `pkg` to the current project file. If `pkg` could refer to
    multiple different packages, specifying `uuid` allows you to disambiguate.
    `@version` optionally allows specifying which versions of packages. Versions
    may be specified by `@1`, `@1.2`, `@1.2.3`, allowing any version with a prefix
    that matches, or ranges thereof, such as `@1.2-3.4.5`. A git-revision can be
    specified by `#branch` or `#commit`.

    If a local path is used as an argument to `add`, the path needs to be a git repository.
    The project will then track that git repository just like if it is was tracking a remote repository online.

    **Examples**
    ```
    pkg> add Example
    pkg> add Example@0.5
    pkg> add Example#master
    pkg> add Example#c37b675
    pkg> add https://github.com/JuliaLang/Example.jl#master
    pkg> add git@github.com:JuliaLang/Example.jl.git
    pkg> add Example=7876af07-990d-54b4-ab0e-23690620f79a
    ```
    """, CMD_RM => md"""

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
    """, CMD_UP => md"""

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
    """, CMD_PREVIEW => md"""

        preview cmd

    Runs the command `cmd` in preview mode. This is defined such that no side effects
    will take place i.e. no packages are downloaded and neither the project nor manifest
    is modified.
    """, CMD_TEST => md"""

        test [opts] pkg[=uuid] ...

        opts: --coverage

    Run the tests for package `pkg`. This is done by running the file `test/runtests.jl`
    in the package directory. The option `--coverage` can be used to run the tests with
    coverage enabled.
    """, CMD_GC => md"""

    Deletes packages that cannot be reached from any existing environment.
    """, CMD_BUILD =>md"""

        build pkg[=uuid] ...

    Run the build script in deps/build.jl for each package in `pkg`` and all of their dependencies in depth-first recursive order.
    If no packages are given, runs the build scripts for all packages in the manifest.
    """, CMD_PIN => md"""

        pin pkg[=uuid] ...

    Pin packages to given versions, or the current version if no version is specified. A pinned package has its version fixed and will not be upgraded or downgraded.
    A pinned package has the symbol `⚲` next to its version in the status list.
    """, CMD_FREE => md"""
        free pkg[=uuid] ...

    Free a pinned package `pkg`, which allows it to be upgraded or downgraded again. If the package is checked out (see `help develop`) then this command
    makes the package no longer being checked out.
    """, CMD_DEVELOP => md"""
        develop [--shared|--local] pkg[=uuid] [#rev] ...

    Make a package available for development. If `pkg` is an existing local path that path will be recorded in
    the manifest and used. Otherwise, a full git clone of `pkg` at rev `rev` is made. The location of the clone is
    controlled by the `--shared` (default) and `--local` arguments. The `--shared` location defaults to
    `~/.julia/dev`, but can be controlled with the `JULIA_PKG_DEVDIR` environment variable. When `--local` is given,
    the clone is placed in a `dev` folder in the current project.
    This operation is undone by `free`.

    *Example*
    ```jl
    pkg> develop Example
    pkg> develop Example#master
    pkg> develop Example#c37b675
    pkg> develop https://github.com/JuliaLang/Example.jl#master
    pkg> develop --local Example
    ```
    """, CMD_PRECOMPILE => md"""
        precompile

    Precompile all the dependencies of the project by running `import` on all of them in a new process.
    """, CMD_INSTANTIATE => md"""
        instantiate
        instantiate [-m|--manifest]
        instantiate [-p|--project]

    Download all the dependencies for the current project at the version given by the project's manifest.
    If no manifest exists or the `--project` option is given, resolve and download the dependencies compatible with the project.
    """, CMD_RESOLVE => md"""
        resolve

    Resolve the project i.e. run package resolution and update the Manifest. This is useful in case the dependencies of developed
    packages have changed causing the current Manifest to_indices be out of sync.
    """
)

function do_help!(
    ctk::Context,
    tokens::Vector{Token},
    repl::REPL.AbstractREPL,
)
    disp = REPL.REPLDisplay(repl)
    if isempty(tokens)
        Base.display(disp, help)
        return
    end
    help_md = md""
    for token in tokens
        if token isa Command
            if haskey(helps, token.kind)
                isempty(help_md.content) ||
                push!(help_md.content, md"---")
                push!(help_md.content, helps[token.kind].content)
            else
                cmderror("Sorry, I don't have any help for the `$(token.val)` command.")
            end
        else
            error("invalid usage of help command")
        end
    end
    Base.display(disp, help_md)
end

function do_rm!(ctx::Context, tokens::Vector{Token})
    # tokens: package names and/or uuids
    mode = PKGMODE_PROJECT
    pkgs = PackageSpec[]
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa String
            push!(pkgs, parse_package(token))
            pkgs[end].mode = mode
        elseif token isa VersionRange
            cmderror("`rm` does not take version specs")
        elseif token isa Option
            if token.kind in (OPT_PROJECT, OPT_MANIFEST)
                mode = PackageMode(token.kind)
            else
                cmderror("invalid option for `rm`: $token")
            end
        end
    end
    isempty(pkgs) &&
        cmderror("`rm` – list packages to remove")
    API.rm(ctx, pkgs)
end

function do_add_or_develop!(ctx::Context, tokens::Vector{Token}, cmd::CommandKind)
    @assert cmd in (CMD_ADD, CMD_DEVELOP)
    mode = cmd == CMD_ADD ? :add : :develop
    # tokens: package names and/or uuids, optionally followed by version specs
    isempty(tokens) &&
        cmderror("`$mode` – list packages to $mode")
    pkgs = PackageSpec[]
    dev_mode = OPT_SHARED # TODO: Make this default configurable
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa String
            push!(pkgs, parse_package(token; add_or_develop=true))
        elseif token isa VersionRange
            pkgs[end].version = VersionSpec(token)
        elseif token isa Rev
            # WE did not get the repo from the
            pkg = pkgs[end]
            if pkg.repo == nothing
                pkg.repo = Types.GitRepo("", token.rev)
            else
                pkgs[end].repo.rev = token.rev
            end
        elseif token isa Option
            if mode === :develop && token.kind in (OPT_LOCAL, OPT_SHARED)
                dev_mode = token.kind
            else
                cmderror("`$mode` doesn't take options: $token")
            end
        end
    end
    dev_dir = mode === :add ? nothing : dev_mode == OPT_LOCAL ?
        joinpath(dirname(ctx.env.project_file), "dev") : nothing
    return API.add_or_develop(ctx, pkgs, mode=mode, devdir=dev_dir)
end

function do_up!(ctx::Context, tokens::Vector{Token})
    # tokens:
    #  - upgrade levels as options: --[fixed|patch|minor|major]
    #  - package names and/or uuids, optionally followed by version specs
    pkgs = PackageSpec[]
    mode = PKGMODE_PROJECT
    level = UPLEVEL_MAJOR
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa String
            push!(pkgs, parse_package(token))
            pkgs[end].version = level
            pkgs[end].mode = mode
        elseif token isa VersionRange
            pkgs[end].version = VersionSpec(token)
        elseif token isa Option
            if token.kind in (OPT_PROJECT, OPT_MANIFEST)
                mode = PackageMode(token.kind)
            elseif token.kind in (OPT_MAJOR, OPT_MINOR, OPT_PATCH, OPT_FIXED)
                level = UpgradeLevel(token.kind)
            else
                cmderror("invalid option for `up`: $(token)")
            end
        end
    end
    API.up(ctx, pkgs; level=level, mode=mode)
end

function do_pin!(ctx::Context, tokens::Vector{Token})
    pkgs = PackageSpec[]
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa String
            push!(pkgs, parse_package(token))
        elseif token isa VersionRange
            if token.lower != token.upper
                cmderror("pinning a package requires a single version, not a versionrange")
            end
            pkgs[end].version = VersionSpec(token)
        else
            cmderror("free only takes a list of packages ")
        end
    end
    API.pin(ctx, pkgs)
end

function do_free!(ctx::Context, tokens::Vector{Token})
    pkgs = PackageSpec[]
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa String
            push!(pkgs, parse_package(token))
        else
            cmderror("free only takes a list of packages")
        end
    end
    API.free(ctx, pkgs)
end

function do_status!(ctx::Context, tokens::Vector{Token})
    mode = PKGMODE_COMBINED
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa Option
            if token.kind in (OPT_PROJECT, OPT_MANIFEST)
                mode = PackageMode(token.kind)
            else
                cmderror("invalid option for `status`: $(token)")
            end
        else
            cmderror("`status` does not take arguments")
        end
    end
    Display.status(ctx, mode)
end

# TODO , test recursive dependencies as on option.
function do_test!(ctx::Context, tokens::Vector{Token})
    pkgs = PackageSpec[]
    coverage = false
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa String
            pkg = parse_package(token)
            pkg.mode = PKGMODE_MANIFEST
            push!(pkgs, pkg)
        elseif token isa Option
            if token.kind == OPT_COVERAGE
                coverage = true
            else
                cmderror("invalid option for `test`: $token")
            end
        else
            # TODO: Better error message
            cmderror("invalid usage for `test`")
        end
    end
    API.test(ctx, pkgs; coverage = coverage)
end

function do_gc!(ctx::Context, tokens::Vector{Token})
    !isempty(tokens) && cmderror("`gc` does not take any arguments")
    API.gc(ctx)
end

function do_build!(ctx::Context, tokens::Vector{Token})
    pkgs = PackageSpec[]
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa String
            push!(pkgs, parse_package(token))
        else
            cmderror("`build` only takes a list of packages")
        end
    end
    API.build(ctx, pkgs)
end

function do_generate!(ctx::Context, tokens::Vector{Token})
    isempty(tokens) && cmderror("`generate` requires a project name as an argument")
    token = popfirst!(tokens)
    token isa String || cmderror("`generate` takes a name of the project to create")
    isempty(tokens) || cmderror("`generate` takes a single project name as an argument")
    API.generate(ctx, token)
end

function do_precompile!(ctx::Context, tokens::Vector{Token})
    if !isempty(tokens)
        cmderror("`precompile` does not take any arguments")
    end
    API.precompile(ctx)
end

function do_instantiate!(ctx::Context, tokens::Vector{Token})
    manifest = nothing
    for token in tokens
        if token isa Option
            if token.kind == OPT_MANIFEST
                manifest = true
            elseif token.kind == OPT_PROJECT
            manifest = false
            else
                cmderror("invalid option for `instantiate`: $(token)")
            end
        else
            cmderror("invalid argument for `instantiate` :$(token)")
        end
    end
    API.instantiate(ctx; manifest=manifest)
end

function do_resolve!(ctx::Context, tokens::Vector{Token})
    !isempty(tokens) && cmderror("`resolve` does not take any arguments")
    API.resolve(ctx)
end

function do_activate!(env::Union{EnvCache,Nothing}, tokens::Vector{Token})
    if isempty(tokens)
        return API.activate()
    else
        path = popfirst!(tokens)
        if !isempty(tokens) || !(path isa String)
            cmderror("`activate` takes an optional path to the env to activate")
        end
        devpath = nothing
        if env !== nothing && haskey(env.project["deps"], path)
            uuid = UUID(env.project["deps"][path])
            info = manifest_info(env, uuid)
            devpath = haskey(info, "path") ? joinpath(dirname(env.project_file), info["path"]) : nothing
        end
        # `pkg> activate path` does the following
        # 1. if path exists, activate that
        # 2. if path exists in deps, and the dep is deved, activate that path (`devpath` above)
        # 3. activate the non-existing directory (e.g. as in `pkg> activate .` for initing a new env)
        if Types.isdir_windows_workaround(path)
            API.activate(abspath(path))
        elseif devpath !== nothing
            API.activate(abspath(devpath))
        else
            API.activate(abspath(path))
        end
    end
end

######################
# REPL mode creation #
######################

# Provide a string macro pkg"cmd" that can be used in the same way
# as the REPLMode `pkg> cmd`. Useful for testing and in environments
# where we do not have a REPL, e.g. IJulia.
struct MiniREPL <: REPL.AbstractREPL
    display::TextDisplay
    t::REPL.Terminals.TTYTerminal
end
function MiniREPL()
    MiniREPL(TextDisplay(stdout), REPL.Terminals.TTYTerminal(get(ENV, "TERM", Sys.iswindows() ? "" : "dumb"), stdin, stdout, stderr))
end
REPL.REPLDisplay(repl::MiniREPL) = repl.display

__init__() = minirepl[] = MiniREPL()

const minirepl = Ref{MiniREPL}()

macro pkg_str(str::String)
    :($(do_cmd)(minirepl[], $str; do_rethrow=true))
end

pkgstr(str::String) = do_cmd(minirepl[], str; do_rethrow=true)

# handle completions
all_commands_sorted = sort!(collect(keys(cmds)))
long_commands = filter(c -> length(c) > 2, all_commands_sorted)
all_options_sorted = [length(opt) > 1 ? "--$opt" : "-$opt" for opt in sort!(collect(keys(opts)))]
long_options = filter(c -> length(c) > 2, all_options_sorted)

struct PkgCompletionProvider <: LineEdit.CompletionProvider end

function LineEdit.complete_line(c::PkgCompletionProvider, s)
    partial = REPL.beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = completions(full, lastindex(partial))
    return ret, partial[range], should_complete
end

function complete_command(s, i1, i2)
    # only show short form commands when no input is given at all
    cmp = filter(cmd -> startswith(cmd, s), isempty(s) ? all_commands_sorted : long_commands)
    return cmp, i1:i2, !isempty(cmp)
end

function complete_option(s, i1, i2)
    # only show short form options if only a dash is given
    cmp = filter(cmd -> startswith(cmd, s), length(s) == 1 && first(s) == '-' ?
                                                all_options_sorted :
                                                long_options)
    return cmp, i1:i2, !isempty(cmp)
end

function complete_package(s, i1, i2, lastcommand, project_opt)
    if lastcommand in [CMD_STATUS, CMD_RM, CMD_UP, CMD_TEST, CMD_BUILD, CMD_FREE, CMD_PIN, CMD_CHECKOUT]
        return complete_installed_package(s, i1, i2, project_opt)
    elseif lastcommand in [CMD_ADD, CMD_DEVELOP]
        return complete_remote_package(s, i1, i2)
    end
    return String[], 0:-1, false
end

function complete_installed_package(s, i1, i2, project_opt)
    pkgs = project_opt ? API.installed(PKGMODE_PROJECT) : API.installed()
    pkgs = sort!(collect(keys(filter((p) -> p[2] != nothing, pkgs))))
    cmp = filter(cmd -> startswith(cmd, s), pkgs)
    return cmp, i1:i2, !isempty(cmp)
end

function complete_remote_package(s, i1, i2)
    cmp = String[]
    julia_version = VERSION
    for reg in Types.registries(;clone_default=false)
        data = Types.read_registry(joinpath(reg, "Registry.toml"))
        for (uuid, pkginfo) in data["packages"]
            name = pkginfo["name"]
            if startswith(name, s)
                compat_data = Operations.load_package_data_raw(
                    VersionSpec, joinpath(reg, pkginfo["path"], "Compat.toml"))
                supported_julia_versions = VersionSpec(VersionRange[])
                for (ver_range, compats) in compat_data
                    for (compat, v) in compats
                        if compat == "julia"
                            union!(supported_julia_versions, VersionSpec(v))
                        end
                    end
                end
                if VERSION in supported_julia_versions
                    push!(cmp, name)
                end
            end
        end
    end
    return cmp, i1:i2, !isempty(cmp)
end

function completions(full, index)
    pre = full[1:index]

    pre_words = split(pre, ' ', keepempty=true)

    # first word should always be a command
    if isempty(pre_words)
        return complete_command("", 1:1)
    else
        to_complete = pre_words[end]
        offset = isempty(to_complete) ? index+1 : to_complete.offset+1

        if length(pre_words) == 1
            return complete_command(to_complete, offset, index)
        end

        # tokenize input, don't offer any completions for invalid commands
        tokens = try
            tokenize(join(pre_words[1:end-1], ' '))[end]
        catch
            return String[], 0:-1, false
        end

        tokens = reverse!(tokens)

        lastcommand = nothing
        project_opt = true
        for t in tokens
            if t isa Command
                lastcommand = t.kind
                break
            end
        end
        for t in tokens
            if t isa Option && t.kind in [OPT_PROJECT, OPT_MANIFEST]
                project_opt = t.kind == OPT_PROJECT
                break
            end
        end

        if lastcommand in [CMD_HELP, CMD_PREVIEW]
            return complete_command(to_complete, offset, index)
        elseif !isempty(to_complete) && first(to_complete) == '-'
            return complete_option(to_complete, offset, index)
        else
            return complete_package(to_complete, offset, index, lastcommand, project_opt)
        end
    end
end

prev_project_file = nothing
prev_project_timestamp = nothing
prev_prefix = ""

function promptf()
    global prev_project_timestamp, prev_prefix, prev_project_file
    project_file = try
        project_file = Base.active_project()
    catch
        nothing
    end
    prefix = ""
    if project_file !== nothing
        if prev_project_file == project_file && prev_project_timestamp == mtime(project_file)
            prefix = prev_prefix
        else
            project = try
                Types.read_project(project_file)
            catch
                nothing
            end
            if project !== nothing
                proj_dir = ispath(project_file) ? realpath(project_file) : project_file
                proj_dir = dirname(proj_dir)
                projname = get(project, "name", nothing)
                if startswith(pwd(), proj_dir) && projname !== nothing
                    name = projname
                else
                    name = basename(proj_dir)
                end
                prefix = string("(", name, ") ")
                prev_prefix = prefix
                prev_project_timestamp = mtime(project_file)
                prev_project_file = project_file
            end
        end
    end
    return prefix * "pkg> "
end

# Set up the repl Pkg REPLMode
function create_mode(repl, main)
    pkg_mode = LineEdit.Prompt(promptf;
        prompt_prefix = Base.text_colors[:blue],
        prompt_suffix = "",
        complete = PkgCompletionProvider(),
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

    shell_mode = nothing
    for mode in Base.active_repl.interface.modes
        if mode isa LineEdit.Prompt
            mode.prompt == "shell> " && (shell_mode = mode)
        end
    end

    repl_keymap = Dict()
    if shell_mode != nothing
        repl_keymap[';'] = function (s,o...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                LineEdit.transition(s, shell_mode) do
                    LineEdit.state(s, shell_mode).input_buffer = buf
                end
            else
                LineEdit.edit_insert(s, ';')
            end
        end
    end

    b = Dict{Any,Any}[
        skeymap, repl_keymap, mk, prefix_keymap, LineEdit.history_keymap,
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
