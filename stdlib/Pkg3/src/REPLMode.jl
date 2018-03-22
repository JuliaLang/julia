module REPLMode

using Markdown
using UUIDs

import REPL
import REPL: LineEdit, REPLCompletions

import ..devdir, ..print_first_command_header, ..API
using ..Types, ..Display, ..Operations

############
# Commands #
############
@enum(CommandKind, CMD_HELP, CMD_STATUS, CMD_SEARCH, CMD_ADD, CMD_RM, CMD_UP,
                   CMD_TEST, CMD_GC, CMD_PREVIEW, CMD_INIT, CMD_BUILD, CMD_FREE,
                   CMD_PIN, CMD_CHECKOUT, CMD_DEVELOP, CMD_GENERATE)

struct Command
    kind::CommandKind
    val::String
end
Base.show(io::IO, cmd::Command) = print(io, cmd.val)

const cmds = Dict(
    "help"      => CMD_HELP,
    "?"         => CMD_HELP,
    "status"    => CMD_STATUS,
    "st"        => CMD_STATUS,
    "."         => CMD_STATUS,
    "search"    => CMD_SEARCH,
    "find"      => CMD_SEARCH,
    "/"         => CMD_SEARCH,
    "add"       => CMD_ADD,
    "install"   => CMD_ADD,
    "+"         => CMD_ADD,
    "rm"        => CMD_RM,
    "remove"    => CMD_RM,
    "uninstall" => CMD_RM,
    "-"         => CMD_RM,
    "up"        => CMD_UP,
    "update"    => CMD_UP,
    "upgrade"   => CMD_UP,
    "test"      => CMD_TEST,
    "gc"        => CMD_GC,
    "preview"   => CMD_PREVIEW,
    "init"      => CMD_INIT,
    "build"     => CMD_BUILD,
    "pin"       => CMD_PIN,
    "free"      => CMD_FREE,
    "checkout"  => CMD_CHECKOUT, # deprecated
    "develop"   => CMD_DEVELOP,
    "dev"       => CMD_DEVELOP,
    "generate"  => CMD_GENERATE,
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
                  OPT_PATCH, OPT_FIXED, OPT_COVERAGE, OPT_NAME, OPT_PATH)

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
        elseif kind in (OPT_ENV, OPT_PATH) && argument == nothing
            cmderror("the `$val` option requires an argument")
        end
        if kind == OPT_PATH
            argument =  replace(argument, "~" => homedir())
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
    "path"     => OPT_PATH,
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

function parse_package(word::AbstractString; context=nothing)# ::PackageSpec
    word = replace(word, "~" => homedir())
    if context in (CMD_ADD, CMD_DEVELOP) && isdir(word)
        pkg = PackageSpec()
        pkg.repo = Types.GitRepo(word)
        return pkg
    elseif occursin(uuid_re, word)
        return PackageSpec(UUID(word))
    elseif occursin(name_re, word)
        return PackageSpec(String(match(name_re, word).captures[1]))
    elseif occursin(name_uuid_re, word)
        m = match(name_uuid_re, word)
        return PackageSpec(String(m.captures[1]), UUID(m.captures[2]))
    else
        if context in (CMD_ADD, CMD_DEVELOP)
            # Guess it is a url then
            pkg = PackageSpec()
            pkg.repo = Types.GitRepo(word)
            return pkg
        else
            cmderror("`$word` cannot be parsed as a package")
        end
    end
end

################
# REPL parsing #
################
const lex_re = r"^[\?\./\+\-](?!\-) | [^@\#\s]+\s*=\s*[^@\#\s]+ | \#\s*[^@\#\s]* | @\s*[^@\#\s]* | [^@\#\s]+"x

const Token = Union{Command, Option, VersionRange, String, Rev}

function tokenize(cmd::String)::Vector{Token}
    print_first_command_header()
    tokens = Token[]
    words = map(m->m.match, eachmatch(lex_re, cmd))
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
        if first(word) == '-'
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
        tokens = tokenize(input)
        do_cmd!(tokens, repl)
    catch err
        if do_rethrow
            rethrow(err)
        end
        if err isa CommandError
            Base.display_error(repl.t.err_stream, ErrorException(err.msg), Ptr{Nothing}[])
        else
            Base.display_error(repl.t.err_stream, err, Base.catch_backtrace())
        end
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
    ctx = Context(env = EnvCache(env_opt))
    if cmd.kind == CMD_PREVIEW
        ctx.preview = true
        isempty(tokens) && cmderror("expected a command to preview")
        cmd = popfirst!(tokens)
    end
    # Using invokelatest to hide the functions from inference.
    # Otherwise it would try to infer everything here.
    cmd.kind == CMD_INIT     ? Base.invokelatest(          do_init!, ctx, tokens) :
    cmd.kind == CMD_HELP     ? Base.invokelatest(          do_help!, ctx, tokens, repl) :
    cmd.kind == CMD_RM       ? Base.invokelatest(            do_rm!, ctx, tokens) :
    cmd.kind == CMD_ADD      ? Base.invokelatest(do_add_or_develop!, ctx, tokens, CMD_ADD) :
    cmd.kind == CMD_CHECKOUT ? Base.invokelatest(do_add_or_develop!, ctx, tokens, CMD_DEVELOP) :
    cmd.kind == CMD_DEVELOP  ? Base.invokelatest(do_add_or_develop!, ctx, tokens, CMD_DEVELOP) :
    cmd.kind == CMD_UP       ? Base.invokelatest(            do_up!, ctx, tokens) :
    cmd.kind == CMD_STATUS   ? Base.invokelatest(        do_status!, ctx, tokens) :
    cmd.kind == CMD_TEST     ? Base.invokelatest(          do_test!, ctx, tokens) :
    cmd.kind == CMD_GC       ? Base.invokelatest(            do_gc!, ctx, tokens) :
    cmd.kind == CMD_BUILD    ? Base.invokelatest(         do_build!, ctx, tokens) :
    cmd.kind == CMD_PIN      ? Base.invokelatest(           do_pin!, ctx, tokens) :
    cmd.kind == CMD_FREE     ? Base.invokelatest(          do_free!, ctx, tokens) :
    cmd.kind == CMD_GENERATE ? Base.invokelatest(      do_generate!, ctx, tokens) :
        cmderror("`$cmd` command not yet implemented")
    return
end

const help = md"""

**Welcome to the Pkg3 REPL-mode**. To return to the `julia>` prompt, either press
backspace when the input line is empty or press Ctrl+C.


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

`generate`: generate files for a new project

`add`: add packages to project

`rm`: remove packages from project or manifest

`up`: update packages in manifest

`preview`: previews a subsequent command without affecting the current state

`test`: run tests for packages

`gc`: garbage collect packages not used for a significant time

`init`: initializes an environment in the current, or git base, directory

`build`: run the build script for packages

`pin`: pins the version of packages

`develop`: clone the full package repo locally for development

`free`: undos a `pin` or `develop`
"""

const helps = Dict(
    CMD_HELP => md"""

        help

    Display this message.

        help cmd ...

    Display usage information for commands listed.

    Available commands: `help`, `status`, `add`, `rm`, `up`, `preview`, `gc`, `test`, `init`, `build`, `free`, `pin`, `develop`.
    """, CMD_STATUS => md"""

        status
        status [-p|--project]
        status [-m|--manifest]

    Show the status of the current environment. By default, the full contents of
    the project file is summarized, showing what version each package is on and
    how it has changed since the last git commit (if in a git repo), as well as
    any changes to manifest packages not already listed. In `--project` mode, the
    status of the project file is summarized. In `--project` mode, the status of
    the project file is summarized.
    """, CMD_GENERATE => md"""

        create name

    Create a project called `name` in the current folder.
    """,
    CMD_ADD => md"""

        add pkg[=uuid] [@version] [#rev] ...

    Add package `pkg` to the current project file. If `pkg` could refer to
    multiple different packages, specifying `uuid` allows you to disambiguate.
    `@version` optionally allows specifying which versions of packages. Versions
    may be specified by `@1`, `@1.2`, `@1.2.3`, allowing any version with a prefix
    that matches, or ranges thereof, such as `@1.2-3.4.5`. A git-revision can be
    specified by `#branch` or `#commit`.

    **Examples**
    ```
    pkg> add Example
    pkg> add Example@0.5
    pkg> add Example#master
    pkg> add Example#c37b675
    pkg> add https://github.com/JuliaLang/Example.jl#master
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

    Deletes packages that are not reached from any environment used within the last 6 weeks.
    """, CMD_INIT => md"""

        init

    Creates an environment in the current directory, or the git base directory if the current directory
    is in a git repository.
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
        develop pkg[=uuid] [#rev] ...

    Make a package available for development. If `pkg` is an existing local path that path will be recorded in
    the manifest and used. Otherwise, a full git clone of `pkg` at rev `rev` is made. The clone is stored in `devdir`,
    which defaults to `~/.julia/dev` and is set by the environment variable `JULIA_PKG_DEVDIR`.
    This operation is undone by `free`.

    *Example*
    ```jl
    pkg> develop Example
    pkg> develop Example#master
    pkg> develop Example#c37b675
    pkg> develop https://github.com/JuliaLang/Example.jl#master
    ```
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
    # tokens: package names and/or uuids, optionally followed by version specs
    isempty(tokens) &&
        cmderror("`add` – list packages to add")
    pkgs = PackageSpec[]
    prev_token_was_package = false
    while !isempty(tokens)
        parsed_package = false
        token = popfirst!(tokens)
        if token isa String
            push!(pkgs, parse_package(token; context=CMD_ADD))
            cmd == CMD_DEVELOP && pkgs[end].repo == nothing && (pkgs[end].repo = Types.GitRepo("", ""))
            parsed_package = true
        elseif token isa VersionRange
            prev_token_was_package ||
                cmderror("package name/uuid must precede version spec `@$token`")
            pkgs[end].version = VersionSpec(token)
        elseif token isa Rev
            prev_token_was_package ||
                cmderror("package name/uuid must precede rev spec `#$(token.rev)`")
            # WE did not get the repo from the
            pkg = pkgs[end]
            if pkg.repo == nothing
                pkg.repo = Types.GitRepo("", token.rev)
            else
                pkgs[end].repo.rev = token.rev
            end
        elseif token isa Option
            cmderror("`add` doesn't take options: $token")
        end
        prev_token_was_package = parsed_package
    end
    return API.add_or_develop(ctx, pkgs, mode=(cmd == CMD_ADD ? :add : :develop))
end

function do_up!(ctx::Context, tokens::Vector{Token})
    # tokens:
    #  - upgrade levels as options: --[fixed|patch|minor|major]
    #  - package names and/or uuids, optionally followed by version specs
    pkgs = PackageSpec[]
    mode = PKGMODE_PROJECT
    level = UPLEVEL_MAJOR
    prev_token_was_package = false
    while !isempty(tokens)
        parsed_package = false
        token = popfirst!(tokens)
        if token isa String
            push!(pkgs, parse_package(token))
            pkgs[end].version = level
            pkgs[end].mode = mode
            parsed_package = true
        elseif token isa VersionRange
            prev_token_was_package ||
                cmderror("package name/uuid must precede version spec `@$token`")
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
        prev_token_was_package = parsed_package
    end
    API.up(ctx, pkgs; level=level, mode=mode)
end

function do_pin!(ctx::Context, tokens::Vector{Token})
    pkgs = PackageSpec[]
    prev_token_was_package = false
    while !isempty(tokens)
        token = popfirst!(tokens)
        parsed_package = false
        if token isa String
            push!(pkgs, parse_package(token))
            parsed_package = true
        elseif token isa VersionRange
            prev_token_was_package ||
                cmderror("package name/uuid must precede version spec `@$token`")
            if token.lower != token.upper
                cmderror("pinning a package requires a single version, not a versionrange")
            end
            pkgs[end].version = VersionSpec(token)
        else
            cmderror("free only takes a list of packages ")
        end
        prev_token_was_package = parsed_package
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

function do_checkout!(ctx::Context, tokens::Vector{Token})
    Base.depwarn("`checkout`` is deprecated, use `develop`", :checkout)
    do_develop!(ctx, tokens)
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

function do_init!(ctx::Context, tokens::Vector{Token})
    if !isempty(tokens)
        cmderror("`init` does currently not take any arguments")
    end
    API.init(ctx)
end

function do_generate!(ctx::Context, tokens::Vector{Token})
    local pkg
    while !isempty(tokens)
        token = popfirst!(tokens)
        if token isa String
            pkg = token
            break # TODO: error message?
        else
            cmderror("`generate` takes a name of the project to create")
        end
    end
    API.generate(pkg)
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
    if lastcommand in [CMD_STATUS, CMD_RM, CMD_UP, CMD_TEST, CMD_BUILD, CMD_FREE, CMD_PIN, CMD_CHECKOUT, CMD_DEVELOP]
        return complete_installed_package(s, i1, i2, project_opt)
    elseif lastcommand in [CMD_ADD]
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
    cmp = filter(cmd -> startswith(cmd, s), collect_package_names())
    return cmp, i1:i2, !isempty(cmp)
end

function collect_package_names()
    r = r"name = \"(.*?)\""
    names = String[]
    for reg in Types.registries(;clone_default=false)
        regcontent = read(joinpath(reg, "Registry.toml"), String)
        append!(names, collect(match.captures[1] for match in eachmatch(r, regcontent)))
    end
    return sort!(names)
end

function completions(full, index)
    pre = full[1:index]

    pre_words = split(pre, ' ', keep=true)

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
            tokenize(join(pre_words[1:end-1], ' '))
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

# Set up the repl Pkg REPLMode
function create_mode(repl, main)
    pkg_mode = LineEdit.Prompt("pkg> ";
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
