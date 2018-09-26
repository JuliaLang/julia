# This file is a part of Julia. License is MIT: https://julialang.org/license

module REPLMode

using Markdown
using UUIDs

import REPL
import REPL: LineEdit, REPLCompletions

import ..devdir, ..Types.casesensitive_isdir, ..TOML
using ..Types, ..Display, ..Operations, ..API

#################
# Git revisions #
#################
struct Rev
    rev::String
end

###########
# Options #
###########
struct OptionSpec
    name::String
    short_name::Union{Nothing,String}
    api::Pair{Symbol, Any}
    is_switch::Bool
end

@enum(OptionClass, OPT_ARG, OPT_SWITCH)
const OptionDeclaration = Tuple{Union{String,Vector{String}}, # name + short_name?
                                OptionClass, # arg or switch
                                Pair{Symbol, Any} # api keywords
                                }

function OptionSpec(x::OptionDeclaration)::OptionSpec
    get_names(name::String) = (name, nothing)
    function get_names(names::Vector{String})
        @assert length(names) == 2
        return (names[1], names[2])
    end

    is_switch = x[2] == OPT_SWITCH
    api = x[3]
    (name, short_name) = get_names(x[1])
    #TODO assert matching lex regex
    if !is_switch
        @assert api.second === nothing || hasmethod(api.second, Tuple{String})
    end
    return OptionSpec(name, short_name, api, is_switch)
end

function OptionSpecs(decs::Vector{OptionDeclaration})::Dict{String, OptionSpec}
    specs = Dict()
    for x in decs
        opt_spec = OptionSpec(x)
        @assert get(specs, opt_spec.name, nothing) === nothing # don't overwrite
        specs[opt_spec.name] = opt_spec
        if opt_spec.short_name !== nothing
            @assert get(specs, opt_spec.short_name, nothing) === nothing # don't overwrite
            specs[opt_spec.short_name] = opt_spec
        end
    end
    return specs
end

struct Option
    val::String
    argument::Union{Nothing,String}
    Option(val::AbstractString) = new(val, nothing)
    Option(val::AbstractString, arg::Union{Nothing,String}) = new(val, arg)
end
Base.show(io::IO, opt::Option) = print(io, "--$(opt.val)", opt.argument == nothing ? "" : "=$(opt.argument)")

function parse_option(word::AbstractString)::Option
    m = match(r"^(?: -([a-z]) | --([a-z]{2,})(?:\s*=\s*(\S*))? )$"ix, word)
    m == nothing && pkgerror("malformed option: ", repr(word))
    option_name = (m.captures[1] != nothing ? m.captures[1] : m.captures[2])
    option_arg = (m.captures[3] == nothing ? nothing : String(m.captures[3]))
    return Option(option_name, option_arg)
end

meta_option_declarations = OptionDeclaration[
    ("preview", OPT_SWITCH, :preview => true)
]
meta_option_specs = OptionSpecs(meta_option_declarations)

################
# Command Spec #
################
@enum(CommandKind, CMD_HELP, CMD_RM, CMD_ADD, CMD_DEVELOP, CMD_UP,
                   CMD_STATUS, CMD_TEST, CMD_GC, CMD_BUILD, CMD_PIN,
                   CMD_FREE, CMD_GENERATE, CMD_RESOLVE, CMD_PRECOMPILE,
                   CMD_INSTANTIATE, CMD_ACTIVATE, CMD_PREVIEW,
                   CMD_REGISTRY_ADD,
                   )
@enum(ArgClass, ARG_RAW, ARG_PKG, ARG_VERSION, ARG_REV, ARG_ALL)
struct ArgSpec
    count::Pair
    parser::Function
    parser_keys::Vector{Pair{Symbol, Any}}
end
const CommandDeclaration = Tuple{CommandKind,
                                 Vector{String}, # names
                                 Union{Nothing,Function}, # handler
                                 Tuple{Pair, # count
                                       Function, # parser
                                       Vector{Pair{Symbol, Any}}, # parser keys
                                       }, # arguments
                                 Vector{OptionDeclaration}, # options
                                 String, #description
                                 Union{Nothing, Markdown.MD}, #help
                                 }
struct CommandSpec
    kind::CommandKind
    canonical_name::String
    short_name::Union{Nothing,String}
    handler::Union{Nothing,Function}
    argument_spec::ArgSpec
    option_specs::Dict{String, OptionSpec}
    description::String
    help::Union{Nothing, Markdown.MD}
end
command_specs = Dict{String,CommandSpec}() # TODO remove this ?

function SuperSpecs(foo)::Dict{String,Dict{String,CommandSpec}}
    super_specs = Dict()
    for x in foo
        sub_specs = CommandSpecs(x.second)
        for name in x.first
            @assert get(super_specs, name, nothing) === nothing
            super_specs[name] = sub_specs
        end
    end
    return super_specs
end

# populate a dictionary: command_name -> command_spec
function CommandSpecs(declarations::Vector{CommandDeclaration})::Dict{String,CommandSpec}
    specs = Dict()
    for dec in declarations
        names = dec[2]
        spec = CommandSpec(dec[1],
                           names[1],
                           length(names) == 2 ? names[2] : nothing,
                           dec[3],
                           ArgSpec(dec[4]...),
                           OptionSpecs(dec[5]),
                           dec[6],
                           dec[end])
        for name in names
            # TODO regex check name
            @assert get(specs, name, nothing) === nothing
            specs[name] = spec
        end
    end
    return specs
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
        pkgerror("`$word` cannot be parsed as a package")
    end
end

################
# REPL parsing #
################
mutable struct Statement
    command::Union{Nothing,CommandSpec}
    options::Vector{String}
    arguments::Vector{String}
    meta_options::Vector{String}
    Statement() = new(nothing, [], [], [])
end

struct QuotedWord
    word::String
    isquoted::Bool
end

function unwrap_option(option::String)
    if startswith(option, "--")
        return length(option) == 2 ? "" : option[3:end]
    elseif length(option) == 2
        return option[end]
    end
end

wrap_option(option::String) =
    length(option) == 1 ? "-$option" : "--$option"

function _statement(words)
    is_option(word) = first(word) == '-'

    word = popfirst!(words)
    # meta options
    while is_option(word)
        if isempty(words)
            if unwrap_option(word) in keys(meta_option_specs)
                return :cmd, "", nothing, true
            else
                return :meta, word, nothing, true
            end
        end
        word = popfirst!(words)
    end
    # command
    if word == "preview"
        if isempty(words)
            return :cmd, "", nothing, true
        end
        word = popfirst!(words)
    end
    if word in keys(super_specs) # have a super command
        super_name = word
        super = super_specs[word]
        if isempty(words)
            return :sub, "", super_name, true
        end
        word = popfirst!(words)
        command = get(super, word, nothing)
        if command === nothing
            if isempty(words)
                return :sub, word, super_name, true
            else
                return nothing
            end
        end
    elseif get(super_specs["package"], word, nothing) !== nothing # given a "package" command
        command = get(super_specs["package"], word, nothing)
    elseif isempty(words) # try to complete the super command
        return :cmd, word, nothing, true
    else
        return nothing
    end
    if isempty(words)
        return :arg, "", command, true
    end
    word = words[end]
    manifest = any(x->x in ["--manifest", "-m"], filter(is_option, words))
    return is_option(word) ?
        (:opt, word, command, true) :
        (:arg, word, command, !manifest)
end

function parse(cmd::String; for_completions=false)
    # replace new lines with ; to support multiline commands
    cmd = replace(replace(cmd, "\r\n" => "; "), "\n" => "; ")
    # tokenize accoring to whitespace / quotes
    qwords = parse_quotes(cmd)
    # tokenzie unquoted tokens according to pkg REPL syntax
    words = lex(qwords)
    # break up words according to ";"(doing this early makes subsequent processing easier)
    word_groups = group_words(words)
    # create statements
    if for_completions
        return _statement(word_groups[end])
    end
    return map(Statement, word_groups)
end

# vector of words -> structured statement
# minimal checking is done in this phase
function Statement(words)::Statement
    is_option(word) = first(word) == '-'
    statement = Statement()

    word = popfirst!(words)
    # meta options
    while is_option(word)
        push!(statement.meta_options, word)
        isempty(words) && pkgerror("no command specified")
        word = popfirst!(words)
    end
    # command
    # special handling for `preview`, just convert it to a meta option under the hood
    if word == "preview"
        if !("--preview" in statement.meta_options)
            push!(statement.meta_options, "--preview")
        end
        isempty(words) && pkgerror("preview requires a command")
        word = popfirst!(words)
    end
    if word in keys(super_specs)
        super = super_specs[word]
        isempty(words) && pkgerror("no subcommand specified")
        word = popfirst!(words)
    else
        super = super_specs["package"]
    end
    command = get(super, word, nothing)
    command !== nothing || pkgerror("expected command. instead got [$word]")
    statement.command = command
    # command arguments
    for word in words
        push!((is_option(word) ? statement.options : statement.arguments), word)
    end
    return statement
end

# break up words according to `;`(doing this early makes subsequent processing easier)
# the final group does not require a trailing `;`
function group_words(words)::Vector{Vector{String}}
    statements = Vector{String}[]
    x = String[]
    for word in words
        if word == ";"
            isempty(x) ? pkgerror("empty statement") : push!(statements, x)
            x = String[]
        else
            push!(x, word)
        end
    end
    isempty(x) || push!(statements, x)
    return statements
end

const lex_re = r"^[\?\./\+\-](?!\-) | ((git|ssh|http(s)?)|(git@[\w\-\.]+))(:(//)?)([\w\.@\:/\-~]+)(\.git)(/)? | [^@\#\s;]+\s*=\s*[^@\#\s;]+ | \#\s*[^@\#\s;]* | @\s*[^@\#\s;]* | [^@\#\s;]+|;"x

function lex(qwords::Vector{QuotedWord})::Vector{String}
    words = String[]
    for qword in qwords
        if qword.isquoted
            push!(words, qword.word)
        else
            append!(words, map(m->m.match, eachmatch(lex_re, qword.word)))
        end
    end
    return words
end

function parse_quotes(cmd::String)::Vector{QuotedWord}
    in_doublequote = false
    in_singlequote = false
    qwords = QuotedWord[]
    token_in_progress = Char[]

    push_token!(is_quoted) = begin
        push!(qwords, QuotedWord(String(token_in_progress), is_quoted))
        empty!(token_in_progress)
    end

    for c in cmd
        if c == '"'
            if in_singlequote # raw char
                push!(token_in_progress, c)
            else # delimiter
                in_doublequote ? push_token!(true) : push_token!(false)
                in_doublequote = !in_doublequote
            end
        elseif c == '\''
            if in_doublequote # raw char
                push!(token_in_progress, c)
            else # delimiter
                in_singlequote ? push_token!(true) : push_token!(false)
                in_singlequote = !in_singlequote
            end
        else
            push!(token_in_progress, c)
        end
    end
    if (in_doublequote || in_singlequote)
        pkgerror("unterminated quote")
    else
        push_token!(false)
    end
    # to avoid complexity in the main loop, empty tokens are allowed above and
    # filtered out before returning
    return filter(x->!isempty(x.word), qwords)
end

##############
# PkgCommand #
##############
const Token = Union{String, VersionRange, Rev}
const ArgToken = Union{VersionRange, Rev}
const PkgToken = Union{String, VersionRange, Rev}
const PkgArguments = Union{Vector{String}, Vector{PackageSpec}}
struct PkgCommand
    meta_options::Vector{Option}
    spec::CommandSpec
    options::Vector{Option}
    arguments::PkgArguments
    PkgCommand() = new([], "", [], [])
    PkgCommand(meta_opts, cmd_name, opts, args) = new(meta_opts, cmd_name, opts, args)
end

const APIOptions = Dict{Symbol, Any}
APIOptions(command::PkgCommand)::Dict{Symbol, Any} =
    APIOptions(command.options, command.spec.option_specs)

function APIOptions(options::Vector{Option},
                    specs::Dict{String, OptionSpec},
                    )::Dict{Symbol, Any}
    keyword_vec = map(options) do opt
        spec = specs[opt.val]
        # opt is switch
        spec.is_switch && return spec.api
        # no opt wrapper -> just use raw argument
        spec.api.second === nothing && return spec.api.first => opt.argument
        # given opt wrapper
        return spec.api.first => spec.api.second(opt.argument)
    end
    return Dict(keyword_vec)
end

function enforce_argument_count(spec::Pair, args::PkgArguments)
    count = length(args)
    spec.first <= count <= spec.second ||
        pkgerror("Wrong number of arguments")
end

# Only for PkgSpec
function package_args(args::Vector{Token}; add_or_dev=false)::Vector{PackageSpec}
    pkgs = PackageSpec[]
    for arg in args
        if arg isa String
            push!(pkgs, parse_package(arg; add_or_develop=add_or_dev))
        elseif arg isa VersionRange
            pkgs[end].version = VersionSpec(arg)
        elseif arg isa Rev
            pkg = pkgs[end]
            if pkg.repo == nothing
                pkg.repo = Types.GitRepo("", arg.rev)
            else
                pkgs[end].repo.rev = arg.rev
            end
        else
            assert(false)
        end
    end
    return pkgs
end

# Only for PkgSpec
function word2token(word::AbstractString)::Token
    if first(word) == '@'
        return VersionRange(word[2:end])
    elseif first(word) == '#'
        return Rev(word[2:end])
    else
        return String(word)
    end
end

# Only for PkgSpec
function enforce_argument_order(args::Vector{Token})
    prev_arg = nothing
    function check_prev_arg(valid_type::DataType, error_message::AbstractString)
        prev_arg isa valid_type || pkgerror(error_message)
    end

    for arg in args
        if arg isa VersionRange
            check_prev_arg(String, "package name/uuid must precede version spec `@$arg`")
        elseif arg isa Rev
            check_prev_arg(String, "package name/uuid must precede rev spec `#$(arg.rev)`")
        end
        prev_arg = arg
    end
end

function parse_pkg(raw_args::Vector{String}; valid=[], add_or_dev=false)
    args::Vector{PkgToken} = map(word2token, raw_args)
    enforce_argument_order(args)
    # enforce spec
    push!(valid, String) # always want at least PkgSpec identifiers
    if !all(x->typeof(x) in valid, args)
        pkgerror("invalid token")
    end
    # convert to final arguments
    return package_args(args; add_or_dev=add_or_dev)
end

function enforce_argument(raw_args::Vector{String}, spec::ArgSpec)::PkgArguments
    args = spec.parser(raw_args; spec.parser_keys...)
    enforce_argument_count(spec.count, args)
    return args
end

function enforce_option(option::String, specs::Dict{String,OptionSpec})::Option
    opt = parse_option(option)
    spec = get(specs, opt.val, nothing)
    spec !== nothing ||
        pkgerror("option '$(opt.val)' is not a valid option")
    if spec.is_switch
        opt.argument === nothing ||
            pkgerror("option '$(opt.val)' does not take an argument, but '$(opt.argument)' given")
    else # option takes an argument
        opt.argument !== nothing ||
            pkgerror("option '$(opt.val)' expects an argument, but no argument given")
    end
    return opt
end

function enforce_meta_options(options::Vector{String}, specs::Dict{String,OptionSpec})::Vector{Option}
    meta_opt_names = keys(specs)
    return map(options) do opt
        tok = enforce_option(opt, specs)
        tok.val in meta_opt_names ||
            pkgerror("option '$opt' is not a valid meta option.")
            #TODO hint that maybe they intended to use it as a command option
        return tok
    end
end

function enforce_opts(options::Vector{String}, specs::Dict{String,OptionSpec})::Vector{Option}
    unique_keys = Symbol[]
    get_key(opt::Option) = specs[opt.val].api.first

    # final parsing
    toks = map(x->enforce_option(x,specs),options)
    # checking
    for opt in toks
        # valid option
        opt.val in keys(specs) ||
            pkgerror("option '$(opt.val)' is not supported")
        # conflicting options
        key = get_key(opt)
        if key in unique_keys
            conflicting = filter(opt->get_key(opt) == key, toks)
            pkgerror("Conflicting options: $conflicting")
        else
            push!(unique_keys, key)
        end
    end
    return toks
end

# this the entry point for the majority of input checks
function PkgCommand(statement::Statement)::PkgCommand
    meta_opts = enforce_meta_options(statement.meta_options,
                                     meta_option_specs)
    args = enforce_argument(statement.arguments,
                            statement.command.argument_spec)
    opts = enforce_opts(statement.options, statement.command.option_specs)
    return PkgCommand(meta_opts, statement.command, opts, args)
end

Context!(ctx::APIOptions)::Context = Types.Context!(collect(ctx))

#############
# Execution #
#############
function do_cmd(repl::REPL.AbstractREPL, input::String; do_rethrow=false)
    try
        statements = parse(input)
        commands = map(PkgCommand, statements)
        for cmd in commands
            do_cmd!(cmd, repl)
        end
    catch err
        if do_rethrow
            rethrow(err)
        end
        if err isa PkgError || err isa ResolverError
            Base.display_error(repl.t.err_stream, ErrorException(sprint(showerror, err)), Ptr{Nothing}[])
        else
            Base.display_error(repl.t.err_stream, err, Base.catch_backtrace())
        end
    end
end

function do_cmd!(command::PkgCommand, repl)
    context = APIOptions(command.meta_options, meta_option_specs)

    # REPL specific commands
    if command.spec.kind == CMD_HELP
        return Base.invokelatest(do_help!, command, repl)
    end

    # API commands
    # TODO is invokelatest still needed?
    api_opts = APIOptions(command)
    if applicable(command.spec.handler, context, command.arguments, api_opts)
        Base.invokelatest(command.spec.handler, context, command.arguments, api_opts)
    else
        Base.invokelatest(command.spec.handler, command.arguments, api_opts)
    end
end

function CommandSpec(command_name::String)::Union{Nothing,CommandSpec}
    # maybe a "package" command
    spec = get(super_specs["package"], command_name, nothing)
    if spec !== nothing
        return spec
    end
    # maybe a "compound command"
    m = match(r"(\w+)-(\w+)", command_name)
    m !== nothing || (return nothing)
    super = get(super_specs, m.captures[1], nothing)
    super !== nothing || (return nothing)
    return get(super, m.captures[2], nothing)
end

function do_help!(command::PkgCommand, repl::REPL.AbstractREPL)
    disp = REPL.REPLDisplay(repl)
    if isempty(command.arguments)
        Base.display(disp, help)
        return
    end
    help_md = md""
    for arg in command.arguments
        spec = CommandSpec(arg)
        if spec === nothing
            pkgerror("'$arg' does not name a command")
        end
        spec.help === nothing &&
            pkgerror("Sorry, I don't have any help for the `$arg` command.")
        isempty(help_md.content) ||
            push!(help_md.content, md"---")
        push!(help_md.content, spec.help)
    end
    Base.display(disp, help_md)
end

# TODO set default Display.status keyword: mode = PKGMODE_COMBINED
do_status!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    Display.status(Context!(ctx), get(api_opts, :mode, PKGMODE_COMBINED))

# TODO , test recursive dependencies as on option.
function do_test!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions)
    foreach(arg -> arg.mode = PKGMODE_MANIFEST, args)
    API.test(Context!(ctx), args; collect(api_opts)...)
end

function do_registry_add!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions)
    println("This is a dummy function for now")
    println("My args are:")
    for arg in args
        println("- $arg")
    end
end

do_precompile!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.precompile(Context!(ctx))

do_resolve!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.resolve(Context!(ctx))

do_gc!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.gc(Context!(ctx); collect(api_opts)...)

do_instantiate!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.instantiate(Context!(ctx); collect(api_opts)...)

do_generate!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.generate(Context!(ctx), args[1])

do_build!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.build(Context!(ctx), args; collect(api_opts)...)

do_rm!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.rm(Context!(ctx), args; collect(api_opts)...)

do_free!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.free(Context!(ctx), args; collect(api_opts)...)

do_up!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions) =
    API.up(Context!(ctx), args; collect(api_opts)...)

function do_activate!(args::PkgArguments, api_opts::APIOptions)
    if isempty(args)
        return API.activate()
    else
        return API.activate(args[1]; collect(api_opts)...)
    end
end

function do_pin!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions)
    for arg in args
        # TODO not sure this is correct
        if arg.version.ranges[1].lower != arg.version.ranges[1].upper
            pkgerror("pinning a package requires a single version, not a versionrange")
        end
    end
    API.pin(Context!(ctx), args; collect(api_opts)...)
end

function do_add!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions)
    api_opts[:mode] = :add
    API.add_or_develop(Context!(ctx), args; collect(api_opts)...)
end

function do_develop!(ctx::APIOptions, args::PkgArguments, api_opts::APIOptions)
    api_opts[:mode] = :develop
    API.add_or_develop(Context!(ctx), args; collect(api_opts)...)
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


const minirepl = Ref{MiniREPL}()

#= __init__() = =# minirepl[] = MiniREPL()

macro pkg_str(str::String)
    :($(do_cmd)(minirepl[], $str; do_rethrow=true))
end

pkgstr(str::String) = do_cmd(minirepl[], str; do_rethrow=true)

# handle completions
mutable struct CompletionCache
    commands::Vector{String}
    canonical_names::Vector{String}
    meta_options::Vector{String}
    options::Dict{CommandKind, Vector{String}}
    subcommands::Dict{String, Vector{String}}
    CompletionCache() = new([],[],[],Dict(),Dict())
end

completion_cache = CompletionCache()

struct PkgCompletionProvider <: LineEdit.CompletionProvider end

function LineEdit.complete_line(c::PkgCompletionProvider, s)
    partial = REPL.beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = completions(full, lastindex(partial))
    return ret, partial[range], should_complete
end

function complete_local_path(s, i1, i2)
    cmp = REPL.REPLCompletions.complete_path(s, i2)
    completions = filter!(isdir, [REPL.REPLCompletions.completion_text(p) for p in cmp[1]])
    return completions, cmp[2], !isempty(completions)
end

function complete_installed_package(s, i1, i2, project_opt)
    pkgs = project_opt ? API.__installed(PKGMODE_PROJECT) : API.__installed()
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

function complete_argument(to_complete, i1, i2, lastcommand, project_opt
                           )::Tuple{Vector{String},UnitRange{Int},Bool}
    if lastcommand == CMD_HELP
        completions = filter(x->startswith(x,to_complete), completion_cache.canonical_names)
        return completions, i1:i2, !isempty(completions)
    elseif lastcommand in [CMD_STATUS, CMD_RM, CMD_UP, CMD_TEST, CMD_BUILD, CMD_FREE, CMD_PIN]
        return complete_installed_package(to_complete, i1, i2, project_opt)
    elseif lastcommand in [CMD_ADD, CMD_DEVELOP]
        if occursin(Base.Filesystem.path_separator_re, to_complete)
            return complete_local_path(to_complete, i1, i2)
        else
            rps = complete_remote_package(to_complete, i1, i2)
            lps = complete_local_path(to_complete, i1, i2)
            return vcat(rps[1], lps[1]), isempty(rps[1]) ? lps[2] : i1:i2, length(rps[1]) + length(lps[1]) > 0
        end
    end
    return String[], 0:-1, false
end

function completions(full, index)::Tuple{Vector{String},UnitRange{Int},Bool}
    pre = full[1:index]
    if isempty(pre)
        return completion_cache.commands, 0:-1, false
    end
    x = parse(pre; for_completions=true)
    if x === nothing # failed parse (invalid command name)
        return String[], 0:-1, false
    end
    (key::Symbol, to_complete::String, spec, proj::Bool) = x
    last = split(pre, ' ', keepempty=true)[end]
    offset = isempty(last) ? index+1 : last.offset+1
    if last != to_complete # require a space before completing next field
        return String[], 0:-1, false
    end
    if key == :arg
        return complete_argument(to_complete, offset, index, spec.kind, proj)
    end
    possible::Vector{String} =
        key == :meta ? completion_cache.meta_options :
        key == :cmd ? completion_cache.commands :
        key == :sub ? completion_cache.subcommands[spec] :
        key == :opt ? completion_cache.options[spec.kind] :
        String[]
    completions = filter(x->startswith(x,to_complete), possible)
    return completions, offset:index, !isempty(completions)
end

prev_project_file = nothing
prev_project_timestamp = nothing
prev_prefix = ""

function promptf()
    global prev_project_timestamp, prev_prefix, prev_project_file
    project_file = try
        Types.find_project_file()
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

########
# SPEC #
########
command_declarations = [
#=
["registry"] => CommandDeclaration[
(
    CMD_REGISTRY_ADD,
    ["add"],
    do_registry_add!,
    (1=>Inf, identity, []),
    [],
    "Currently just a placeholder for a future command",
    nothing,
),
], #registry
=#

["package"] => CommandDeclaration[
(   CMD_TEST,
    ["test"],
    do_test!,
    (0=>Inf, parse_pkg, []),
    [
        ("coverage", OPT_SWITCH, :coverage => true),
    ],
    "run tests for packages",
    md"""

    test [opts] pkg[=uuid] ...

    opts: --coverage

Run the tests for package `pkg`. This is done by running the file `test/runtests.jl`
in the package directory. The option `--coverage` can be used to run the tests with
coverage enabled. The `startup.jl` file is disabled during testing unless
julia is started with `--startup-file=yes`.
    """,
),( CMD_HELP,
    ["help", "?"],
    nothing,
    (0=>Inf, identity, []),
    [],
    "show this message",
    md"""

    help

Display this message.

    help cmd ...

Display usage information for commands listed.

Available commands: `help`, `status`, `add`, `rm`, `up`, `preview`, `gc`, `test`, `build`, `free`, `pin`, `develop`.
    """,
),( CMD_INSTANTIATE,
    ["instantiate"],
    do_instantiate!,
    (0=>0, identity, []),
    [
        (["project", "p"], OPT_SWITCH, :manifest => false),
        (["manifest", "m"], OPT_SWITCH, :manifest => true),
    ],
    "downloads all the dependencies for the project",
    md"""
    instantiate
    instantiate [-m|--manifest]
    instantiate [-p|--project]

Download all the dependencies for the current project at the version given by the project's manifest.
If no manifest exists or the `--project` option is given, resolve and download the dependencies compatible with the project.
    """,
),( CMD_RM,
    ["remove", "rm"],
    do_rm!,
    (1=>Inf, parse_pkg, []),
    [
        (["project", "p"], OPT_SWITCH, :mode => PKGMODE_PROJECT),
        (["manifest", "m"], OPT_SWITCH, :mode => PKGMODE_MANIFEST),
    ],
    "remove packages from project or manifest",
    md"""

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
    """,
),( CMD_ADD,
    ["add"],
    do_add!,
    (1=>Inf, parse_pkg, [:add_or_dev => true, :valid => [VersionRange, Rev]]),
    [],
    "add packages to project",
    md"""

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
    """,
),( CMD_DEVELOP,
    ["develop", "dev"],
    do_develop!,
    (1=>Inf, parse_pkg, [:add_or_dev => true, :valid => [VersionRange]]),
    [
        ("local", OPT_SWITCH, :shared => false),
        ("shared", OPT_SWITCH, :shared => true),
    ],
    "clone the full package repo locally for development",
    md"""
    develop [--shared|--local] pkg[=uuid] ...

Make a package available for development. If `pkg` is an existing local path that path will be recorded in
the manifest and used. Otherwise, a full git clone of `pkg` at rev `rev` is made. The location of the clone is
controlled by the `--shared` (default) and `--local` arguments. The `--shared` location defaults to
`~/.julia/dev`, but can be controlled with the `JULIA_PKG_DEVDIR` environment variable. When `--local` is given,
the clone is placed in a `dev` folder in the current project.
This operation is undone by `free`.

*Example*
```jl
pkg> develop Example
pkg> develop https://github.com/JuliaLang/Example.jl
pkg> develop ~/mypackages/Example
pkg> develop --local Example
```
    """,
),( CMD_FREE,
    ["free"],
    do_free!,
    (1=>Inf, parse_pkg, []),
    [],
    "undoes a `pin`, `develop`, or stops tracking a repo",
    md"""
    free pkg[=uuid] ...

Free a pinned package `pkg`, which allows it to be upgraded or downgraded again. If the package is checked out (see `help develop`) then this command
makes the package no longer being checked out.
    """,
),( CMD_PIN,
    ["pin"],
    do_pin!,
    (1=>Inf, parse_pkg, [:valid => [VersionRange]]),
    [],
    "pins the version of packages",
    md"""

    pin pkg[=uuid] ...

Pin packages to given versions, or the current version if no version is specified. A pinned package has its version fixed and will not be upgraded or downgraded.
A pinned package has the symbol `⚲` next to its version in the status list.
    """,
),( CMD_BUILD,
    ["build"],
    do_build!,
    (0=>Inf, parse_pkg, []),
    [],
    "run the build script for packages",
    md"""

    build pkg[=uuid] ...

Run the build script in `deps/build.jl` for each package in `pkg` and all of their dependencies in depth-first recursive order.
If no packages are given, runs the build scripts for all packages in the manifest.
The `startup.jl` file is disabled during building unless julia is started with `--startup-file=yes`.
    """,
),( CMD_RESOLVE,
    ["resolve"],
    do_resolve!,
    (0=>0, identity, []),
    [],
    "resolves to update the manifest from changes in dependencies of developed packages",
    md"""
    resolve

Resolve the project i.e. run package resolution and update the Manifest. This is useful in case the dependencies of developed
packages have changed causing the current Manifest to be out of sync.
    """,
),( CMD_ACTIVATE,
    ["activate"],
    do_activate!,
    (0=>1, identity, []),
    [
        ("shared", OPT_SWITCH, :shared => true),
    ],
    "set the primary environment the package manager manipulates",
    md"""
    activate
    activate [--shared] path

Activate the environment at the given `path`, or the home project environment if no `path` is specified.
The active environment is the environment that is modified by executing package commands.
When the option `--shared` is given, `path` will be assumed to be a directory name and searched for in the
`environments` folders of the depots in the depot stack. In case no such environment exists in any of the depots,
it will be placed in the first depot of the stack.
    """ ,
),( CMD_UP,
    ["update", "up"],
    do_up!,
    (0=>Inf, parse_pkg, [:valid => [VersionRange]]),
    [
        (["project", "p"], OPT_SWITCH, :mode => PKGMODE_PROJECT),
        (["manifest", "m"], OPT_SWITCH, :mode => PKGMODE_MANIFEST),
        ("major", OPT_SWITCH, :level => UPLEVEL_MAJOR),
        ("minor", OPT_SWITCH, :level => UPLEVEL_MINOR),
        ("patch", OPT_SWITCH, :level => UPLEVEL_PATCH),
        ("fixed", OPT_SWITCH, :level => UPLEVEL_FIXED),
    ],
    "update packages in manifest",
    md"""

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
    """,
),( CMD_GENERATE,
    ["generate"],
    do_generate!,
    (1=>1, identity, []),
    [],
    "generate files for a new project",
    md"""

    generate pkgname

Create a project called `pkgname` in the current folder.
    """,
),( CMD_PRECOMPILE,
    ["precompile"],
    do_precompile!,
    (0=>0, identity, []),
    [],
    "precompile all the project dependencies",
    md"""
    precompile

Precompile all the dependencies of the project by running `import` on all of them in a new process.
    """,
),( CMD_STATUS,
    ["status", "st"],
    do_status!,
    (0=>0, identity, []),
    [
        (["project", "p"], OPT_SWITCH, :mode => PKGMODE_PROJECT),
        (["manifest", "m"], OPT_SWITCH, :mode => PKGMODE_MANIFEST),
    ],
    "summarize contents of and changes to environment",
    md"""

    status
    status [-p|--project]
    status [-m|--manifest]

Show the status of the current environment. By default, the full contents of
the project file is summarized, showing what version each package is on and
how it has changed since the last git commit (if in a git repo), as well as
any changes to manifest packages not already listed. In `--project` mode, the
status of the project file is summarized. In `--manifest` mode the output also
includes the dependencies of explicitly added packages.
    """,
),( CMD_GC,
    ["gc"],
    do_gc!,
    (0=>0, identity, []),
    [],
    "garbage collect packages not used for a significant time",
    md"""

Deletes packages that cannot be reached from any existing environment.
    """,
),( # preview is not a regular command.
    # this is here so that preview appears as a registered command to users
    CMD_PREVIEW,
    ["preview"],
    nothing,
    (1=>Inf, identity, []),
    [],
    "previews a subsequent command without affecting the current state",
    md"""

    preview cmd

Runs the command `cmd` in preview mode. This is defined such that no side effects
will take place i.e. no packages are downloaded and neither the project nor manifest
is modified.
    """,
),
], #package
] #command_declarations

super_specs = SuperSpecs(command_declarations)
# cache things you need for completions
completion_cache.meta_options = sort(map(wrap_option, collect(keys(meta_option_specs))))
completion_cache.commands = sort(append!(collect(keys(super_specs)),
                                         collect(keys(super_specs["package"]))))
let names = String[]
    for (super, specs) in pairs(super_specs)
        super == "package" && continue # skip "package"
        for spec in unique(values(specs))
            push!(names, join([super, spec.canonical_name], "-"))
        end
    end
    for spec in unique(values(super_specs["package"]))
        push!(names, spec.canonical_name)
    end
    completion_cache.canonical_names = names
    sort!(completion_cache.canonical_names)
end
for (k, v) in pairs(super_specs)
    completion_cache.subcommands[k] = sort(collect(keys(v)))
    for spec in values(v)
        completion_cache.options[spec.kind] =
            sort(map(wrap_option, collect(keys(spec.option_specs))))
    end
end
# TODO remove this
command_specs = super_specs["package"]

const help = md"""

**Welcome to the Pkg REPL-mode**. To return to the `julia>` prompt, either press
backspace when the input line is empty or press Ctrl+C.


**Synopsis**

    pkg> cmd [opts] [args]

Multiple commands can be given on the same line by interleaving a `;` between the commands.

**Commands**
"""

for command in completion_cache.canonical_names
    spec = CommandSpec(command)
    push!(help.content, Markdown.parse("`$command`: $(spec.description)"))
end

end #module
