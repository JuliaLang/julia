require("options.jl", "textwrap.jl")

module ArgParse
using Base

using TextWrap
using OptionsMod

export
# types
    ArgParseSettings,
    ArgParseError,
    ArgParseField, # it shouldn't be necessary to export this, yet it is for some reason

# functions & macros
    add_arg_table,
    @add_arg_table,
    add_arg_group,
    set_default_arg_group,
    import_settings,
    usage_string,
    parse_args

import Base.ref, Base.assign, Base.has

# auxiliary functions/constants
_found_a_bug() = error("you just found a bug in the ArgParse module, please report it.")
const _nbsp = "\u00a0"

# actions
#{{{
const _all_actions = [:store_arg, :store_true, :store_false, :store_const,
                      :append_arg, :append_const, :count_invocations,
                      :command, :show_help, :show_version]

const _internal_actions = [:store_arg, :store_true, :store_false, :store_const,
                           :append_arg, :append_const, :count_invocations,
                           :command_arg, :command_flag,
                           :show_help, :show_version]

const _nonflag_actions = [:store_arg, :append_arg, :command_arg]
_is_flag_action(a::Symbol) = !contains(_nonflag_actions, a)

const _multi_actions = [:append_arg, :append_const]
_is_multi_action(a::Symbol) = contains(_multi_actions, a)

const _command_actions = [:command_arg, :command_flag]
_is_command_action(a::Symbol) = contains(_command_actions, a)
#}}}

# ArgConsumerType
#{{{
type ArgConsumerType
    num::Int
    desc::Char
    function ArgConsumerType(n::Integer)
        if n < 0
            error("negative number of arguments")
        end
        new(n, 'N')
    end
    function ArgConsumerType(c::Char)
        if !(c == 'A' || c == '?' || c == '*' || c == '+' || c == 'R')
            error("nargs must be an integer or one of 'A', '?', '*', '+', 'R'")
        end
        new(0, c)
    end
end
ArgConsumerType() = ArgConsumerType('A')

function show(io::IO, nargs::ArgConsumerType)
    if nargs.desc == 'N'
        show(io, nargs.num)
    elseif nargs.desc == 'A'
        show(io, "Auto")
    elseif nargs.desc == 'R'
        show(io, "Remainder")
    else
        show(io, nargs.desc)
    end
end

_is_multi_nargs(nargs::ArgConsumerType) = (nargs.desc != 'A' && nargs.desc != '?')

function _default_action(nargs::Union(Int, Char))
    if isa(nargs, Int) && nargs == 0
        return :store_true
    else
        return :store_arg
    end
end

function _default_action(nargs::ArgConsumerType)
    if nargs.desc == 'N' && nargs.num == 0
        return :store_true
    else
        return :store_arg
    end
end
#}}}

# ArgParseGroup
#{{{
type ArgParseGroup
    name::String
    desc::String
    ArgParseGroup(n::String, d::String) = new(n, d)
end
ArgParseGroup(n::Symbol, d::String) = new(string(n), d)

const _cmd_group = ArgParseGroup("commands", "commands")
const _pos_group = ArgParseGroup("positional", "positional arguments")
const _opt_group = ArgParseGroup("optional", "optional arguments")

const _std_groups = [_cmd_group, _pos_group, _opt_group]
#}}}

# ArgParseField
#{{{
type ArgParseField
    dest_name::String
    long_opt_name::Vector{String}
    short_opt_name::Vector{String}
    arg_type::Type
    action::Symbol
    nargs::ArgConsumerType
    default
    constant
    range_tester::Function
    required::Bool
    help::String
    metavar::String
    group::String
    fake::Bool
    function ArgParseField()
        return new("", String[], String[], Any, :store_true, ArgConsumerType(),
                   nothing, nothing, x->true, false, "", "", "", false)
    end
end

_is_flag(arg::ArgParseField) = _is_flag_action(arg.action)

_is_arg(arg::ArgParseField) = isempty(arg.long_opt_name) && isempty(arg.short_opt_name)

_is_cmd(arg::ArgParseField) = _is_command_action(arg.action)

_cmd_dest_name = "%COMMAND%"
#}}}

# ArgParseTable
#{{{
type ArgParseTable
    fields::Vector{ArgParseField}
    subsettings::Dict{String,Any} # this in fact will be a Dict{String,ArgParseSettings}
    ArgParseTable() = new(ArgParseField[], (String=>Any)[])
end
#}}}

# ArgParseSettings
#{{{
type ArgParseSettings
    prog::String
    description::String
    epilog::String
    usage::String
    version::String
    add_help::Bool
    add_version::Bool
    error_on_conflict::Bool
    suppress_warnings::Bool
    allow_ambiguous_opts::Bool
    commands_are_required::Bool
    args_groups::Vector{ArgParseGroup}
    default_group::String
    args_table::ArgParseTable
    exc_handler::Function

    function ArgParseSettings(prog::String, desc::String, add_help::Bool)
        this = new(prog, desc, "", "", "Unknown version", add_help, false,
                   true, false, false, true, copy(_std_groups), "",
                   ArgParseTable(), _default_handler)
        return this
    end
end
ArgParseSettings(prog::String, desc::String) = ArgParseSettings(prog, desc, true)
ArgParseSettings(prog::String) = ArgParseSettings(prog, "")
ArgParseSettings() = ArgParseSettings("")

typealias ArgName{T<:String} Union(T, Vector{T})

ref(s::ArgParseSettings, c::String) = s.args_table.subsettings[c]
has(s::ArgParseSettings, c::String) = has(s.args_table.subsettings, c)
assign(s::ArgParseSettings, x::ArgParseSettings, c::String) = assign(s.args_table.subsettings, x, c)

#}}}

# fields declarations sanity checks
#{{{
function _check_name_format(name::ArgName)
    if isempty(name)
        error("empty name")
    end
    if isa(name, Vector)
        for n in name
            if isempty(n)
                error("empty name")
            end
            if !begins_with(n, '-')
                error("only options can have multiple names")
            end
        end
    end
    return true
end

function _check_type(opt, T::Type, message::String)
    if !isa(opt, T)
        error(message)
    end
    return true
end

function _warn_extra_opts(opts::Vector{Symbol}, valid_keys::Vector{Symbol})
    for k in opts
        found = false
        for vk in valid_keys
            if k == vk
                found = true
                break
            end
        end
        if !found
            println(stderr_stream, "warning: ignored option: $k")
        end
    end
    return true
end

function _check_action_is_valid(action::Symbol)
    if !contains(_all_actions, action)
        error("invalid action: $action")
    end
end

function _check_nargs_and_action(nargs::ArgConsumerType, action::Symbol)
    if _is_flag_action(action) && (nargs.num != 0 || (nargs.desc != 'N' && nargs.desc != 'A'))
        error("incompatible nargs and action (flag-action $action, nargs=$nargs)")
    elseif _is_command_action(action) && nargs.desc != 'A'
        error("incompatible nargs and action (command action, nargs=$nargs)")
    elseif (nargs.desc == 'N' && nargs.num == 0)
        error("incompatible nargs and action (non-flag-action $action, nargs=$nargs)")
    end
    return true
end

function _check_long_opt_name(name::String, settings::ArgParseSettings)
    if contains(name, '=')
        error("illegal option name: $name (contains '=')")
    elseif ismatch(r"\s", name)
        error("illegal option name: $name (containes whitespace)")
    elseif contains(name, _nbsp)
        error("illegal option name: $name (containes non-breakable-space)")
    elseif settings.add_help && name == "help"
        error("option --help is reserved in the current settings")
    elseif settings.add_version && name == "version"
        error("option --version is reserved in the current settings")
    end
    return true
end

function _check_short_opt_name(name::String, settings::ArgParseSettings)
    if strlen(name) != 1
        error("short options must use a single character")
    elseif name == "="
        error("illegal short option name: $name")
    elseif ismatch(r"\s", name)
        error("illegal option name: $name (containes whitespace)")
    elseif contains(name, _nbsp)
        error("illegal option name: $name (containes non-breakable-space)")
    elseif !settings.allow_ambiguous_opts && ismatch(r"[0-9._(]", name)
        error("ambiguous option name: $name (disabled in the current settings)")
    elseif settings.add_help && name == "h"
        error("option -h is reserved for help in the current settings")
    end
    return true
end

function _check_arg_name(name::String)
    if ismatch(r"^%[A-Z]*%$", name)
        error("invalid positional arg name: $name (is reserved)")
    end
    return true
end

function _check_dest_name(name::String)
    if ismatch(r"^%[A-Z]*%$", name)
        error("invalid dest_name: $name (is reserved)")
    end
    return true
end

function _idstring(arg::ArgParseField)
    if _is_arg(arg)
        return "argument $(arg.metavar)"
    elseif !isempty(arg.long_opt_name)
        return "option --$(arg.long_opt_name[1])"
    else
        return "option -$(arg.short_opt_name[1])"
    end
end

# TODO improve (test more nonsensical cases)
function _check_arg_makes_sense(settings::ArgParseSettings, arg::ArgParseField)
    if !_is_arg(arg) || _is_command_action(arg.action)
        return true
    end

    has_cmd = false
    has_nonreq = false
    for f in settings.args_table.fields
        if _is_arg(f)
            if _is_command_action(f.action)
                has_cmd = true
            end
            if !f.required
                has_nonreq = true
            end
        end
    end
    if has_cmd
        error("non-command $(_idstring(arg)) can't follow commands")
    end
    if has_nonreq && arg.required
        error("required $(_idstring(arg)) can't follow non-required arguments")
    end
    return true
end

function _check_conflicts_with_commands(settings::ArgParseSettings, new_arg::ArgParseField, allow_future_merge::Bool)
    for (cmd, ss) in settings.args_table.subsettings
        if cmd == new_arg.dest_name
            error("$(_idstring(new_arg)) has the same destination of a command: $cmd")
        end
    end
    for a in settings.args_table.fields
        if _is_cmd(a) && !_is_cmd(new_arg)
            for l1 in a.long_opt_name, l2 in new_arg.long_opt_name
                if l1 == l2
                    # TODO be less strict here and below, and allow partial override?
                    error("long opt name --$(l1) already in use by command $(a.constant)")
                end
            end
            for s1 in a.short_opt_name, s2 in new_arg.short_opt_name
                if s1 == s2
                    error("short opt name -$(s1) already in use by command $(a.constant)")
                end
            end
        elseif _is_cmd(a) && _is_cmd(new_arg) && a.constant == new_arg.constant
            if !allow_future_merge
                error("command $(a.constant) already in use")
            elseif (_is_arg(a) && !_is_arg(new_arg)) || (!_is_arg(a) && _is_arg(new_arg))
                error("$(_idstring(a)) and $(_idstring(new_arg)) are incompatible")
            end
        end
    end
    return true
end

function _check_conflicts_with_commands(settings::ArgParseSettings, new_cmd::String)
    for a in settings.args_table.fields
        if new_cmd == a.dest_name
            error("command $new_cmd has the same destination of $(_idstring(a))")
        end
    end
    return true
end

function _check_for_duplicates(args::Vector{ArgParseField}, new_arg::ArgParseField)
    for a in args
        for l1 in a.long_opt_name, l2 in new_arg.long_opt_name
            if l1 == l2
                error("duplicate long opt name $(l1)")
            end
        end
        for s1 in a.short_opt_name, s2 in new_arg.short_opt_name
            if s1 == s2
                error("duplicate short opt name $(s1)")
            end
        end
        if _is_arg(a) && _is_arg(new_arg) && a.metavar == new_arg.metavar
            error("two arguments have the same metavar: $(a.metavar)")
        end
        if a.dest_name == new_arg.dest_name
            if a.arg_type != new_arg.arg_type
                error("$(_idstring(a)) and $(_idstring(new_arg)) have the same destination but different arg types")
            elseif (_is_multi_action(a.action) && !_is_multi_action(new_arg.action)) ||
                   (!_is_multi_action(a.action) && _is_multi_action(new_arg.action))
                error("$(_idstring(a)) and $(_idstring(new_arg)) have the same destination but incompatible actions")
            end
        end
    end
    return true
end
function _check_default_type(default, arg_type::Type)
    if !(default === nothing) && !isa(default, arg_type)
        error("the default value is of the incorrect type (typeof(default)=$(typeof(default)), arg_type=$arg_type)")
    end
    return true
end
function _check_default_type_multi(default, arg_type::Type)
    if !(default === nothing) && !isa(default, Vector{None}) &&
            !(isa(default, Vector) && (arg_type <: eltype(default)))
        error("the default value is of the incorrect type (typeof(default)=$(typeof(default)), should be a Vector{T} with T<:$arg_type})")
    end
    return true
end
function _check_default_type_multi2(default, arg_type::Type)
    if !(default === nothing) && !isa(default, Vector{None}) &&
            !(isa(default, Vector) && (Vector{arg_type} <: eltype(default)))
        error("the default value is of the incorrect type (typeof(default)=$(typeof(default)), should be a Vector{T} with Vector{$arg_type}<:T)")
    end
    return true
end
function _check_range_default(default, range_tester::Function)
    if default === nothing
        return true
    end
    local res::Bool
    try
        res = range_tester(default)
    catch err
        error("the range_tester function must be a defined for the default value and return a Bool")
    end
    if !res
        error("the default value must pass the range_tester function")
    end
    return true
end
function _check_range_default_multi(default, range_tester::Function)
    if default === nothing
        return true
    end
    @assert isa(default, Array)
    for d in default
        local res::Bool
        try
            res = range_tester(d)
        catch err
            error("the range_tester function must be a defined for all the default values and return a Bool")
        end
        if !res
            error("all the default values must pass the range_tester function")
        end
    end
    return true
end
function _check_range_default_multi2(default, range_tester::Function)
    if default === nothing
        return true
    end
    @assert isa(default, Array)
    for dl in default
        @assert isa(dl, Array)
        for d in dl
            local res::Bool
            try
                res = range_tester(d)
            catch err
                error("the range_tester function must be a defined for all the default values and return a Bool")
            end
            if !res
                error("all the default values must pass the range_tester function")
            end
        end
    end
    return true
end
function _check_metavar(metavar::String)
    if strlen(metavar) == 0
        error("empty metavar")
    elseif begins_with(metavar, '-')
        error("metavars cannot begin with -")
    elseif ismatch(r"\s", metavar)
        error("illegal metavar name: $metavar (containes whitespace)")
    elseif contains(metavar, _nbsp)
        error("illegal metavar name: $metavar (containes non-breakable-space)")
    end
    return true
end

function _check_group_name(name::String)
    if isempty(name)
        error("empty group name")
    elseif begins_with(name, '#')
        error("invalid group name (starts with #)")
    end
    return true
end
#}}}

# add_arg_table and related
#{{{
function _name_to_fieldnames(name::ArgName, settings::ArgParseSettings)
    pos_arg = ""
    long_opts = String[]
    short_opts = String[]
    if isa(name, Vector)
        for n in name
            if begins_with(n, "--")
                if n == "--"
                    error("illegal option name: --")
                end
                long_opt_name = n[3:end]
                _check_long_opt_name(long_opt_name, settings)
                push(long_opts, long_opt_name)
            else
                @assert begins_with(n, '-')
                if n == "-"
                    error("illegal option name: -")
                end
                short_opt_name = n[2:end]
                _check_short_opt_name(short_opt_name, settings)
                push(short_opts, short_opt_name)
            end
        end
    else
        if begins_with(name, "--")
            if name == "--"
                error("illegal option name: --")
            end
            long_opt_name = name[3:end]
            _check_long_opt_name(long_opt_name, settings)
            push(long_opts, long_opt_name)
        elseif begins_with(name, '-')
            if name == "-"
                error("illegal option name: -")
            end
            short_opt_name = name[2:end]
            _check_short_opt_name(short_opt_name, settings)
            push(short_opts, short_opt_name)
        else
            _check_arg_name(name)
            pos_arg = name
        end
    end
    return pos_arg, long_opts, short_opts
end

function _auto_dest_name(pos_arg::String, long_opts::Vector{String}, short_opts::Vector{String})
    if pos_arg == ""
        @assert !isempty(long_opts) || !isempty(short_opts)
        if !isempty(long_opts)
            return long_opts[1]
        else
            return short_opts[1]
        end
    else
        return pos_arg
    end
end

function _get_cmd_prog_hint(arg::ArgParseField)
    if !isempty(arg.short_opt_name)
        cmd_prog_hint = "-" * arg.short_opt_name[1]
    elseif !isempty(arg.long_opt_name)
        cmd_prog_hint = "--" * arg.long_opt_name[1]
    else
        cmd_prog_hint = arg.constant
    end
    return cmd_prog_hint
end


function add_arg_table(settings::ArgParseSettings, table::Union(ArgName, Options)...)
    has_name = false
    for i = 1:length(table)
        if !has_name && isa(table[i], Options)
            error("option field must be preceded by the arg name")
        end
        if isa(table[i], ArgName)
            has_name = true
        else
            has_name = false
        end
    end
    i = 1
    while i <= length(table)
        if i+1 <= length(table) && isa(table[i+1], Options)
            _add_arg_field(settings, table[i], table[i+1])
            i += 2
        else
            _add_arg_field(settings, table[i], Options())
            i += 1
        end
    end
end

macro add_arg_table(s, x...)
    # transform the tuple into a vector, so that
    # we can manipulate it
    x = {x...}
    # escape the ArgParseSettings
    s = esc(s)
    # start building the return expression
    exret = quote
        if !isa($s, ArgParseSettings)
            error("first argument to @add_arg_table must be of type ArgParseSettings")
        end
    end
    # initialize the name and the options expression
    name = nothing
    exopt = Any[:Options]

    # iterate over the arguments
    i = 1
    while i <= length(x)
        y = x[i]
        if isa(y, Expr) && y.head == :block
            # found a begin..end block: expand its contents
            # in-place and restart from the same position
            del(x, i)
            i0 = i
            for z in y.args
                insert(x, i, z)
                i += 1
            end
            i = i0
            continue
        elseif isa(y, String) || (isa(y, Expr) && (y.head == :vcat || y.head == :tuple))
            # found a string, or a vector expression, or a tuple:
            # this must be the option name
            if isa(y, Expr) && y.head == :tuple
                # transform tuples into vectors
                y.head = :vcat
            end
            if !(name === nothing)
                # there was a previous arg field on hold
                # first, concretely build the options
                opt = expr(:call, exopt)
                # then, build the _add_arg_field expression
                exaaf = Any[:_add_arg_field, s, name, opt]
                # then, call _add_arg_field
                aaf = expr(:call, exaaf)
                # store it in the output expression
                exret = quote
                    $exret
                    $aaf
                end
            end
            # put the name on hold, reinitialize the options expression
            name = y
            exopt = Any[:Options]
            i += 1
        elseif isa(y,Expr) && (y.head == :(=) || y.head == :(=>) || y.head == :(:=))
            # found an assignment: add it to the current options expression
            push(exopt, expr(:quote, y.args[1]))
            push(exopt, esc(y.args[2]))
            i += 1
        elseif isa(y, LineNumberNode)
            # a line number node, ignore
            i += 1
            continue
        else
            # anything else: ignore, but issue a warning
            println(stderr_stream, "warning: @add_arg_table: ignoring expression ", y)
            i += 1
        end
    end
    if !(name === nothing)
        # there is an arg field on hold
        # same as above
        opt = expr(:call, exopt)
        exaaf = Any[:_add_arg_field, s, name, opt]
        aaf = expr(:call, exaaf)
        exret = quote
            $exret
            $aaf
        end
    end

    # the return value when invoking the macro
    # will be the ArgParseSettings object
    exret = quote
        $exret
        $s
    end

    # return the resulting expression
    exret
end

function _get_group(group::String, arg::ArgParseField, settings::ArgParseSettings)
    if isempty(group)
        if _is_cmd(arg)
            return _cmd_group
        elseif _is_arg(arg)
            return _pos_group
        else
            return _opt_group
        end
    else
        for ag in settings.args_groups
            if group == ag.name
                return ag
            end
        end
        error("group $group not found, use add_arg_group to add it")
    end
    _found_a_bug()
end
_get_group_name(group::String, arg::ArgParseField, settings::ArgParseSettings) =
    _get_group(group, arg, settings).name

function _add_arg_field(settings::ArgParseSettings, name::ArgName, desc::Options)
    _check_name_format(name)

    supplied_opts = keys(desc.key2index)

    @defaults desc begin
        nargs = ArgConsumerType()
        action = _default_action(nargs)
        arg_type = Any
        default = nothing
        constant = nothing
        required = false
        range_tester = x->true
        dest_name = ""
        help = ""
        metavar = ""
        force_override = !settings.error_on_conflict
        group = settings.default_group
    end
    @check_used(desc)

    _check_type(nargs, Union(ArgConsumerType,Int,Char), "nargs must be an Int or a Char")
    _check_type(action, Union(String,Symbol), "action must be a String or a Symbol")
    _check_type(arg_type, Type, "invalid arg_type")
    _check_type(required, Bool, "required must be a Bool")
    _check_type(range_tester, Function, "range_tester must be a Function")
    _check_type(dest_name, String, "dest_name must be a String")
    _check_type(help, String, "help must be a String")
    _check_type(metavar, String, "metavar must be a String")
    _check_type(force_override, Bool, "force_override must be a Bool")
    _check_type(group, Union(String,Symbol), "group must be a String or a Symbol")

    if !isa(nargs, ArgConsumerType)
        nargs = ArgConsumerType(nargs)
    end
    if !isa(action, Symbol)
        action = symbol(action)
    end

    is_opt = isa(name, Vector) || begins_with(name, '-')

    _check_action_is_valid(action)

    if action == :command
        action = is_opt ? (:command_flag) : (:command_arg)
    end

    _check_nargs_and_action(nargs, action)

    new_arg = ArgParseField()

    is_flag = _is_flag_action(action)
    if !is_opt && is_flag
        error("error: invalid action for positional argument: $action")
    end
    if !is_opt && nargs.desc == '?'
        error("error: invalid 'nargs' for positional argument: ?")
    end

    pos_arg, long_opts, short_opts = _name_to_fieldnames(name, settings)

    new_arg.dest_name = _auto_dest_name(pos_arg, long_opts, short_opts)

    new_arg.long_opt_name = long_opts
    new_arg.short_opt_name = short_opts
    new_arg.nargs = nargs
    new_arg.action = action

    group = string(group)
    if contains(supplied_opts, :group) && !isempty(group)
        _check_group_name(group)
    end
    new_arg.group = _get_group_name(group, new_arg, settings)

    if (action == :store_const || action == :append_const) &&
           !contains(supplied_opts, :constant)
        error("action $action requires the 'constant' field")
    end

    valid_keys = [:nargs, :action, :help, :force_override, :group]
    if is_flag
        if action == :store_const || action == :append_const
            append!(valid_keys, [:default, :constant, :arg_type, :dest_name])
        elseif action == :store_true || action == :store_false ||
               action == :count_invocations || action == :command_flag
            push(valid_keys, :dest_name)
        elseif action == :show_help || action == :show_version
        else
            _found_a_bug()
        end
    elseif is_opt
        append!(valid_keys, [:arg_type, :default, :range_tester, :dest_name, :metavar])
        if nargs.desc == '?'
            push(valid_keys, :constant)
        end
    else
        if action != :command_arg
            append!(valid_keys, [:arg_type, :default, :range_tester, :required, :metavar])
        end
    end
    if !settings.suppress_warnings
        _warn_extra_opts(supplied_opts, valid_keys)
    end

    if _is_command_action(action)
        if contains(supplied_opts, :dest_name)
            cmd_name = dest_name
        else
            cmd_name = new_arg.dest_name
        end
    end
    if contains(supplied_opts, :dest_name) &&
           contains(valid_keys, :dest_name) &&
           action != :command_flag
        new_arg.dest_name = dest_name
    end

    _check_dest_name(dest_name)

    set_if_valid(k, x) = if contains(valid_keys, k) new_arg.(k) = x end

    set_if_valid(:arg_type, arg_type)
    set_if_valid(:default, deepcopy(default))
    set_if_valid(:constant, deepcopy(constant))
    set_if_valid(:range_tester, range_tester)
    set_if_valid(:required, required)
    set_if_valid(:help, help)
    set_if_valid(:metavar, metavar)

    if !is_flag
        if isempty(new_arg.metavar)
            if is_opt
                new_arg.metavar = uppercase(new_arg.dest_name)
            else
                new_arg.metavar = new_arg.dest_name
            end
        end
        _check_metavar(new_arg.metavar)
    end

    if _is_command_action(action)
        new_arg.dest_name = _cmd_dest_name
        new_arg.arg_type = String
        new_arg.constant = cmd_name
        cmd_prog_hint = _get_cmd_prog_hint(new_arg)
    end

    if is_flag
        if action == :store_true
            new_arg.arg_type = Bool
            new_arg.default = false
            new_arg.constant =  true
        elseif action == :store_false
            new_arg.arg_type = Bool
            new_arg.default = true
            new_arg.constant =  false
        elseif action == :count_invocations
            new_arg.arg_type = Int
            new_arg.default = 0
        elseif action == :store_const || action == :append_const
            if contains(supplied_opts, :arg_type)
                _check_default_type(new_arg.default, new_arg.arg_type)
                _check_default_type(new_arg.constant, new_arg.arg_type)
            else
                if typeof(new_arg.default) == typeof(new_arg.constant)
                    new_arg.arg_type = typeof(new_arg.default)
                else
                    new_arg.arg_type = Any
                end
            end
            if action == :append_const
                if new_arg.default === nothing || new_arg.default == []
                    new_arg.default = Array(new_arg.arg_type, 0)
                end
            end
        elseif action == :command_flag
            # nothing to do
        elseif action == :show_help || action == :show_version
            # nothing to do
        else
            _found_a_bug()
        end
    else
        arg_type = new_arg.arg_type
        range_tester = new_arg.range_tester
        default = new_arg.default

        if !_is_multi_action(new_arg.action) && !_is_multi_nargs(new_arg.nargs)
            _check_default_type(default, arg_type)
            _check_range_default(default, range_tester)
        elseif !_is_multi_action(new_arg.action) || !_is_multi_nargs(new_arg.nargs)
            _check_default_type_multi(default, arg_type)
            _check_range_default_multi(default, range_tester)
        else
            _check_default_type_multi2(default, arg_type)
            _check_range_default_multi2(default, range_tester)
        end
        if (_is_multi_action(new_arg.action) && _is_multi_nargs(new_arg.nargs)) && (default === nothing || default == [])
            new_arg.default = Array(Vector{arg_type}, 0)
        elseif (_is_multi_action(new_arg.action) || _is_multi_nargs(new_arg.nargs)) && (default === nothing || default == [])
            new_arg.default = Array(arg_type, 0)
        end

        if is_opt && nargs.desc == '?'
            constant = new_arg.constant
            if !_is_multi_nargs(new_arg.nargs)
                _check_default_type(constant, arg_type)
                _check_range_default(constant, range_tester)
            else
                _check_default_type_multi(constant, arg_type)
                _check_range_default_multi(constant, range_tester)
            end
        end
    end

    if action == :command_arg
        for f in settings.args_table.fields
            if f.action == :command_arg
                new_arg.fake = true
                break
            end
        end
    end

    _check_arg_makes_sense(settings, new_arg)

    _check_conflicts_with_commands(settings, new_arg, false)
    if force_override
        _override_duplicates(settings.args_table.fields, new_arg)
    else
        _check_for_duplicates(settings.args_table.fields, new_arg)
    end
    push(settings.args_table.fields, new_arg)
    if _is_command_action(action)
        _add_command(settings, cmd_name, cmd_prog_hint, force_override)
    end
    return
end

function _add_command(settings::ArgParseSettings, command::String, prog_hint::String, force_override::Bool)
    if has(settings, command)
        #return settings[command]
        error("command $command already added")
    end
    if force_override
        _override_conflicts_with_commands(settings, command)
    else
        _check_conflicts_with_commands(settings, command)
    end
    settings[command] = ArgParseSettings()
    ss = settings[command]
    ss.prog = "$(settings.prog) $prog_hint"
    ss.description = ""
    ss.epilog = ""
    ss.usage = ""
    ss.version = settings.version
    ss.add_help = settings.add_help
    ss.add_version = settings.add_version
    ss.error_on_conflict = settings.error_on_conflict
    ss.suppress_warnings = settings.suppress_warnings
    ss.allow_ambiguous_opts = settings.allow_ambiguous_opts
    ss.exc_handler = settings.exc_handler

    return ss
end

_autogen_group_name(desc::String) = "#$(hash(desc))"

add_arg_group(settings::ArgParseSettings, desc::String) =
    _add_arg_group(settings, desc, _autogen_group_name(desc), true)
add_arg_group(settings::ArgParseSettings, desc::String, tag::Union(String,Symbol)) =
    add_arg_group(settings, desc, tag, true)
function add_arg_group(settings::ArgParseSettings, desc::String, tag::Union(String,Symbol), set_as_default::Bool)
    name = string(tag)
    _check_group_name(name)
    _add_arg_group(settings, desc, name, set_as_default)
end

function _add_arg_group(settings::ArgParseSettings, desc::String, name::String, set_as_default::Bool)
    already_added = false
    for ag in settings.args_groups
        if ag.name == name
            already_added = true
        end
    end
    if !already_added
        push(settings.args_groups, ArgParseGroup(name, desc))
    end
    if set_as_default
        settings.default_group = name
    end
    return settings
end

set_default_arg_group(settings::ArgParseSettings) = set_default_arg_group(settings, "")
function set_default_arg_group(settings::ArgParseSettings, name::Union(String,Symbol))
    name = string(name)
    if begins_with(name, '#')
        error("invalid group name: $name (begins with #)")
    end
    if isempty(name)
        settings.default_group = ""
        return
    end
    for ag in settings.args_groups
        if ag.name == name
            settings.default_group = name
            return
        end
    end
    error("group $name not found")
end
#}}}

# import_settings & friends
#{{{
function _override_conflicts_with_commands(settings::ArgParseSettings, new_cmd::String)
    ids0 = Int[]
    for ia in 1:length(settings.args_table.fields)
        a = settings.args_table.fields[ia]
        if new_cmd == a.dest_name
            push(ids0, ia)
        end
    end
    while !isempty(ids0)
        del(settings.args_table.fields, pop(ids0))
    end
end
function _override_duplicates(args::Vector{ArgParseField}, new_arg::ArgParseField)
    ids0 = Int[]
    for ia in 1:length(args)
        a = args[ia]
        if (a.dest_name == new_arg.dest_name) &&
            ((a.arg_type != new_arg.arg_type) ||
             (_is_multi_action(a.action) && !_is_multi_action(new_arg.action)) ||
             (!_is_multi_action(a.action) && _is_multi_action(new_arg.action)))
            # unsolvable conflict, mark for deletion
            push(ids0, ia)
            continue
        end
        if _is_arg(a) && _is_arg(new_arg) && a.metavar == new_arg.metavar
            # unsolvable conflict, mark for deletion
            push(ids0, ia)
            continue
        end

        if _is_arg(a) || _is_arg(new_arg)
            # not an option, skip
            continue
        end

        if _is_cmd(a) && _is_cmd(new_arg) && a.constant == new_arg.constant && !_is_arg(a)
            @assert !_is_arg(new_arg) # this is ensured by _check_settings_are_compatible
            # two command flags with the same command -> should have already be taken car of,
            # by either _check_settings_are_compatible or _merge_commands
            continue
        end

        # delete conflicting long options
        ids = Int[]
        for il1 = 1:length(a.long_opt_name), l2 in new_arg.long_opt_name
            l1 = a.long_opt_name[il1]
            if l1 == l2
                push(ids, il1)
            end
        end
        while !isempty(ids)
            del(a.long_opt_name, pop(ids))
        end

        # delete conflicting short options
        ids = Int[]
        for is1 in 1:length(a.short_opt_name), s2 in new_arg.short_opt_name
            s1 = a.short_opt_name[is1]
            if s1 == s2
                push(ids, is1)
            end
        end
        while !isempty(ids)
            del(a.short_opt_name, pop(ids))
        end

        # if everything was deleted, remove the field altogether
        # (i.e. mark it for deletion)
        if isempty(a.long_opt_name) && isempty(a.short_opt_name)
            push(ids0, ia)
        end
    end

    # actually remove the marked fields
    while !isempty(ids0)
        del(args, pop(ids0))
    end
end

function _check_settings_are_compatible(settings::ArgParseSettings, other::ArgParseSettings)
    table = settings.args_table
    otable = other.args_table

    for a in otable.fields
        _check_conflicts_with_commands(settings, a, true)
        if settings.error_on_conflict
            _check_for_duplicates(table.fields, a)
        end
    end

    for (subk, subs) in otable.subsettings
        if settings.error_on_conflict
            _check_conflicts_with_commands(settings, subk)
        end
        if has(settings, subk)
            _check_settings_are_compatible(settings[subk], subs)
        end
    end
    return true
end

function _merge_commands(fields::Vector{ArgParseField}, ofields::Vector{ArgParseField})
    oids = Int[]
    for a in fields, ioa = 1:length(ofields)
        oa = ofields[ioa]
        if _is_cmd(a) && _is_cmd(oa) && a.constant == oa.constant && !_is_arg(a)
            @assert !_is_arg(oa) # this is ensured by _check_settings_are_compatible
            for l in oa.long_opt_name
                if !contains(a.long_opt_name, l)
                    push(a.long_opt_name, l)
                end
            end
            for s in oa.short_opt_name
                if !contains(a.short_opt_name, s)
                    push(a.short_opt_name, s)
                end
            end
            a.group = oa.group # note: the group may not be present yet, but it will be
                               #       added later
            push(oids, ioa)
        end
    end
    # we return the merged ofields indices, since we still need to use them for overriding options
    # before we actually remove them
    return oids
end

function _fix_commands_fields(fields::Vector{ArgParseField})
    cmd_found = false
    for a in fields
        if _is_arg(a) && _is_cmd(a)
            a.fake = cmd_found
            cmd_found = true
        end
    end
end

import_settings(settings::ArgParseSettings, other::ArgParseSettings) =
    import_settings(settings, other, true)
function import_settings(settings::ArgParseSettings, other::ArgParseSettings, args_only::Bool)
    _check_settings_are_compatible(settings, other)

    fields = settings.args_table.fields
    ofields = deepcopy(other.args_table.fields)
    merged_oids = _merge_commands(fields, ofields)
    if !settings.error_on_conflict
        for a in ofields
            _override_duplicates(fields, a)
        end
        for (subk, subs) in other.args_table.subsettings
            _override_conflicts_with_commands(settings, subk)
        end
    end
    while !isempty(merged_oids)
        del(ofields, pop(merged_oids))
    end
    append!(fields, ofields)
    for oag in other.args_groups
        skip = false
        for ag in settings.args_groups
            if (!begins_with(oag.name, '#') && oag.name == ag.name) ||
               (begins_with(ag.name, '#') && oag.desc == ag.desc)
                skip = true
                break
            end
        end
        if skip
            continue
        end
        push(settings.args_groups, deepcopy(oag))
    end

    _fix_commands_fields(fields)

    if !args_only
        settings.add_help = other.add_help
        settings.add_version = other.add_version
        settings.error_on_conflict = other.error_on_conflict
        settings.suppress_warnings = other.suppress_warnings
        settings.exc_handler = other.exc_handler
        settings.allow_ambiguous_opts = other.allow_ambiguous_opts
        settings.commands_are_required = other.commands_are_required
        settings.default_group = other.default_group
    end
    for (subk, subs) in other.args_table.subsettings
        cmd_prog_hint = ""
        for oa in other.args_table.fields
            if _is_cmd(oa) && oa.constant == subk
                cmd_prog_hint = _get_cmd_prog_hint(oa)
                break
            end
        end
        if !has(settings, subk)
            _add_command(settings, subk, cmd_prog_hint, !settings.error_on_conflict)
        elseif !isempty(cmd_prog_hint)
            settings[subk].prog = "$(settings.prog) $cmd_prog_hint"
        end
        import_settings(settings[subk], subs, args_only)
    end
    return settings
end
#}}}

# ArgParseError
#{{{
type ArgParseError <: Exception
    text::String
end

_argparse_error(x) = throw(ArgParseError(x))
#}}}

# parsing checks
#{{{
function _test_range(range_tester::Function, arg)
    local rng_chk::Bool
    try
        rng_chk = range_tester(arg)
    catch
        return false
    end
    return rng_chk
end

function _test_required_args(settings::ArgParseSettings, found_args::Set{String})
    for f in settings.args_table.fields
        if _is_arg(f) && f.required && !has(found_args, f.metavar)
            _argparse_error("required argument $(f.metavar) was not provided")
        end
    end
    return true
end
#}}}

# parsing aux functions
#{{{
_parse_item(it_type::Type{Any}, x::String) = x
_parse_item{T<:String}(it_type::Type{T}, x::String) = convert(T, x)
function _parse_item(it_type::Type, x::String)
    local r
    try
        if strlen(x) == 0
            y = ""
        else
            y = eval(parse(x)[1])
        end
        r = convert(it_type, y)
    catch
        _argparse_error("invalid argument: $x (must be of type $it_type)")
    end
    return r
end

const _number_regex =
    r"^[+-]?                                          # optional sign
        (
          0x[0-9a-fA-F](_?[0-9a-fA-F])*             | # hex
          (                                           # float mantissa
            [0-9](_?[0-9])*(\.([0-9](_?[0-9])*)?)?  | #   start with digit
            \.[0-9](_?[0-9])*                         #   start with dot
          )([eE][-+]?[0-9]+)?                         # float optional exp
        )
      $"x

function _looks_like_an_option(arg::String, settings::ArgParseSettings)
    if !begins_with(arg, '-') || arg == "-"
        return false
    elseif begins_with(arg, "--")
        return true
    end
    # begins with '-'
    if !ismatch(_number_regex, arg)
        # it's not a number
        return true
    end
    # looks like a number; but is it overridden by an option?
    d = arg[2]
    for a in settings.args_table.fields, s in a.short_opt_name
        if s == d
            return true
        end
    end
    # it's a number
    return false
end

function usage_string(settings::ArgParseSettings)
    if !isempty(settings.usage)
        return settings.usage
    end

    if isempty(settings.prog)
        usage_pre = "usage: <command>"
    else
        usage_pre = "usage: " * settings.prog
    end

    lc_len_limit = 24

    cmd_lst = {}
    pos_lst = {}
    opt_lst = {}
    for f in settings.args_table.fields
        if _is_cmd(f)
            if !isempty(f.short_opt_name)
                idstr = "-" * f.short_opt_name[1]
            elseif !isempty(f.long_opt_name)
                idstr = "--" * f.long_opt_name[1]
            else
                idstr = f.metavar
            end
            push(cmd_lst, idstr)
        elseif _is_arg(f)
            if !f.required
                bra_pre = "["
                bra_post = "]"
            else
                bra_pre = ""
                bra_post = ""
            end
            if f.nargs.desc == 'N'
                arg_str = strcat(ntuple(f.nargs.num, i->(i==1?f.metavar:(_nbsp * f.metavar)))...)
            elseif f.nargs.desc == 'A'
                arg_str = f.metavar
            elseif f.nargs.desc == '?'
                _found_a_bug()
            elseif f.nargs.desc == '*' || f.nargs.desc == 'R' || f.nargs.desc == '+'
                arg_str = f.metavar * "..."
            else
                _found_a_bug()
            end
            push(pos_lst, bra_pre * arg_str * bra_post)
        else
            if !isempty(f.short_opt_name)
                opt_str1 = "-" * f.short_opt_name[1]
            else
                opt_str1 = "--" * f.long_opt_name[1]
            end
            if _is_flag(f)
                opt_str2 = ""
            else
                if f.nargs.desc == 'N'
                    opt_str2 = strcat(ntuple(f.nargs.num, i->(_nbsp * f.metavar))...)
                elseif f.nargs.desc == 'A'
                    opt_str2 = _nbsp * f.metavar
                elseif f.nargs.desc == '?'
                    opt_str2 = _nbsp * "[" * f.metavar * "]"
                elseif f.nargs.desc == '*' || f.nargs.desc == 'R'
                    opt_str2 = _nbsp * "[" * f.metavar * "...]"
                elseif f.nargs.desc == '+'
                    opt_str2 = _nbsp * f.metavar * _nbsp * "[" * f.metavar * "...]"
                else
                    _found_a_bug()
                end
            end
            new_opt = "[" * opt_str1 * opt_str2 * "]"
            push(opt_lst, new_opt)
        end
    end
    if isempty(opt_lst)
        optl_str = ""
    else
        optl_str = " " * join(opt_lst, " ")
    end
    if isempty(pos_lst)
        posl_str = ""
    else
        posl_str = " " * join(pos_lst, " ")
    end
    if isempty(cmd_lst)
        cmdl_str = ""
    else
        if !settings.commands_are_required
            bra_pre = "["
            bra_post = "]"
        else
            bra_pre = "{"
            bra_post = "}"
        end
        cmdl_str = " " * bra_pre * join(cmd_lst, "|") * bra_post
    end

    usage_len = strlen(usage_pre) + 1
    twopts = @options begin
        break_long_words = false
        break_on_hyphens = false
        subsequent_indent = min(usage_len, lc_len_limit)
    end

    str_nonwrapped = usage_pre * optl_str * posl_str * cmdl_str
    str_wrapped = wrap(str_nonwrapped, twopts)

    out_str = replace(str_wrapped, _nbsp, ' ')
    return out_str
end

function _gen_help_text(arg::ArgParseField, settings::ArgParseSettings)
    pre = isempty(arg.help) ? "" : " "
    if !_is_flag(arg)
        type_str = ""
        default_str = ""
        const_str = ""
        if !_is_command_action(arg.action)
            if arg.arg_type != Any
                type_str = pre * "(type: " * string(arg.arg_type)
            end
            if !(arg.default === nothing) && !isequal(arg.default, [])
                mid = isempty(type_str) ? " (" : ", "
                default_str = mid * "default: " * string(arg.default)
            end
            if arg.nargs.desc == '?'
                mid = isempty(type_str) && isempty(default_str) ? " (" : ", "
                const_str = mid * "without arg: " * string(arg.constant)
            end
        end
        post = (isempty(type_str) && isempty(default_str) && isempty(const_str)) ? "" : ")"
        return arg.help * type_str * default_str * const_str * post
    else
        return arg.help
    end
end

function _print_group(lst::Vector, desc::String, lc_usable_len::Int, lc_len::Int,
                      lmargin::String, rmargin::String,
                      twopts_block1::Options, twopts_block2::Options)
    if isempty(lst)
        return
    end
    println(desc * ":")
    for l in lst
        l1len = strlen(l[1])
        if l1len <= lc_usable_len
            rfill = " " ^ (lc_len - l1len)
            ll_nonwrapped = l[1] * rfill * rmargin * l[2]
            ll_wrapped = wrap(ll_nonwrapped, twopts_block1)
            println(replace(ll_wrapped, _nbsp, ' '))
        else
            println(lmargin, l[1])
            println_wrapped(l[2], twopts_block2)
        end
    end
    println()
end

function _show_help(settings::ArgParseSettings)

    twopts_desc = @options begin
        break_long_words = false
        break_on_hyphens = false
    end

    twopts_block1 = @options begin
        break_long_words = false
        break_on_hyphens = false
    end

    twopts_block2 = @options begin
        break_long_words = false
        break_on_hyphens = false
    end

    lc_len_limit = 24
    lc_left_indent = 2
    lc_right_margin = 2

    lc_usable_len = lc_len_limit - lc_left_indent - lc_right_margin
    max_lc_len = 0

    usage_str = usage_string(settings)

    group_lists = (String=>Vector{Any})[]
    for ag in settings.args_groups
        group_lists[ag.name] = Any[]
    end
    for f in settings.args_table.fields
        dest_lst = group_lists[f.group]
        if _is_arg(f)
            push(dest_lst, {f.metavar, _gen_help_text(f, settings)})
            max_lc_len = max(max_lc_len, strlen(f.metavar))
        else
            opt_str1 = join([["-"*x for x in f.short_opt_name], ["--"*x for x in f.long_opt_name]], ", ")
            if _is_flag(f)
                opt_str2 = ""
            else
                if f.nargs.desc == 'N'
                    opt_str2 = strcat(ntuple(f.nargs.num, i->(_nbsp * f.metavar))...)
                elseif f.nargs.desc == 'A'
                    opt_str2 = _nbsp * f.metavar
                elseif f.nargs.desc == '?'
                    opt_str2 = _nbsp * "[" * f.metavar * "]"
                elseif f.nargs.desc == '*' || f.nargs.desc == 'R'
                    opt_str2 = _nbsp * "[" * f.metavar * "...]"
                elseif f.nargs.desc == '+'
                    opt_str2 = _nbsp * f.metavar * _nbsp * "[" * f.metavar * "...]"
                else
                    _found_a_bug()
                end
            end
            new_opt = {opt_str1 * opt_str2, _gen_help_text(f, settings)}
            push(dest_lst, new_opt)
            max_lc_len = max(max_lc_len, strlen(new_opt[1]))
        end
    end

    lc_len = min(lc_usable_len, max_lc_len)
    lmargin = " " ^ lc_left_indent
    rmargin = " " ^ lc_right_margin

    sindent = lmargin * " " ^ lc_len * rmargin

    @set_options(twopts_block1, initial_indent => lmargin, subsequent_indent => sindent)
    @set_options(twopts_block2, initial_indent => sindent, subsequent_indent => sindent)

    println(usage_str)
    println()
    if length(settings.description) > 0
        println_wrapped(settings.description, twopts_desc)
        println()
    end

    for ag in settings.args_groups
        _print_group(group_lists[ag.name], ag.desc, lc_usable_len, lc_len,
                     lmargin, rmargin, twopts_block1, twopts_block2)
     end

    if length(settings.epilog) > 0
        println_wrapped(settings.epilog, twopts_desc)
        println()
    end
    exit(0)
end

function _show_version(settings::ArgParseSettings)
    println(settings.version)
    exit(0)
end

function _has_cmd(settings::ArgParseSettings)
    for a in settings.args_table.fields
        if _is_cmd(a)
            return true
        end
    end
    return false
end
#}}}

# parse_args & friends
#{{{
function _default_handler(settings::ArgParseSettings, err)
    println(stderr_stream, err.text)
    println(stderr_stream, usage_string(settings))
    exit(1)
end

parse_args(settings::ArgParseSettings) = parse_args(ARGS, settings)

function parse_args(args_list::Vector, settings::ArgParseSettings)
    local parsed_args
    try
        parsed_args = _parse_args_unhandled(args_list, settings)
    catch err
        if isa(err, ArgParseError)
            settings.exc_handler(settings, err)
        else
            throw(err)
        end
    end
    parsed_args
end

function _parse_args_unhandled(args_list::Vector, settings::ArgParseSettings)
    if any(map(x->!isa(x,String), args_list))
        _argparse_error("malformed args_list")
    end

    version_added = false
    help_added = false

    if settings.add_version
        settings.add_version = false
        _add_arg_field(settings, "--version",
            @options begin
                action = :show_version
                help = "show version information and exit"
                group = ""
            end)
        version_added = true
    end
    if settings.add_help
        settings.add_help = false
        _add_arg_field(settings, ["--help","-h"],
            @options begin
                action = :show_help
                help = "show this help message and exit"
                group = ""
            end)
        help_added = true
    end

    found_args = Set{String}()
    out_dict = (String=>Any)[]

    for f in settings.args_table.fields
        if f.action == :show_help || f.action == :show_version
            continue
        end
        out_dict[f.dest_name] = deepcopy(f.default)
    end

    arg_delim_found = false
    last_ind = 0
    last_arg = 0
    command = nothing
    while last_ind < length(args_list)
        last_ind += 1

        arg = args_list[last_ind]
        if arg == "--"
            arg_delim_found = true
            continue
        elseif !arg_delim_found && begins_with(arg, "--")
            i,j = search(arg, '=')
            if i != 0
                opt_name = arg[3:i-1]
                arg_after_eq = arg[j:end]
            else
                opt_name = arg[3:end]
                arg_after_eq = nothing
            end
            if strlen(opt_name) == 0
                _argparse_error("illegal option: $arg")
            end
            last_ind, command, out_dict = _parse_long_opt(settings, opt_name, last_ind, arg_after_eq, args_list, out_dict)
        elseif !arg_delim_found && _looks_like_an_option(arg, settings)
            shopts_lst = arg[2:end]
            last_ind, command, shopts_lst_rest, out_dict = _parse_short_opt(settings, shopts_lst, last_ind, args_list, out_dict)
            if !(command === nothing) && !(shopts_lst_rest === nothing)
                args_list = copy(args_list)
                args_list[last_ind] = "-" * shopts_lst_rest
                last_ind -= 1
            end
        else
            last_ind, last_arg, command, out_dict = _parse_arg(settings, last_ind, last_arg, arg_delim_found, args_list, out_dict)
            add(found_args, settings.args_table.fields[last_arg].metavar)
        end
        if !(command === nothing)
            break
        end
    end
    _test_required_args(settings, found_args)
    if !(command === nothing)
        if !has(settings, command)
            _argparse_error("unknown command: $command")
        end
        out_dict[command] = parse_args(args_list[last_ind+1:end], settings[command])
    elseif settings.commands_are_required && _has_cmd(settings)
        _argparse_error("No command given")
    end

    if help_added
        pop(settings.args_table.fields)
        settings.add_help = true
    end
    if version_added
        pop(settings.args_table.fields)
        settings.add_version = true
    end
    return out_dict
end

# common parse functions
#{{{
function _parse1_flag(settings::ArgParseSettings, f::ArgParseField, has_arg::Bool, opt_name::String, out_dict::Dict)
    if has_arg
        _argparse_error("option $opt_name takes no arguments")
    end
    command = nothing
    if f.action == :store_true
        out_dict[f.dest_name] = true
    elseif f.action == :store_false
        out_dict[f.dest_name] = false
    elseif f.action == :store_const
        out_dict[f.dest_name] = f.constant
    elseif f.action == :append_const
        push(out_dict[f.dest_name], f.constant)
    elseif f.action == :count_invocations
        out_dict[f.dest_name] += 1
    elseif f.action == :command_flag
        out_dict[f.dest_name] = f.constant
        command = f.constant
    elseif f.action == :show_help
        _show_help(settings)
    elseif f.action == :show_version
        _show_version(settings)
    end
    return out_dict, command
end

function _err_arg_required(name::String, num::Int, is_opt::Bool)
    _argparse_error((is_opt?"option":"argument")*" $name requires $num argument(s)")
end
function _err_arg_outofrange(name::String, a, is_opt::Bool)
    _argparse_error("out of range " *
                    (is_opt?"argument to option":"input to argument") *
                    " $name: $a")
end

function _parse1_optarg(settings::ArgParseSettings, f::ArgParseField, rest, args_list, name::String,
                        is_opt::Bool, arg_delim_found::Bool,
                        out_dict::Dict, last_ind::Int)
    arg_consumed = false
    command = nothing
    if _is_multi_nargs(f.nargs)
        opt_arg = Array(f.arg_type, 0)
    end
    if f.nargs.desc == 'N'
        num = f.nargs.num
        @assert num > 0
        corr = (rest === nothing) ? 0 : 1
        if length(args_list) - last_ind + corr < num
            _err_arg_required(name, num, is_opt)
        end
        if !(rest === nothing)
            a = _parse_item(f.arg_type, rest)
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            push(opt_arg, a)
            arg_consumed = true
        end
        for i = (1+corr):num
            last_ind += 1
            a = _parse_item(f.arg_type, args_list[last_ind])
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            push(opt_arg, a)
        end
    elseif f.nargs.desc == 'A'
        if !(rest === nothing)
            a = _parse_item(f.arg_type, rest)
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            opt_arg = a
            arg_consumed = true
        else
            if length(args_list) - last_ind < 1
                @assert is_opt
                _argparse_error("option $name requires an argument")
            end
            last_ind += 1
            a = _parse_item(f.arg_type, args_list[last_ind])
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            opt_arg = a
        end
    elseif f.nargs.desc == '?'
        if !is_opt
            _found_a_bug()
        end
        if !(rest === nothing)
            a = _parse_item(f.arg_type, rest)
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            opt_arg = a
            arg_consumed = true
        else
            if length(args_list) - last_ind < 1
                opt_arg = deepcopy(f.constant)
            else
                last_ind += 1
                a = _parse_item(f.arg_type, args_list[last_ind])
                if !_test_range(f.range_tester, a)
                    _err_arg_outofrange(name, a, is_opt)
                end
                opt_arg = a
            end
        end
    elseif f.nargs.desc == '*' || f.nargs.desc == '+'
        arg_found = false
        if !(rest === nothing)
            a = _parse_item(f.arg_type, rest)
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            push(opt_arg, a)
            arg_consumed = true
            arg_found = true
        end
        while last_ind < length(args_list)
            if !arg_delim_found && _looks_like_an_option(args_list[last_ind+1], settings)
                break
            end
            last_ind += 1
            a = _parse_item(f.arg_type, args_list[last_ind])
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            push(opt_arg, a)
            arg_found = true
        end
        if f.nargs.desc == '+' && !arg_found
            @assert is_opt
            _argparse_error("option $name requires at least one (not-looking-like-an-option) argument")
        end
    elseif f.nargs.desc == 'R'
        if !(rest === nothing)
            a = _parse_item(f.arg_type, rest)
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            push(opt_arg, a)
            arg_consumed = true
        end
        while last_ind < length(args_list)
            last_ind += 1
            a = _parse_item(f.arg_type, args_list[last_ind])
            if !_test_range(f.range_tester, a)
                _err_arg_outofrange(name, a, is_opt)
            end
            push(opt_arg, a)
        end
    else
        _found_a_bug()
    end
    if f.action == :store_arg
        out_dict[f.dest_name] = opt_arg
    elseif f.action == :append_arg
        push(out_dict[f.dest_name], opt_arg)
    elseif f.action == :command_arg
        out_dict[f.dest_name] = opt_arg
        command = opt_arg
    else
        _found_a_bug()
    end
    return out_dict, last_ind, arg_consumed, command
end
#}}}

# parse long opts
#{{{
function _parse_long_opt(settings::ArgParseSettings, opt_name::String, last_ind::Int, arg_after_eq::Union(String,Nothing), args_list::Vector, out_dict::Dict)
    local f::ArgParseField
    local fln::String
    exact_match = false
    nfound = 0
    for g in settings.args_table.fields
        for ln in g.long_opt_name
            if ln == opt_name
                exact_match = true
                nfound = 1
                f = g
                fln = ln
                break
            elseif begins_with(ln, opt_name)
                nfound += 1
                f = g
                fln = ln
            end
        end
        if exact_match
            break
        end
    end
    if nfound == 0
        _argparse_error("unrecognized option --$opt_name")
    elseif nfound > 1
        _argparse_error("long option --$opt_name is ambiguous ($nfound partial matches)")
    end

    opt_name = fln

    if _is_flag(f)
        out_dict, command = _parse1_flag(settings, f, !(arg_after_eq === nothing), "--"*opt_name, out_dict)
    else
        out_dict, last_ind, arg_consumed, command =
                _parse1_optarg(settings, f, arg_after_eq, args_list, "--"*opt_name,
                               true, false,
                               out_dict, last_ind)
    end
    return last_ind, command, out_dict
end
#}}}

# parse short opts
#{{{
function _parse_short_opt(settings::ArgParseSettings, shopts_lst::String, last_ind::Int, args_list::Vector, out_dict::Dict)
    command = nothing
    rest_as_arg = nothing
    sind = start(shopts_lst)
    while !done(shopts_lst, sind)
        opt_char, next_sind = next(shopts_lst, sind)
        if !done(shopts_lst, next_sind)
            next_opt_char, next2_sind = next(shopts_lst, next_sind)
            if next_opt_char == '='
                next_is_eq = true
                rest_as_arg = shopts_lst[next2_sind:end]
            else
                next_is_eq = false
                rest_as_arg = shopts_lst[next_sind:end]
            end
        else
            next_is_eq = false
            rest_as_arg = nothing
        end

        opt_name = string(opt_char)
        arg_consumed = false

        local f::ArgParseField
        found = false
        for f in settings.args_table.fields
            for sn in f.short_opt_name
                if sn == opt_name
                    found = true
                    break
                end
            end
            if found
                break
            end
        end
        if !found
            _argparse_error("unrecognized option -$opt_name")
        end
        if _is_flag(f)
            out_dict, command = _parse1_flag(settings, f, next_is_eq, "-"*opt_name, out_dict)
        else
            out_dict, last_ind, arg_consumed, command =
                    _parse1_optarg(settings, f, rest_as_arg, args_list, "-"*opt_name,
                                   true, false,
                                   out_dict, last_ind)
        end
        if arg_consumed
            break
        end
        if !(command === nothing) && !(rest_as_arg === nothing)
            if isempty(rest_as_arg)
                rest_as_arg = nothing
            end
            break
        end
        sind = next_sind
    end
    return last_ind, command, rest_as_arg, out_dict
end
#}}}

# parse args
#{{{
function _parse_arg(settings::ArgParseSettings, last_ind::Int, last_arg::Int, arg_delim_found::Bool, args_list::Vector, out_dict::Dict)
    local new_arg_ind
    found = false
    local f::ArgParseField
    for new_arg_ind = last_arg+1:length(settings.args_table.fields)
        f = settings.args_table.fields[new_arg_ind]
        if _is_arg(f) && f.fake == false
            found = true
            break
        end
    end
    if !found
        _argparse_error("too many arguments")
    end

    out_dict, last_ind, arg_consumed, command =
            _parse1_optarg(settings, f, nothing, args_list, f.dest_name,
                           false, arg_delim_found,
                           out_dict, last_ind-1)

    return last_ind, new_arg_ind, command, out_dict
end
#}}}
#}}}


end # module ArgParse
