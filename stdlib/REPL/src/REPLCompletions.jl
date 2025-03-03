# This file is a part of Julia. License is MIT: https://julialang.org/license

module REPLCompletions

export completions, shell_completions, bslash_completions, completion_text, named_completion

using Core: Const
# We want to insulate the REPLCompletion module from any changes the user may
# make to the compiler, since it runs by default and the system becomes unusable
# if it breaks.
const CC = Base.Compiler
using Base.Meta
using Base: propertynames, something, IdSet
using Base.Filesystem: _readdirx

using ..REPL.LineEdit: NamedCompletion

abstract type Completion end

struct TextCompletion <: Completion
    text::String
end

struct KeywordCompletion <: Completion
    keyword::String
end

struct KeyvalCompletion <: Completion
    keyval::String
end

struct PathCompletion <: Completion
    path::String
end

struct ModuleCompletion <: Completion
    parent::Module
    mod::String
end

struct PackageCompletion <: Completion
    package::String
end

struct PropertyCompletion <: Completion
    value
    property::Symbol
end

struct FieldCompletion <: Completion
    typ::DataType
    field::Symbol
end

struct MethodCompletion <: Completion
    tt # may be used by an external consumer to infer return type, etc.
    method::Method
    MethodCompletion(@nospecialize(tt), method::Method) = new(tt, method)
end

struct BslashCompletion <: Completion
    completion::String # what is actually completed, for example "\trianglecdot"
    name::String # what is displayed, for example "◬ \trianglecdot"
end
BslashCompletion(completion::String) = BslashCompletion(completion, completion)

struct ShellCompletion <: Completion
    text::String
end

struct DictCompletion <: Completion
    dict::AbstractDict
    key::String
end

struct KeywordArgumentCompletion <: Completion
    kwarg::String
end

# interface definition
function Base.getproperty(c::Completion, name::Symbol)
    if name === :text
        return getfield(c, :text)::String
    elseif name === :keyword
        return getfield(c, :keyword)::String
    elseif name === :path
        return getfield(c, :path)::String
    elseif name === :parent
        return getfield(c, :parent)::Module
    elseif name === :mod
        return getfield(c, :mod)::String
    elseif name === :package
        return getfield(c, :package)::String
    elseif name === :property
        return getfield(c, :property)::Symbol
    elseif name === :field
        return getfield(c, :field)::Symbol
    elseif name === :method
        return getfield(c, :method)::Method
    elseif name === :bslash
        return getfield(c, :bslash)::String
    elseif name === :text
        return getfield(c, :text)::String
    elseif name === :key
        return getfield(c, :key)::String
    elseif name === :kwarg
        return getfield(c, :kwarg)::String
    end
    return getfield(c, name)
end

_completion_text(c::TextCompletion) = c.text
_completion_text(c::KeywordCompletion) = c.keyword
_completion_text(c::KeyvalCompletion) = c.keyval
_completion_text(c::PathCompletion) = c.path
_completion_text(c::ModuleCompletion) = c.mod
_completion_text(c::PackageCompletion) = c.package
_completion_text(c::PropertyCompletion) = sprint(Base.show_sym, c.property)
_completion_text(c::FieldCompletion) = sprint(Base.show_sym, c.field)
_completion_text(c::MethodCompletion) = repr(c.method)
_completion_text(c::ShellCompletion) = c.text
_completion_text(c::DictCompletion) = c.key
_completion_text(c::KeywordArgumentCompletion) = c.kwarg*'='

completion_text(c) = _completion_text(c)::String

named_completion(c::BslashCompletion) = NamedCompletion(c.completion, c.name)

function named_completion(c)
    text = completion_text(c)::String
    return NamedCompletion(text, text)
end

named_completion_completion(c) = named_completion(c).completion::String

const Completions = Tuple{Vector{Completion}, UnitRange{Int}, Bool}

function completes_global(x, name)
    return startswith(x, name) && !('#' in x)
end

function appendmacro!(syms, macros, needle, endchar)
    for macsym in macros
        s = String(macsym)
        if endswith(s, needle)
            from = nextind(s, firstindex(s))
            to = prevind(s, sizeof(s)-sizeof(needle)+1)
            push!(syms, s[from:to]*endchar)
        end
    end
end

function append_filtered_mod_names!(ffunc::Function, suggestions::Vector{Completion},
                                    mod::Module, name::String, complete_internal_only::Bool)
    imported = usings = !complete_internal_only
    ssyms = names(mod; all=true, imported, usings)
    filter!(ffunc, ssyms)
    macros = filter(x -> startswith(String(x), "@" * name), ssyms)

    # don't complete string and command macros when the input matches the internal name like `r_` to `r"`
    if !startswith(name, "@")
        filter!(macros) do m
            s = String(m)
            if endswith(s, "_str") || endswith(s, "_cmd")
                occursin(name, first(s, length(s)-4))
            else
                true
            end
        end
    end

    syms = String[sprint((io,s)->Base.show_sym(io, s; allow_macroname=true), s) for s in ssyms if completes_global(String(s), name)]
    appendmacro!(syms, macros, "_str", "\"")
    appendmacro!(syms, macros, "_cmd", "`")
    for sym in syms
        push!(suggestions, ModuleCompletion(mod, sym))
    end
    return suggestions
end

# REPL Symbol Completions
function complete_symbol!(suggestions::Vector{Completion},
                          @nospecialize(prefix), name::String, context_module::Module;
                          complete_modules_only::Bool=false,
                          shift::Bool=false)
    local mod, t, val
    complete_internal_only = false
    if prefix !== nothing
        res = repl_eval_ex(prefix, context_module)
        res === nothing && return Completion[]
        if res isa Const
            val = res.val
            if isa(val, Module)
                mod = val
                if !shift
                    # when module is explicitly accessed, show internal bindings that are
                    # defined by the module, unless shift key is pressed
                    complete_internal_only = true
                end
            else
                t = typeof(val)
            end
        else
            t = CC.widenconst(res)
        end
    else
        mod = context_module
    end

    if @isdefined(mod) # lookup names available within the module
        let modname = nameof(mod),
            is_main = mod===Main
            append_filtered_mod_names!(suggestions, mod, name, complete_internal_only) do s::Symbol
                if Base.isdeprecated(mod, s)
                    return false
                elseif s === modname
                    return false # exclude `Main.Main.Main`, etc.
                elseif complete_modules_only && !completes_module(mod, s)
                    return false
                elseif is_main && s === :MainInclude
                    return false
                end
                return true
            end
        end
    elseif @isdefined(val) # looking for a property of an instance
        try
            for property in propertynames(val, false)
                # TODO: support integer arguments (#36872)
                if property isa Symbol && startswith(string(property), name)
                    push!(suggestions, PropertyCompletion(val, property))
                end
            end
        catch
        end
    elseif @isdefined(t) && field_completion_eligible(t)
        # Looking for a member of a type
        add_field_completions!(suggestions, name, t)
    end
    return suggestions
end

completes_module(mod::Module, x::Symbol) = isdefined(mod, x) && isa(getglobal(mod, x), Module)

function add_field_completions!(suggestions::Vector{Completion}, name::String, @nospecialize(t))
    if isa(t, Union)
        add_field_completions!(suggestions, name, t.a)
        add_field_completions!(suggestions, name, t.b)
    else
        @assert isconcretetype(t)
        fields = fieldnames(t)
        for field in fields
            isa(field, Symbol) || continue # Tuple type has ::Int field name
            s = string(field)
            if startswith(s, name)
                push!(suggestions, FieldCompletion(t, field))
            end
        end
    end
end

const GENERIC_PROPERTYNAMES_METHOD = which(propertynames, (Any,))

function field_completion_eligible(@nospecialize t)
    if isa(t, Union)
        return field_completion_eligible(t.a) && field_completion_eligible(t.b)
    end
    isconcretetype(t) || return false
    # field completion is correct only when `getproperty` fallbacks to `getfield`
    match = Base._which(Tuple{typeof(propertynames),t}; raise=false)
    match === nothing && return false
    return match.method === GENERIC_PROPERTYNAMES_METHOD
end

function complete_from_list!(suggestions::Vector{Completion}, T::Type, list::Vector{String}, s::String)
    r = searchsorted(list, s)
    i = first(r)
    n = length(list)
    while i <= n && startswith(list[i],s)
        r = first(r):i
        i += 1
    end
    for kw in list[r]
        push!(suggestions, T(kw))
    end
    return suggestions
end

const sorted_keywords = [
    "abstract type", "baremodule", "begin", "break", "catch", "ccall",
    "const", "continue", "do", "else", "elseif", "end", "export",
    "finally", "for", "function", "global", "if", "import",
    "let", "local", "macro", "module", "mutable struct",
    "primitive type", "quote", "return", "struct",
    "try", "using", "while"]

complete_keyword!(suggestions::Vector{Completion}, s::String) =
    complete_from_list!(suggestions, KeywordCompletion, sorted_keywords, s)

const sorted_keyvals = ["false", "true"]

complete_keyval!(suggestions::Vector{Completion}, s::String) =
    complete_from_list!(suggestions, KeyvalCompletion, sorted_keyvals, s)

function do_raw_escape(s)
    # escape_raw_string with delim='`' and ignoring the rule for the ending \
    return replace(s, r"(\\+)`" => s"\1\\`")
end
function do_shell_escape(s)
    return Base.shell_escape_posixly(s)
end
function do_string_escape(s)
    return escape_string(s, ('\"','$'))
end

const PATH_cache_lock = Base.ReentrantLock()
const PATH_cache = Set{String}()
PATH_cache_task::Union{Task,Nothing} = nothing # used for sync in tests
next_cache_update::Float64 = 0.0
function maybe_spawn_cache_PATH()
    global PATH_cache_task, next_cache_update
    @lock PATH_cache_lock begin
        PATH_cache_task isa Task && !istaskdone(PATH_cache_task) && return
        time() < next_cache_update && return
        PATH_cache_task = Threads.@spawn begin
            REPLCompletions.cache_PATH()
            @lock PATH_cache_lock PATH_cache_task = nothing # release memory when done
        end
        Base.errormonitor(PATH_cache_task)
    end
end

# caches all reachable files in PATH dirs
function cache_PATH()
    path = get(ENV, "PATH", nothing)
    path isa String || return

    global next_cache_update

    # Calling empty! on PATH_cache would be annoying for async typing hints as completions would temporarily disappear.
    # So keep track of what's added this time and at the end remove any that didn't appear this time from the global cache.
    this_PATH_cache = Set{String}()

    @debug "caching PATH files" PATH=path
    pathdirs = split(path, @static Sys.iswindows() ? ";" : ":")

    next_yield_time = time() + 0.01

    t = @elapsed for pathdir in pathdirs
        actualpath = try
            realpath(pathdir)
        catch ex
            ex isa Base.IOError || rethrow()
            # Bash doesn't expect every folder in PATH to exist, so neither shall we
            continue
        end

        if actualpath != pathdir && in(actualpath, pathdirs)
            # Remove paths which (after resolving links) are in the env path twice.
            # Many distros eg. point /bin to /usr/bin but have both in the env path.
            continue
        end

        path_entries = try
            _readdirx(pathdir)
        catch e
            # Bash allows dirs in PATH that can't be read, so we should as well.
            if isa(e, Base.IOError) || isa(e, Base.ArgumentError)
                continue
            else
                # We only handle IOError and ArgumentError here
                rethrow()
            end
        end
        for entry in path_entries
            # In a perfect world, we would filter on whether the file is executable
            # here, or even on whether the current user can execute the file in question.
            try
                if isfile(entry)
                    @lock PATH_cache_lock push!(PATH_cache, entry.name)
                    push!(this_PATH_cache, entry.name)
                end
            catch e
                # `isfile()` can throw in rare cases such as when probing a
                # symlink that points to a file within a directory we do not
                # have read access to.
                if isa(e, Base.IOError)
                    continue
                else
                    rethrow()
                end
            end
            if time() >= next_yield_time
                yield() # to avoid blocking typing when -t1
                next_yield_time = time() + 0.01
            end
        end
    end

    @lock PATH_cache_lock begin
        intersect!(PATH_cache, this_PATH_cache) # remove entries from PATH_cache that weren't found this time
        next_cache_update = time() + 10 # earliest next update can run is 10s after
    end

    @debug "caching PATH files took $t seconds" length(pathdirs) length(PATH_cache)
    return PATH_cache
end

function complete_path(path::AbstractString;
                       use_envpath=false,
                       shell_escape=false,
                       raw_escape=false,
                       string_escape=false,
                       contract_user=false)
    @assert !(shell_escape && string_escape)
    if Base.Sys.isunix() && occursin(r"^~(?:/|$)", path)
        # if the path is just "~", don't consider the expanded username as a prefix
        if path == "~"
            dir, prefix = homedir(), ""
        else
            dir, prefix = splitdir(homedir() * path[2:end])
        end
    else
        dir, prefix = splitdir(path)
    end
    entries = try
        if isempty(dir)
            _readdirx()
        elseif isdir(dir)
            _readdirx(dir)
        else
            return Completion[], dir, false
        end
    catch ex
        ex isa Base.IOError || rethrow()
        return Completion[], dir, false
    end

    matches = Set{String}()
    for entry in entries
        if startswith(entry.name, prefix)
            is_dir = try isdir(entry) catch ex; ex isa Base.IOError ? false : rethrow() end
            push!(matches, is_dir ? entry.name * "/" : entry.name)
        end
    end

    if use_envpath && isempty(dir)
        # Look for files in PATH as well. These are cached in `cache_PATH` in an async task to not block typing.
        # If we cannot get lock because its still caching just pass over this so that typing isn't laggy.
        maybe_spawn_cache_PATH() # only spawns if enough time has passed and the previous caching task has completed
        @lock PATH_cache_lock begin
            for file in PATH_cache
                startswith(file, prefix) && push!(matches, file)
            end
        end
    end

    matches = ((shell_escape ? do_shell_escape(s) : string_escape ? do_string_escape(s) : s) for s in matches)
    matches = ((raw_escape ? do_raw_escape(s) : s) for s in matches)
    matches = Completion[PathCompletion(contract_user ? contractuser(s) : s) for s in matches]
    return matches, dir, !isempty(matches)
end

function complete_path(path::AbstractString,
                       pos::Int;
                       use_envpath=false,
                       shell_escape=false,
                       string_escape=false,
                       contract_user=false)
    ## TODO: enable this depwarn once Pkg is fixed
    #Base.depwarn("complete_path with pos argument is deprecated because the return value [2] is incorrect to use", :complete_path)
    paths, dir, success = complete_path(path; use_envpath, shell_escape, string_escape)
    if Base.Sys.isunix() && occursin(r"^~(?:/|$)", path)
        # if the path is just "~", don't consider the expanded username as a prefix
        if path == "~"
            dir, prefix = homedir(), ""
        else
            dir, prefix = splitdir(homedir() * path[2:end])
        end
    else
        dir, prefix = splitdir(path)
    end
    startpos = pos - lastindex(prefix) + 1
    Sys.iswindows() && map!(paths, paths) do c::PathCompletion
        # emulation for unnecessarily complicated return value, since / is a
        # perfectly acceptable path character which does not require quoting
        # but is required by Pkg's awkward parser handling
        return endswith(c.path, "/") ? PathCompletion(chop(c.path) * "\\\\") : c
    end
    return paths, startpos:pos, success
end

function complete_expanduser(path::AbstractString, r)
    expanded =
        try expanduser(path)
        catch e
            e isa ArgumentError || rethrow()
            path
        end
    return Completion[PathCompletion(expanded)], r, path != expanded
end

# Returns a range that includes the method name in front of the first non
# closed start brace from the end of the string.
function find_start_brace(s::AbstractString; c_start='(', c_end=')')
    r = reverse(s)
    i = firstindex(r)
    braces = in_comment = 0
    in_single_quotes = in_double_quotes = in_back_ticks = false
    num_single_quotes_in_string = count('\'', s)
    while i <= ncodeunits(r)
        c, i = iterate(r, i)
        if c == '#' && i <= ncodeunits(r) && iterate(r, i)[1] == '='
            c, i = iterate(r, i) # consume '='
            new_comments = 1
            # handle #=#=#=#, by counting =# pairs
            while i <= ncodeunits(r) && iterate(r, i)[1] == '#'
                c, i = iterate(r, i) # consume '#'
                iterate(r, i)[1] == '=' || break
                c, i = iterate(r, i) # consume '='
                new_comments += 1
            end
            if c == '='
                in_comment += new_comments
            else
                in_comment -= new_comments
            end
        elseif !in_single_quotes && !in_double_quotes && !in_back_ticks && in_comment == 0
            if c == c_start
                braces += 1
            elseif c == c_end
                braces -= 1
            elseif c == '\'' && num_single_quotes_in_string % 2 == 0
                # ' can be a transpose too, so check if there are even number of 's in the string
                # TODO: This probably needs to be more robust
                in_single_quotes = true
            elseif c == '"'
                in_double_quotes = true
            elseif c == '`'
                in_back_ticks = true
            end
        else
            if in_single_quotes &&
                c == '\'' && i <= ncodeunits(r) && iterate(r, i)[1] != '\\'
                in_single_quotes = false
            elseif in_double_quotes &&
                c == '"' && i <= ncodeunits(r) && iterate(r, i)[1] != '\\'
                in_double_quotes = false
            elseif in_back_ticks &&
                c == '`' && i <= ncodeunits(r) && iterate(r, i)[1] != '\\'
                in_back_ticks = false
            elseif in_comment > 0 &&
                c == '=' && i <= ncodeunits(r) && iterate(r, i)[1] == '#'
                # handle =#=#=#=, by counting #= pairs
                c, i = iterate(r, i) # consume '#'
                old_comments = 1
                while i <= ncodeunits(r) && iterate(r, i)[1] == '='
                    c, i = iterate(r, i) # consume '='
                    iterate(r, i)[1] == '#' || break
                    c, i = iterate(r, i) # consume '#'
                    old_comments += 1
                end
                if c == '#'
                    in_comment -= old_comments
                else
                    in_comment += old_comments
                end
            end
        end
        braces == 1 && break
    end
    braces != 1 && return 0:-1, -1
    method_name_end = reverseind(s, i)
    startind = nextind(s, something(findprev(in(non_identifier_chars), s, method_name_end), 0))::Int
    return (startind:lastindex(s), method_name_end)
end

struct REPLCacheToken end

struct REPLInterpreter <: CC.AbstractInterpreter
    limit_aggressive_inference::Bool
    world::UInt
    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::Vector{CC.InferenceResult}
    function REPLInterpreter(limit_aggressive_inference::Bool=false;
                             world::UInt = Base.get_world_counter(),
                             inf_params::CC.InferenceParams = CC.InferenceParams(;
                                 aggressive_constant_propagation=true),
                             opt_params::CC.OptimizationParams = CC.OptimizationParams(),
                             inf_cache::Vector{CC.InferenceResult} = CC.InferenceResult[])
        return new(limit_aggressive_inference, world, inf_params, opt_params, inf_cache)
    end
end
CC.InferenceParams(interp::REPLInterpreter) = interp.inf_params
CC.OptimizationParams(interp::REPLInterpreter) = interp.opt_params
CC.get_inference_world(interp::REPLInterpreter) = interp.world
CC.get_inference_cache(interp::REPLInterpreter) = interp.inf_cache
CC.cache_owner(::REPLInterpreter) = REPLCacheToken()

# REPLInterpreter is only used for type analysis, so it should disable optimization entirely
CC.may_optimize(::REPLInterpreter) = false

# REPLInterpreter doesn't need any sources to be cached, so discard them aggressively
CC.transform_result_for_cache(::REPLInterpreter, ::CC.InferenceResult, edges::Core.SimpleVector) = nothing

# REPLInterpreter analyzes a top-level frame, so better to not bail out from it
CC.bail_out_toplevel_call(::REPLInterpreter, ::CC.InferenceLoopState, ::CC.InferenceState) = false

# `REPLInterpreter` aggressively resolves global bindings to enable reasonable completions
# for lines like `Mod.a.|` (where `|` is the cursor position).
# Aggressive binding resolution poses challenges for the inference cache validation
# (until https://github.com/JuliaLang/julia/issues/40399 is implemented).
# To avoid the cache validation issues, `REPLInterpreter` only allows aggressive binding
# resolution for top-level frame representing REPL input code and for child uncached frames
# that are constant propagated from the top-level frame ("repl-frame"s). This works, even if
# those global bindings are not constant and may be mutated in the future, since:
# a.) "repl-frame"s are never cached, and
# b.) mutable values are never observed by any cached frames.
#
# `REPLInterpreter` also aggressively concrete evaluate `:inconsistent` calls within
# "repl-frame" to provide reasonable completions for lines like `Ref(Some(42))[].|`.
# Aggressive concrete evaluation allows us to get accurate type information about complex
# expressions that otherwise can not be constant folded, in a safe way, i.e. it still
# doesn't evaluate effectful expressions like `pop!(xs)`.
# Similarly to the aggressive binding resolution, aggressive concrete evaluation doesn't
# present any cache validation issues because "repl-frame" is never cached.

# `REPLInterpreter` is specifically used by `repl_eval_ex`, where all top-level frames are
# `repl_frame` always. However, this assumption wouldn't stand if `REPLInterpreter` were to
# be employed, for instance, by `typeinf_ext_toplevel`.
is_repl_frame(sv::CC.InferenceState) = sv.linfo.def isa Module && sv.cache_mode === CC.CACHE_MODE_NULL

function is_call_graph_uncached(sv::CC.InferenceState)
    CC.is_cached(sv) && return false
    parent = CC.frame_parent(sv)
    parent === nothing && return true
    return is_call_graph_uncached(parent::CC.InferenceState)
end

# aggressive global binding resolution within `repl_frame`
function CC.abstract_eval_globalref(interp::REPLInterpreter, g::GlobalRef, bailed::Bool,
                                    sv::CC.InferenceState)
    # Ignore saw_latestworld
    if (interp.limit_aggressive_inference ? is_repl_frame(sv) : is_call_graph_uncached(sv))
        partition = CC.abstract_eval_binding_partition!(interp, g, sv)
        if CC.is_defined_const_binding(CC.binding_kind(partition))
            return CC.RTEffects(Const(CC.partition_restriction(partition)), Union{}, CC.EFFECTS_TOTAL)
        else
            b = convert(Core.Binding, g)
            if CC.binding_kind(partition) == CC.PARTITION_KIND_GLOBAL && isdefined(b, :value)
                return CC.RTEffects(Const(b.value), Union{}, CC.EFFECTS_TOTAL)
            end
        end
        return CC.RTEffects(Union{}, UndefVarError, CC.EFFECTS_THROWS)
    end
    return @invoke CC.abstract_eval_globalref(interp::CC.AbstractInterpreter, g::GlobalRef, bailed::Bool,
                                              sv::CC.InferenceState)
end

function is_repl_frame_getproperty(sv::CC.InferenceState)
    def = sv.linfo.def
    def isa Method || return false
    def.name === :getproperty || return false
    CC.is_cached(sv) && return false
    return is_repl_frame(CC.frame_parent(sv))
end

# aggressive global binding resolution for `getproperty(::Module, ::Symbol)` calls within `repl_frame`
function CC.builtin_tfunction(interp::REPLInterpreter, @nospecialize(f),
                              argtypes::Vector{Any}, sv::CC.InferenceState)
    if f === Core.getglobal && (interp.limit_aggressive_inference ? is_repl_frame_getproperty(sv) : is_call_graph_uncached(sv))
        if length(argtypes) == 2
            a1, a2 = argtypes
            if isa(a1, Const) && isa(a2, Const)
                a1val, a2val = a1.val, a2.val
                if isa(a1val, Module) && isa(a2val, Symbol)
                    g = GlobalRef(a1val, a2val)
                    if isdefined_globalref(g)
                        return Const(ccall(:jl_get_globalref_value, Any, (Any,), g))
                    end
                    return Union{}
                end
            end
        end
    end
    return @invoke CC.builtin_tfunction(interp::CC.AbstractInterpreter, f::Any,
                                        argtypes::Vector{Any}, sv::CC.InferenceState)
end

# aggressive concrete evaluation for `:inconsistent` frames within `repl_frame`
function CC.concrete_eval_eligible(interp::REPLInterpreter, @nospecialize(f),
                                   result::CC.MethodCallResult, arginfo::CC.ArgInfo,
                                   sv::CC.InferenceState)
    if (interp.limit_aggressive_inference ? is_repl_frame(sv) : is_call_graph_uncached(sv))
        neweffects = CC.Effects(result.effects; consistent=CC.ALWAYS_TRUE)
        result = CC.MethodCallResult(result.rt, result.exct, neweffects, result.edge,
                                     result.edgecycle, result.edgelimited, result.volatile_inf_result)
    end
    ret = @invoke CC.concrete_eval_eligible(interp::CC.AbstractInterpreter, f::Any,
                                            result::CC.MethodCallResult, arginfo::CC.ArgInfo,
                                            sv::CC.InferenceState)
    if ret === :semi_concrete_eval
        # while the base eligibility check probably won't permit semi-concrete evaluation
        # for `REPLInterpreter` (given it completely turns off optimization),
        # this ensures we don't inadvertently enter irinterp
        ret = :none
    end
    return ret
end

# allow constant propagation for mutable constants
function CC.const_prop_argument_heuristic(interp::REPLInterpreter, arginfo::CC.ArgInfo, sv::CC.InferenceState)
    if !interp.limit_aggressive_inference
        any(@nospecialize(a)->isa(a, Const), arginfo.argtypes) && return true # even if mutable
    end
    return @invoke CC.const_prop_argument_heuristic(interp::CC.AbstractInterpreter, arginfo::CC.ArgInfo, sv::CC.InferenceState)
end

function resolve_toplevel_symbols!(src::Core.CodeInfo, mod::Module)
    @ccall jl_resolve_definition_effects_in_ir(
        #=jl_array_t *stmts=# src.code::Any,
        #=jl_module_t *m=# mod::Any,
        #=jl_svec_t *sparam_vals=# Core.svec()::Any,
        #=jl_value_t *binding_edge=# C_NULL::Ptr{Cvoid},
        #=int binding_effects=# 0::Int)::Cvoid
    return src
end

# lower `ex` and run type inference on the resulting top-level expression
function repl_eval_ex(@nospecialize(ex), context_module::Module; limit_aggressive_inference::Bool=false)
    if (isexpr(ex, :toplevel) || isexpr(ex, :tuple)) && !isempty(ex.args)
        # get the inference result for the last expression
        ex = ex.args[end]
    end
    lwr = try
        Meta.lower(context_module, ex)
    catch # macro expansion failed, etc.
        return nothing
    end
    if lwr isa Symbol
        return isdefined(context_module, lwr) ? Const(getfield(context_module, lwr)) : nothing
    end
    lwr isa Expr || return Const(lwr) # `ex` is literal
    isexpr(lwr, :thunk) || return nothing # lowered to `Expr(:error, ...)` or similar
    src = lwr.args[1]::Core.CodeInfo

    resolve_toplevel_symbols!(src, context_module)
    # construct top-level `MethodInstance`
    mi = ccall(:jl_method_instance_for_thunk, Ref{Core.MethodInstance}, (Any, Any), src, context_module)

    interp = REPLInterpreter(limit_aggressive_inference)
    result = CC.InferenceResult(mi)
    frame = CC.InferenceState(result, src, #=cache=#:no, interp)

    # NOTE Use the fixed world here to make `REPLInterpreter` robust against
    #      potential invalidations of `Core.Compiler` methods.
    Base.invoke_in_world(COMPLETION_WORLD[], CC.typeinf, interp, frame)

    result = frame.result.result
    result === Union{} && return nothing # for whatever reason, callers expect this as the Bottom and/or Top type instead
    return result
end

# `COMPLETION_WORLD[]` will be initialized within `__init__`
# (to allow us to potentially remove REPL from the sysimage in the future).
# Note that inference from the `code_typed` call below will use the current world age
# rather than `typemax(UInt)`, since `Base.invoke_in_world` uses the current world age
# when the given world age is higher than the current one.
const COMPLETION_WORLD = Ref{UInt}(typemax(UInt))

# Generate code cache for `REPLInterpreter` now:
# This code cache will be available at the world of `COMPLETION_WORLD`,
# assuming no invalidation will happen before initializing REPL.
# Once REPL is loaded, `REPLInterpreter` will be resilient against future invalidations.
code_typed(CC.typeinf, (REPLInterpreter, CC.InferenceState))

# Method completion on function call expression that look like :(max(1))
MAX_METHOD_COMPLETIONS::Int = 40
function _complete_methods(ex_org::Expr, context_module::Module, shift::Bool)
    funct = repl_eval_ex(ex_org.args[1], context_module)
    funct === nothing && return 2, nothing, [], Set{Symbol}()
    funct = CC.widenconst(funct)
    args_ex, kwargs_ex, kwargs_flag = complete_methods_args(ex_org, context_module, true, true)
    return kwargs_flag, funct, args_ex, kwargs_ex
end

function complete_methods(ex_org::Expr, context_module::Module=Main, shift::Bool=false)
    kwargs_flag, funct, args_ex, kwargs_ex = _complete_methods(ex_org, context_module, shift)::Tuple{Int, Any, Vector{Any}, Set{Symbol}}
    out = Completion[]
    kwargs_flag == 2 && return out # one of the kwargs is invalid
    kwargs_flag == 0 && push!(args_ex, Vararg{Any}) # allow more arguments if there is no semicolon
    complete_methods!(out, funct, args_ex, kwargs_ex, shift ? -2 : MAX_METHOD_COMPLETIONS, kwargs_flag == 1)
    return out
end

MAX_ANY_METHOD_COMPLETIONS::Int = 10
function recursive_explore_names!(seen::IdSet, callee_module::Module, initial_module::Module, exploredmodules::IdSet{Module}=IdSet{Module}())
    push!(exploredmodules, callee_module)
    for name in names(callee_module; all=true, imported=true)
        if !Base.isdeprecated(callee_module, name) && !startswith(string(name), '#') && isdefined(initial_module, name)
            func = getfield(callee_module, name)
            if !isa(func, Module)
                funct = Core.Typeof(func)
                push!(seen, funct)
            elseif isa(func, Module) && func ∉ exploredmodules
                recursive_explore_names!(seen, func, initial_module, exploredmodules)
            end
        end
    end
end
function recursive_explore_names(callee_module::Module, initial_module::Module)
    seen = IdSet{Any}()
    recursive_explore_names!(seen, callee_module, initial_module)
    seen
end

function complete_any_methods(ex_org::Expr, callee_module::Module, context_module::Module, moreargs::Bool, shift::Bool)
    out = Completion[]
    args_ex, kwargs_ex, kwargs_flag = try
        # this may throw, since we set default_any to false
        complete_methods_args(ex_org, context_module, false, false)
    catch ex
        ex isa ArgumentError || rethrow()
        return out
    end
    kwargs_flag == 2 && return out # one of the kwargs is invalid

    # moreargs determines whether to accept more args, independently of the presence of a
    # semicolon for the ".?(" syntax
    moreargs && push!(args_ex, Vararg{Any})

    for seen_name in recursive_explore_names(callee_module, callee_module)
        complete_methods!(out, seen_name, args_ex, kwargs_ex, MAX_ANY_METHOD_COMPLETIONS, false)
    end

    if !shift
        # Filter out methods where all arguments are `Any`
        filter!(out) do c
            isa(c, TextCompletion) && return false
            isa(c, MethodCompletion) || return true
            sig = Base.unwrap_unionall(c.method.sig)::DataType
            return !all(@nospecialize(T) -> T === Any || T === Vararg{Any}, sig.parameters[2:end])
        end
    end

    return out
end

function detect_invalid_kwarg!(kwargs_ex::Vector{Symbol}, @nospecialize(x), kwargs_flag::Int, possible_splat::Bool)
    n = isexpr(x, :kw) ? x.args[1] : x
    if n isa Symbol
        push!(kwargs_ex, n)
        return kwargs_flag
    end
    possible_splat && isexpr(x, :...) && return kwargs_flag
    return 2 # The kwarg is invalid
end

function detect_args_kwargs(funargs::Vector{Any}, context_module::Module, default_any::Bool, broadcasting::Bool)
    args_ex = Any[]
    kwargs_ex = Symbol[]
    kwargs_flag = 0
    # kwargs_flag is:
    # * 0 if there is no semicolon and no invalid kwarg
    # * 1 if there is a semicolon and no invalid kwarg
    # * 2 if there are two semicolons or more, or if some kwarg is invalid, which
    #        means that it is not of the form "bar=foo", "bar" or "bar..."
    for i in (1+!broadcasting):length(funargs)
        ex = funargs[i]
        if isexpr(ex, :parameters)
            kwargs_flag = ifelse(kwargs_flag == 0, 1, 2) # there should be at most one :parameters
            for x in ex.args
                kwargs_flag = detect_invalid_kwarg!(kwargs_ex, x, kwargs_flag, true)
            end
        elseif isexpr(ex, :kw)
            kwargs_flag = detect_invalid_kwarg!(kwargs_ex, ex, kwargs_flag, false)
        else
            if broadcasting
                # handle broadcasting, but only handle number of arguments instead of
                # argument types
                push!(args_ex, Any)
            else
                argt = repl_eval_ex(ex, context_module)
                if argt !== nothing
                    push!(args_ex, CC.widenconst(argt))
                elseif default_any
                    push!(args_ex, Any)
                else
                    throw(ArgumentError("argument not found"))
                end
            end
        end
    end
    return args_ex, Set{Symbol}(kwargs_ex), kwargs_flag
end

is_broadcasting_expr(ex::Expr) = ex.head === :. && isexpr(ex.args[2], :tuple)

function complete_methods_args(ex::Expr, context_module::Module, default_any::Bool, allow_broadcasting::Bool)
    if allow_broadcasting && is_broadcasting_expr(ex)
        return detect_args_kwargs((ex.args[2]::Expr).args, context_module, default_any, true)
    end
    return detect_args_kwargs(ex.args, context_module, default_any, false)
end

function complete_methods!(out::Vector{Completion}, @nospecialize(funct), args_ex::Vector{Any}, kwargs_ex::Set{Symbol}, max_method_completions::Int, exact_nargs::Bool)
    # Input types and number of arguments
    t_in = Tuple{funct, args_ex...}
    m = Base._methods_by_ftype(t_in, nothing, max_method_completions, Base.get_world_counter(),
        #=ambig=# true, Ref(typemin(UInt)), Ref(typemax(UInt)), Ptr{Int32}(C_NULL))
    if !isa(m, Vector)
        push!(out, TextCompletion(sprint(Base.show_signature_function, funct) * "( too many methods, use SHIFT-TAB to show )"))
        return
    end
    for match in m
        # TODO: if kwargs_ex, filter out methods without kwargs?
        push!(out, MethodCompletion(match.spec_types, match.method))
    end
    # TODO: filter out methods with wrong number of arguments if `exact_nargs` is set
end

include("latex_symbols.jl")
include("emoji_symbols.jl")

const non_identifier_chars = [" \t\n\r\"\\'`\$><=:;|&{}()[],+-*/?%^~"...]
const whitespace_chars = [" \t\n\r"...]
# "\"'`"... is added to whitespace_chars as non of the bslash_completions
# characters contain any of these characters. It prohibits the
# bslash_completions function to try and complete on escaped characters in strings
const bslash_separators = [whitespace_chars..., "\"'`"...]

const subscripts = Dict(k[3]=>v[1] for (k,v) in latex_symbols if startswith(k, "\\_") && length(k)==3)
const subscript_regex = Regex("^\\\\_[" * join(isdigit(k) || isletter(k) ? "$k" : "\\$k" for k in keys(subscripts)) * "]+\\z")
const superscripts = Dict(k[3]=>v[1] for (k,v) in latex_symbols if startswith(k, "\\^") && length(k)==3)
const superscript_regex = Regex("^\\\\\\^[" * join(isdigit(k) || isletter(k) ? "$k" : "\\$k" for k in keys(superscripts)) * "]+\\z")

# Aux function to detect whether we're right after a using or import keyword
function get_import_mode(s::String)
    # allow all of these to start with leading whitespace and macros like @eval and @eval(
    # ^\s*(?:@\w+\s*(?:\(\s*)?)?

    # match simple cases like `using |` and `import  |`
    mod_import_match_simple = match(r"^\s*(?:@\w+\s*(?:\(\s*)?)?\b(using|import)\s*$", s)
    if mod_import_match_simple !== nothing
        if mod_import_match_simple[1] == "using"
            return :using_module
        else
            return :import_module
        end
    end
    # match module import statements like `using Foo|`, `import Foo, Bar|` and `using Foo.Bar, Baz, |`
    mod_import_match = match(r"^\s*(?:@\w+\s*(?:\(\s*)?)?\b(using|import)\s+([\w\.]+(?:\s*,\s*[\w\.]+)*),?\s*$", s)
    if mod_import_match !== nothing
        if mod_import_match.captures[1] == "using"
            return :using_module
        else
            return :import_module
        end
    end
    # now match explicit name import statements like `using Foo: |` and `import Foo: bar, baz|`
    name_import_match = match(r"^\s*(?:@\w+\s*(?:\(\s*)?)?\b(using|import)\s+([\w\.]+)\s*:\s*([\w@!\s,]+)$", s)
    if name_import_match !== nothing
        if name_import_match[1] == "using"
            return :using_name
        else
            return :import_name
        end
    end
    return nothing
end

function close_path_completion(dir, path, str, pos)
    path = unescape_string(replace(path, "\\\$"=>"\$"))
    path = joinpath(dir, path)
    # ...except if it's a directory...
    Base.isaccessibledir(path) && return false
    # ...and except if there's already a " at the cursor.
    return lastindex(str) <= pos || str[nextind(str, pos)] != '"'
end

function bslash_completions(string::String, pos::Int, hint::Bool=false)
    slashpos = something(findprev(isequal('\\'), string, pos), 0)
    if (something(findprev(in(bslash_separators), string, pos), 0) < slashpos &&
        !(1 < slashpos && (string[prevind(string, slashpos)]=='\\')))
        # latex / emoji symbol substitution
        s = string[slashpos:pos]
        latex = get(latex_symbols, s, "")
        if !isempty(latex) # complete an exact match
            return (true, (Completion[BslashCompletion(latex)], slashpos:pos, true))
        elseif occursin(subscript_regex, s)
            sub = map(c -> subscripts[c], s[3:end])
            return (true, (Completion[BslashCompletion(sub)], slashpos:pos, true))
        elseif occursin(superscript_regex, s)
            sup = map(c -> superscripts[c], s[3:end])
            return (true, (Completion[BslashCompletion(sup)], slashpos:pos, true))
        end
        emoji = get(emoji_symbols, s, "")
        if !isempty(emoji)
            return (true, (Completion[BslashCompletion(emoji)], slashpos:pos, true))
        end
        # return possible matches; these cannot be mixed with regular
        # Julian completions as only latex / emoji symbols contain the leading \
        symbol_dict = startswith(s, "\\:") ? emoji_symbols : latex_symbols
        namelist = Iterators.filter(k -> startswith(k, s), keys(symbol_dict))
        completions = Completion[BslashCompletion(name, "$(symbol_dict[name]) $name") for name in sort!(collect(namelist))]
        return (true, (completions, slashpos:pos, true))
    end
    return (false, (Completion[], 0:-1, false))
end

function dict_identifier_key(str::String, tag::Symbol, context_module::Module=Main)
    if tag === :string
        str_close = str*"\""
    elseif tag === :cmd
        str_close = str*"`"
    else
        str_close = str
    end
    frange, end_of_identifier = find_start_brace(str_close, c_start='[', c_end=']')
    isempty(frange) && return (nothing, nothing, nothing)
    objstr = str[1:end_of_identifier]
    objex = Meta.parse(objstr, raise=false, depwarn=false)
    objt = repl_eval_ex(objex, context_module)
    isa(objt, Core.Const) || return (nothing, nothing, nothing)
    obj = objt.val
    isa(obj, AbstractDict) || return (nothing, nothing, nothing)
    (Base.haslength(obj) && length(obj)::Int < 1_000_000) || return (nothing, nothing, nothing)
    begin_of_key = something(findnext(!isspace, str, nextind(str, end_of_identifier) + 1), # +1 for [
                             lastindex(str)+1)
    return (obj, str[begin_of_key:end], begin_of_key)
end

# This needs to be a separate non-inlined function, see #19441
@noinline function find_dict_matches(identifier::AbstractDict, partial_key)
    matches = String[]
    for key in keys(identifier)
        rkey = repr(key)
        startswith(rkey,partial_key) && push!(matches,rkey)
    end
    return matches
end

# Identify an argument being completed in a method call. If the argument is empty, method
# suggestions will be provided instead of argument completions.
function identify_possible_method_completion(partial, last_idx)
    fail = 0:-1, Expr(:nothing), 0:-1, 0

    # First, check that the last punctuation is either ',', ';' or '('
    idx_last_punct = something(findprev(x -> ispunct(x) && x != '_' && x != '!', partial, last_idx), 0)::Int
    idx_last_punct == 0 && return fail
    last_punct = partial[idx_last_punct]
    last_punct == ',' || last_punct == ';' || last_punct == '(' || return fail

    # Then, check that `last_punct` is only followed by an identifier or nothing
    before_last_word_start = something(findprev(in(non_identifier_chars), partial, last_idx), 0)
    before_last_word_start == 0 && return fail
    all(isspace, @view partial[nextind(partial, idx_last_punct):before_last_word_start]) || return fail

    # Check that `last_punct` is either the last '(' or placed after a previous '('
    frange, method_name_end = find_start_brace(@view partial[1:idx_last_punct])
    method_name_end ∈ frange || return fail

    # Strip the preceding ! operators, if any, and close the expression with a ')'
    s = replace(partial[frange], r"\G\!+([^=\(]+)" => s"\1"; count=1) * ')'
    ex = Meta.parse(s, raise=false, depwarn=false)
    isa(ex, Expr) || return fail

    # `wordrange` is the position of the last argument to complete
    wordrange = nextind(partial, before_last_word_start):last_idx
    return frange, ex, wordrange, method_name_end
end

# Provide completion for keyword arguments in function calls
function complete_keyword_argument(partial::String, last_idx::Int, context_module::Module;
                                   shift::Bool=false)
    frange, ex, wordrange, = identify_possible_method_completion(partial, last_idx)
    fail = Completion[], 0:-1, frange
    ex.head === :call || is_broadcasting_expr(ex) || return fail

    kwargs_flag, funct, args_ex, kwargs_ex = _complete_methods(ex, context_module, true)::Tuple{Int, Any, Vector{Any}, Set{Symbol}}
    kwargs_flag == 2 && return fail # one of the previous kwargs is invalid

    methods = Completion[]
    complete_methods!(methods, funct, Any[Vararg{Any}], kwargs_ex, shift ? -1 : MAX_METHOD_COMPLETIONS, kwargs_flag == 1)
    # TODO: use args_ex instead of Any[Vararg{Any}] and only provide kwarg completion for
    # method calls compatible with the current arguments.

    # For each method corresponding to the function call, provide completion suggestions
    # for each keyword that starts like the last word and that is not already used
    # previously in the expression. The corresponding suggestion is "kwname=".
    # If the keyword corresponds to an existing name, also include "kwname" as a suggestion
    # since the syntax "foo(; kwname)" is equivalent to "foo(; kwname=kwname)".
    last_word = partial[wordrange] # the word to complete
    kwargs = Set{String}()
    for m in methods
        # if MAX_METHOD_COMPLETIONS is hit a single TextCompletion is return by complete_methods! with an explanation
        # which can be ignored here
        m isa TextCompletion && continue
        m::MethodCompletion
        possible_kwargs = Base.kwarg_decl(m.method)
        current_kwarg_candidates = String[]
        for _kw in possible_kwargs
            kw = String(_kw)
            if !endswith(kw, "...") && startswith(kw, last_word) && _kw ∉ kwargs_ex
                push!(current_kwarg_candidates, kw)
            end
        end
        union!(kwargs, current_kwarg_candidates)
    end

    suggestions = Completion[KeywordArgumentCompletion(kwarg) for kwarg in kwargs]

    # Only add these if not in kwarg space. i.e. not in `foo(; `
    if kwargs_flag == 0
        complete_symbol!(suggestions, #=prefix=#nothing, last_word, context_module; shift)
        complete_keyval!(suggestions, last_word)
    end

    return sort!(suggestions, by=named_completion_completion), wordrange
end

function get_loading_candidates(pkgstarts::String, project_file::String)
    loading_candidates = String[]
    d = Base.parsed_toml(project_file)
    pkg = get(d, "name", nothing)::Union{String, Nothing}
    if pkg !== nothing && startswith(pkg, pkgstarts)
        push!(loading_candidates, pkg)
    end
    deps = get(d, "deps", nothing)::Union{Dict{String, Any}, Nothing}
    if deps !== nothing
        for (pkg, _) in deps
            startswith(pkg, pkgstarts) && push!(loading_candidates, pkg)
        end
    end
    return loading_candidates
end

function complete_loading_candidates!(suggestions::Vector{Completion}, pkgstarts::String, project_file::String)
    for name in get_loading_candidates(pkgstarts, project_file)
        push!(suggestions, PackageCompletion(name))
    end
    return suggestions
end

function complete_identifiers!(suggestions::Vector{Completion},
                               context_module::Module, string::String, name::String,
                               pos::Int, separatorpos::Int, startpos::Int;
                               comp_keywords::Bool=false,
                               complete_modules_only::Bool=false,
                               shift::Bool=false)
    if comp_keywords
        complete_keyword!(suggestions, name)
        complete_keyval!(suggestions, name)
    end
    if separatorpos > 1 && (string[separatorpos] == '.' || string[separatorpos] == ':')
        s = string[1:prevind(string, separatorpos)]
        # First see if the whole string up to `pos` is a valid expression. If so, use it.
        prefix = Meta.parse(s, raise=false, depwarn=false)
        if isexpr(prefix, :incomplete)
            s = string[startpos:pos]
            # Heuristic to find the start of the expression. TODO: This would be better
            # done with a proper error-recovering parser.
            if 0 < startpos <= lastindex(string) && string[startpos] == '.'
                i = prevind(string, startpos)
                while 0 < i
                    c = string[i]
                    if c in (')', ']')
                        if c == ')'
                            c_start = '('
                            c_end = ')'
                        elseif c == ']'
                            c_start = '['
                            c_end = ']'
                        end
                        frange, end_of_identifier = find_start_brace(string[1:prevind(string, i)], c_start=c_start, c_end=c_end)
                        isempty(frange) && break # unbalanced parens
                        startpos = first(frange)
                        i = prevind(string, startpos)
                    elseif c in ('\'', '\"', '\`')
                        s = "$c$c"*string[startpos:pos]
                        break
                    else
                        break
                    end
                    s = string[startpos:pos]
                end
            end
            if something(findlast(in(non_identifier_chars), s), 0) < something(findlast(isequal('.'), s), 0)
                lookup_name, name = rsplit(s, ".", limit=2)
                name = String(name)
                prefix = Meta.parse(lookup_name, raise=false, depwarn=false)
            end
            isexpr(prefix, :incomplete) && (prefix = nothing)
        elseif isexpr(prefix, (:using, :import))
            arglast = prefix.args[end] # focus on completion to the last argument
            if isexpr(arglast, :.)
                # We come here for cases like:
                # - `string`: "using Mod1.Mod2.M"
                # - `ex`: :(using Mod1.Mod2)
                # - `name`: "M"
                # Now we transform `ex` to `:(Mod1.Mod2)` to allow `complete_symbol!` to
                # complete for inner modules whose name starts with `M`.
                # Note that `complete_modules_only=true` is set within `completions`
                prefix = nothing
                firstdot = true
                for arg = arglast.args
                    if arg === :.
                        # override `context_module` if multiple `.` accessors are used
                        if firstdot
                            firstdot = false
                        else
                            context_module = parentmodule(context_module)
                        end
                    elseif arg isa Symbol
                        if prefix === nothing
                            prefix = arg
                        else
                            prefix = Expr(:., prefix, QuoteNode(arg))
                        end
                    else # invalid expression
                        prefix = nothing
                        break
                    end
                end
            end
        elseif isexpr(prefix, :call) && length(prefix.args) > 1
            isinfix = s[end] != ')'
            # A complete call expression that does not finish with ')' is an infix call.
            if !isinfix
                # Handle infix call argument completion of the form bar + foo(qux).
                frange, end_of_identifier = find_start_brace(@view s[1:prevind(s, end)])
                if !isempty(frange) # if find_start_brace fails to find the brace just continue
                    isinfix = Meta.parse(@view(s[frange[1]:end]), raise=false, depwarn=false) == prefix.args[end]
                end
            end
            if isinfix
                prefix = prefix.args[end]
            end
        elseif isexpr(prefix, :macrocall) && length(prefix.args) > 1
            # allow symbol completions within potentially incomplete macrocalls
            if s[end] ≠ '`' && s[end] ≠ ')'
                prefix = prefix.args[end]
            end
        end
    else
        prefix = nothing
    end
    complete_symbol!(suggestions, prefix, name, context_module; complete_modules_only, shift)
    return suggestions
end

function completions(string::String, pos::Int, context_module::Module=Main, shift::Bool=true, hint::Bool=false)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = Base.incomplete_tag(Meta.parse(partial, raise=false, depwarn=false))

    if !hint # require a tab press for completion of these
        # ?(x, y)TAB lists methods you can call with these objects
        # ?(x, y TAB lists methods that take these objects as the first two arguments
        # MyModule.?(x, y)TAB restricts the search to names in MyModule
        rexm = match(r"(\w+\.|)\?\((.*)$", partial)
        if rexm !== nothing
            # Get the module scope
            if isempty(rexm.captures[1])
                callee_module = context_module
            else
                modname = Symbol(rexm.captures[1][1:end-1])
                if isdefined(context_module, modname)
                    callee_module = getfield(context_module, modname)
                    if !isa(callee_module, Module)
                        callee_module = context_module
                    end
                else
                    callee_module = context_module
                end
            end
            moreargs = !endswith(rexm.captures[2], ')')
            callstr = "_(" * rexm.captures[2]
            if moreargs
                callstr *= ')'
            end
            ex_org = Meta.parse(callstr, raise=false, depwarn=false)
            if isa(ex_org, Expr)
                return complete_any_methods(ex_org, callee_module::Module, context_module, moreargs, shift), (0:length(rexm.captures[1])+1) .+ rexm.offset, false
            end
        end
    end

    # if completing a key in a Dict
    identifier, partial_key, loc = dict_identifier_key(partial, inc_tag, context_module)
    if identifier !== nothing
        matches = find_dict_matches(identifier, partial_key)
        length(matches)==1 && (lastindex(string) <= pos || string[nextind(string,pos)] != ']') && (matches[1]*=']')
        length(matches)>0 && return Completion[DictCompletion(identifier, match) for match in sort!(matches)], loc::Int:pos, true
    end

    suggestions = Completion[]

    # Check if this is a var"" string macro that should be completed like
    # an identifier rather than a string.
    # TODO: It would be nice for the parser to give us more information here
    # so that we can lookup the macro by identity rather than pattern matching
    # its invocation.
    varrange = findprev("var\"", string, pos)

    expanded = nothing
    was_expanded = false

    if varrange !== nothing
        ok, ret = bslash_completions(string, pos)
        ok && return ret
        startpos = first(varrange) + 4
        separatorpos = something(findprev(isequal('.'), string, first(varrange)-1), 0)
        name = string[startpos:pos]
        complete_identifiers!(suggestions, context_module, string, name,
                              pos, separatorpos, startpos;
                              shift)
        return sort!(unique!(named_completion, suggestions), by=named_completion_completion), (separatorpos+1):pos, true
    elseif inc_tag === :cmd
        # TODO: should this call shell_completions instead of partially reimplementing it?
        let m = match(r"[\t\n\r\"`><=*?|]| (?!\\)", reverse(partial)) # fuzzy shell_parse in reverse
            startpos = nextind(partial, reverseind(partial, m.offset))
            r = startpos:pos
            scs::String = string[r]

            expanded = complete_expanduser(scs, r)
            was_expanded = expanded[3]
            if was_expanded
                scs = (only(expanded[1])::PathCompletion).path
                # If tab press, ispath and user expansion available, return it now
                # otherwise see if we can complete the path further before returning with expanded ~
                !hint && ispath(scs) && return expanded::Completions
            end

            path::String = replace(scs, r"(\\+)\g1(\\?)`" => "\1\2`") # fuzzy unescape_raw_string: match an even number of \ before ` and replace with half as many
            # This expansion with "\\ "=>' ' replacement and shell_escape=true
            # assumes the path isn't further quoted within the cmd backticks.
            path = replace(path, r"\\ " => " ", r"\$" => "\$") # fuzzy shell_parse (reversed by shell_escape_posixly)
            paths, dir, success = complete_path(path, shell_escape=true, raw_escape=true)

            if success && !isempty(dir)
                let dir = do_raw_escape(do_shell_escape(dir))
                    # if escaping of dir matches scs prefix, remove that from the completions
                    # otherwise make it the whole completion
                    if endswith(dir, "/") && startswith(scs, dir)
                        r = (startpos + sizeof(dir)):pos
                    elseif startswith(scs, dir * "/")
                        r = nextind(string, startpos + sizeof(dir)):pos
                    else
                        map!(paths, paths) do c::PathCompletion
                            p = dir * "/" * c.path
                            was_expanded && (p = contractuser(p))
                            return PathCompletion(p)
                        end
                    end
                end
            end
            if isempty(paths) && !hint && was_expanded
                # if not able to provide completions, not hinting, and ~ expansion was possible, return ~ expansion
                return expanded::Completions
            else
                return sort!(paths, by=p->p.path), r::UnitRange{Int}, success
            end
        end
    elseif inc_tag === :string
        # Find first non-escaped quote
        let m = match(r"\"(?!\\)", reverse(partial))
            startpos = nextind(partial, reverseind(partial, m.offset))
            r = startpos:pos
            scs::String = string[r]

            expanded = complete_expanduser(scs, r)
            was_expanded = expanded[3]
            if was_expanded
                scs = (only(expanded[1])::PathCompletion).path
                # If tab press, ispath and user expansion available, return it now
                # otherwise see if we can complete the path further before returning with expanded ~
                !hint && ispath(scs) && return expanded::Completions
            end

            path = try
                unescape_string(replace(scs, "\\\$"=>"\$"))
            catch ex
                ex isa ArgumentError || rethrow()
                nothing
            end
            if !isnothing(path)
                paths, dir, success = complete_path(path::String, string_escape=true)

                if length(paths) == 1
                    p = (paths[1]::PathCompletion).path
                    hint && was_expanded && (p = contractuser(p))
                    if close_path_completion(dir, p, path, pos)
                        paths[1] = PathCompletion(p * "\"")
                    end
                end

                if success && !isempty(dir)
                    let dir = do_string_escape(dir)
                        # if escaping of dir matches scs prefix, remove that from the completions
                        # otherwise make it the whole completion
                        if endswith(dir, "/") && startswith(scs, dir)
                            r = (startpos + sizeof(dir)):pos
                        elseif startswith(scs, dir * "/") && dir != dirname(homedir())
                            was_expanded && (dir = contractuser(dir))
                            r = nextind(string, startpos + sizeof(dir)):pos
                        else
                            map!(paths, paths) do c::PathCompletion
                                p = dir * "/" * c.path
                                hint && was_expanded && (p = contractuser(p))
                                return PathCompletion(p)
                            end
                        end
                    end
                end

                # Fallthrough allowed so that Latex symbols can be completed in strings
                if success
                    return sort!(paths, by=p->p.path), r::UnitRange{Int}, success
                elseif !hint && was_expanded
                    # if not able to provide completions, not hinting, and ~ expansion was possible, return ~ expansion
                    return expanded::Completions
                end
            end
        end
    end
    # if path has ~ and we didn't find any paths to complete just return the expanded path
    was_expanded && return expanded::Completions

    ok, ret = bslash_completions(string, pos)
    ok && return ret

    # Make sure that only bslash_completions is working on strings
    inc_tag === :string && return Completion[], 0:-1, false
    if inc_tag === :other
        frange, ex, wordrange, method_name_end = identify_possible_method_completion(partial, pos)
        if last(frange) != -1 && all(isspace, @view partial[wordrange]) # no last argument to complete
            if ex.head === :call
                return complete_methods(ex, context_module, shift), first(frange):method_name_end, false
            elseif is_broadcasting_expr(ex)
                return complete_methods(ex, context_module, shift), first(frange):(method_name_end - 1), false
            end
        end
    elseif inc_tag === :comment
        return Completion[], 0:-1, false
    end

    # Check whether we can complete a keyword argument in a function call
    kwarg_completion, wordrange = complete_keyword_argument(partial, pos, context_module; shift)
    isempty(wordrange) || return kwarg_completion, wordrange, !isempty(kwarg_completion)

    startpos = nextind(string, something(findprev(in(non_identifier_chars), string, pos), 0))
    # strip preceding ! operator
    if (m = match(r"\G\!+", partial, startpos)) isa RegexMatch
        startpos += length(m.match)
    end

    separatorpos = something(findprev(isequal('.'), string, pos), 0)
    namepos = max(startpos, separatorpos+1)
    name = string[namepos:pos]
    import_mode = get_import_mode(string)
    if import_mode === :using_module || import_mode === :import_module
        # Given input lines like `using Foo|`, `import Foo, Bar|` and `using Foo.Bar, Baz, |`:
        # Let's look only for packages and modules we can reach from here

        # If there's no dot, we're in toplevel, so we should
        # also search for packages
        s = string[startpos:pos]
        if separatorpos <= startpos
            for dir in Base.load_path()
                if basename(dir) in Base.project_names && isfile(dir)
                    complete_loading_candidates!(suggestions, s, dir)
                end
                isdir(dir) || continue
                for entry in _readdirx(dir)
                    pname = entry.name
                    if pname[1] != '.' && pname != "METADATA" &&
                        pname != "REQUIRE" && startswith(pname, s)
                        # Valid file paths are
                        #   <Mod>.jl
                        #   <Mod>/src/<Mod>.jl
                        #   <Mod>.jl/src/<Mod>.jl
                        if isfile(entry)
                            endswith(pname, ".jl") && push!(suggestions,
                                                            PackageCompletion(pname[1:prevind(pname, end-2)]))
                        else
                            mod_name = if endswith(pname, ".jl")
                                pname[1:prevind(pname, end-2)]
                            else
                                pname
                            end
                            if isfile(joinpath(entry, "src",
                                               "$mod_name.jl"))
                                push!(suggestions, PackageCompletion(mod_name))
                            end
                        end
                    end
                end
            end
        end
        comp_keywords = false
        complete_modules_only = import_mode === :using_module # allow completion for `import Mod.name` (where `name` is not a module)
    elseif import_mode === :using_name || import_mode === :import_name
        # `using Foo: |` and `import Foo: bar, baz|`
        separatorpos = findprev(isequal(':'), string, pos)::Int
        comp_keywords = false
        complete_modules_only = false
    else
        comp_keywords = !isempty(name) && startpos > separatorpos
        complete_modules_only = false
    end

    complete_identifiers!(suggestions, context_module, string, name,
                          pos, separatorpos, startpos;
                          comp_keywords, complete_modules_only, shift)
    return sort!(unique!(named_completion, suggestions), by=named_completion_completion), namepos:pos, true
end

function shell_completions(string, pos, hint::Bool=false)
    # First parse everything up to the current position
    scs = string[1:pos]
    args, last_arg_start = try
        Base.shell_parse(scs, true)::Tuple{Expr,Int}
    catch ex
        ex isa ArgumentError || ex isa ErrorException || rethrow()
        return Completion[], 0:-1, false
    end
    ex = args.args[end]::Expr
    # Now look at the last thing we parsed
    isempty(ex.args) && return Completion[], 0:-1, false
    lastarg = ex.args[end]
    # As Base.shell_parse throws away trailing spaces (unless they are escaped),
    # we need to special case here.
    # If the last char was a space, but shell_parse ignored it search on "".
    if isexpr(lastarg, :incomplete) || isexpr(lastarg, :error)
        partial = string[last_arg_start:pos]
        ret, range = completions(partial, lastindex(partial), Main, true, hint)
        range = range .+ (last_arg_start - 1)
        return ret, range, true
    elseif endswith(scs, ' ') && !endswith(scs, "\\ ")
        r = pos+1:pos
        paths, dir, success = complete_path("", use_envpath=false, shell_escape=true)
        return paths, r, success
    elseif all(@nospecialize(arg) -> arg isa AbstractString, ex.args)
        # Join these and treat this as a path
        path::String = join(ex.args)
        r = last_arg_start:pos

        # Also try looking into the env path if the user wants to complete the first argument
        use_envpath = length(args.args) < 2

        expanded = complete_expanduser(path, r)
        was_expanded = expanded[3]
        if was_expanded
            path = (only(expanded[1])::PathCompletion).path
            # If tab press, ispath and user expansion available, return it now
            # otherwise see if we can complete the path further before returning with expanded ~
            !hint && ispath(path) && return expanded::Completions
        end

        paths, dir, success = complete_path(path, use_envpath=use_envpath, shell_escape=true, contract_user=was_expanded)

        if success && !isempty(dir)
            let dir = do_shell_escape(dir)
                # if escaping of dir matches scs prefix, remove that from the completions
                # otherwise make it the whole completion
                partial = string[last_arg_start:pos]
                if endswith(dir, "/") && startswith(partial, dir)
                    r = (last_arg_start + sizeof(dir)):pos
                elseif startswith(partial, dir * "/")
                    r = nextind(string, last_arg_start + sizeof(dir)):pos
                else
                    map!(paths, paths) do c::PathCompletion
                        return PathCompletion(dir * "/" * c.path)
                    end
                end
            end
        end
        # if ~ was expanded earlier and the incomplete string isn't a path
        # return the path with contracted user to match what the hint shows. Otherwise expand ~
        # i.e. require two tab presses to expand user
        if was_expanded && !ispath(path)
            map!(paths, paths) do c::PathCompletion
                PathCompletion(contractuser(c.path))
            end
        end
        return paths, r, success
    end
    return Completion[], 0:-1, false
end

function __init__()
    COMPLETION_WORLD[] = Base.get_world_counter()
    return nothing
end

end # module
