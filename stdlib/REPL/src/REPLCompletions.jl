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
using Base.JuliaSyntax: @K_str, @KSet_str, parseall, byte_range, children, is_prefix_call, is_trivia, kind

using ..REPL.LineEdit: NamedCompletion
using ..REPL.SyntaxUtil: CursorNode, find_parent, seek_pos, char_range, char_last, children_nt, find_delim

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
    complete_internal_only = isempty(name)
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

function do_cmd_escape(s)
    return Base.escape_raw_string(Base.shell_escape_posixly(s), '`')
end
function do_shell_escape(s)
    return Base.shell_escape_posixly(s)
end
function do_string_escape(s)
    return escape_string(s, ('\"','$'))
end
function do_string_unescape(s)
    s = replace(s, "\\\$"=>"\$")
    try
        unescape_string(s)
    catch e
        e isa ArgumentError || rethrow()
        s # it is unlikely, but if it isn't a valid string, maybe it was a valid path, and just needs escape_string called?
    end
end

function joinpath_withsep(dir, path; dirsep)
    dir == "" && return path
    dir[end] == dirsep ? dir * path : dir * dirsep * path
end

const PATH_cache_lock = Base.ReentrantLock()
const PATH_cache = Set{String}()
PATH_cache_task::Union{Task,Nothing} = nothing
PATH_cache_condition::Union{Threads.Condition, Nothing} = nothing # used for sync in tests
next_cache_update::Float64 = 0.0
function maybe_spawn_cache_PATH()
    global PATH_cache_task, PATH_cache_condition, next_cache_update
    @lock PATH_cache_lock begin
        # Extract to local variables to enable flow-sensitive type inference for these global variables
        PATH_cache_task_local = PATH_cache_task
        PATH_cache_task_local isa Task && !istaskdone(PATH_cache_task_local) && return
        time() < next_cache_update && return
        PATH_cache_task = PATH_cache_task_local = Threads.@spawn begin
            try
                REPLCompletions.cache_PATH()
            finally
                @lock PATH_cache_lock begin
                    next_cache_update = time() + 10 # earliest next update can run is 10s after
                    PATH_cache_task = nothing # release memory when done
                    PATH_cache_condition_local = PATH_cache_condition
                    PATH_cache_condition_local !== nothing && notify(PATH_cache_condition_local)
                end
            end
        end
        Base.errormonitor(PATH_cache_task_local)
    end
end

# caches all reachable files in PATH dirs
function cache_PATH()
    path = get(ENV, "PATH", nothing)
    path isa String || return

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
    end

    @debug "caching PATH files took $t seconds" length(pathdirs) length(PATH_cache)
    return PATH_cache
end

function complete_path(path::AbstractString;
                       use_envpath=false,
                       shell_escape=false,
                       cmd_escape=false,
                       string_escape=false,
                       contract_user=false,
                       dirsep=Sys.iswindows() ? '\\' : '/')
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
            push!(matches, is_dir ? joinpath_withsep(entry.name, ""; dirsep) : entry.name)
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
    matches = ((cmd_escape ? do_cmd_escape(s) : s) for s in matches)
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
    paths, dir, success = complete_path(path; use_envpath, shell_escape, string_escape, dirsep='/')

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

function is_call_stack_uncached(sv::CC.InferenceState)
    CC.is_cached(sv) && return false
    parent = CC.frame_parent(sv)
    parent === nothing && return true
    return is_call_stack_uncached(parent::CC.InferenceState)
end

# aggressive global binding resolution within `repl_frame`
function CC.abstract_eval_globalref(interp::REPLInterpreter, g::GlobalRef, bailed::Bool,
                                    sv::CC.InferenceState)
    # Ignore saw_latestworld
    if (interp.limit_aggressive_inference ? is_repl_frame(sv) : is_call_stack_uncached(sv))
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

# aggressive concrete evaluation for `:inconsistent` frames within `repl_frame`
function CC.concrete_eval_eligible(interp::REPLInterpreter, @nospecialize(f),
                                   result::CC.MethodCallResult, arginfo::CC.ArgInfo,
                                   sv::CC.InferenceState)
    if (interp.limit_aggressive_inference ? is_repl_frame(sv) : is_call_stack_uncached(sv))
        neweffects = CC.Effects(result.effects; consistent=CC.ALWAYS_TRUE)
        result = CC.MethodCallResult(result.rt, result.exct, neweffects, result.edge,
                                     result.edgecycle, result.edgelimited, result.call_result)
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

# Perform some post-hoc mutation on lowered code, as expected by some abstract interpretation
# routines, especially for `:foreigncall` and `:cglobal`.
function resolve_toplevel_symbols!(src::Core.CodeInfo, mod::Module)
    @ccall jl_resolve_definition_effects_in_ir(
        #=jl_array_t *stmts=# src.code::Any,
        #=jl_module_t *m=# mod::Any,
        #=jl_svec_t *sparam_vals=# Core.svec()::Any,
        #=jl_value_t *binding_edge=# C_NULL::Ptr{Cvoid},
        #=int binding_effects=# 0::Int)::Cvoid
    return src
end

function construct_toplevel_mi(src::Core.CodeInfo, context_module::Module)
    resolve_toplevel_symbols!(src, context_module)
    return @ccall jl_method_instance_for_thunk(src::Any, context_module::Any)::Ref{Core.MethodInstance}
end

# lower `ex` and run type inference on the resulting top-level expression
function repl_eval_ex(@nospecialize(ex), context_module::Module; limit_aggressive_inference::Bool=false)
    expr_has_error(ex) && return nothing
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

    mi = construct_toplevel_mi(src, context_module)
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
    isempty(ex_org.args) && return 2, nothing, [], Set{Symbol}()
    # Desugar do block call into call with lambda
    if ex_org.head === :do && length(ex_org.args) >= 2
        ex_call = ex_org.args[1]
        ex_args = [x for x in ex_call.args if !(x isa Expr && x.head === :parameters)]
        ex_params = findfirst(x -> x isa Expr && x.head === :parameters, ex_call.args)
        new_args = [ex_args[1], ex_org.args[end], ex_args[2:end]...]
        ex_params !== nothing && push!(new_args, ex_call.args[ex_params])
        ex_org = Expr(:call, new_args...)
    end
    funct = repl_eval_ex(ex_org.args[1], context_module)
    funct === nothing && return 2, nothing, [], Set{Symbol}()
    funct = CC.widenconst(funct)
    args_ex, kwargs_ex, kwargs_flag = complete_methods_args(ex_org, context_module, true, true)
    return kwargs_flag, funct, args_ex, kwargs_ex
end

# cursor_pos: either :positional (complete either kwargs or positional) or :kwargs (beyond semicolon)
function complete_methods(ex_org::Expr, context_module::Module=Main, shift::Bool=false, cursor_pos::Symbol=:positional)
    kwargs_flag, funct, args_ex, kwargs_ex = _complete_methods(ex_org, context_module, shift)::Tuple{Int, Any, Vector{Any}, Set{Symbol}}
    out = Completion[]
    # Allow more arguments when cursor before semicolon, even if kwargs are present
    cursor_pos == :positional && kwargs_flag == 1 && (kwargs_flag = 0)
    kwargs_flag == 2 && return out # one of the kwargs is invalid
    kwargs_flag == 0 && push!(args_ex, Vararg{Any}) # allow more arguments if there is no semicolon
    complete_methods!(out, funct, args_ex, kwargs_ex, shift ? -2 : MAX_METHOD_COMPLETIONS, kwargs_flag == 1)
    return out
end

MAX_ANY_METHOD_COMPLETIONS::Int = 10

function accessible(mod::Module, private::Bool)
    bindings = IdSet{Any}(Core.Typeof(getglobal(mod, s)) for s in names(mod; all=private, imported=private, usings=private)
                   if !Base.isdeprecated(mod, s) && !startswith(string(s), '#') && !startswith(string(s), '@') && isdefined(mod, s))
    delete!(bindings, Module)
    return collect(bindings)
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

    for seen_name in accessible(callee_module, callee_module === context_module)
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
    return (false, (Completion[], 1:0, false))
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

# Provide completion for keyword arguments in function calls
# Returns true if the current argument must be a keyword because the cursor is beyond the semicolon
function complete_keyword_argument!(suggestions::Vector{Completion},
                                    ex::Expr, last_word::String,
                                    context_module::Module,
                                    arg_pos::Symbol; shift::Bool=false)
    kwargs_flag, funct, args_ex, kwargs_ex = _complete_methods(ex, context_module, true)::Tuple{Int, Any, Vector{Any}, Set{Symbol}}
    kwargs_flag == 2 && return false # one of the previous kwargs is invalid

    methods = Completion[]
    # Limit kwarg completions to cases when function is concretely known; looking up
    # matching methods for abstract functions — particularly `Any` or `Function` — can
    # take many seconds to run over the thousands of possible methods. Note that
    # isabstracttype would return naively return true for common constructor calls
    # like Array, but the REPL's introspection here may know their Type{T}.
    isconcretetype(funct) || return false
    complete_methods!(methods, funct, Any[Vararg{Any}], kwargs_ex, -1, arg_pos == :kwargs)
    # TODO: use args_ex instead of Any[Vararg{Any}] and only provide kwarg completion for
    # method calls compatible with the current arguments.

    # For each method corresponding to the function call, provide completion suggestions
    # for each keyword that starts like the last word and that is not already used
    # previously in the expression. The corresponding suggestion is "kwname=".
    # If the keyword corresponds to an existing name, also include "kwname" as a suggestion
    # since the syntax "foo(; kwname)" is equivalent to "foo(; kwname=kwname)".
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
            # HACK: Should consider removing current arg from AST.
            if !endswith(kw, "...") && startswith(kw, last_word) && (_kw ∉ kwargs_ex || kw == last_word)
                push!(current_kwarg_candidates, kw)
            end
        end
        union!(kwargs, current_kwarg_candidates)
    end

    for kwarg in kwargs
        push!(suggestions, KeywordArgumentCompletion(kwarg))
    end
    return kwargs_flag != 0 && arg_pos == :kwargs
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

function complete_loading_candidates!(suggestions::Vector{Completion}, s::String)
    for name in ("Core", "Base")
        startswith(name, s) && push!(suggestions, PackageCompletion(name))
    end

    # If there's no dot, we're in toplevel, so we should
    # also search for packages
    for dir in Base.load_path()
        if basename(dir) in Base.project_names && isfile(dir)
            for name in get_loading_candidates(s, dir)
                push!(suggestions, PackageCompletion(name))
            end
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

function completions(string::String, pos::Int, context_module::Module=Main, shift::Bool=true, hint::Bool=false)
    # filename needs to be string so macro can be evaluated
    node = parseall(CursorNode, string, ignore_errors=true, keep_parens=true, filename="none")
    cur = @something seek_pos(node, pos) node

    # Back up before whitespace to get a more useful AST node.
    pos_not_ws = findprev(!isspace, string, pos)
    cur_not_ws = something(seek_pos(node, pos_not_ws), node)

    suggestions = Completion[]
    sort_suggestions() = sort!(unique!(named_completion, suggestions), by=named_completion_completion)

    # Search for methods (requires tab press):
    #   ?(x, y)TAB           lists methods you can call with these objects
    #   ?(x, y TAB           lists methods that take these objects as the first two arguments
    #   MyModule.?(x, y)TAB  restricts the search to names in MyModule
    if !hint
        cs = method_search(view(string, 1:pos), context_module, shift)
        cs !== nothing && return cs
    end

    # Complete keys in a Dict:
    #   my_dict[ TAB
    n, key, closed = find_ref_key(cur_not_ws, pos)
    if n !== nothing
        key::UnitRange{Int}
        obj = dict_eval(Expr(n), context_module)
        if obj !== nothing
            # Skip leading whitespace inside brackets.
            i = @something findnext(!isspace, string, first(key)) nextind(string, last(key))
            key = i:last(key)
            s = string[intersect(key, 1:pos)]
            matches = find_dict_matches(obj, s)
            length(matches) == 1 && !closed && (matches[1] *= ']')
            if length(matches) > 0
                ret = Completion[DictCompletion(obj, match) for match in sort!(matches)]
                return ret, key, true
            end
        end
    end

    # Complete Cmd strings:
    #   `fil TAB                 => `file
    #   `file ~/exa TAB          => `file ~/example.txt
    #   `file ~/example.txt TAB  => `file /home/user/example.txt
    if (n = find_parent(cur, K"CmdString")) !== nothing
        off = n.position - 1
        ret, r, success = shell_completions(string[char_range(n)], pos - off, hint, cmd_escape=true)
        success && return ret, r .+ off, success
    end

    # Complete ordinary strings:
    #  "~/exa TAB         => "~/example.txt"
    #  "~/example.txt TAB => "/home/user/example.txt"
    r, closed = find_str(cur)
    if r !== nothing
        s = do_string_unescape(string[r])
        ret, success = complete_path_string(s, hint; string_escape=true,
                                            dirsep=Sys.iswindows() ? '\\' : '/')
        if length(ret) == 1 && !closed && close_path_completion(ret[1].path)
            ret[1] = PathCompletion(ret[1].path * '"')
        end
        success && return ret, r, success
    end

    # Backlash symbols:
    #   \pi => π
    # Comes after string completion so backslash escapes are not misinterpreted.
    ok, ret = bslash_completions(string, pos)
    ok && return ret

    # Don't fall back to symbol completion inside strings or comments.
    inside_cmdstr = find_parent(cur, K"cmdstring") !== nothing
    (kind(cur) in KSet"String Comment ErrorEofMultiComment" || inside_cmdstr) &&
         return Completion[], 1:0, false

    n, arg_pos = find_prefix_call(cur_not_ws)
    if n !== nothing
        func = first(children_nt(n))
        e = Expr(n)
        # Remove arguments past the first parse error (allows unclosed parens)
        if is_broadcasting_expr(e)
            i = findfirst(x -> x isa Expr && x.head == :error, e.args[2].args)
            i !== nothing && deleteat!(e.args[2].args, i:lastindex(e.args[2].args))
        else
            i = findfirst(x -> x isa Expr && x.head == :error, e.args)
            i !== nothing && deleteat!(e.args, i:lastindex(e.args))
        end

        # Method completion:
        #   foo( TAB     => list of method signatures for foo
        #   foo(x, TAB   => list of methods signatures for foo with x as first argument
        if kind(cur_not_ws) in KSet"( , ;"
            # Don't provide method completions unless the cursor is after: '(' ',' ';'
            return complete_methods(e, context_module, shift, arg_pos), char_range(func), false

        # Keyword argument completion:
        #   foo(ar TAB   => keyword arguments like `arg1=`
        elseif kind(cur) == K"Identifier"
            r = char_range(cur)
            s = string[intersect(r, 1:pos)]
            # Return without adding more suggestions if kwargs only
            complete_keyword_argument!(suggestions, e, s, context_module, arg_pos; shift) &&
                return sort_suggestions(), r, true
        end
    end

    # Symbol completion
    # TODO: Should completions replace the identifier at the cursor?
    looks_like_ident = Base.isidentifier(@view string[intersect(char_range(cur), 1:pos)])
    if cur.parent !== nothing && kind(cur.parent) == K"var"
        # Replace the entire var"foo", but search using only "foo".
        r = intersect(char_range(cur.parent), 1:pos)
        r2 = char_range(children_nt(cur.parent)[1])
        s = string[intersect(r2, 1:pos)]
    elseif kind(cur) == K"MacroName"
        # Include the `@`
        r = intersect(prevind(string, cur.position):char_last(cur), 1:pos)
        s = string[r]
    elseif looks_like_ident || kind(cur) in KSet"Bool Identifier @"
        r = intersect(char_range(cur), 1:pos)
        s = string[r]
    else
        r = nextind(string, pos):pos
        s = ""
    end

    complete_modules_only = false
    prefix = node_prefix(cur, context_module)
    comp_keywords = prefix === nothing && !isempty(s)

    # Complete loadable module names:
    #   import Mod TAB
    #   import Mod1, Mod2 TAB
    #   using Mod TAB
    if (n = find_parent(cur, K"importpath")) !== nothing
        # Given input lines like `using Foo|`, `import Foo, Bar|` and `using Foo.Bar, Baz, |`:
        # Let's look only for packages and modules we can reach from here
        if prefix == nothing
            complete_loading_candidates!(suggestions, s)
            return sort_suggestions(), r, true
        end

        # Allow completion for `import Mod.name` (where `name` is not a module)
        complete_modules_only = prefix == nothing || kind(n.parent) == K"using"
        comp_keywords = false
    end

    if comp_keywords
        complete_keyword!(suggestions, s)
        complete_keyval!(suggestions, s)
    end

    complete_symbol!(suggestions, prefix, s, context_module; complete_modules_only, shift)
    return sort_suggestions(), r, true
end

function close_path_completion(path)
    path = expanduser(path)
    path = do_string_unescape(path)
    !Base.isaccessibledir(path)
end

# Lowering can misbehave with nested error expressions.
function expr_has_error(@nospecialize(e))
    e isa Expr || return false
    e.head === :error &&  return true
    any(expr_has_error, e.args)
end

# Is the cursor inside the square brackets of a ref expression?  If so, returns:
# - The ref node
# - The range of characters for the brackets
# - A flag indicating if the closing bracket is present
function find_ref_key(cur::CursorNode, pos::Int)
    n = find_parent(cur, K"ref")
    n !== nothing || return nothing, nothing, nothing
    key, closed = find_delim(n, K"[", K"]")
    first(key) - 1 <= pos <= last(key) || return nothing, nothing, nothing
    n, key, closed
end

# If the cursor is in a literal string, return the contents and char range
# inside the quotes.  Ignores triple strings.
function find_str(cur::CursorNode)
    n = find_parent(cur, K"string")
    n !== nothing || return nothing, nothing
    find_delim(n, K"\"", K"\"")
end

# Is the cursor directly inside of the arguments of a prefix call (no nested
# expressions)?  If so, return:
#   - The call node
#   - Either :positional or :kwargs, if the cursor is before or after the `;`
function find_prefix_call(cur::CursorNode)
    n = cur.parent
    n !== nothing || return nothing, nothing
    is_call(n) = kind(n) in KSet"call dotcall" && is_prefix_call(n)
    if kind(n) == K"parameters"
        is_call(n.parent) || return nothing, nothing
        n.parent, :kwargs
    else
        # Check that we are beyond the function name.
        is_call(n) && cur.index > children_nt(n)[1].index || return nothing, nothing
        n, :positional
    end
end

# If node is the field in a getfield-like expression, return the value
# complete_symbol! should use as the prefix.
function node_prefix(node::CursorNode, context_module::Module)
    node.parent !== nothing || return nothing
    p = node.parent
    # In x.var"y", the parent is the "var" when the cursor is on "y".
    kind(p) == K"var" && (p = p.parent)

    # expr.node => expr
    if kind(p) == K"."
        n = children_nt(p)[1]
        # Don't use prefix if we are the value
        n !== node || return nothing
        return Expr(n)
    end

    if kind(p) == K"importpath"
        if p.parent !== nothing && kind(p.parent) == K":" && p.index_nt > 1
            # import A.B: C.node
            chain = children_nt(children_nt(p.parent)[1])
            append!(chain, children_nt(p)[1:end-1])
        else
            # import A.node
            # import A.node: ...
            chain = children_nt(p)[1:node.index_nt]
            # Don't include the node under cursor in prefix unless it is `.`
            kind(chain[end]) != K"." && deleteat!(chain, lastindex(chain))
        end
        length(chain) > 0 || return nothing

        # (:importpath :x :y :z) => (:. (:. :x :y) :z)
        # (:importpath :. :. :z) => (:. (parentmodule context_module) :z)
        if (i = findlast(x -> kind(x) == K".", chain)) !== nothing
            init = context_module
            for j in 2:i
                init = parentmodule(init)
            end
            deleteat!(chain, 1:i)
        else
            # No leading `.`, init is the first element of the path
            init = chain[1].val
            deleteat!(chain, 1)
        end

        # Convert the "chain" into nested (. a b) expressions.
        all(x -> kind(x) == K"Identifier", chain) || return nothing
        return foldl((x, y) -> Expr(:., x, Expr(:quote, y.val)), chain; init)
    end

    nothing
end

function dict_eval(@nospecialize(e), context_module::Module=Main)
    objt = repl_eval_ex(e.args[1], context_module)
    isa(objt, Core.Const) || return nothing
    obj = objt.val
    isa(obj, AbstractDict) || return nothing
    (Base.haslength(obj) && length(obj)::Int < 1_000_000) || return nothing
    return obj
end

function method_search(partial::AbstractString, context_module::Module, shift::Bool)
    rexm = match(r"([\w.]+.)?\?\((.*)$", partial)
    if rexm !== nothing
        # Get the module scope
        callee_module = context_module
        if !isnothing(rexm.captures[1])
            modnames = map(Symbol, split(something(rexm.captures[1]), '.'))
            for m in modnames
                if isdefined(callee_module, m)
                    callee_module = getfield(callee_module, m)
                    if !isa(callee_module, Module)
                        callee_module = context_module
                        break
                    end
                end
            end
        end
        moreargs = !endswith(rexm.captures[2], ')')
        callstr = "_(" * rexm.captures[2]
        if moreargs
            callstr *= ')'
        end
        ex_org = Meta.parse(callstr, raise=false, depwarn=false)
        if isa(ex_org, Expr)
            pos_q = isnothing(rexm.captures[1]) ? 1 : sizeof(something(rexm.captures[1]))+1 # position after ?
            return complete_any_methods(ex_org, callee_module::Module, context_module, moreargs, shift), (0:pos_q) .+ rexm.offset, false
        end
    end
end

function shell_completions(str, pos, hint::Bool=false; cmd_escape::Bool=false)
    # First parse everything up to the current position
    scs = str[1:pos]
    args, last_arg_start = try
        Base.shell_parse(scs, true)::Tuple{Expr,Int}
    catch ex
        ex isa ArgumentError || ex isa ErrorException || rethrow()
        return Completion[], 1:0, false
    end
    ex = args.args[end]::Expr
    # Now look at the last thing we parsed
    isempty(ex.args) && return Completion[], 1:0, false
    # Concatenate every string fragment so dir\file completes correctly.
    lastarg = all(x -> x isa String, ex.args) ? string(ex.args...) : ex.args[end]

    # As Base.shell_parse throws away trailing spaces (unless they are escaped),
    # we need to special case here.
    # If the last char was a space, but shell_parse ignored it search on "".
    if isexpr(lastarg, :incomplete) || isexpr(lastarg, :error)
        partial = str[last_arg_start:pos]
        ret, range = completions(partial, lastindex(partial), Main, true, hint)
        range = range .+ (last_arg_start - 1)
        return ret, range, true
    elseif endswith(scs, ' ') && !endswith(scs, "\\ ")
        r = pos+1:pos
        paths, dir, success = complete_path(""; use_envpath=false, shell_escape=!cmd_escape, cmd_escape, dirsep='/')
        return paths, r, success
    elseif all(@nospecialize(arg) -> arg isa AbstractString, ex.args)
        # Join these and treat this as a path
        path::String = join(ex.args)
        r = last_arg_start:pos

        # Also try looking into the env path if the user wants to complete the first argument
        use_envpath = length(args.args) < 2

        paths, success = complete_path_string(path, hint; use_envpath, shell_escape=!cmd_escape, cmd_escape, dirsep='/')
        return paths, r, success
    end
    return Completion[], 1:0, false
end

function complete_path_string(path, hint::Bool=false;
                              shell_escape::Bool=false,
                              cmd_escape::Bool=false,
                              string_escape::Bool=false,
                              dirsep='/',
                              kws...)
    # Expand "~" and remember if we expanded it.
    local expanded
    try
        let p = expanduser(path)
            expanded = path != p
            path = p
        end
    catch e
        e isa ArgumentError || rethrow()
        expanded = false
    end

    function escape(p)
        shell_escape && (p = do_shell_escape(p))
        string_escape && (p = do_string_escape(p))
        cmd_escape && (p = do_cmd_escape(p))
        p
    end

    paths, dir, success = complete_path(path; dirsep, kws...)

    # Expand '~' if the user hits TAB after exhausting completions (either
    # because we have found an existing file, or there is no such file).
    full_path = try
        ispath(path) || isempty(paths)
    catch err
        # access(2) errors unhandled by ispath: EACCES, EIO, ELOOP, ENAMETOOLONG
        if err isa Base.IOError
            false
        elseif err isa Base.ArgumentError && occursin("embedded NULs", err.msg)
            false
        else
            rethrow()
        end
    end
    expanded && !hint && full_path && return Completion[PathCompletion(escape(path))], true

    # Expand '~' if the user hits TAB on a path ending in '/'.
    expanded && (hint || path != dir * "/") && (dir = contractuser(dir))

    map!(paths) do c::PathCompletion
        p = joinpath_withsep(dir, c.path; dirsep)
        PathCompletion(escape(p))
    end
    return sort!(paths, by=p->p.path), success
end

function __init__()
    COMPLETION_WORLD[] = Base.get_world_counter()
    return nothing
end

end # module
