# This file is a part of Julia. License is MIT: https://julialang.org/license

module REPLCompletions

export completions, shell_completions, bslash_completions, completion_text

using Core: CodeInfo, MethodInstance, CodeInstance, Const
const CC = Core.Compiler
using Base.Meta
using Base: propertynames, something

abstract type Completion end

struct TextCompletion <: Completion
    text::String
end

struct KeywordCompletion <: Completion
    keyword::String
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
    bslash::String
end

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
_completion_text(c::PathCompletion) = c.path
_completion_text(c::ModuleCompletion) = c.mod
_completion_text(c::PackageCompletion) = c.package
_completion_text(c::PropertyCompletion) = sprint(Base.show_sym, c.property)
_completion_text(c::FieldCompletion) = sprint(Base.show_sym, c.field)
_completion_text(c::MethodCompletion) = repr(c.method)
_completion_text(c::BslashCompletion) = c.bslash
_completion_text(c::ShellCompletion) = c.text
_completion_text(c::DictCompletion) = c.key
_completion_text(c::KeywordArgumentCompletion) = c.kwarg*'='

completion_text(c) = _completion_text(c)::String

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

function filtered_mod_names(ffunc::Function, mod::Module, name::AbstractString, all::Bool = false, imported::Bool = false)
    ssyms = names(mod, all = all, imported = imported)
    filter!(ffunc, ssyms)
    macros = filter(x -> startswith(String(x), "@" * name), ssyms)
    syms = String[sprint((io,s)->Base.show_sym(io, s; allow_macroname=true), s) for s in ssyms if completes_global(String(s), name)]
    appendmacro!(syms, macros, "_str", "\"")
    appendmacro!(syms, macros, "_cmd", "`")
    return [ModuleCompletion(mod, sym) for sym in syms]
end

# REPL Symbol Completions
function complete_symbol(@nospecialize(ex), name::String, @nospecialize(ffunc), context_module::Module=Main)
    mod = context_module

    lookup_module = true
    t = Union{}
    val = nothing
    if ex !== nothing
        res = repl_eval_ex(ex, context_module)
        res === nothing && return Completion[]
        if res isa Const
            val = res.val
            if isa(val, Module)
                mod = val
                lookup_module = true
            else
                lookup_module = false
                t = typeof(val)
            end
        else
            lookup_module = false
            t = CC.widenconst(res)
        end
    end

    suggestions = Completion[]
    if lookup_module
        # We will exclude the results that the user does not want, as well
        # as excluding Main.Main.Main, etc., because that's most likely not what
        # the user wants
        p = let mod=mod, modname=nameof(mod)
            s->(!Base.isdeprecated(mod, s) && s != modname && ffunc(mod, s)::Bool)
        end
        # Looking for a binding in a module
        if mod == context_module
            # Also look in modules we got through `using`
            mods = ccall(:jl_module_usings, Any, (Any,), context_module)::Vector
            for m in mods
                append!(suggestions, filtered_mod_names(p, m::Module, name))
            end
            append!(suggestions, filtered_mod_names(p, mod, name, true, true))
        else
            append!(suggestions, filtered_mod_names(p, mod, name, true, false))
        end
    elseif val !== nothing # looking for a property of an instance
        for property in propertynames(val, false)
            # TODO: support integer arguments (#36872)
            if property isa Symbol && startswith(string(property), name)
                push!(suggestions, PropertyCompletion(val, property))
            end
        end
    else
        # Looking for a member of a type
        if t isa DataType && t != Any
            # Check for cases like Type{typeof(+)}
            if Base.isType(t)
                t = typeof(t.parameters[1])
            end
            # Only look for fields if this is a concrete type
            if isconcretetype(t)
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
    end
    suggestions
end

const sorted_keywords = [
    "abstract type", "baremodule", "begin", "break", "catch", "ccall",
    "const", "continue", "do", "else", "elseif", "end", "export", "false",
    "finally", "for", "function", "global", "if", "import",
    "let", "local", "macro", "module", "mutable struct",
    "primitive type", "quote", "return", "struct",
    "true", "try", "using", "while"]

function complete_keyword(s::Union{String,SubString{String}})
    r = searchsorted(sorted_keywords, s)
    i = first(r)
    n = length(sorted_keywords)
    while i <= n && startswith(sorted_keywords[i],s)
        r = first(r):i
        i += 1
    end
    Completion[KeywordCompletion(kw) for kw in sorted_keywords[r]]
end

function complete_path(path::AbstractString, pos::Int; use_envpath=false, shell_escape=false)
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
    local files
    try
        if isempty(dir)
            files = readdir()
        elseif isdir(dir)
            files = readdir(dir)
        else
            return Completion[], 0:-1, false
        end
    catch
        return Completion[], 0:-1, false
    end

    matches = Set{String}()
    for file in files
        if startswith(file, prefix)
            id = try isdir(joinpath(dir, file)) catch; false end
            # joinpath is not used because windows needs to complete with double-backslash
            push!(matches, id ? file * (@static Sys.iswindows() ? "\\\\" : "/") : file)
        end
    end

    if use_envpath && length(dir) == 0
        # Look for files in PATH as well
        local pathdirs = split(ENV["PATH"], @static Sys.iswindows() ? ";" : ":")

        for pathdir in pathdirs
            local actualpath
            try
                actualpath = realpath(pathdir)
            catch
                # Bash doesn't expect every folder in PATH to exist, so neither shall we
                continue
            end

            if actualpath != pathdir && in(actualpath,pathdirs)
                # Remove paths which (after resolving links) are in the env path twice.
                # Many distros eg. point /bin to /usr/bin but have both in the env path.
                continue
            end

            local filesinpath
            try
                filesinpath = readdir(pathdir)
            catch e
                # Bash allows dirs in PATH that can't be read, so we should as well.
                if isa(e, Base.IOError) || isa(e, Base.ArgumentError)
                    continue
                else
                    # We only handle IOError and ArgumentError here
                    rethrow()
                end
            end

            for file in filesinpath
                # In a perfect world, we would filter on whether the file is executable
                # here, or even on whether the current user can execute the file in question.
                if startswith(file, prefix) && isfile(joinpath(pathdir, file))
                    push!(matches, file)
                end
            end
        end
    end

    matchList = Completion[PathCompletion(shell_escape ? replace(s, r"\s" => s"\\\0") : s) for s in matches]
    startpos = pos - lastindex(prefix) + 1 - count(isequal(' '), prefix)
    # The pos - lastindex(prefix) + 1 is correct due to `lastindex(prefix)-lastindex(prefix)==0`,
    # hence we need to add one to get the first index. This is also correct when considering
    # pos, because pos is the `lastindex` a larger string which `endswith(path)==true`.
    return matchList, startpos:pos, !isempty(matchList)
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
    braces = 0
    r = reverse(s)
    i = firstindex(r)
    in_single_quotes = false
    in_double_quotes = false
    in_back_ticks = false
    in_comment = 0
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
            elseif c == '\''
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

struct REPLInterpreterCache
    dict::IdDict{MethodInstance,CodeInstance}
end
REPLInterpreterCache() = REPLInterpreterCache(IdDict{MethodInstance,CodeInstance}())
const REPL_INTERPRETER_CACHE = REPLInterpreterCache()

function get_code_cache()
    # XXX Avoid storing analysis results into the cache that persists across precompilation,
    #     as [sys|pkg]image currently doesn't support serializing externally created `CodeInstance`.
    #     Otherwise, `CodeInstance`s created by `REPLInterpreter``, that are much less optimized
    #     that those produced by `NativeInterpreter`, will leak into the native code cache,
    #     potentially causing runtime slowdown.
    #     (see https://github.com/JuliaLang/julia/issues/48453).
    if (@ccall jl_generating_output()::Cint) == 1
        return REPLInterpreterCache()
    else
        return REPL_INTERPRETER_CACHE
    end
end

struct REPLInterpreter <: CC.AbstractInterpreter
    repl_frame::CC.InferenceResult
    world::UInt
    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::Vector{CC.InferenceResult}
    code_cache::REPLInterpreterCache
    function REPLInterpreter(repl_frame::CC.InferenceResult;
                             world::UInt = Base.get_world_counter(),
                             inf_params::CC.InferenceParams = CC.InferenceParams(),
                             opt_params::CC.OptimizationParams = CC.OptimizationParams(),
                             inf_cache::Vector{CC.InferenceResult} = CC.InferenceResult[],
                             code_cache::REPLInterpreterCache = get_code_cache())
        return new(repl_frame, world, inf_params, opt_params, inf_cache, code_cache)
    end
end
CC.InferenceParams(interp::REPLInterpreter) = interp.inf_params
CC.OptimizationParams(interp::REPLInterpreter) = interp.opt_params
CC.get_world_counter(interp::REPLInterpreter) = interp.world
CC.get_inference_cache(interp::REPLInterpreter) = interp.inf_cache
CC.code_cache(interp::REPLInterpreter) = CC.WorldView(interp.code_cache, CC.WorldRange(interp.world))
CC.get(wvc::CC.WorldView{REPLInterpreterCache}, mi::MethodInstance, default) = get(wvc.cache.dict, mi, default)
CC.getindex(wvc::CC.WorldView{REPLInterpreterCache}, mi::MethodInstance) = getindex(wvc.cache.dict, mi)
CC.haskey(wvc::CC.WorldView{REPLInterpreterCache}, mi::MethodInstance) = haskey(wvc.cache.dict, mi)
CC.setindex!(wvc::CC.WorldView{REPLInterpreterCache}, ci::CodeInstance, mi::MethodInstance) = setindex!(wvc.cache.dict, ci, mi)

# REPLInterpreter is only used for type analysis, so it should disable optimization entirely
CC.may_optimize(::REPLInterpreter) = false

# REPLInterpreter analyzes a top-level frame, so better to not bail out from it
CC.bail_out_toplevel_call(::REPLInterpreter, ::CC.InferenceLoopState, ::CC.InferenceState) = false

# `REPLInterpreter` aggressively resolves global bindings to enable reasonable completions
# for lines like `Mod.a.|` (where `|` is the cursor position).
# Aggressive binding resolution poses challenges for the inference cache validation
# (until https://github.com/JuliaLang/julia/issues/40399 is implemented).
# To avoid the cache validation issues, `REPLInterpreter` only allows aggressive binding
# resolution for top-level frame representing REPL input code (`repl_frame`) and for child
# `getproperty` frames that are constant propagated from the `repl_frame`. This works, since
# a.) these frames are never cached, and
# b.) their results are only observed by the non-cached `repl_frame`.
#
# `REPLInterpreter` also aggressively concrete evaluate `:inconsistent` calls within
# `repl_frame` to provide reasonable completions for lines like `Ref(Some(42))[].|`.
# Aggressive concrete evaluation allows us to get accurate type information about complex
# expressions that otherwise can not be constant folded, in a safe way, i.e. it still
# doesn't evaluate effectful expressions like `pop!(xs)`.
# Similarly to the aggressive binding resolution, aggressive concrete evaluation doesn't
# present any cache validation issues because `repl_frame` is never cached.

is_repl_frame(interp::REPLInterpreter, sv::CC.InferenceState) = interp.repl_frame === sv.result

# aggressive global binding resolution within `repl_frame`
function CC.abstract_eval_globalref(interp::REPLInterpreter, g::GlobalRef,
                                    sv::CC.InferenceState)
    if is_repl_frame(interp, sv)
        if CC.isdefined_globalref(g)
            return Const(ccall(:jl_get_globalref_value, Any, (Any,), g))
        end
        return Union{}
    end
    return @invoke CC.abstract_eval_globalref(interp::CC.AbstractInterpreter, g::GlobalRef,
                                              sv::CC.InferenceState)
end

function is_repl_frame_getproperty(interp::REPLInterpreter, sv::CC.InferenceState)
    def = sv.linfo.def
    def isa Method || return false
    def.name === :getproperty || return false
    sv.cached && return false
    return is_repl_frame(interp, sv.parent)
end

# aggressive global binding resolution for `getproperty(::Module, ::Symbol)` calls within `repl_frame`
function CC.builtin_tfunction(interp::REPLInterpreter, @nospecialize(f),
                              argtypes::Vector{Any}, sv::CC.InferenceState)
    if f === Core.getglobal && is_repl_frame_getproperty(interp, sv)
        if length(argtypes) == 2
            a1, a2 = argtypes
            if isa(a1, Const) && isa(a2, Const)
                a1val, a2val = a1.val, a2.val
                if isa(a1val, Module) && isa(a2val, Symbol)
                    g = GlobalRef(a1val, a2val)
                    if CC.isdefined_globalref(g)
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
    if is_repl_frame(interp, sv)
        neweffects = CC.Effects(result.effects; consistent=CC.ALWAYS_TRUE)
        result = CC.MethodCallResult(result.rt, result.edgecycle, result.edgelimited,
                                     result.edge, neweffects)
    end
return @invoke CC.concrete_eval_eligible(interp::CC.AbstractInterpreter, f::Any,
                                         result::CC.MethodCallResult, arginfo::CC.ArgInfo,
                                         sv::CC.InferenceState)
end

function resolve_toplevel_symbols!(mod::Module, src::Core.CodeInfo)
    newsrc = copy(src)
    @ccall jl_resolve_globals_in_ir(
        #=jl_array_t *stmts=# newsrc.code::Any,
        #=jl_module_t *m=# mod::Any,
        #=jl_svec_t *sparam_vals=# Core.svec()::Any,
        #=int binding_effects=# 0::Int)::Cvoid
    return newsrc
end

# lower `ex` and run type inference on the resulting top-level expression
function repl_eval_ex(@nospecialize(ex), context_module::Module)
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

    # construct top-level `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.specTypes = Tuple{}

    mi.def = context_module
    src = resolve_toplevel_symbols!(context_module, src)
    @atomic mi.uninferred = src

    result = CC.InferenceResult(mi)
    interp = REPLInterpreter(result)
    frame = CC.InferenceState(result, src, #=cache=#:no, interp)::CC.InferenceState

    CC.typeinf(interp, frame)

    result = frame.result.result
    result === Union{} && return nothing # for whatever reason, callers expect this as the Bottom and/or Top type instead
    return result
end

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

    seen = Base.IdSet()
    for name in names(callee_module; all=true)
        if !Base.isdeprecated(callee_module, name) && isdefined(callee_module, name) && !startswith(string(name), '#')
            func = getfield(callee_module, name)
            if !isa(func, Module)
                funct = Core.Typeof(func)
                if !in(funct, seen)
                    push!(seen, funct)
                    complete_methods!(out, funct, args_ex, kwargs_ex, MAX_ANY_METHOD_COMPLETIONS, false)
                end
            elseif callee_module === Main && isa(func, Module)
                callee_module2 = func
                for name in names(callee_module2)
                    if !Base.isdeprecated(callee_module2, name) && isdefined(callee_module2, name) && !startswith(string(name), '#')
                        func = getfield(callee_module, name)
                        if !isa(func, Module)
                            funct = Core.Typeof(func)
                            if !in(funct, seen)
                                push!(seen, funct)
                                complete_methods!(out, funct, args_ex, kwargs_ex, MAX_ANY_METHOD_COMPLETIONS, false)
                            end
                        end
                    end
                end
            end
        end
    end

    if !shift
        # Filter out methods where all arguments are `Any`
        filter!(out) do c
            isa(c, TextCompletion) && return false
            isa(c, MethodCompletion) || return true
            sig = Base.unwrap_unionall(c.method.sig)::DataType
            return !all(T -> T === Any || T === Vararg{Any}, sig.parameters[2:end])
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

# Aux function to detect whether we're right after a
# using or import keyword
function afterusing(string::String, startpos::Int)
    (isempty(string) || startpos == 0) && return false
    str = string[1:prevind(string,startpos)]
    isempty(str) && return false
    rstr = reverse(str)
    r = findfirst(r"\s(gnisu|tropmi)\b", rstr)
    r === nothing && return false
    fr = reverseind(str, last(r))
    return occursin(r"^\b(using|import)\s*((\w+[.])*\w+\s*,\s*)*$", str[fr:end])
end

function close_path_completion(str, startpos, r, paths, pos)
    length(paths) == 1 || return false  # Only close if there's a single choice...
    _path = str[startpos:prevind(str, first(r))] * (paths[1]::PathCompletion).path
    path = expanduser(replace(_path, r"\\ " => " "))
    # ...except if it's a directory...
    try
        isdir(path)
    catch e
        e isa Base.IOError || rethrow() # `path` cannot be determined to be a file
    end && return false
    # ...and except if there's already a " at the cursor.
    return lastindex(str) <= pos || str[nextind(str, pos)] != '"'
end

function bslash_completions(string::String, pos::Int)
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
        if startswith(s, "\\:") # emoji
            namelist = Iterators.filter(k -> startswith(k, s), keys(emoji_symbols))
        else # latex
            namelist = Iterators.filter(k -> startswith(k, s), keys(latex_symbols))
        end
        return (true, (Completion[BslashCompletion(name) for name in sort!(collect(namelist))], slashpos:pos, true))
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
    obj = context_module
    for name in split(str[frange[1]:end_of_identifier], '.')
        Base.isidentifier(name) || return (nothing, nothing, nothing)
        sym = Symbol(name)
        isdefined(obj, sym) || return (nothing, nothing, nothing)
        obj = getfield(obj, sym)
    end
    (isa(obj, AbstractDict) && length(obj)::Int < 1_000_000) || return (nothing, nothing, nothing)
    begin_of_key = something(findnext(!isspace, str, nextind(str, end_of_identifier) + 1), # +1 for [
                             lastindex(str)+1)
    return (obj::AbstractDict, str[begin_of_key:end], begin_of_key)
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
function complete_keyword_argument(partial, last_idx, context_module)
    frange, ex, wordrange, = identify_possible_method_completion(partial, last_idx)
    fail = Completion[], 0:-1, frange
    ex.head === :call || is_broadcasting_expr(ex) || return fail

    kwargs_flag, funct, args_ex, kwargs_ex = _complete_methods(ex, context_module, true)::Tuple{Int, Any, Vector{Any}, Set{Symbol}}
    kwargs_flag == 2 && return fail # one of the previous kwargs is invalid

    methods = Completion[]
    complete_methods!(methods, funct, Any[Vararg{Any}], kwargs_ex, -1, kwargs_flag == 1)
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
    append!(suggestions, complete_symbol(nothing, last_word, Returns(true), context_module))

    return sort!(suggestions, by=completion_text), wordrange
end

function project_deps_get_completion_candidates(pkgstarts::String, project_file::String)
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
    return Completion[PackageCompletion(name) for name in loading_candidates]
end

function complete_identifiers!(suggestions::Vector{Completion}, @nospecialize(ffunc::Function), context_module::Module, string::String, name::String, pos::Int, dotpos::Int, startpos::Int, comp_keywords=false)
    ex = nothing
    comp_keywords && append!(suggestions, complete_keyword(name))
    if dotpos > 1 && string[dotpos] == '.'
        s = string[1:dotpos-1]
        # First see if the whole string up to `pos` is a valid expression. If so, use it.
        ex = Meta.parse(s, raise=false, depwarn=false)
        if isexpr(ex, :incomplete)
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

                ex = Meta.parse(lookup_name, raise=false, depwarn=false)
            end
            isexpr(ex, :incomplete) && (ex = nothing)
        end
    end
    append!(suggestions, complete_symbol(ex, name, ffunc, context_module))
    return sort!(unique(suggestions), by=completion_text), (dotpos+1):pos, true
end

function completions(string::String, pos::Int, context_module::Module=Main, shift::Bool=true)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = Base.incomplete_tag(Meta.parse(partial, raise=false, depwarn=false))

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

    # if completing a key in a Dict
    identifier, partial_key, loc = dict_identifier_key(partial, inc_tag, context_module)
    if identifier !== nothing
        matches = find_dict_matches(identifier, partial_key)
        length(matches)==1 && (lastindex(string) <= pos || string[nextind(string,pos)] != ']') && (matches[1]*=']')
        length(matches)>0 && return Completion[DictCompletion(identifier, match) for match in sort!(matches)], loc::Int:pos, true
    end

    ffunc = Returns(true)
    suggestions = Completion[]

    # Check if this is a var"" string macro that should be completed like
    # an identifier rather than a string.
    # TODO: It would be nice for the parser to give us more information here
    # so that we can lookup the macro by identity rather than pattern matching
    # its invocation.
    varrange = findprev("var\"", string, pos)

    if varrange !== nothing
        ok, ret = bslash_completions(string, pos)
        ok && return ret
        startpos = first(varrange) + 4
        dotpos = something(findprev(isequal('.'), string, first(varrange)-1), 0)
        return complete_identifiers!(Completion[], ffunc, context_module, string,
            string[startpos:pos], pos, dotpos, startpos)
    # otherwise...
    elseif inc_tag in [:cmd, :string]
        m = match(r"[\t\n\r\"`><=*?|]| (?!\\)", reverse(partial))
        startpos = nextind(partial, reverseind(partial, m.offset))
        r = startpos:pos

        expanded = complete_expanduser(replace(string[r], r"\\ " => " "), r)
        expanded[3] && return expanded  # If user expansion available, return it

        paths, r, success = complete_path(replace(string[r], r"\\ " => " "), pos)

        if inc_tag === :string && close_path_completion(string, startpos, r, paths, pos)
            paths[1] = PathCompletion((paths[1]::PathCompletion).path * "\"")
        end

        #Latex symbols can be completed for strings
        (success || inc_tag === :cmd) && return sort!(paths, by=p->p.path), r, success
    end

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
    kwarg_completion, wordrange = complete_keyword_argument(partial, pos, context_module)
    isempty(wordrange) || return kwarg_completion, wordrange, !isempty(kwarg_completion)

    dotpos = something(findprev(isequal('.'), string, pos), 0)
    startpos = nextind(string, something(findprev(in(non_identifier_chars), string, pos), 0))
    # strip preceding ! operator
    if (m = match(r"\G\!+", partial, startpos)) isa RegexMatch
        startpos += length(m.match)
    end

    name = string[max(startpos, dotpos+1):pos]
    comp_keywords = !isempty(name) && startpos > dotpos
    if afterusing(string, startpos)
        # We're right after using or import. Let's look only for packages
        # and modules we can reach from here

        # If there's no dot, we're in toplevel, so we should
        # also search for packages
        s = string[startpos:pos]
        if dotpos <= startpos
            for dir in Base.load_path()
                if basename(dir) in Base.project_names && isfile(dir)
                    append!(suggestions, project_deps_get_completion_candidates(s, dir))
                end
                isdir(dir) || continue
                for pname in readdir(dir)
                    if pname[1] != '.' && pname != "METADATA" &&
                        pname != "REQUIRE" && startswith(pname, s)
                        # Valid file paths are
                        #   <Mod>.jl
                        #   <Mod>/src/<Mod>.jl
                        #   <Mod>.jl/src/<Mod>.jl
                        if isfile(joinpath(dir, pname))
                            endswith(pname, ".jl") && push!(suggestions,
                                                            PackageCompletion(pname[1:prevind(pname, end-2)]))
                        else
                            mod_name = if endswith(pname, ".jl")
                                pname[1:prevind(pname, end-2)]
                            else
                                pname
                            end
                            if isfile(joinpath(dir, pname, "src",
                                               "$mod_name.jl"))
                                push!(suggestions, PackageCompletion(mod_name))
                            end
                        end
                    end
                end
            end
        end
        ffunc = (mod,x)->(Base.isbindingresolved(mod, x) && isdefined(mod, x) && isa(getfield(mod, x), Module))
        comp_keywords = false
    end

    startpos == 0 && (pos = -1)
    dotpos < startpos && (dotpos = startpos - 1)
    return complete_identifiers!(suggestions, ffunc, context_module, string,
        name, pos, dotpos, startpos, comp_keywords)
end

function shell_completions(string, pos)
    # First parse everything up to the current position
    scs = string[1:pos]
    local args, last_parse
    try
        args, last_parse = Base.shell_parse(scs, true)::Tuple{Expr,UnitRange{Int}}
    catch
        return Completion[], 0:-1, false
    end
    ex = args.args[end]::Expr
    # Now look at the last thing we parsed
    isempty(ex.args) && return Completion[], 0:-1, false
    arg = ex.args[end]
    if all(s -> isa(s, AbstractString), ex.args)
        arg = arg::AbstractString
        # Treat this as a path

        # As Base.shell_parse throws away trailing spaces (unless they are escaped),
        # we need to special case here.
        # If the last char was a space, but shell_parse ignored it search on "".
        ignore_last_word = arg != " " && scs[end] == ' '
        prefix = ignore_last_word ? "" : join(ex.args)

        # Also try looking into the env path if the user wants to complete the first argument
        use_envpath = !ignore_last_word && length(args.args) < 2

        return complete_path(prefix, pos, use_envpath=use_envpath, shell_escape=true)
    elseif isexpr(arg, :incomplete) || isexpr(arg, :error)
        partial = scs[last_parse]
        ret, range = completions(partial, lastindex(partial))
        range = range .+ (first(last_parse) - 1)
        return ret, range, true
    end
    return Completion[], 0:-1, false
end

function UndefVarError_hint(io::IO, ex::UndefVarError)
    var = ex.var
    if var === :or
        print(io, "\nsuggestion: Use `||` for short-circuiting boolean OR.")
    elseif var === :and
        print(io, "\nsuggestion: Use `&&` for short-circuiting boolean AND.")
    elseif var === :help
        println(io)
        # Show friendly help message when user types help or help() and help is undefined
        show(io, MIME("text/plain"), Base.Docs.parsedoc(Base.Docs.keywords[:help]))
    elseif var === :quit
        print(io, "\nsuggestion: To exit Julia, use Ctrl-D, or type exit() and press enter.")
    end
end

function __init__()
    Base.Experimental.register_error_hint(UndefVarError_hint, UndefVarError)
    nothing
end

end # module
