# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    code_lowered(f, types; generated=true, debuginfo=:default)

Return an array of the lowered forms (IR) for the methods matching the given generic function
and type signature.

If `generated` is `false`, the returned `CodeInfo` instances will correspond to fallback
implementations. An error is thrown if no fallback implementation exists.
If `generated` is `true`, these `CodeInfo` instances will correspond to the method bodies
yielded by expanding the generators.

The keyword `debuginfo` controls the amount of code metadata present in the output.

Note that an error will be thrown if `types` are not concrete types when `generated` is
`true` and any of the corresponding methods are an `@generated` method.
"""
function code_lowered(@nospecialize(f), @nospecialize(t=Tuple); generated::Bool=true, debuginfo::Symbol=:default)
    if @isdefined(IRShow)
        debuginfo = IRShow.debuginfo(debuginfo)
    elseif debuginfo === :default
        debuginfo = :source
    end
    if debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    world = get_world_counter()
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    ret = CodeInfo[]
    for m in method_instances(f, t, world)
        if generated && hasgenerator(m)
            if may_invoke_generator(m)
                code = ccall(:jl_code_for_staged, Ref{CodeInfo}, (Any, UInt, Ptr{Cvoid}), m, world, C_NULL)
            else
                error("Could not expand generator for `@generated` method ", m, ". ",
                      "This can happen if the provided argument types (", t, ") are ",
                      "not concrete types, but the `generated` argument is `true`.")
            end
        else
            code = uncompressed_ir(m.def::Method)
            debuginfo === :none && remove_linenums!(code)
        end
        push!(ret, code)
    end
    return ret
end

# for backwards compat
const uncompressed_ast = uncompressed_ir
const _uncompressed_ast = _uncompressed_ir

function method_instances(@nospecialize(f), @nospecialize(t), world::UInt)
    tt = signature_type(f, t)
    results = Core.MethodInstance[]
    # this make a better error message than the typeassert that follows
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    for match in _methods_by_ftype(tt, -1, world)::Vector
        instance = specialize_method(match::Core.MethodMatch)
        push!(results, instance)
    end
    return results
end

function method_instance(@nospecialize(f), @nospecialize(t);
                         world=Base.get_world_counter(), method_table=nothing)
    tt = signature_type(f, t)
    mi = ccall(:jl_method_lookup_by_tt, Any,
                (Any, Csize_t, Any),
                tt, world, method_table)
    return mi::Union{Nothing, MethodInstance}
end

default_debug_info_kind() = unsafe_load(cglobal(:jl_default_debug_info_kind, Cint))

# this type mirrors jl_cgparams_t (documented in julia.h)
struct CodegenParams
    """
    If enabled, generate the necessary code to support the --track-allocations
    command line flag to julia itself. Note that the option itself does not enable
    allocation tracking. Rather, it merely generates the support code necessary
    to perform allocation tracking if requested by the command line option.
    """
    track_allocations::Cint

    """
    If enabled, generate the necessary code to support the --code-coverage
    command line flag to julia itself. Note that the option itself does not enable
    code coverage. Rather, it merely generates the support code necessary
    to code coverage if requested by the command line option.
    """
    code_coverage::Cint

    """
    If enabled, force the compiler to use the specialized signature
    for all generated functions, whenever legal. If disabled, the choice is made
    heuristically and specsig is only used when deemed profitable.
    """
    prefer_specsig::Cint

    """
    If enabled, enable emission of `.debug_names` sections.
    """
    gnu_pubnames::Cint

    """
    Controls what level of debug info to emit. Currently supported values are:
    - 0: no debug info
    - 1: full debug info
    - 2: Line tables only
    - 3: Debug directives only

    The integer values currently match the llvm::DICompilerUnit::DebugEmissionKind enum,
    although this is not guaranteed.
    """
    debug_info_kind::Cint

    """
    Controls the debug_info_level parameter, equivalent to the -g command line option.
    """
    debug_info_level::Cint

    """
    If enabled, generate a GC safepoint at the entry to every function. Emitting
    these extra safepoints can reduce the amount of time that other threads are
    waiting for the currently running thread to reach a safepoint. The cost for
    a safepoint is small, but non-zero. The option is enabled by default.
    """
    safepoint_on_entry::Cint

    """
    If enabled, add an implicit argument to each function call that is used to
    pass down the current task local state pointer. This argument is passed
    using the `swiftself` convention, which in the ordinary case means that the
    pointer is kept in a register and accesses are thus very fast. If this option
    is disabled, the task local state pointer must be loaded from thread local
    storage, which incurs a small amount of additional overhead. The option is enabled by
    default.
    """
    gcstack_arg::Cint

    """
    If enabled, use the Julia PLT mechanism to support lazy-resolution of `ccall`
    targets. The option may be disabled for use in environments where the julia
    runtime is unavailable, but is otherwise recommended to be enabled, even if
    lazy resolution is not required, as the Julia PLT mechanism may have superior
    performance compared to the native platform mechanism. The options is enabled by default.
    """
    use_jlplt::Cint

    """
        If enabled emit LLVM IR for all functions even if wouldn't be compiled
        for some reason (i.e functions that return a constant value).
    """
    force_emit_all::Cint

    function CodegenParams(; track_allocations::Bool=true, code_coverage::Bool=true,
                   prefer_specsig::Bool=false,
                   gnu_pubnames::Bool=true, debug_info_kind::Cint = default_debug_info_kind(),
                   debug_info_level::Cint = Cint(JLOptions().debug_level), safepoint_on_entry::Bool=true,
                   gcstack_arg::Bool=true, use_jlplt::Bool=true, force_emit_all::Bool=false)
        return new(
            Cint(track_allocations), Cint(code_coverage),
            Cint(prefer_specsig),
            Cint(gnu_pubnames), debug_info_kind,
            debug_info_level, Cint(safepoint_on_entry),
            Cint(gcstack_arg), Cint(use_jlplt), Cint(force_emit_all))
    end
end

# this type mirrors jl_emission_params_t (documented in julia.h)
struct EmissionParams
    emit_metadata::Cint

    function EmissionParams(; emit_metadata::Bool=true)
        return new(Cint(emit_metadata))
    end
end

"""
    code_typed(f, types; kw...)

Returns an array of type-inferred lowered form (IR) for the methods matching the given
generic function and type signature.

# Keyword Arguments

- `optimize::Bool = true`: optional, controls whether additional optimizations,
  such as inlining, are also applied.
- `debuginfo::Symbol = :default`: optional, controls the amount of code metadata present
  in the output, possible options are `:source` or `:none`.

# Internal Keyword Arguments

This section should be considered internal, and is only for who understands Julia compiler
internals.

- `world::UInt = Base.get_world_counter()`: optional, controls the world age to use
  when looking up methods, use current world age if not specified.
- `interp::Core.Compiler.AbstractInterpreter = Core.Compiler.NativeInterpreter(world)`:
  optional, controls the abstract interpreter to use, use the native interpreter if not specified.

# Examples

One can put the argument types in a tuple to get the corresponding `code_typed`.

```julia
julia> code_typed(+, (Float64, Float64))
1-element Vector{Any}:
 CodeInfo(
1 ─ %1 = Base.add_float(x, y)::Float64
└──      return %1
) => Float64
```
"""
function code_typed(@nospecialize(f), @nospecialize(types=default_tt(f)); kwargs...)
    if isa(f, Core.OpaqueClosure)
        return code_typed_opaque_closure(f, types; kwargs...)
    end
    tt = signature_type(f, types)
    return code_typed_by_type(tt; kwargs...)
end

# returns argument tuple type which is supposed to be used for `code_typed` and its family;
# if there is a single method this functions returns the method argument signature,
# otherwise returns `Tuple` that doesn't match with any signature
function default_tt(@nospecialize(f))
    ms = methods(f)
    if length(ms) == 1
        return tuple_type_tail(only(ms).sig)
    else
        return Tuple
    end
end

function raise_match_failure(name::Symbol, @nospecialize(tt))
    @noinline
    sig_str = sprint(Base.show_tuple_as_call, Symbol(""), tt)
    error("$name: unanalyzable call given $sig_str")
end

const REFLECTION_COMPILER = RefValue{Union{Nothing, Module}}(nothing)

function invoke_in_typeinf_world(args...)
    vargs = Any[args...]
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Any}, Cint), vargs, length(vargs))
end

function invoke_default_compiler(fname::Symbol, args...)
    if REFLECTION_COMPILER[] === nothing
        return invoke_in_typeinf_world(getglobal(Compiler, fname), args...)
    else
        return getglobal(REFLECTION_COMPILER[], fname)(args...)
    end
end

function invoke_interp_compiler(interp, fname::Symbol, args...)
    if interp === nothing
        return invoke_default_compiler(fname, args...)
    else
        T = typeof(interp)
        while true
            Tname = typename(T).name
            Tname === :Any && error("Expected Interpreter")
            Tname === :AbstractInterpreter && break
            T = supertype(T)
        end
        return getglobal(typename(T).module, fname)(args...)
    end
end

"""
    code_typed_by_type(types::Type{<:Tuple}; ...)

Similar to [`code_typed`](@ref), except the argument is a tuple type describing
a full signature to query.
"""
function code_typed_by_type(@nospecialize(tt::Type);
                            optimize::Bool=true,
                            debuginfo::Symbol=:default,
                            world::UInt=get_world_counter(),
                            interp=nothing)
    passed_interp = interp
    interp = passed_interp === nothing ? invoke_default_compiler(:_default_interp, world) : interp
    (ccall(:jl_is_in_pure_context, Bool, ()) || world == typemax(UInt)) &&
        error("code reflection cannot be used from generated functions")
    if @isdefined(IRShow)
        debuginfo = IRShow.debuginfo(debuginfo)
    elseif debuginfo === :default
        debuginfo = :source
    end
    if debuginfo !== :source && debuginfo !== :none
        throw(ArgumentError("'debuginfo' must be either :source or :none"))
    end
    tt = to_tuple_type(tt)
    matches = invoke_interp_compiler(passed_interp, :_findall_matches, interp, tt)
    matches === nothing && raise_match_failure(:code_typed, tt)
    asts = []
    for match in matches.matches
        match = match::Core.MethodMatch
        code = invoke_interp_compiler(passed_interp, :typeinf_code, interp, match, optimize)
        if code === nothing
            push!(asts, match.method => Any)
        else
            debuginfo === :none && remove_linenums!(code)
            push!(asts, code => code.rettype)
        end
    end
    return asts
end

function get_oc_code_rt(passed_interp, oc::Core.OpaqueClosure, types, optimize::Bool)
    @nospecialize oc types
    ccall(:jl_is_in_pure_context, Bool, ()) &&
        error("code reflection cannot be used from generated functions")
    m = oc.source
    if isa(m, Method)
        if isdefined(m, :source)
            if optimize
                tt = Tuple{typeof(oc.captures), to_tuple_type(types).parameters...}
                mi = specialize_method(m, tt, Core.svec())
                interp = invoke_interp_compiler(passed_interp, :_default_interp, m.primary_world)
                code = invoke_interp_compiler(passed_interp, :typeinf_code, interp, mi, optimize)
                if code isa CodeInfo
                    return Pair{CodeInfo, Any}(code, code.rettype)
                end
                error("inference not successful")
            else
                code = _uncompressed_ir(m)
                return Pair{CodeInfo, Any}(code, typeof(oc).parameters[2])
            end
        else
            # OC constructed from optimized IR
            codeinst = m.specializations.cache
            # XXX: the inferred field is not normally a CodeInfo, but this assumes it is guaranteed to be always
            return Pair{CodeInfo, Any}(codeinst.inferred, codeinst.rettype)
        end
    else
        error("encountered invalid Core.OpaqueClosure object")
    end
end

function code_typed_opaque_closure(oc::Core.OpaqueClosure, types;
                                   debuginfo::Symbol=:default,
                                   optimize::Bool=true,
                                   interp=nothing,
                                   _...)
    @nospecialize oc types
    (code, rt) = get_oc_code_rt(interp, oc, types, optimize)
    debuginfo === :none && remove_linenums!(code)
    return Any[Pair{CodeInfo,Any}(code, rt)]
end

"""
    code_ircode(f, [types])

Return an array of pairs of `IRCode` and inferred return type if type inference succeeds.
The `Method` is included instead of `IRCode` otherwise.

See also: [`code_typed`](@ref)

# Internal Keyword Arguments

This section should be considered internal, and is only for who understands Julia compiler
internals.

- `world::UInt = Base.get_world_counter()`: optional, controls the world age to use
  when looking up methods, use current world age if not specified.
- `interp::Core.Compiler.AbstractInterpreter = Core.Compiler.NativeInterpreter(world)`:
  optional, controls the abstract interpreter to use, use the native interpreter if not specified.
- `optimize_until::Union{Int,String,Nothing} = nothing`: optional,
  controls the optimization passes to run.
  If it is a string, it specifies the name of the pass up to which the optimizer is run.
  If it is an integer, it specifies the number of passes to run.
  If it is `nothing` (default), all passes are run.

# Examples

One can put the argument types in a tuple to get the corresponding `code_ircode`.

```julia
julia> Base.code_ircode(+, (Float64, Int64))
1-element Vector{Any}:
 388 1 ─ %1 = Base.sitofp(Float64, _3)::Float64
    │   %2 = Base.add_float(_2, %1)::Float64
    └──      return %2
     => Float64

julia> Base.code_ircode(+, (Float64, Int64); optimize_until = "compact 1")
1-element Vector{Any}:
 388 1 ─ %1 = Base.promote(_2, _3)::Tuple{Float64, Float64}
    │   %2 = Core._apply_iterate(Base.iterate, Base.:+, %1)::Float64
    └──      return %2
     => Float64
```
"""
function code_ircode(@nospecialize(f), @nospecialize(types = default_tt(f)); kwargs...)
    if isa(f, Core.OpaqueClosure)
        error("OpaqueClosure not supported")
    end
    tt = signature_type(f, types)
    return code_ircode_by_type(tt; kwargs...)
end

"""
    code_ircode_by_type(types::Type{<:Tuple}; ...)

Similar to [`code_ircode`](@ref), except the argument is a tuple type describing
a full signature to query.
"""
function code_ircode_by_type(
    @nospecialize(tt::Type);
    world::UInt=get_world_counter(),
    interp=nothing,
    optimize_until::Union{Int,String,Nothing}=nothing,
)
    passed_interp = interp
    interp = passed_interp === nothing ? invoke_default_compiler(:_default_interp, world) : interp
    (ccall(:jl_is_in_pure_context, Bool, ()) || world == typemax(UInt)) &&
        error("code reflection cannot be used from generated functions")
    tt = to_tuple_type(tt)
    matches = invoke_interp_compiler(passed_interp, :_findall_matches, interp, tt)
    matches === nothing && raise_match_failure(:code_ircode, tt)
    asts = []
    for match in matches.matches
        match = match::Core.MethodMatch
        (code, ty) = invoke_interp_compiler(passed_interp, :typeinf_ircode, interp, match, optimize_until)
        if code === nothing
            push!(asts, match.method => Any)
        else
            push!(asts, code => ty)
        end
    end
    return asts
end

function _builtin_return_type(passed_interp, interp,
                              @nospecialize(f::Core.Builtin), @nospecialize(types))
    argtypes = Any[to_tuple_type(types).parameters...]
    rt = invoke_interp_compiler(passed_interp, :builtin_tfunction, interp, f, argtypes, nothing)
    return invoke_interp_compiler(passed_interp, :widenconst, rt)
end

function _builtin_effects(passed_interp, interp,
                          @nospecialize(f::Core.Builtin), @nospecialize(types))
    argtypes = Any[to_tuple_type(types).parameters...]
    rt = invoke_interp_compiler(passed_interp, :builtin_tfunction, interp, f, argtypes, nothing)
    return invoke_interp_compiler(passed_interp, :builtin_effects,
        invoke_interp_compiler(passed_interp, :typeinf_lattice, interp),
        f, argtypes, rt)
end

function _builtin_exception_type(passed_interp, interp,
                                 @nospecialize(f::Core.Builtin), @nospecialize(types))
    effects = _builtin_effects(passed_interp, interp, f, types)
    return invoke_interp_compiler(passed_interp, :is_nothrow, effects) ? Union{} : Any
end

check_generated_context(world::UInt) =
    (ccall(:jl_is_in_pure_context, Bool, ()) || world == typemax(UInt)) &&
        error("code reflection cannot be used from generated functions")

# TODO rename `Base.return_types` to `Base.infer_return_types`

"""
    Base.return_types(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::NativeInterpreter=Core.Compiler.NativeInterpreter(world)) -> rts::Vector{Any}

Return a list of possible return types for a given function `f` and argument types `types`.
The list corresponds to the results of type inference on all the possible method match
candidates for `f` and `types` (see also [`methods(f, types)`](@ref methods).

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `rts::Vector{Any}`: The list of return types that are figured out by inference on
  methods matching with the given `f` and `types`. The list's order matches the order
  returned by `methods(f, types)`.

# Examples

```julia
julia> Base.return_types(sum, Tuple{Vector{Int}})
1-element Vector{Any}:
 Int64

julia> methods(sum, (Union{Vector{Int},UnitRange{Int}},))
# 2 methods for generic function "sum" from Base:
 [1] sum(r::AbstractRange{<:Real})
     @ range.jl:1399
 [2] sum(a::AbstractArray; dims, kw...)
     @ reducedim.jl:1010

julia> Base.return_types(sum, (Union{Vector{Int},UnitRange{Int}},))
2-element Vector{Any}:
 Int64 # the result of inference on sum(r::AbstractRange{<:Real})
 Int64 # the result of inference on sum(a::AbstractArray; dims, kw...)
```

!!! warning
    The `Base.return_types` function should not be used from generated functions;
    doing so will result in an error.
"""
function return_types(@nospecialize(f), @nospecialize(types=default_tt(f));
                      world::UInt=get_world_counter(),
                      interp=nothing)
    passed_interp = interp
    interp = passed_interp === nothing ? invoke_default_compiler(:_default_interp, world) : interp
    check_generated_context(world)
    if isa(f, Core.OpaqueClosure)
        _, rt = only(code_typed_opaque_closure(f, types; Compiler))
        return Any[rt]
    elseif isa(f, Core.Builtin)
        return Any[_builtin_return_type(passed_interp, interp, f, types)]
    end
    tt = signature_type(f, types)
    matches = invoke_interp_compiler(passed_interp, :_findall_matches, interp, tt)
    matches === nothing && raise_match_failure(:return_types, tt)
    rts = Any[]
    for match in matches.matches
        ty = invoke_interp_compiler(passed_interp, :typeinf_type, interp, match::Core.MethodMatch)
        push!(rts, something(ty, Any))
    end
    return rts
end

"""
    Base.infer_return_type(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world)) -> rt::Type

Returns an inferred return type of the function call specified by `f` and `types`.

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `rt::Type`: An inferred return type of the function call specified by the given call signature.

!!! note
    Note that, different from [`Base.return_types`](@ref), this doesn't give you the list
    return types of every possible method matching with the given `f` and `types`.
    It returns a single return type, taking into account all potential outcomes of
    any function call entailed by the given signature type.

# Examples

```julia
julia> checksym(::Symbol) = :symbol;

julia> checksym(x::Any) = x;

julia> Base.infer_return_type(checksym, (Union{Symbol,String},))
Union{String, Symbol}

julia> Base.return_types(checksym, (Union{Symbol,String},))
2-element Vector{Any}:
 Symbol
 Union{String, Symbol}
```

It's important to note the difference here: `Base.return_types` gives back inferred results
for each method that matches the given signature `checksum(::Union{Symbol,String})`.
On the other hand `Base.infer_return_type` returns one collective result that sums up all those possibilities.

!!! warning
    The `Base.infer_return_type` function should not be used from generated functions;
    doing so will result in an error.
"""
function infer_return_type(@nospecialize(f), @nospecialize(types=default_tt(f));
                           world::UInt=get_world_counter(),
                           interp=nothing)
    passed_interp = interp
    interp = passed_interp === nothing ? invoke_default_compiler(:_default_interp, world) : interp
    check_generated_context(world)
    if isa(f, Core.OpaqueClosure)
        return last(only(code_typed_opaque_closure(f, types; interp=passed_interp)))
    elseif isa(f, Core.Builtin)
        return _builtin_return_type(passed_interp, interp, f, types)
    end
    tt = signature_type(f, types)
    matches = invoke_interp_compiler(passed_interp, :_findall_matches, interp, tt)
    matches === nothing && raise_match_failure(:infer_return_type, tt)
    rt = Union{}
    for match in matches.matches
        ty = invoke_interp_compiler(passed_interp, :typeinf_type, interp, match::Core.MethodMatch)
        rt = invoke_interp_compiler(passed_interp, :tmerge, rt, something(ty, Any))
    end
    return rt
end

"""
    Base.infer_exception_types(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::NativeInterpreter=Core.Compiler.NativeInterpreter(world)) -> excts::Vector{Any}

Return a list of possible exception types for a given function `f` and argument types `types`.
The list corresponds to the results of type inference on all the possible method match
candidates for `f` and `types` (see also [`methods(f, types)`](@ref methods).
It works like [`Base.return_types`](@ref), but it infers the exception types instead of the return types.

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `excts::Vector{Any}`: The list of exception types that are figured out by inference on
  methods matching with the given `f` and `types`. The list's order matches the order
  returned by `methods(f, types)`.

# Examples

```julia
julia> throw_if_number(::Number) = error("number is given");

julia> throw_if_number(::Any) = nothing;

julia> Base.infer_exception_types(throw_if_number, (Int,))
1-element Vector{Any}:
 ErrorException

julia> methods(throw_if_number, (Any,))
# 2 methods for generic function "throw_if_number" from Main:
 [1] throw_if_number(x::Number)
     @ REPL[1]:1
 [2] throw_if_number(::Any)
     @ REPL[2]:1

julia> Base.infer_exception_types(throw_if_number, (Any,))
2-element Vector{Any}:
 ErrorException # the result of inference on `throw_if_number(::Number)`
 Union{}        # the result of inference on `throw_if_number(::Any)`
```

!!! warning
    The `Base.infer_exception_types` function should not be used from generated functions;
    doing so will result in an error.
"""
function infer_exception_types(@nospecialize(f), @nospecialize(types=default_tt(f));
                               world::UInt=get_world_counter(),
                               interp=nothing)
    passed_interp = interp
    interp = passed_interp === nothing ? invoke_default_compiler(:_default_interp, world) : interp
    check_generated_context(world)
    if isa(f, Core.OpaqueClosure)
        return Any[Any] # TODO
    elseif isa(f, Core.Builtin)
        return Any[_builtin_exception_type(passed_interp, interp, f, types)]
    end
    tt = signature_type(f, types)
    matches = invoke_interp_compiler(passed_interp, :_findall_matches, interp, tt)
    matches === nothing && raise_match_failure(:infer_exception_types, tt)
    excts = Any[]
    for match in matches.matches
        frame = invoke_interp_compiler(passed_interp, :typeinf_frame, interp, match::Core.MethodMatch, #=run_optimizer=#false)
        if frame === nothing
            exct = Any
        else
            exct = invoke_interp_compiler(passed_interp, :widenconst, frame.result.exc_result)
        end
        push!(excts, exct)
    end
    return excts
end

"""
    Base.infer_exception_type(
        f, types=default_tt(f);
        world::UInt=get_world_counter(),
        interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world)) -> exct::Type

Returns the type of exception potentially thrown by the function call specified by `f` and `types`.

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `exct::Type`: The inferred type of exception that can be thrown by the function call
  specified by the given call signature.

!!! note
    Note that, different from [`Base.infer_exception_types`](@ref), this doesn't give you the list
    exception types for every possible matching method with the given `f` and `types`.
    It returns a single exception type, taking into account all potential outcomes of
    any function call entailed by the given signature type.

# Examples

```julia
julia> f1(x) = x * 2;

julia> Base.infer_exception_type(f1, (Int,))
Union{}
```

The exception inferred as `Union{}` indicates that `f1(::Int)` will not throw any exception.

```julia
julia> f2(x::Int) = x * 2;

julia> Base.infer_exception_type(f2, (Integer,))
MethodError
```

This case is pretty much the same as with `f1`, but there's a key difference to note. For
`f2`, the argument type is limited to `Int`, while the argument type is given as `Tuple{Integer}`.
Because of this, taking into account the chance of the method error entailed by the call
signature, the exception type is widened to `MethodError`.

!!! warning
    The `Base.infer_exception_type` function should not be used from generated functions;
    doing so will result in an error.
"""
function infer_exception_type(@nospecialize(f), @nospecialize(types=default_tt(f));
                              world::UInt=get_world_counter(),
                              interp=nothing)
    passed_interp = interp
    interp = passed_interp === nothing ? invoke_default_compiler(:_default_interp, world) : interp
    check_generated_context(world)
    if isa(f, Core.OpaqueClosure)
        return Any # TODO
    elseif isa(f, Core.Builtin)
        return _builtin_exception_type(passed_interp, interp, f, types)
    end
    tt = signature_type(f, types)
    exct = invoke_interp_compiler(passed_interp, :_infer_exception_type, interp, tt, false)
    exct === nothing && raise_match_failure(:infer_exception_type, tt)
    return exct
end

"""
    Base.infer_effects(
        f, types=default_tt(f);
        optimize::Bool=true,
        world::UInt=get_world_counter(),
        interp::Core.Compiler.AbstractInterpreter=Core.Compiler.NativeInterpreter(world)) -> effects::Effects

Returns the possible computation effects of the function call specified by `f` and `types`.

# Arguments
- `f`: The function to analyze.
- `types` (optional): The argument types of the function. Defaults to the default tuple type of `f`.
- `optimize` (optional): Whether to run additional effects refinements based on post-optimization analysis.
- `world` (optional): The world counter to use for the analysis. Defaults to the current world counter.
- `interp` (optional): The abstract interpreter to use for the analysis. Defaults to a new `Core.Compiler.NativeInterpreter` with the specified `world`.

# Returns
- `effects::Effects`: The computed effects of the function call specified by the given call signature.
  See the documentation of [`Effects`](@ref Core.Compiler.Effects) or [`Base.@assume_effects`](@ref)
  for more information on the various effect properties.

!!! note
    Note that, different from [`Base.return_types`](@ref), this doesn't give you the list
    effect analysis results for every possible matching method with the given `f` and `types`.
    It returns a single effect, taking into account all potential outcomes of any function
    call entailed by the given signature type.

# Examples

```julia
julia> f1(x) = x * 2;

julia> Base.infer_effects(f1, (Int,))
(+c,+e,+n,+t,+s,+m,+i)
```

This function will return an `Effects` object with information about the computational
effects of the function `f1` when called with an `Int` argument.

```julia
julia> f2(x::Int) = x * 2;

julia> Base.infer_effects(f2, (Integer,))
(+c,+e,!n,+t,+s,+m,+i)
```

This case is pretty much the same as with `f1`, but there's a key difference to note. For
`f2`, the argument type is limited to `Int`, while the argument type is given as `Tuple{Integer}`.
Because of this, taking into account the chance of the method error entailed by the call
signature, the `:nothrow` bit gets tainted.

!!! warning
    The `Base.infer_effects` function should not be used from generated functions;
    doing so will result in an error.

$(Compiler.effects_key_string)

# See Also
- [`Compiler.Effects`](@ref): A type representing the computational effects of a method call.
- [`Base.@assume_effects`](@ref): A macro for making assumptions about the effects of a method.
"""
function infer_effects(@nospecialize(f), @nospecialize(types=default_tt(f));
                       optimize::Bool=true,
                       world::UInt=get_world_counter(),
                       interp=nothing)
    passed_interp = interp
    interp = passed_interp === nothing ? invoke_default_compiler(:_default_interp, world) : interp
    check_generated_context(world)
    if isa(f, Core.Builtin)
        return _builtin_effects(passed_interp, interp, f, types)
    end
    tt = signature_type(f, types)
    effects = invoke_interp_compiler(passed_interp, :_infer_effects, interp, tt, optimize)
    effects === nothing && raise_match_failure(:infer_effects, tt)
    return effects
end

"""
    print_statement_costs(io::IO, f, types)

Print type-inferred and optimized code for `f` given argument types `types`,
prepending each line with its cost as estimated by the compiler's inlining engine.
"""
function print_statement_costs(io::IO, @nospecialize(f), @nospecialize(t); kwargs...)
    tt = signature_type(f, t)
    print_statement_costs(io, tt; kwargs...)
end

function print_statement_costs(io::IO, @nospecialize(tt::Type);
                               world::UInt=get_world_counter(),
                               interp=nothing)
    passed_interp = interp
    interp = passed_interp === nothing ? invoke_default_compiler(:_default_interp, world) : interp
    tt = to_tuple_type(tt)
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    matches = invoke_interp_compiler(passed_interp, :_findall_matches, interp, tt)
    matches === nothing && raise_match_failure(:print_statement_costs, tt)
    cst = Int[]
    for match in matches.matches
        match = match::Core.MethodMatch
        println(io, match.method)
        code = invoke_interp_compiler(passed_interp, :typeinf_code, interp, match, true)
        if code === nothing
            println(io, "  inference not successful")
        else
            empty!(cst)
            resize!(cst, length(code.code))
            maxcost = invoke_interp_compiler(passed_interp, :statement_costs!, interp, cst, code.code, code, match)
            nd = ndigits(maxcost)
            irshow_config = IRShow.IRShowConfig() do io, linestart, idx
                print(io, idx > 0 ? lpad(cst[idx], nd+1) : " "^(nd+1), " ")
                return ""
            end
            IRShow.show_ir(io, code, irshow_config)
        end
        println(io)
    end
end

print_statement_costs(args...; kwargs...) = print_statement_costs(stdout, args...; kwargs...)

function _which(@nospecialize(tt::Type);
    method_table #=::Union{Nothing,Core.MethodTable,Compiler.MethodTableView}=# =nothing,
    world::UInt=get_world_counter(),
    raise::Bool=true)
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    match, = invoke_default_compiler(:findsup_mt, tt, world, method_table)
    if match === nothing
        raise && error("no unique matching method found for the specified argument types")
        return nothing
    end
    return match
end

"""
    which(f, types)

Returns the method of `f` (a `Method` object) that would be called for arguments of the given `types`.

If `types` is an abstract type, then the method that would be called by `invoke` is returned.

See also: [`parentmodule`](@ref), [`@which`](@ref Main.InteractiveUtils.@which), and [`@edit`](@ref Main.InteractiveUtils.@edit).
"""
function which(@nospecialize(f), @nospecialize(t))
    tt = signature_type(f, t)
    world = get_world_counter()
    match, _ = invoke_default_compiler(:_findsup, tt, nothing, world)
    if match === nothing
        me = MethodError(f, t, world)
        ee = ErrorException(sprint(io -> begin
            println(io, "Calling invoke(f, t, args...) would throw:");
            Base.showerror(io, me);
        end))
        throw(ee)
    end
    return match.method
end

"""
    which(types::Type{<:Tuple})

Returns the method that would be called by the given type signature (as a tuple type).
"""
function which(@nospecialize(tt#=::Type=#))
    return _which(tt).method
end

"""
    which(module, symbol)

Return the module in which the binding for the variable referenced by `symbol` in `module` was created.
"""
function which(m::Module, s::Symbol)
    if !isdefined(m, s)
        error("\"$s\" is not defined in module $m")
    end
    return binding_module(m, s)
end

# function reflection

"""
    nameof(f::Function)::Symbol

Get the name of a generic `Function` as a symbol. For anonymous functions,
this is a compiler-generated name. For explicitly-declared subtypes of
`Function`, it is the name of the function's type.
"""
function nameof(f::Function)
    return typeof(f).name.singletonname
end

function nameof(f::Core.IntrinsicFunction)
    name = ccall(:jl_intrinsic_name, Ptr{UInt8}, (Core.IntrinsicFunction,), f)
    return ccall(:jl_symbol, Ref{Symbol}, (Ptr{UInt8},), name)
end

"""
    parentmodule(f::Function)::Module

Determine the module containing the (first) definition of a generic
function.
"""
parentmodule(f::Function) = parentmodule(typeof(f))

"""
    parentmodule(f::Function, types)::Module

Determine the module containing the first method of a generic function `f` matching
the specified `types`.
"""
function parentmodule(@nospecialize(f), @nospecialize(types))
    m = methods(f, types)
    if isempty(m)
        error("no matching methods")
    end
    return parentmodule(first(m))
end

"""
    parentmodule(m::Method)::Module

Return the module in which the given method `m` is defined.

!!! compat "Julia 1.9"
    Passing a `Method` as an argument requires Julia 1.9 or later.
"""
parentmodule(m::Method) = m.module

"""
    hasmethod(f, t::Type{<:Tuple}[, kwnames]; world=get_world_counter())::Bool

Determine whether the given generic function has a method matching the given
`Tuple` of argument types with the upper bound of world age given by `world`.

If a tuple of keyword argument names `kwnames` is provided, this also checks
whether the method of `f` matching `t` has the given keyword argument names.
If the matching method accepts a variable number of keyword arguments, e.g.
with `kwargs...`, any names given in `kwnames` are considered valid. Otherwise
the provided names must be a subset of the method's keyword arguments.

See also [`applicable`](@ref).

!!! compat "Julia 1.2"
    Providing keyword argument names requires Julia 1.2 or later.

# Examples
```jldoctest
julia> hasmethod(length, Tuple{Array})
true

julia> f(; oranges=0) = oranges;

julia> hasmethod(f, Tuple{}, (:oranges,))
true

julia> hasmethod(f, Tuple{}, (:apples, :bananas))
false

julia> g(; xs...) = 4;

julia> hasmethod(g, Tuple{}, (:a, :b, :c, :d))  # g accepts arbitrary kwargs
true
```
"""
function hasmethod(@nospecialize(f), @nospecialize(t))
    return Core._hasmethod(signature_type(f, t))
end

function Core.kwcall(kwargs::NamedTuple, ::typeof(hasmethod), @nospecialize(f), @nospecialize(t))
    world = kwargs.world::UInt # make sure this is the only local, to avoid confusing kwarg_decl()
    return ccall(:jl_gf_invoke_lookup, Any, (Any, Any, UInt), signature_type(f, t), nothing, world) !== nothing
end

function hasmethod(f, t, kwnames::Tuple{Vararg{Symbol}}; world::UInt=get_world_counter())
    @nospecialize
    world == typemax(UInt) && error("code reflection cannot be used from generated functions")
    isempty(kwnames) && return hasmethod(f, t; world)
    t = to_tuple_type(t)
    ft = Core.Typeof(f)
    u = unwrap_unionall(t)::DataType
    tt = rewrap_unionall(Tuple{typeof(Core.kwcall), NamedTuple, ft, u.parameters...}, t)
    match = ccall(:jl_gf_invoke_lookup, Any, (Any, Any, UInt), tt, nothing, world)
    match === nothing && return false
    kws = ccall(:jl_uncompress_argnames, Array{Symbol,1}, (Any,), (match::Method).slot_syms)
    isempty(kws) && return true # some kwfuncs simply forward everything directly
    for kw in kws
        endswith(String(kw), "...") && return true
    end
    kwnames = collect(kwnames)
    return issubset(kwnames, kws)
end

"""
    fbody = bodyfunction(basemethod::Method)

Find the keyword "body function" (the function that contains the body of the method
as written, called after all missing keyword-arguments have been assigned default values).
`basemethod` is the method you obtain via [`which`](@ref) or [`methods`](@ref).
"""
function bodyfunction(basemethod::Method)
    fmod = parentmodule(basemethod)
    # The lowered code for `basemethod` should look like
    #   %1 = mkw(kwvalues..., #self#, args...)
    #        return %1
    # where `mkw` is the name of the "active" keyword body-function.
    ast = uncompressed_ast(basemethod)
    if isa(ast, Core.CodeInfo) && length(ast.code) >= 2
        callexpr = ast.code[end-1]
        if isa(callexpr, Expr) && callexpr.head === :call
            fsym = callexpr.args[1]
            while true
                if isa(fsym, Symbol)
                    return getfield(fmod, fsym)
                elseif isa(fsym, GlobalRef)
                    if fsym.mod === Core && fsym.name === :_apply
                        fsym = callexpr.args[2]
                    elseif fsym.mod === Core && fsym.name === :_apply_iterate
                        fsym = callexpr.args[3]
                    end
                    if isa(fsym, Symbol)
                        return getfield(fmod, fsym)::Function
                    elseif isa(fsym, GlobalRef)
                        return getfield(fsym.mod, fsym.name)::Function
                    elseif isa(fsym, Core.SSAValue)
                        fsym = ast.code[fsym.id]
                    else
                        return nothing
                    end
                elseif isa(fsym, Core.SSAValue)
                    fsym = ast.code[fsym.id]
                else
                    return nothing
                end
            end
        end
    end
    return nothing
end

"""
    Base.isambiguous(m1, m2; ambiguous_bottom=false)::Bool

Determine whether two methods `m1` and `m2` may be ambiguous for some call
signature. This test is performed in the context of other methods of the same
function; in isolation, `m1` and `m2` might be ambiguous, but if a third method
resolving the ambiguity has been defined, this returns `false`.
Alternatively, in isolation `m1` and `m2` might be ordered, but if a third
method cannot be sorted with them, they may cause an ambiguity together.

For parametric types, the `ambiguous_bottom` keyword argument controls whether
`Union{}` counts as an ambiguous intersection of type parameters – when `true`,
it is considered ambiguous, when `false` it is not.

# Examples
```jldoctest
julia> foo(x::Complex{<:Integer}) = 1
foo (generic function with 1 method)

julia> foo(x::Complex{<:Rational}) = 2
foo (generic function with 2 methods)

julia> m1, m2 = collect(methods(foo));

julia> typeintersect(m1.sig, m2.sig)
Tuple{typeof(foo), Complex{Union{}}}

julia> Base.isambiguous(m1, m2, ambiguous_bottom=true)
true

julia> Base.isambiguous(m1, m2, ambiguous_bottom=false)
false
```
"""
function isambiguous(m1::Method, m2::Method; ambiguous_bottom::Bool=false)
    m1 === m2 && return false
    ti = typeintersect(m1.sig, m2.sig)
    ti === Bottom && return false
    function inner(ti)
        ti === Bottom && return false
        if !ambiguous_bottom
            has_bottom_parameter(ti) && return false
        end
        world = get_world_counter()
        world == typemax(UInt) && return true # intersecting methods are always ambiguous in the generator world, which is true, albeit maybe confusing for some
        min = Ref{UInt}(typemin(UInt))
        max = Ref{UInt}(typemax(UInt))
        has_ambig = Ref{Int32}(0)
        ms = collect(Core.MethodMatch, _methods_by_ftype(ti, nothing, -1, world, true, min, max, has_ambig)::Vector)
        has_ambig[] == 0 && return false
        if !ambiguous_bottom
            filter!(ms) do m::Core.MethodMatch
                return !has_bottom_parameter(m.spec_types)
            end
        end
        # if ml-matches reported the existence of an ambiguity over their
        # intersection, see if both m1 and m2 seem to be involved in it
        # (if one was fully dominated by a different method, we want to will
        # report the other ambiguous pair)
        have_m1 = have_m2 = false
        for match in ms
            m = match.method
            m === m1 && (have_m1 = true)
            m === m2 && (have_m2 = true)
        end
        if !have_m1 || !have_m2
            # ml-matches did not need both methods to expose the reported ambiguity
            return false
        end
        if !ambiguous_bottom
            # since we're intentionally ignoring certain ambiguities (via the
            # filter call above), see if we can now declare the intersection fully
            # covered even though it is partially ambiguous over Union{} as a type
            # parameter somewhere
            minmax = nothing
            for match in ms
                m = match.method
                match.fully_covers || continue
                if minmax === nothing || morespecific(m, minmax)
                    minmax = m
                end
            end
            if minmax === nothing || minmax == m1 || minmax == m2
                return true
            end
            for match in ms
                m = match.method
                m === minmax && continue
                if !morespecific(minmax, m)
                    if match.fully_covers || !morespecific(m, minmax)
                        return true
                    end
                end
            end
            return false
        end
        return true
    end
    if !(ti <: m1.sig && ti <: m2.sig)
        # When type-intersection fails, it's often also not commutative. Thus
        # checking the reverse may allow detecting ambiguity solutions
        # correctly in more cases (and faster).
        ti2 = typeintersect(m2.sig, m1.sig)
        if ti2 <: m1.sig && ti2 <: m2.sig
            ti = ti2
        elseif ti != ti2
            # TODO: this would be the more correct way to handle this case, but
            #       people complained so we don't do it
            #inner(ti2) || return false # report that the type system failed to decide if it was ambiguous by saying they definitely are
            return false # report that the type system failed to decide if it was ambiguous by saying they definitely are not
        else
            return false # report that the type system failed to decide if it was ambiguous by saying they definitely are not
        end
    end
    inner(ti) || return false
    # otherwise type-intersection reported an ambiguity we couldn't solve
    return true
end

"""
    @invoke f(arg::T, ...; kwargs...)

Provides a convenient way to call [`invoke`](@ref) by expanding
`@invoke f(arg1::T1, arg2::T2; kwargs...)` to `invoke(f, Tuple{T1,T2}, arg1, arg2; kwargs...)`.
When an argument's type annotation is omitted, it's replaced with `Core.Typeof` that argument.
To invoke a method where an argument is untyped or explicitly typed as `Any`, annotate the
argument with `::Any`.

It also supports the following syntax:
- `@invoke (x::X).f` expands to `invoke(getproperty, Tuple{X,Symbol}, x, :f)`
- `@invoke (x::X).f = v::V` expands to `invoke(setproperty!, Tuple{X,Symbol,V}, x, :f, v)`
- `@invoke (xs::Xs)[i::I]` expands to `invoke(getindex, Tuple{Xs,I}, xs, i)`
- `@invoke (xs::Xs)[i::I] = v::V` expands to `invoke(setindex!, Tuple{Xs,V,I}, xs, v, i)`

# Examples

```jldoctest
julia> @macroexpand @invoke f(x::T, y)
:(Core.invoke(f, Tuple{T, Core.Typeof(y)}, x, y))

julia> @invoke 420::Integer % Unsigned
0x00000000000001a4

julia> @macroexpand @invoke (x::X).f
:(Core.invoke(Base.getproperty, Tuple{X, Core.Typeof(:f)}, x, :f))

julia> @macroexpand @invoke (x::X).f = v::V
:(Core.invoke(Base.setproperty!, Tuple{X, Core.Typeof(:f), V}, x, :f, v))

julia> @macroexpand @invoke (xs::Xs)[i::I]
:(Core.invoke(Base.getindex, Tuple{Xs, I}, xs, i))

julia> @macroexpand @invoke (xs::Xs)[i::I] = v::V
:(Core.invoke(Base.setindex!, Tuple{Xs, V, I}, xs, v, i))
```

!!! compat "Julia 1.7"
    This macro requires Julia 1.7 or later.

!!! compat "Julia 1.9"
    This macro is exported as of Julia 1.9.

!!! compat "Julia 1.10"
    The additional syntax is supported as of Julia 1.10.
"""
macro invoke(ex)
    topmod = _topmod(__module__)
    f, args, kwargs = destructure_callex(topmod, ex)
    types = Expr(:curly, :Tuple)
    out = Expr(:call, GlobalRef(Core, :invoke))
    isempty(kwargs) || push!(out.args, Expr(:parameters, kwargs...))
    push!(out.args, f)
    push!(out.args, types)
    for arg in args
        if isexpr(arg, :(::))
            push!(out.args, arg.args[1])
            push!(types.args, arg.args[2])
        else
            push!(out.args, arg)
            push!(types.args, Expr(:call, GlobalRef(Core, :Typeof), arg))
        end
    end
    return esc(out)
end

apply_gr(gr::GlobalRef, @nospecialize args...) = getglobal(gr.mod, gr.name)(args...)
apply_gr_kw(@nospecialize(kwargs::NamedTuple), gr::GlobalRef, @nospecialize args...) = Core.kwcall(kwargs, getglobal(gr.mod, gr.name), args...)

function invokelatest_gr(gr::GlobalRef, @nospecialize args...; kwargs...)
    @inline
    kwargs = merge(NamedTuple(), kwargs)
    if isempty(kwargs)
        return invokelatest(apply_gr, gr, args...)
    end
    return invokelatest(apply_gr_kw, kwargs, gr, args...)
end

"""
    @invokelatest f(args...; kwargs...)

Provides a convenient way to call [`invokelatest`](@ref).
`@invokelatest f(args...; kwargs...)` will simply be expanded into
`Base.invokelatest(f, args...; kwargs...)`.

It also supports the following syntax:
- `@invokelatest x.f` expands to `Base.invokelatest(getproperty, x, :f)`
- `@invokelatest x.f = v` expands to `Base.invokelatest(setproperty!, x, :f, v)`
- `@invokelatest xs[i]` expands to `Base.invokelatest(getindex, xs, i)`
- `@invokelatest xs[i] = v` expands to `Base.invokelatest(setindex!, xs, v, i)`

!!! note
    If `f` is a global, it will be resolved consistently
    in the (latest) world as the call target. However, all other arguments
    (as well as `f` itself if it is not a literal global) will be evaluated
    in the current world age.

!!! compat "Julia 1.7"
    This macro requires Julia 1.7 or later.

!!! compat "Julia 1.9"
    Prior to Julia 1.9, this macro was not exported, and was called as `Base.@invokelatest`.

!!! compat "Julia 1.10"
    The additional `x.f` and `xs[i]` syntax requires Julia 1.10.
"""
macro invokelatest(ex)
    topmod = _topmod(__module__)
    f, args, kwargs = destructure_callex(topmod, ex)

    if !isa(f, GlobalRef)
        out_f = Expr(:call, GlobalRef(Base, :invokelatest))
        isempty(kwargs) || push!(out_f.args, Expr(:parameters, kwargs...))

        if isexpr(f, :(.))
            s = gensym()
            check = quote
                $s = $(f.args[1])
                isa($s, Module)
            end
            push!(out_f.args, Expr(:(.), s, f.args[2]))
        else
            push!(out_f.args, f)
        end
        append!(out_f.args, args)

        if @isdefined(s)
            f = :(GlobalRef($s, $(f.args[2])))
        elseif !isa(f, Symbol)
            return esc(out_f)
        else
            check = :($(Expr(:isglobal, f)))
        end
    end

    out_gr = Expr(:call, GlobalRef(Base, :invokelatest_gr))
    isempty(kwargs) || push!(out_gr.args, Expr(:parameters, kwargs...))
    push!(out_gr.args, isa(f, GlobalRef) ? QuoteNode(f) :
                       isa(f, Symbol) ? QuoteNode(GlobalRef(__module__, f)) :
                       f)
    append!(out_gr.args, args)

    if isa(f, GlobalRef)
        return esc(out_gr)
    end

    # f::Symbol
    return esc(:($check ? $out_gr : $out_f))
end

function destructure_callex(topmod::Module, @nospecialize(ex))
    function flatten(xs)
        out = Any[]
        for x in xs
            if isexpr(x, :tuple)
                append!(out, x.args)
            else
                push!(out, x)
            end
        end
        return out
    end

    kwargs = Any[]
    if isexpr(ex, :call) # `f(args...)`
        f = first(ex.args)
        args = Any[]
        for x in ex.args[2:end]
            if isexpr(x, :parameters)
                append!(kwargs, x.args)
            elseif isexpr(x, :kw)
                push!(kwargs, x)
            else
                push!(args, x)
            end
        end
    elseif isexpr(ex, :.)   # `x.f`
        f = GlobalRef(topmod, :getproperty)
        args = flatten(ex.args)
    elseif isexpr(ex, :ref) # `x[i]`
        f = GlobalRef(topmod, :getindex)
        args = flatten(ex.args)
    elseif isexpr(ex, :(=)) # `x.f = v` or `x[i] = v`
        lhs, rhs = ex.args
        if isexpr(lhs, :.)
            f = GlobalRef(topmod, :setproperty!)
            args = flatten(Any[lhs.args..., rhs])
        elseif isexpr(lhs, :ref)
            f = GlobalRef(topmod, :setindex!)
            args = flatten(Any[lhs.args[1], rhs, lhs.args[2]])
        else
            throw(ArgumentError("expected a `setproperty!` expression `x.f = v` or `setindex!` expression `x[i] = v`"))
        end
    else
        throw(ArgumentError("expected a `:call` expression `f(args...; kwargs...)`"))
    end
    return f, args, kwargs
end
