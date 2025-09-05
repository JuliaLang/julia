# This file is a part of Julia. License is MIT: https://julialang.org/license

# macro wrappers for various reflection functions

using Base: insert!, replace_ref_begin_end!,
    infer_return_type, infer_exception_type, infer_effects, code_ircode, isexpr

# defined in Base so it's possible to time all imports, including InteractiveUtils and its deps
# via. `Base.@time_imports` etc.
import Base: @time_imports, @trace_compile, @trace_dispatch

typesof_expr(args::Vector{Any}, where_params::Union{Nothing, Vector{Any}} = nothing) = rewrap_where(:(Tuple{$(Any[esc(reescape(get_typeof, a)) for a in args]...)}), where_params)
typesof_expr_unescaped(args::Vector{Any}, where_params::Union{Nothing, Vector{Any}} = nothing) = rewrap_where(:(Tuple{$(Any[reescape(get_typeof, a) for a in args]...)}), where_params)

function extract_where_parameters(ex::Expr)
    isexpr(ex, :where) || return ex, nothing
    ex.args[1], ex.args[2:end]
end

function rewrap_where(ex::Expr, where_params::Union{Nothing, Vector{Any}})
    isnothing(where_params) && return ex
    Expr(:where, ex, esc.(where_params)...)
end

function reescape(f::Function, @nospecialize ex)
    isa(ex, Expr) || return f(ex)
    unescaped = Meta.unescape(ex)
    new = f(unescaped)
    return Meta.reescape(new, ex)
end

get_typeof(ex::Ref) = ex[]
function get_typeof(@nospecialize ex)
    isexpr(ex, :(::), 1) && return ex.args[1]
    isexpr(ex, :(::), 2) && return ex.args[2]
    if isexpr(ex, :..., 1)
        splatted = ex.args[1]
        isexpr(splatted, :(::), 1) && return Expr(:curly, :(Core.Vararg), splatted.args[1])
        return :(Any[Core.Typeof(x) for x in $splatted]...)
    end
    return :(Core.Typeof($ex))
end

function is_broadcasting_call(ex)
    isa(ex, Expr) || return false
    # Standard broadcasting: f.(x)
    isexpr(ex, :.) && length(ex.args) ≥ 2 && isexpr(ex.args[2], :tuple) && return true
    # Infix broadcasting: x .+ y, x .<< y, etc.
    if isexpr(ex, :call)
        f = ex.args[1]
        f == :.. && return false
        string(f)[1] == '.' && return true
    end
    return false
end
is_broadcasting_expr(ex) = is_broadcasting_call(ex) || is_broadcasting_assignment(ex)
function is_broadcasting_assignment(ex)
    isa(ex, Expr) || return false
    isexpr(ex, :.) && return false
    head = string(ex.head)
    # x .= y, x .+= y, x .<<= y, etc.
    head[begin] == '.' && head[end] == '=' && return true
    return false
end

"""
Transform a dot expression into one where each argument has been replaced by a
variable "xj" (with j an integer from 1 to the returned i).
The list `args` contains the original arguments that have been replaced.
"""
function recursive_dotcalls!(ex, args, i=1)
    if is_broadcasting_expr(ex)
        if is_broadcasting_assignment(ex)
            (start, branches) = (1, ex.args)
        elseif isexpr(ex, :.)
            (start, branches) = (1, ex.args[2].args)
        else
            (start, branches) = (2, ex.args)
        end
        for j in start:length(branches)::Int
            branch, i = recursive_dotcalls!(branches[j], args, i)
            branches[j] = branch
        end
        return ex, i
    elseif isexpr(ex, :parameters)
        for j in eachindex(ex.args)
            param, i = recursive_dotcalls!(ex.args[j], args, i)
            ex.args[j] = param
        end
        return ex, i
    end
    newarg = Symbol('x', i)
    if isexpr(ex, :...)
        newarg = Expr(:..., newarg)
        push!(args, only(ex.args))
    elseif isexpr(ex, :kw)
        newarg = Expr(:kw, ex.args[1], newarg)
        push!(args, ex.args[end])
    else
        push!(args, ex)
    end
    return newarg, i+1
end

function extract_farg(@nospecialize arg)
    !isexpr(arg, :(::), 1) && return arg
    fT = arg.args[1]
    :($construct_callable($fT))
end

function construct_callable(@nospecialize(func::Type))
    # Support function singleton types such as `(::typeof(f))(args...)`
    Base.issingletontype(func) && isdefined(func, :instance) && return func.instance
    # Don't support type annotations otherwise, we don't want to give wrong answers
    # for callables such as `(::Returns{Int})(args...)` where using `Returns{Int}`
    # would give us code for the constructor, not for the callable object.
    throw(ArgumentError("If the function type is explicitly provided via a type annotation, it must be a singleton whose only instance is the callable object.
                         To remove this restriction, the reflection macro must set `use_signature_tuple = true` if the reflection function supports a single signature tuple type argument, such as `Tuple{typeof(f), argtypes...}`"))
end

function separate_kwargs(exs::Vector{Any})
    args = []
    kwargs = []
    for ex in exs
        if isexpr(ex, :kw)
            push!(kwargs, ex)
        elseif isexpr(ex, :parameters)
            for kw in ex.args
                push!(kwargs, kw)
            end
        else
            push!(args, ex)
        end
    end
    args, kwargs
end

function are_kwargs_valid(kwargs::Vector{Any})
    for kwarg in kwargs
        isexpr(kwarg, :..., 1) && continue
        isexpr(kwarg, :kw, 2) && isa(kwarg.args[1], Symbol) && continue
        isexpr(kwarg, :(::), 2) && continue
        isa(kwarg, Symbol) && continue
        isexpr(kwarg, :escape) && continue
        isexpr(kwarg, :var"hygienic-scope") && continue
        return false
    end
    return true
end

# Generate an expression that merges `kwargs` onto a single `NamedTuple`
function generate_merged_namedtuple_type(kwargs::Vector{Any})
    nts = Any[]
    ntargs = Pair{Symbol, Any}[]
    for ex in kwargs
        if isexpr(ex, :..., 1)
            if !isempty(ntargs)
                # Construct a `NamedTuple` containing the previous parameters.
                push!(nts, generate_namedtuple_type(ntargs))
                empty!(ntargs)
            end
            push!(nts, Expr(:call, typeof_nt, ex.args[1]))
        elseif isexpr(ex, :kw, 2)
            push!(ntargs, ex.args[1]::Symbol => reescape(get_typeof, ex.args[2]))
        elseif isexpr(ex, :(::), 2)
            push!(ntargs, ex.args[1]::Symbol => reescape(get_typeof, ex))
        else
            push!(ntargs, ex => reescape(get_typeof, ex))
        end
    end
    !isempty(ntargs) && push!(nts, generate_namedtuple_type(ntargs))
    return :($merge_namedtuple_types($(nts...)))
end

function generate_namedtuple_type(ntargs::Vector{Pair{Symbol, Any}})
    names = Expr(:tuple)
    tt = Expr(:curly, :Tuple)
    for (name, type) in ntargs
        push!(names.args, QuoteNode(name))
        push!(tt.args, type)
    end
    return :(NamedTuple{$names, $tt})
end

typeof_nt(nt::NamedTuple) = typeof(nt)
typeof_nt(nt::Base.Pairs) = typeof(values(nt))

function merge_namedtuple_types(nt::Type{<:NamedTuple}, nts::Type{<:NamedTuple}...)
    @nospecialize
    isempty(nts) && return nt
    names = Symbol[]
    types = Any[]
    for nt in (nt, nts...)
        for (name, type) in zip(fieldnames(nt), fieldtypes(nt))
            i = findfirst(==(name), names)
            if isnothing(i)
                push!(names, name)
                push!(types, type)
            else
                types[i] = type
            end
        end
    end
    return NamedTuple{Tuple(names), Tuple{types...}}
end

function gen_call(fcn, args, where_params, kws; use_signature_tuple::Bool, not_an_opaque_closure::Bool = true)
    f, args... = args
    args = collect(Any, args)
    if !use_signature_tuple
        f = esc(reescape(extract_farg, f))
        tt = typesof_expr(args, where_params)
        return :($fcn($f, $tt; $(kws...)))
    end
    # We use a signature tuple only if we are sure we won't get an opaque closure as first argument.
    # If we do get one, we have to use the 2-argument form.
    if isexpr(f, :(::)) || not_an_opaque_closure
        # We have a type, not a value, so not an opaque closure.
        sigt = typesof_expr(Any[f, args...], where_params)
        return :($fcn($sigt; $(kws...)))
    end
    tt = typesof_expr(args, where_params)
    sigt = typesof_expr_unescaped(Any[:f, esc.(args)...], where_params)
    return quote
        f = $(esc(f))
        if isa(f, Core.OpaqueClosure)
            $fcn(f, $tt; $(kws...))
        else
            $fcn($sigt; $(kws...))
        end
    end
end

function expand_ref_begin_end!(f::Function, ex, __module__::Module)
    arr = ex.args[1]
    args = copy(ex.args)
    new = replace_ref_begin_end!(__module__, ex)
    modified = ex.args .≠ args
    if any(modified) && (isexpr(arr, :(::), 1) || isexpr(arr, :(::), 2) || isexpr(arr, :..., 1))
        return Expr(:call, :error, "`begin` or `end` cannot be used with a type-annotated left-hand side argument for an indexing syntax")
    end
    call = f(ex)
    !any(modified) && return call
    fixup_hygiene_for_ref_temporary!(new)
    # We have to mutate `ex`, then return `new` which evaluates `arr` before use.
    ex.head = call.head
    ex.args = call.args
    return new
end

function fixup_hygiene_for_ref_temporary!(ex)
    # Match the local variable `##S#...` so we may escape its definition.
    # We don't want to use `escs = 1` in `replace_ref_begin_end_!` because
    # then we delegate escaping to this function, whereas we otherwise manage
    # ourselves the escaping in all other code paths.
    isexpr(ex, :block) || return
    decl = ex.args[1]
    isexpr(decl, :local, 1) || return
    assignment = decl.args[1]
    isexpr(assignment, :(=), 2) || return
    variable = assignment.args[1]
    startswith(string(variable), "##S#") || return
    decl.args[1] = esc(assignment)
end

is_code_macro(fcn) = startswith(string(fcn), "code_")

"""
    gen_call_with_extracted_types(__module__, fcn, ex, kws = Expr[]; is_source_reflection = !is_code_macro(fcn), supports_binding_reflection = false, use_signature_tuple = false)

Destructures the input expression `ex` into a function call or a binding access, then generates a call to either:
- `fcn(f, tt; kws...)`
- `fcn(sigt; kws...)` # if `use_signature_tuple = true`
- `fcn(mod, name; kws...)` # if `supports_binding_reflection = true`

## `fcn` API requirements

`fcn` is a user function expected to satisfy the following API:
- `fcn(f, tt)`: `f` is a value (such as `sum`, unlike `typeof(sum)`), and `tt := Tuple{argtypes...}`
  is a `Tuple` holding argument types. `f` may be a `Core.OpaqueClosure`.

If `use_signature_tuple = true`:
- `fcn(sigt)`: `sigt := Tuple{typeof(f), argtypes...}` represents the low-level signature tuple to be used for introspection.

If `supports_binding_reflection = true`:
- `fcn(mod::Module, name::Symbol)`: `name` is the name of a binding that may or may not exist in `mod`.

!!! warning
    This function is not public and may be subject to breaking changes. However, we recognize that it may
    be very convenient for macro developers, and as it is already used by a certain number of packages,
    we will do our best to avoid breakages.

## Examples

Here are a few usage patterns that may help you get started.

For most "code" macros (`@code_typed`, `@code_llvm`, `@code_native` etc):
```julia
    gen_call_with_extracted_types(__module__, fcn, ex, kws; is_source_reflection = false, use_signature_tuple = true #= may be false =#)
```

For source reflection macros (`@which`, `@edit`, `@less` etc):
```julia
    gen_call_with_extracted_types(__module__, fcn, ex, kws; is_source_reflection = true, use_signature_tuple = true #= may be false =#)
```

# Extended help

## Type annotations

Type annotations may be used instead of concrete values for the callable or for any of the arguments. The generated code
will directly use the right-hand side of the type annotation instead of extracting the type of a value at runtime.

This is particularly useful for callable objects (notably, for those that are hard to construct by hand on the spot),
or when wanting to provide a type that is not concrete. However, support for callable objects requires setting
`use_signature_tuple` to true, which is not a default (see the corresponding section below).

Constraints on type parameters are also supported with a `where` syntax, enabling these patterns:
- `f(x::Vector{T}, y::T) where {T}`
- `(::Returns{T})() where {T<:Real}`
- `(::MyPolynomial{N,T})(::T, ::AbstractArray{T,N}) where {N,T}`

Type-annotated expressions may be mixed with runtime values, as in `x + ::Float64`.

## Broadcasting

When `ex` is a broadcasting expression (a broadcasted assignment `a .+= b` or a broadcasted function call `a .+ b`),
there is no actual function that corresponds to this expression because lowering maps it to more than one call.

If `is_source_reflection` is true, we assume that `fcn` uses provenance information (e.g. used by `@edit` to go
to a source location, or `@which` to get the method matching the input). In this case, we don't have a clear
semantic source to give (shall it be `broadcasted`, or `materialize`, or something else?), so we return a throwing
expression.

However, if provenance is not of interest, we define an intermediate function on the spot that performs the broadcast,
then carry on using this function. For example, for the input expression `a .+ b`, we emit the anonymous function
`(a, b) -> a .+ b` then call `fcn` just as if the user had issued a call to this anonymous function. That should be the
desired behavior for most macros that want to map an expression to the corresponding generated code, as in `@code_typed`
or `@code_llvm` for instance.

## Binding reflection

Expressions of the form `a.b` (or `a.b.c` and so on) are by default interpreted as calls to `getproperty`.
However, if the value corresponding to the left-hand side (`a`, `a.b`, etc) is a module, some implementations
may instead be interested in the binding lookup, instead of the function call. If that is the case,
`supports_binding_reflection` may be set to `true` which will emit a call to `fcn(a, :b)` (or `fcn(a.b, :c)` etc).

## Tuple signature type

If `use_signature_tuple = true`, then a single tuple consisting of `Tuple{ft, argtypes...}` will be formed
and provided to `fcn`. `fcn` is then expected to use `ft` as the callable type with no further transformation.

This behavior is required to enable support type-annotated callable objects.

To understand this requirement, we'll use `code_typed` as an example. `code_typed(f, ())` interprets its input as the signature
`Tuple{typeof(f)}`, and `code_typed(Returns{Int}, ())` interprets that as the signature `Tuple{Type{Returns{Int}}}`, corresponding
to the type constructor.
To remove the ambiguity, `code_typed` must support an implementation that directly accepts a function type. This implementation
is assumed to be the method for `fcn(sigt::Type{<:Tuple})`.
"""
function gen_call_with_extracted_types(__module__, fcn, ex0, kws = Expr[]; is_source_reflection = !is_code_macro(fcn), supports_binding_reflection = false, use_signature_tuple = false)
    # Ignore assignments (e.g. `@edit a = f(x)` gets turned into `@edit f(x)`)
    if isa(ex0, Expr) && ex0.head === :(=) && isa(ex0.args[1], Symbol)
        return gen_call_with_extracted_types(__module__, fcn, ex0.args[2], kws; is_source_reflection, supports_binding_reflection, use_signature_tuple)
    end
    where_params = nothing
    if isa(ex0, Expr)
        ex0, where_params = extract_where_parameters(ex0)
    end
    if isa(ex0, Expr)
        if ex0.head === :do && isexpr(get(ex0.args, 1, nothing), :call)
            # Normalize `f(args...) do ... end` calls to `f(do_anonymous_function, args...)`
            if length(ex0.args) != 2
                return Expr(:call, :error, "ill-formed do call")
            end
            i = findlast(@nospecialize(a)->(isexpr(a, :kw) || isexpr(a, :parameters)), ex0.args[1].args)
            args = copy(ex0.args[1].args)
            insert!(args, (isnothing(i) ? 2 : 1+i::Int), ex0.args[2])
            ex0 = Expr(:call, args...)
        end
        if is_broadcasting_expr(ex0) && !is_source_reflection
            # Manually wrap top-level broadcasts in a function.
            # We don't do that if `fcn` reflects into the source,
            # because that destroys provenance information.
            args = Any[]
            ex, i = recursive_dotcalls!(copy(ex0), args)
            xargs = [Symbol('x', j) for j in 1:i-1]
            dotfuncname = gensym("dotfunction")
            call = gen_call(fcn, Any[dotfuncname, args...], where_params, kws; use_signature_tuple)
            return quote
                let $(esc(:($dotfuncname($(xargs...)) = $ex)))
                    $call
                end
            end
        elseif isexpr(ex0, :.) && is_source_reflection
            # If `ex0` has the form A.B (or some chain A.B.C.D) and `fcn` reflects into the source,
            # `A` (or `A.B.C`) may be a module, in which case `fcn` is probably more interested in
            # the binding rather than the `getproperty` call.
            # If binding reflection is not supported, we generate an error; `getproperty(::Module, field)`
            # is not going to be interesting to reflect into, so best to allow future non-breaking support
            # for binding reflection in case the macro may eventually support that.
            fully_qualified_symbol = true
            ex1 = ex0
            while ex1 isa Expr && ex1.head === :.
                fully_qualified_symbol = (length(ex1.args) == 2 &&
                                            ex1.args[2] isa QuoteNode &&
                                            ex1.args[2].value isa Symbol)
                fully_qualified_symbol || break
                ex1 = ex1.args[1]
            end
            fully_qualified_symbol &= ex1 isa Symbol
            if fully_qualified_symbol || isexpr(ex1, :(::), 1)
                call_reflection = gen_call(fcn, [getproperty; ex0.args], where_params, kws; use_signature_tuple)
                isexpr(ex0.args[1], :(::), 1) && return call_reflection
                if supports_binding_reflection
                    binding_reflection = :($fcn(arg1, $(ex0.args[2]); $(kws...)))
                else
                    binding_reflection = :(error("expression is not a function call"))
                end
                return quote
                    local arg1 = $(esc(ex0.args[1]))
                    if isa(arg1, Module)
                        $binding_reflection
                    else
                        $call_reflection
                    end
                end
            end
        end
        if is_broadcasting_expr(ex0)
            return Expr(:call, :error, "dot expressions are not lowered to "
                * "a single function call, so @$fcn cannot analyze "
                * "them. You may want to use Meta.@lower to identify "
                * "which function call to target.")
        end
        if any(@nospecialize(a)->(isexpr(a, :kw) || isexpr(a, :parameters)), ex0.args)
            args, kwargs = separate_kwargs(ex0.args)
            are_kwargs_valid(kwargs) || return quote
                error("keyword argument format unrecognized; they must be of the form `x` or `x = <value>`")
                $(esc(ex0)) # trigger syntax errors if any
            end
            nt = generate_merged_namedtuple_type(kwargs)
            nt = Ref(nt) # ignore `get_typeof` handling
            return gen_call(fcn, Any[Core.kwcall, nt, args...], where_params, kws; use_signature_tuple)
        elseif ex0.head === :call
            args = copy(ex0.args)
            if ex0.args[1] === :^ && length(ex0.args) >= 3 && isa(ex0.args[3], Int)
                pushfirst!(args, Base.literal_pow)
                args[4] = :(Val($(ex0.args[3])))
            end
            return gen_call(fcn, args, where_params, kws; use_signature_tuple, not_an_opaque_closure = false)
        elseif ex0.head === :(=) && length(ex0.args) == 2
            lhs, rhs = ex0.args
            if isa(lhs, Expr)
                if lhs.head === :(.)
                    return gen_call(fcn, Any[Base.setproperty!, lhs.args..., rhs], where_params, kws; use_signature_tuple)
                elseif lhs.head === :ref
                    return expand_ref_begin_end!(lhs, __module__) do ex
                        gen_call(fcn, Any[setindex!, ex.args[1], rhs, ex.args[2:end]...], where_params, kws; use_signature_tuple)
                    end
                end
            end
        elseif ex0.head === :vcat || ex0.head === :typed_vcat
            if ex0.head === :vcat
                f, hf = Base.vcat, Base.hvcat
                args = ex0.args
            else
                f, hf = Base.typed_vcat, Base.typed_hvcat
                args = ex0.args[2:end]
            end
            if any(@nospecialize(a)->isa(a,Expr) && a.head === :row, args)
                rows = Any[ (isa(x,Expr) && x.head === :row ? x.args : Any[x]) for x in args ]
                lens = map(length, rows)
                args = Any[Expr(:tuple, lens...); vcat(rows...)]
                ex0.head === :typed_vcat && pushfirst!(args, ex0.args[1])
                return gen_call(fcn, Any[hf, args...], where_params, kws; use_signature_tuple)
            else
                return gen_call(fcn, Any[f, ex0.args...], where_params, kws; use_signature_tuple)
            end
        elseif ex0.head === :ref
            return expand_ref_begin_end!(ex0, __module__) do ex
                gen_call(fcn, Any[getindex, ex.args...], where_params, kws; use_signature_tuple)
            end
        else
            for (head, f) in Any[:hcat => Base.hcat,
                                 :(.) => Base.getproperty,
                                 :vect => Base.vect,
                                 Symbol("'") => Base.adjoint,
                                 :typed_hcat => Base.typed_hcat,
                                 :string => string]
                ex0.head === head || continue
                return gen_call(fcn, Any[f, ex0.args...], where_params, kws; use_signature_tuple)
            end
        end
    end
    if isa(ex0, Expr) && ex0.head === :macrocall # Make @edit @time 1+2 edit the macro by using the types of the *expressions*
        args = [#=__source__::=#LineNumberNode, #=__module__::=#Module, Core.Typeof.(ex0.args[3:end])...]
        return gen_call(fcn, Any[ex0.args[1], Ref.(args)...], where_params, kws; use_signature_tuple)
    end

    ex = Meta.lower(__module__, ex0)
    isa(ex, Expr) || return Expr(:call, :error, "expression is not a function call or symbol")

    return Expr(:call, :error, "expression is not a function call, \
                                    or is too complex for @$fcn to analyze; \
                                    break it down to simpler parts if possible. \
                                    In some cases, you may want to use Meta.@lower.")
end

"""
Same behaviour as `gen_call_with_extracted_types` except that keyword arguments
of the form "foo=bar" are passed on to the called function as well.
The keyword arguments must be given before the mandatory argument.
"""
function gen_call_with_extracted_types_and_kwargs(__module__, fcn, ex0; is_source_reflection = !is_code_macro(fcn), supports_binding_reflection = false, use_signature_tuple = false)
    kws = Expr[]
    arg = ex0[end] # Mandatory argument
    for i in 1:length(ex0)-1
        x = ex0[i]
        if x isa Expr && x.head === :(=) # Keyword given of the form "foo=bar"
            if length(x.args) != 2
                return Expr(:call, :error, "Invalid keyword argument: $x")
            end
            push!(kws, Expr(:kw, esc(x.args[1]), esc(x.args[2])))
        else
            return Expr(:call, :error, "@$fcn expects only one non-keyword argument")
        end
    end
    return gen_call_with_extracted_types(__module__, fcn, arg, kws; is_source_reflection, supports_binding_reflection, use_signature_tuple)
end

for fname in [:which, :less, :edit, :functionloc]
    @eval begin
        macro ($fname)(ex0)
            gen_call_with_extracted_types(__module__, $(Expr(:quote, fname)), ex0, Expr[];
                                          is_source_reflection = true,
                                          supports_binding_reflection = $(fname === :which),
                                          use_signature_tuple = true)
        end
    end
end

macro which(ex0::Symbol)
    ex0 = QuoteNode(ex0)
    return :(which($__module__, $ex0))
end

for fname in [:code_warntype, :code_llvm, :code_native,
              :infer_return_type, :infer_effects, :infer_exception_type]
    @eval macro ($fname)(ex0...)
        gen_call_with_extracted_types_and_kwargs(__module__, $(QuoteNode(fname)), ex0; is_source_reflection = false, use_signature_tuple = $(in(fname, [:code_warntype, :code_llvm, :code_native])))
    end
end

for fname in [:code_typed, :code_lowered, :code_ircode]
    @eval macro ($fname)(ex0...)
        thecall = gen_call_with_extracted_types_and_kwargs(__module__, $(QuoteNode(fname)), ex0; is_source_reflection = false, use_signature_tuple = true)
        quote
            local results = $thecall
            length(results) == 1 ? results[1] : results
        end
    end
end

"""
    @functionloc

Applied to a function or macro call, it evaluates the arguments to the specified call, and
returns a tuple `(filename,line)` giving the location for the method that would be called for those arguments.
It calls out to the [`functionloc`](@ref) function.
"""
:@functionloc

"""
    @which

Applied to a function or macro call, it evaluates the arguments to the specified call, and
returns the `Method` object for the method that would be called for those arguments. Applied
to a variable, it returns the module in which the variable was bound. It calls out to the
[`which`](@ref) function.

See also: [`@less`](@ref), [`@edit`](@ref).
"""
:@which

"""
    @less

Evaluates the arguments to the function or macro call, determines their types, and calls the [`less`](@ref)
function on the resulting expression.

See also: [`@edit`](@ref), [`@which`](@ref), [`@code_lowered`](@ref).
"""
:@less

"""
    @edit

Evaluates the arguments to the function or macro call, determines their types, and calls the [`edit`](@ref)
function on the resulting expression.

See also: [`@less`](@ref), [`@which`](@ref).
"""
:@edit

"""
    @code_typed

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_typed`](@ref) on the resulting expression. Use the optional argument `optimize` with

    @code_typed optimize=true foo(x)

to control whether additional optimizations, such as inlining, are also applied.

See also: [`code_typed`](@ref), [`@code_warntype`](@ref), [`@code_lowered`](@ref), [`@code_llvm`](@ref), [`@code_native`](@ref).
"""
:@code_typed

"""
    @code_lowered

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_lowered`](@ref) on the resulting expression.

See also: [`code_lowered`](@ref), [`@code_warntype`](@ref), [`@code_typed`](@ref), [`@code_llvm`](@ref), [`@code_native`](@ref).
"""
:@code_lowered

"""
    @code_warntype

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_warntype`](@ref) on the resulting expression.

See also: [`code_warntype`](@ref), [`@code_typed`](@ref), [`@code_lowered`](@ref), [`@code_llvm`](@ref), [`@code_native`](@ref).
"""
:@code_warntype

"""
    @code_llvm

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_llvm`](@ref) on the resulting expression.
Set the optional keyword arguments `raw`, `dump_module`, `debuginfo`, `optimize`
by putting them and their value before the function call, like this:

    @code_llvm raw=true dump_module=true debuginfo=:default f(x)
    @code_llvm optimize=false f(x)

`optimize` controls whether additional optimizations, such as inlining, are also applied.
`raw` makes all metadata and dbg.* calls visible.
`debuginfo` may be one of `:source` (default) or `:none`,  to specify the verbosity of code comments.
`dump_module` prints the entire module that encapsulates the function.

See also: [`code_llvm`](@ref), [`@code_warntype`](@ref), [`@code_typed`](@ref), [`@code_lowered`](@ref), [`@code_native`](@ref).
"""
:@code_llvm

"""
    @code_native

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_native`](@ref) on the resulting expression.

Set any of the optional keyword arguments `syntax`, `debuginfo`, `binary` or `dump_module`
by putting it before the function call, like this:

    @code_native syntax=:intel debuginfo=:default binary=true dump_module=false f(x)

* Set assembly syntax by setting `syntax` to `:intel` (default) for Intel syntax or `:att` for AT&T syntax.
* Specify verbosity of code comments by setting `debuginfo` to `:source` (default) or `:none`.
* If `binary` is `true`, also print the binary machine code for each instruction precedented by an abbreviated address.
* If `dump_module` is `false`, do not print metadata such as rodata or directives.

See also: [`code_native`](@ref), [`@code_warntype`](@ref), [`@code_typed`](@ref), [`@code_lowered`](@ref), [`@code_llvm`](@ref).
"""
:@code_native

"""
    @time_imports

A macro to execute an expression and produce a report of any time spent importing packages and their
dependencies. Any compilation time will be reported as a percentage, and how much of which was recompilation, if any.

One line is printed per package or package extension. The duration shown is the time to import that package itself, not including the time to load any of its dependencies.

On Julia 1.9+ [package extensions](@ref man-extensions) will show as Parent → Extension.

!!! note
    During the load process a package sequentially imports all of its dependencies, not just its direct dependencies.

```julia-repl
julia> @time_imports using CSV
     50.7 ms  Parsers 17.52% compilation time
      0.2 ms  DataValueInterfaces
      1.6 ms  DataAPI
      0.1 ms  IteratorInterfaceExtensions
      0.1 ms  TableTraits
     17.5 ms  Tables
     26.8 ms  PooledArrays
    193.7 ms  SentinelArrays 75.12% compilation time
      8.6 ms  InlineStrings
     20.3 ms  WeakRefStrings
      2.0 ms  TranscodingStreams
      1.4 ms  Zlib_jll
      1.8 ms  CodecZlib
      0.8 ms  Compat
     13.1 ms  FilePathsBase 28.39% compilation time
   1681.2 ms  CSV 92.40% compilation time
```

!!! compat "Julia 1.8"
    This macro requires at least Julia 1.8

"""
:@time_imports

"""
    @trace_compile

A macro to execute an expression and show any methods that were compiled (or recompiled in yellow),
like the julia args `--trace-compile=stderr --trace-compile-timing` but specifically for a call.

```julia-repl
julia> @trace_compile rand(2,2) * rand(2,2)
#=   39.1 ms =# precompile(Tuple{typeof(Base.rand), Int64, Int64})
#=  102.0 ms =# precompile(Tuple{typeof(Base.:(*)), Array{Float64, 2}, Array{Float64, 2}})
2×2 Matrix{Float64}:
 0.421704  0.864841
 0.211262  0.444366
```

!!! compat "Julia 1.12"
    This macro requires at least Julia 1.12

"""
:@trace_compile

"""
    @trace_dispatch

A macro to execute an expression and report methods that were compiled via dynamic dispatch,
like the julia arg `--trace-dispatch=stderr` but specifically for a call.

!!! compat "Julia 1.12"
    This macro requires at least Julia 1.12

"""
:@trace_dispatch

"""
    @activate Component

Activate a newly loaded copy of an otherwise builtin component. The `Component`
to be activated will be resolved using the ordinary rules of module resolution
in the current environment.

When using `@activate`, additional options for a component may be specified in
square brackets `@activate Compiler[:option1, :option]`

Currently `Compiler` and `JuliaLowering` are the only available components that
may be activatived.

For `@activate Compiler`, the following options are available:
1. `:reflection` - Activate the compiler for reflection purposes only.
                   The ordinary reflection functionality in `Base` and `InteractiveUtils`.
                   Will use the newly loaded compiler. Note however, that these reflection
                   functions will still interact with the ordinary native cache (both loading
                   and storing). An incorrect compiler implementation may thus corrupt runtime
                   state if reflection is used. Use external packages like `Cthulhu.jl`
                   introspecting compiler behavior with a separated cache partition.

2. `:codegen`   - Activate the compiler for internal codegen purposes. The new compiler
                  will be invoked whenever the runtime requests compilation.

`@activate Compiler` without options is equivalent to `@activate Compiler[:reflection]`.

"""
macro activate(what)
    options = Symbol[]
    if isexpr(what, :ref)
        Component = what.args[1]
        for i = 2:length(what.args)
            arg = what.args[i]
            if !isa(arg, QuoteNode) || !isa(arg.value, Symbol)
                error("Usage Error: Option $arg is not a symbol")
            end
            push!(options, arg.value)
        end
    else
        Component = what
    end
    if !isa(Component, Symbol)
        error("Usage Error: Component $Component is not a symbol")
    end
    allowed_components = (:Compiler, :JuliaLowering)
    if !(Component in allowed_components)
        error("Usage Error: Component $Component is not recognized. Expected one of $allowed_components")
    end
    if Component === :Compiler && isempty(options)
        push!(options, :reflection)
    end
    options = map(options) do opt
        Expr(:kw, opt, true)
    end
    return :(Base.require($__module__, $(QuoteNode(Component))).activate!(; $(options...)))
end
