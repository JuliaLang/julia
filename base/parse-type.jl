# This file is a part of Julia. License is MIT: https://julialang.org/license
# Parses a string into a Julia type object, e.g. `Int`, `Array{Int, 2}`, etc.
"""
    Base.parse(Type, str;    module_context=Main)
    Base.parse(Type{T}, str; module_context=Main)

Parse a string into a Julia type object, which must be a subtype of `Type{T}`. Parsing a
string directly into a Type object is faster and safer than using eval (since it prevents
arbitrary code execution).

The provided `str` should be a valid julia type expression, e.g. `parse(Type, "Int")`,
`parse(Type, "Vector{Any}")`, etc, which parses as a subtype of the provided `Type{T}`.
Throws an `ArgumentError` if parsing fails.

A more specific `Type{T}` can be used to restrict the parsed type to match a provided
supertype. For example, `parse(Type{<:Number}, str)` could return `Int`, `Float64`, or any
other subtype of `Number`.

The parsing uses the context of the `module_context`, which defaults to `Main`.

# Examples
```julia
julia> parse(Type, "Int")
Int64

julia> parse(Type{<:Vector{<:Integer}}, "Vector{Int}")
Vector{Int64}

julia> parse(Type{<:Integer}, "Int")
Int64

julia> parse(Type{Integer}, "Int")
ERROR: TypeError: in typeassert, expected Type{Integer}, got Type{Int64}

julia> parse(Type{<:Integer}, "Float32")
ERROR: TypeError: in typeassert, expected Type{<:Integer}, got Type{Float32}

julia> parse(Type, "CustomSet{Int}", module_context=CustomSets)
CustomSets.CustomSet{Int64}
```
"""
function Base.parse(::Type{T}, str::AbstractString; module_context=Main) where T<:Type
    # tryparse(Type) instead of (T), so we only have to do the type check once.
    v = Base.tryparse(Type, str; module_context)
    if v === nothing
        throw(ArgumentError("invalid type expression: $str"))
    end
    # Don't pay for the assertion if not needed (~2 μs)
    T === Type && return v
    return v::T
end
function Base.tryparse(::Type{T}, str::AbstractString; module_context=Main) where {T<:Type}
    ast = Meta.parse(str, raise=false)
    # (This only needs to be checked once, so it's checked here, rather than in
    # _try_parse_type, since _try_parse_type is self-recursive.)
    if ast isa Expr && ast.head === :incomplete
        return nothing
    end
    v = _try_parse_type(module_context, ast, false)
    v === nothing && return nothing
    # Don't pay for the type check if not needed (~2 μs)
    T === Type && return v
    v isa T || return nothing
    return v
end

# Simplify the commonly repeated pattern checking return values.
macro _bail_if_nothing(expr)
    Base.remove_linenums!(quote
        v = $(esc(expr))
        v === nothing && return nothing
        v
    end)
end

# NOTE: This pattern is a hard-coded part of types: unnamed type variables start with `#s`.
_unnamed_type_var() = Symbol("#s$(gensym())")

function _try_parse_type(module_context, ast, raise::Bool, type_vars = nothing)
    if ast isa Expr && ast.head == :curly
        # If the type expression has type parameters, iterate the params, evaluating them
        # recursively, and finally construct the output type with the evaluated params.

        @_bail_if_nothing typ = _try_parse_qualified_type(module_context, ast.args[1], raise, type_vars)

        # Parse the type parameters, if there are any.

        # If any of the type parameters are unnamed type restrictions, like `<:Number`, we
        # will construct new anonymous type variables for them, and wrap the returned type
        # in a UnionAll.
        new_type_vars = Vector{TypeVar}()
        if length(ast.args) > 1
            # PERF: Reuse the vector to save allocations
            for i in 2:length(ast.args)
                arg = ast.args[i]
                if arg isa Expr && arg.head === :(<:) && length(arg.args) == 1
                    # Change `Vector{<:Number}` to `Vector{#s#27} where #s#27<:Number`
                    type_var = TypeVar(_unnamed_type_var(), @_bail_if_nothing(_try_parse_type(module_context, arg.args[1], raise, type_vars)))
                    push!(new_type_vars, type_var)
                    ast.args[i] = type_var
                else
                    @_bail_if_nothing ast.args[i] = _try_parse_type(module_context, ast.args[i], raise, type_vars)
                end
            end
        end
        # PERF: Drop the first element, instead of args[2:end], to avoid a new sub-vector
        popfirst!(ast.args)
        body = try
            typ{ast.args...}
        catch
            raise && throw(ArgumentError("invalid type expression: $ast"))
            return nothing
        end
        # Handle any new type vars we created
        if !isempty(new_type_vars)
            # Now work backwards through the new type vars and construct our wrapper UnionAlls:
            for type_var in Iterators.reverse(new_type_vars)
                body = UnionAll(type_var, body)
            end
        end
        return body
    elseif ast isa Expr && ast.head == :where
        # Collect all the type vars
        # Keep them in order, since we need to wrap the UnionAlls in reverse order.
        new_type_vars = TypeVar[]
        type_vars = Dict{Symbol, TypeVar}()
        for i in 2:length(ast.args)
            @_bail_if_nothing type_var = _try_parse_type_var(module_context, ast.args[i], raise, type_vars)
            type_var::TypeVar
            type_vars[type_var.name] = type_var
            push!(new_type_vars, type_var)
        end
        # Then evaluate the body in the context of those type vars
        @_bail_if_nothing body = _try_parse_type(module_context, ast.args[1], raise, type_vars)
        # Now work backwards through the new type vars and construct our wrapper UnionAlls:
        for type_var in Iterators.reverse(new_type_vars)
            body = UnionAll(type_var, body)
        end
        return body
    elseif ast isa Expr && ast.head == :call && ast.args[1] === :typeof
        return typeof(@_bail_if_nothing _try_parse_type(module_context, ast.args[2], raise, type_vars))
    elseif ast isa Expr && ast.head == :call
        return @_bail_if_nothing _try_parse_isbits_constructor(module_context, ast, raise, type_vars)
    else
        return @_bail_if_nothing _try_parse_qualified_type(module_context, ast, raise, type_vars)
    end
end
_try_parse_qualified_type(module_context, val, _, _) = val
function _try_parse_qualified_type(module_context, ast::Expr, raise::Bool, type_vars)
    if ast.head !== :(.)
        raise && throw(ArgumentError("Failed to parse type expression. Expected a \
                qualified type, e.g. `Base.Dict`, got: `$ast`"))
        return nothing
    end
    @_bail_if_nothing mod = _try_parse_qualified_type(module_context, ast.args[1], raise, type_vars)
    value = ast.args[2]
    if value isa QuoteNode
        value = value.value
    end
    return getglobal(mod, value)
end
function _try_parse_qualified_type(module_context, sym::Symbol, raise::Bool, type_vars)
    # First try to look up the symbol in the type vars
    if type_vars !== nothing
        v_if_found = get(type_vars, sym, :not_found)
        if v_if_found !== :not_found
            return v_if_found
        end
    end

    # Otherwise, this should be a top-level name in the provided module.

    if !raise  # Skip the isdefined check if !raise, since getglobal will throw anyway.
        !isdefined(module_context, sym) && return nothing
    end
    getglobal(module_context, sym)
end

# Parses constant isbits constructor expressions, like `Int32(10)` or `Point(0,0)`, as used in type
# parameters like `Val{10}()` or `DefaultDict{Point(0,0)}`.
function _try_parse_isbits_constructor(module_context, ast, raise::Bool, type_vars)
    @_bail_if_nothing typ = _try_parse_type(module_context, ast.args[1], raise, type_vars)
    # PERF: Reuse the args vector when parsing the type values.
    popfirst!(ast.args)
    for i in 1:length(ast.args)
        @_bail_if_nothing ast.args[i] = _try_parse_type(module_context, ast.args[i], raise, type_vars)
    end
    # We use reinterpret to avoid evaluating code, which may have side effects.
    return reinterpret(typ, Tuple(ast.args))
end

_try_parse_type_var(module_context, ast::Symbol, raise::Bool, _type_vars) = Core.TypeVar(ast)
function _try_parse_type_var(module_context, ast::Expr, raise, type_vars)
    if ast.head === :(<:)
        return Core.TypeVar(ast.args[1]::Symbol, @_bail_if_nothing _try_parse_type(module_context, ast.args[2], raise, type_vars))
    elseif ast.head === :(>:)
        return Core.TypeVar(ast.args[2]::Symbol, @_bail_if_nothing _try_parse_type(module_context, ast.args[1], raise, type_vars))
    elseif ast.head === :comparison
        if ast.args[2] === :(<:)
            @assert ast.args[4] === :(<:) "invalid bounds in \"where\": $ast"
            return Core.TypeVar(ast.args[3]::Symbol, @_bail_if_nothing(_try_parse_type(module_context, ast.args[1], raise, type_vars)), @_bail_if_nothing(_try_parse_type(module_context, ast.args[5], raise, type_vars)))
        else
            @assert ast.args[2] === ast.args[4] === :(>:) "invalid bounds in \"where\": $ast"
            return Core.TypeVar(ast.args[3]::Symbol, @_bail_if_nothing(_try_parse_type(module_context, ast.args[5], raise, type_vars)), @_bail_if_nothing(_try_parse_type(module_context, ast.args[1], raise, type_vars)))
        end
    else
        @assert false "invalid bounds in \"where\": $ast"
    end
end
