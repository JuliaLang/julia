# Parses a string into a Julia type object, e.g. `Int`, `Array{Int, 2}`, etc.
function Base.parse(::Type{T}, str::AbstractString) where T<:Type
    # tryparse(Type) instead of (T), so we only have to do the type check once.
    v = Base.tryparse(Type, str)
    if v === nothing
        throw(ArgumentError("invalid type expression: $str"))
    end
    # Don't pay for the assertion if not needed (~2 μs)
    T === Type && return v
    return v::T
end
function Base.tryparse(::Type{T}, str::AbstractString) where {T<:Type}
    ast = Meta.parse(str, raise=false)
    # (This only needs to be checked once, so it's checked here, rather than in
    # _try_parse_type, since _try_parse_type is self-recursive.)
    if ast isa Expr && ast.head === :incomplete
        return nothing
    end
    v = _try_parse_type(ast, false)
    v === nothing && return nothing
    # Don't pay for the type check if not needed (~2 μs)
    T === Type && return v
    v isa T || return nothing
    return v
end

# Simplify the commonly repeated pattern checking return values.
macro _or_nothing(expr)
    Base.remove_linenums!(quote
        v = $(esc(expr))
        v === nothing && return nothing
        v
    end)
end

# NOTE: This pattern is a hard-coded part of types: unnamed type variables start with `#s`.
_unnamed_type_var() = Symbol("#s$(gensym())")

function _try_parse_type(ast, raise::Bool, type_vars = nothing)
    if ast isa Expr && ast.head == :curly
        # If the type expression has type parameters, iterate the params, evaluating them
        # recursively, and finally construct the output type with the evaluated params.

        @_or_nothing typ = _try_parse_qualified_type(ast.args[1], raise, type_vars)
        length(ast.args) == 1 && return typ
        # If any of the type parameters are unnamed type restrictions, like `<:Number`, we
        # will construct new anonymous type variables for them, and wrap the returned type
        # in a UnionAll.
        new_type_vars = Vector{TypeVar}()
        # PERF: Reuse the vector to save allocations
        for i in 2:length(ast.args)
            arg = ast.args[i]
            if arg isa Expr && arg.head === :(<:) && length(arg.args) == 1
                # Change `Vector{<:Number}` to `Vector{#s#27} where #s#27<:Number`
                type_var = TypeVar(_unnamed_type_var(), @_or_nothing(_try_parse_type(arg.args[1], raise, type_vars)))
                push!(new_type_vars, type_var)
                ast.args[i] = type_var
            else
                @_or_nothing ast.args[i] = _try_parse_type(ast.args[i], raise, type_vars)
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
            @_or_nothing type_var = _try_parse_type_var(ast.args[i], raise, type_vars)
            type_var::TypeVar
            type_vars[type_var.name] = type_var
            push!(new_type_vars, type_var)
        end
        # Then evaluate the body in the context of those type vars
        @_or_nothing body = _try_parse_type(ast.args[1], raise, type_vars)
        # Now work backwards through the new type vars and construct our wrapper UnionAlls:
        for type_var in Iterators.reverse(new_type_vars)
            body = UnionAll(type_var, body)
        end
        return body
    elseif ast isa Expr && ast.head == :call && ast.args[1] === :typeof
        return typeof(@_or_nothing _try_parse_type(ast.args[2], raise, type_vars))
    elseif ast isa Expr && ast.head == :call
        return @_or_nothing _try_parse_isbits_constructor(ast, raise, type_vars)
    else
        return @_or_nothing _try_parse_qualified_type(ast, raise, type_vars)
    end
end
_try_parse_qualified_type(val, _, _) = val
function _try_parse_qualified_type(ast::Expr, raise::Bool, type_vars)
    if ast.head !== :(.)
        raise && throw(ArgumentError("Failed to parse type expression. Expected a \
                qualified type, e.g. `Base.Dict`, got: `$ast`"))
        return nothing
    end
    @_or_nothing mod = _try_parse_qualified_type(ast.args[1], raise, type_vars)
    value = ast.args[2]
    if value isa QuoteNode
        value = value.value
    end
    return getglobal(mod, value)
end
function _try_parse_qualified_type(sym::Symbol, raise::Bool, type_vars)
    # First try to look up the symbol in the type vars
    if type_vars !== nothing
        v_if_found = get(type_vars, sym, :not_found)
        if v_if_found !== :not_found
            return v_if_found
        end
    end
    #@show type_vars

    # Otherwise, this is a top-level name.

    # If it is a module name (e.g. `Core` or `Pkg`), in order for us to parse a type from a
    # top-level module, the module would have to be loaded. So we look to the loaded_modules
    # to find it.
    for mod in values(Base.loaded_modules)
        if sym == nameof(mod)
            return mod
        end
    end

    # Finally, we can assume that this is a name defined in Main, since if it was in a
    # module other than Main, it should have been qualified. These would include names like
    # `Int` or `Dict`. So we look these up directly.

    if !raise  # Skip the isdefined check if !raise, since getglobal will throw anyway.
        !isdefined(Main, sym) && return nothing
    end
    getglobal(Main, sym)
end

# Parses constant isbits constructor expressions, like `Int32(10)` or `Point(0,0)`, as used in type
# parameters like `Val{10}()` or `DefaultDict{Point(0,0)}`.
function _try_parse_isbits_constructor(ast, raise::Bool, type_vars)
    @_or_nothing typ = _try_parse_type(ast.args[1], raise, type_vars)
    # PERF: Reuse the args vector when parsing the type values.
    popfirst!(ast.args)
    for i in 1:length(ast.args)
        @_or_nothing ast.args[i] = _try_parse_type(ast.args[i], raise, type_vars)
    end
    # We use reinterpret to avoid evaluating code, which may have side effects.
    return reinterpret(typ, Tuple(ast.args))
end

_try_parse_type_var(ast::Symbol, raise::Bool, _type_vars) = Core.TypeVar(ast)
function _try_parse_type_var(ast::Expr, raise, type_vars)
    if ast.head === :(<:)
        return Core.TypeVar(ast.args[1]::Symbol, @_or_nothing _try_parse_type(ast.args[2], raise, type_vars))
    elseif ast.head === :(>:)
        return Core.TypeVar(ast.args[2]::Symbol, @_or_nothing _try_parse_type(ast.args[1], raise, type_vars))
    elseif ast.head === :comparison
        if ast.args[2] === :(<:)
            @assert ast.args[4] === :(<:) "invalid bounds in \"where\": $ast"
            return Core.TypeVar(ast.args[3]::Symbol, @_or_nothing(_try_parse_type(ast.args[1], raise, type_vars)), @_or_nothing(_try_parse_type(ast.args[5], raise, type_vars)))
        else
            @assert ast.args[2] === ast.args[4] === :(>:) "invalid bounds in \"where\": $ast"
            return Core.TypeVar(ast.args[3]::Symbol, @_or_nothing(_try_parse_type(ast.args[5], raise, type_vars)), @_or_nothing(_try_parse_type(ast.args[1], raise, type_vars)))
        end
    else
        @assert false "invalid bounds in \"where\": $ast"
    end
end
