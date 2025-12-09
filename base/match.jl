# This file is a part of Julia. License is MIT: https://julialang.org/license

# Pattern matching runtime support

"""
    MatchError <: Exception

Exception thrown when a `match` expression fails to match any pattern.

# Examples
```julia
match x
    1 -> "one"
    2 -> "two"
end  # throws MatchError if x is not 1 or 2
```
"""
struct MatchError <: Exception
    value::Any
end

function showerror(io::IO, e::MatchError)
    print(io, "MatchError: no pattern matched value ")
    show(io, e.value)
end

# Abstract base type for all matchers
abstract type Matcher end

# Wildcard matcher: matches anything, captures nothing
struct Wildcard <: Matcher end

function match(::Wildcard, @nospecialize(value))
    return Dict{Symbol,Any}()
end

# Literal matcher: matches by ==
struct Literal <: Matcher
    value::Any
end

function match(m::Literal, @nospecialize(value))
    m.value == value ? Dict{Symbol,Any}() : nothing
end

# Capture matcher: captures value under a name
struct Capture <: Matcher
    name::Symbol
end

function match(m::Capture, @nospecialize(value))
    return Dict{Symbol,Any}(m.name => value)
end

# Type matcher: matches by isa
struct TypeMatcher <: Matcher
    type::Type
end

function match(m::TypeMatcher, @nospecialize(value))
    value isa m.type ? Dict{Symbol,Any}() : nothing
end

# Typed capture: captures with type constraint
struct TypedCapture <: Matcher
    name::Symbol
    type::Type
end

function match(m::TypedCapture, @nospecialize(value))
    value isa m.type ? Dict{Symbol,Any}(m.name => value) : nothing
end

# Tuple matcher: structural match for tuples
struct TupleMatcher <: Matcher
    matchers::Vector{Matcher}
end

TupleMatcher(matchers::Matcher...) = TupleMatcher(collect(Matcher, matchers))

function match(m::TupleMatcher, @nospecialize(value))
    value isa Tuple || return nothing
    length(value) == length(m.matchers) || return nothing
    bindings = Dict{Symbol,Any}()
    for (matcher, v) in zip(m.matchers, value)
        result = match(matcher, v)
        result === nothing && return nothing
        merge!(bindings, result)
    end
    return bindings
end

# Alternation matcher: tries each pattern, returns first match
struct Alternation <: Matcher
    matchers::Vector{Matcher}
end

Alternation(matchers::Matcher...) = Alternation(collect(Matcher, matchers))

function match(m::Alternation, @nospecialize(value))
    for matcher in m.matchers
        result = match(matcher, value)
        result !== nothing && return result
    end
    return nothing
end

# Call matcher: matches constructor patterns like Some(x)
struct CallMatcher <: Matcher
    f::Any  # The constructor/type to match
    matchers::Vector{Matcher}
end

CallMatcher(f, matchers::Matcher...) = CallMatcher(f, collect(Matcher, matchers))

function match(m::CallMatcher, @nospecialize(value))
    # For types, check if value is an instance
    if m.f isa Type
        value isa m.f || return nothing
        # Match against fields
        nfields(value) == length(m.matchers) || return nothing
        bindings = Dict{Symbol,Any}()
        for (i, matcher) in enumerate(m.matchers)
            result = match(matcher, getfield(value, i))
            result === nothing && return nothing
            merge!(bindings, result)
        end
        return bindings
    else
        # For non-type constructors, we can't deconstruct
        # This is for future extensibility
        return nothing
    end
end

# @match macro - provides macro-level pattern matching
"""
    @match value begin
        pattern1 -> result1
        pattern2 -> result2
        _ -> default
    end

Pattern matching macro. Evaluates `value` and matches it against each pattern
in order. Returns the result of the first matching pattern's expression.

Throws `MatchError` if no pattern matches.

# Patterns
- Literals: `1`, `"hello"`, `:symbol`
- Wildcard: `_` matches anything
- Variables: `x` captures the matched value
- Type assertions: `x::Int` captures with type check, `::Int` just checks type
- Tuples: `(a, b, c)` destructures tuples
- Alternation: `p1 | p2` matches if either pattern matches
- Value escaping: `\$x` matches against the value of variable `x`

# Examples
```julia
result = @match x begin
    0 -> "zero"
    n::Int -> "integer: \$n"
    (a, b) -> "tuple: \$a, \$b"
    _ -> "something else"
end
```
"""
macro match(value, body)
    # Transform the body from begin...end block with -> expressions
    # into a match expression
    if !(body isa Expr && body.head === :block)
        error("@match body must be a begin...end block")
    end

    arms = Expr[]
    for ex in body.args
        ex isa LineNumberNode && continue
        if ex isa Expr && ex.head === :->
            push!(arms, ex)
        else
            error("@match arms must be `pattern -> result` expressions")
        end
    end

    if isempty(arms)
        error("@match requires at least one arm")
    end

    # Build nested if/else structure
    scrutinee = gensym("value")
    result = :(throw(MatchError($scrutinee)))

    for arm in reverse(arms)
        pattern = arm.args[1]
        body_expr = arm.args[2]
        cond, bindings = pattern_to_condition(pattern, scrutinee)

        if isempty(bindings)
            result = Expr(:if, cond, body_expr, result)
        else
            # Create a let block for the bindings
            let_bindings = [Expr(:(=), name, val) for (name, val) in bindings]
            body_with_bindings = Expr(:let, Expr(:block, let_bindings...), body_expr)
            result = Expr(:if, cond, body_with_bindings, result)
        end
    end

    return esc(Expr(:let, Expr(:(=), scrutinee, value), result))
end

# Convert a pattern to a condition expression and bindings
function pattern_to_condition(pattern, scrutinee)
    if pattern === :_
        # Wildcard
        return true, Pair{Symbol,Any}[]
    elseif pattern isa Symbol
        # Variable capture
        return true, [pattern => scrutinee]
    elseif pattern isa Union{Number, String, Char, Bool}
        # Literal
        return :($scrutinee == $pattern), Pair{Symbol,Any}[]
    elseif pattern isa QuoteNode
        # Quoted symbol
        return :($scrutinee === $(pattern)), Pair{Symbol,Any}[]
    elseif pattern isa Expr
        if pattern.head === :$
            # Escaped value
            return :($scrutinee == $(pattern.args[1])), Pair{Symbol,Any}[]
        elseif pattern.head === :(::)
            if length(pattern.args) == 1
                # ::T - just type check
                T = pattern.args[1]
                return :($scrutinee isa $T), Pair{Symbol,Any}[]
            else
                # x::T - capture with type check
                name = pattern.args[1]
                T = pattern.args[2]
                return :($scrutinee isa $T), [name => scrutinee]
            end
        elseif pattern.head === :tuple
            # Tuple destructuring
            n = length(pattern.args)
            conds = [:($scrutinee isa Tuple), :(length($scrutinee) == $n)]
            bindings = Pair{Symbol,Any}[]
            for (i, p) in enumerate(pattern.args)
                elem_var = gensym("elem")
                elem_cond, elem_bindings = pattern_to_condition(p, elem_var)
                if elem_cond !== true
                    push!(conds, :($elem_var = $scrutinee[$i]; $elem_cond))
                else
                    for (name, _) in elem_bindings
                        push!(bindings, name => :($scrutinee[$i]))
                    end
                    continue
                end
                for (name, val) in elem_bindings
                    if val === elem_var
                        push!(bindings, name => :($scrutinee[$i]))
                    else
                        push!(bindings, name => val)
                    end
                end
            end
            cond = foldl((a, b) -> :($a && $b), conds; init=true)
            return cond, bindings
        elseif pattern.head === :call && pattern.args[1] === :|
            # Alternation
            conds = Any[]
            all_bindings = Pair{Symbol,Any}[]
            for p in pattern.args[2:end]
                c, b = pattern_to_condition(p, scrutinee)
                push!(conds, c)
                append!(all_bindings, b)
            end
            cond = foldl((a, b) -> :($a || $b), conds)
            # For alternation, bindings are tricky - for now just use first set
            return cond, unique(first, all_bindings)
        elseif pattern.head === :call
            # Constructor pattern
            f = pattern.args[1]
            conds = [:($scrutinee isa $f)]
            bindings = Pair{Symbol,Any}[]
            for (i, p) in enumerate(pattern.args[2:end])
                field_var = gensym("field")
                field_cond, field_bindings = pattern_to_condition(p, field_var)
                if field_cond !== true
                    push!(conds, :($field_var = getfield($scrutinee, $i); $field_cond))
                end
                for (name, val) in field_bindings
                    if val === field_var
                        push!(bindings, name => :(getfield($scrutinee, $i)))
                    else
                        push!(bindings, name => val)
                    end
                end
            end
            cond = foldl((a, b) -> :($a && $b), conds; init=true)
            return cond, bindings
        end
    end
    # Default: treat as literal comparison
    return :($scrutinee == $pattern), Pair{Symbol,Any}[]
end
