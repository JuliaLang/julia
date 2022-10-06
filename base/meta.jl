# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Convenience functions for metaprogramming.
"""
module Meta

using ..CoreLogging

export quot,
       isexpr,
       isidentifier,
       isoperator,
       isunaryoperator,
       isbinaryoperator,
       ispostfixoperator,
       replace_sourceloc!,
       show_sexpr,
       @dump

using Base: isidentifier, isoperator, isunaryoperator, isbinaryoperator, ispostfixoperator
import Base: isexpr

"""
    Meta.quot(ex)::Expr

Quote expression `ex` to produce an expression with head `quote`. This can for
instance be used to represent objects of type `Expr` in the AST. See also the
manual section about [QuoteNode](@ref man-quote-node).

# Examples
```jldoctest
julia> eval(Meta.quot(:x))
:x

julia> dump(Meta.quot(:x))
Expr
  head: Symbol quote
  args: Array{Any}((1,))
    1: Symbol x

julia> eval(Meta.quot(:(1+2)))
:(1 + 2)
```
"""
quot(ex) = Expr(:quote, ex)

"""
    Meta.isexpr(ex, head[, n])::Bool

Return true if `ex` is an `Expr` with the given type `head` and optionally that
the argument list is of length `n`. `head` may be a `Symbol` or collection of
`Symbol`s. For example, to check that a macro was passed a function call
expression, you might use `isexpr(ex, :call)`.

# Examples
```jldoctest
julia> ex = :(f(x))
:(f(x))

julia> Meta.isexpr(ex, :block)
false

julia> Meta.isexpr(ex, :call)
true

julia> Meta.isexpr(ex, [:block, :call]) # multiple possible heads
true

julia> Meta.isexpr(ex, :call, 1)
false

julia> Meta.isexpr(ex, :call, 2)
true
```
"""
isexpr

"""
    replace_sourceloc!(location, expr)

Overwrite the caller source location for each macro call in `expr`, returning
the resulting AST.  This is useful when you need to wrap a macro inside a
macro, and want the inner macro to see the `__source__` location of the outer
macro.  For example:

```
macro test_is_one(ex)
    replace_sourceloc!(__source__, :(@test \$(esc(ex)) == 1))
end
@test_is_one 2
```

`@test` now reports the location of the call `@test_is_one 2` to the user,
rather than line 2 where `@test` is used as an implementation detail.
"""
function replace_sourceloc!(sourceloc, @nospecialize(ex))
    if ex isa Expr
        if ex.head === :macrocall
            ex.args[2] = sourceloc
        end
        map!(e -> replace_sourceloc!(sourceloc, e), ex.args, ex.args)
    end
    return ex
end


"""
    Meta.show_sexpr([io::IO,], ex)

Show expression `ex` as a lisp style S-expression.

# Examples
```jldoctest
julia> Meta.show_sexpr(:(f(x, g(y,z))))
(:call, :f, :x, (:call, :g, :y, :z))
```
"""
show_sexpr(ex) = show_sexpr(stdout, ex)
show_sexpr(io::IO, ex) = show_sexpr(io, ex, 0)
show_sexpr(io::IO, ex, indent::Int) = show(io, ex)

const sexpr_indent_width = 2

function show_sexpr(io::IO, ex::QuoteNode, indent::Int)
    inner = indent + sexpr_indent_width
    print(io, "(:quote, #QuoteNode\n", " "^inner)
    show_sexpr(io, ex.value, inner)
    print(io, '\n', " "^indent, ')')
end
function show_sexpr(io::IO, ex::Expr, indent::Int)
    inner = indent + sexpr_indent_width
    print(io, '(')
    show_sexpr(io, ex.head, inner)
    for arg in ex.args
        print(io, ex.head === :block ? ",\n"*" "^inner : ", ")
        show_sexpr(io, arg, inner)
    end
    if isempty(ex.args)
        print(io, ",)")
    else
        print(io, (ex.head === :block ? "\n"*" "^indent : ""), ')')
    end
end

"""
    @dump expr

Show every part of the representation of the given expression. Equivalent to
[`dump(:(expr))`](@ref dump).
"""
macro dump(expr)
    return :(dump($(QuoteNode(expr))))
end

"""
    lower(m, x)

Takes the expression `x` and returns an equivalent expression in lowered form
for executing in module `m`.
See also [`code_lowered`](@ref).
"""
lower(m::Module, @nospecialize(x)) = ccall(:jl_expand, Any, (Any, Any), x, m)

"""
    @lower [m] x

Return lowered form of the expression `x` in module `m`.
By default `m` is the module in which the macro is called.
See also [`lower`](@ref).
"""
macro lower(code)
    return :(lower($__module__, $(QuoteNode(code))))
end
macro lower(mod, code)
    return :(lower($(esc(mod)), $(QuoteNode(code))))
end


## interface to parser ##

"""
    ParseError(msg)

The expression passed to the [`parse`](@ref) function could not be interpreted as a valid Julia
expression.
"""
struct ParseError <: Exception
    msg::String
end

function _parse_string(text::AbstractString, filename::AbstractString,
                       lineno::Integer, index::Integer, options)
    if index < 1 || index > ncodeunits(text) + 1
        throw(BoundsError(text, index))
    end
    ex, offset::Int = Core._parse(text, filename, lineno, index-1, options)
    ex, offset+1
end

"""
    parse(str, start; greedy=true, raise=true, depwarn=true)

Parse the expression string and return an expression (which could later be
passed to eval for execution). `start` is the code unit index into `str` of the
first character to start parsing at (as with all string indexing, these are not
character indices). If `greedy` is `true` (default), `parse` will try to consume
as much input as it can; otherwise, it will stop as soon as it has parsed a
valid expression. Incomplete but otherwise syntactically valid expressions will
return `Expr(:incomplete, "(error message)")`. If `raise` is `true` (default),
syntax errors other than incomplete expressions will raise an error. If `raise`
is `false`, `parse` will return an expression that will raise an error upon
evaluation. If `depwarn` is `false`, deprecation warnings will be suppressed.

```jldoctest
julia> Meta.parse("(α, β) = 3, 5", 1) # start of string
(:((α, β) = (3, 5)), 16)

julia> Meta.parse("(α, β) = 3, 5", 1, greedy=false)
(:((α, β)), 9)

julia> Meta.parse("(α, β) = 3, 5", 16) # end of string
(nothing, 16)

julia> Meta.parse("(α, β) = 3, 5", 11) # index of 3
(:((3, 5)), 16)

julia> Meta.parse("(α, β) = 3, 5", 11, greedy=false)
(3, 13)
```
"""
function parse(str::AbstractString, pos::Integer; greedy::Bool=true, raise::Bool=true,
               depwarn::Bool=true)
    ex, pos = _parse_string(str, "none", 1, pos, greedy ? :statement : :atom)
    if raise && isa(ex,Expr) && ex.head === :error
        throw(ParseError(ex.args[1]))
    end
    return ex, pos
end

"""
    parse(str; raise=true, depwarn=true)

Parse the expression string greedily, returning a single expression. An error is thrown if
there are additional characters after the first expression. If `raise` is `true` (default),
syntax errors will raise an error; otherwise, `parse` will return an expression that will
raise an error upon evaluation.  If `depwarn` is `false`, deprecation warnings will be
suppressed.

```jldoctest
julia> Meta.parse("x = 3")
:(x = 3)

julia> Meta.parse("x = ")
:($(Expr(:incomplete, "incomplete: premature end of input")))

julia> Meta.parse("1.0.2")
ERROR: Base.Meta.ParseError("invalid numeric constant \\\"1.0.\\\"")
Stacktrace:
[...]

julia> Meta.parse("1.0.2"; raise = false)
:($(Expr(:error, "invalid numeric constant \"1.0.\"")))
```
"""
function parse(str::AbstractString; raise::Bool=true, depwarn::Bool=true)
    ex, pos = parse(str, 1, greedy=true, raise=raise, depwarn=depwarn)
    if isa(ex,Expr) && ex.head === :error
        return ex
    end
    if pos <= ncodeunits(str)
        raise && throw(ParseError("extra token after end of expression"))
        return Expr(:error, "extra token after end of expression")
    end
    return ex
end

function parseatom(text::AbstractString, pos::Integer; filename="none", lineno=1)
    return _parse_string(text, String(filename), lineno, pos, :atom)
end

function parseall(text::AbstractString; filename="none", lineno=1)
    ex,_ = _parse_string(text, String(filename), lineno, 1, :all)
    return ex
end

"""
    partially_inline!(code::Vector{Any}, slot_replacements::Vector{Any},
                      type_signature::Type{<:Tuple}, static_param_values::Vector{Any},
                      slot_offset::Int, statement_offset::Int,
                      boundscheck::Symbol)

Return `code` after performing an in-place partial inlining pass on the Julia IR stored
within it.

The kind of inlining transformations performed by this function are those that are generally
possible given only a runtime type signature for a method invocation and the corresponding
method's lowered IR. Thus, this function is mainly useful when preparing Julia IR to be
emitted from a `@generated` function.

The performed transformations are:

- replace slot numbers in the range `1:length(slot_replacements)` with the corresponding items in `slot_replacements`
- increment other slot numbers by `slot_offset`
- substitute static parameter placeholders (e.g. `Expr(:static_parameter, 1)`) with the corresponding
values in `static_param_values`
- increment any statement indices present in the IR (`GotoNode`s, `SSAValue`s, etc.) by `statement_offset`
(useful when the caller plans to prepend new statements to the IR)
- turn off boundschecking (if `boundscheck === :off`) or propagate boundschecking (if `boundscheck === :propagate`)

This function is similar to `Core.Compiler.ssa_substitute!`, but works on pre-type-inference
IR instead of the optimizer's IR.
"""
function partially_inline!(code::Vector{Any}, slot_replacements::Vector{Any},
                           @nospecialize(type_signature)#=::Type{<:Tuple}=#,
                           static_param_values::Vector{Any},
                           slot_offset::Int, statement_offset::Int,
                           boundscheck::Symbol)
    for i = 1:length(code)
        isassigned(code, i) || continue
        code[i] = _partially_inline!(code[i], slot_replacements, type_signature,
                                     static_param_values, slot_offset,
                                     statement_offset, boundscheck)
    end
    return code
end

function _partially_inline!(@nospecialize(x), slot_replacements::Vector{Any},
                            @nospecialize(type_signature), static_param_values::Vector{Any},
                            slot_offset::Int, statement_offset::Int,
                            boundscheck::Symbol)
    if isa(x, Core.SSAValue)
        return Core.SSAValue(x.id + statement_offset)
    end
    if isa(x, Core.GotoNode)
        return Core.GotoNode(x.label + statement_offset)
    end
    if isa(x, Core.SlotNumber)
        id = x.id
        if 1 <= id <= length(slot_replacements)
            return slot_replacements[id]
        end
        return Core.SlotNumber(id + slot_offset)
    end
    if isa(x, Core.NewvarNode)
        return Core.NewvarNode(_partially_inline!(x.slot, slot_replacements, type_signature,
                                                  static_param_values, slot_offset,
                                                  statement_offset, boundscheck))
    end
    if isa(x, Core.PhiNode)
        partially_inline!(x.values, slot_replacements, type_signature, static_param_values,
                          slot_offset, statement_offset, boundscheck)
        x.edges .+= slot_offset
        return x
    end
    if isa(x, Core.ReturnNode)
        return Core.ReturnNode(
            _partially_inline!(x.val, slot_replacements, type_signature, static_param_values,
                               slot_offset, statement_offset, boundscheck),
        )
    end
    if isa(x, Core.GotoIfNot)
        return Core.GotoIfNot(
            _partially_inline!(x.cond, slot_replacements, type_signature, static_param_values,
                               slot_offset, statement_offset, boundscheck),
            x.dest + statement_offset,
        )
    end
    if isa(x, Expr)
        head = x.head
        if head === :static_parameter
            if isassigned(static_param_values, x.args[1])
                return QuoteNode(static_param_values[x.args[1]])
            end
            return x
        elseif head === :cfunction
            @assert !isa(type_signature, UnionAll) || !isempty(spvals)
            if !isa(x.args[2], QuoteNode) # very common no-op
                x.args[2] = _partially_inline!(x.args[2], slot_replacements, type_signature,
                                               static_param_values, slot_offset,
                                               statement_offset, boundscheck)
            end
            x.args[3] = _instantiate_type_in_env(x.args[3], type_signature, static_param_values)
            x.args[4] = Core.svec(Any[_instantiate_type_in_env(argt, type_signature, static_param_values) for argt in x.args[4]]...)
        elseif head === :foreigncall
            @assert !isa(type_signature, UnionAll) || !isempty(static_param_values)
            for i = 1:length(x.args)
                if i == 2
                    x.args[2] = _instantiate_type_in_env(x.args[2], type_signature, static_param_values)
                elseif i == 3
                    x.args[3] = Core.svec(Any[_instantiate_type_in_env(argt, type_signature, static_param_values) for argt in x.args[3]]...)
                elseif i == 4
                    @assert isa(x.args[4], Int)
                elseif i == 5
                    @assert isa((x.args[5]::QuoteNode).value, Union{Symbol, Tuple{Symbol, UInt8}})
                else
                    x.args[i] = _partially_inline!(x.args[i], slot_replacements,
                                                   type_signature, static_param_values,
                                                   slot_offset, statement_offset,
                                                   boundscheck)
                end
            end
        elseif head === :boundscheck
            if boundscheck === :propagate
                return x
            elseif boundscheck === :off
                return false
            else
                return true
            end
        elseif head === :gotoifnot
            x.args[1] = _partially_inline!(x.args[1], slot_replacements, type_signature,
                                           static_param_values, slot_offset,
                                           statement_offset, boundscheck)
            x.args[2] += statement_offset
        elseif head === :enter
            x.args[1] += statement_offset
        elseif head === :isdefined
            arg = x.args[1]
            # inlining a QuoteNode or literal into `Expr(:isdefined, x)` is invalid, replace with true
            if isa(arg, Core.SlotNumber)
                id = arg.id
                if 1 <= id <= length(slot_replacements)
                    replacement = slot_replacements[id]
                    if isa(replacement, Union{Core.SlotNumber, GlobalRef, Symbol})
                        return Expr(:isdefined, replacement)
                    else
                        @assert !isa(replacement, Expr)
                        return true
                    end
                end
                return Expr(:isdefined, Core.SlotNumber(id + slot_offset))
            elseif isexpr(arg, :static_parameter)
                if isassigned(static_param_values, arg.args[1])
                    return true
                end
                return x
            else
                @assert isa(arg, Union{GlobalRef, Symbol})
                return x
            end
        elseif !Core.Compiler.is_meta_expr_head(head)
            partially_inline!(x.args, slot_replacements, type_signature, static_param_values,
                              slot_offset, statement_offset, boundscheck)
        end
    end
    return x
end

_instantiate_type_in_env(x, spsig, spvals) = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), x, spsig, spvals)

end # module
