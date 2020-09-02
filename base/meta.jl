# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Convenience functions for metaprogramming.
"""
module Meta

using ..CoreLogging

export quot,
       isexpr,
       show_sexpr,
       @dump

"""
    Meta.quot(ex)::Expr

Quote expression `ex` to produce an expression with head `quote`. This can for instance be used to represent objects of type `Expr` in the AST.
See also the manual section about [QuoteNode](@ref man-quote-node).

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

Check if `ex` is an expression with head `head` and `n` arguments.

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
isexpr(@nospecialize(ex), head::Symbol) = isa(ex, Expr) && ex.head === head
isexpr(@nospecialize(ex), heads) = isa(ex, Expr) && in(ex.head, heads)
isexpr(@nospecialize(ex), head::Symbol, n::Int) = isa(ex, Expr) && ex.head === head && length(ex.args) == n
isexpr(@nospecialize(ex), heads, n::Int) = isa(ex, Expr) && in(ex.head, heads) && length(ex.args) == n

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
    msg::AbstractString
end

function _parse_string(text::AbstractString, filename::AbstractString,
                       index::Integer, options)
    if index < 1 || index > ncodeunits(text) + 1
        throw(BoundsError(text, index))
    end
    ex, offset::Int = Core._parse(text, filename, index-1, options)
    ex, offset+1
end

"""
    parse(str, start; greedy=true, raise=true, depwarn=true)

Parse the expression string and return an expression (which could later be passed to eval
for execution). `start` is the index of the first character to start parsing. If `greedy` is
`true` (default), `parse` will try to consume as much input as it can; otherwise, it will
stop as soon as it has parsed a valid expression. Incomplete but otherwise syntactically
valid expressions will return `Expr(:incomplete, "(error message)")`. If `raise` is `true`
(default), syntax errors other than incomplete expressions will raise an error. If `raise`
is `false`, `parse` will return an expression that will raise an error upon evaluation. If
`depwarn` is `false`, deprecation warnings will be suppressed.

```jldoctest
julia> Meta.parse("x = 3, y = 5", 7)
(:(y = 5), 13)

julia> Meta.parse("x = 3, y = 5", 5)
(:((3, y) = 5), 13)
```
"""
function parse(str::AbstractString, pos::Integer; greedy::Bool=true, raise::Bool=true,
               depwarn::Bool=true)
    ex, pos = _parse_string(str, "none", pos, greedy ? :statement : :atom)
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

function parseatom(text::AbstractString, pos::Integer; filename="none")
    return _parse_string(text, filename, pos, :atom)
end

function parseall(text::AbstractString; filename="none")
    ex,_ = _parse_string(text, filename, 1, :all)
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
    if isa(x, Expr)
        head = x.head
        if head === :static_parameter
            return QuoteNode(static_param_values[x.args[1]])
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
                    @assert isa((x.args[5]::QuoteNode).value, Symbol)
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
        elseif !is_meta_expr_head(head)
            partially_inline!(x.args, slot_replacements, type_signature, static_param_values,
                              slot_offset, statement_offset, boundscheck)
        end
    end
    return x
end

_instantiate_type_in_env(x, spsig, spvals) = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), x, spsig, spvals)

is_meta_expr_head(head::Symbol) = (head === :inbounds || head === :boundscheck || head === :meta || head === :loopinfo)

end # module
