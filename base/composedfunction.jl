# This file is a part of Julia. License is MIT: https://julialang.org/license

# function composition

"""
    f ∘ g

Compose functions: i.e. `(f ∘ g)(args...; kwargs...)` means `f(g(args...; kwargs...))`. The `∘` symbol can be
entered in the Julia REPL (and most editors, appropriately configured) by typing `\\circ<tab>`.

Function composition also works in prefix form: `∘(f, g)` is the same as `f ∘ g`.
The prefix form supports composition of multiple functions: `∘(f, g, h) = f ∘ g ∘ h`
and splatting `∘(fs...)` for composing an iterable collection of functions.

!!! compat "Julia 1.4"
    Multiple function composition requires at least Julia 1.4.

!!! compat "Julia 1.5"
    Composition of one function ∘(f) requires at least Julia 1.5.

!!! compat "Julia 1.7"
    Using keyword arguments requires at least Julia 1.7.

# Examples
```jldoctest
julia> map(uppercase∘first, ["apple", "banana", "carrot"])
3-element Vector{Char}:
 'A': ASCII/Unicode U+0041 (category Lu: Letter, uppercase)
 'B': ASCII/Unicode U+0042 (category Lu: Letter, uppercase)
 'C': ASCII/Unicode U+0043 (category Lu: Letter, uppercase)

julia> fs = [
           x -> 2x
           x -> x/2
           x -> x-1
           x -> x+1
       ];

julia> ∘(fs...)(3)
3.0
```
See also [`ComposedFunction`](@ref), [`!f::Function`](@ref).
"""
function ∘ end

"""
    ComposedFunction{Outer,Inner} <: Function

Represents the composition of two callable objects `outer::Outer` and `inner::Inner`. That is
```julia
ComposedFunction(outer, inner)(args...; kw...) === outer(inner(args...; kw...))
```
The preferred way to construct instance of `ComposedFunction` is to use the composition operator [`∘`](@ref):
```jldoctest
julia> sin ∘ cos === ComposedFunction(sin, cos)
true

julia> typeof(sin∘cos)
ComposedFunction{typeof(sin), typeof(cos)}
```
The composed pieces are stored in the fields of `ComposedFunction` and can be retrieved as follows:
```jldoctest
julia> composition = sin ∘ cos
sin ∘ cos

julia> composition.outer === sin
true

julia> composition.inner === cos
true
```
!!! compat "Julia 1.6"
    ComposedFunction requires at least Julia 1.6. In earlier versions `∘` returns an anonymous function instead.

See also [`∘`](@ref).
"""
struct ComposedFunction{O,I} <: Function
    outer::O
    inner::I
    ComposedFunction{O, I}(outer, inner) where {O, I} = new{O, I}(outer, inner)
    ComposedFunction(outer, inner) = new{Core.Typeof(outer),Core.Typeof(inner)}(outer, inner)
end

(c::ComposedFunction)(x...; kw...) = call_composed(unwrap_composed(c), x, kw)
@assume_effects :terminates_globally unwrap_composed(c::ComposedFunction) =
    (unwrap_composed(c.outer)..., unwrap_composed(c.inner)...)
unwrap_composed(c) = (maybeconstructor(c),)
call_composed(fs, x, kw) = (@inline; fs[1](call_composed(tail(fs), x, kw)))
call_composed(fs::Tuple{Any}, x, kw) = fs[1](x...; kw...)

struct Constructor{F} <: Function end
(::Constructor{F})(args...; kw...) where {F} = (@inline; F(args...; kw...))
maybeconstructor(::Type{F}) where {F} = Constructor{F}()
maybeconstructor(f) = f

∘(f) = f
∘(f, g) = ComposedFunction(f, g)
∘(f, g, h...) = ∘(f ∘ g, h...)

function show(io::IO, c::ComposedFunction)
    c.outer isa ComposedFunction ? show(io, c.outer) : _showcomposed(io, c.outer)
    print(io, " ∘ ")
    _showcomposed(io, c.inner)
end

#shows !f instead of (!) ∘ f when ! is the outermost function
function show(io::IO, c::ComposedFunction{typeof(!)})
    print(io, '!')
    _showcomposed(io, c.inner)
end

_showcomposed(io::IO, x) = show(io, x)
#display operators like + and - inside parens
_showcomposed(io::IO, f::Function) = isoperator(Symbol(f)) ? (print(io, '('); show(io, f); print(io, ')')) : show(io, f)
#nesting for chained composition
_showcomposed(io::IO, f::ComposedFunction) = (print(io, '('); show(io, f); print(io, ')'))
#no nesting when ! is the outer function in a composition chain
_showcomposed(io::IO, f::ComposedFunction{typeof(!)}) = show(io, f)

"""
    !f::Function

Predicate function negation: when the argument of `!` is a function, it returns a composed function which computes the boolean negation of `f`.

See also [`∘`](@ref).

# Examples
```jldoctest
julia> str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
"∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

julia> filter(isletter, str)
"εδxyδfxfyε"

julia> filter(!isletter, str)
"∀  > 0, ∃  > 0: |-| <  ⇒ |()-()| < "
```

!!! compat "Julia 1.9"
    Starting with Julia 1.9, `!f` returns a [`ComposedFunction`](@ref) instead of an anonymous function.
"""
!(f::Function) = (!) ∘ f
!(f::ComposedFunction{typeof(!)}) = f.inner #allows !!f === f
