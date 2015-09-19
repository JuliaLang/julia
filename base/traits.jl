# This just adds a few convenience functions & macros around Holy Traits.

export istrait, trait, @traitdef, @traitimpl, @traitfn, Not, Trait
const curmod = module_name(current_module())

"""
All Traits are subtypes of abstract type Trait.  (SUPER is not used
here but in Traits.jl)
"""
abstract Trait{SUPER}
# a concrete Trait will look like
# immutable Tr1{X,Y} <: Trait end
# where X and Y are the types involved in the trait.

"""
The set of all types not belonging to a trait is encoded by wrapping
it with Not{}, e.g.  Not{Tr1{X,Y}}
"""
abstract Not{T<:Trait} <: Trait

# Helper to strip an even number of Not{}s off: Not{Not{T}}->T
stripNot{T<:Trait}(::Type{T}) = T
stripNot{T<:Trait}(::Type{Not{T}}) = Not{T}
stripNot{T<:Trait}(::Type{Not{Not{T}}}) = stripNot(T)

"""
A trait is defined as full filled if this function is the identity function for that trait.
Otherwise it returns the trait wrapped in `Not`.

Example:
```
trait(IsBits{Int}) # returns IsBits{Int}
trait(IsBits{Array}) # returns Not{IsBits{Array}}
```

Instead of using `@traitimpl` one can define a method for `trait` to
implement a trait.  If this uses `@generated` functions it will be
in-lined away.  For example the `IsBits` trait is defined by:
```
@traitdef IsBits{X}
@generated trait{X}(::Type{IsBits{X}}) =
    isbits(X) ? :(IsBits{X}) : :(Not{IsBits{X}})
```
"""
trait{T<:Trait}(::Type{T}) = Not{T}
trait{T<:Trait}(::Type{Not{T}}) = trait(T)

## Under the hood, a trait is then implemented for specific types by
## defining:
#   trait(::Type{Tr1{Int,Float64}}) = Tr1{Int,Float64}
# or
#   trait{I<:Integer,F<:AbstractFloat}(::Type{Tr1{I,F}}) = Tr1{I,F}

"""
This function checks whether a trait is fulfilled by a specific
set of types.
```
istrait(Tr1{Int,Float64}) => return true or false
```
"""
istrait(::Any) = error("Argument is not a Trait.")
istrait{T<:Trait}(tr::Type{T}) = trait(tr)==stripNot(tr) ? true : false # Problem, this can run into issue #265
                                                                        # thus is redefine when traits are defined
"""
Used to define a trait.  Traits, like types, are camel cased.
Often they start with `Is` or `Has`.

Examples:
```
@traitdef IsFast{X}
@traitdef IsSlow{X,Y}
```
"""
macro traitdef(tr)
    :(immutable $(esc(tr)) <: Trait end)
end

# # helper as not available during bootstrap:
# function _dec(x::Unsigned, pad::Int, neg::Bool)
#     i = neg + max(pad,ndigits0z(x))
#     a = Array(UInt8,i)
#     while i > neg
#         a[i] = '0'+rem(x,10)
#         x = oftype(x,div(x,10))
#         i -= 1
#     end
#     if neg; a[1]='-'; end
#     ASCIIString(a)
# end

# A helper function to insert several args into an expression as this
# does not work early in bootstrap:
# :(T{$(ar...)})
#
# Use instead:
# tmp = :(T{})
# addtoargs!(tmp, ar)
function addtoargs!(e::Expr, ar::Array{Any,1})
    n1 = arraylen(e.args)
    n2 = arraylen(ar)
    tmp = Array(Any,n1+n2)
    for i=1:n1
        tmp[i] = e.args[i]
    end
    for i=n1+1:n1+n2
        tmp[i] = ar[i-n1]
    end
    e.args = tmp
    return e
end
getindex(A::Array, i1::Real) = arrayref(A, to_index(i1))

# TODO: do this more generally?
const syms = (:X1,:X2,:X3,:X4,:X5,:X6,:X7,:X8,:X9,:X10,:X11,:X12,:X13,:X14,
              :X15,:X16,:X17,:X18,:X19,:X20,:X21,:X22,:X23,:X24,:X25,:X26,
              :X27,:X28,:X29,:X30,:X31,:X32,:X33,:X34,:X35,:X36,:X37,:X38,
              :X39,:X40,:X41,:X42,:X43,:X44,:X45,:X46,:X47,:X48,:X49,:X50,
              :X51,:X52,:X53,:X54,:X55,:X56,:X57,:X58,:X59,:X60,:X61,:X62,
              :X63,:X64,:X65,:X66,:X67,:X68,:X69,:X70,:X71,:X72,:X73,:X74,
              :X75,:X76,:X77,:X78,:X79,:X80,:X81,:X82,:X83,:X84,:X85,:X86,
              :X87,:X88,:X89,:X90,:X91,:X92,:X93,:X94,:X95,:X96,:X97,:X98,:X99,:X100)

"""
Used to add a type or type-tuple to a trait.  By default a type does
not belong to a trait.

Example:
```
@traitdef IsFast{X}
@traitimpl IsFast{Array{Int,1}}
```
"""
macro traitimpl(tr)
    # makes
    # trait{X1<:Int,X2<:Float64}(::Type{Tr1{X1,X2}}) = Tr1{X1,X2}
    n = arraylen(tr.args)-1
    typs = [tr.args[i] for i=2:n+1]
    trname = esc(tr.args[1])
    curly = Array(Any,n)
    paras = Array(Any,n)
    for i=1:n
        ty = typs[i]
        v = syms[i]
        curly[i] = Expr(:(<:), esc(v), esc(ty))  #:($v<:$ty)
        paras[i] = esc(v)
    end
    # some shenanigans needed for bootstrap:
    # arg = :(::Type{$trname{$(paras...)}})
    tmp = :($trname{})
    addtoargs!(tmp, paras)
    arg = :(::Type{$tmp})
    # fnhead = :($curmod.trait{$(curly...)}($arg))
    tmp = :($curmod.trait{})
    addtoargs!(tmp, curly)
    fnhead = :($tmp($arg))
    # isfnhead = :($curmod.istrait{$(curly...)}($arg))
    tmp = :($curmod.istrait{})
    addtoargs!(tmp, curly)
    isfnhead = :($tmp($arg))
    # tmp = $trname{$(paras...)}
    tmp = :($trname{})
    addtoargs!(tmp, paras)
    quote
        $fnhead = $tmp
        $isfnhead = true # Add the istrait definition as otherwise
                         # method-caching can be an issue.
    end
end

"""
Defines a function dispatching on a trait:
```
@traitfn f{X,Y;  Tr1{X,Y}}(x::X,y::Y) = ...
@traitfn f{X,Y; !Tr1{X,Y}}(x::X,y::Y) = ... # which is just sugar for:
@traitfn f{X,Y; Not{Tr1{X,Y}}}(x::X,y::Y) = ...
```
"""
macro traitfn(tfn)
    # Need
    # f{X,Y}(x::X,Y::Y) = f(trait(Tr1{X,Y}), x, y)
    # f(::False, x, y)= ...
    if tfn.head==:macrocall
        # if it is
        # @traitfn @generated f{X,Y;  Tr1{X,Y}}(x::X,y::Y) = ...
        hasmac = true
        mac = tfn.args[1]
        tfn = tfn.args[2]
    else
        hasmac = false
    end
    fhead = tfn.args[1]
    fbody = tfn.args[2]
    fname = fhead.args[1].args[1]
    args = [fhead.args[i] for i=2:arraylen(fhead.args)]
    typs = [fhead.args[1].args[i] for i=3:arraylen(fhead.args[1].args)]
    trait = fhead.args[1].args[2].args[1]
    if isnegated(trait)
        trait = trait.args[2]
        val = :(::Type{$curmod.Not{$trait}})
    else
        val = :(::Type{$trait})
    end

    # The wrapper function:
    # fwrap = $fname{$(typs...)}($(args...)) = (@_inline_meta(); $fname($curmod.trait($trait), $(striparg(args)...)))
    #         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    #              tmp1                                                  tmp2
    tmp1 = :($fname{})
    addtoargs!(tmp1, typs)
    tmp1 = :($tmp1())
    addtoargs!(tmp1, args)
    tmp2 = :($fname($curmod.trait($trait)))
    addtoargs!(tmp2, stripType(striparg(args)))
    fwrap = :($tmp1 = ($curmod.@_inline_meta(); $tmp2))

    # The function containing the logic
    if !hasmac
        # fn = :($fname{$(typs...)}($val, $(args...)) = (@_inline_meta(); $fbody))
        #        ^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^
        #           tmp1                   tmp2
        tmp1 = :($fname{})
        addtoargs!(tmp1, typs)
        tmp2 = :($tmp1($val,))
        addtoargs!(tmp2, args)
        fn = :($tmp2 = ($curmod.@_inline_meta(); $fbody))
    else
        # during bootstrap it's just on possible to insert that darned
        # macrocall any simpler than this:
           fn = :(@dummy function $fname{}($val)
                   $curmod.@_inline_meta()
                   $fbody
               end)
        fn.args[1] = mac
        addtoargs!(fn.args[2].args[1].args[1], typs)
        addtoargs!(fn.args[2].args[1], args)
    end
    esc(quote
        $fwrap
        $fn
    end)
end

######
## Helpers
######

# true if :(!(Tr{x}))
isnegated(t::Expr) = t.head==:call

# [:(x::X)] -> [:x]
striparg{T}(args::Array{T,1}) = [striparg(args[i]) for i=1:arraylen(args)]
striparg(a::Symbol) = a
striparg(a::Expr) = a.args[1]

# :(Type{X}) -> X, X->X
stripType{T}(args::Array{T,1}) = [stripType(args[i]) for i=1:arraylen(args)]
stripType(a::Symbol) = a
stripType(a::Expr) = (a.head==:curly && a.args[1]==:Type) ? a.args[2] : a

####
# Some trait definitions
####
export IsAnything, IsNothing, IsCallable

"Trait which contains all types"
@traitdef IsAnything{X}
trait{X}(::Type{IsAnything{X}}) = IsAnything{X}
"Trait which contains no types"
typealias IsNothing{X} Not{IsAnything{X}}

#"Trait of all callable objects"
@traitdef IsCallable{X}
@generated function trait{X}(::Type{IsCallable{X}})
    if X==Function ||  length(methods(call, (X,Vararg)))>0
        return IsCallable{X}
    else
        return Not{IsCallable{X}}
    end
end

"Trait of all isbits-types"
@traitdef IsBits{X}
@generated function trait{X}(::Type{IsBits{X}})
    isbits(X) ? :(IsBits{X}) : :(Not{IsBits{X}})
end

"Trait of all immutable types"
@traitdef IsImmutable{X}
@generated function trait{X}(::Type{IsImmutable{X}})
    X.mutable ? :(Not{IsImmutable{X}}) : :(IsImmutable{X})
end

"Trait of all leaf types types"
@traitdef IsLeafType{X}
@generated function trait{X}(::Type{IsLeafType{X}})
    X.mutable ? :(Not{IsLeafType{X}}) : :(IsLeafType{X})
end
