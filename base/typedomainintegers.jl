# This file is a part of Julia. License is MIT: https://julialang.org/license

baremodule TypeDomainIntegers
    baremodule Basic
        using Base: @nospecialize
        export
            natural_successor, natural_predecessor,
            NonnegativeInteger, PositiveInteger, PositiveIntegerUpperBound,
            with_refined_type, zero
        baremodule RecursiveStep
            using Base: @nospecialize
            export recursive_step
            function recursive_step(@nospecialize t::Type)
                Union{Nothing,t}
            end
        end
        baremodule UpperBounds
            using ..RecursiveStep
            const s = Integer
            abstract type A{P <: recursive_step(s)} <: s    end
            abstract type B{P <: recursive_step(A)} <: A{P} end
        end
        const NonnegativeIntegerUpperBound = UpperBounds.B
        using .RecursiveStep
        struct NonnegativeIntegerImpl{
            Predecessor<:recursive_step(NonnegativeIntegerUpperBound),
        } <: NonnegativeIntegerUpperBound{Predecessor}
            predecessor::Predecessor
            function stricter(t::UnionAll)
                t{P} where {P <: recursive_step(t)}
            end
            global const NonnegativeIntegerImplTighter = stricter(NonnegativeIntegerImpl)
            global const NonnegativeInteger = stricter(NonnegativeIntegerImplTighter)
            global const PositiveInteger = let t = NonnegativeIntegerImplTighter
                t{P} where {P <: t}
            end
            global const PositiveIntegerUpperBound = let t = UpperBounds.A
                t{P} where {P <: t}
            end
            global function new_nonnegative_integer(p::P) where {P<:recursive_step(NonnegativeInteger)}
                t_p = P::DataType
                r = new{t_p}(p)
                with_refined_type(r)
            end
        end
        function with_refined_type(@nospecialize r::NonnegativeInteger)
            r
        end
        function natural_successor(o::NonnegativeInteger)
            new_nonnegative_integer(o)::PositiveInteger
        end
        function natural_predecessor(o::PositiveInteger)
            r = o.predecessor
            with_refined_type(r)
        end
        function zero()
            new_nonnegative_integer(nothing)
        end
    end

    baremodule LazyMinus
        using ..Basic
        using Base: @nospecialize
        export NegativeInteger, TypeDomainInteger, negated
        struct NegativeInteger{X<:PositiveInteger} <: Integer
            x::X
            global function new_negative_integer(x::X) where {X<:PositiveInteger}
                new{X}(x)
            end
        end
        const TypeDomainInteger = Union{NonnegativeInteger,NegativeInteger}
        function negated(@nospecialize n::TypeDomainInteger)
            if n isa NegativeInteger
                n.x
            else
                n = n::NonnegativeInteger
                if n isa PositiveIntegerUpperBound
                    new_negative_integer(n)
                else
                    n
                end
            end
        end
    end

    baremodule RecursiveAlgorithms
        using ..Basic, ..LazyMinus
        using Base: !, +, -, <, @assume_effects, @inline, @nospecialize
        export subtracted, added, to_int, from_int, is_even, multiplied, is_less
        @assume_effects :foldable function is_less((@nospecialize l::NonnegativeInteger), @nospecialize r::NonnegativeInteger)
            if r isa PositiveIntegerUpperBound
                if l isa PositiveIntegerUpperBound
                    let pl = natural_predecessor(l), pr = natural_predecessor(r), res = @inline is_less(pl, pr)
                        res::Bool
                    end
                else
                    true
                end
            else
                false
            end
        end
        @assume_effects :foldable function subtracted((@nospecialize l::NonnegativeInteger), @nospecialize r::NonnegativeInteger)
            l_pos = l isa PositiveIntegerUpperBound
            if r isa PositiveIntegerUpperBound
                if l_pos
                    let a = natural_predecessor(l), b = natural_predecessor(r), ret = @inline subtracted(a, b)
                        ret::TypeDomainInteger
                    end
                else
                    negated(r)
                end
            else
                if l_pos
                    l::PositiveInteger
                else
                    zero()
                end
            end
        end
        @assume_effects :foldable function added((@nospecialize l::NonnegativeInteger), @nospecialize r::NonnegativeInteger)
            ret = if r isa PositiveIntegerUpperBound
                let a = natural_successor(l), b = natural_predecessor(r)
                    @inline added(a, b)
                end
            else
                l
            end
            with_refined_type(ret)
        end
        @assume_effects :foldable function to_int(@nospecialize o::NonnegativeInteger)
            if o isa PositiveIntegerUpperBound
                let p = natural_predecessor(o), t = @inline to_int(p)
                    t::Int + 1
                end
            else
                0
            end::Int
        end
        struct ConvertNaturalToNegativeException <: Exception end
        @assume_effects :foldable function from_int(n::Int)
            if n < 0
                throw(ConvertNaturalToNegativeException())
            end
            ret = if n === 0
                zero()
            else
                let v = n - 1, p = @inline from_int(v)
                    p = with_refined_type(p)
                    natural_successor(p)
                end
            end
            with_refined_type(ret)
        end
        @assume_effects :foldable function is_even(@nospecialize o::NonnegativeInteger)
            if o isa PositiveIntegerUpperBound
                let p = natural_predecessor(o)
                    if p isa PositiveIntegerUpperBound
                        let s = natural_predecessor(p), r = @inline is_even(s)
                            r::Bool
                        end
                    else
                        false
                    end
                end
            else
                true
            end
        end
        @assume_effects :foldable function multiplied((@nospecialize l::NonnegativeInteger), @nospecialize r::NonnegativeInteger)
            if r isa PositiveIntegerUpperBound
                let p = natural_predecessor(r), x = @inline multiplied(l, p)
                    added(x, l)
                end
            else
                zero()
            end
        end
    end

    baremodule BaseOverloads
        using ..Basic, ..RecursiveAlgorithms, ..LazyMinus
        using Base: Base, convert, <, +, -, *, ==, isequal, isless, !, @nospecialize
        function Base.zero(@nospecialize unused::Type{<:TypeDomainInteger})
            zero()
        end
        function Base.zero(@nospecialize unused::TypeDomainInteger)
            zero()
        end
        function Base.iszero(@nospecialize n::TypeDomainInteger)
            if n isa NegativeInteger
                false
            else
                n = with_refined_type(n)
                if n isa PositiveIntegerUpperBound
                    false
                else
                    true
                end
            end
        end
        const ZeroOrOne = Union{typeof(zero()),typeof(natural_successor(zero()))}
        function to_bool(@nospecialize n::ZeroOrOne)
            if n isa PositiveIntegerUpperBound
                true
            else
                false
            end
        end
        function from_bool(b::Bool)
            z = zero()
            if b
                natural_successor(z)
            else
                z
            end
        end
        const TypeDomainIntegerType = Union{Type{TypeDomainInteger},Type{NonnegativeInteger}}
        function Base.convert(::Type{Bool}, @nospecialize o::ZeroOrOne)
            to_bool(o)
        end
        function Base.convert((@nospecialize unused::TypeDomainIntegerType), n::Bool)
            from_bool(n)
        end
        function Base.convert(::Type{Int}, @nospecialize o::TypeDomainInteger)
            if o isa NegativeInteger
                -to_int(negated(o))
            else
                o = o::NonnegativeInteger
                to_int(o)
            end
        end
        function Base.convert(::Type{NonnegativeInteger}, n::Int)
            from_int(n)
        end
        function Base.convert(::Type{TypeDomainInteger}, n::Int)
            if n < 0
                negated(from_int(-n))
            else
                from_int(n)
            end
        end
        function NonnegativeInteger(n::Union{Bool,Int})
            convert(NonnegativeInteger, n)
        end
        function TypeDomainInteger(n::Union{Bool,Int})
            convert(TypeDomainInteger, n)
        end
        function Bool(@nospecialize n::ZeroOrOne)
            to_bool(n)
        end
        function Int(@nospecialize n::TypeDomainInteger)
            convert(Int, n)
        end
        function Base.:(-)((@nospecialize l::TypeDomainInteger), @nospecialize r::TypeDomainInteger)
            n = negated(r)
            l + n
        end
        function Base.:(+)((@nospecialize l::TypeDomainInteger), @nospecialize r::TypeDomainInteger)
            l_neg = l isa NegativeInteger
            if r isa NegativeInteger
                if l_neg
                    l = l::NegativeInteger
                    let a = negated(l)::PositiveInteger, b = negated(r)::PositiveInteger, s = added(a, b)
                        negated(s)
                    end
                else
                    l = l::NonnegativeInteger
                    let b = negated(r)::PositiveInteger
                        subtracted(l, b)
                    end
                end
            else
                r = r::NonnegativeInteger
                if l_neg
                    l = l::NegativeInteger
                    let a = negated(l)::PositiveInteger
                        subtracted(r, a)
                    end
                else
                    l = l::NonnegativeInteger
                    added(l, r)
                end
            end
        end
        function Base.propertynames(
            (@nospecialize unused::Union{Basic.NonnegativeIntegerImpl,NegativeInteger}),
            ::Bool = false,
        )
            ()
        end
        function Base.iseven(@nospecialize o::TypeDomainInteger)
            if o isa NegativeInteger
                is_even(negated(o))
            else
                o = o::NonnegativeInteger
                is_even(o)
            end
        end
        function Base.isodd(@nospecialize o::TypeDomainInteger)
            !Base.iseven(o)
        end
        function Base.:(==)((@nospecialize l::TypeDomainInteger), @nospecialize r::TypeDomainInteger)
            l === r
        end
        function Base.isequal((@nospecialize l::TypeDomainInteger), @nospecialize r::TypeDomainInteger)
            l === r
        end
        function Base.:(<)((@nospecialize l::TypeDomainInteger), @nospecialize r::TypeDomainInteger)
            l_neg = l isa NegativeInteger
            if r isa NegativeInteger
                if l_neg
                    l = l::NegativeInteger
                    let a = negated(l)::PositiveInteger, b = negated(r)::PositiveInteger
                        is_less(b, a)
                    end
                else
                    false
                end
            else
                r = r::NonnegativeInteger
                if l_neg
                    true
                else
                    l = l::NonnegativeInteger
                    is_less(l, r)
                end
            end
        end
        function Base.isless((@nospecialize l::TypeDomainInteger), @nospecialize r::TypeDomainInteger)
            l < r
        end
        function Base.:(==)((@nospecialize l::TypeDomainInteger), @nospecialize r::Number)
            i = convert(Int, l)
            i == r
        end
        function Base.isequal((@nospecialize l::TypeDomainInteger), @nospecialize r::Number)
            i = convert(Int, l)
            isequal(i, r)
        end
        function Base.:(<)((@nospecialize l::TypeDomainInteger), @nospecialize r::Real)
            i = convert(Int, l)
            i < r
        end
        function Base.isless((@nospecialize l::TypeDomainInteger), @nospecialize r::Real)
            i = convert(Int, l)
            isless(i, r)
        end
        function Base.:(==)((@nospecialize l::Number), @nospecialize r::TypeDomainInteger)
            i = convert(Int, r)
            l == i
        end
        function Base.isequal((@nospecialize l::Number), @nospecialize r::TypeDomainInteger)
            i = convert(Int, r)
            isequal(l, i)
        end
        function Base.:(<)((@nospecialize l::Real), @nospecialize r::TypeDomainInteger)
            i = convert(Int, r)
            l < i
        end
        function Base.isless((@nospecialize l::Real), @nospecialize r::TypeDomainInteger)
            i = convert(Int, r)
            isless(l, i)
        end
        function Base.one(@nospecialize unused::Type{<:TypeDomainInteger})
            natural_successor(zero())
        end
        function Base.one(@nospecialize unused::TypeDomainInteger)
            natural_successor(zero())
        end
        function Base.isone(@nospecialize n::TypeDomainInteger)
            if n isa NegativeInteger
                false
            else
                n = n::NonnegativeInteger
                if n isa PositiveIntegerUpperBound
                    let p = natural_predecessor(n)
                        if p isa PositiveIntegerUpperBound
                            false
                        else
                            true
                        end
                    end
                else
                    false
                end
            end
        end
        function Base.:(*)((@nospecialize l::TypeDomainInteger), @nospecialize r::TypeDomainInteger)
            l_neg = l isa NegativeInteger
            if r isa NegativeInteger
                if l_neg
                    l = l::NegativeInteger
                    let a = negated(l)::PositiveInteger, b = negated(r)::PositiveInteger
                        multiplied(a, b)::PositiveInteger
                    end
                else
                    l = l::NonnegativeInteger
                    let b = negated(r)::PositiveInteger, m = multiplied(l, b)
                        negated(m)
                    end
                end
            else
                r = r::NonnegativeInteger
                if l_neg
                    l = l::NegativeInteger
                    let a = negated(l)::PositiveInteger, m = multiplied(a, r)
                        negated(m)
                    end
                else
                    l = l::NonnegativeInteger
                    multiplied(l, r)
                end
            end
        end
        function Base.:(+)((@nospecialize l::Number), @nospecialize r::TypeDomainInteger)
            if r isa NegativeInteger
                let pos = negated(r), posm1 = natural_predecessor(pos)
                    if posm1 isa PositiveIntegerUpperBound
                        l - convert(Int, pos)
                    else
                        l - true
                    end
                end
            else
                r = r::NonnegativeInteger
                if r isa PositiveIntegerUpperBound
                    let p = natural_predecessor(r)
                        if p isa PositiveIntegerUpperBound
                            l + convert(Int, r)
                        else
                            l + true
                        end
                    end
                else
                    l
                end
            end
        end
        function Base.:(+)((@nospecialize l::TypeDomainInteger), @nospecialize r::Number)
            # addition is commutative
            r + l
        end
        function Base.:(-)((@nospecialize l::Number), @nospecialize r::TypeDomainInteger)
            l + negated(r)
        end
        function Base.:(-)((@nospecialize l::TypeDomainInteger), @nospecialize r::Number)
            if l isa NegativeInteger
                convert(Int, l) - r
            else
                l = l::NonnegativeInteger
                if l isa PositiveIntegerUpperBound
                    let p = natural_predecessor(l)
                        if p isa PositiveIntegerUpperBound
                            convert(Int, l) - r
                        else
                            true - r
                        end
                    end
                else
                    -r
                end
            end
        end
        function Base.:(*)((@nospecialize l::Number), @nospecialize r::TypeDomainInteger)
            if r isa NegativeInteger
                let pos = negated(r), posm1 = natural_predecessor(pos)
                    if posm1 isa PositiveIntegerUpperBound
                        l * convert(Int, r)
                    else
                        -l
                    end
                end
            else
                r = r::NonnegativeInteger
                if r isa PositiveIntegerUpperBound
                    let p = natural_predecessor(r)
                        if p isa PositiveIntegerUpperBound
                            l * convert(Int, r)
                        else
                            l
                        end
                    end
                else
                    zero()
                end
            end
        end
        function Base.:(*)((@nospecialize l::TypeDomainInteger), @nospecialize r::Number)
            if l isa NegativeInteger
                let pos = negated(l), posm1 = natural_predecessor(pos)
                    if posm1 isa PositiveIntegerUpperBound
                        convert(Int, l) * r
                    else
                        -r
                    end
                end
            else
                l = l::NonnegativeInteger
                if l isa PositiveIntegerUpperBound
                    let p = natural_predecessor(l)
                        if p isa PositiveIntegerUpperBound
                            convert(Int, l) * r
                        else
                            r
                        end
                    end
                else
                    zero()
                end
            end
        end
        function Base.:(-)(@nospecialize n::TypeDomainInteger)
            negated(n)
        end
    end

    export
        natural_successor, natural_predecessor, NonnegativeInteger, PositiveInteger,
        PositiveIntegerUpperBound, NegativeInteger, TypeDomainInteger

    """
        natural_successor(::NonnegativeInteger)

    Return the successor of a natural number.
    """
    const natural_successor = Basic.natural_successor

    """
        natural_predecessor(::PositiveInteger)

    Return the predecessor of a nonzero natural number.
    """
    const natural_predecessor = Basic.natural_predecessor

    """
        NonnegativeInteger

    Nonnegative integers in the type domain.

    The implementation is basically the unary numeral system. Especially inspired by
    the Peano axioms/Zermelo construction of the natural numbers.
    """
    const NonnegativeInteger = Basic.NonnegativeInteger

    """
        PositiveInteger

    Positive integers in the type domain. Subtypes [`NonnegativeInteger`](@ref).
    """
    const PositiveInteger = Basic.PositiveInteger

    """
        PositiveIntegerUpperBound

    Positive integers in the type domain. Supertypes [`PositiveInteger`](@ref).
    """
    const PositiveIntegerUpperBound = Basic.PositiveIntegerUpperBound

    """
        ConvertNaturalToNegativeException

    Thrown when a conversion of a negative integer to a natural number is attempted.
    """
    const ConvertNaturalToNegativeException = RecursiveAlgorithms.ConvertNaturalToNegativeException

    """
        NegativeInteger

    Negative integers in the type domain.
    """
    const NegativeInteger = LazyMinus.NegativeInteger

    """
        TypeDomainInteger

    Integers in the type domain.
    """
    const TypeDomainInteger = LazyMinus.TypeDomainInteger
end
