# This file is a part of Julia. License is MIT: https://julialang.org/license

baremodule TypeDomainIntegers
    baremodule Basic
        using Base: @nospecialize
        export
            natural_successor, natural_predecessor,
            NonnegativeInteger, PositiveInteger, PositiveIntegerUpperBound,
            zero
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
                r::NonnegativeInteger
            end
        end
        function natural_successor(o::NonnegativeInteger)
            new_nonnegative_integer(o)::PositiveInteger
        end
        function natural_predecessor(o::PositiveInteger)
            r = o.predecessor
            r::NonnegativeInteger
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

    baremodule PrimitiveTypes
        using Base: unsigned, map
        const types_signed = (Int8, Int16, Int32, Int64, Int128)
        const types_unsigned = (UInt8, UInt16, UInt32, UInt64, UInt128)
        const types_int_without_bool = (types_signed..., types_unsigned...)
        const types_int_with_bool = (Bool, types_int_without_bool...)
        const types_float = (Float16, Float32, Float64)
        const types_all = (types_int_with_bool..., types_float...)
        function union_of_types(t::Tuple{Vararg{DataType}})
            Union{t...,}::Type
        end
        const type_type = Type{<:Type}
        function type(t::Type)
            Type{t}::type_type
        end
        function type_union_of_types(t::Tuple{Vararg{Type}})
            s = map(type, t)
            union_of_types(s)::type_type
        end
        const TypesSigned = union_of_types(types_signed)
        const TypesSignedType = type_union_of_types(types_signed)
        const TypesAll = union_of_types(types_all)
        const TypesAllType = type_union_of_types(types_all)
    end

    baremodule RecursiveAlgorithms
        using ..Basic, ..LazyMinus, ..PrimitiveTypes
        using Base: Base, signbit, typemax, !, +, -, <, @assume_effects, @inline, @nospecialize
        export subtracted, added, to_narrowest_signed_int, from_abs_int, is_even, multiplied, is_less
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
            ret::NonnegativeInteger
        end
        const PSigned = PrimitiveTypes.TypesSigned
        @assume_effects :foldable function widening_increment(n::PSigned)
            if n < typemax(n)
                n + true
            else
                Base.widen(n) + true
            end::PSigned
        end
        @assume_effects :foldable function abs_decrement(n::PSigned)
            if signbit(n)
                n + true
            else
                n - true
            end::PSigned
        end
        @assume_effects :foldable function to_narrowest_signed_int(@nospecialize o::NonnegativeInteger)
            if o isa PositiveIntegerUpperBound
                let p = natural_predecessor(o), t = @inline to_narrowest_signed_int(p)
                    widening_increment(t)
                end
            else
                Int8(0)
            end
        end
        struct ConvertNaturalToNegativeException <: Exception end
        @assume_effects :foldable function from_abs_int(n::PSigned)
            ret = if Base.iszero(n)
                zero()
            else
                let v = abs_decrement(n), p = @inline from_abs_int(v)
                    p = p::NonnegativeInteger
                    natural_successor(p)
                end
            end
            ret::NonnegativeInteger
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

    baremodule Conversion
        using Base: map, signbit, -, @nospecialize
        using ..Basic, ..LazyMinus, ..RecursiveAlgorithms, ..PrimitiveTypes
        using ..RecursiveAlgorithms: ConvertNaturalToNegativeException
        export
            tdnn_to_int, tdi_to_int, tdnn_from_int, tdi_from_int,
            tdnn_to_x,   tdi_to_x,   tdnn_from_x,   tdi_from_x
        function tdnn_to_int(@nospecialize n::NonnegativeInteger)
            if n isa PositiveIntegerUpperBound
                let p = natural_predecessor(n)
                    if p isa PositiveIntegerUpperBound
                        to_narrowest_signed_int(n)
                    else
                        true
                    end
                end
            else
                false
            end
        end
        function tdi_to_int(@nospecialize n::TypeDomainInteger)
            if n isa NegativeInteger
                let m = negated(n)::PositiveInteger
                    -to_narrowest_signed_int(m)
                end
            else
                tdnn_to_int(n)
            end
        end
        const PSigned = PrimitiveTypes.TypesSigned
        const TNumber = Type{<:Number}
        function tdnn_from_int(i::PSigned)
            if signbit(i)
                throw(ConvertNaturalToNegativeException())
            end
            from_abs_int(i)
        end
        function tdi_from_int(i::PSigned)
            n = from_abs_int(i)
            if signbit(i)
                negated(n)::NegativeInteger
            else
                n
            end
        end
        function tdnn_to_x(x::TNumber, @nospecialize n::NonnegativeInteger)
            i = tdnn_to_int(n)
            x(i)
        end
        function tdi_to_x(x::TNumber, @nospecialize n::TypeDomainInteger)
            i = tdi_to_int(n)
            x(i)
        end
        function x_to_int(x::Number)
            t = Int16  # presumably wide enough for any type domain integer
            t(x)::t
        end
        function tdnn_from_x(x::Number)
            i = x_to_int(x)
            tdnn_from_int(i)
        end
        function tdi_from_x(x::Number)
            i = x_to_int(x)
            tdi_from_int(i)
        end
    end

    baremodule BaseOverloadsPromotion
        using ..Basic, ..RecursiveAlgorithms, ..LazyMinus, ..PrimitiveTypes
        using ..Basic: UpperBounds
        using Base: Base, @nospecialize, @eval
        struct UnexpectedException <: Exception end
        const ZeroOrOne = Union{typeof(zero()),typeof(natural_successor(zero()))}
        for type ∈ PrimitiveTypes.types_all
            @eval function Base.promote_rule(
                (@nospecialize tdt::Type{<:TypeDomainInteger}),
                ::Type{$type},
            )
                if tdt <: Union{}
                    throw(UnexpectedException())
                end
                t = if tdt <: ZeroOrOne
                    Bool
                else
                    Int16  # presumably wide enough for any type domain integer
                end
                Base.promote_type(t, $type)
            end
        end
    end

    baremodule BaseOverloads
        using ..Basic, ..RecursiveAlgorithms, ..LazyMinus, ..PrimitiveTypes, ..Conversion
        using Base: Base, convert, <, +, -, *, ==, isequal, isless, !, @nospecialize, @eval
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
                n = n::NonnegativeInteger
                if n isa PositiveIntegerUpperBound
                    false
                else
                    true
                end
            end
        end
        const PAll = PrimitiveTypes.TypesAll
        for type ∈ PrimitiveTypes.types_all
            @eval begin
                function Base.convert(::Type{$type}, @nospecialize n::TypeDomainInteger)
                    tdi_to_x($type, n)
                end
                function (::Type{$type})(@nospecialize n::TypeDomainInteger)
                    tdi_to_x($type, n)
                end
            end
        end
        function Base.convert(::Type{NonnegativeInteger}, x::PAll)
            tdnn_from_x(x)
        end
        function Base.convert(::Type{TypeDomainInteger}, x::PAll)
            tdi_from_x(x)
        end
        function NonnegativeInteger(x::PAll)
            tdnn_from_x(x)
        end
        function TypeDomainInteger(x::PAll)
            tdi_from_x(x)
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
        function Base.:(-)(@nospecialize n::TypeDomainInteger)
            negated(n)
        end
    end

    baremodule BaseHelpers
        using ..Basic, ..LazyMinus, ..Conversion
        using Base: convert, isequal, iszero, <, +, -, *, ==, !, @nospecialize
        export apply_n_t, apply_t_n
        function apply_n_t(::typeof(+), (@nospecialize l::Number), @nospecialize r::TypeDomainInteger)
            if r isa NegativeInteger
                let pos = negated(r)::PositiveInteger
                    l + -tdi_to_int(pos)
                end
            else
                r = r::NonnegativeInteger
                if r isa PositiveIntegerUpperBound
                    l + tdi_to_int(r)
                else
                    l
                end
            end
        end
        function apply_t_n(::typeof(+), (@nospecialize l::TypeDomainInteger), @nospecialize r::Number)
            # addition is commutative
            apply_n_t(+, r, l)
        end
        function apply_n_t(::typeof(-), (@nospecialize l::Number), @nospecialize r::TypeDomainInteger)
            apply_n_t(+, l, negated(r))
        end
        function apply_t_n(::typeof(-), (@nospecialize l::TypeDomainInteger), @nospecialize r::Number)
            if l isa NegativeInteger
                tdi_to_int(l) - r
            else
                l = l::NonnegativeInteger
                if l isa PositiveIntegerUpperBound
                    tdi_to_int(l) - r
                else
                    -r
                end
            end
        end
        function apply_n_t(::typeof(*), (@nospecialize l::Number), @nospecialize r::TypeDomainInteger)
            if r isa NegativeInteger
                let pos = negated(r), posm1 = natural_predecessor(pos)
                    if posm1 isa PositiveIntegerUpperBound
                        l * tdi_to_int(r)
                    else
                        -l
                    end
                end
            else
                r = r::NonnegativeInteger
                if r isa PositiveIntegerUpperBound
                    let p = natural_predecessor(r)
                        if p isa PositiveIntegerUpperBound
                            l * tdi_to_int(r)
                        else
                            l
                        end
                    end
                else
                    zero()
                end
            end
        end
        function apply_t_n(::typeof(*), (@nospecialize l::TypeDomainInteger), @nospecialize r::Number)
            if l isa NegativeInteger
                let pos = negated(l), posm1 = natural_predecessor(pos)
                    if posm1 isa PositiveIntegerUpperBound
                        tdi_to_int(l) * r
                    else
                        -r
                    end
                end
            else
                l = l::NonnegativeInteger
                if l isa PositiveIntegerUpperBound
                    let p = natural_predecessor(l)
                        if p isa PositiveIntegerUpperBound
                            tdi_to_int(l) * r
                        else
                            r
                        end
                    end
                else
                    zero()
                end
            end
        end
        function apply_n_t(
            func::Union{typeof(isequal),typeof(==)},
            (@nospecialize l::Number),
            (@nospecialize r::TypeDomainInteger),
        )
            if r isa NegativeInteger
                func(l, tdi_to_int(r))
            else
                r = r::NonnegativeInteger
                if r isa PositiveIntegerUpperBound
                    func(l, tdi_to_int(r))
                else
                    iszero(l)
                end
            end
        end
        function apply_t_n(
            func::Union{typeof(isequal),typeof(==)},
            (@nospecialize l::TypeDomainInteger),
            (@nospecialize r::Number),
        )
            # equality is commutative
            apply_n_t(func, r, l)
        end
    end

    baremodule BaseOverloadsBinaryOperations
        using Base: Base, isequal, +, -, *, ==, @nospecialize, @eval
        using ..Basic, ..LazyMinus, ..PrimitiveTypes, ..BaseHelpers
        for type ∈ PrimitiveTypes.types_all, func ∈ (:+, :-, :*, :(==), :isequal)
            @eval begin
                function Base.$func((@nospecialize l::$type), (@nospecialize r::TypeDomainInteger))
                    apply_n_t($func, l, r)
                end
                function Base.$func((@nospecialize l::TypeDomainInteger), (@nospecialize r::$type))
                    apply_t_n($func, l, r)
                end
            end
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
