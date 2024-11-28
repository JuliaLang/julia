# This file is a part of Julia. License is MIT: https://julialang.org/license

# Adapted from the TypeDomainNaturalNumbers.jl package.
module _TypeDomainNumbers
    module Zeros
        export Zero
        struct Zero end
    end

    module PositiveIntegers
        module RecursiveStep
            using ...Zeros
            export recursive_step
            function recursive_step(@nospecialize t::Type)
                Union{Zero, t}
            end
        end
        module UpperBounds
            using ..RecursiveStep
            abstract type A end
            abstract type B{P <: recursive_step(A)} <: A    end
            abstract type C{P <: recursive_step(B)} <: B{P} end
            abstract type D{P <: recursive_step(C)} <: C{P} end
        end
        using .RecursiveStep
        const PositiveIntegerUpperBound = UpperBounds.A
        const PositiveIntegerUpperBoundTighter = UpperBounds.D
        export
            natural_successor, natural_predecessor,
            NonnegativeInteger, NonnegativeIntegerUpperBound,
            PositiveInteger, PositiveIntegerUpperBound
        struct PositiveInteger{
            Predecessor <: recursive_step(PositiveIntegerUpperBoundTighter),
        } <: PositiveIntegerUpperBoundTighter{Predecessor}
            predecessor::Predecessor
            global const NonnegativeInteger = recursive_step(PositiveInteger)
            global const NonnegativeIntegerUpperBound = recursive_step(PositiveIntegerUpperBound)
            global function natural_successor(p::P) where {P <: NonnegativeInteger}
                new{P}(p)
            end
        end
        function natural_predecessor(@nospecialize o::PositiveInteger)
            getfield(o, :predecessor)  # avoid specializing `getproperty` for each number
        end
    end

    module IntegersGreaterThanOne
        using ..PositiveIntegers
        export
            IntegerGreaterThanOne, IntegerGreaterThanOneUpperBound,
            natural_predecessor_predecessor
        const IntegerGreaterThanOne = let t = PositiveInteger
            t{P} where {P <: t}
        end
        const IntegerGreaterThanOneUpperBound = let t = PositiveIntegerUpperBound
            PositiveIntegers.UpperBounds.B{P} where {P <: t}
        end
        function natural_predecessor_predecessor(@nospecialize x::IntegerGreaterThanOne)
            natural_predecessor(natural_predecessor(x))
        end
    end

    module Constants
        using ..Zeros, ..PositiveIntegers
        export n0, n1
        const n0 = Zero()
        const n1 = natural_successor(n0)
    end

    module Utils
        using ..PositiveIntegers, ..IntegersGreaterThanOne, ..Constants
        using Base: @assume_effects
        export half_floor, half_ceiling
        @assume_effects :foldable :nothrow function half_floor(@nospecialize m::NonnegativeInteger)
            if m isa IntegerGreaterThanOneUpperBound
                let n = natural_predecessor_predecessor(m), rec = half_floor(n)
                    natural_successor(rec)
                end
            else
                n0
            end
        end
        @assume_effects :foldable :nothrow function half_ceiling(@nospecialize m::NonnegativeInteger)
            if m isa IntegerGreaterThanOneUpperBound
                let n = natural_predecessor_predecessor(m), rec = half_ceiling(n)
                    natural_successor(rec)
                end
            else
                if m isa PositiveIntegerUpperBound
                    n1
                else
                    n0
                end
            end
        end
    end
end

module _TypeDomainNumberTupleUtils
    using
        .._TypeDomainNumbers.PositiveIntegers, .._TypeDomainNumbers.IntegersGreaterThanOne,
        .._TypeDomainNumbers.Constants, .._TypeDomainNumbers.Utils, .._TupleTypeByLength
    using Base: @assume_effects, front, tail
    export tuple_type_domain_length, split_tuple_into_halves, skip_from_front, skip_from_tail
    @assume_effects :foldable :nothrow function tuple_type_domain_length(@nospecialize tup::Tuple)
        if tup isa Tuple1OrMore
            let t = tail(tup), rec = tuple_type_domain_length(t)
                natural_successor(rec)
            end
        else
            n0
        end
    end
    @assume_effects :foldable function skip_from_front((@nospecialize tup::Tuple), @nospecialize skip_count::NonnegativeInteger)
        if skip_count isa PositiveIntegerUpperBound
            let cm1 = natural_predecessor(skip_count), t = tail(tup)
                @inline skip_from_front(t, cm1)
            end
        else
            tup
        end
    end
    @assume_effects :foldable function skip_from_tail((@nospecialize tup::Tuple), @nospecialize skip_count::NonnegativeInteger)
        if skip_count isa PositiveIntegerUpperBound
            let cm1 = natural_predecessor(skip_count), t = front(tup)
                @inline skip_from_tail(t, cm1)
            end
        else
            tup
        end
    end
    function split_tuple_into_halves(@nospecialize tup::Tuple)
        len = tuple_type_domain_length(tup)
        len_l = half_floor(len)
        len_r = half_ceiling(len)
        tup_l = skip_from_tail(tup, len_r)
        tup_r = skip_from_front(tup, len_l)
        (tup_l, tup_r)
    end
end
