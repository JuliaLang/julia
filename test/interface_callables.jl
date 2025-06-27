using Test

module ExampleTypes end

@testset "interface callables" begin
    Base.@nospecializeinfer function max_methods_of(@nospecialize callable::Any)
        raw = Base._stable_typeof(callable).name.max_methods
        if iszero(raw)
            3  # default value, TODO: how to avoid hardcoding this?
        else
            Int(raw)
        end
    end
    """
        interface_callables::Dict{DataType, Vector{Any}}

    In each key-value pair:

    * the key is the direct supertype of the newly-defined type

    * the values are the applicable interface callables
    """
    interface_callables = let
        nums = Any[widen, zero, one, oneunit]
        keys_and_values = Any[
            (Any => Any[eltype, Base.IteratorSize, Base.IteratorEltype]),
            (DenseVector{Float32} => Any[Base.elsize]),
            (Real => nums),
            (AbstractFloat => nums),
            (Integer => nums),
        ]
        Dict{DataType, Vector{Any}}(keys_and_values)
    end
    interface_callables_supertypes = collect(DataType, keys(interface_callables))
    function example_type_name(i::Int, j::Int)
        n = string('_', i, '_', j)
        Symbol(n)
    end
    function tests(interface_callables, interface_callables_supertypes)
        for (i, supertype) ∈ enumerate(interface_callables_supertypes)
            for (j, callable) ∈ enumerate(interface_callables[supertype])
                typ = getproperty(ExampleTypes, example_type_name(i, j))
                method_match_length = length(methods(callable, Tuple{Type{<:typ}}))
                # `max_methods` is high-enough for good inference
                @test method_match_length ≤ max_methods_of(callable)
                # don't match an excessive number of methods for the newly-defined type
                @test method_match_length ≤ 2
            end
        end
    end
    for (i, supertype) ∈ enumerate(interface_callables_supertypes)  # define types
        for (j, _) ∈ enumerate(interface_callables[supertype])
            type_name = example_type_name(i, j)
            @eval ExampleTypes struct $type_name{X} <: $supertype
                x::X
            end
        end
    end
    @testset "first pass: without any new method defined for the new type" begin
        tests(interface_callables, interface_callables_supertypes)
    end
    for (i, supertype) ∈ enumerate(interface_callables_supertypes)  # define methods for new types
        for (j, callable) ∈ enumerate(interface_callables[supertype])
            callable_name = nameof(callable)
            type_name = example_type_name(i, j)
            @eval ExampleTypes function Base.$callable_name(::Type{<:$type_name}) end
        end
    end
    @testset "second pass: with a new method defined for the new type" begin
        tests(interface_callables, interface_callables_supertypes)
    end
end
