# Tests for typegroup blocks (mutually recursive type definitions)
# See https://github.com/JuliaLang/julia/issues/269

using Test

@testset "typegroup blocks" begin

    @testset "basic mutual recursion" begin
        # Classic graph example: nodes and edges reference each other
        typegroup
            struct TG_Node
                edges::Vector{TG_Edge}
            end
            struct TG_Edge
                from::TG_Node
                to::TG_Node
            end
        end
        @test fieldtype(TG_Node, :edges) == Vector{TG_Edge}
        @test fieldtype(TG_Edge, :from) == TG_Node
        @test fieldtype(TG_Edge, :to) == TG_Node

        # Can construct instances
        n1 = TG_Node(TG_Edge[])
        n2 = TG_Node(TG_Edge[])
        e = TG_Edge(n1, n2)
        push!(n1.edges, e)
        @test n1.edges[1].to === n2
    end

    @testset "parametric types" begin
        # Parametric mutual recursion
        typegroup
            struct TG_PNode{T}
                data::T
                edges::Vector{TG_PEdge{T}}
            end
            struct TG_PEdge{T}
                from::TG_PNode{T}
                to::TG_PNode{T}
            end
        end
        @test fieldtype(TG_PNode{Int}, :edges) == Vector{TG_PEdge{Int}}
        @test fieldtype(TG_PEdge{String}, :from) == TG_PNode{String}

        # Can construct parametric instances
        n1 = TG_PNode(42, TG_PEdge{Int}[])
        n2 = TG_PNode(99, TG_PEdge{Int}[])
        e = TG_PEdge(n1, n2)
        @test e.from.data == 42
        @test e.to.data == 99
    end

    @testset "self-referential types" begin
        # Single type referencing itself (degenerate case)
        typegroup
            struct TG_SelfRef
                next::Union{Nothing, TG_SelfRef}
            end
        end
        @test fieldtype(TG_SelfRef, :next) == Union{Nothing, TG_SelfRef}

        node3 = TG_SelfRef(nothing)
        node2 = TG_SelfRef(node3)
        node1 = TG_SelfRef(node2)
        @test node1.next.next === node3
    end

    @testset "mutable structs" begin
        typegroup
            mutable struct TG_MutNode
                edges::Vector{TG_MutEdge}
            end
            mutable struct TG_MutEdge
                from::TG_MutNode
                to::TG_MutNode
            end
        end
        @test ismutabletype(TG_MutNode)
        @test ismutabletype(TG_MutEdge)

        n1 = TG_MutNode(TG_MutEdge[])
        n2 = TG_MutNode(TG_MutEdge[])
        e = TG_MutEdge(n1, n2)
        push!(n1.edges, e)
        # Can mutate
        e.to = n1
        @test e.to === n1
    end

    @testset "return value" begin
        # typegroup returns nothing (types are defined as side effect)
        result = typegroup
            struct TG_ReturnTest
                x::Int
            end
        end
        @test result === nothing
    end

    @testset "where clause in field types" begin
        # Field types with where clauses (UnionAll)
        typegroup
            struct TG_Container
                # Field type uses where clause with reference to TG_Item
                items::Vector{TG_Item{T} where T}
            end
            struct TG_Item{T}
                value::T
                parent::TG_Container
            end
        end
        @test fieldtype(TG_Container, :items) == Vector{TG_Item{T} where T}
        @test fieldtype(TG_Item{Int}, :parent) == TG_Container

        # Can construct and use
        c = TG_Container(TG_Item[])
        item = TG_Item(42, c)
        push!(c.items, item)
        @test c.items[1].value == 42
        @test c.items[1].parent === c
    end

end
