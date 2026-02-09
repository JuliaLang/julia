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

    @testset "parametric mutual recursion with Union" begin
        # Issue: parametric types with Union{Nothing, OtherType{T}} fields
        # This tests cycle detection during type instantiation
        typegroup
            struct TG_UnionA{T}
                value::T
                other::Union{Nothing, TG_UnionB{T}}
            end
            struct TG_UnionB{T}
                value::T
                other::Union{Nothing, TG_UnionA{T}}
            end
        end
        @test fieldtype(TG_UnionA{Int}, :other) == Union{Nothing, TG_UnionB{Int}}
        @test fieldtype(TG_UnionB{Int}, :other) == Union{Nothing, TG_UnionA{Int}}

        # Construct instances
        a = TG_UnionA{Int}(1, nothing)
        b = TG_UnionB{Int}(2, nothing)
        a2 = TG_UnionA{Int}(3, b)
        b2 = TG_UnionB{Int}(4, a)
        @test a.other === nothing
        @test a2.other.value == 2
        @test b2.other.value == 1
    end

    @testset "parametric direct mutual reference" begin
        # Issue: direct reference (not through Union) caused stack overflow
        # This tests mayinlinealloc cycle detection
        typegroup
            struct TG_DirectA{T}
                value::T
                other::Union{Nothing, TG_DirectB{T}}
            end
            struct TG_DirectB{T}
                target::TG_DirectA{T}  # Direct reference, not Union
                weight::Float64
            end
        end
        @test fieldtype(TG_DirectA{Int}, :other) == Union{Nothing, TG_DirectB{Int}}
        @test fieldtype(TG_DirectB{Int}, :target) == TG_DirectA{Int}

        # Construct instances
        a = TG_DirectA{Int}(42, nothing)
        b = TG_DirectB{Int}(a, 1.5)
        a2 = TG_DirectA{Int}(99, b)
        @test a2.other.target.value == 42
        @test a2.other.weight == 1.5
    end

    @testset "parametric with Vector wrapping" begin
        # Issue: Vector{OtherType{T}} caused stack overflow during layout computation
        typegroup
            struct TG_VecNode{T}
                value::T
                edges::Vector{TG_VecEdge{T}}
            end
            struct TG_VecEdge{T}
                target::TG_VecNode{T}
                weight::Float64
            end
        end
        @test fieldtype(TG_VecNode{Int}, :edges) == Vector{TG_VecEdge{Int}}
        @test fieldtype(TG_VecEdge{String}, :target) == TG_VecNode{String}

        # Construct graph
        n1 = TG_VecNode{Int}(1, TG_VecEdge{Int}[])
        n2 = TG_VecNode{Int}(2, TG_VecEdge{Int}[])
        e1 = TG_VecEdge{Int}(n2, 1.0)
        e2 = TG_VecEdge{Int}(n1, 2.0)
        n3 = TG_VecNode{Int}(3, [e1, e2])
        @test length(n3.edges) == 2
        @test n3.edges[1].target.value == 2
        @test n3.edges[2].target.value == 1
    end

    @testset "three-way parametric mutual recursion" begin
        # Test cycle detection with more than two types
        typegroup
            struct TG_ThreeA{T}
                value::T
                b::Union{Nothing, TG_ThreeB{T}}
            end
            struct TG_ThreeB{T}
                value::T
                c::Union{Nothing, TG_ThreeC{T}}
            end
            struct TG_ThreeC{T}
                value::T
                a::Union{Nothing, TG_ThreeA{T}}
            end
        end
        @test fieldtype(TG_ThreeA{Int}, :b) == Union{Nothing, TG_ThreeB{Int}}
        @test fieldtype(TG_ThreeB{Int}, :c) == Union{Nothing, TG_ThreeC{Int}}
        @test fieldtype(TG_ThreeC{Int}, :a) == Union{Nothing, TG_ThreeA{Int}}

        # Construct chain
        a = TG_ThreeA{Int}(1, nothing)
        c = TG_ThreeC{Int}(3, a)
        b = TG_ThreeB{Int}(2, c)
        a2 = TG_ThreeA{Int}(4, b)
        @test a2.b.c.a.value == 1
    end

    @testset "multiple type parameters" begin
        # Mutual recursion with multiple type parameters
        typegroup
            struct TG_MultiA{K,V}
                key::K
                value::V
                other::Union{Nothing, TG_MultiB{K,V}}
            end
            struct TG_MultiB{K,V}
                key::K
                value::V
                other::Union{Nothing, TG_MultiA{K,V}}
            end
        end
        @test fieldtype(TG_MultiA{String,Int}, :other) == Union{Nothing, TG_MultiB{String,Int}}

        a = TG_MultiA{String,Int}("a", 1, nothing)
        b = TG_MultiB{String,Int}("b", 2, a)
        @test b.other.key == "a"
        @test b.other.value == 1
    end

    @testset "four-way mutual recursion" begin
        typegroup
            struct TG_FourA{T}
                b::Union{Nothing, TG_FourB{T}}
                d::Union{Nothing, TG_FourD{T}}
            end
            struct TG_FourB{T}
                c::Union{Nothing, TG_FourC{T}}
                a::Union{Nothing, TG_FourA{T}}
            end
            struct TG_FourC{T}
                d::Union{Nothing, TG_FourD{T}}
                b::Union{Nothing, TG_FourB{T}}
            end
            struct TG_FourD{T}
                a::Union{Nothing, TG_FourA{T}}
                c::Union{Nothing, TG_FourC{T}}
            end
        end
        a = TG_FourA{Int}(nothing, nothing)
        b = TG_FourB{Int}(nothing, a)
        c = TG_FourC{Int}(nothing, b)
        d = TG_FourD{Int}(a, c)
        @test d.a === a
        @test d.c.b.a === a
    end

    @testset "graph with typed edges" begin
        # Pattern from Rust's petgraph
        typegroup
            struct TG_Graph{N, E}
                nodes::Vector{TG_GraphNode{N, E}}
            end
            struct TG_GraphNode{N, E}
                data::N
                edges::Vector{TG_GraphEdge{N, E}}
            end
            struct TG_GraphEdge{N, E}
                weight::E
                target::TG_GraphNode{N, E}
            end
        end
        n1 = TG_GraphNode{String, Float64}("A", TG_GraphEdge{String,Float64}[])
        n2 = TG_GraphNode{String, Float64}("B", TG_GraphEdge{String,Float64}[])
        e = TG_GraphEdge{String, Float64}(1.5, n2)
        push!(n1.edges, e)
        g = TG_Graph{String, Float64}([n1, n2])
        @test g.nodes[1].edges[1].target.data == "B"
    end

    @testset "JSON-like recursive structure" begin
        typegroup
            struct TG_JSONValue
                data::Union{Nothing, Bool, Int, Float64, String, TG_JSONArray, TG_JSONObject}
            end
            struct TG_JSONArray
                elements::Vector{TG_JSONValue}
            end
            struct TG_JSONObject
                pairs::Vector{Pair{String, TG_JSONValue}}
            end
        end
        arr = TG_JSONArray([TG_JSONValue(42), TG_JSONValue("hello")])
        obj = TG_JSONObject([Pair("array", TG_JSONValue(arr))])
        @test obj.pairs[1].second.data.elements[1].data == 42
    end

    @testset "doubly-linked list" begin
        typegroup
            mutable struct TG_DLNode{T}
                value::T
                prev::Union{Nothing, TG_DLNode{T}}
                next::Union{Nothing, TG_DLNode{T}}
            end
        end
        n1 = TG_DLNode(1, nothing, nothing)
        n2 = TG_DLNode(2, n1, nothing)
        n1.next = n2
        @test n1.next.value == 2
        @test n2.prev.value == 1
    end

    @testset "binary tree with parent pointer" begin
        typegroup
            mutable struct TG_BinTree{T}
                value::T
                parent::Union{Nothing, TG_BinTree{T}}
                left::Union{Nothing, TG_BinTree{T}}
                right::Union{Nothing, TG_BinTree{T}}
            end
        end
        root = TG_BinTree(10, nothing, nothing, nothing)
        left = TG_BinTree(5, root, nothing, nothing)
        right = TG_BinTree(15, root, nothing, nothing)
        root.left = left
        root.right = right
        @test root.left.parent === root
        @test root.right.value == 15
    end

    @testset "lambda calculus AST" begin
        typegroup
            struct TG_LamVar
                name::Symbol
            end
            struct TG_LamAbs
                param::Symbol
                body::Union{TG_LamVar, TG_LamAbs, TG_LamApp}
            end
            struct TG_LamApp
                func::Union{TG_LamVar, TG_LamAbs, TG_LamApp}
                arg::Union{TG_LamVar, TG_LamAbs, TG_LamApp}
            end
        end
        v = TG_LamVar(:x)
        abs = TG_LamAbs(:x, v)
        app = TG_LamApp(abs, v)
        @test app.func.param == :x
    end

    @testset "entity-component pattern" begin
        typegroup
            struct TG_Entity
                id::Int
                components::Dict{Symbol, TG_Component}
            end
            struct TG_Component
                owner::TG_Entity
                data::Any
            end
        end
        e = TG_Entity(1, Dict{Symbol,TG_Component}())
        c = TG_Component(e, "health")
        e.components[:health] = c
        @test e.components[:health].owner === e
    end

    @testset "NamedTuple fields" begin
        typegroup
            struct TG_NTNode
                data::@NamedTuple{value::Int, edge::Union{Nothing, TG_NTEdge}}
            end
            struct TG_NTEdge
                info::@NamedTuple{from::TG_NTNode, to::TG_NTNode, weight::Float64}
            end
        end
        n1 = TG_NTNode((value=1, edge=nothing))
        n2 = TG_NTNode((value=2, edge=nothing))
        e = TG_NTEdge((from=n1, to=n2, weight=1.0))
        @test e.info.from.data.value == 1
    end

    @testset "bounded type parameters" begin
        typegroup
            struct TG_BoundedA{T <: Number}
                b::Union{Nothing, TG_BoundedB{T}}
            end
            struct TG_BoundedB{T <: Number}
                a::Union{Nothing, TG_BoundedA{T}}
            end
        end
        a = TG_BoundedA{Int}(nothing)
        b = TG_BoundedB{Float64}(nothing)
        @test fieldtype(TG_BoundedA{Int}, :b) == Union{Nothing, TG_BoundedB{Int}}
    end

    @testset "deeply nested Union" begin
        typegroup
            struct TG_DeepUnionA
                x::Union{Nothing, Union{Int, Union{String, Union{Float64, TG_DeepUnionB}}}}
            end
            struct TG_DeepUnionB
                y::Union{Nothing, TG_DeepUnionA}
            end
        end
        a = TG_DeepUnionA(nothing)
        b = TG_DeepUnionB(a)
        @test b.y === a
    end

    @testset "supertype referencing incomplete type" begin
        typegroup
            struct TG_SuperRefA <: AbstractVector{TG_SuperRefB}
                data::Vector{TG_SuperRefB}
            end
            struct TG_SuperRefB
                a::TG_SuperRefA
            end
        end
        @test TG_SuperRefA <: AbstractVector{TG_SuperRefB}
    end

    @testset "self-referential supertype parameter" begin
        # Node{T} <: AbstractVector{Node{T}} -- type references itself in supertype params
        typegroup
            struct TG_SelfSuperNode{T} <: AbstractVector{TG_SelfSuperNode{T}}
                data::T
            end
        end
        @test TG_SelfSuperNode{Int} <: AbstractVector{TG_SelfSuperNode{Int}}
        @test supertype(TG_SelfSuperNode{Int}) == AbstractVector{TG_SelfSuperNode{Int}}
        n = TG_SelfSuperNode{Int}(42)
        @test n.data == 42

        # Two types where one references itself in supertype
        typegroup
            struct TG_SelfSuperA{T} <: AbstractVector{TG_SelfSuperA{T}}
                b::Union{Nothing, TG_SelfSuperB{T}}
            end
            struct TG_SelfSuperB{T}
                a::TG_SelfSuperA{T}
            end
        end
        @test TG_SelfSuperA{Int} <: AbstractVector{TG_SelfSuperA{Int}}
        @test fieldtype(TG_SelfSuperB{Int}, :a) == TG_SelfSuperA{Int}
    end

    @testset "Tuple fields with incomplete types" begin
        # Self-referential Tuple field
        typegroup
            struct TG_TupleSelf
                data::Tuple{TG_TupleSelf}
            end
        end
        @test fieldtype(TG_TupleSelf, :data) == Tuple{TG_TupleSelf}

        # Tuple with two types from typegroup
        typegroup
            struct TG_TupleA
                data::Tuple{Int, TG_TupleB}
            end
            struct TG_TupleB
                x::Int
            end
        end
        @test fieldtype(TG_TupleA, :data) == Tuple{Int, TG_TupleB}
        a = TG_TupleA((42, TG_TupleB(99)))
        @test a.data[1] == 42
        @test a.data[2].x == 99

        # NTuple with self-reference through Union
        typegroup
            struct TG_NTupleNode
                neighbors::NTuple{3, Union{Nothing, TG_NTupleNode}}
            end
        end
        @test fieldtype(TG_NTupleNode, :neighbors) == NTuple{3, Union{Nothing, TG_NTupleNode}}
        n = TG_NTupleNode((nothing, nothing, nothing))
        @test n.neighbors[1] === nothing

        # Tuple with Union containing incomplete type
        typegroup
            struct TG_TupleUnion
                data::Tuple{Int, Union{Nothing, TG_TupleUnion}}
            end
        end
        t = TG_TupleUnion((42, nothing))
        @test t.data[1] == 42
        @test t.data[2] === nothing
        t2 = TG_TupleUnion((99, t))
        @test t2.data[2].data[1] == 42
    end

end
