# AttrGraph substrate tests (§3.7 Level 1): the ONE storage core under both
# the IR statement table (IRBody composes it) and SyntaxGraph (wraps it):
# row/pool storage, packed two-mode ops words, attribute columns, and
# compact_graph!-as-GC.

using UnifiedIR: AttrGraph, KIND_UNSET, OPS_LEAF, newrow!, newnode!, setchildren!,
    set_children_range!, children_range, child_ids, child_id, is_leaf, numchildren,
    nnodes, node_kind, set_node_kind!, with_cols, edge_ranges_view, EdgeRangesView,
    edges_view, EdgeView, compact_graph!, getattrcol, ensure_attrcol!, setattrnode!,
    getattrnode,
    # generic tree porcelain (§3.7)
    Tree, NodeList, children, mapchildren, mknode, mkleaf, mktree, newleaf, newnode,
    copy_ast, copy_attrs!, provenance, prov, prov_end, provenance_terminal,
    rawkind, setrawkind!, new_id!, setattr!, hasattr, attrnames, node_string,
    foldtree, traverse, print_tree, tree_ids

@testset "AttrGraph substrate" begin
    @testset "nodes, pool edges, packed words" begin
        g = AttrGraph()
        a = newnode!(g); b = newnode!(g); c = newnode!(g)
        @test (a, b, c) == (1, 2, 3)
        @test nnodes(g) == 3
        @test is_leaf(g, a) && is_leaf(g, b) && is_leaf(g, c)
        @test numchildren(g, a) == 0
        @test children_range(g, a) == 0:-1        # leaf convention
        @test g.ops[a] == OPS_LEAF

        setchildren!(g, a, [b, c])
        @test !is_leaf(g, a)
        @test numchildren(g, a) == 2
        @test collect(child_ids(g, a)) == [2, 3]
        @test child_id(g, a, 1) == 2 && child_id(g, a, 2) == 3
        @test children_range(g, a) == 1:2
        @test length(g.operands) == 2
        # edges are STMT-tagged words in the SHARED operand pool (§3.7)
        @test UnifiedIR.optag(g.operands[1]) == UnifiedIR.TAG_STMT

        # empty child list is NOT a leaf (distinct from never-set)
        setchildren!(g, b, Int[])
        @test !is_leaf(g, b)
        @test numchildren(g, b) == 0
        @test first(children_range(g, b)) != 0

        # kind column
        @test node_kind(g, a) == KIND_UNSET
        set_node_kind!(g, a, UnifiedIR.Kind(0x0012))
        @test node_kind(g, a) == UnifiedIR.Kind(0x0012)
    end

    @testset "setchildren! same-or-smaller reuse / growth (store_ops! discipline)" begin
        g = AttrGraph()
        a = newnode!(g); b = newnode!(g); c = newnode!(g); d = newnode!(g)
        setchildren!(g, a, [b, c, d])
        n0 = length(g.operands)
        r0 = children_range(g, a)
        # smaller: reuses the range in place
        setchildren!(g, a, [d, b])
        @test length(g.operands) == n0
        @test first(children_range(g, a)) == first(r0)
        @test collect(child_ids(g, a)) == [4, 2]
        # growth: appends a fresh range, orphaning the old slots
        setchildren!(g, a, [b, c, d, b])
        @test length(g.operands) == n0 + 4
        @test collect(child_ids(g, a)) == [2, 3, 4, 2]
    end

    @testset "edge_ranges / edges views (identity + read/write)" begin
        g = AttrGraph()
        a = newnode!(g); b = newnode!(g)
        setchildren!(g, a, [b])
        v = edge_ranges_view(g)
        v2 = edge_ranges_view(with_cols(g, Dict{Symbol,Any}()))
        @test v === v2                       # egal across sibling views
        @test length(v) == 2
        @test v[a] == 1:1
        @test v[b] == 0:-1
        v[b] = 0:-1
        @test is_leaf(g, b)
        set_children_range!(g, b, 1:1)       # manual pool layout
        @test collect(child_ids(g, b)) == [2]

        ev = edges_view(g)
        @test ev === edges_view(with_cols(g, Dict{Symbol,Any}()))
        @test ev[1] == 2
        ev[1] = 1
        @test child_id(g, a, 1) == 1
        push!(ev, 2)
        @test length(g.operands) == 2
    end

    @testset "attribute columns (dynamic mode)" begin
        g = AttrGraph()
        a = newnode!(g); b = newnode!(g)
        ensure_attrcol!(g, :name)
        setattrnode!(g, a, :name, "root")
        @test getattrnode(g, a, :name, nothing) == "root"
        @test getattrnode(g, b, :name, :missing) === :missing
        @test getattrcol(g, :name) isa Dict{Int,Any}
        # sibling view shares node storage but not the column set
        g2 = with_cols(g, Dict{Symbol,Any}())
        @test nnodes(g2) == 2
        c = newnode!(g2)
        @test nnodes(g) == 3 && c == 3
    end

    @testset "compact_graph!: GC from roots" begin
        # tree: 1 -> (2, 3); 3 -> (4,); plus dead nodes 5, 6 (6 -> (5,))
        g = AttrGraph()
        for _ in 1:6; newnode!(g); end
        setchildren!(g, 1, [2, 3])
        setchildren!(g, 3, [4])
        setchildren!(g, 6, [5])
        for i in 1:6; set_node_kind!(g, i, UnifiedIR.Kind(i)); end
        ensure_attrcol!(g, :tag)
        for i in 1:6; setattrnode!(g, i, :tag, i * 10); end

        remap = compact_graph!(g, [1])
        @test remap == Int32[1, 2, 3, 4, 0, 0]
        @test nnodes(g) == 4
        @test length(g.operands) == 3
        @test collect(child_ids(g, 1)) == [2, 3]
        @test collect(child_ids(g, 3)) == [4]
        @test is_leaf(g, 2) && is_leaf(g, 4)
        @test [node_kind(g, i) for i in 1:4] == UnifiedIR.Kind.(1:4)
        col = getattrcol(g, :tag)
        @test col == Dict{Int,Any}(1 => 10, 2 => 20, 3 => 30, 4 => 40)
    end

    @testset "compact_graph!: renumbering + orphaned pool reclamation" begin
        g = AttrGraph()
        for _ in 1:5; newnode!(g); end
        setchildren!(g, 2, [4])
        setchildren!(g, 2, [4, 5])          # growth orphans the first range
        setchildren!(g, 4, [5])
        @test length(g.operands) == 4       # 1 orphaned + 3 live
        remap = compact_graph!(g, [2])
        @test remap == Int32[0, 1, 0, 2, 3]
        @test nnodes(g) == 3
        @test length(g.operands) == 3       # orphan reclaimed
        @test collect(child_ids(g, 1)) == [2, 3]
        @test collect(child_ids(g, 2)) == [3]
    end

    @testset "compact_graph!: attribute refs traced and remapped" begin
        # 1 -> (2,); node 2's :src references node 4 (not a child);
        # 4 -> (5,); node 3 is dead. :src must keep 4,5 alive and be rewritten.
        g = AttrGraph()
        for _ in 1:5; newnode!(g); end
        setchildren!(g, 1, [2])
        setchildren!(g, 4, [5])
        ensure_attrcol!(g, :src)
        setattrnode!(g, 2, :src, 4)
        setattrnode!(g, 3, :src, 1)          # on a dead node: dropped, not traced
        refs(name, v) = name === :src && v isa Int ? (v,) : nothing
        rmv(name, v, m) = name === :src && v isa Int ? Int(m[v]) : v
        remap = compact_graph!(g, [1]; attr_refs = refs, remap_attr = rmv)
        @test remap == Int32[1, 2, 0, 3, 4]
        @test nnodes(g) == 4
        col = getattrcol(g, :src)
        @test col == Dict{Int,Any}(2 => 3)   # node 2 kept id 2; ref 4 => new id 3
        @test collect(child_ids(g, 3)) == [4]
    end

    @testset "compact_graph!: errors" begin
        g = AttrGraph()
        newnode!(g)
        @test_throws ArgumentError compact_graph!(g, [7])
    end

    @testset "generic tree porcelain over an AttrGraph" begin
        g = AttrGraph()
        ensure_attrcol!(g, :kind)     # not required for rawkind; used via newleaf? no —
        ensure_attrcol!(g, :source)
        ensure_attrcol!(g, :name)
        # construction primitives
        leafa = newleaf(g, LineNumberNode(1), 0x0010)   # Integer converts via kind_bits
        leafb = setattr!(newleaf(g, LineNumberNode(2), 0x0011), :name, "b")
        node = newnode(g, LineNumberNode(3), 0x0012, tree_ids(leafa, leafb))
        @test node isa Tree
        @test rawkind(node) == UnifiedIR.Kind(0x0012)
        @test numchildren(node) == 2
        @test node[1]._id == leafa._id && node[2]._id == leafb._id
        @test node[end]._id == leafb._id
        @test children(node) isa NodeList
        @test is_leaf(node[1])
        @test node[2].name == "b"
        @test get(node[2], :name, nothing) == "b"
        @test get(node[1], :name, :none) === :none
        @test hasattr(node[2], :name) && !hasattr(node[1], :name)
        @test :name in collect(attrnames(node[2]))
        # provenance chain (mk* records node links; terminals pass through)
        node2 = mknode(node, children(node))
        @test prov(node2)._id == node._id
        @test provenance_terminal(node2) == LineNumberNode(3)
        @test [p._id for p in provenance(node2)] == [node._id]
        # copy-on-write leaf keeps kind + attrs
        leafb2 = mkleaf(node[2])
        @test rawkind(leafb2) == UnifiedIR.Kind(0x0011) && leafb2.name == "b"
        # mapchildren: alloc-free unchanged fast path
        @test mapchildren(identity, g, node) === node
        swapped = mapchildren(c -> c._id == leafa._id ? node[2] : node[1], g, node)
        @test [c._id for c in children(swapped)] == [leafb._id, leafa._id]
        @test rawkind(swapped) == rawkind(node)
        # structural ≈ (default payload hook: kind + shape)
        @test node ≈ mktree(node)
        @test !(node ≈ leafa)
        # fold / traverse / printing
        @test foldtree((n, _) -> n + 1, 0, node) == 3
        seen = Int[]
        traverse(n -> push!(seen, n._id), node)
        @test seen == [node._id, leafa._id, leafb._id]
        @test occursin("name=\"b\"", node_string(node))
        io = IOBuffer()
        print_tree(io, node)
        @test occursin("#$(node._id)", String(take!(io)))
        # cross-graph copy_ast (follows provenance links)
        g2 = AttrGraph()
        ensure_attrcol!(g2, :kind); ensure_attrcol!(g2, :source); ensure_attrcol!(g2, :name)
        node3 = copy_ast(g2, node2)
        @test UnifiedIR.syntax_graph(node3) === g2
        @test rawkind(node3) == rawkind(node2)
        @test numchildren(node3) == 2 && node3[2].name == "b"
        @test provenance_terminal(node3) == LineNumberNode(3)
    end

    @testset "IR rows through the SAME tree porcelain (§3.7)" begin
        # An AST is a floating body in the all-node-refs dialect; the same
        # cursor/porcelain works on IR rows through the STMT-tagged projection.
        src = """
        node @expr(%1::Int64)  layout=floating {
          eq %2 = test.mul %1, %1 :: Int64
          eq %3 = test.mul %1, const 2 :: Int64
          eq %4 = test.add %2, %3 :: Int64
        }
        """
        ir0 = parse_ir(src)
        @test UnifiedIR.layout(ir0) == UnifiedIR.LAYOUT_FLOATING
        # a provenance column through the same §3.5 machinery
        ir = convert_universe((source = SparseCol{Any}(),), ir0)

        root = Tree(ir, 4)
        @test [c._id for c in children(root)] == [2, 3]   # STMT projection
        @test numchildren(Tree(ir, 3)) == 1               # CONST operand skipped
        @test is_leaf(Tree(ir, 1))
        @test rawkind(root) == K"test.add"

        # the same provenance() over a :source column on IR rows
        setattr!(ir, 4, :source, 2)
        setattr!(ir, 2, :source, LineNumberNode(42))
        @test [p._id for p in provenance(root)] == [2]
        @test prov(root)._id == 2
        @test provenance_terminal(root) == LineNumberNode(42)

        # the same printer + fold
        io = IOBuffer()
        print_tree(io, root)
        out = String(take!(io))
        @test occursin("#4", out) && occursin("#1", out)
        @test foldtree((n, _) -> n + 1, 0, root) == 6     # preorder over the DAG: 4,2,1,1,3,1

        # the same mapchildren rewrite (alloc-free fast path + mknode)
        @test mapchildren(identity, ir, root) === root
        n0 = nstmts(ir)
        swapped = mapchildren(c -> Tree(ir, c._id == 2 ? 3 : 2), ir, root)
        @test nstmts(ir) == n0 + 1                        # fresh row via the shared new_id!
        @test [c._id for c in children(swapped)] == [3, 2]
        @test rawkind(swapped) == rawkind(root)           # kind copied through the core column
        @test prov(swapped)._id == root._id               # provenance recorded on the IR row

        # lowering is the progressive introduction of ordering into the SAME
        # rows: schedule! the floating body to dense
        ir2, remap = UnifiedIR.schedule!(ir)
        @test UnifiedIR.layout(ir2) == UnifiedIR.LAYOUT_DENSE
        @test UnifiedIR.nstmts(ir2) == n0 + 1
    end

    @testset "graph-qualified provenance: the seam-crossing walk (§3.7 L2)" begin
        # syntax-side graph with a :source chain ending at a terminal
        g = AttrGraph()
        ensure_attrcol!(g, :source)
        n1 = newleaf(g, "file.jl:3", 0x0021)         # terminal source ref (opaque)
        n2 = mkleaf(Tree(g, n1._id))                  # chain: n2 -> n1 -> "file.jl:3"

        # IR side: floating body whose :source column holds CURSORS into g
        src = """
        node @expr(%1::Int64)  layout=floating {
          eq %2 = test.mul %1, %1 :: Int64
        }
        """
        ir = convert_universe((source = UnifiedIR.ProvenanceCol(),), parse_ir(src))
        setattr!(ir, 2, :source, n2)                  # graph-qualified reference

        st = Tree(ir, 2)
        # (b) hop: prov crosses into the syntax graph
        @test prov(st) isa Tree && UnifiedIR.syntax_graph(prov(st)) === g
        @test prov(st)._id == n2._id
        # one walk, zero seams: IR stmt -> syntax nodes -> terminal
        @test [p._id for p in provenance(st)] == [n2._id, n1._id]
        @test UnifiedIR.syntax_graph(provenance(st)) === g
        @test prov_end(st)._id == n1._id
        @test provenance_terminal(st) == "file.jl:3"
        # (a)/(c) unchanged on the syntax side
        @test provenance_terminal(Tree(g, n2._id)) == "file.jl:3"

        # Annotation-class column mechanics: compact! rekeys, values untouched
        b = Builder(name = :provdemo, cols = (source = UnifiedIR.ProvenanceCol(),))
        a  = append_stmt!(b, K"region_arg"; type = Int64)
        d  = append_stmt!(b, K"test.add", a, a; type = Int64)    # dead
        y  = append_stmt!(b, K"test.mul", a, a; type = Int64)
        append_stmt!(b, K"return", y)
        ir2 = finish!(b)
        setattr!(ir2, Int(y.id), :source, n2)
        UnifiedIR.delete_stmt!(ir2, d)
        ir2, rs = compact!(ir2)
        newy = Int(rs.stmt[y.id])
        @test newy != 0 && newy != Int(y.id)                      # rekeyed
        col = UnifiedIR.getattr(ir2, :source)
        @test haskey(col, newy) && col[newy] === n2               # value NEVER rewritten
        @test !haskey(col, Int(y.id))
        @test provenance_terminal(Tree(ir2, newy)) == "file.jl:3" # walk survives compact!
        # Annotation class: replace_stmt! does not invalidate the column
        @test UnifiedIR.semclass(UnifiedIR.ProvenanceCol) isa UnifiedIR.Annotation
    end

    @testset "collect_syntax!: AST-lifetime GC (§3.7 L2 step 3)" begin
        # Syntax-like graph: two "surface" trees with terminals, an
        # intermediate rewrite node, a macro frame, and garbage.
        function mkgraph()
            g = AttrGraph()
            ensure_attrcol!(g, :source)
            ensure_attrcol!(g, :macro_source)
            surf1 = newleaf(g, "demo.jl:1", 0x0031)         # terminal ref
            surf2 = newleaf(g, "demo.jl:2", 0x0032)
            frame = mkleaf(surf2)                            # will carry :macro_source
            setattrnode!(g, frame._id, :macro_source, surf1._id)
            mid  = mkleaf(Tree(g, frame._id))                # frame-less intermediate
            lo1  = mkleaf(surf1)                             # what the IR points at
            lo2  = mkleaf(Tree(g, mid._id))                  # chain: lo2->mid->frame->surf2
            garbage = newnode(g, "dead", 0x0039, tree_ids(newleaf(g, "dead2", 0x003a)))
            return g, surf1, surf2, frame, mid, lo1, lo2, garbage
        end
        function mkir(g, targets...)
            b = Builder(name = :m, cols = (source = UnifiedIR.ProvenanceCol(),))
            a = append_stmt!(b, K"region_arg"; type = Int64)
            local last = a
            for _ in targets
                last = append_stmt!(b, K"test.mul", last, last; type = Int64)
            end
            append_stmt!(b, K"return", last)
            ir = finish!(b)
            for (i, t) in enumerate(targets)
                setattr!(ir, i + 1, :source, t)
            end
            return ir
        end

        # (a) collecting with live IRs preserves every provenance walk
        g, surf1, surf2, frame, mid, lo1, lo2, garbage = mkgraph()
        ir1 = mkir(g, Tree(g, lo1._id))
        ir2 = mkir(g, Tree(g, lo2._id))          # two IRs share one graph
        walk(ir, i) = UnifiedIR.provenance_terminal(Tree(ir, i))
        pre1 = walk(ir1, 2); pre2 = walk(ir2, 2)
        n0 = nnodes(UnifiedIR.substrate(g))
        remap = collect_syntax!(g, (ir1, ir2))   # :conservative default
        @test nnodes(UnifiedIR.substrate(g)) < n0                # garbage collected
        @test remap[garbage._id] == 0
        @test walk(ir1, 2) == pre1 && walk(ir2, 2) == pre2       # byte-identical walks
        col1 = UnifiedIR.getattr(ir1, :source)
        @test col1[2] isa Tree                                    # values stay cursors
        @test UnifiedIR.syntax_graph(col1[2]) === g               # graph identity unchanged
        # conservative keeps the whole chain incl. frame + macro target
        @test remap[frame._id] != 0 && remap[mid._id] != 0 && remap[surf1._id] != 0

        # staleness contract: an IR NOT handed in is stale (RemapSet discipline)
        g, surf1, surf2, frame, mid, lo1, lo2, garbage = mkgraph()
        ir1 = mkir(g, Tree(g, lo1._id))
        ir3 = mkir(g, Tree(g, lo2._id))
        remap = collect_syntax!(g, (ir1,))                        # ir3 NOT subscribed
        stale = UnifiedIR.getattr(ir3, :source)[2]
        @test getfield(stale, :_id) == lo2._id                    # unrewritten...
        @test remap[lo2._id] == 0                                 # ...and its node was collected

        # (b) empty live set (module roots only / nothing) reclaims everything
        g, = mkgraph()
        collect_syntax!(g, ())
        @test nnodes(UnifiedIR.substrate(g)) == 0
        g, surf1 = mkgraph()
        collect_syntax!(g, (); extra_roots = (surf1,))            # module-level root
        @test nnodes(UnifiedIR.substrate(g)) == 1

        # :prune shortens through frame-less intermediates, keeps frames
        g, surf1, surf2, frame, mid, lo1, lo2, garbage = mkgraph()
        ir2 = mkir(g, Tree(g, lo2._id))
        pre = walk(ir2, 2)
        remap = collect_syntax!(g, (ir2,); policy = :prune)
        @test walk(ir2, 2) == pre                                 # terminal preserved
        @test remap[mid._id] == 0                                 # frame-less intermediate pruned
        @test remap[frame._id] != 0                               # macro frame kept
        @test remap[surf1._id] != 0                               # macro target kept
        @test_throws ArgumentError collect_syntax!(g, (); policy = :nonsense)
    end

    @testset "ONE substrate under IR and graphs (§3.7)" begin
        # The IR statement table IS an AttrGraph (plus IR-only columns);
        # a syntax graph is the same struct with tree conventions on top.
        b = Builder(name = :shared_demo)
        a = append_stmt!(b, K"region_arg"; type = Int64)
        y = append_stmt!(b, K"test.mul", a, a; type = Int64)
        append_stmt!(b, K"return", y)
        ir = finish!(b)

        core = getfield(ir.body, :graph)
        @test core isa AttrGraph
        g = AttrGraph()                       # graph-flavored instance
        @test typeof(g).name === typeof(core).name   # literally the same struct

        # the forwarded IRBody row properties ARE the AttrGraph fields
        @test ir.body.kind === core.kind
        @test ir.body.ops === core.ops
        @test ir.body.operands === core.operands
        @test Int(ir.body.len) == Int(core.len) == nstmts(ir)

        # both flavors speak the same word vocabulary over the same pool type
        n1 = newnode!(g); n2 = newnode!(g)
        setchildren!(g, n1, [n2])
        @test UnifiedIR.is_ops_inline(g.ops[n2])          # leaf = inline-mode word
        @test !UnifiedIR.is_ops_inline(g.ops[n1])         # children = pool-mode word
        @test typeof(g.operands) === typeof(core.operands)

        # and the same compaction machinery: compact! (IR) and compact_graph!
        # (graph) share the pool rebuild (append_remapped_range!) and the
        # §3.5 column hooks.
        ir2, _rs = compact!(ir)
        @test nstmts(ir2) == 3
        @test interpret(ir2, 6) == 36
        remap = compact_graph!(g, [n1])
        @test remap == Int32[1, 2]
    end
end
