# splice_body! tests (§4.2): the library-owned inlining primitive — bulk copy
# with remapping, world intersection, pool relocation, column class behavior,
# and the cross-universe hard error.

using UnifiedIR
using UnifiedIR: op_stmt, op_inline, op_region, op_sparam, StmtId, RegionId, Operand
using Test

# Annotation-class column for the ride-through test (same shape as the one in
# acceptance.jl, distinct name so both files load side by side).
struct SpMarkCol
    inner::SparseCol{Symbol}
end
SpMarkCol() = SpMarkCol(SparseCol{Symbol}())
UnifiedIR.semclass(::Type{SpMarkCol}) = UnifiedIR.Annotation()
UnifiedIR.col_grow!(::SpMarkCol, n::Integer, oldlen::Integer) = nothing
UnifiedIR.col_compact!(c::SpMarkCol, old_of_new::Vector{Int32}) =
    (UnifiedIR.col_compact!(c.inner, old_of_new); c)
UnifiedIR.col_clear!(c::SpMarkCol) = UnifiedIR.col_clear!(c.inner)
Base.getindex(c::SpMarkCol, s) = c.inner[s]
Base.setindex!(c::SpMarkCol, v, s) = (c.inner[s] = v)

# caller: f(a) = <splice point>(a) + 5, splice point is a placeholder call
function sp_mkcaller(; cols = UnifiedIR.NOCOLS, opaque::Bool = false)
    b = Builder(name = :caller, cols = cols)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    t = opaque ? append_stmt!(b, K"test.opaque", a; type = Int64) :
                 append_stmt!(b, K"call", :callee_placeholder, a; type = Int64)
    r = append_stmt!(b, K"test.add", t, 5; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    return ir, a, t
end

@testset "splice: straight-line pure callee at a call-shaped stmt" begin
    callee = let
        b = Builder(name = :sq)
        x = append_stmt!(b, K"region_arg"; type = Int64)
        y = append_stmt!(b, K"test.mul", x, x; type = Int64,
                         debug = (Int32(7), Int32(8), Int32(9)))
        z = append_stmt!(b, K"test.add", y, 1; type = Int64)
        append_stmt!(b, K"return", z)
        finish!(b)
    end
    ir, a, t = sp_mkcaller()
    @test any(c -> c === :callee_placeholder, ir.body.constants)
    editable(ir)
    retop = splice_body!(ir, t, callee; argmap = Operand[op_stmt(a)])
    @test UnifiedIR.optag(retop) == UnifiedIR.TAG_STMT
    ir, rs = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 3) == 15                      # (3*3+1)+5
    @test interpret(ir, -4) == 22
    # the splice point is gone, and its orphaned placeholder constant was
    # collected by compact!
    @test all(i -> stmt_kind(ir, StmtId(i)) != K"call", 1:nstmts(ir))
    @test !any(c -> c === :callee_placeholder, ir.body.constants)
    # debug tier: the spliced stmt carries the callee's codeloc triple
    muls = [i for i in 1:nstmts(ir) if stmt_kind(ir, StmtId(i)) == K"test.mul"]
    @test length(muls) == 1
    @test UnifiedIR.stmt_debug(ir, StmtId(muls[1])) == (Int32(7), Int32(8), Int32(9))
    # TODO(§3.6): inlined-at CHAINING of the debug tier needs a DebugInfo
    # stream in `ir.meta`; the core stores only per-stmt codeloc triples today.
end

@testset "splice: structured callee (loop + nested if), region remapping" begin
    # g(n) = loop(s=0, j=1) { s2 = s + (j>2 ? 2j : j); continue j<=n }, extract 1
    callee = let
        b = Builder(name = :structured)
        n = append_stmt!(b, K"region_arg"; type = Int64)
        r = build_loop!(b, 0, 1; type = Tuple{Int64,Int64}, argtypes = Any[Int64, Int64]) do b, args
            s, j = args
            c = append_stmt!(b, K"test.icmp", :sgt, j, 2; type = Bool)
            w = build_if!(b, c; type = Int64,
                          f_else = b -> append_stmt!(b, K"result", j)) do b
                d = append_stmt!(b, K"test.mul", j, 2; type = Int64)
                append_stmt!(b, K"result", d)
            end
            s2 = append_stmt!(b, K"test.add", s, w; type = Int64)
            j2 = append_stmt!(b, K"test.add", j, 1; type = Int64)
            cnd = append_stmt!(b, K"test.icmp", :sle, j2, n; type = Bool)
            body = UnifiedIR.current_region(b)
            append_stmt!(b, K"continue", op_region(body), op_stmt(cnd), op_stmt(s2), op_stmt(j2))
        end
        s = append_stmt!(b, K"extract", op_stmt(r), op_inline(1); type = Int64)
        append_stmt!(b, K"return", s)
        finish!(b)
    end
    @test verify_ir(callee; level = 1)
    expected5 = interpret(callee, 5)
    ir, a, t = sp_mkcaller(opaque = true)             # test.opaque splice point
    editable(ir)
    splice_body!(ir, t, callee; argmap = Operand[op_stmt(a)])
    ir, rs = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 5) == expected5 + 5           # differential vs the callee
    @test interpret(ir, 1) == interpret(callee, 1) + 5
    # callee is untouched (splice copies)
    @test verify_ir(callee; level = 1)
    @test interpret(callee, 5) == expected5
end

@testset "splice: world validity intersection" begin
    callee = let
        b = Builder(name = :w)
        x = append_stmt!(b, K"region_arg"; type = Int64)
        append_stmt!(b, K"return", x)
        finish!(b)
    end
    callee.valid_worlds = (UInt64(5), UInt64(100))
    ir, a, t = sp_mkcaller()
    ir.valid_worlds = (UInt64(2), UInt64(50))
    editable(ir)
    splice_body!(ir, t, callee; argmap = Operand[op_stmt(a)])
    @test ir.valid_worlds == (UInt64(5), UInt64(50))  # intersected, both ends
    ir, _ = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 4) == 9                       # identity callee: a + 5
end

@testset "splice: constant / global / sparam relocation" begin
    callee = let
        b = Builder(name = :cst)
        x = append_stmt!(b, K"region_arg"; type = Int64)
        s = append_stmt!(b, K"test.opaque", "spliced-const", x; type = String)
        g = append_stmt!(b, K"globalref", GlobalRef(Base, :pi); type = Any)
        p = append_stmt!(b, K"test.add", op_stmt(x), op_sparam(1); type = Int64)
        o = append_stmt!(b, K"test.opaque", p, s, g; type = Int64)   # returns p
        append_stmt!(b, K"return", o)
        finish!(b)
    end
    ir, a, t = sp_mkcaller(opaque = true)
    editable(ir)
    # unsubstituted static parameter is an error
    @test_throws "unsubstituted static parameter" splice_body!(
        ir, t, callee; argmap = Operand[op_stmt(a)])
    ir, a, t = sp_mkcaller(opaque = true)
    editable(ir)
    splice_body!(ir, t, callee; argmap = Operand[op_stmt(a)], sparams = Any[1000])
    ir, _ = compact!(ir)
    @test verify_ir(ir; level = 1)
    # pooled constant and global were relocated into the caller's pools
    @test any(c -> c === "spliced-const", ir.body.constants)
    @test any(g -> g == GlobalRef(Base, :pi), ir.body.globals)
    # sparam 1 was substituted with 1000: (4 + 1000) + 5
    @test interpret(ir, 4) == 1009
end

@testset "splice: universe mismatch is a hard error; convert_universe fixes it" begin
    callee = let
        b = Builder(name = :ucal, cols = (m = DenseCol{Int}(0),))
        x = append_stmt!(b, K"region_arg"; type = Int64)
        y = append_stmt!(b, K"test.mul", x, 3; type = Int64)
        append_stmt!(b, K"return", y)
        finish!(b)
    end
    ir, a, t = sp_mkcaller()                          # NOCOLS caller
    editable(ir)
    @test_throws "column universes differ" splice_body!(
        ir, t, callee; argmap = Operand[op_stmt(a)])
    # convert_universe: dropping a column requires explicit consent
    @test_throws "missing from target universe" convert_universe(NamedTuple(), callee)
    callee2 = convert_universe(NamedTuple(), callee; drop = (:m,))
    splice_body!(ir, t, callee2; argmap = Operand[op_stmt(a)])
    ir, _ = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 4) == 17                      # 4*3 + 5
end

@testset "splice: column values ride (Annotation), Derived invalidated" begin
    U() = (mark = SpMarkCol(), ci = SparseCol{Symbol}())
    callee = let
        b = Builder(name = :mc, cols = U())
        x = append_stmt!(b, K"region_arg"; type = Int64)
        y = append_stmt!(b, K"test.mul", x, 2; type = Int64)
        b.ir.body.cols.mark[y] = :from_callee
        append_stmt!(b, K"return", y)
        finish!(b)
    end
    b = Builder(name = :mcaller, cols = U())
    a = append_stmt!(b, K"region_arg"; type = Int64)
    t = append_stmt!(b, K"test.opaque", a; type = Int64)
    append_stmt!(b, K"return", t)
    ir = finish!(b)
    ir.body.cols.mark[a] = :caller_arg
    ir.body.cols.ci[t] = :stale_callinfo
    editable(ir)
    splice_body!(ir, t, callee; argmap = Operand[op_stmt(a)])
    ir, rs = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 21) == 42
    # Annotation values: caller's kept, callee's carried onto the spliced stmt
    @test ir.body.cols.mark[StmtId(rs.stmt[a.id])] === :caller_arg
    muls = [i for i in 1:nstmts(ir) if stmt_kind(ir, StmtId(i)) == K"test.mul"]
    @test length(muls) == 1
    @test ir.body.cols.mark[StmtId(muls[1])] === :from_callee
    # Derived values: conservatively invalidated by the splice (§3.5)
    @test all(i -> ir.body.cols.ci[StmtId(i)] === nothing, 1:nstmts(ir))
end

@testset "splice: shape preconditions" begin
    ir, a, t = sp_mkcaller()
    editable(ir)
    # argmap arity
    callee = let
        b = Builder(name = :two)
        x = append_stmt!(b, K"region_arg"; type = Int64)
        y = append_stmt!(b, K"region_arg"; type = Int64)
        z = append_stmt!(b, K"test.add", x, y; type = Int64)
        append_stmt!(b, K"return", z)
        finish!(b)
    end
    @test_throws "argmap length" splice_body!(ir, t, callee; argmap = Operand[op_stmt(a)])
    # callee must be dense/sealed
    ecallee = let
        b = Builder(name = :ed)
        x = append_stmt!(b, K"region_arg"; type = Int64)
        append_stmt!(b, K"return", x)
        finish!(b)
    end
    editable(ecallee)
    @test_throws "must be dense" splice_body!(ir, t, ecallee; argmap = Operand[op_stmt(a)])
    # multiple returns are rejected in v1 (inliner normalizes first)
    tworet = let
        b = Builder(name = :er)
        x = append_stmt!(b, K"region_arg"; type = Bool)
        s = build_if!(b, x; type = Int64, f_else = b -> append_stmt!(b, K"result", 0)) do b
            append_stmt!(b, K"return", 1)
        end
        append_stmt!(b, K"return", s)
        finish!(b)
    end
    @test_throws "returns" splice_body!(ir, t, tworet; argmap = Operand[op_stmt(StmtId(1))])
    # a single but non-root (early) return is likewise rejected in v1
    earlyret = let
        b = Builder(name = :er2)
        x = append_stmt!(b, K"region_arg"; type = Bool)
        s = build_if!(b, x; type = Int64, f_else = b -> append_stmt!(b, K"result", 0)) do b
            append_stmt!(b, K"return", 1)
        end
        append_stmt!(b, K"unreachable")
        finish!(b)
    end
    @test_throws "early return" splice_body!(ir, t, earlyret; argmap = Operand[op_stmt(StmtId(1))])
end
