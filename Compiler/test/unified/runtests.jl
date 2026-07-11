using Test
using UnifiedIR
import Compiler
const UnifiedCompiler = Compiler.load_unified!()
# bring the port's exported API into scope (the standalone package did this
# via `using UnifiedCompiler`)
for n in names(UnifiedCompiler)
    n === :Unified && continue
    @eval const $(n) = UnifiedCompiler.$(n)
end
const CC = Compiler

mysum(n) = begin s = 0; i = 1; while i <= n; s += i; i += 1; end; s end
myabs(x) = x < 0 ? -x : x
mypow(x, n) = begin r = 1; for _ in 1:n; r *= x; end; r end
branchy(x) = x > 10 ? "big" : x > 0 ? "small" : "neg"
tup(a, b) = (a + b, a - b)
strcat(s, n) = s * "!" ^ n
undefv(c) = begin local y; if c; y = 1; end; y end
fact(n) = n <= 1 ? 1 : n * fact(n - 1)
compute(x) = begin a = 2 + 3; b = a * x; t = (b, a); t[1] + t[2] end
double(x) = x + x
work(n) = begin s = 0; for i in 1:n; s += double(i); end; s end

@testset "converter differential (CodeInfo → UIR → CodeInfo → execute)" begin
    for (f, args, inputs) in [
        (mysum, (Int,), [(10,), (0,), (100,)]),
        (myabs, (Int,), [(5,), (-7,)]),
        (mypow, (Int, Int), [(2, 10), (3, 0)]),
        (branchy, (Int,), [(11,), (5,), (-1,)]),
        (tup, (Int, Int), [(3, 4)]),
        (strcat, (String, Int), [("hey", 3)]),
    ]
        g = UnifiedCompiler.redefine_through_ir(f, Tuple{args...})
        for inp in inputs
            @test isequal(f(inp...), Base.invokelatest(g, inp...))
        end
    end
    gu = UnifiedCompiler.redefine_through_ir(undefv, Tuple{Bool})
    @test Base.invokelatest(gu, true) == 1
    @test_throws UndefVarError Base.invokelatest(gu, false)
end

@testset "inference port (native on UnifiedIR)" begin
    st = UnifiedCompiler.UInferState()
    ir = UnifiedCompiler.lowered_ir(double, Tuple{Int64})
    @test UnifiedCompiler.infer_ir!(ir, Any[CC.Const(double), Int64]; state=st) == Int64
    ir = UnifiedCompiler.lowered_ir(branchy, Tuple{Int64})
    rt = UnifiedCompiler.infer_ir!(ir, Any[CC.Const(branchy), Int64]; state=st)
    @test rt == String
    ir = UnifiedCompiler.lowered_ir(mysum, Tuple{Int64})
    @test UnifiedCompiler.infer_ir!(ir, Any[CC.Const(mysum), Int64]; state=st) == Int64
    ir = UnifiedCompiler.lowered_ir(fact, Tuple{Int64})
    @test UnifiedCompiler.infer_ir!(ir, Any[CC.Const(fact), Int64]; state=st) == Int64
    # interprocedural const prop
    ir = UnifiedCompiler.lowered_ir(double, Tuple{Int64})
    rt = UnifiedCompiler.infer_ir!(ir, Any[CC.Const(double), CC.Const(21)]; state=st)
    @test rt isa CC.Const && rt.val == 42
end

@testset "optimizer port" begin
    st = UnifiedCompiler.UInferState()
    ir = UnifiedCompiler.lowered_ir(compute, Tuple{Int64})
    n0 = UnifiedIR.nstmts(ir)
    ir = UnifiedCompiler.optimize_ir!(ir, Any[CC.Const(compute), Int64]; state=st)
    @test UnifiedIR.nstmts(ir) < n0 ÷ 2      # cells + tuple + consts gone
    @test ir.meta[:rettype] == Int64
    g = UnifiedCompiler.define_ir_method!(@__MODULE__, gensym(:compute), 2, ir)
    @test Base.invokelatest(g, 7) == compute(7)
end

# --- optimizer parity upgrades (SROA / ADCE / inlining, §10.4) --------------

struct PtT; x::Int; y::Int; end
mutable struct MutT; v::Int; n::Int; end
getpt(a, b) = begin p = PtT(a, b); p.x + p.y end
mutuse(a, b) = begin m = MutT(a, 0); m.v = m.v + b; m.n = m.n + 1; m.v * m.n end
abstract type ShT end
struct C1T <: ShT; r::Float64; end
struct C2T <: ShT; s::Float64; end
sarea(c::C1T) = 3.0 * c.r * c.r
sarea(q::C2T) = q.s * q.s
pick(b) = b ? C1T(2.0) : C2T(3.0)
sareaof(b) = sarea(pick(b))
@inline trydiv(a, b) = try; div(a, b); catch; 0; end
usetrydiv(a, b) = trydiv(a, b) + 1
callpt(x) = getpt(x, x)

function opt_of(f, ats...)
    st = UnifiedCompiler.UInferState()
    ir = UnifiedCompiler.lowered_ir(f, Tuple{ats...})
    UnifiedCompiler.optimize_ir!(ir, Any[CC.Const(f), ats...]; state=st)
end
count_kind(ir, k) = count(s -> UnifiedIR.stmt_kind(ir, s) === k, collect(UnifiedIR.each_stmt(ir)))

@testset "SROA (immutable + mutable) and ADCE" begin
    # immutable-struct SROA: new + getfields fully forwarded, dead new removed
    ir = opt_of(getpt, Int, Int)
    @test count_kind(ir, UnifiedIR.@K_str("new")) == 0
    @test count_kind(ir, UnifiedIR.@K_str("extract")) == 0
    g = UnifiedCompiler.define_ir_method!(@__MODULE__, gensym(:getpt), 3, ir)
    @test Base.invokelatest(g, 7, 7) == getpt(7, 7)
    # mutable-struct SROA: allocation replaced by cells, then promoted away
    ir = opt_of(mutuse, Int, Int)
    @test count_kind(ir, UnifiedIR.@K_str("new")) == 0
    @test count_kind(ir, UnifiedIR.@K_str("cell")) == 0
    g = UnifiedCompiler.define_ir_method!(@__MODULE__, gensym(:mutuse), 3, ir)
    @test Base.invokelatest(g, 3, 4) == mutuse(3, 4)
end

@testset "inlining upgrades (multi-return, invoke, union split)" begin
    # multi-return callee (@inline try/catch) through the loop-wrapper
    ir = opt_of(usetrydiv, Int, Int)
    g = UnifiedCompiler.define_ir_method!(@__MODULE__, gensym(:usetrydiv), 3, ir)
    @test Base.invokelatest(g, 7, 2) == usetrydiv(7, 2)
    @test Base.invokelatest(g, 7, 0) == usetrydiv(7, 0)   # handler path
    # union split: isa-dispatch chain emitted, both arms inlined
    ir = opt_of(sareaof, Bool)
    @test count_kind(ir, UnifiedIR.@K_str("if")) >= 1
    g = UnifiedCompiler.define_ir_method!(@__MODULE__, gensym(:sareaof), 2, ir)
    @test Base.invokelatest(g, true) == sareaof(true)
    @test Base.invokelatest(g, false) == sareaof(false)
    # invoke-site inlining
    st = UnifiedCompiler.UInferState()
    ir = UnifiedCompiler.lowered_ir(callpt, Tuple{Int})
    for s in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.stmt_kind(ir, s) === UnifiedIR.@K_str("call") || continue
        UnifiedCompiler.static_operand_value(ir, UnifiedIR.getop(ir, s, 1)) === getpt || continue
        mi = CC.specialize_method(Base._which(Tuple{typeof(getpt),Int,Int};
                                              world = Base.get_world_counter()))
        UnifiedIR.replace_stmt!(ir, s, UnifiedIR.@K_str("invoke"),
                                UnifiedIR.vop(ir, mi), UnifiedIR.operands(ir, s)...;
                                type = UnifiedIR.stmt_type(ir, s))
        break
    end
    @test count_kind(ir, UnifiedIR.@K_str("invoke")) == 1
    ir = UnifiedCompiler.optimize_ir!(ir, Any[CC.Const(callpt), Int]; state=st)
    @test count_kind(ir, UnifiedIR.@K_str("invoke")) == 0
    g = UnifiedCompiler.define_ir_method!(@__MODULE__, gensym(:callpt), 2, ir)
    @test Base.invokelatest(g, 5) == callpt(5)
end

@testset "queries (§8.5)" begin
    @test UnifiedCompiler.infer_return(compute, Any[Int64]) == Int64
    rc = UnifiedCompiler.infer_return(compute, Any[CC.Const(4)])
    @test rc isa CC.Const && rc.val == compute(4)
    fx = UnifiedCompiler.effects_of(+, Any[Int64, Int64])
    @test fx.consistent && fx.effect_free && fx.nothrow && fx.terminates
    tir = UnifiedCompiler.typed_ir(compute, Any[Int64])
    @test tir isa UnifiedIR.IR && tir.meta[:rettype] == Int64
end

@testset "activation via CompilerPlugins (ordinary replacement mechanism)" begin
    @test with_unified_compiler(work, 10) == work(10)
    s = UnifiedCompiler.shadow_stats()
    @test s.seen > 0
    @test s.errors == 0
    @test s.verified == s.converted
end

# --- inference: InterConditional, SCC caching, LimitedAccuracy, concrete eval

checknothing_t(x) = x === nothing
intercond_f(x) = checknothing_t(x) ? 0 : x + 1
mutrec_a(n) = n <= 0 ? 0 : mutrec_b(n - 1) + 1
mutrec_b(n) = n <= 0 ? 1 : mutrec_a(n - 1) * 1
selfrec_in_cycle_a(n) = n <= 0 ? 0 : selfrec_in_cycle_b(n - 1) + 1
selfrec_in_cycle_b(n) = n <= 1 ? Int8(1) : (n % 2 == 0 ? selfrec_in_cycle_a(n - 1) : selfrec_in_cycle_b(n - 2))
totaladd(a, b) = a + b
concuser() = totaladd(20, 22)
for i in 1:19
    @eval $(Symbol(:chainf, i))(x) = $(Symbol(:chainf, i + 1))(x) + 1
end
chainf20(x) = x
chaindriver(x) = chainf1(x)

@testset "InterConditional (interprocedural Conditional return)" begin
    rt = UnifiedCompiler.infer_return(intercond_f, Any[Union{Int,Nothing}])
    @test rt == Int   # requires translating checknothing_t's conditional
end

@testset "SCC cycle results commit to the permanent cache" begin
    st = UnifiedCompiler.UInferState()
    ir = UnifiedCompiler.lowered_ir(mutrec_a, Tuple{Int64})
    rt = UnifiedCompiler.infer_ir!(ir, Any[CC.Const(mutrec_a), Int64]; state = st)
    @test CC.widenconst(rt) == Int64
    # both cycle members must be permanently cached after resolution
    cached = Set(mi.def.name for mi in keys(st.cache) if mi.def isa Method)
    @test :mutrec_a in cached || :mutrec_b in cached
    @test isempty(st.cycle_scratch)
    # a nested self-recursive member inside an outer cycle must converge to
    # the joint fixpoint (the self-edge's contribution, not one unrolling)
    st2 = UnifiedCompiler.UInferState()
    ir2 = UnifiedCompiler.lowered_ir(selfrec_in_cycle_a, Tuple{Int64})
    rt2 = UnifiedCompiler.infer_ir!(ir2, Any[CC.Const(selfrec_in_cycle_a), Int64]; state = st2)
    srt2 = Core.Compiler.return_type(selfrec_in_cycle_a, Tuple{Int64})
    @test srt2 <: CC.widenconst(rt2)   # never narrower than stock
end

@testset "LimitedAccuracy quarantine (depth-cutoff results never cached)" begin
    st = UnifiedCompiler.UInferState(UnifiedCompiler.UInferConfig(;
        max_depth = 8, native_fallback = false))
    ir = UnifiedCompiler.lowered_ir(chaindriver, Tuple{Int64})
    UnifiedCompiler.infer_ir!(ir, Any[CC.Const(chaindriver), Int64]; state = st)
    @test st.limited > 0    # the cutoff fired
    for mi in keys(st.cache)
        mi.def isa Method || continue
        @test !startswith(String(mi.def.name), "chainf")
    end
    for k in keys(st.constcache)
        mi = k[1]
        @test !(mi isa Core.MethodInstance && mi.def isa Method &&
                startswith(String(mi.def.name), "chainf"))
    end
    # an undamaged state infers the chain precisely
    st2 = UnifiedCompiler.UInferState()
    ir2 = UnifiedCompiler.lowered_ir(chaindriver, Tuple{Int64})
    rt2 = UnifiedCompiler.infer_ir!(ir2, Any[CC.Const(chaindriver), Int64]; state = st2)
    @test CC.widenconst(rt2) == Int64
end

@testset "concrete evaluation of total const calls" begin
    st = UnifiedCompiler.UInferState()
    ir = UnifiedCompiler.lowered_ir(concuser, Tuple{})
    rt = UnifiedCompiler.infer_ir!(ir, Any[CC.Const(concuser)]; state = st)
    @test rt isa CC.Const && rt.val == 42
end

@testset "structured-IR refinement soundness (backedge/throw-edge kills)" begin
    # A pre-loop constant store must not freeze in-loop reads of a cell the
    # loop body re-stores (the refinement is invalid on iterations ≥ 2).
    # Miscompiled as an infinite loop when this regressed: i = i + 1 was
    # const-folded to i = 2. Structured (region-form) IR only — islands get
    # this via per-block edge-map merging.
    b = UnifiedIR.Builder(name = :backedge_ref)
    UnifiedIR.append_stmt!(b, K"region_arg"; type = Any)   # #self#
    n = UnifiedIR.append_stmt!(b, K"region_arg"; type = Int64)
    ci = UnifiedIR.append_stmt!(b, K"cell", Int64; type = Any)
    UnifiedIR.append_stmt!(b, K"cell_set", ci, 1)
    lp = UnifiedIR.append_stmt!(b, K"loop"; type = Any)
    body = UnifiedIR.open_region!(b, lp; kind = UnifiedIR.REGION_LOOP_BODY)
    g = UnifiedIR.append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(ci); type = Any)
    g2 = UnifiedIR.append_stmt!(b, K"call", GlobalRef(Base, :add_int), g, 1; type = Any)
    UnifiedIR.append_stmt!(b, K"cell_set", ci, g2)
    cnd = UnifiedIR.append_stmt!(b, K"call", GlobalRef(Base, :sle_int), g2, n; type = Any)
    UnifiedIR.append_stmt!(b, K"continue", UnifiedIR.op_region(body), UnifiedIR.op_stmt(cnd))
    UnifiedIR.close_region!(b)
    fin = UnifiedIR.append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(ci); type = Any)
    UnifiedIR.append_stmt!(b, K"return", fin)
    ir = UnifiedIR.finish!(b)
    st = UnifiedCompiler.UInferState()
    rt = UnifiedCompiler.infer_ir!(ir, Any[Any, Int64]; state = st)
    @test CC.widenconst(rt) == Int64
    # the in-loop read must NOT be a constant
    @test !(UnifiedIR.stmt_type(ir, g) isa CC.Const)
    # end-to-end: optimize + execute; must terminate with the right answer
    ir = UnifiedCompiler.optimize_ir!(ir, Any[Any, Int64]; state = st)
    f = UnifiedCompiler.define_ir_method!(@__MODULE__, gensym(:backedge_ref), 2, ir)
    @test Base.invokelatest(f, 5) == 6

    # throw-edge: a handler must not see body-store refinements as facts
    tb = UnifiedIR.Builder(name = :throwedge_ref)
    UnifiedIR.append_stmt!(tb, K"region_arg"; type = Any)  # #self#
    a = UnifiedIR.append_stmt!(tb, K"region_arg"; type = Int64)
    c2 = UnifiedIR.append_stmt!(tb, K"cell", Int64; type = Any)
    UnifiedIR.append_stmt!(tb, K"cell_set", c2, 1)
    tr = UnifiedIR.append_stmt!(tb, K"try"; type = Any)
    UnifiedIR.open_region!(tb, tr; kind = UnifiedIR.REGION_BODY)
    UnifiedIR.append_stmt!(tb, K"cell_set", c2, 2)
    mayth = UnifiedIR.append_stmt!(tb, K"call", GlobalRef(Base, :sqrt), a; type = Any)
    UnifiedIR.append_stmt!(tb, K"cell_set", c2, 3)
    UnifiedIR.append_stmt!(tb, K"result", mayth)
    UnifiedIR.close_region!(tb)
    UnifiedIR.open_region!(tb, tr; kind = UnifiedIR.REGION_HANDLER)
    UnifiedIR.append_stmt!(tb, K"region_arg"; type = Any)
    hg = UnifiedIR.append_stmt!(tb, K"cell_get", UnifiedIR.op_stmt(c2); type = Any)
    UnifiedIR.append_stmt!(tb, K"result", hg)
    UnifiedIR.close_region!(tb)
    UnifiedIR.append_stmt!(tb, K"return", tr)
    ir2 = UnifiedIR.finish!(tb)
    UnifiedCompiler.infer_ir!(ir2, Any[Any, Int64]; state = st)
    # handler read joins {1,2,3}: must not be pinned to the last body store
    @test !(UnifiedIR.stmt_type(ir2, hg) isa CC.Const)
end

include("typed_exit.jl")
include("structurize.jl")
