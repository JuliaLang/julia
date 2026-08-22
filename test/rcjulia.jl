# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for RCJulia — restricted-capability evaluation mode
# (`Core._rcjulia_call`): an execution mode that is `:foldable` by
# construction. Operations requiring a withheld capability throw
# CapabilityError instead of executing.

using Test

pcall(f, args...) = Core._rcjulia_call(Base.get_world_counter(), f, args...)

# Helpers defined at toplevel so they have stable method identities.
rc_add(x, y) = x + y
rc_tuplen(t) = nfields(t)
struct RCJuliaParams{R,C} end
rc_paramprod(::Type{P}) where {P} = (P.parameters[1]::Int) * (P.parameters[2]::Int)
rc_throws(x) = x < 0 ? throw(DomainError(x, "negative")) : x
rc_loop(n) = (s = 0; for i = 1:n; s += i; end; s)
rc_splat(t) = tuple(t...)
rc_gen(::Val{N}) where {N} = ntuple(identity, Val(N))
rc_reads_global() = RCJULIA_NONCONST_GLOBAL
rc_reads_const() = RCJULIA_CONST_GLOBAL
rc_closure_call(f, x) = f(x)
rc_nested_call(w, f) = Core._rcjulia_call(w, f)

# recursion shapes, exercising the well-founded order
rc_countdown(n) = n <= 0 ? 0 : rc_countdown(n - 1)            # bits value decreases
rc_countup(n) = n >= 10 ? n : rc_countup(n + 1)               # bits value increases
rc_same(x) = rc_same(x)                                       # egal re-entry
rc_below_zero(n) = n == -3 ? 0 : rc_below_zero(n - 1)         # crosses into negative bits
rc_fact(n) = n <= 1 ? 1 : n * rc_fact(n - 1)
rc_peel(t) = t === () ? 0 : rc_peel(Base.tail(t))             # typeof(t) shrinks structurally
rc_val_descent(::Val{0}) = 0
rc_val_descent(::Val{N}) where {N} = rc_val_descent(Val(N - 1)) # type parameter decreases
rc_ttail(::Type{T}) where {T<:Tuple} = Tuple{Base.argtail(T.parameters...)...}
rc_tdesc(::Type{T}) where {T<:Tuple} =                        # type argument shrinks structurally
    T === Tuple{} ? 0 : 1 + rc_tdesc(rc_ttail(T))
rc_mutual_a(n) = n <= 0 ? 0 : rc_mutual_b(n)
rc_mutual_b(n) = rc_mutual_a(n - 1)

global RCJULIA_NONCONST_GLOBAL::Int = 1
const RCJULIA_CONST_GLOBAL = 42

mutable struct RCJuliaMutable
    x::Int
end
struct RCJuliaImmutable
    x::Int
end
mutable struct RCJuliaMutableConst
    const c::Int
    x::Int
end

# Two generations of a method, with the world in between captured (at
# toplevel, so later statements see the redefinition).
rc_redefined() = 1
const RCJULIA_WORLD1 = Base.get_world_counter()
rc_redefined() = 2
const RCJULIA_WORLD2 = Base.get_world_counter()

@testset "basic evaluation" begin
    @test pcall(rc_add, 1, 2) === 3
    @test pcall(rc_add, 1.5, 2.5) === 4.0
    @test pcall(tuple, 1, 2) === (1, 2)
    @test pcall(rc_tuplen, (1, 2, 3)) === 3
    @test pcall(typeof, 1) === Int
    @test pcall(===, 1, 1) === true
    # construction of immutables is legal
    @test pcall(RCJuliaImmutable, 7) === RCJuliaImmutable(7)
    @test pcall(getfield, RCJuliaImmutable(7), :x) === 7
    # type computation
    @test pcall(Core.apply_type, NTuple, 3, Float64) === NTuple{3,Float64}
    @test pcall(fieldtype, RCJuliaImmutable, 1) === Int
end

@testset "const fields of mutable objects are readable" begin
    # `const` fields never change after construction, so reading them is
    # consistent; this is also what admits set-once runtime metadata such as
    # DataType.parameters (the computed-field-types use case)
    @test pcall(rc_paramprod, RCJuliaParams{2,3}) === 6
    let m = RCJuliaMutableConst(1, 2)
        @test pcall(getfield, m, :c) === 1
        @test pcall(isdefined, m, :c) === true
        @test_throws CapabilityError pcall(getfield, m, :x)
        @test_throws CapabilityError pcall(isdefined, m, :x)
    end
end

@testset "tuple splatting and generated functions" begin
    @test pcall(rc_splat, (1, 2, 3)) === (1, 2, 3)
    # generated function expansion is always permitted; the expanded code
    # then executes under the mode
    @test pcall(rc_gen, Val(3)) === (1, 2, 3)
    # splatting an array reads mutable memory
    let a = [1, 2, 3]
        @test_throws CapabilityError pcall(rc_splat, a)
    end
end

@testset "consistent throwing is permitted" begin
    @test pcall(rc_throws, 1) === 1
    @test_throws DomainError pcall(rc_throws, -1)
    @test_throws DivideError pcall(div, 1, 0)
    # after an exception propagates out, the mode is fully exited
    @test RCJULIA_NONCONST_GLOBAL === 1
end

@testset "mutable memory reads are trapped" begin
    let m = RCJuliaMutable(1)
        @test_throws CapabilityError pcall(getfield, m, :x)
        @test_throws CapabilityError pcall(isdefined, m, :x)
    end
    let a = [1, 2, 3]
        @test_throws CapabilityError pcall(getindex, a, 1)
        @test_throws CapabilityError pcall(length, a)
    end
end

@testset "mutation and mutable allocation are trapped" begin
    let m = RCJuliaMutable(1)
        @test_throws CapabilityError pcall(setfield!, m, :x, 2)
        @test m.x === 1
    end
    @test_throws CapabilityError pcall(RCJuliaMutable, 1)
    @test_throws CapabilityError pcall(Vector{Int}, undef, 3)
    @test_throws CapabilityError pcall(push!, Int[], 1)
    # string building goes through IOBuffer/Memory
    @test_throws CapabilityError pcall(string, 1, "x")
end

@testset "globals" begin
    @test pcall(rc_reads_const) === 42
    @test pcall(getglobal, @__MODULE__, :RCJULIA_CONST_GLOBAL) === 42
    @test_throws CapabilityError pcall(rc_reads_global)
    @test_throws CapabilityError pcall(getglobal, @__MODULE__, :RCJULIA_NONCONST_GLOBAL)
    @test_throws CapabilityError pcall(setglobal!, @__MODULE__, :RCJULIA_NONCONST_GLOBAL, 2)
    @test pcall(isdefinedglobal, @__MODULE__, :RCJULIA_CONST_GLOBAL) === true
    @test pcall(isdefinedglobal, @__MODULE__, :this_name_is_not_defined_anywhere) === false
    @test_throws CapabilityError pcall(isdefinedglobal, @__MODULE__, :RCJULIA_NONCONST_GLOBAL)
    @test_throws UndefVarError pcall(getglobal, @__MODULE__, :this_name_is_not_defined_anywhere)
end

@testset "well-founded recursion" begin
    # loops have backedges: iteration must be expressed as recursion
    @test_throws CapabilityError pcall(rc_loop, 3)
    # value recursion with decreasing bits is permitted
    @test pcall(rc_countdown, 5) === 0
    @test pcall(rc_fact, 5) === 120
    # increasing bits is not a decrease in the order
    @test_throws CapabilityError pcall(rc_countup, 0)
    # egal re-entry is guaranteed divergent under consistency
    @test_throws CapabilityError pcall(rc_same, 1)
    # the bits order is unsigned: -1 is larger than 0
    @test_throws CapabilityError pcall(rc_below_zero, 0)
    # structurally smaller types: tuple peeling and type-argument descent
    @test pcall(rc_peel, (1, 2, 3)) === 0
    @test pcall(rc_tdesc, Tuple{Int,Int,Int}) === 3
    # descent through distinct specializations (decreasing type parameter)
    @test pcall(rc_val_descent, Val(10)) === 0
    # mutual recursion, each method decreasing per occurrence
    @test pcall(rc_mutual_a, 5) === 0
    # the physical frame cap fires deterministically before stack overflow
    @test_throws CapabilityError pcall(rc_val_descent, Val(2000))
    let err = try pcall(rc_val_descent, Val(2000)); nothing catch e; e end
        @test err isa CapabilityError && err.op === :depth_limit
    end
    let err = try pcall(rc_countup, 0); nothing catch e; e end
        @test err isa CapabilityError && err.op === :recursion
    end
end

@testset "world-mutating and impure operations are trapped" begin
    @test_throws CapabilityError pcall(Core.eval, @__MODULE__, :(1 + 1))
    @test_throws CapabilityError pcall(Core.invokelatest, rc_add, 1, 2)
    @test_throws CapabilityError pcall(Core.finalizer, identity, RCJuliaMutable(1))
    @test_throws CapabilityError pcall(Core.current_scope)
    @test_throws CapabilityError pcall(Core._expr, :call, :f)
    @test_throws CapabilityError pcall(print, "hello")
    @test_throws CapabilityError pcall(time_ns)
end

@testset "world freezing" begin
    # a frozen world sees the definitions of that world
    @test Core._rcjulia_call(RCJULIA_WORLD1, rc_redefined) === 1
    @test Core._rcjulia_call(RCJULIA_WORLD2, rc_redefined) === 2
    @test pcall(rc_redefined) === 2
    # nesting may lower the frozen world...
    @test Core._rcjulia_call(RCJULIA_WORLD2, rc_nested_call, RCJULIA_WORLD1, rc_redefined) === 1
    # ...but not raise it
    @test_throws CapabilityError Core._rcjulia_call(RCJULIA_WORLD1, rc_nested_call, RCJULIA_WORLD2, rc_redefined)
end

@testset "closures and higher-order calls" begin
    @test pcall(rc_closure_call, x -> x + 1, 1) === 2
    @test pcall(map, x -> 2x, (1, 2, 3)) === (2, 4, 6)
end

@testset "CapabilityError" begin
    @test CapabilityError <: Exception
    err = try
        pcall(rc_loop, 1)
        nothing
    catch e
        e
    end
    @test err isa CapabilityError
    @test err.op === :backedge
    @test occursin("RCJulia", sprint(showerror, err))
    # the trap is deterministic; the exception is egal across replays
    err2 = try pcall(rc_loop, 1); nothing catch e; e end
    @test err === err2
end

@testset "every builtin and intrinsic has decided RCJulia semantics" begin
    # This test is the audited classification of ALL builtins and intrinsics
    # for RCJulia mode. When adding a builtin or intrinsic, you must decide
    # its RCJulia semantics and add it to exactly one set below (and, for
    # trapped operations, add the corresponding `jl_check_rc` in the runtime).

    # Builtins that execute under the mode (their semantics are consistent in
    # a frozen world, or their C implementation performs no restricted
    # operation).
    BUILTIN_ALLOWED = Set([
        "_compute_sparams", "_rcjulia_call", "_svec_len", "_svec_ref",
        "_typevar", "applicable", "apply_type", "bitsizeof", "compilerbarrier",
        "donotdelete", "fieldtype", "get_binding_type", "===", "isa", "<:",
        "ifelse", "intrinsic_call", "memoryrefnew", "memoryrefoffset",
        "nfields", "sizeof", "svec", "throw", "throw_methoderror", "tuple",
        "typeassert", "typeof", "has_free_typevars",
    ])
    # Builtins that execute conditionally: legal on immutable/`const`/
    # tuple-splat operands, CapabilityError otherwise.
    BUILTIN_CONDITIONAL = Set([
        "getfield", "getglobal", "isdefined", "isdefinedglobal",
        "_apply_iterate",
    ])
    # Builtins that always throw CapabilityError under the mode.
    BUILTIN_TRAPPED = Set([
        "_abstracttype", "_call_in_world_total", "_equiv_typedef", "_expr",
        "_import", "_new_cancel_source", "_primitivetype", "_setsuper!",
        "_structtype", "_task", "_typebody!", "_using", "cancellation_point!",
        "current_scope", "declare_const", "declare_global", "define_method",
        "finalizer", "invoke", "invoke_in_world", "invokelatest", "memorynew",
        "memoryref_isassigned", "memoryrefget", "memoryrefmodify!",
        "memoryrefreplace!", "memoryrefset!", "memoryrefsetonce!",
        "memoryrefunset!", "memoryrefswap!", "modifyfield!", "modifyglobal!",
        "opaque_closure_call", "replacefield!", "replaceglobal!", "setfield!",
        "setfieldonce!", "setglobal!", "setglobalonce!", "swapfield!",
        "swapglobal!", "task_result_type",
    ])
    classified = union(BUILTIN_ALLOWED, BUILTIN_CONDITIONAL, BUILTIN_TRAPPED)
    @test isempty(intersect(BUILTIN_ALLOWED, BUILTIN_TRAPPED))
    @test isempty(intersect(BUILTIN_CONDITIONAL, BUILTIN_TRAPPED))
    # every builtin reachable as a Core binding is classified; classification
    # is by function identity (via the canonical creation name), so alias
    # bindings such as `Core.getproperty === Core.getfield` and
    # `Core._call_latest === Core.invokelatest` inherit their target's semantics
    for n in names(Core; all=true)
        isdefined(Core, n) || continue
        f = getglobal(Core, n)
        f isa Core.Builtin || continue
        @test string(nameof(f)) in classified
    end
    # and the classification exactly matches the full builtin table when the
    # runtime sources are available (covers builtins not bound in Core, such
    # as opaque_closure_call)
    proto = joinpath(@__DIR__, "..", "src", "builtin_proto.h")
    if isfile(proto)
        tbl = Set(m.captures[1] for m in eachmatch(r"XX\(\w+,\s*\"([^\"]+)\"\)", read(proto, String)))
        @test tbl == classified
    end

    # Intrinsics that always throw CapabilityError under the mode: pointer
    # and atomic memory access, machine queries, library symbol lookup, and
    # native-code execution.
    INTRINSIC_TRAPPED = Set([
        :pointerref, :pointerset, :atomic_fence, :atomic_pointerref,
        :atomic_pointerset, :atomic_pointerswap, :atomic_pointermodify,
        :atomic_pointerreplace, :cglobal, :have_fma, :llvmcall,
    ])
    # All other intrinsics are value-deterministic and allowed. (The
    # fast-math aliases and `muladd_float` share the strict runtime
    # implementations, so they are deterministic under interpretation; if the
    # mode ever compiles natively they must be pinned to those semantics.
    # The hidden `cglobal_auto` shares `cglobal`'s trap.)
    INTRINSIC_ALLOWED = Set([
        :abs_float, :add_float, :add_float_fast, :add_int, :add_ptr, :and_int,
        :ashr_int, :bitcast, :bswap_int, :ceil_llvm, :checked_sadd_int,
        :checked_sdiv_int, :checked_smul_int, :checked_srem_int,
        :checked_ssub_int, :checked_uadd_int, :checked_udiv_int,
        :checked_umul_int, :checked_urem_int, :checked_usub_int,
        :copysign_float, :ctlz_int, :ctpop_int, :cttz_int, :div_float,
        :div_float_fast, :eq_float, :eq_float_fast, :eq_int, :flipsign_int,
        :floor_llvm, :fma_float, :fpext, :fpiseq, :fptosi, :fptoui, :fptrunc,
        :le_float, :le_float_fast, :lshr_int, :lt_float, :lt_float_fast,
        :max_float, :max_float_fast, :min_float, :min_float_fast, :mul_float,
        :mul_float_fast, :mul_int, :muladd_float, :ne_float, :ne_float_fast,
        :ne_int, :neg_float, :neg_float_fast, :neg_int, :not_int, :or_int,
        :rint_llvm, :sdiv_int, :sext_int, :shl_int, :sitofp, :sle_int,
        :slt_int, :sqrt_llvm, :sqrt_llvm_fast, :srem_int, :sub_float,
        :sub_float_fast, :sub_int, :sub_ptr, :trunc_int, :trunc_llvm,
        :udiv_int, :uitofp, :ule_int, :ult_int, :urem_int, :xor_int,
        :zext_int,
    ])
    @test isempty(intersect(INTRINSIC_ALLOWED, INTRINSIC_TRAPPED))
    intr_classified = union(INTRINSIC_ALLOWED, INTRINSIC_TRAPPED)
    for n in names(Core.Intrinsics; all=true)
        isdefined(Core.Intrinsics, n) || continue
        getglobal(Core.Intrinsics, n) isa Core.IntrinsicFunction || continue
        @test n in intr_classified
    end

    # spot-check that the trapped intrinsics actually trap (before touching
    # their operands)
    p = Ptr{Int}(0)
    @test_throws CapabilityError pcall(Core.Intrinsics.pointerref, p, 1, 1)
    @test_throws CapabilityError pcall(Core.Intrinsics.pointerset, p, 1, 1, 1)
    @test_throws CapabilityError pcall(Core.Intrinsics.atomic_pointerref, p, :monotonic)
    @test_throws CapabilityError pcall(Core.Intrinsics.atomic_fence, :seq_cst, :system)
    @test_throws CapabilityError pcall(Core.Intrinsics.have_fma, Float64)
    @test_throws CapabilityError pcall(Core.Intrinsics.cglobal, :jl_n_threads, Int)
    @test_throws CapabilityError pcall(Core.Intrinsics.llvmcall)
end

# quoted `Expr` literals materialize fresh mutable ASTs (copyast) and trap;
# quoted immutables (QuoteNode of a Symbol) are plain values and are fine
rc_quoted_expr() = :(1 + 2)
rc_quoted_sym() = :a_symbol
@testset "quoted AST literals" begin
    @test_throws CapabilityError pcall(rc_quoted_expr)
    @test pcall(rc_quoted_sym) === :a_symbol
end
