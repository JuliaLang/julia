# This file is a part of Julia. License is MIT: https://julialang.org/license

############################
# nothrow shadow synthesis #
############################
#
# For a method instance `mi` with body `f`, its "nothrow shadow" is a pair of
# derived code instances cached on `mi` itself under non-default cache owners:
#
#  - the CHECK (`NothrowCheckOwner`): same arguments as `mi`, returns `Bool`;
#    `true` guarantees that calling `mi` with these arguments does not throw.
#    It is effect-free, nothrow and terminating, so callers may freely insert
#    (or delete) it.
#  - the ASSUME variant (`NothrowAssumeOwner`): the body of `mi` simplified
#    under the assumption that it does not throw (must-throw paths removed,
#    checked inner calls devirtualized to their own assume variants). Callers
#    may only execute it when the check returned `true`.
#
# Both are derived from the UNTRANSFORMED post-inference IR (before inlining
# has expanded anything), early in the optimization pipeline of `mi` itself -
# the only point where that representation naturally exists. Derivation is
# compositional at call granularity: a may-throw inner call is replaced by a
# guarded call of the callee's own shadow pair (looked up from the cache; the
# recursion grounds at a small table of builtin bounds-check formulas). The
# derived IR is cached as `CodeInstance`s whose `inferred` field holds the
# optimized `IRCode` directly (they are consumed by inlining only and are
# never invoked or compiled standalone).
#
# The effect-split inliner (`analyze_invoke_split_effects_synthesis`) prefers
# these cached shadows and falls back to the post-optimization synthesis
# (`synthesize_nothrow_check_ir`/`assume_nothrow_ir`) when they are absent.

# NB: bootstrap-safe (raw ccalls only; Base string operations are not
# available while the compiler compiles itself)
function get_bool_env_shadow()
    p = ccall(:getenv, Ptr{UInt8}, (Ptr{UInt8},), "JULIA_NOTHROW_SHADOW")
    p == C_NULL && return true
    return ccall(:strcmp, Cint, (Ptr{UInt8}, Ptr{UInt8}), p, "0") != 0
end
function get_int_env_shadow_max()
    p = ccall(:getenv, Ptr{UInt8}, (Ptr{UInt8},), "JULIA_NOTHROW_SHADOW_MAX")
    p == C_NULL && return -1
    return Int(ccall(:strtol, Clong, (Ptr{UInt8}, Ptr{Ptr{UInt8}}, Cint), p, C_NULL, 10))
end

struct NothrowCheckOwner end
struct NothrowAssumeOwner end

function lookup_shadow(@nospecialize(owner), mi::MethodInstance, world::UInt)
    ci = ccall(:jl_rettype_inferred, Any, (Any, Any, UInt, UInt), owner, mi, world, world)
    ci === nothing && return nothing
    ci = ci::CodeInstance
    inferred = @atomic :monotonic ci.inferred
    inferred isa CodeInfo || return nothing
    return ci
end

# Inflate the shadow IR stored on a shadow `CodeInstance` (stored as
# `CodeInfo` so that codegen can compile any shadow invoke that survives into
# final code)
function shadow_ir(ci::CodeInstance)
    src = (@atomic :monotonic ci.inferred)::CodeInfo
    return inflate_ir(src, ci.def::MethodInstance) # (inflate_ir copies)
end

# The result of a shadow derivation, stashed on the `OptimizationState` and
# cached as `CodeInstance`s by `finish!` once the validity worlds and edges of
# the real code instance are known
struct NothrowShadow
    check::IRCode
    assume::IRCode
end

# Maximum size of a method body we attempt to derive a shadow for
const SHADOW_STMT_BUDGET = 1000

##########################
# derivation: the walker #
##########################

# Bail out of derivation via exception-free early return: the builder returns
# `nothing` whenever it encounters a statement it cannot classify.

mutable struct ShadowBuilder
    # linear statement buffers (block targets use FINAL block indices, patched
    # in `finish_shadow_ir!`)
    stmts::Vector{Any}
    types::Vector{Any}
    infos::Vector{CallInfo}
    flags::Vector{UInt32}
    # start statement index of each emitted block
    bbstarts::Vector{Int}
    # source block -> (entry, exit) emitted block index (a source block may be
    # split by inserted guards)
    bbentry::Vector{Int}
    bbexit::Vector{Int}
    # ssa remapping: source SSAValue id -> emitted SSAValue id (0 = unmapped)
    ssamap::Vector{Int}
    # emitted GotoIfNot statements whose target is the shared `return false`
    # block (patched at the end)
    falsebranches::Vector{Int}
end

function ShadowBuilder(nsrcstmts::Int, nsrcblocks::Int)
    return ShadowBuilder(Any[], Any[], CallInfo[], UInt32[],
        Int[1], zeros(Int, nsrcblocks), zeros(Int, nsrcblocks),
        zeros(Int, nsrcstmts), Int[])
end

function emit!(builder::ShadowBuilder, @nospecialize(stmt), @nospecialize(typ),
               info::CallInfo, flag::UInt32)
    push!(builder.stmts, stmt)
    push!(builder.types, typ)
    push!(builder.infos, info)
    push!(builder.flags, flag)
    return length(builder.stmts)
end

function start_block!(builder::ShadowBuilder)
    push!(builder.bbstarts, length(builder.stmts) + 1)
    return length(builder.bbstarts)
end

current_block(builder::ShadowBuilder) = length(builder.bbstarts)

# Remap a source-IR value to the emitted IR
function remap_value(builder::ShadowBuilder, @nospecialize(v))
    if isa(v, SSAValue)
        newid = builder.ssamap[v.id]
        newid == 0 && return nothing # forward reference we did not emit; bail
        return SSAValue(newid)
    end
    return v
end

function remap_stmt(builder::ShadowBuilder, @nospecialize(stmt))
    (isa(stmt, PhiNode) || isa(stmt, GotoNode) || isa(stmt, GotoIfNot)) &&
        return stmt # handled by their emitters
    # NB: `userefs` mutates `Expr`s in place; the source IR must stay intact
    isa(stmt, Expr) && (stmt = copy(stmt))
    urs = userefs(stmt)
    for useref in urs
        v = useref[]
        if isa(v, SSAValue)
            nv = remap_value(builder, v)
            nv === nothing && return nothing
            useref[] = nv
        end
    end
    return urs[]
end

# Kept calls inherit their inference `CallInfo` so the flattening inliner can
# resolve them, but infos can carry VOLATILE inlining sources (an inlined
# `SemiConcreteResult.ir` is consumed destructively): the shadow must own
# copies, or flattening would corrupt the source the real method's own
# inlining pass still needs.
function sanitize_shadow_info(info::CallInfo)
    if isa(info, MethodMatchInfo)
        needscopy = false
        for r in info.call_results
            isa(r, SemiConcreteResult) && (needscopy = true; break)
        end
        needscopy || return info
        new_info = MethodMatchInfo(info.results, info.mt, info.atype, info.fullmatch)
        copyto!(new_info.edges, info.edges)
        for i = 1:length(info.call_results)
            r = info.call_results[i]
            if isa(r, SemiConcreteResult)
                r = SemiConcreteResult(r.edge, copy(r.ir), r.effects, r.spec_info)
            end
            new_info.call_results[i] = r
        end
        return new_info
    end
    isa(info, NoCallInfo) && return info
    # unknown info kinds may carry volatile sources we cannot see; drop them
    # (the flatten then outlines such calls instead of inlining them)
    return NoCallInfo()
end

# A conservative classification of the throwing builtins the derivation knows
# how to synthesize an explicit bounds condition for. Returns the emitted
# `Bool` condition SSA, or `nothing` if the builtin is not supported.
function emit_builtin_check!(builder::ShadowBuilder, ir::IRCode, @nospecialize(stmt))
    isexpr(stmt, :call) || return nothing
    length(stmt.args) >= 1 || return nothing
    ft = argextype(stmt.args[1], ir)
    f = singleton_type(ft)
    if f === Core.memoryrefnew && length(stmt.args) == 4
        # memoryrefnew(ref, i, boundscheck): in bounds iff
        # 1 <= memoryrefoffset(ref) + i - 1 <= length(ref.mem)
        refarg = remap_value(builder, stmt.args[2])
        iarg = remap_value(builder, stmt.args[3])
        (refarg === nothing || iarg === nothing) && return nothing
        reft = widenconst(argextype(stmt.args[2], ir))
        (isa(reft, DataType) && reft <: GenericMemoryRef) || return nothing
        it = widenconst(argextype(stmt.args[3], ir))
        it === Int || return nothing
        off = emit!(builder, Expr(:call, Core.memoryrefoffset, refarg), Int,
                    NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_NOUB)
        mem = emit!(builder, Expr(:call, Core.getfield, refarg, QuoteNode(:mem)),
                    memtype_of_ref(reft), NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_NOUB)
        len = emit!(builder, Expr(:call, Core.getfield, SSAValue(mem), QuoteNode(:length)),
                    Int, NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_NOUB)
        # zero-based index: memoryrefoffset(ref) + i - 2, unsigned-compared to
        # the length (folds the two-sided check into one comparison)
        offi = emit!(builder, Expr(:call, Core.Intrinsics.add_int, SSAValue(off), iarg),
                     Int, NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_NOUB)
        zi = emit!(builder, Expr(:call, Core.Intrinsics.sub_int, SSAValue(offi), 2),
                   Int, NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_NOUB)
        uzi = emit!(builder, Expr(:call, Core.Intrinsics.bitcast, UInt, SSAValue(zi)),
                    UInt, NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_NOUB)
        ulen = emit!(builder, Expr(:call, Core.Intrinsics.bitcast, UInt, SSAValue(len)),
                     UInt, NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_NOUB)
        cond = emit!(builder, Expr(:call, Core.Intrinsics.ult_int, SSAValue(uzi), SSAValue(ulen)),
                     Bool, NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_NOUB)
        return SSAValue(cond)
    end
    return nothing
end

function memtype_of_ref(@nospecialize(reft))
    if isa(reft, DataType) && reft <: GenericMemoryRef && length(reft.parameters) == 3
        kind, T, AS = reft.parameters
        if !isa(kind, TypeVar) && !isa(T, TypeVar) && !isa(AS, TypeVar)
            return GenericMemory{kind, T, AS}
        end
    end
    return GenericMemory
end

# Resolve a `:call` statement to a unique, fully-covering method instance via
# its inference-derived call info, or `nothing`
function resolve_unique_call(ir::IRCode, idx::Int)
    info = ir.stmts[idx][:info]
    # an `invoke_split_effects` statement carries the info of the equivalent
    # plain call (possibly wrapped when a user precondition matched)
    isa(info, InvokeSplitEffectsInfo) && (info = info.info)
    if isa(info, MethodMatchInfo)
        results = info.results
        length(results) == 1 || return nothing
        match = results[1]
        match.fully_covers || return nothing
        return specialize_method(match)
    end
    return nothing
end

# After passing the guard, the assume-variant call keeps the callee's effects
# with nothrow (and, since v1 derivation bails on stores, effect-freedom)
const SHADOW_GUARDED_FLAGS = IR_FLAG_NOTHROW | IR_FLAG_EFFECT_FREE | IR_FLAG_TERMINATES | IR_FLAG_INLINE

"""
    derive_nothrow_shadow(ir::IRCode, sv::OptimizationState) -> Union{Nothing,NothrowShadow}

Derive the nothrow shadow pair for the (early, untransformed) `ir` of the
method instance being optimized, composing cached shadows of inner calls.
Returns `nothing` whenever any statement cannot be classified (may-throw call
without a cached shadow, side effects other than deletable ones, exception
handlers, ...).
"""
function derive_nothrow_shadow(ir::IRCode, sv::OptimizationState)
    length(ir.stmts) <= SHADOW_STMT_BUDGET || return nothing
    world = get_inference_world(sv.inlining.interp)
    check = derive_shadow_variant(ir, sv, world, #=check=#true)
    check === nothing && return nothing
    assume = derive_shadow_variant(ir, sv, world, #=check=#false)
    assume === nothing && return nothing
    if is_asserts()
        verify_ir(check, true, false, optimizer_lattice(sv.inlining.interp), sv.linfo)
        verify_ir(assume, true, false, optimizer_lattice(sv.inlining.interp), sv.linfo)
    end
    # Flatten: inline the (inline-flagged) callee-shadow invokes the walker
    # emitted, so cached shadows are self-contained and consumers need no
    # second inlining round. Edges for the inlined shadows accrue to the real
    # method being optimized, which is exactly right for invalidation.
    # NB: use a FRESH inlining state (sharing the edge list, but with an empty
    # optimizer cache): the shared state's resolver may destructively consume
    # in-flight local inference sources that the real method's own inlining
    # pass still needs.
    flatten_state = InliningState(sv.inlining.edges, sv.inlining.interp,
                                  IdDict{MethodInstance,CodeInstance}())
    check = compact!(ssa_inlining_pass!(check, flatten_state, false))
    assume = compact!(ssa_inlining_pass!(assume, flatten_state, false))
    if is_asserts()
        verify_ir(check, true, false, optimizer_lattice(sv.inlining.interp), sv.linfo)
        verify_ir(assume, true, false, optimizer_lattice(sv.inlining.interp), sv.linfo)
    end
    return NothrowShadow(check, assume)
end

function derive_shadow_variant(ir::IRCode, sv::OptimizationState, world::UInt, ischeck::Bool)
    nblocks = length(ir.cfg.blocks)
    builder = ShadowBuilder(length(ir.stmts), nblocks)
    for bbidx = 1:nblocks
        bb = ir.cfg.blocks[bbidx]
        builder.bbentry[bbidx] = bbidx == 1 ? 1 : start_block!(builder)
        for i in bb.stmts
            inst = ir.stmts[i]
            stmt = inst[:stmt]
            typ = inst[:type]
            flag = inst[:flag]
            islast = (i == last(bb.stmts))
            if stmt === nothing || isa(stmt, QuoteNode)
                builder.ssamap[i] = emit!(builder, stmt, typ, NoCallInfo(), flag)
            elseif isa(stmt, GlobalRef)
                # only nothrow global reads are freely duplicable
                has_flag(flag, IR_FLAG_NOTHROW) || return nothing
                builder.ssamap[i] = emit!(builder, stmt, typ, NoCallInfo(), flag)
            elseif isa(stmt, PiNode)
                v = remap_value(builder, stmt.val)
                v === nothing && return nothing
                builder.ssamap[i] = emit!(builder, PiNode(v, stmt.typ), typ, NoCallInfo(), flag)
            elseif isa(stmt, PhiNode)
                # values remapped now; edges patched in finish_shadow_ir!
                # (kept as SOURCE block indices until then)
                values = Vector{Any}(undef, length(stmt.values))
                for k = 1:length(stmt.values)
                    if isassigned(stmt.values, k)
                        v = remap_value(builder, stmt.values[k])
                        # conservative: a back edge value not yet emitted is
                        # only representable if it is emitted under the same
                        # statement index mapping; give up otherwise
                        if v === nothing
                            src = stmt.values[k]
                            isa(src, SSAValue) || return nothing
                            # forward reference (loop back edge): record the
                            # SOURCE id negated as a sentinel, resolved in the
                            # final pass once the mapping is complete
                            v = SSAValue(-src.id)
                        end
                        values[k] = v
                    end
                end
                builder.ssamap[i] = emit!(builder, PhiNode(copy(stmt.edges), values), typ, NoCallInfo(), flag)
            elseif isa(stmt, ReturnNode)
                if isdefined(stmt, :val)
                    if ischeck
                        emit!(builder, ReturnNode(true), Any, NoCallInfo(), IR_FLAG_NULL)
                    else
                        v = remap_value(builder, stmt.val)
                        v === nothing && return nothing
                        emit!(builder, ReturnNode(v), Any, NoCallInfo(), IR_FLAG_NULL)
                    end
                else
                    # unreachable: in the check this means a must-throw path
                    # was taken, i.e. the answer is `false`; in the assume
                    # variant the path is impossible (compaction removes it)
                    if ischeck
                        emit!(builder, ReturnNode(false), Any, NoCallInfo(), IR_FLAG_NULL)
                    else
                        emit!(builder, ReturnNode(), Union{}, NoCallInfo(), IR_FLAG_NULL)
                    end
                end
            elseif isa(stmt, GotoNode)
                emit!(builder, GotoNode(stmt.label), Any, NoCallInfo(), IR_FLAG_NULL)
            elseif isa(stmt, GotoIfNot)
                cond = remap_value(builder, stmt.cond)
                cond === nothing && return nothing
                # branches themselves must be nothrow (Bool condition)
                has_flag(flag, IR_FLAG_NOTHROW) || widenconst(argextype(stmt.cond, ir)) === Bool || return nothing
                emit!(builder, GotoIfNot(cond, stmt.dest), Any, NoCallInfo(), IR_FLAG_NULL)
            elseif isa(stmt, EnterNode) || isa(stmt, PhiCNode) || isa(stmt, UpsilonNode)
                return nothing # exception handlers not supported
            elseif isexpr(stmt, :loopinfo) || isexpr(stmt, :gc_preserve_begin) || isexpr(stmt, :gc_preserve_end)
                builder.ssamap[i] = emit!(builder, remap_stmt(builder, stmt), typ, NoCallInfo(), flag)
            elseif isexpr(stmt, :boundscheck)
                # NB: copy - `adjust_boundscheck!` mutates these in place
                builder.ssamap[i] = emit!(builder, copy(stmt), typ, NoCallInfo(), flag)
            elseif isexpr(stmt, :call) || isexpr(stmt, :invoke) || isexpr(stmt, :new) || isexpr(stmt, :splatnew)
                if has_flag(flag, IR_FLAG_NOTHROW)
                    if has_flag(flag, IR_FLAG_EFFECT_FREE)
                        nstmt = remap_stmt(builder, stmt)
                        nstmt === nothing && return nothing
                        builder.ssamap[i] = emit!(builder, nstmt, typ, sanitize_shadow_info(inst[:info]), flag)
                    else
                        # v1: no deletable-store support in the early
                        # derivation; the post-optimization synthesis still
                        # covers those kernels
                        return nothing
                    end
                elseif typ === Union{} && has_flag(flag, IR_FLAG_EFFECT_FREE | IR_FLAG_TERMINATES)
                    # deterministically throws with no other effect: in the
                    # check this is `return false`, in the assume variant the
                    # path is unreachable. The block's unreachable terminator
                    # provides the actual `return`; drop the throw itself.
                    builder.ssamap[i] = emit!(builder, nothing, Nothing, NoCallInfo(), IR_FLAGS_REMOVABLE)
                else
                    # a may-throw call: compose the callee's cached shadow
                    isexpr(stmt, :call) || return nothing
                    nstmt = remap_stmt(builder, stmt)
                    nstmt === nothing && return nothing
                    cond = emit_builtin_check!(builder, ir, stmt)
                    if cond !== nothing
                        if ischeck
                            # guard, then the (now known-inbounds) operation
                            # itself if its value is needed; for builtins the
                            # value computation is the same expression with
                            # the boundscheck argument replaced by `false`
                            push!(builder.falsebranches, emit!(builder,
                                GotoIfNot(cond, 0), Any, NoCallInfo(), IR_FLAG_NULL))
                            start_block!(builder)
                        end
                        guarded = copy(nstmt::Expr)
                        guarded.args[end] = false # boundscheck argument
                        builder.ssamap[i] = emit!(builder, guarded, typ, inst[:info],
                                                  SHADOW_GUARDED_FLAGS & ~IR_FLAG_INLINE)
                    else
                        mi = resolve_unique_call(ir, i)
                        mi === nothing && return nothing
                        check_ci = lookup_shadow(NothrowCheckOwner(), mi, world)
                        assume_ci = lookup_shadow(NothrowAssumeOwner(), mi, world)
                        (check_ci === nothing || assume_ci === nothing) && return nothing
                        args = Any[nstmt.args...]
                        # `Core.invoke_split_effects(which, f, args...)` is
                        # semantically `f(args...)` (and its call info - which
                        # `resolve_unique_call` consulted - describes that
                        # inner call): compose the TARGET's shadow with the
                        # inner argument list, not the raw statement's
                        if length(args) >= 3 &&
                           singleton_type(argextype(stmt.args[1], ir)) === Core.invoke_split_effects
                            args = args[3:end]
                        end
                        if ischeck
                            ok = emit!(builder, Expr(:invoke, check_ci, args...), Bool,
                                       NoCallInfo(), IR_FLAGS_REMOVABLE | IR_FLAG_INLINE)
                            push!(builder.falsebranches, emit!(builder,
                                GotoIfNot(SSAValue(ok), 0), Any, NoCallInfo(), IR_FLAG_NULL))
                            start_block!(builder)
                        end
                        builder.ssamap[i] = emit!(builder, Expr(:invoke, assume_ci, args...), typ,
                                                  NoCallInfo(), SHADOW_GUARDED_FLAGS)
                    end
                end
            else
                return nothing # unclassified statement kind
            end
        end
        builder.bbexit[bbidx] = current_block(builder)
        # a fall-through source block whose emitted exit is followed by
        # emitted guard blocks stays consistent because guards only split
        # WITHIN the block, before its (single) terminator
    end
    return finish_shadow_ir(builder, ir, ischeck)
end

function finish_shadow_ir(builder::ShadowBuilder, srcir::IRCode, ischeck::Bool)
    # shared `return false` block for all failed guards (check variant only)
    falsebb = 0
    if !isempty(builder.falsebranches)
        start_block!(builder)
        emit!(builder, ReturnNode(false), Any, NoCallInfo(), IR_FLAG_NULL)
        falsebb = current_block(builder)
    end
    nstmts = length(builder.stmts)
    nblocks = length(builder.bbstarts)
    # patch branch targets (source block index -> emitted entry block),
    # false-branch targets, phi edges (source pred -> emitted exit block) and
    # phi back-edge value forward references
    for i = 1:nstmts
        stmt = builder.stmts[i]
        if isa(stmt, GotoNode)
            builder.stmts[i] = GotoNode(builder.bbentry[stmt.label])
        elseif isa(stmt, GotoIfNot)
            if stmt.dest == 0
                builder.stmts[i] = GotoIfNot(stmt.cond, falsebb)
            else
                builder.stmts[i] = GotoIfNot(stmt.cond, builder.bbentry[stmt.dest])
            end
        elseif isa(stmt, PhiNode)
            edges = stmt.edges
            for k = 1:length(edges)
                edges[k] = Int32(builder.bbexit[edges[k]])
            end
            for k = 1:length(stmt.values)
                if isassigned(stmt.values, k)
                    v = stmt.values[k]
                    if isa(v, SSAValue) && v.id < 0
                        newid = builder.ssamap[-v.id]
                        newid == 0 && return nothing # never emitted; malformed
                        stmt.values[k] = SSAValue(newid)
                    end
                end
            end
        end
    end
    # assemble the instruction stream
    stmts = InstructionStream(nstmts)
    for i = 1:nstmts
        stmts.stmt[i] = builder.stmts[i]
        stmts.type[i] = builder.types[i]
        stmts.info[i] = builder.infos[i]
        stmts.flag[i] = builder.flags[i]
    end
    # block ranges and edges
    blocks = BasicBlock[]
    for b = 1:nblocks
        lo = builder.bbstarts[b]
        hi = b == nblocks ? nstmts : builder.bbstarts[b+1] - 1
        lo <= hi || return nothing # empty block (malformed)
        push!(blocks, BasicBlock(StmtRange(lo, hi), Int[], Int[]))
    end
    push!(blocks[1].preds, 0) # the entry block has a virtual predecessor
    for b = 1:nblocks
        term = stmts.stmt[last(blocks[b].stmts)]
        if isa(term, GotoNode)
            push!(blocks[b].succs, term.label)
            push!(blocks[term.label].preds, b)
        elseif isa(term, GotoIfNot)
            b == nblocks && return nothing
            push!(blocks[b].succs, b + 1)
            push!(blocks[b+1].preds, b)
            if term.dest != b + 1
                push!(blocks[b].succs, term.dest)
                push!(blocks[term.dest].preds, b)
            end
        elseif isa(term, ReturnNode)
        else # fall through
            b == nblocks && return nothing
            push!(blocks[b].succs, b + 1)
            push!(blocks[b+1].preds, b)
        end
    end
    cfg = CFG(blocks, Int[blocks[b].stmts.start for b = 2:nblocks])
    debuginfo = DebugInfoStream(stmts.line)
    argtypes = copy(srcir.argtypes)
    ir = IRCode(stmts, cfg, debuginfo, argtypes, Expr[], copy(srcir.sptypes), srcir.valid_worlds)
    return compact!(ir, true)
end

##############################
# pipeline + cache insertion #
##############################

# Derivation entry, called from `run_passes_ipo_safe` right after the first
# compaction (calls still unexpanded, inference call info available). The
# result is stashed on the `OptimizationState` and cached by `finish!` with
# the validity worlds and edges of the real code instance.
# Debugging knobs: JULIA_NOTHROW_SHADOW=0 disables derivation entirely;
# JULIA_NOTHROW_SHADOW_MAX=n derives only for the first n eligible methods
const SHADOW_COUNTER = RefValue(0)
function derive_nothrow_shadows!(ir::IRCode, sv::OptimizationState)
    sv.nothrow_shadow = nothing
    enabled = get_bool_env_shadow()
    enabled || return ir
    usability = shadow_worth_deriving(ir)
    usability || return ir
    maxn = get_int_env_shadow_max()
    if maxn >= 0
        n = (SHADOW_COUNTER[] += 1)
        n <= maxn || return ir
        if n == maxn
            # bisection support: the last permitted derivation is the suspect.
            # NB: bootstrap-safe printing only (no `string(::MethodInstance)`)
            def = sv.linfo.def
            if isa(def, Method)
                ccall(:jl_safe_printf, Cvoid, (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Csize_t),
                      "SHADOW[last]: %s.%s nargs=%zd\n",
                      ccall(:jl_symbol_name, Ptr{UInt8}, (Any,), nameof(def.module)),
                      ccall(:jl_symbol_name, Ptr{UInt8}, (Any,), def.name),
                      Csize_t(def.nargs))
            end
        end
    end
    shadow = try
        derive_nothrow_shadow(ir, sv)
    catch
        nothing # conservative: derivation must never break optimization
    end
    if shadow !== nothing && is_asserts()
        try
            verify_ir(shadow.check, true, false, optimizer_lattice(sv.inlining.interp), sv.linfo)
            verify_ir(shadow.assume, true, false, optimizer_lattice(sv.inlining.interp), sv.linfo)
        catch
            shadow = nothing
        end
    end
    sv.nothrow_shadow = shadow
    return ir
end

# Cheap pre-filter: only attempt derivation when the body contains at least
# one may-throw statement (otherwise the method is nothrow and needs no split)
function shadow_worth_deriving(ir::IRCode)
    length(ir.stmts) <= SHADOW_STMT_BUDGET || return false
    for i = 1:length(ir.stmts)
        inst = ir.stmts[i]
        stmt = inst[:stmt]
        iscallstmt(stmt) || continue
        has_flag(inst[:flag], IR_FLAG_NOTHROW) && continue
        return true
    end
    return false
end

# Convert a derived shadow `IRCode` into a compilable standalone `CodeInfo`
# (mirroring the `Core.OpaqueClosure(::IRCode)` recipe)
function shadow_codeinfo(ir::IRCode, mi::MethodInstance, @nospecialize(rt),
                         min_world::UInt, max_world::UInt, edges::SimpleVector)
    ir = copy(ir)
    ir.debuginfo.def === nothing && (ir.debuginfo.def = mi)
    nargtypes = length(ir.argtypes)
    src = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    src.slotnames = fill(:none, nargtypes)
    src.slotflags = fill(zero(UInt8), nargtypes)
    src.slottypes = copy(ir.argtypes)
    src.min_world = min_world
    src.max_world = max_world
    def = mi.def
    src.isva = isa(def, Method) ? def.isva : false
    src.nargs = UInt(nargtypes)
    src = ir_to_codeinf!(src, ir)
    src.rettype = widenconst(rt)
    src.edges = edges
    return src
end

# Cache the derived shadows as `CodeInstance`s on `mi`, sharing the validity
# worlds and edges of the freshly finished real code instance `ci`
function cache_nothrow_shadows!(shadow::NothrowShadow, ci::CodeInstance, mi::MethodInstance)
    min_world = @atomic :monotonic ci.min_world
    max_world = @atomic :monotonic ci.max_world
    edges = @atomic :monotonic ci.edges
    for (owner, ir, rt) in ((NothrowCheckOwner(), shadow.check, Bool),
                            (NothrowAssumeOwner(), shadow.assume, ci.rettype))
        src = shadow_codeinfo(ir, mi, rt, min_world, max_world, edges)
        sci = CodeInstance(mi, owner, widenconst(rt), Any, nothing, src, zero(Int32),
            min_world, max_world, zero(UInt32), nothing, nothing, edges)
        if max_world == typemax(UInt)
            store_backedges(sci, edges)
        end
        ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, sci)
    end
    return nothing
end
