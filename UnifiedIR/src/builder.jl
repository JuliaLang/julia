# The unsealed builder sub-state (§4.1): append is the primitive, regions are
# explicitly opened/closed, `finish!` seals and verifies.

mutable struct Builder{Cols}
    ir::IR{Cols}
    open::Vector{RegionId}          # open-region stack; open[1] = root
    finished::Bool
end

"""
    Builder(; cols=NOCOLS, argtypes=Any[], sptypes=Any[], meta...)

Start building a function body. Region 1 (the root body) is open; append the
function parameters as leading `region_arg` statements.
"""
function Builder(; cols = NOCOLS, argtypes::Vector{Any} = Any[],
                 sptypes::Vector{Any} = Any[], name::Symbol = :anon)
    ir = IR{typeof(cols)}(BodyOwner(LAYOUT_BUILDER, 0), IRBody(cols), Region[],
                          argtypes, sptypes, (UInt64(0), typemax(UInt64)),
                          nothing, Pair{StmtId,Operand}[], AnalysisCache(),
                          Dict{Symbol,Any}(:name => name))
    root = Region(REGION_BODY, NULL_STMT, NULL_REGION)
    root.first = StmtId(1)
    push!(ir.regions, root)
    return Builder{typeof(cols)}(ir, [RegionId(1)], false)
end

current_region(b::Builder) = b.open[end]

function _append_row!(ir::IR, k::Kind, opsword::UInt64, @nospecialize(type),
                      flag::UInt32, debug::NTuple{3,Int32}, region::RegionId)
    body = ir.body
    # the shared substrate row-append primitive (len + kind + ops word +
    # §3.5 column growth) — the same one SyntaxGraph's new_id! uses
    id = newrow!(body.graph, k, opsword)
    # IR-only columns
    push!(body.type, type)
    push!(body.flag, flag)
    push!(body.debug, debug)
    push!(body.region, region)
    ir.cache.stmt_epoch += 1
    return StmtId(id)
end

"""
    append_stmt!(b, kind, ops...; type=Any, flag=default, debug=(0,0,0)) -> StmtId

Append a statement to the current open region. Value arguments are converted
with `vop` (StmtId → STMT, small ints/bools → INLINE, other → CONST intern);
pass `Operand`s directly for full control.
"""
function append_stmt!(b::Builder, k::Kind, args...;
                      type = Any, flag::Union{Nothing,UInt32} = nothing,
                      debug::NTuple{3,Int32} = (Int32(0), Int32(0), Int32(0)))
    b.finished && error("builder already finished")
    info = kindinfo(k)
    ops = Operand[a isa Operand ? a : vop(b.ir, a) for a in args]
    check_arity(info, length(ops))
    if info.name === :region_arg
        reg = getregion(b.ir, current_region(b))
        # region_args must lead their region's span (L0)
        b.ir.body.len == 0 || Int(b.ir.body.len) < reg.first.id ||
            all(i -> b.ir.body.kind[i] === K"region_arg",
                reg.first.id:Int(b.ir.body.len)) ||
            error("region_arg must occupy the leading positions of its region")
    end
    f = flag === nothing ? info.effects : flag
    info.result == 0 && (type = Nothing)   # zero-result stmts have no meaningful type
    w = encode_ops!(b.ir.body, k, ops)
    s = _append_row!(b.ir, k, w, type, f, debug, current_region(b))
    if info.name === :region_arg
        push!(getregion(b.ir, current_region(b)).args, s)
    end
    return s
end

function check_arity(info::KindInfo, n::Int)
    n >= info.minops || error("kind $(info.qualified): expected at least $(info.minops) operands, got $n")
    info.maxops >= 0 && n > info.maxops &&
        error("kind $(info.qualified): expected at most $(info.maxops) operands, got $n")
    return nothing
end

"""
    open_region!(b, owner; kind=REGION_ARM, activation=ACT_IMMEDIATE) -> RegionId

Open a region owned by `owner`, which must be the last appended statement or
the owner of the immediately preceding closed region (owned regions are
contiguous immediately after their owner).
"""
function open_region!(b::Builder, owner::StmtId;
                      kind::RegionKind = REGION_ARM,
                      activation::Activation = ACT_IMMEDIATE)
    b.finished && error("builder already finished")
    owns_regions(stmt_kind(b.ir, owner)) || stmt_kind(b.ir, owner) === K"closure" ||
        error("kind $(kindname(stmt_kind(b.ir, owner))) does not own regions")
    stmt_region(b.ir, owner) == current_region(b) ||
        error("open_region!: owner must be in the current open region")
    r = Region(kind, owner, current_region(b); activation)
    r.first = StmtId(Int(b.ir.body.len) + 1)
    push!(b.ir.regions, r)
    rid = RegionId(length(b.ir.regions))
    push!(b.open, rid)
    b.ir.cache.region_epoch += 1
    return rid
end

"Open an ownerless guard region (floating dialect, §3.3)."
function open_guard_region!(b::Builder, cond::Value; negated::Bool = false,
                            parent::RegionId = current_region(b))
    r = Region(REGION_GUARD, NULL_STMT, parent; cond, negated)
    r.first = StmtId(Int(b.ir.body.len) + 1)
    push!(b.ir.regions, r)
    rid = RegionId(length(b.ir.regions))
    push!(b.open, rid)
    b.ir.cache.region_epoch += 1
    return rid
end

function close_region!(b::Builder)
    length(b.open) > 1 || error("cannot close the root region")
    rid = pop!(b.open)
    reg = getregion(b.ir, rid)
    reg.last = StmtId(Int(b.ir.body.len))
    return rid
end

"""
    finish!(b) -> IR

Close out the build: seals to dense layout and runs L0 (+ the minimal L1
subset).
"""
function finish!(b::Builder; verify::Bool = true)
    b.finished && error("builder already finished")
    length(b.open) == 1 || error("finish!: $(length(b.open) - 1) region(s) still open")
    root = getregion(b.ir, RegionId(1))
    root.last = StmtId(Int(b.ir.body.len))
    b.finished = true
    b.ir.owner.state = LAYOUT_DENSE
    b.ir.cache.layout_epoch += 1
    verify && verify_ir(b.ir; level = 0)
    return b.ir
end

# Convenience block-style region builders --------------------------------------

"""
    build_if!(f_then, b, cond; type=Any, f_else=nothing) -> StmtId

Append an `if` op and build its arms with `f_then(b)`/`f_else(b)` (each must
terminate its arm).
"""
function build_if!(f_then, b::Builder, cond; type = Any, f_else = nothing)
    s = append_stmt!(b, K"if", cond; type)
    open_region!(b, s; kind = REGION_ARM)
    f_then(b)
    close_region!(b)
    if f_else !== nothing
        open_region!(b, s; kind = REGION_ARM)
        f_else(b)
        close_region!(b)
    end
    return s
end

"""
    build_loop!(f_body, b, inits...; type=Any, argtypes) -> StmtId

Append a `loop` op with carried-value inits; `f_body(b, args::Vector{StmtId})`
receives the carried region args and must terminate the body.
"""
function build_loop!(f_body, b::Builder, inits...; type = Any,
                     argtypes = Any[Any for _ in inits])
    s = append_stmt!(b, K"loop", inits...; type)
    r = open_region!(b, s; kind = REGION_LOOP_BODY)
    args = StmtId[append_stmt!(b, K"region_arg"; type = argtypes[i]) for i in 1:length(inits)]
    f_body(b, args)
    close_region!(b)
    return s
end
