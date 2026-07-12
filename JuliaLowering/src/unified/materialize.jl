# Closure materialization (§5.7): residual `closure` ops — after the shared
# promotion fixpoint has decided captures structurally — become runtime
# closure types + extracted method IRs, mirroring convert_closures' output
# shape so downstream (interpretation, differentials) is unchanged.
#
# Per residual closure op, in home-activation order:
#   * the capture set IS `closure_environment` (the derived free values:
#     ordered SSA values + shared cells);
#   * the closure type is created with the EXISTING runtime machinery
#     (`eval_closure_type`): value captures are type-parameterized fields,
#     surviving shared cells are `Core.Box` fields;
#   * the deferred region is EXTRACTED into a standalone method IR whose
#     region 1 is `#self#` + the params; capture references become
#     `getfield(#self#, name)` (values) or field-box get/set sequences
#     (shared cells), and nested closure ops ride along to be materialized
#     recursively;
#   * the op itself is replaced by `new(apply_type(T, typeof(v)...), caps…)`;
#   * a trampoline method `(self::T)(args...) = interpret(method_ir, self,
#     args...)` is defined on the new type, so instances are CALLABLE and
#     the interpreter differential runs end-to-end through UnifiedIR on both
#     sides of the call boundary — no aliasing to natively-created types.
#
# Surviving shared cells in the enclosing frame are lowered to the same
# runtime containers (`Core.Box()` + getfield/setfield! with the
# #20016-style guards the emitter already placed as
# cell_isdefined/throw_undef_if_not), so `new` receives real boxes.

const _UIR = UnifiedIR   # local alias (this file is included into UnifiedBackend)

_cell_bind_name(jlctx, cellcol, c) = begin
    bid = cellcol[c]
    bid isa Int ? String(jlctx.bindings.info[bid].name) : "box"
end

"""
    materialize_closures!(jlctx, ir, encname) -> Vector{LoweredMethod}

Materialize every residual closure region in `ir` (recursively through
extracted bodies) and lower surviving `cell_shared` cells to runtime
containers. Mutates `ir` (ends compacted + L1-verified); returns the
extracted closure methods.
"""
function materialize_closures!(jlctx, ir::_UIR.IR, encname::Symbol)
    extras = LoweredMethod[]
    anyclosure = any(s -> _UIR.stmt_kind(ir, s) === _UIR.K"closure",
                     collect(_UIR.each_stmt(ir)))
    anyshared = any(s -> _UIR.stmt_kind(ir, s) === _UIR.K"cell_shared",
                    collect(_UIR.each_stmt(ir)))
    (anyclosure || anyshared) || return extras
    _UIR.layout(ir) === _UIR.LAYOUT_DENSE && _UIR.editable(ir)
    while true
        target = _UIR.NULL_STMT
        for s in _UIR.each_stmt(ir)
            _UIR.is_tombstone(ir, s) && continue
            _UIR.stmt_kind(ir, s) === _UIR.K"closure" || continue
            _UIR.activation_root(ir, _UIR.stmt_region(ir, s)) ==
                _UIR.root_region(ir) || continue
            target = s
            break
        end
        _UIR.isnull(target) && break
        _materialize_one!(jlctx, ir, target, extras, encname)
    end
    _lower_shared_cells!(jlctx, ir)
    _UIR.compact!(ir)
    _UIR.verify_ir(ir; level = 1)
    return extras
end

function _materialize_one!(jlctx, ir::_UIR.IR, s::StmtId,
                           extras::Vector{LoweredMethod}, encname::Symbol)
    _UIR.nops(ir, s) == 0 ||
        throw(UnsupportedForm("closure", "materializing a flagged (isva) closure op"))
    env = _UIR.closure_environment(ir, s)
    clcol = _UIR.getattr(ir, :clbind)
    cellcol = _UIR.getattr(ir, :cellbind)
    bid = clcol[s]
    cb = bid isa Int ? get(jlctx.closure_bindings, bid, nothing) : nothing
    name_stack = cb === nothing ? String[String(encname), "#anon#"] : cb.name_stack
    mname = Symbol(last(name_stack))

    # ---- field spec (values first, then cells; names deduplicated) --------
    fsyms = Symbol[]
    used = Set{String}()
    function uniq(nm::String)
        base = nm
        i = 1
        while nm in used
            nm = "$base#$i"
            i += 1
        end
        push!(used, nm)
        return Symbol(nm)
    end
    for i in eachindex(env.values)
        push!(fsyms, uniq("v#$i"))
    end
    for c in env.cells
        push!(fsyms, uniq(_cell_bind_name(jlctx, cellcol, c)))
    end
    flags = Bool[]
    for _ in env.values
        push!(flags, false)                  # type-parameterized value field
    end
    for _ in env.cells
        push!(flags, true)                   # Core.Box shared field
    end

    # ---- the closure type + extracted method IR ----------------------------
    mod = jlctx.mod::Module
    tyname = Symbol(reserve_module_binding_i(mod, string("#", join(name_stack, "#"), "##")))
    T = eval_closure_type(mod, tyname, Core.svec(fsyms...), Core.svec(flags...))

    rs = _UIR.live_owned_regions(ir, s)
    breg = _UIR.getregion(ir, rs[1])
    mb = _UIR.Builder(name = mname,
                      cols = (cellbind = BindCol(), clbind = BindCol()))
    stmtmap = Dict{Int32,Operand}()
    regmap = Dict{Int32,RegionId}()
    selfarg = append_stmt!(mb, _UIR.K"region_arg"; type = Any)
    slotnames = Symbol[Symbol("#self#")]
    for a in breg.args
        na = append_stmt!(mb, _UIR.K"region_arg"; type = _UIR.stmt_type(ir, a))
        stmtmap[a.id] = op_stmt(na)
        push!(slotnames, :_)
    end
    boxmap = Dict{Int32,Tuple{Operand,Symbol}}()
    for (i, v) in enumerate(env.values)
        g = append_stmt!(mb, _UIR.K"call", GlobalRef(Core, :getfield),
                         op_stmt(selfarg), fsyms[i]; type = Any)
        stmtmap[v.id] = op_stmt(g)
    end
    for (j, c) in enumerate(env.cells)
        fi = length(env.values) + j
        g = append_stmt!(mb, _UIR.K"call", GlobalRef(Core, :getfield),
                         op_stmt(selfarg), fsyms[fi]; type = Any)
        boxmap[c.id] = (op_stmt(g), :contents)
    end
    _copy_region!(mb, ir, rs[1], stmtmap, regmap, boxmap)
    mir = _UIR.finish!(mb)

    # nested closure ops rode along: materialize them inside the new method
    nested = materialize_closures!(jlctx, mir, mname)
    _UIR.verify_ir(mir; level = 1)
    push!(extras, LoweredMethod(mname, 1 + length(breg.args), slotnames, mir))
    append!(extras, nested)

    # trampoline: instances of the new type interpret the extracted body, so
    # the enclosing differential crosses the call boundary inside UnifiedIR
    # (T is a UnionAll when value captures parameterize the layout)
    tramp = :((self::$T)(args...) = $(_UIR.interpret)($mir, self, args...))
    Base.eval(mod, tramp)

    # ---- enclosing rewrite: apply_type + new, then kill the region ---------
    tref = vop(ir, GlobalRef(mod, tyname))
    ctop = tref
    if !isempty(env.values)
        tvs = Operand[]
        for v in env.values
            tv = _UIR.insert_before!(ir, s, _UIR.K"call",
                                     GlobalRef(Core, :_typeof_captured_variable),
                                     op_stmt(v); type = Any)
            push!(tvs, op_stmt(tv))
        end
        ct = _UIR.insert_before!(ir, s, _UIR.K"call", GlobalRef(Core, :apply_type),
                                 tref, tvs...; type = Any)
        ctop = op_stmt(ct)
    end
    caps = Operand[op_stmt(v) for v in env.values]
    append!(caps, Operand[op_stmt(c) for c in env.cells])
    inst = _UIR.insert_before!(ir, s, _UIR.K"new", ctop, caps...; type = Any)
    _UIR.replace_uses!(ir, s => op_stmt(inst))
    _UIR.flush_renames!(ir)
    _UIR.kill_stmt!(ir, s)
    return nothing
end

# Copy one region's direct members (region_args excluded — the caller placed
# them) into the currently open region of `mb`, recursing through owned
# regions; cell ops on environment cells become container field operations.
function _copy_region!(mb::_UIR.Builder, ir::_UIR.IR, r::RegionId,
                       stmtmap::Dict{Int32,Operand}, regmap::Dict{Int32,RegionId},
                       boxmap::Dict{Int32,Tuple{Operand,Symbol}})
    function maprop(o::Operand)::Operand
        t = _UIR.optag(o)
        if t == _UIR.TAG_STMT
            m = get(stmtmap, _UIR.asstmt(o).id, nothing)
            m === nothing &&
                throw(UnsupportedForm("closure",
                    "extracted body references an unmapped statement %$(_UIR.asstmt(o).id)"))
            return m
        elseif t == _UIR.TAG_REGION || t == _UIR.TAG_BLOCK
            nr = get(regmap, Int32(_UIR.payload(o)), nothing)
            nr === nothing &&
                throw(UnsupportedForm("closure", "extracted body exits its region"))
            return t == _UIR.TAG_REGION ? _UIR.op_region(nr) : _UIR.op_block(nr)
        elseif t == _UIR.TAG_CONST
            return vop(mb.ir, _UIR.getconst(ir, o))
        elseif t == _UIR.TAG_GLOBAL
            return vop(mb.ir, _UIR.getglobal_op(ir, o))
        elseif t == _UIR.TAG_SPARAM
            throw(UnsupportedForm("closure", "extracted body references a static parameter"))
        else
            return o
        end
    end
    cellcol_src = _UIR.getattr(ir, :cellbind)
    clcol_src = _UIR.getattr(ir, :clbind)
    cellcol_dst = _UIR.getattr(mb.ir, :cellbind)
    clcol_dst = _UIR.getattr(mb.ir, :clbind)
    for s in _UIR.region_stmts(ir, r)
        k = _UIR.stmt_kind(ir, s)
        k === _UIR.K"region_arg" && continue
        # cell ops on environment cells -> container field operations
        if (k === _UIR.K"cell_get" || k === _UIR.K"cell_set" ||
            k === _UIR.K"cell_new" || k === _UIR.K"cell_isdefined") &&
           _UIR.nops(ir, s) >= 1 &&
           _UIR.optag(_UIR.getop(ir, s, 1)) == _UIR.TAG_STMT &&
           haskey(boxmap, _UIR.asstmt(_UIR.getop(ir, s, 1)).id)
            box, fsym = boxmap[_UIR.asstmt(_UIR.getop(ir, s, 1)).id]
            if k === _UIR.K"cell_get"
                n = append_stmt!(mb, _UIR.K"call", GlobalRef(Core, :getfield),
                                 box, fsym; type = _UIR.stmt_type(ir, s),
                                 debug = _UIR.stmt_debug(ir, s))
                stmtmap[s.id] = op_stmt(n)
            elseif k === _UIR.K"cell_set"
                append_stmt!(mb, _UIR.K"call", GlobalRef(Core, :setfield!),
                             box, fsym, maprop(_UIR.getop(ir, s, 2));
                             type = Any, debug = _UIR.stmt_debug(ir, s))
            elseif k === _UIR.K"cell_isdefined"
                n = append_stmt!(mb, _UIR.K"call", GlobalRef(Core, :isdefined),
                                 box, fsym; type = Bool,
                                 debug = _UIR.stmt_debug(ir, s))
                stmtmap[s.id] = op_stmt(n)
            else
                throw(UnsupportedForm("closure",
                    "re-declaration of a captured variable inside a closure body"))
            end
            continue
        end
        ops = Operand[maprop(_UIR.getop(ir, s, j)) for j in 1:_UIR.nops(ir, s)]
        n = append_stmt!(mb, k, ops...; type = _UIR.stmt_type(ir, s),
                         flag = _UIR.stmt_flag(ir, s), debug = _UIR.stmt_debug(ir, s))
        stmtmap[s.id] = op_stmt(n)
        b = cellcol_src[s]
        b isa Int && (cellcol_dst[n] = b)
        b2 = clcol_src[s]
        b2 isa Int && (clcol_dst[n] = b2)
        if _UIR.owns_regions(k)
            for rid in _UIR.live_owned_regions(ir, s)
                reg = _UIR.getregion(ir, rid)
                nr = _UIR.open_region!(mb, n; kind = reg.kind,
                                       activation = reg.activation)
                regmap[rid.id] = nr
                for a in reg.args
                    na = append_stmt!(mb, _UIR.K"region_arg";
                                      type = _UIR.stmt_type(ir, a))
                    stmtmap[a.id] = op_stmt(na)
                end
                _copy_region!(mb, ir, rid, stmtmap, regmap, boxmap)
                _UIR.close_region!(mb)
            end
        end
    end
    return nothing
end

# Surviving shared cells (true shared captures, or shared cells whose
# closures were all removed) become runtime containers in the frame itself:
# the cell statement becomes the container constructor call and every cell
# op becomes the corresponding field operation. The emitter's checked-read
# guards (cell_isdefined + throw_undef_if_not) become the #20016 pattern
# with the right variable name.
function _lower_shared_cells!(jlctx, ir::_UIR.IR)
    for c in collect(_UIR.each_stmt(ir))
        _UIR.is_tombstone(ir, c) && continue
        _UIR.stmt_kind(ir, c) === _UIR.K"cell_shared" || continue
        fsym = :contents
        uses = Tuple{StmtId,Int}[]
        _UIR.each_ssa_use(ir) do site, used
            used == c || return
            site isa _UIR.StmtOperand || return
            push!(uses, (site.user, Int(site.opidx)))
            return
        end
        for (u, opidx) in uses
            _UIR.is_tombstone(ir, u) && continue
            k = _UIR.stmt_kind(ir, u)
            if k === _UIR.K"cell_get" && opidx == 1
                _UIR.replace_stmt!(ir, u, _UIR.K"call",
                                   vop(ir, GlobalRef(Core, :getfield)),
                                   op_stmt(c), vop(ir, fsym);
                                   type = _UIR.stmt_type(ir, u))
            elseif k === _UIR.K"cell_isdefined" && opidx == 1
                _UIR.replace_stmt!(ir, u, _UIR.K"call",
                                   vop(ir, GlobalRef(Core, :isdefined)),
                                   op_stmt(c), vop(ir, fsym); type = Bool)
            elseif k === _UIR.K"cell_set" && opidx == 1
                _UIR.insert_before!(ir, u, _UIR.K"call",
                                    GlobalRef(Core, :setfield!), op_stmt(c),
                                    fsym, _UIR.getop(ir, u, 2); type = Any)
                _UIR.delete_stmt!(ir, u)
            elseif k === _UIR.K"cell_new" && opidx == 1
                throw(UnsupportedForm("closure",
                    "cell_new on a shared cell survived materialization"))
            end
            # other uses (the `new` capture argument) keep referencing the
            # statement, which becomes the container constructor below
        end
        _UIR.replace_stmt!(ir, c, _UIR.K"call", vop(ir, GlobalRef(Core, :Box));
                           type = Any)
    end
    return nothing
end
