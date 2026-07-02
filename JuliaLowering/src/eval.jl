# Non-incremental lowering API for non-toplevel non-module expressions.
# May be removed?

function lower(mod::Module, ex0::SyntaxTree; expr_compat_mode::Bool=false, world::UInt=Base.get_world_counter(),
               soft_scope::Union{Nothing,Bool}=nothing)
     ctx1, ex1 = expand_forms_1(  mod,  ex0, expr_compat_mode, world)
     ctx2, ex2 = expand_forms_2(  ctx1, ex1)
     ctx3, ex3 = resolve_scopes(  ctx2, ex2; soft_scope)
     ctx4, ex4 = convert_closures(ctx3, ex3)
    _ctx5, ex5 = linearize_ir(    ctx4, ex4)
    ex5
end

function macroexpand(mod::Module, ex::SyntaxTree; expr_compat_mode::Bool=false, world::UInt=Base.get_world_counter())
    _ctx1, ex1 = expand_forms_1(mod, ex, expr_compat_mode, world)
    ex1
end

# Incremental lowering API which can manage toplevel and module expressions.
#
# This iteration API is oddly bespoke and arguably somewhat non-Julian for two
# reasons:
#
# * Lowering knows when new modules are required, and may request them with
#   `:begin_module`. However `eval()` generates those modules so they need to
#   be passed back into lowering. So we can't just use `Base.iterate()`. (Put a
#   different way, we have a situation which is suited to coroutines but we
#   don't want to use full Julia `Task`s for this.)
# * We might want to implement this `eval()` in Julia's C runtime code or early
#   in bootstrap. Hence using SimpleVector and Symbol as the return values of
#   `lower_step()`
#
# We might consider changing at least the second of these choices, depending on
# how we end up putting this into Base.

struct LoweringIterator{Attrs}
    expr_compat_mode::Bool # later stored in module?
    todo::Vector{Tuple{SyntaxTree{Attrs}, Bool, Int}}
end

function lower_init(ex::SyntaxTree{T}; expr_compat_mode::Bool=false) where {T}
    LoweringIterator{T}(expr_compat_mode, [(ex, false, 0)])
end

function lower_step(iter::LoweringIterator, mod::Module, world::UInt;
                    soft_scope::Union{Nothing,Bool}=nothing)
    if isempty(iter.todo)
        return Core.svec(:done)
    end

    top_ex, is_module_body, child_idx = pop!(iter.todo)
    if child_idx > 0
        if child_idx <= numchildren(top_ex)
            push!(iter.todo, (top_ex, is_module_body, child_idx + 1))
            ex = top_ex[child_idx]
        elseif is_module_body
            return Core.svec(:end_module)
        else
            return lower_step(iter, mod, world; soft_scope)
        end
    else
        ex = top_ex
    end

    k = kind(ex)
    if !(k in KSet"toplevel module")
        ctx1, ex = expand_forms_1(mod, ex, iter.expr_compat_mode, world)
        k = kind(ex)
    end
    if k == K"toplevel"
        push!(iter.todo, (ex, false, 1))
        return lower_step(iter, mod, world; soft_scope)
    elseif k == K"module"
        (version, notbare, name, body) = @stm ex begin
            [K"module" version nb_st name body] ->
                (version.value, nb_st.value, name, body)
            [K"module" nb_st name body] ->
                (nothing, nb_st.value, name, body)
        end
        if kind(name) != K"Identifier"
            throw(LoweringError(name, "Expected module name"))
        end
        newmod_name = Symbol(name.name_val)
        loc = source_location(LineNumberNode, ex)
        push!(iter.todo, (body, true, 1))
        return Core.svec(:begin_module, version, newmod_name, notbare, loc)
    else
        # Non macro expansion parts of lowering
        @assert @isdefined(ctx1) "Assertion to tell the compiler about the definedness of this variable"
         ctx2, ex2 = expand_forms_2(ctx1, ex)
         ctx3, ex3 = resolve_scopes(ctx2, ex2; soft_scope)
         ctx4, ex4 = convert_closures(ctx3, ex3)
        _ctx5, ex5 = linearize_ir(ctx4, ex4)
        thunk = to_lowered_expr(ex5)
        return Core.svec(:thunk, thunk)
    end
end


#-------------------------------------------------------------------------------

function codeinfo_has_image_globalref(@nospecialize(e))
    if e isa GlobalRef
        return 0x00 !== @ccall jl_object_in_image(e.mod::Any)::UInt8
    elseif e isa Core.CodeInfo
        return any(codeinfo_has_image_globalref, e.code)
    else
        return false
    end
end

const _CodeInfo_need_ver = v"1.12.0-DEV.512"
@static if VERSION < _CodeInfo_need_ver
    function _CodeInfo(args...)
        error("Constructing a CodeInfo using JuliaLowering currently requires Julia version $_CodeInfo_need_ver or greater")
    end
else
    # debuginfo changed completely as of https://github.com/JuliaLang/julia/pull/52415
    # nargs / isva was added as of       https://github.com/JuliaLang/julia/pull/54341
    # field rettype added in             https://github.com/JuliaLang/julia/pull/54655
    # field has_image_globalref added in https://github.com/JuliaLang/julia/pull/57433
    # CodeInfo constructor. TODO: Should be in Core
    let
        fns = fieldnames(Core.CodeInfo)
        fts = fieldtypes(Core.CodeInfo)
        conversions = [:(convert($t, $n)) for (t,n) in zip(fts, fns)]

        expected_fns = (:code, :debuginfo, :ssavaluetypes, :ssaflags, :slotnames, :slotflags, :slottypes, :rettype, :parent, :edges, :min_world, :max_world, :method_for_inference_limit_heuristics, :nargs, :propagate_inbounds, :has_fcall, :has_image_globalref, :nospecializeinfer, :isva, :inlining, :constprop, :purity, :inlining_cost)
        expected_fts = (Vector{Any}, Core.DebugInfo, Any, Vector{UInt32}, Vector{Symbol}, Vector{UInt8}, Any, Any, Any, Any, UInt, UInt, Any, UInt, Bool, Bool, Bool, Bool, Bool, UInt8, UInt8, UInt16, UInt16)
        code = if fns != expected_fns || fts != expected_fts
            :(function _CodeInfo(args...)
                  error(string(
                      "JuliaLowering didn't recognize Core.CodeInfo's fields; ",
                      "it may need updating to match Core.CodeInfo.\n",
                      "expected field names: $($expected_fns)\n",
                      "expected field types: $($expected_fts)\n"))
              end)
        else
            :(function _CodeInfo($(fns...))
                $(Expr(:new, :(Core.CodeInfo), conversions...))
            end)
        end

        Core.eval(@__MODULE__, code)
    end
end

"""
Uncompressed form of DebugInfo's linetable::String.  When compressing, some
conveniences are erased:
- `file` is not present
- `line_offset` is identical
- `spans` pairs (s1, s2) are stored `(s1-byte_offset, s2-s1+1)`
- `line_starts` are stored `x-byte_offset`
"""
struct SourceByteTable
    file::Symbol
    line_offset::Int32
    spans::Vector{Tuple{Int32,Int32}}
    line_starts::Vector{Int32}
    function SourceByteTable(file, line_offset, spans, line_starts)
        @assert issorted(spans)
        @assert allunique(spans)
        @assert issorted(line_starts)
        @assert allunique(line_starts)
        @assert length(line_starts) > 0
        for s in spans
            @assert 0 < s[2] "linenode provenance; expected SourceFile"
            @assert 0 < s[1] <= s[2]+1
        end
        if !isempty(spans)
            @assert !isempty(line_starts)
            min_byte = spans[begin][begin]
            max_byte = maximum(last, spans)
            @assert line_starts[begin] <= min_byte
            for ls in line_starts[begin+1:end]
                @assert min_byte < ls
                @assert ls <= max_byte
            end
        else
            # Not used for now
            @assert false
        end

        new(file, line_offset, spans, line_starts)
    end
end
function SourceByteTable(sf::SourceFile, spans::Vector{Tuple{Int32, Int32}})
    # Trim all newlines outside SBT's range
    line_starts = map(ls->Int32(ls+sf.byte_offset), sf.line_starts)
    b0, _ = JuliaSyntax.source_line_range(sf, spans[1][1])
    first_line = sf.first_line
    while length(line_starts) >= 2 && line_starts[2] <= b0
        popfirst!(line_starts)
        first_line += 1
    end
    max_byte = maximum(last, spans)
    while !isempty(line_starts) && max_byte < line_starts[end]
        pop!(line_starts)
    end
    SourceByteTable(Symbol(sf.filename), first_line, spans, line_starts)
end

function _take32(io::IOBuffer, n::Integer)
    n in (0, 1, 2, 4) || throw(ArgumentError("Unsupported byte count"))
    v = Int32(0)
    n >= 1 && (v |= Int32(read(io, UInt8)))
    n >= 2 && (v |= Int32(read(io, UInt8))<<8)
    n >= 4 && (v |= Int32(read(io, UInt8))<<16)
    n >= 4 && (v |= Int32(read(io, UInt8))<<24)
    return v
end

function _push32(io::IOBuffer, v::Int32, n)
    n in (0, 1, 2, 4) || throw(ArgumentError("Unsupported byte count"))
    n >= 1 && write(io, v % UInt8)
    n >= 2 && write(io, (v>>>8) % UInt8)
    n >= 4 && write(io, (v>>>16) % UInt8)
    n >= 4 && write(io, (v>>>24) % UInt8)
    nothing
end

_encoded_len(max::Int32) = Int32(max == 0 ? 0 :
    max < typemax(UInt8) ? 1 :
    max < typemax(UInt16) ? 2 : 4)

function compress_sbt(sbt::SourceByteTable)
    min_byte = sbt.line_starts[1]
    max_byte = Int32(0)
    max_span = Int32(0)
    for (b1,b2) in sbt.spans
        max_span = max(max_span, (b2+Int32(1))-b1)
        max_byte = max(max_byte, b2)
    end

    max_byte_rel = Int32(min_byte >= max_byte ? 1 : (max_byte - min_byte))
    nlocs::Int32 = length(sbt.spans)
    encl_span = _encoded_len(max_span)
    encl_byte = _encoded_len(max_byte_rel)
    final_len = 14 + # header
        (encl_byte + encl_span) * nlocs +
        (encl_byte * length(sbt.line_starts))

    io = IOBuffer(;sizehint=final_len)
    _push32(io, min_byte, 4)
    _push32(io, sbt.line_offset, 4)
    _push32(io, nlocs, 4)
    _push32(io, encl_byte, 1)
    _push32(io, encl_span, 1)
    for (b1, b2) in sbt.spans
        _push32(io, b1 - min_byte, encl_byte)
        _push32(io, b2 - b1 + Int32(1), encl_span)
    end
    for n in sbt.line_starts
        _push32(io, n - min_byte, encl_byte)
    end

    out = take!(io)
    let l = length(out)
        @assert l == final_len "wrong final length $l"
    end
    return String(out)
end

function uncompress_sbt(di::Core.DebugInfo)
    di.linetable isa String || throw(ArgumentError("linetable: expected string"))
    io = IOBuffer(di.linetable)
    byte_offset = _take32(io, 4)
    line_offset = _take32(io, 4)
    nlocs = _take32(io, 4)
    byte_encl = _take32(io, 1)
    span_encl = _take32(io, 1)

    let newlines_offset = (byte_encl + span_encl) * nlocs
        @assert bytesavailable(io) >= newlines_offset "compressed string too short"
        @assert byte_encl == 0 ||
            (bytesavailable(io) - newlines_offset) % byte_encl == 0 "bad newlines"
    end

    out_spans = Tuple{Int32,Int32}[]
    for i in 1:nlocs
        s1 = _take32(io, byte_encl)
        s2 = _take32(io, span_encl)
        push!(out_spans, (s1+byte_offset, s1+byte_offset+s2-1))
    end

    out_newlines = Int32[]
    while bytesavailable(io) > 0
        push!(out_newlines, _take32(io, byte_encl) + byte_offset)
    end
    return SourceByteTable(di.def, line_offset, out_spans, out_newlines)
end

const LINENODE_SPAN_END = Int32(-5)

# Byte-precise `DebugInfo` requires `Core.DebugInfo` to accept a `String` linetable,
# which is only available on recent Julia.  On older versions (e.g. v1.12) we degrade to
# line-based `DebugInfo` so that lowering still produces a valid `CodeInfo`, at the cost of
# byte-precise source attribution.
const _has_byte_precise_debuginfo =
    hasmethod(Core.DebugInfo, Tuple{Symbol, String, Core.SimpleVector, String})

function _di_pos(st::SyntaxTree)
    src = JuliaSyntax.unexpanded_sourceref(st)
    pos = if src isa SourceRef
        (Int32(first_byte(src)), Int32(last_byte(src)))
    elseif src isa LineNumberNode
        (Int32(src.line), LINENODE_SPAN_END)
    else
        @jl_assert false st
    end
end

# TODO sourcefile(::LNN) should return Symbol, not LNN
_di_sourcefile(st) =
    let x = JuliaSyntax.unexpanded_sourceref(st)
        x isa LineNumberNode ? x.file : x.file[]::SourceFile
    end

# A single pass over all IR to collect unique byte/line positions and CodeInfos
function collect_locs!(node_sources, codeinfos, top_sf, st)
    if kind(st) === K"code_info"
        push!(codeinfos, st)
        # TODO: macro_source is ignored for now
        get!(node_sources, st._id, _di_pos(st))
        for c in children(st[1])
            node_sources[c._id] =
                if _di_sourcefile(c) !== top_sf
                    top_sf isa SourceFile &&
                        @warn "inconsistent provenance for child" c st
                    node_sources[st._id]
                else
                    _di_pos(c)
                end
            collect_locs!(node_sources, codeinfos, top_sf, c)
        end
    elseif !is_leaf(st)
        # Non-toplevel codeinfo can contain nested codeinfo (opaque closures)
        for c in children(st)
            collect_locs!(node_sources, codeinfos, top_sf, c)
        end
    end
    nothing
end

function add_ci_debuginfo!(st::SyntaxTree, file::Symbol,
                           top_sbt::Union{String, Nothing},
                           node_sources::Dict{NodeId, Tuple{Int32, Int32}},
                           spans::Vector{Tuple{Int32, Int32}})
    @jl_assert kind(st) === K"code_info" st
    locs = let a = sizehint!(Vector{Int32}(), 3*numchildren(st[1]))
        for c in children(st[1])
            if top_sbt isa String # precise provenance
                push!(a, Int32(searchsortedfirst(spans, node_sources[c._id])))
            else
                i = searchsortedfirst(spans, node_sources[c._id])
                @jl_assert spans[i][2] == LINENODE_SPAN_END (c, "lno with span end?")
                push!(a, spans[i][1])
            end
            push!(a, Int32(0))
            push!(a, Int32(0))
        end
        a
    end

    setattr!(st, :debuginfo, Core.DebugInfo(
        file, top_sbt, Core.svec(),
        @ccall(jl_compress_codelocs((-1)::Int32, locs::Any,
                                    numchildren(st[1])::Csize_t)::String)))
end

# Populate `.debuginfo` on all K"code_info" in `st`
function add_debuginfo!(st::SyntaxTree)
    @jl_assert kind(st) === K"code_info" st
    node_sources = Dict{NodeId, Tuple{Int32, Int32}}()
    codeinfos = SyntaxList(st._graph)
    top_sf = _di_sourcefile(st)
    collect_locs!(node_sources, codeinfos, top_sf, st)
    byte_precise = _has_byte_precise_debuginfo && top_sf isa SourceFile
    if !byte_precise && top_sf isa SourceFile
        # Without byte-precise support, degrade each byte span to its line number
        # so the line-based path below emits valid `DebugInfo` (same shape as the
        # `LineNumberNode` case).
        for id in collect(keys(node_sources))
            line = Int32(JuliaSyntax.source_line(top_sf, node_sources[id][1]))
            node_sources[id] = (line, LINENODE_SPAN_END)
        end
    end
    spans = sort!(unique(values(node_sources)))
    if byte_precise
        top_sbt = compress_sbt(SourceByteTable(top_sf, spans))
        file = Symbol(top_sf.filename)
    else
        top_sbt = nothing
        file = top_sf isa SourceFile ? Symbol(top_sf.filename) : Symbol(top_sf)
    end
    for ci in codeinfos
        add_ci_debuginfo!(ci, file, top_sbt, node_sources, spans)
    end
end

# flisp: jl_new_code_info_from_ir (method.c)
function compute_ssaflags(st::SyntaxTree)
    @jl_assert kind(st) == K"block" st
    stmts = children(st)
    out = zeros(UInt32, length(stmts))
    inline_flags = Vector{Bool}()
    inbounds_depth = 0
    purity_flags = Vector{UInt32}()

    # Note this should probably go in validation or be a user-facing
    # loweringerror, but method.c only checks this in asserts builds, so we may
    # need to allow these to be unbalanced
    function checked_pop!(stk)
        @jl_assert(!isempty(stk), (st, "ssaflags pop without push"))
        pop!(stk)
    end
    for (i, stmt) in enumerate(stmts)
        is_flag_stmt = true
        @stm stmt begin
            [K"inbounds" [K"Value"]] -> stmt[1].value::Bool ?
                (inbounds_depth += 1) : # push
                (inbounds_depth = 0)    # clear
            [K"inbounds_pop"] -> (inbounds_depth = max(0, inbounds_depth-1))
            [K"boundscheck" _...] -> nothing
            [K"inline" [K"Value"]] -> stmt[1].value::Bool ?
                push!(inline_flags, true) : checked_pop!(inline_flags)
            [K"noinline" [K"Value"]] -> stmt[1].value::Bool ?
                push!(inline_flags, false) : checked_pop!(inline_flags)
            [K"purity"] -> checked_pop!(purity_flags)
            [K"purity" _ _...] -> push!(
                purity_flags,
                UInt32(purity_expr_to_flags(stmt)) << Core.Compiler.NUM_IR_FLAGS)
            _ -> is_flag_stmt = false
        end
        flag = UInt32(0)
        if !isempty(inline_flags)
            flag |= (inline_flags[end] ?
                Core.Compiler.IR_FLAG_INLINE : Core.Compiler.IR_FLAG_NOINLINE)
        end
        if inbounds_depth != 0
            flag |= Core.Compiler.IR_FLAG_INBOUNDS
        end
        if !isempty(purity_flags)
            for pf in purity_flags
                flag |= pf
            end
        end
        out[i] = is_flag_stmt ? UInt32(0) : flag
    end
    @jl_assert length(out) == length(stmts) st
    @jl_assert length(inline_flags) == 0 st
    @jl_assert length(purity_flags) == 0 st
    out
end

# Convert SyntaxTree to the CodeInfo+Expr data structures understood by the
# Julia runtime
function to_code_info(ex::SyntaxTree, slots::Vector{Slot}, meta::CompileHints)
    nargs = sum((s.kind==:argument for s in slots), init=0)
    slotnames = Vector{Symbol}(undef, length(slots))
    slot_rename_inds = Dict{String,Int}()
    slotflags = Vector{UInt8}(undef, length(slots))
    for (i, slot) in enumerate(slots)
        name = slot.name
        # TODO: Do we actually want unique names here? The C code in
        # `jl_new_code_info_from_ir` has logic to simplify gensym'd names and
        # use the empty string for compiler-generated bindings.
        if name !== UNUSED
            ni = get(slot_rename_inds, name, 0)
            slot_rename_inds[name] = ni + 1
            if ni > 0
                name = "$name@$ni"
            end
        end
        sname = Symbol(name)
        slotnames[i] = sname
        slotflags[i] =                   # Inference          | Codegen
            slot.is_read          << 3 | # SLOT_USED          | jl_vinfo_sa
            slot.is_single_assign << 4 | # SLOT_ASSIGNEDONCE  | -
            slot.is_maybe_undef   << 5 | # SLOT_USEDUNDEF     | jl_vinfo_usedundef
            slot.is_called        << 6   # SLOT_CALLED        | -
    end

    stmts = map(_to_lowered_expr, children(ex[1]))
    has_image_globalref = any(codeinfo_has_image_globalref, stmts)
    ssaflags = compute_ssaflags(ex[1])
    propagate_inbounds =
        get(meta, :propagate_inbounds, false)
    # TODO: Set true if there's a foreigncall
    has_fcall = false
    nospecializeinfer =
        get(meta, :nospecializeinfer, false)
    inlining =
        get(meta, :inline, false) ? 0x01 :
        get(meta, :noinline, false) ? 0x02 : 0x00
    constprop =
        get(meta, :aggressive_constprop, false) ? 0x01 :
        get(meta, :no_constprop, false) ? 0x02 : 0x00
    purity =
        let eo = get(meta, :purity, nothing)
            isnothing(eo) ? 0x0000 : eo::UInt16
        end

    # The following CodeInfo fields always get their default values for
    # uninferred code.
    ssavaluetypes      = length(stmts) # Why does the runtime code do this?
    slottypes          = nothing
    parent             = nothing
    method_for_inference_limit_heuristics = nothing
    edges               = nothing
    min_world           = Csize_t(1)
    max_world           = typemax(Csize_t)
    isva                = false
    inlining_cost       = 0xffff
    rettype             = Any

    @jl_assert(length(stmts) == numchildren(ex[1]), ex)

    _CodeInfo(
        stmts,
        ex.debuginfo,
        ssavaluetypes,
        ssaflags,
        slotnames,
        slotflags,
        slottypes,
        rettype,
        parent,
        edges,
        min_world,
        max_world,
        method_for_inference_limit_heuristics,
        nargs,
        propagate_inbounds,
        has_fcall,
        has_image_globalref,
        nospecializeinfer,
        isva,
        inlining,
        constprop,
        purity,
        inlining_cost
    )
end

@fzone "JL: to_lowered_expr" function to_lowered_expr(ex::SyntaxTree)
    ensure_attributes!(ex._graph; debuginfo=Any)
    add_debuginfo!(ex)
    _to_lowered_expr(ex)
end

function _to_lowered_expr(ex::SyntaxTree)
    k = kind(ex)
    if is_literal(k)
        ex.value
    elseif k == K"nothing"
        nothing
    elseif k == K"core"
        GlobalRef(Core, Symbol(ex.name_val::String))
    elseif k == K"top"
        GlobalRef(Base, Symbol(ex.name_val::String))
    elseif k == K"globalref"
        GlobalRef(ex.mod::Module, Symbol(ex.name_val::String))
    elseif k == K"Identifier"
        # Implicitly refers to name in parent module
        # TODO: Should we even have plain identifiers at this point or should
        # they all effectively be resolved into GlobalRef earlier?
        Symbol(ex.name_val::String)
    elseif k == K"SourceLocation"
        QuoteNode(source_location(LineNumberNode, ex))
    elseif k == K"Symbol"
        QuoteNode(Symbol(ex.name_val::String))
    elseif k == K"slot"
        Core.SlotNumber(ex.var_id::IdTag)
    elseif k == K"static_parameter"
        Expr(:static_parameter, ex.var_id::IdTag)
    elseif k == K"SSAValue"
        Core.SSAValue(ex.var_id::IdTag)
    elseif k == K"return"
        Core.ReturnNode(_to_lowered_expr(ex[1]))
    elseif k == K"inert"
        est_to_expr(remove_scope_layer!(ex))
    elseif k == K"inert_syntaxtree"
        ex[1]
    elseif k == K"code_info"
        ir = to_code_info(ex, ex.slots, ex.meta)
        if ex.is_toplevel_thunk
            Expr(:thunk, ir) # TODO: Maybe nice to just return a CodeInfo here?
        else
            ir
        end
    elseif k == K"Value"
        # TODO: we still do this in some interpolation, genfunc situations
        # @jl_assert !isa_lowering_ast_node(ex.value) (
        #     ex, string("smuggling AST through Value is asking for trouble; ",
        #                "find a SyntaxTree representation"))
        ex.value isa LineNumberNode ? QuoteNode(ex.value) : ex.value
    elseif k == K"goto"
        Core.GotoNode(ex[1].id)
    elseif k == K"gotoifnot"
        Core.GotoIfNot(_to_lowered_expr(ex[1]), ex[2].id)
    elseif k == K"enter"
        catch_idx = ex[1].id
        numchildren(ex) == 1 ?
            Core.EnterNode(catch_idx) :
            Core.EnterNode(catch_idx, _to_lowered_expr(ex[2]))
    elseif k == K"method"
        cs = map(_to_lowered_expr, children(ex))
        # Ad-hoc unwrapping to satisfy `Expr(:method)` expectations
        cs1 = cs[1]
        c1 = cs1 isa QuoteNode ? cs1.value : cs1
        Expr(:method, c1, cs[2:end]...)
    elseif k == K"newvar"
        Core.NewvarNode(_to_lowered_expr(ex[1]))
    elseif k == K"opaque_closure_method"
        args = map(_to_lowered_expr, children(ex))
        # opaque_closure_method has special non-evaluated semantics for the
        # `functionloc` line number node so we need to undo a level of quoting
        arg4 = args[4]
        @jl_assert arg4 isa QuoteNode ex
        args[4] = arg4.value
        Expr(:opaque_closure_method, args...)
    elseif k == K"meta"
        args = Any[_to_lowered_expr(e) for e in children(ex)]
        # Unpack K"Symbol" QuoteNode as `Expr(:meta)` requires an identifier here.
        arg1 = args[1]
        @jl_assert (arg1 isa QuoteNode) ex
        args[1] = arg1.value
        Expr(:meta, args...)
    elseif k == K"foreignsymbol"
        @jl_assert kind(ex[1]) == K"tuple" ex
        _foreignsymbol_expr(ex[1])
    elseif k == K"static_eval"
        @jl_assert numchildren(ex) == 1 ex
        _to_lowered_expr(ex[1])
    elseif k == K"cfunction"
        # For a scope-resolved callable (`K"static_eval"`), drop the module tag
        # and emit a bare Symbol so `method.c` resolves it in the method's
        # module at eval time, matching Base `@cfunction`'s runtime semantics.
        ret = Expr(:cfunction)
        for (i, e) in enumerate(children(ex))
            if i == 2 && kind(e) == K"static_eval" && kind(e[1]) == K"globalref"
                push!(ret.args, QuoteNode(Symbol(e[1].name_val::String)))
            else
                push!(ret.args, _to_lowered_expr(e))
            end
        end
        return ret
    elseif k in KSet"inline noinline inbounds inbounds_pop purity"
        # only used in compute_ssaflags (see method.c)
        nothing
    else
        # Allowed forms according to https://docs.julialang.org/en/v1/devdocs/ast/
        #
        # call invoke static_parameter `=` method struct_type abstract_type
        # primitive_type global const new splatnew isdefined
        # enter leave pop_exception inbounds boundscheck loopinfo copyast meta
        # lambda
        head = k == K"call"      ? :call       :
               k == K"new"       ? :new        :
               k == K"splatnew"  ? :splatnew   :
               k == K"="         ? :(=)        :
               k == K"leave"     ? :leave      :
               k == K"isdefined" ? :isdefined  :
               k == K"loopinfo"  ? :loopinfo   :
               k == K"boundscheck"       ? :boundscheck       :
               k == K"latestworld"       ? :latestworld       :
               k == K"pop_exception"     ? :pop_exception     :
               k == K"captured_local"    ? :captured_local    :
               k == K"gc_preserve_begin" ? :gc_preserve_begin :
               k == K"gc_preserve_end"   ? :gc_preserve_end   :
               k == K"foreigncall"       ? :foreigncall       :
               k == K"foreignglobal"     ? :foreignglobal     :
               k == K"cfunction"         ? :cfunction         :
               k == K"aliasscope"        ? :aliasscope        :
               k == K"popaliasscope"     ? :popaliasscope     :
               k == K"new_opaque_closure" ? :new_opaque_closure :
               nothing
        if isnothing(head)
            throw(LoweringError(ex, "Unhandled form for kind $k"))
        end
        ret = Expr(head)
        for e in children(ex)
            push!(ret.args, _to_lowered_expr(e))
        end
        return ret
    end
end

# ultra-permissive conversion allowing unlowered structure, but lowered leaves
function _foreignsymbol_expr(ex)
    if is_leaf(ex) || kind(ex) == K"inert"
        _to_lowered_expr(ex)
    else
        k = kind(ex)
        Expr(Symbol((k === K"unknown_head" ? ex.name_val : untokenize(k))::String),
             map(_foreignsymbol_expr, children(ex))...)
    end
end

#-------------------------------------------------------------------------------
# Our version of eval - should be upstreamed though?
@fzone "JL: eval" function eval(mod::Module, ex::SyntaxTree;
                                macro_world::UInt=Base.get_world_counter(),
                                soft_scope::Union{Nothing,Bool}=nothing,
                                opts...)
    iter = lower_init(ex; opts...)
    _eval(mod, iter; soft_scope)
end

# Version of eval() taking `Expr` (or Expr tree leaves of any type)
function eval(mod::Module, @nospecialize(ex); opts...)
    eval(mod, expr_to_est(ex); opts...)
end

function _eval(mod::Module, iter::LoweringIterator; soft_scope::Union{Nothing,Bool}=nothing)
    modules = Module[mod]
    result = nothing
    while true
        thunk = lower_step(iter, modules[end], Base.get_world_counter(); soft_scope)::Core.SimpleVector
        type = thunk[1]::Symbol
        if type == :done
            break
        elseif type == :begin_module
            filename = something(thunk[5].file, :none)
            mod = @ccall jl_begin_new_module(
                modules[end]::Any, thunk[3]::Symbol, thunk[2]::Any, thunk[4]::Cint,
                filename::Cstring, thunk[5].line::Cint)::Module
            push!(modules, mod)
        elseif type == :end_module
            @ccall jl_end_new_module(modules[end]::Module)::Cvoid
            result = pop!(modules)
        else
            @assert type == :thunk
            result = Core.eval(modules[end], thunk[2])
        end
    end
    @assert length(modules) === 1
    return result
end

"""
    include(mod::Module, path::AbstractString)

Evaluate the contents of the input source file in the global scope of module
`mod`. Every module (except those defined with baremodule) has its own
definition of `include()` omitting the `mod` argument, which evaluates the file
in that module. Returns the result of the last evaluated expression of the
input file. During including, a task-local include path is set to the directory
containing the file. Nested calls to include will search relative to that path.
This function is typically used to load source interactively, or to combine
files in packages that are broken into multiple source files.
"""
function include(mod::Module, path::AbstractString)
    path, prev = Base._include_dependency(mod, path)
    code = read(path, String)
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    try
        return include_string(mod, code, path)
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
end

"""
    include_string(mod::Module, code::AbstractString, filename::AbstractString="string")

Like `include`, except reads code from the given string rather than from a file.
"""
function include_string(mod::Module, code::AbstractString, filename::AbstractString="string";
                        expr_compat_mode=false, version::VersionNumber=VERSION)
    eval(mod, parseall(SyntaxTree, code; filename=filename, version=version); expr_compat_mode)
end

include(path::AbstractString) = include(JuliaLowering, path)
