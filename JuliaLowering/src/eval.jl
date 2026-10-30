# Non-incremental lowering API for non-toplevel non-module expressions.
# May be removed?

function lower(mod::Module, ex_in::SyntaxTree; soft_scope::Union{Nothing,Bool}=nothing)
    ex0 = rebase_layers(ex_in, mod)
    world = Base.get_world_counter()
    ex1 = expand_forms_1(ex0, world, true)
    ctx2, ex2 = expand_forms_2(ex1, world)
    ctx3, ex3 = resolve_scopes(ctx2, ex2; soft_scope)
    ctx4, ex4 = convert_closures(ctx3, ex3)
    _ctx5, ex5 = linearize_ir(ctx4, ex4)
    ex5
end

function macroexpand(mod::Module, ex_in::SyntaxTree; recursive::Bool=true)
    # TODO: prevent this from being called in macro bodies
    ex0 = rebase_layers(ex_in, mod)
    expand_forms_1(ex0, Base.get_world_counter(), recursive)
end

"May be used in macros or from any module"
function macroexpand(st::SyntaxTree)
    DEBUG && assert_expandable(st)
    ctx = MacroExpansionContext(st, Base.get_world_counter(), true)
    expand_forms_1(ctx, st)
end

# If a top-level thunk has any existing layer, we can assume all syntax has the
# same base layer: either it was produced by a macro expansion and went through
# `apply_expansion_layer`, or it was produced by parsing (no layers).

# We ignore old the base layer's module, which should usually be the same as the
# current lowering module.  (counterexample: macroexpand in mod A producing
# escaped :toplevel st, then eval st in mod B, but flisp does the same thing by
# spamming globalrefs to mod A throughout st).
function rebase_layers(st, mod::Module)
    out = if st.context.layer === nothing
        # assert no layers throughout tree
        sc = SyntaxContext(mod, st.context.edition)
        fill_context(st, sc)
    else
        base = base_layer(st.context)
        newbase = ScopeLayer(mod, nothing)
        _rebase_layers(
            st, Dict{ScopeLayer, ScopeLayer}(base=>newbase),
            Dict{SyntaxContext, SyntaxContext}())
    end
    DEBUG && assert_expandable(out)
    out
end

function _rebase_layers(st, slmap, scmap)
    sc = st.context
    sc2 = get(scmap, sc, nothing)
    if isnothing(sc2)
        sl2 = _get_sl!(slmap, sc.layer)
        sc2 = scmap[sc] = SyntaxContext(sl2, sc.unexpanded, sc.edition, sc.internal)
    end
    if is_leaf(st) || numchildren(st) == 0
        @mknode(st; context=sc2)
    else
        cs = mapsyntax(c->_rebase_layers(c, slmap, scmap), children(st))
        @mknode(st; context=sc2, children=cs)
    end
end

function _get_sl!(slmap, sl::ScopeLayer)
    out = get(slmap, sl, nothing)
    out isa ScopeLayer && return out
    slmap[sl] = ScopeLayer(
        sl.mod, isnothing(sl.escaped) ? nothing : _get_sl!(slmap, sl.escaped))
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

struct LoweringIterator
    todo::Vector{Tuple{SyntaxTree, Bool, Int}}
end

function lower_init(ex::SyntaxTree)
    LoweringIterator([(ex, false, 0)])
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

    k = head(ex)
    if k !== :toplevel && k !== :module
        ex = rebase_layers(ex, mod)
        ex = expand_forms_1(ex, world, true)
        k = head(ex)
    end
    if k == :toplevel
        push!(iter.todo, (ex, false, 1))
        return lower_step(iter, mod, world; soft_scope)
    elseif k == :module
        (version, notbare, mname, body) = @stm ex begin
            [:module version nb_st mname body] ->
                (version.value, nb_st.value, mname, body)
            [:module nb_st mname body] ->
                (nothing, nb_st.value, mname, body)
        end
        if head(mname) != :identifier
            throw(LoweringError(mname, "Expected module name"))
        end
        if head(body) != :block
            throw(LoweringError(body, "module expression third argument must be a block"))
        end
        newmod_name = Symbol(syntax_name(mname))
        coverage_visit_line(mod, ex)
        loc = first_linenode(ex)
        push!(iter.todo, (body, true, 1))
        return Core.svec(:begin_module, version, newmod_name, notbare, loc)
    elseif k === :thunk && numchildren(ex) == 1
        return Core.svec(:thunk, Expr(:thunk, ex[1].value))
    else
        if (k === :error || k === :incomplete) && numchildren(ex) == 1
            ex = _error_to_throw(ex)
        end
        coverage_visit_line(mod, ex)
         ctx2, ex2 = expand_forms_2(ex, world)
         ctx3, ex3 = resolve_scopes(ctx2, ex2, !is_module_body; soft_scope)
         ctx4, ex4 = convert_closures(ctx3, ex3)
        _ctx5, ex5 = linearize_ir(ctx4, ex4)
        out = to_lowered_expr(ex5)
        return Core.svec((out isa Core.CodeInfo ? :value : :thunk), out)
    end
end

# We want a "top-level scope" stack frame (see `jl_eval_throw`)
function _error_to_throw(ex)
    err = ex[1].value
    exc = err isa String ? ErrorException(string("syntax: ", err)) : err
    isa_lowering_ast_node(exc) && throw(exc)
    @mknode(ex; head=:call, children=Syntax[
        newleaf(ex, :value, Core.throw), newleaf(ex, :value, exc)])
end

function coverage_visit_line(mod, st)
    Base.JLOptions().code_coverage == 0 && return
    lno = Base.first_linenode(st)
    (lno.line > 0 && lno.file isa Symbol) || return
    file = String(lno.file)
    if ccall(:jl_coverage_enabled_for, Cint, (Any, Cstring), mod, file) != 0
        ccall(:jl_coverage_visit_line, Cvoid, (Cstring, Csize_t, Cint),
              file, sizeof(file), lno.line)
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

function codeinfo_has_fcall(@nospecialize(e))
    if e isa Expr
        if e.head === :(=)
            return codeinfo_has_fcall(e.args[2])
        end
        return e.head === :foreigncall || e.head === :foreignglobal ||
            e.head === :cfunction
    end
    return false
end

let fns = fieldnames(Core.CodeInfo), fts = fieldtypes(Core.CodeInfo),
    conversions = [:(convert($t, $n)) for (t, n) in zip(fts, fns)]

    @eval function _CodeInfo($(fns...))
        $(Expr(:new, :(Core.CodeInfo), conversions...))
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
            @assert 0 < s[2] "linenode provenance; expected SourceCode"
            @assert 0 < s[1] <= s[2]+1
        end
        if !isempty(spans)
            @assert !isempty(line_starts)
            min_byte = spans[begin][begin]
            max_byte = maximum(maximum, spans)
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
function SourceByteTable(sf::SourceCode, spans::Vector{Tuple{Int32, Int32}})
    # Trim all newlines outside SBT's range
    line_starts = map(ls->Int32(ls+sf.byte_offset), sf.line_starts)
    first_line = sf.first_line
    while length(line_starts) >= 2 && line_starts[2] <= spans[1][1]
        popfirst!(line_starts)
        first_line += 1
    end
    max_byte = maximum(maximum, spans)
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

# JETLS compat: Core.Debuginfo is not byte-precise before 1.14
const _has_byte_precise_debuginfo =
    hasmethod(Core.DebugInfo, Tuple{Symbol, String, Core.SimpleVector, String})

function _di_pos(st::SyntaxTree, expanded=false)
    src = (expanded ? sourceref : unexpanded_sourceref)(st)
    pos = if src isa SourceRef
        (Int32(first_byte(src)), Int32(last_byte(src)))
    elseif src isa LineNumberNode
        (Int32(src.line), LINENODE_SPAN_END)
    else
        @jl_assert false st
    end
end

function _di_sourcefile(st, expanded=false)
    x = (expanded ? sourceref : unexpanded_sourceref)(st)
    x isa LineNumberNode ? something(x.file, :var"") : x.code::SourceCode
end

# A single pass over all IR to collect unique byte/line positions and CodeInfos.
#
# Macro provenance: The previous LineNumberNode-based system achieved a sane
# amount of debuginfo by accidentally losing most of it (e.g. different
# expansions but same provenance file name, or user-deleted line nodes, or lack
# of macro-def line nodes).  Here, we have the problem that we need to omit some
# information.  Only keep the pre-expansion provenance, as we always do, as well
# as the latest-expansion provenance (if any) as a DebugInfo edge.
function collect_locs(st::Syntax)
    codeinfos = Syntax[]
    files = Union{Symbol, SourceCode}[]
    spansets = Set{Tuple{Int32, Int32}}[]
    top_file = _di_sourcefile(st)
    top_spanset = Set{Tuple{Int32, Int32}}()
    push!(top_spanset, _di_pos(st))
    _collect_locs!(st, codeinfos, top_file, top_spanset, files, spansets)
    codeinfos, top_file, top_spanset, files, spansets
end
function _collect_locs!(st, codeinfos, top_file, top_spanset, files, spansets)
    collect1(file, pos) = let i = findfirst(==(file), files)
        if i === nothing
            push!(files, file)
            push!(spansets, let s = Set{Tuple{Int32, Int32}}(); push!(s, pos); end)
        else
            push!(spansets[i], pos)
        end
    end
    if head(st) === :code_info
        push!(codeinfos, st)
        for c in children(st[2])
            c_file = _di_sourcefile(c)
            if c_file === top_file
                push!(top_spanset, _di_pos(c))
            else
                # unexpected when c_file isa sourcecode (likely need further
                # restriction in apply_expansion_layer or adopt_scope)
                @jl_assert c_file isa Symbol c
                collect1(c_file, _di_pos(c))
            end
            mfile = _di_sourcefile(c, true)
            macro_prov(c) === nothing ||
                collect1(mfile, _di_pos(c, true))
            _collect_locs!(c, codeinfos, top_file, top_spanset, files, spansets)
        end
    elseif !is_leaf(st)
        # Non-toplevel codeinfo can contain nested codeinfo (opaque closures)
        for c in children(st)
            _collect_locs!(c, codeinfos, top_file, top_spanset, files, spansets)
        end
    end
    nothing
end

function add_ci_debuginfo!(st::SyntaxTree,
                           top_file::Union{SourceCode, Symbol},
                           top_sbt::Union{String, Nothing},
                           top_spans::Vector{Tuple{Int32, Int32}},
                           edge_files::Vector{Union{Symbol, SourceCode}},
                           edge_spans::Vector{Vector{Tuple{Int32, Int32}}},
                           edges::Core.SimpleVector)
    @jl_assert head(st) === :code_info && head(st[2]) === :block st
    locs = let a = sizehint!(Vector{Int32}(), 3*numchildren(st[2]))
        local out_1 = Int32(0)
        for c in children(st[2])
            # Need (top_spans_i, edge_i, edge_spans[edge_i]_i)
            out_2, out_3 = Int32(0), Int32(0)
            ufile = _di_sourcefile(c)
            upos = _di_pos(c)
            foreign_linenode = false
            # false due to trim_provenance (`@eval`, generated-func desugaring)
            # @jl_assert typeof(top_file) === typeof(ufile) c st
            if ufile isa SourceCode # precise provenance
                out_1 = Int32(searchsortedfirst(top_spans, upos))
            elseif ufile === top_file
                i = searchsortedfirst(top_spans, upos)
                @jl_assert top_spans[i][2] == LINENODE_SPAN_END (c, "lno with span end?")
                out_1 = top_spans[i][1]
            end
            if ufile !== top_file
                # this loc must be from Expr translation, so assume this is more
                # useful than the expanded provenance (which is almost(?)
                # certainly the same as the unexpanded provenance), and use
                # out_1 from the previous iteration
                out_2 = edge_i = findfirst(==(ufile), edge_files)
                @jl_assert out_2 !== nothing (st, "macro loc not collected?")
                out_3 = Int32(searchsortedfirst(edge_spans[edge_i], upos))
            elseif macro_prov(c) !== nothing
                efile = _di_sourcefile(c, true)
                epos = _di_pos(c, true)
                if efile !== ufile || epos !== upos
                    out_2 = edge_i = findfirst(==(efile), edge_files)
                    @jl_assert out_2 !== nothing (st, "macro loc not collected?")
                    out_3 = Int32(searchsortedfirst(edge_spans[edge_i], epos))
                end
            end
            push!(a, out_1)
            push!(a, out_2)
            push!(a, out_3)
        end
        a
    end
    top_filename = top_file isa SourceCode ? top_file.filename : top_file
    setmeta!(st, :debuginfo, Core.DebugInfo(
        # We don't trim edges (this svec is likely to not need it, and small in
        # the first place.)  We'd also need to re-calculate indices if we did.
        top_filename, top_sbt, Core.svec(edges...), compress_codelocs(locs)))
end

function compress_codelocs(locs::Vector{Int32})
    l = length(locs)
    @assert mod(l, 3) == 0
    @ccall(jl_compress_codelocs((-1)::Int32, locs::Any, div(l, 3)::Csize_t)::String)
end

function mk_edge_debuginfo(file::Union{SourceCode, Symbol}, spans)
    if file isa SourceCode
        locs = compress_codelocs(
            Int32[x for j in eachindex(spans) for x in (j, 0, 0)])
        Core.DebugInfo(
            file.filename,
            compress_sbt(SourceByteTable(file, spans)), Core.svec(), locs)
    else
        locs = compress_codelocs(Int32[x for s in spans for x in (s[1], 0, 0)])
        Core.DebugInfo(file, nothing, Core.svec(), locs)
    end
end

# Populate `.debuginfo` on all :code_info in `st`
function add_debuginfo!(st::SyntaxTree)
    @jl_assert head(st) === :code_info st
    codeinfos, top_file, top_spanset, e_files, e_spansets = collect_locs(st)
    if !_has_byte_precise_debuginfo # lowered code isn't executable
        for ci in codeinfos
            setmeta!(ci, :debuginfo, Core.NullDebugInfo)
        end
        return
    end
    e_spans = Vector{Tuple{Int32, Int32}}[]
    e_debuginfos = Core.DebugInfo[]
    for (file, spanset) in zip(e_files, e_spansets)
        spans = sort!(collect(spanset))
        push!(e_spans, spans)
        push!(e_debuginfos, mk_edge_debuginfo(file, spans))
    end
    top_spans = sort!(collect(top_spanset))
    di1 = mk_edge_debuginfo(top_file, top_spans)
    for ci in codeinfos
        add_ci_debuginfo!(
            ci, top_file, di1.linetable, top_spans, e_files, e_spans,
            Core.svec(e_debuginfos...))
    end
end

# flisp: jl_new_code_info_from_ir (method.c)
function compute_ssaflags(st::SyntaxTree)
    @jl_assert head(st) == :block st
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
            [:inbounds [:value]] -> stmt[1].value::Bool ?
                (inbounds_depth += 1) : # push
                (inbounds_depth = 0)    # clear
            [:inbounds_pop] -> (inbounds_depth = max(0, inbounds_depth-1))
            [:boundscheck _...] -> nothing
            [:inline [:value]] -> stmt[1].value::Bool ?
                push!(inline_flags, true) : checked_pop!(inline_flags)
            [:noinline [:value]] -> stmt[1].value::Bool ?
                push!(inline_flags, false) : checked_pop!(inline_flags)
            [:purity] -> checked_pop!(purity_flags)
            [:purity _ _...] -> push!(
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
function to_code_info(ex::SyntaxTree)
    slots = ex[1].value::Vector{Slot}
    meta = ex.meta
    nargs = sum((s.kind==:argument for s in slots), init=0)
    slotnames = Vector{Symbol}(undef, length(slots))
    slotflags = Vector{UInt8}(undef, length(slots))
    for (i, slot) in enumerate(slots)
        name = slot.name
        if name !== UNUSED && startswith(name, ERASE_SLOTNAME_PREFIX)
            name = ""
        elseif i > 1 && startswith(name, "##")
            name = name[3:end]
        end
        sname = Symbol(name)
        slotnames[i] = sname
        slotflags[i] =                   # Inference          | Codegen
            slot.is_read          << 3 | # SLOT_USED          | jl_vinfo_sa
            slot.is_single_assign << 4 | # SLOT_ASSIGNEDONCE  | -
            slot.is_maybe_undef   << 5 | # SLOT_USEDUNDEF     | jl_vinfo_usedundef
            slot.is_called        << 6   # SLOT_CALLED        | -
    end

    stmts = Any[_to_lowered_expr(e) for e in children(ex[2])]
    has_image_globalref = any(codeinfo_has_image_globalref, stmts)
    ssaflags = compute_ssaflags(ex[2])
    propagate_inbounds =
        get(meta, :propagate_inbounds, false)
    has_fcall = any(codeinfo_has_fcall, stmts)
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

    @jl_assert(length(stmts) == numchildren(ex[2]), ex)

    _CodeInfo(
        stmts,
        getmeta(ex, :debuginfo, nothing),
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

"""
This pass convert's JuliaLowering's internal representation of untyped IR into
a form the Julia runtime understands. This is a necessary decoupling which
separates the development of JuliaLowering.jl from the evolution of the Julia
runtime itself.
"""
@fzone "JL: to_lowered_expr" function to_lowered_expr(ex::SyntaxTree)
    @jl_assert head(ex) === :thunk || head(ex) === :code_info ex
    add_debuginfo!(head(ex) === :thunk ? ex[1] : ex)
    _to_lowered_expr(ex)
end

function _to_lowered_expr(ex::SyntaxTree)
    k = head(ex)
    if k == :nothing
        nothing
    elseif k == :core
        GlobalRef(Core, Symbol(syntax_name(ex)))
    elseif k == :top
        GlobalRef(Base, Symbol(syntax_name(ex)))
    elseif k == :globalref
        GlobalRef(ex.mod::Module, Symbol(syntax_name(ex)))
    elseif k == :identifier
        # TODO: assert false (only reachable from simdloop?)
        Symbol(syntax_name(ex))
    elseif k == :sourcelocation
        QuoteNode(first_linenode(ex))
    elseif k == :symbol
        QuoteNode(Symbol(syntax_name(ex)))
    elseif k == :slot
        Core.SlotNumber(syntax_id(ex))
    elseif k == :static_parameter
        Expr(:static_parameter, syntax_id(ex))
    elseif k == :ssavalue
        Core.SSAValue(syntax_id(ex))
    elseif k == :return
        v = _to_lowered_expr(ex[1])
        @jl_assert Base.Compiler.is_valid_return(v) ex
        Core.ReturnNode(v)
    elseif k == :inert
        est_to_expr(ex)
    elseif k == :syntaxinert
        ex[1]
    elseif k == :code_info
        to_code_info(ex)
    elseif k == :value
        @jl_assert !isa_lowering_ast_node(ex.value) (
            ex, string("smuggling AST through Value is asking for trouble; ",
                       "find a SyntaxTree representation"))
        ex.value isa LineNumberNode ? QuoteNode(ex.value) : ex.value
    elseif k == :goto
        Core.GotoNode(syntax_id(ex[1]))
    elseif k == :gotoifnot
        Core.GotoIfNot(_to_lowered_expr(ex[1]), syntax_id(ex[2]))
    elseif k == :enter
        catch_idx = syntax_id(ex[1])
        numchildren(ex) == 1 ?
            Core.EnterNode(catch_idx) :
            Core.EnterNode(catch_idx, _to_lowered_expr(ex[2]))
    elseif k == :newvar
        Core.NewvarNode(_to_lowered_expr(ex[1]))
    elseif k == :opaque_closure_method
        args = Any[_to_lowered_expr(e) for e in children(ex)]
        # opaque_closure_method has special non-evaluated semantics for the
        # `functionloc` line number node so we need to undo a level of quoting
        arg4 = args[4]
        @jl_assert arg4 isa QuoteNode ex
        args[4] = arg4.value
        Expr(:opaque_closure_method, args...)
    elseif k == :meta
        args = Any[_to_lowered_expr(e) for e in children(ex)]
        # Unpack :symbol QuoteNode as `Expr(:meta)` requires an identifier here.
        for i in eachindex(args)
            args[i] isa QuoteNode && (args[i] = args[i].value)
        end
        Expr(:meta, args...)
    elseif k == :throw_undef_if_not
        Expr(k, Symbol(syntax_name(ex[1])), _to_lowered_expr(ex[2]))
    elseif k == :foreignsymbol
        # foreignsymbol wraps the first argument of a foreigncall /
        # foreignglobal when it should not be lowered (and should mostly be
        # treated as :inert), but requires scope resolution and special
        # conversion to Expr.
        @jl_assert head(ex[1]) == :tuple ex
        _foreignsymbol_expr(ex[1])
    elseif k == :static_eval
        @jl_assert numchildren(ex) == 1 ex
        _to_lowered_expr(ex[1])
    elseif k == :cfunction
        # For a scope-resolved callable (`:static_eval`), drop the module tag
        # and emit a bare Symbol so `method.c` resolves it in the method's
        # module at eval time, matching Base `@cfunction`'s runtime semantics.
        ret = Expr(:cfunction)
        for (i, e) in enumerate(children(ex))
            if i == 2 && head(e) == :static_eval && head(e[1]) == :globalref
                push!(ret.args, QuoteNode(Symbol(syntax_name(e[1]))))
            else
                push!(ret.args, _to_lowered_expr(e))
            end
        end
        return ret
    elseif k in (:inline, :noinline, :inbounds, :inbounds_pop, :purity)
        # only used in compute_ssaflags (see method.c)
        nothing
    else
        k == :call ||
            k == :new ||
            k == :splatnew ||
            k == :(=) ||
            k == :leave ||
            k == :copyast ||
            k == :isdefined ||
            k == :loopinfo ||
            k == :thunk ||
            k == :boundscheck ||
            k == :latestworld ||
            k == :pop_exception ||
            k == :captured_local ||
            k == :gc_preserve_begin ||
            k == :gc_preserve_end ||
            k == :foreigncall ||
            k == :foreignglobal ||
            k == :cfunction ||
            k == :aliasscope ||
            k == :popaliasscope ||
            k == :new_opaque_closure ||
            k == :the_exception ||
            throw(LoweringError(ex, "Unknown syntax form $k"))
        ret = Expr(k)
        for e in children(ex)
            push!(ret.args, _to_lowered_expr(e))
        end
        return ret
    end
end

# ultra-permissive conversion allowing unlowered structure, but lowered leaves
function _foreignsymbol_expr(ex)
    if is_leaf(ex) || head(ex) == :inert
        _to_lowered_expr(ex)
    else
        Expr(head(ex), Any[_foreignsymbol_expr(e) for e in children(ex)]...)
    end
end

@fzone "JL: eval" function eval(mod::Module, @nospecialize(ex);
                                soft_scope::Union{Nothing,Bool}=nothing)
    if !(ex isa SyntaxTree)
        ex = expr_to_est(ex)
    end
    return _eval(mod, lower_init(ex), false, soft_scope)
end

# TODO: Consumers expect LoadError from macros and ErrorException from lowering,
# but better printing could be done if we were to change this.
function to_compatible_exception(@nospecialize(exc))
    if exc isa LoweringError && !exc.internal
        ErrorException(string("syntax: ", sprint(showerror, exc)))
    elseif exc isa MacroExpansionError
        loc = _macrocall_expr_location(exc.context.macrocall)
        lnn = loc isa MacroSource ? loc.lno : loc
        file = lnn.file isa Symbol && lnn.file !== :var"" ? String(lnn.file) : "<macrocall>"
        LoadError(file, lnn.line, to_compatible_exception(exc.err))
    else
        exc
    end
end

function _eval(mod::Module, iter::LoweringIterator,
               error_compat::Bool, soft_scope::Union{Nothing,Bool})
    modules = Module[mod]
    result = nothing
    while true
        thunk = try
            invoke_in_lowering_world(
                ()->lower_step(iter, modules[end],
                               Base.get_world_counter(); soft_scope))::Core.SimpleVector
        catch exc
            error_compat ? rethrow(to_compatible_exception(exc)) : rethrow()
        end
        type = thunk[1]::Symbol
        if type == :done
            break
        elseif type == :begin_module
            loc = thunk[5]::LineNumberNode
            filename = something(loc.file, :var"")::Symbol
            mod = @ccall jl_begin_new_module(
                modules[end]::Any, thunk[3]::Symbol, thunk[2]::Any, (thunk[4]::Bool)::Cint,
                filename::Cstring, loc.line::Cint)::Module
            push!(modules, mod)
        elseif type == :end_module
            @ccall jl_end_new_module(modules[end]::Module)::Cvoid
            result = pop!(modules)
        elseif type == :thunk
            code = thunk[2]
            if !(Meta.isexpr(code, :thunk, 1) && code.args[1] isa Core.CodeInfo)
                throw(ErrorException("syntax: expected (thunk x::CodeInfo)"))
            end
            result = @ccall jl_eval_thunk(
                modules[end]::Any, code.args[1]::Any, #=fast=#1::Cint)::Any
        elseif type == :value
            result = thunk[2]
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
function include_string(mapexpr::Function, mod::Module, code::AbstractString,
                        filename::AbstractString)
    version = if invokelatest(isdefined, mod, Symbol("#_internal_julia_parse"))
        vp = invokelatest(getglobal, mod, Symbol("#_internal_julia_parse"))
        vp isa Base.VersionedParse ? vp.edition : Base.LATEST_STABLE_EDITION
    else
        Base.LATEST_STABLE_EDITION
    end
    st = parseall(SyntaxTree, code; filename, version=VersionNumber(version),
                  ignore_warnings=true)
    @jl_assert head(st) === :toplevel st
    if mapexpr !== identity
        # TODO: Is there any way to support provenance here?
        local last = nothing
        sc = SyntaxContext(mod, version)
        for c in children(st)
            last = eval(mod, expr_to_est(
                mapexpr(est_to_expr(c)), first_linenode(c), sc))
        end
        last
    else
        eval(mod, st)
    end
end
include_string(mod, code, filename="string") =
    include_string(identity, mod, code, filename)

include(path::AbstractString) = include(JuliaLowering, path)
