using Base: SyntaxContext, SyntaxTree, SourceRef, @mknode, sourceref

sourcefile(src::SourceRef) = (src.file::Base.RefValue{SourceFile})[]
first_byte(src::SourceRef) = Int(src.first_byte)
last_byte(src::SourceRef) = Int(src.last_byte)
byte_range(src::SourceRef) = first_byte(src):last_byte(src)

# TODO: Adding these methods to support LineNumberNode is kind of hacky but we
# can remove these after JuliaLowering becomes self-bootstrapping for macros
# and we a proper SourceRef for @ast's @HERE form.
byte_range(::LineNumberNode) = 0:0
source_location(src::LineNumberNode) = (src.line, 0)
source_location(::Type{LineNumberNode}, src::LineNumberNode) = src
source_line(src::LineNumberNode) = src.line
# The following somewhat strange cases are for where LineNumberNode is standing in
# for SourceFile because we've only got Expr-based provenance info
sourcefile(src::LineNumberNode) = src
sourcetext(::LineNumberNode) = SubString("")
source_location(src::LineNumberNode, _byte_index::Integer) = (src.line, 0)
source_location(::Type{LineNumberNode}, src::LineNumberNode, _byte_index::Integer) = src
filename(src::LineNumberNode) = string(src.file)

sourcefile(ex::SyntaxTree) = sourcefile(sourceref(ex))
byte_range(ex::SyntaxTree) = byte_range(sourceref(ex))

function sourcetext(ex::SyntaxTree)
    sf = sourcefile(ex)
    sf isa LineNumberNode && return SubString("")
    view(sf, byte_range(ex))
end

function highlight(io::IO, src::LineNumberNode; note="")
    print(io, src, " - ", note)
end

function highlight(io::IO, src::SourceRef; kws...)
    highlight(io, sourcefile(src), first_byte(src):last_byte(src); kws...)
end

# function Base.show(io::IO, ::MIME"text/plain", src::SourceRef)
#     highlight(io, src; note="these are the bytes you're looking for 😊", context_lines_inner=20)
# end

function flags(ex::SyntaxTree)
    ex.syntax_flags
end

children(ex::SyntaxTree) = Base.children(ex)
numchildren(ex::SyntaxTree) = Base.numchildren(ex)
is_leaf(ex::SyntaxTree) = Base.is_leaf(ex)
head(ex::SyntaxTree) = Base.head(ex)

# todo: remove
SyntaxList(rest::SyntaxTree...) = SyntaxTree[rest...]

#-------------------------------------------------------------------------------
# RawGreenNode->SyntaxTree1

# We assume all literal kinds, with their literal value loaded into the tree,
# are discernible by `typeof(value)` where needed.
function _kind_to_head(k::Kind)
    if is_literal(k)
        :value
    elseif is_error(k)
        :error
    else
        # operators given a string value by parse_julia_literal would ideally
        # become :identifier here, but those aren't specified.
        Symbol(lowercase(string(k)))
    end
end
const _kh_cache = Dict{Kind, Symbol}(
    Kind(i)=>_kind_to_head(Kind(i)) for i in values(_kind_str_to_int))
kind_to_head(k::Kind) = _kh_cache[k]

const _syntactic_operator_heads = Set{Symbol}(
    kind_to_head(k) for k in KSet"&& || . ... -> = := .= op= .op=")
is_syntactic_operator(h::Symbol) = h in _syntactic_operator_heads

function lower_identifier_name(name::AbstractString, h::Symbol)
    h === :macro_name   ? (name == "." ? "@__dot__" : "@$name") :
    h === :strmacroname ? "@$(name)_str" :
    h === :cmdmacroname ? "@$(name)_cmd" :
    name
end

should_include_node(st::SyntaxTree) = !is_trivia(st) || head(st) === :error

function version_to_expr(ex::SyntaxTree)
    @assert head(ex) === :version
    nv = numeric_flags(flags(ex))
    return VersionNumber(1, nv ÷ 10, nv % 10)
end

function build_tree(::Type{SyntaxTree}, stream::ParseStream;
                    filename=nothing, first_line=1)
    cursor = RedTreeCursor(stream)
    sf = Ref(SourceFile(stream; filename, first_line))
    source = SourceRef(sf, first_byte(stream), last_byte(stream))
    cs = SyntaxList()
    context = SyntaxContext(nothing, nothing, stream.version, false)
    for c in reverse_toplevel_siblings(cursor)
        is_trivia(c) && !is_error(kind(c)) && continue
        push!(cs, SyntaxTree(sf, c, context))
    end
    # There may be multiple non-trivia toplevel nodes (e.g. parse error)
    length(cs) === 1 && return only(cs)
    id = SyntaxTree(:wrapper, reverse(cs), nothing, source, context)
    return id
end

function Base.SyntaxTree(sf::Base.RefValue{SourceFile}, cursor::RedTreeCursor, context)
    green_id = GC.@preserve sf begin
        raw_offset, txtbuf = _unsafe_wrap_substring(sf[].code)
        offset = raw_offset - sf[].byte_offset
        _insert_green(sf, txtbuf, offset, cursor, context)
    end
    gst = green_id
    out = _green_to_est(gst, 0, gst)
    @assert !isnothing(out) "SyntaxTree requires >0 nontrivia nodes"
    return out
end

function _insert_green(sf::Base.RefValue{SourceFile},
                       txtbuf::Vector{UInt8}, offset::Int,
                       cursor::RedTreeCursor, context::SyntaxContext)
    source = SourceRef(sf, first_byte(cursor), last_byte(cursor))
    k = kind(cursor)
    h = kind_to_head(k)
    syntax_flags = remove_flags(flags(cursor), NON_TERMINAL_FLAG)
    if is_error(k)
        # the parser leaves unspecified junk in here, so we can't insert the
        # green tree without making the tree impossible to validate
        text = sourcefile(source)[byte_range(source)]
        return @mknode(;head=h, source, context, children=SyntaxTree[
            @mknode(;head=:value, source, context,
                    value="$(_token_error_descriptions[k]): `$text`")])
    elseif !is_leaf(cursor)
        cs = SyntaxList()
        for c in reverse(cursor)
            push!(cs, _insert_green(sf, txtbuf, offset, c, context))
        end
        st = @mknode(;head=h, children=reverse!(cs), source, context, syntax_flags)
    else
        v = if is_identifier(k) || is_literal(k) || is_operator(k) || k === K"VERSION"
            let v = parse_julia_literal(
                txtbuf, head(cursor), byte_range(cursor) .+ offset)
                # TODO: Fixes in JuliaSyntax to avoid ever converting to Symbol
                v isa Symbol ? string(v) : v
            end
        else
            nothing
        end
        st = @mknode(;head=h, children=nothing, value=v, source, context, syntax_flags)
    end
    return st
end

"""
Convert green `st` to a SyntaxTree with Expr structure.  `parent_i` is the final
position of `convert(st)` (our return value) within `convert(parent)`.  If
`parent_i == 0`, neither it nor our `parent` are known or relevant to this
conversion.

We can't assume much about `st` since it's anything the parser produces.  Our
correctness is defined against existing text->Expr transformations.

All node rearrangements and head changes are determined before recursing on
children, unlike in `node_to_expr`.  This is because knowing our parent's head
and our position within it ahead-of-time makes conversion simpler.  By default,
for each node `st`, we
  1. let `cs` be `children(st)` minus (non-recursively) all trivia and parens
  2. rearrange `cs` based on length(cs), their/our/parent's head/flags, etc.
  3. let `ret_cs` be `map(convert, cs)`
  4. return our new node `convert(st)` with `ret_cs` as children.
However, we can stop and return an answer between any of these steps.  For
example, deleting a child is easy in (2), but new non-leaf children we insert
should be added to `ret_cs` rather than `cs` (unless the new child has
pre-transformation structure and we're OK with step 3 creating it again).
"""
function _green_to_est(parent::SyntaxTree, parent_i::Int,
                       st::SyntaxTree; kw_in_params=false)
    if !should_include_node(st)
        @assert head(parent) === :none && parent_i === 0
        return nothing
    end

    k = head(st)
    context = st.context # without macro expansion, we know this is uniform
    syntax_name(x) = x.value::String
    symleaf(s::String) =
        @mknode(;head=:identifier, value=s, source=st, context)
    core_globalref(s::String) =
        @mknode(;head=:identifier, value=s, source=st, context, mod=Core)
    valleaf(@nospecialize(v)) =
        @mknode(;head=:value, value=v, source=st, context)

    if is_leaf(st)
        return if k === :cmdmacroname || k === :strmacroname
            name = lower_identifier_name(syntax_name(st), k)
            symleaf(name)
        elseif k === :version
            valleaf(version_to_expr(st))
        elseif (v = st.value; v isa Union{Int128,UInt128,BigInt})
            # syntax TODO: likely unnecessary; this is just to match RGN->Expr,
            # which added this to match flisp parsing text->Expr.
            macname = v isa Int128 ? "@int128_str" :
                v isa UInt128 ? "@uint128_str" : "@big_str"
            mac = core_globalref(macname)
            arg = valleaf(replace(sourcetext(st), '_'=>""))
            ret_cids = SyntaxList(mac, valleaf(nothing), arg)
            @mknode(;source=st, context, head=:macrocall, children=ret_cids)
        elseif k === :identifier || k === :value
            st
        elseif st.value isa String
            # certain heads should really be identifiers.  known: &, |, :
            symleaf(syntax_name(st))
        else
            error("unknown leaf kind $(head(st))")
        end
    end

    # Non-leaf cases: each branch should either set `ret_k` and `cs` or recurse
    # manually and return a finished SyntaxTree
    ret_k::Symbol = k
    cs = preprocessed_green_children(st)
    n_cs = length(cs)

    if k === :string && n_cs > 0
        return _string_to_est(st, cs; unwrap_literal=true)
    elseif k === :cmdstring && n_cs > 0
        # (cmdstring _...) => (macrocall Core.@cmd lno joined_str)
        cmd_arg = _string_to_est(st, cs; unwrap_literal=true)
        loc_st = valleaf(source_location(LineNumberNode, st))
        return @mknode(;source=st, context, head=:macrocall,
                       children=SyntaxList(core_globalref("@cmd"), loc_st, cmd_arg))
    elseif k === :macro_name && n_cs === 1
        # "M.@x" => (. M (macro_name x)) => (. M @x)
        # "@M.x" => (macro_name (. M x)) => (. M @x)
        #           (macro_name else) => else
        if head(cs[1]) === :identifier
            return symleaf(lower_identifier_name(syntax_name(cs[1]), :macro_name))
        else
            inner_st = cs[1]
            inner_cs = preprocessed_green_children(inner_st)
            if (length(inner_cs) === 2 && head(inner_st) === :. &&
                head(inner_cs[2]) === :identifier)
                (lhs, raw_m) = _green_to_est(cs[1], 1, inner_cs[1]), inner_cs[2]
                mname_s = lower_identifier_name(syntax_name(raw_m), :macro_name)
                mname = @mknode(raw_m; children=nothing, value=mname_s)
                mname_inert = @mknode(;source=raw_m, context, head=:inert,
                                      children=SyntaxList(mname))
                return @mknode(inner_st; children=SyntaxList(lhs, mname_inert))
            else
                return _green_to_est(parent, 1, inner_st)
            end
        end
    elseif k === :?
        ret_k = :if
    elseif k === :var"op=" && n_cs === 3
        # (op= a + b) => (+= a b)
        # (.op= a + b) => (.+= a b) below
        # TODO: worst Expr, defined in terms of isoperator, fix me pls
        op_s = syntax_name(cs[2]) * '='
        lhs = _green_to_est(st, 0, cs[1])
        rhs = _green_to_est(st, 0, cs[3])
        return @mknode(;source=st, context, head=Symbol(op_s), children=SyntaxList(lhs, rhs))
    elseif k === :var".op=" && n_cs === 3
        op_s = '.' * syntax_name(cs[2]) * '='
        lhs = _green_to_est(st, 0, cs[1])
        rhs = _green_to_est(st, 0, cs[3])
        return @mknode(;source=st, context, head=Symbol(op_s), children=SyntaxList(lhs, rhs))
    elseif k === :var"op=" && n_cs === 1
        # (op= +) => +=   (the operator name itself, eg when quoted as `:(+=)`)
        return symleaf(syntax_name(cs[1]) * '=')
    elseif k === :var".op=" && n_cs === 1
        # (.op= +) => .+=
        return symleaf('.' * syntax_name(cs[1]) * '=')
    elseif k === :dotsidentifier
        # `..`/`...` used as an ordinary identifier (eg the `..` operator)
        return symleaf(repeat('.', numeric_flags(st)))
    elseif k === :macrocall && n_cs > 0
        # LineNumberNodes are not usually added to the tree as they are in Expr,
        # but this specifically inserts the macrocall child for compatibility
        loc_st = let loc = source_location(LineNumberNode, st)
            if n_cs >= 2 && head(cs[2]) === :version
                v = version_to_expr(popat!(cs, 2))
                @static if isdefined(Core, :MacroSource)
                    loc = Core.MacroSource(loc, v)
                end
            end
            valleaf(loc)
        end
        insert!(cs, 2, loc_st)
        # foo`x` parses to (macrocall foo::cmdmacroname (cmdstring ::cmdstring))
        # so we need to unwrap the cmdstring literal or else we get two macrocalls
        if n_cs >= 2 && head(cs[1]) === :cmdmacroname
            ret_cs = _map_green_to_est(st, cs)
            ret_cs[3] = ret_cs[3][3] # node leak
            return @mknode(st; children=ret_cs)
        end
        do_ex = head(cs[end]) === :do ? pop!(cs) : nothing
        _reorder_parameters!(cs, 3)
        !isnothing(do_ex) && return _make_do_expression(st, cs, do_ex)
    elseif k === :doc
        # (doc str obj) => (macrocall Core.@doc lno str obj)
        ret_k = :macrocall
        pushfirst!(cs, valleaf(source_location(LineNumberNode, st)))
        pushfirst!(cs, core_globalref("@doc"))
    elseif k === :dotcall || k === :call && n_cs > 0
        if is_infix_op_call(st) || is_postfix_op_call(st)
            cs[2], cs[1] = cs[1], cs[2]
        end
        if is_postfix_op_call(st) && head(cs[1]) == :identifier &&
            syntax_name(cs[1]) === "'"
            popfirst!(cs)
            ret_k = :var"'"
        end
        do_ex = head(cs[end]) === :do ? pop!(cs) : nothing
        _reorder_parameters!(cs, 2)
        if k === :dotcall
            if is_prefix_call(st)
                # (dotcall f args...) => (. f (tuple args...))
                ret_cs = _map_green_to_est(st, cs)
                tuple = @mknode(;source=st, context,
                                head=:tuple, children=ret_cs[2:end])
                return @mknode(;source=st, context,
                               head=:., children=SyntaxList(ret_cs[1], tuple))
            else
                # (dotcall + args...) => (call .+ args...)
                ret_k = :call
                if head(cs[1]) === :identifier
                    cs[1] = symleaf('.' * syntax_name(cs[1]))
                end
            end
        end
        !isnothing(do_ex) && return _make_do_expression(st, cs, do_ex)
    elseif k === :.
        if n_cs === 2
            # (. lhs rhs) => (. lhs (inert rhs))
            lhs = _green_to_est(st, 1, cs[1])
            rhs = _green_to_est(st, 2, cs[2])
            inert_rhs = head(rhs) in (:quote, :inert) ? rhs :
                @mknode(;source=cs[2], context,
                        head=:inert, children=SyntaxList(rhs))
            return @mknode(st; children=SyntaxList(lhs, inert_rhs))
        elseif n_cs === 1
            # (. x) => (. x) or .x
            # TODO: This is the one place where :parens change the result,
            # meaning that either Expr is doing something wrong or SyntaxNode is
            # deleting semantics.
            paren_st = filter(should_include_node, children(parent))[1]
            coalesce_dot = !(head(paren_st) === :parens) && parent_i === 1 &&
                head(parent) in (:call, :dotcall, :curly, :quote)

            if (coalesce_dot || is_syntactic_operator(head(cs[1])) ||
                head(parent) === :comparison && iseven(parent_i))
                return symleaf('.' * syntax_name(cs[1]))
            end
        end
    elseif k === :ref || k === :curly
        _reorder_parameters!(cs, 2)
    elseif k === :for && n_cs === 2
        # (for (iteration iter1) body) => (for iter1 body)
        iters = preprocessed_green_children(cs[1])
        if length(iters) === 1
            cs[1] = iters[1]
        end
    elseif k === :iteration
        # (for (iteration iter1 iters...) body) => (for (block iter1 iters...) body)
        @assert head(parent) === :for && parent_i === 1
        ret_k = :block
    elseif k === :vect || k === :braces
        _reorder_parameters!(cs, 1)
    elseif k === :tuple
        # Unwrap singleton, no-trailing-comma tuple in a couple cases:
        # (function (tuple (... xs)) body) => (function (... xs) body)
        # (-> (tuple _) body) => (-> _ body), assuming _ not parameters
        if n_cs === 1 && parent_i === 1 &&
            !has_flags(st, TRAILING_COMMA_FLAG)
            p_k = head(parent)
            c_k = head(cs[1])
            if (p_k === :function && c_k === :...) ||
                (p_k === :-> && c_k !== :parameters)
                return _green_to_est(parent, parent_i, cs[1])
            end
        elseif n_cs === 2 && head(parent) === :-> && parent_i === 1 &&
            head(cs[2]) === :parameters && head(cs[1]) !== :...
            # This case should really be deleted.
            # (-> (tuple x (parameters y)) _) => (-> (block x y) _)
            c2_cs = preprocessed_green_children(cs[2])
            if length(c2_cs) === 0
                ret_k = :block
                pop!(cs)
            elseif length(c2_cs) === 1
                ret_k = :block
                cs[2] = c2_cs[1]
            end
        end
        _reorder_parameters!(cs, 1)
    elseif k === :where && n_cs === 2
        # (where lhs (braces a b c)) => (where lhs a b c)
        if head(cs[2]) === :braces
            rhs = pop!(cs)
            append!(cs, preprocessed_green_children(rhs))
            _reorder_parameters!(cs, 2)
        end
    elseif k === :try
        # anything => (try try_block e catch_block [finally_block] [else_block])
        try_ = cs[1]
        st_false = valleaf(false)
        catch_var = catch_ = else_ = finally_ = st_false
        for c in cs[2:end]
            inner_cs = preprocessed_green_children(c)
            if head(c) === :catch
                if head(inner_cs[1]) !== :placeholder
                    catch_var = inner_cs[1]
                end
                catch_ = inner_cs[2]
            elseif head(c) === :else
                else_ = only(inner_cs)
            elseif head(c) === :finally
                finally_ = only(inner_cs)
            elseif head(c) === :error
                return @mknode(c) # give up
            else
                @assert false "Illegal subclause in `try`"
            end
        end
        empty!(cs)
        push!(cs, try_, catch_var, catch_)
        if finally_ != st_false || else_ != st_false
            push!(cs, finally_)
            if else_ != st_false
                push!(cs, else_)
            end
        end
    elseif k === :generator && n_cs >= 2
        # let (g2 x iter) mean (generator x iter.children...)
        # (generator val iter_1 ... iter_n) =>
        # (flatten (g2 (... (flatten (g2 (g2 val i_n) i_{n-1})) ...) i_1))
        g_out = _green_to_est(st, 1, popfirst!(cs))
        for c in Iterators.reverse(cs)
            gen_cs = let rest = head(c) === :iteration ?
                preprocessed_green_children(c) : SyntaxList(c)
                rest = _map_green_to_est(st, rest; undef_parent=true)
                pushfirst!(rest, g_out)
            end
            g_out = @mknode(st; children=gen_cs)
            if c !== cs[end]
                source = c === cs[begin] ? st : c
                g_out = @mknode(;source, context,
                                head=:flatten, children=SyntaxList(g_out))
            end
        end
        return g_out
    elseif k === :filter
        @assert n_cs === 2
        # (filter (iteration is...) cond) => (filter cond is...)
        cond = pop!(cs)
        cs = preprocessed_green_children(cs[1])
        pushfirst!(cs, cond)
    elseif k === :in
        ret_k = :(=)
    elseif k === :nrow || k === :ncat
        pushfirst!(cs, valleaf(numeric_flags(flags(st))))
    elseif k === :typed_ncat
        insert!(cs, 2, valleaf(numeric_flags(flags(st))))
    elseif k === :elseif
        # (elseif cond body) => (elseif (block cond) body)
        # RGN->Expr block-wraps for linenodes; we do it for parity
        ret_cs = _map_green_to_est(st, cs)
        ret_cs[1] = @mknode(;source=cs[1], context,
                            head=:block, children=SyntaxList(ret_cs[1]))
        return @mknode(st; children=ret_cs)
    elseif k === :-> && head(cs[2]) !== :block
        ret_cs = _map_green_to_est(st, cs)
        ret_cs[2] = @mknode(;source=cs[2], context,
                            head=:block, children=SyntaxList(ret_cs[2]))
        return @mknode(st; children=ret_cs)
    elseif k === :function && n_cs >= 2 &&
        has_flags(st, SHORT_FORM_FUNCTION_FLAG)
        # (function-= callex body) => (= callex (block body))
        # exception: no block on "x' = y", or if body is already a block
        if head(cs[2]) !== :block && !is_postfix_op_call(cs[1])
            ret_cs = _map_green_to_est(st, cs)
            ret_cs[2] = @mknode(;source=cs[2], context,
                                head=:block, children=SyntaxList(ret_cs[2]))
            return @mknode(;source=st, context, head=:(=), children=ret_cs)
        end
        ret_k = :(=)
    elseif k === :module
        not_bare = valleaf(!has_flags(st, BARE_MODULE_FLAG))
        insert!(cs, head(cs[1]) === :version ? 2 : 1, not_bare)
    elseif k === :quote && n_cs === 1
        # (quote something_simple) => (inert something_simple)
        ret_c = _green_to_est(st, 1, cs[1])
        return is_leaf(ret_c) && !(ret_c.value isa Bool) ?
            @mknode(;source=st, context, head=:inert, children=SyntaxList(ret_c)) :
            @mknode(st; children=SyntaxList(ret_c))
    elseif k === :do
        ret_k = :->
    elseif k === :block
        # (let (block x) _...) => (let x _...)
        # (let (block (= x y)) _...) => (let (= x y) _...)
        # (let (block (:: x y)) _...) => (let (:: x y) _...)
        # (struct _ (block (doc "foo" field1) (doc "bar" field2))) =>
        # (struct _ (block "foo" field1 "bar" field2))
        if head(parent) === :let && parent_i === 1 && n_cs === 1
            out = _green_to_est(st, 1, cs[1])
            return head(out) in (:identifier, :(=), :(::)) ? out :
                @mknode(st; children=SyntaxList(out))
        elseif head(parent) === :struct && parent_i === 3
            cs_tmp = SyntaxList()
            for c in cs
                head(c) === :doc ?
                    append!(cs_tmp, preprocessed_green_children(c)) :
                    push!(cs_tmp, c)
            end
            cs = cs_tmp
        end
    elseif (k === :local || k === :global) && n_cs === 1
        # (local (const _)) => (const (local _))
        # (local (tuple a b c)) => (local a b c)
        if head(cs[1]) === :const
            ret_c1_cs = _map_green_to_est(st, preprocessed_green_children(cs[1]))
            ret_cs = SyntaxList(@mknode(st; children=ret_c1_cs))
            return @mknode(cs[1]; children=ret_cs)
        elseif head(cs[1]) === :tuple
            cs = preprocessed_green_children(cs[1])
        end
    elseif k === :return && n_cs === 0
        push!(cs, valleaf(nothing))
    elseif k === :juxtapose
        ret_k = :call
        pushfirst!(cs, symleaf("*"))
    elseif k === :struct
        is_mutable = valleaf(has_flags(st, MUTABLE_FLAG))
        pushfirst!(cs, is_mutable)
    elseif k === :importpath
        ret_k = :.
        for i in eachindex(cs)
            if head(cs[i]) === :inert
                inner_cs = preprocessed_green_children(cs[i])
                length(inner_cs) === 1 && (cs[i] = only(inner_cs))
            end
        end
    elseif k === :wrapper # parse errors only
        ret_k = :block
    elseif k === :parameters
        kw_in_params = head(parent) === :parameters && parent_i === 1 ?
            kw_in_params : !(head(parent) in (:vect, :curly, :braces, :ref))
    elseif k === :(=)
        p_k = head(parent)
        because_params = p_k === :parameters && parent_i >= 1 && kw_in_params
        because_call = parent_i > 1 && (p_k == :ref ||
            p_k in (:call, :dotcall) && is_prefix_call(parent))
        ret_k = because_params || because_call ? :kw : :(=)
    elseif k in (:var, :char, :parens) && n_cs === 1
        # Reachable if this is the top node
        return _green_to_est(parent, parent_i, cs[1])
    end

    # Recurse on `cs`.  If no children change, just return `st`.
    ret_cs = _map_green_to_est(st, cs; kw_in_params)
    return ret_cs == children(st) && ret_k == head(st) ?
        st : @mknode(;source=st, head=ret_k, children=ret_cs, context)
end

function _map_green_to_est(parent::SyntaxTree, cs;
                           kw_in_params=false, undef_parent=false)
    ret_cs = SyntaxList()
    for (i, c) in enumerate(cs)
        new_c = _green_to_est(parent, undef_parent ? 0 : i, c; kw_in_params)
        @assert should_include_node(new_c)
        push!(ret_cs, new_c)
    end
    ret_cs
end

# When converting, first delete trivia and wrapper nodes in children so we can
# observe child kinds before recursing, thus creating fewer "temporary" nodes
function preprocessed_green_children(st::SyntaxTree)
    cs = filter(should_include_node, children(st))
    for i in eachindex(cs)
        while !is_leaf(cs[i]) && head(cs[i]) in (:var, :char, :parens)
            inner_cs = preprocessed_green_children(cs[i])
            if length(inner_cs) === 1
                cs[i] = inner_cs[1]
            else
                break
            end
        end
    end
    return cs
end

# (call f a b (parameters c d) (parameters e)) =>
# (call f (parameters (parameters e) c d) a b)
function _reorder_parameters!(cs::Vector{SyntaxTree}, params_pos::Int)
    (length(cs) > params_pos && head(cs[end]) === :parameters) || return cs
    local param_ball = pop!(cs)
    while length(cs) >= 1 && head(cs[end]) === :parameters
        next_ball_cs = pushfirst!(copy(children(cs[end])), param_ball)
        # `mknode` leaks nodes, but having multiple `parameters` blocks is
        # extremely rare nonsense syntax (`f(a,b;c=d;e)`)
        param_ball = @mknode(cs[end]; children=next_ball_cs)
        pop!(cs)
    end
    insert!(cs, params_pos, param_ball)
    nothing
end

# (call args... (do _...)) -> (do (call args...) (-> _...))
#
# Expects preprocessed and rearranged `args`
function _make_do_expression(st::SyntaxTree, args::Vector{SyntaxTree}, doex::SyntaxTree)
    ret_doex = _green_to_est(st, 0, doex)
    ret_callex = @mknode(st; children=_map_green_to_est(st, args))
    return @mknode(;source=st, context=st.context, head=:do,
                   children=SyntaxList(ret_callex, ret_doex))
end

# A `string` or `cmdstring` may have multiple literal strings within (from
# newlines when triple-quoting).  A `string` may have interpolated values.
#
# (string "a" "b" "c") => "abc" # unwrap_literal=true
# (string "a" "b" "c" 1) => (string "abc" 1)
# (string "a" "b" (string "c" "d")) => (string "ab" (string "cd"))
#
# (cmdstring "a"::cmdstring "b"::cmdstring) => "ab"
#
# Converting children-first (as _string_to_Expr does) would make this much
# harder by converting literal strings without the parent's knowledge
function _string_to_est(st::SyntaxTree, cs::Vector{SyntaxTree}; unwrap_literal)
    ret_cs = SyntaxList()
    is_literal_str(c) = c.value isa String && head(c) == :value
    cur_str = false
    next_str = length(cs) > 0 && is_literal_str(cs[1])
    buf = IOBuffer()
    for i in eachindex(cs)
        c = cs[i]
        (prev_str, cur_str) = (cur_str, next_str)
        next_str = i != lastindex(cs) && is_literal_str(cs[i+1])
        # optimization: push the current child mostly unchanged if the following
        # one isn't a literal string
        if !prev_str && cur_str && !next_str
            push!(ret_cs, c)
        elseif cur_str
            write(buf, c.value)
            if !next_str
                ret_c = @mknode(;head=:value, value=String(take!(buf)),
                                source=st, context=st.context)
                push!(ret_cs, ret_c)
            end
        else
            ret_c = !is_leaf(c) && head(c) === :string ?
                _string_to_est(c, preprocessed_green_children(c);
                               unwrap_literal=false) :
                _green_to_est(st, i, c)

            push!(ret_cs, ret_c)
        end
    end
    if unwrap_literal && length(ret_cs) === 1 && is_literal_str(ret_cs[1])
        return @mknode(ret_cs[1]; source=st)
    end
    return @mknode(st; children=ret_cs)
end
