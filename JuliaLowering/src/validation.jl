struct ValidationDiagnostic
    sts::SyntaxList
    msgs::Vector{String}
    loc::LineNumberNode # for noting where failures come from in this file
end
ValidationDiagnostic(st::SyntaxTree, msg, loc) =
    ValidationDiagnostic(
        SyntaxList(syntax_graph(st), NodeId[st._id]), String[msg], loc)

"""
The type returned by all `vst` functions.  There are three answers this can
represent:
  - valid `(true, nothing)`
  - known-invalid `(false, [errors...])`
  - don't know/not my job `(false, nothing)`.
"""
struct ValidationResult
    ok::Bool
    errors::Union{Nothing, Vector{ValidationDiagnostic}}
end

pass() = ValidationResult(true, nothing)
unknown() = ValidationResult(false, nothing)
fail(st::SyntaxTree, msg="invalid syntax", loc=nothing) =
    ValidationResult(false, [ValidationDiagnostic(
        st, msg, something(loc, LineNumberNode(0)))])
macro fail(st, msg)
    esc(:($fail($st, $msg, $(QuoteNode(__source__)))))
end

is_known(vr::ValidationResult) = vr.ok || vr.errors !== nothing

function Base.var"&"(vr1::ValidationResult, vr2::ValidationResult)
    errors_out = isnothing(vr1.errors) ? vr2.errors :
        isnothing(vr2.errors) ? vr1.errors :
        union(vr1.errors, vr2.errors)
    ValidationResult(vr1.ok & vr2.ok, errors_out)
end

"""
Note that this, unlike `&`, is only a true `|` in the case that at least one
argument is `ok` or at most one argument produces errors.  If both sides are
`!ok` with errors, use the first argument's errors.
"""
function Base.var"|"(vr1::ValidationResult, vr2::ValidationResult)
    vr1.ok && return vr1
    vr2.ok && return vr2
    ValidationResult(false, !isnothing(vr1.errors) ? vr1.errors : vr2.errors)
end

abstract type ValidationContext end

function Base.all(f::Function, vcx::ValidationContext, itr; kws...)
    ok = pass()
    for i in itr
        ok &= f(vcx, i; kws...)
    end
    return ok
end

#-------------------------------------------------------------------------------
# Post-macro-expansion (st1)

"""
This context contains recursive flags that would otherwise require keyword
arguments to all validation functions, usually to remember the kinds of
structures we're in.

By default, assume we are validating a usual lowering input (top-level) that has
been macroexpanded.
"""
Base.@kwdef struct Validation1Context <: ValidationContext
    toplevel::Bool=true     # not in any lambda body
    in_gscope::Bool=true    # not in any scope; implies toplevel
    in_loop::Bool=false     # any break/continue allowed
    in_symblock::Bool=false # labeled break allowed
    inner_cond::Bool=false  # methods not allowed in control flow in an outer
                            # function.  true in if (args 2-3), &&, || (arg 2+)
    return_ok::Bool=true    # yes usually (even outside of functions), no in
                            # comprehensions/generators
                            # syntax TODO: no return in finally? type decls?
    # assign_ok::Bool=true    # no in vect, curly, [typed_]h/v/ncat

    # fixme: flisp happens to allow reading of underscore vars if they are
    # introduced like `(function (where (call f (:: arg T)...) _) body)`, and
    # the underscore is used in T.  See #60626.
    #
    # Mod._ is also readable
    readable_underscore::Bool=false

    # vst0 shares this context type since macro expansion doesn't recurse
    # into some forms, and most parts of the AST are the same.
    unexpanded::Bool=false
end

function with(vcx::Validation1Context;
              toplevel     =vcx.toplevel,
              in_gscope    =vcx.in_gscope,
              in_loop      =vcx.in_loop,
              in_symblock  =vcx.in_symblock,
              inner_cond   =vcx.inner_cond,
              return_ok    =vcx.return_ok,
              readable_underscore=vcx.readable_underscore,
              unexpanded   =vcx.unexpanded)
    Validation1Context(
        toplevel, in_gscope, in_loop, in_symblock, inner_cond, return_ok,
        readable_underscore, unexpanded)
end

"""
Executable grammar of the input language to lowering (post-macro-expansion).

This should serve three purposes:
(1) A readable reference for the julia AST structure (for e.g. macro authors).
(2) A set of assumptions we can use in lowering (a guard against many forms of
    invalid input).  If `valid_st1(st)` returns true, lowering is expected to
    produce correct output given `st` (possibly by throwing a LoweringError).
(3) The place we throw helpful user-facing errors given malformed ASTs.

Only AST structure is checked.  Roughly, this means node kinds and child counts
given this node's parents (to a finite depth) and the current
Validation1Context.

We don't check some other things:
- This pass assumes that required attributes exist, that leaf-only (or not)
  kinds are leaves (or not).  See `assert_syntaxtree`.
- Scope issues are caught later in lowering, e.g. declaring something local and
  global.
- Checking that certain forms don't appear in value position is also handled
  later in lowering.
"""
function valid_st1(st::SyntaxTree)
    DEBUG && assert_syntaxtree(st)
    vr = vst1(Validation1Context(), st)
    @jl_assert is_known(vr) st
    return vr
end

vst1(vcx::Validation1Context, st::SyntaxTree)::ValidationResult = match st
    case K"Identifier"()
        vst1_ident(vcx, st)
    case _ if is_expr_value(st)
        pass()
    case K"block"(xs...)
        all(vst1, vcx, xs)
    case K"let"(K"block"(decls...), body)
        all(vst1_symdecl_or_assign, vcx, decls) &
            vst1(with(vcx; in_gscope=false), body)
    case K"let"(decl, body)
        vst1_symdecl_or_assign(vcx, decl) &
            vst1(with(vcx; in_gscope=false), body)
    case K"if"(_...)
        inner_vcx = vcx.toplevel ? with(vcx; inner_cond=true) : vcx
        match st
        case K"if"(K"generated"(), t, f)
            vst1(inner_vcx, t) & vst1_else(inner_vcx, f)
        case K"if"(K"generated"(), _...)
            @fail(st, "if-generated requires both true and false cases")
        case K"if"(cond, t)
            vst1(vcx, cond) & vst1(inner_vcx, t)
        case K"if"(cond, t, f)
            vst1(vcx, cond) & vst1(inner_vcx, t) & vst1_else(inner_vcx, f)
        case _
            @fail(st, "expected (if cond body) or (if cond body else)")
        end
    case K"try"(_...)
        vst1_try(vcx, st)
    case K"function"(_...)
        vst1_function(vcx, st)
    case K"question"(x)
        vst1(vcx, x)
    case K"except"(x, E)
        vst1(vcx, x) & vst1(vcx, E)
    case K"match"(_...)
        vst1_match(vcx, st)
    case K"match_assign"(pat, rhs)
        vst1(vcx, rhs) # pattern is checked in lowering
    case K"call"(_...)
        vst1_call(vcx, st)
    case K"'"(x)
        vst1(vcx, x)
    case K"."(f, K"tuple"(_...))
        vst1_dotcall(vcx, st)
    case K"."(l, r)
        vst1(vcx, l) & vst1_dot_getproperty_rhs(vcx, r)
    case K"."(x)
        vst1(vcx, x) # BroadcastFunction(x)
    case K"do"(call, lam)
        (vst1_call(vcx, call) | vst1_dotcall(vcx, call) | vst0_macrocall(vcx, call)) &
            vst1_lam(vcx, lam)
    case K"="(_...)
        vst1_assign(vcx, st)
    case _ if (vr=vst1_dotted_or_op_assign(vcx, st); is_known(vr))
        vr
    case K"return"(val)
        vcx.return_ok ?
            vst1(vcx, val) :
            @fail(st, "`return` not allowed inside comprehension or generator")
    case K"continue"() if vcx.in_loop
        pass()
    case K"continue"(lab) if vcx.in_loop
        vst1_ident(vcx, lab; lhs=true)
    # An unlabeled break is also allowed inside anonymous `@label` blocks;
    # breaking through a named block is rejected with a precise error during
    # linearization.
    case K"break"() if vcx.in_loop||vcx.in_symblock
        pass()
    case K"break"(lab) if vcx.in_loop||vcx.in_symblock
        vst1_ident(vcx, lab; lhs=true)
    case K"break"(lab, x) if vcx.in_loop||vcx.in_symblock
        vst1_ident(vcx, lab; lhs=true) & vst1(vcx, x)
    case K"for"(K"block"(is...), body)
        all(vst1_iter, vcx, is) &
            vst1(with(vcx; in_loop=true, in_gscope=false), body)
    case K"for"(iter1, body)
        vst1_iter(vcx, iter1) &
            vst1(with(vcx; in_loop=true, in_gscope=false), body)
    case K"while"(cond, body)
        vst1(vcx, cond) &
            vst1(with(vcx; in_loop=true, in_gscope=false), body)
    case K"tuple"(_...)
        vst1_tuple(vcx, st)
    case K"curly"(t, tvs...)
        vst1(vcx, t) & no_assignment(tvs, "type parameter list") &
            all(vst1_curly_typevar, vcx, tvs)
    case K"where"(t, tds...)
        vst1(vcx, t) & all(vst1_typevar_decl, vcx, tds)
    case K"string"(xs...)
        all(vst1_splat_or_val, vcx, xs)
    case K"->"(_...)
        vst1_lam(vcx, st)
    case K"flatten"(g)
        vst1_generator(vcx, g)
    case K"generator"(_...)
        vst1_generator(vcx, st)
    case K"comprehension"(K"flatten"(g))
        vst1_generator(vcx, g)
    case K"comprehension"(g)
        vst1_generator(vcx, g)
    case K"comprehension"(xs...)
        # HACK: We shouldn't be creating trees here, but this is extremely rare
        # (deprecated even in 2016)
        vst1_generator(vcx, @ast st._graph st [K"generator" xs...])
    case K"typed_comprehension"(t, K"flatten"(g))
        vst1(vcx, t) & vst1_generator(vcx, g)
    case K"typed_comprehension"(t, g)
        vst1(vcx, t) & vst1_generator(vcx, g)
    case K"comparison"(xs...)
        length(xs) < 3 || iseven(length(xs)) ?
        @fail(st, "`comparison` expects n>=3 args and odd n") :
        # TODO: can we restrict xs[2:2:end] to identifier or .identifier?
        all(vst1, vcx, xs[2:2:end]) &
        all(vst1, vcx, xs[1:2:end])
    case K"<:"(x)
        vst1(vcx, x)
    case K">:"(x)
        vst1(vcx, x)
    case K"<:"(x, y)
        vst1(vcx, x) & vst1(vcx, y)
    case K">:"(x, y)
        vst1(vcx, x) & vst1(vcx, y)
    case K"-->"(xs...)
        all(vst1, vcx, xs)
    case K"::"(x, y)
        vst1(vcx, x) & vst1(vcx, y)
    # TODO: inner_cond on args[2:end]
    case K"&&"(xs...)
        all(vst1, vcx, xs)
    case K"||"(xs...)
        all(vst1, vcx, xs)
    case K".&&"(x, y)
        vst1(vcx, x) & vst1(vcx, y)
    case K".||"(x, y)
        vst1(vcx, x) & vst1(vcx, y)
    case _ if (vr=vst1_arraylike(vcx, st); is_known(vr))
        vr
    # syntax TODO: disallow pre-desugared const, broken with complex rhs
    case K"const"(l, r)
        vst1_ident(vcx, l; lhs=true) & vst1(vcx, r)
    case K"const"(K"global"(x))
        !vcx.toplevel ?
            @fail(st, "unsupported `const` inside function") :
            vst1_const_assign(vcx, x)
    case K"const"(x)
        !vcx.toplevel ?
            @fail(st, "unsupported `const` inside function") :
            vst1_const_assign(vcx, x)
    case K"global"(xs...)
        minlen(st, xs, 1) & all(vst1_global_arg, vcx, xs)
    case K"local"(xs...)
        minlen(st, xs, 1) & all(vst1_local_arg, vcx, xs)
    case K"macrocall"(_...)
        vst0_macrocall(vcx, st)
    case K"quote"(x)
        vcx.unexpanded ? vst0_quoted(vcx, x; quote_level=1) :
            @fail(st, "interpolating quote not valid syntax after macro expansion")

    #---------------------------------------------------------------------------
    # Forms not produced by the parser
    case K"ssavalue"(K"Value"())
        pass()
    case K"static_parameter"(K"Value"())
        pass()
    case K"inert"(_)
        pass()
    case K"inert_syntaxtree"(_)
        pass()
    case K"core"(K"Identifier"())
        pass()
    case K"top"(K"Identifier"())
        pass()
    case K"meta"(_...)
        pass() # TODO
    case K"toplevel"(xs...)
        pass() # this will be validated when we lower it
    case K"opaque_closure"(argt, lb, ub, bool, lam)
        all(vst1, vcx, [argt, lb, ub, bool]) & vst1_lam(vcx, lam)
    case K"symboliclabel"(lab)
        vst1_ident(vcx, lab; lhs=true)
    case K"symbolicgoto"(lab)
        vst1_ident(vcx, lab; lhs=true)
    case K"oldsymbolicgoto"(lab)
        vst1_ident(vcx, lab; lhs=true)
    case K"symbolicblock"(lab, body)
        vst1_ident(vcx, lab; lhs=true) & vst1(with(vcx; in_symblock=true), body)
    case K"gc_preserve"(x, ids...)
        vst1(vcx, x) & all(vst1_ident, vcx, ids)
    # lowering TODO: 0 args segfaults
    case K"gc_preserve_begin"(ids...)
        all(vst1_ident, vcx, ids)
    case K"gc_preserve_end"(ids...)
        all(vst1_ident, vcx, ids)
    case K"isdefined"(K"Identifier"())
        pass()
    case K"lambda"(K"block"(b1...), K"block"(b2...), _)
        all(vst1_ident, vcx, b1) &
        all(vst1_ident, vcx, b2) &
        (kind(st[3]) === K"->" ? vst1_lam(vcx, st[3]) :
            vst1(with(vcx; return_ok=true, toplevel=false, in_gscope=false), st[3]))
    case K"softscope"(_)
        pass()
    case K"softscope"()
        pass()
    case K"generated"()
        pass()
    case K"foreigncall"(fname, rt, at, cconv, roots_args...)
        # TODO: could be stricter
        vst1(vcx, fname) &
        vst1(vcx, rt) &
        vst1(vcx, at) &
        vst1(vcx, cconv) &
        all(vst1, vcx, roots_args)
    case K"foreignglobal"(fname)
        vst1(vcx, fname) # TODO: could be stricter
    case K"cfunction"(K"Value"(), f, rt, at, K"inert"(K"Identifier"()))
        vst1(vcx, f) & vst1(vcx, rt) & vst1(vcx, at)
    case K"cconv"(tup, nreq)
        (get(tup, :value, nothing) isa Tuple &&
            get(nreq, :value, nothing) isa Int) ? pass() :
            @fail(st, "expected (cconv convention_tuple n_req_args)")
    case K"tryfinally"(t, f)
        vst1(vcx, t) & vst1(vcx, f)
    case K"tryfinally"(t, f, scope)
        vst1(vcx, t) & vst1(vcx, f) & vst1(vcx, scope)
    case K"loopinfo"(_...)
        pass() # TODO
    case K"boundscheck"()
        pass() # optional bool arg does nothing
    case K"boundscheck"(K"Value"()) if (st[1].value isa Bool)
        pass()
    case K"inbounds"(K"Value"()) if (st[1].value isa Bool)
        pass()
    case K"inbounds"(K"Identifier"()) if (st[1].name_val == "pop")
        pass()
    case K"inline"(K"Value"()) if (st[1].value isa Bool)
        pass()
    case K"noinline"(K"Value"()) if (st[1].value isa Bool)
        pass()
    case K"purity"()
        pass()
    case K"purity"(_, _...)
        numchildren(st) == fieldcount(Base.EffectsOverride) ?
            pass() : @fail(st, "wrong number of args to `purity` expression")
    case K"locals"()
        pass()
    case K"islocal"(_)
        pass()
    case K"isglobal"(_)
        pass()
    case K"copyast"(K"inert"(_))
        pass()
    case K"new"(t, args...)
        vst1(vcx, t) & all(vst1, vcx, args)
    case K"splatnew"(t, arg)
        vst1(vcx, t) & vst1(vcx, arg)
    case K"thisfunction"()
        vcx.toplevel ?
            @fail(st, "can only be used inside a function") :
            !vcx.return_ok ?
            @fail(st, "current function not defined in comprehension or generator") : pass()
    case K"unknown_head"()
        head = st.name_val
        head === "latestworld-if-toplevel" ? pass() :
            @fail(st, string("unknown expr head: ", head))
    case K"aliasscope"()
        pass()
    case K"popaliasscope"()
        pass()

    #---------------------------------------------------------------------------
    # Invalid forms for which we want to produce detailed errors
    case K"..."(_...)
        @fail(st, "unexpected `...`\nsplatting can only be done into a `call`, `tuple`, `curly`, or array-like expression")
    case K"parameters"(_...)
        @fail(st, "unexpected semicolon")
    case K"braces"(_...)
        @fail(st, "`{ }` outside of `where` is reserved for future use")
    case K"bracescat"(_...)
        @fail(st, "`{ }` outside of `where` is reserved for future use")
    case K"atomic"(_...)
        @fail(st, "unimplemented or unsupported `atomic` declaration")
    case K"::"(x)
        @fail(st, "`::` must be written `value::type` outside function argument lists")
    # internal
    case K"Symbol"()
        @fail(st, "`Symbol` kind not valid until desugaring")
    case K"Placeholder"()
        @fail(st, "`Placeholder` kind not valid until desugaring")
    case K"unknown_head"(_...)
        @fail(st, string("unknown expr head: ", st.name_val))
    case K"$"(x)
        @fail(st, raw"`$` expression outside string or quote")
    case K"continue"(_...)
        @fail(st, "`continue` outside of a `while` or `for` loop")
    case K"break"()
        @fail(st, "unlabeled `break` outside of a `while` or `for` loop")
    case K"break"(_...)
        @fail(st, "labeled `break` outside of loop or symbolic block")
    case _
        let top_vr = vst1_toplevel_only(vcx, st)
            if vcx.toplevel
                top_vr
            else
                !top_vr.ok ? unknown() :
                    @fail(st, "this syntax is only allowed at top level")
            end
        end | @fail(st, string(
            "invalid syntax: unknown form `", kind(st),
            "` or number of arguments ", numchildren(st)))
end

vst1_toplevel_only(vcx, st) = match st
    # body will be validated when lowered
    case K"module"(K"Value"(), K"Value"(), K"Identifier"(), K"block"(xs...))
        !(st[1].value isa VersionNumber) ? @fail(st[1], "expected version") :
        !(st[2].value isa Bool) ? @fail(st[2], "expected boolean bare flag") :
        pass()
    case K"module"(K"Value"(), K"Identifier"(), K"block"(xs...))
        !(st[1].value isa Bool) ? @fail(st[1], "expected boolean bare flag") :
        pass()
    case K"macro"(_...)
        vst1_macro(vcx, st)
    case K"struct"(K"Value"(), sig, K"block"(body...))
        vst1_typesig(vcx, sig) & (
            !(st[1].value isa Bool) ? @fail(st[1], "expected mutable flag") :
                _struct_noassign(vcx, body) & all(vst1_struct_arg, vcx, body))
    case K"abstract"(sig)
        vst1_typesig(vcx, sig)
    case K"primitive"(sig, n)
        vst1_typesig(vcx, sig) & vst1(vcx, n)
    case K"import"(K":"(p1, ps...))
        (vst1_importpath(vcx, p1; dots_ok=true) &
        all(vst1_importpath, vcx, ps; dots_ok=false))
    case K"using"(K":"(p1, ps...))
        (vst1_importpath(vcx, p1; dots_ok=true) &
        all(vst1_importpath, vcx, ps; dots_ok=false))
    case K"import"(ps...)
        minlen(st, ps, 1) & all(vst1_importpath, vcx, ps; dots_ok=true)
    case K"using"(ps...)
        minlen(st, ps, 1) & all(vst1_importpath, vcx, ps; dots_ok=true)
    case K"public"(xs...)
        all(vst1_ident, vcx, xs)
    case K"export"(xs...)
        all(vst1_ident, vcx, xs)
    case K"latestworld"()
        pass()
    case K"typegroup"(K"block"(xs...))
        all(vst1, vcx, xs)
    case _
        unknown()
end

#-------------------------------------------------------------------------------

vst1_local_arg(vcx, st) = match st
    case K"function"(_...)
        vst1_function(vcx, st)
    case _
        vst1_symdecl_or_assign(vcx, st) | vst1_dotted_or_op_assign(vcx, st) |
            @fail(st, "invalid local declaration: expected identifier or assignment")
end

vst1_global_arg(vcx, st) = match st
    case K"function"(_...)
        vcx.toplevel ?
            vst1_function(vcx, st) :
            @fail(st, "global function needs to be placed at top level, or use eval")
    case _
        vst1_symdecl_or_assign(vcx, st) | vst1_dotted_or_op_assign(vcx, st) |
            @fail(st, "invalid global declaration: expected identifier or assignment")
end

# `match` patterns don't work so well with n dots and m identifiers
# one of:
# (as (importpath . . . x y z) ident)
#     (importpath . . . x y z)
# where y, z may be quoted (syntax TODO: require var"" for odd identifiers?)
function vst1_importpath(vcx, st; dots_ok)
    ok = pass()
    path_components = match st
    case K"as"(K"."(xs...), K"Identifier"())
        xs
    case K"as"(K"."(xs...), x)
        ok &= @fail(x, "expected identifier")
        xs
    case K"."(xs...)
        xs
    case _
        return @fail(st, "malformed import path")
    end
    seen_first = false
    for c in path_components
        if kind(c) === K"Identifier" && c.name_val::String === "."
            if !dots_ok || seen_first
                ok &= @fail(c, "unexpected `.` in import path")
            end
            continue
        end
        ok = ok & vst1_ident(vcx, c)
        seen_first = true
    end
    return !seen_first ? @fail(st, "expected identifier in `importpath`") : ok
end

vst1_tuple(vcx, st) = match st
    case K"tuple"(K"parameters"(kws...))
        all(vst1_call_kwarg, vcx, kws)
    case K"tuple"(K"parameters"(_, _...), _, _...)
        @fail(
            st[1], "cannot mix tuple `(a,b,c)` and named tuple `(;a,b,c)` syntax")
    case K"tuple"(args...) if any(x->kind(x)===K"=", args)
        all(vst1_call_arg, vcx, args)
    case K"tuple"(xs...)
        all(vst1_splat_or_val, vcx, xs)
    case _
        @fail(st, "malformed tuple")
end

vst1_else(vcx, st) = match st
    case K"elseif"(cond, t)
        vst1(vcx, cond) &
            vst1(vcx, t)
    case K"elseif"(cond, t, f)
        vst1(vcx, cond) &
            vst1(vcx, t) &
            vst1_else(vcx, f)
    case _
        vst1(vcx, st)
end

# TODO: disallow (has-unmatched-symbolic-goto? tryb)
vst1_try(vcx, st) = match st
    case K"try"(_)
        @fail(st, "try without catch or finally")
    case K"try"(tryb, cvar, catchb)
        vst1(vcx, tryb) &
            vst1_try_catchvar(vcx, cvar) &
            vst1(vcx, catchb)
    case K"try"(tryb, cvar, catchb, finallyb)
        vst1(vcx, tryb) &
            vst1_try_catchvar(vcx, cvar) &
            vst1(vcx, catchb) &
            vst1(vcx, finallyb)
    case K"try"(tryb, cvar, catchb, finallyb, elseb)
        vst1(vcx, tryb) &
            vst1_try_catchvar(vcx, cvar) &
            vst1(vcx, catchb) &
            vst1(vcx, finallyb) &
            vst1(vcx, elseb)
    case _
        @fail(st, "malformed `try` expression")
end

vst1_try_catchvar(_vcx, st) = match st
    case K"Identifier"()
        pass()
    case K"Value"() if st.value===false
        pass()
end

# syntax TODO:
# - const is inoperative in the function case
# - single-arg const with no value (presumably to poison this name) was likely
#   not intended to work, and can only be produced by macros
vst1_const_assign(vcx, st) = match st
    case K"="(_, _)
        vst1_assign(vcx, st; in_const=true)
    case K"Identifier"()
        pass()
    case K"local"(_...)
        @fail(st, "unsupported `const local` declaration")
    case _
        @fail(st, "expected assignment after `const`")
end

# syntax TODO: all-underscore variables may be read from with dot syntax
vst1_dot_getproperty_rhs(vcx, st) = match st
    case K"inert"(x)
        pass()
    case K"inert_syntaxtree"(x)
        pass()
    case K"Identifier"()
        pass()
    case _ if is_expr_value(st)
        pass()
    case _
        @fail(st, "invalid `.` syntax")
end

# We can't validate A.B in general (usually lowers to getproperty), but it shows
# up in a number of syntax special cases where we can. (flisp: sym-ref?)
vst1_calldecl_dot_name(vcx, st) = match st
    case K"."(l, r)
        vst1_calldecl_dot_name(vcx, l) &
            vst1_calldecl_dot_name_rhs(vcx, r) |
            @fail(st, "invalid `.` form")
    case K"Value"()
        pass()
    case _
        vst1_ident(vcx, st)
end

vst1_calldecl_dot_name_rhs(vcx, st) = match st
    case K"inert"(x)
        vst1_calldecl_dot_name_rhs(vcx, x)
    case K"inert_syntaxtree"(x)
        vst1_calldecl_dot_name_rhs(vcx, x)
    case K"Identifier"()
        vst1_ident(vcx, st; lhs=true)
    case K"Value"() if st.value isa String
        _ident_str(vcx, st, st.value; lhs=true)
    case K"String"()
        _ident_str(vcx, st, st.value; lhs=true)
    case K"tuple"(_...)
        @fail(st, "dotcall syntax not valid here")
    case _
        @fail(st, "invalid `.` syntax")
end

vst1_symdecl_or_assign(vcx, st) =
    @fail(st, "expected identifier or assignment") |
    vst1_symdecl(vcx, st) | vst1_assign(vcx, st)

vst1_symdecl(vcx, st) = match st
    case K"Identifier"()
        pass()
    case K"::"(K"Identifier"(), t)
        vst1(vcx, t)
    case _
        @fail(st, "expected identifier or `identifier::type`")
end

# TODO: globalref (identifier with .mod) might not be valid everywhere; check
# usage of this function
vst1_ident(vcx, st; lhs=false) = match st
    case K"Identifier"()
        _ident_str(vcx, st, st.name_val; lhs)
    case _
        @fail(st, "expected identifier")
end
function _ident_str(vcx, st, s::String; lhs=false)
    if !lhs && !vcx.readable_underscore && is_writeonly_est_name(s)
        @fail(st, "all-underscore identifiers are write-only and their values cannot be used in expressions")
    elseif lhs && s in ("ccall", "cglobal")
        @fail(st, string(s, " is a reserved identifier"))
    else
        pass()
    end
end

"N.B. this shouldn't be used after `est_to_dst`, as JuliaLowering uses the
Placeholder kind when we have write-only identifiers"
function is_writeonly_est_name(s::String)
    (all(==('_'), s) || s == UNUSED) && length(s) > 0
end

vst1_call(vcx, st) = match st
    case K"call"(K"Identifier"(), args...) if st[1].name_val==="cglobal"
        (1 <= length(args) <= 2 ? pass() :
            @fail(st, "cglobal must have one or two arguments")) &
        all(vst1_call_arg, vcx, args)
    case K"call"(f, K"parameters"(kwargs...), args...)
        (vst1_ident(vcx, f) | vst1(vcx, f)) &
        all(vst1_call_arg, vcx, args) &
        all(vst1_call_kwarg, vcx, kwargs)
    case K"call"(f, args...)
        (vst1_ident(vcx, f) | vst1(vcx, f)) &
        all(vst1_call_arg, vcx, args)
    case K"call"(_...)
        @fail(st, "malformed `call`")
    case _
        unknown()
end

vst1_dotcall(vcx, st) = match st
    case K"."(f, K"tuple"(K"parameters"(kwargs...), args...))
        vst1(vcx, f) & all(vst1_call_kwarg, vcx, kwargs) &
        all(vst1_call_arg, vcx, args)
    case K"."(f, K"tuple"(args...))
        vst1(vcx, f) & all(vst1_call_arg, vcx, args)
    case _
        unknown()
end

# Arg to call (not function decl), pre-semicolon.  This can be anything, but
# additionally allow `kw` and `...` forms.
vst1_call_arg(vcx, st) = match st
    case K"kw"(id, val)
        vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    case _
        vst1_splat_or_val(vcx, st)
end

# Arg to `parameters` (post-semicolon) in a call (not function decl).  Stricter
# than `vst1_call_arg`.  `=` desugars to `kw`.
vst1_call_kwarg(vcx, st) = match st
    case K"Identifier"()
        pass()
    case K"kw"(id, val)
        vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    case K"="(id, val)
        vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    case K"..."(x)
        vst1(vcx, x)
    case K"."(x, K"inert"(id))
        vst1(vcx, x) & vst1_ident(vcx, id; lhs=true)
    case K"."(x, K"inert_syntaxtree"(id))
        vst1(vcx, x) & vst1_ident(vcx, id; lhs=true)
    case K"call"(K"Identifier"(), symval, v) if (st[1].name_val==="=>")
        vst1(vcx, symval) & vst1(vcx, v)
    case _
        @fail(st, "expected identifier, `=`, or `...` after semicolon")
end

vst1_lam(vcx, st) = let
    f_vcx = with(vcx; return_ok=true, toplevel=false, in_gscope=false)
    match st
    case K"->"(l, r)
        vst1_lam_lhs(with(f_vcx; return_ok=false), l) & vst1(f_vcx, r)
    case _
        @fail(st, "expected `->` expression")
    end
end

vst1_lam_lhs(vcx, st) = match st
    case K"tuple"(K"parameters"(_...), ps...)
        _calldecl_positionals(vcx, ps, true) & vst1_calldecl_kws(vcx, st[1])
    case K"tuple"(ps...)
        _calldecl_positionals(vcx, ps, true)
    case K"where"(ps, tds...)
        vst1_lam_lhs(vcx, ps) & all(vst1_typevar_decl, vcx, tds)
    # syntax TODO: This is handled badly in the parser
    case K"block"()
        pass()
    case K"block"(x)
        _calldecl_positionals(vcx, SyntaxList(x), true)
    case K"block"(x, p)
        _calldecl_positionals(vcx, SyntaxList(x), true) &
            match p
            case K"="(kw, v)
                vst1_param(vcx, kw) & vst1(vcx, v)
            case K"..."(kw)
                vst1_param_varkw(vcx, kw)
            case _
                vst1_param(vcx, p)
            end
    case K"block"(_, _, _, _...)
        @fail(st, "more than one semicolon in signature")
    # unwrapped single arg
    case _
        ps = SyntaxList(st)
        _calldecl_positionals(vcx, ps, true)
end

# `match` statements: validate the scrutinee, guards and arm bodies as
# expressions; patterns are checked during lowering
function vst1_match(vcx, st)
    numchildren(st) >= 1 || return @fail(st, "malformed `match`")
    scrut = st[1]
    r = if kind(scrut) == K"as" && numchildren(scrut) == 2
        vst1(vcx, scrut[1])
    else
        vst1(vcx, scrut)
    end
    # a match participates in the default break scope
    arm_vcx = with(vcx; in_symblock=true)
    for a in children(st)[2:end]
        if !(kind(a) in KSet"case case_except" && numchildren(a) == 2)
            return @fail(a, "malformed `case` arm in `match`")
        end
        pg = a[1]
        if kind(pg) == K"guard" && numchildren(pg) == 2
            r = r & vst1(arm_vcx, pg[2])
        end
        r = r & vst1(arm_vcx, a[2])
    end
    return r
end

vst1_function(vcx, st) = let
    f_vcx = with(vcx; return_ok=true, toplevel=false, in_gscope=false)
    # lowering TODO: conditional nested function definitions are known to be
    # broken, but are not disallowed, and can be found in stdlibs.
    # vcx.inner_cond && @fail(st, "conditional inner method definitions\
    #     are not supported; use `()->()` syntax instead")
    match st
    case K"function"(name)
        vst1_ident(vcx, name)
    case K"function"(callex, body)
        vst1_function_calldecl(with(f_vcx; return_ok=false), callex) &
            vst1(f_vcx, body)
    case K"="(callex, body)
        vst1_function_calldecl(with(f_vcx; return_ok=false), callex) &
            vst1(f_vcx, body)
    case _
        @fail(st, "malformed `function`")
    end
end

# Note that we consistently refer to children of a declaring call as
# "parameters" rather than arguments (and children of a K"parameters" block as
# "keyword args/params") so we don't mix them up with children to a real call,
# whose valid forms are subtly different.

vst1_function_calldecl(vcx, st) = match st
    case K"except"(callex, E)
        vst1_function_calldecl(vcx, callex) &
            vst1(with(vcx, readable_underscore=true), E)
    case K"where"(callex, tds...)
        vst1_function_calldecl(vcx, callex) & all(vst1_typevar_decl, vcx, tds)
    case K"::"(callex, rt)
        vst1_simple_calldecl(vcx, callex) &
            vst1(with(vcx, readable_underscore=true), rt)
    case _
        vst1_simple_calldecl(vcx, st)
end

vst1_simple_calldecl(vcx, st) = match st
    case K"call"(f, K"parameters"(_...), ps...)
        vst1_calldecl_name(vcx, f) &
            _calldecl_positionals(vcx, ps, false) &
            vst1_calldecl_kws(vcx, st[2])
    case K"call"(f, ps...)
        vst1_calldecl_name(vcx, f) &
            _calldecl_positionals(vcx, ps, false)
    # anonymous function syntax `function (x); end` or `function (x...); end` is
    # subject to bad-arglist rules (block, etc.)
    case _
        vst1_lam_lhs(vcx, st) | @fail(st, "malformed `call` in function decl")
end

vst1_macro(vcx, st) = match st
    case K"macro"(m)
        vst1_ident(vcx, m; lhs=true) | vst1_ident(vcx, m; lhs=false)
    case K"macro"(K"call"(_, K"parameters"(_...), _...), _...)
        @fail(st[1][end], "macros cannot accept keyword arguments")
    case K"macro"(K"call"(m, ps...), body)
        let vcx = with(vcx; return_ok=false, toplevel=false, in_gscope=false)
            vst1_macro_calldecl_name(vcx, m) &
                _calldecl_positionals(vcx, ps, false) &
                vst1(with(vcx; return_ok=true), body)
        end
    case K"macro"(K"where"(_...), _...)
        @fail(st[1], "`where` not allowed in macro signatures")
    case K"macro"(_...)
        @fail(st, "malformed `macro`")
    case _
        unknown()
end

# Macros may have either underscore or reserved (ccall, cglobal) names
vst1_macro_calldecl_name(vcx, st) = match st
    case K"."(_, _)
        vst1_calldecl_dot_name(vcx, st)
    case _
        @fail(st, "invalid macro name") |
            vst1_ident(vcx, st; lhs=true) | vst1_ident(vcx, st; lhs=false)
end

vst1_calldecl_name(vcx, st) = match (st=strip_arg_meta(st))
    case K"Identifier"()
        vst1_ident(vcx, st; lhs=true) &
            (!is_dotted_operator(st.name_val::String) ? pass() :
            @fail(st, "dotted operator is not a valid function name"))
    case K"."(_, _)
        vst1_calldecl_dot_name(vcx, st)
    case K"curly"(t, tvs...)
        vst1_calldecl_name(vcx, t) & all(vst1, vcx, tvs)
    case K"Value"()
        pass() # GlobalRef works. Function? Type?
    # callable type
    case K"::"(t)
        vst1(vcx, t)
    case K"::"(x, t)
        vst1_pparam_simple_tuple(vcx, x) & vst1(vcx, t)
    # TODO: @overlay broken in many cases, should be stricter
    case K"overlay"(mt, x)
        vst1(vcx, mt) & vst1_calldecl_name(vcx, x)

    case K"where"(t, tds...)
        vst1_calldecl_name(vcx, t) & all(vst1_typevar_decl, vcx, tds)
    case _
        @fail(st, "invalid function name")
end

strip_arg_meta(st) = match st
    case K"meta"(s, arg)
        meta_s = get(s, :name_val, "")::String
        kind(arg) === K"meta" ? st :
            !(meta_s in ("specialize", "nospecialize")) ? st : arg
    case _
        st
end

# Check mandatory and optional positional params:
# `[pparam* pparam_and_default* pparam_and_splatdefault? pparam_va?]`
# TODO: add list matching to `match`
function _calldecl_positionals(vcx, params_meta, eq_is_kw)
    isempty(params_meta) && return pass()
    ok = Ref(pass())
    params = map(strip_arg_meta, params_meta)
    va_ok = vst1_pparam_va(vcx, params[end]; eq_is_kw)
    if is_known(va_ok)
        params = params[1:end-1]
        ok[] &= va_ok
    end
    require_assign = false
    for (i, p) in enumerate(params)
        if kind(p) === K"kw" || kind(p) === K"=" && eq_is_kw
            require_assign = true
            allow_val_splat = i == lastindex(params)
            ok[] &= vst1_pparam_and_default(vcx, p; eq_is_kw, allow_val_splat)
        elseif kind(p) === K"..."
            ok[] &= @fail(p, "`...` may only be used on the final parameter")
        elseif require_assign # TODO: multi-syntaxtree error
            ok[] &= @fail(p, "all function parameters after an optional parameter must also be optional")
        else
            ok[] &= vst1_pparam_typed_tuple(vcx, p)
        end
    end
    return ok[]
end

# TODO: flisp optional-positional-defs counts a wrapped `...` as an optional
# arg, not a vararg, but it appears to work the same
vst1_pparam_va(vcx, st; eq_is_kw) = match st
    case K"kw"(K"..."(va), val)
        vst1_pparam_typed_tuple(vcx, va) & vst1_splat_or_val(vcx, val)
    case K"="(K"..."(va), val) if eq_is_kw
        vst1_pparam_typed_tuple(vcx, va) & vst1_splat_or_val(vcx, val)
    case K"..."(va)
        vst1_pparam_typed_tuple(vcx, va)
    case _
        unknown()
end

# destructuring args: function f(a, (x, y)) ...  TODO: the strip_arg_meta call
# here corresponds to no-op nospecialize, and should ideally be removed.
vst1_pparam_typed_tuple(vcx, st) = match (st=strip_arg_meta(st))
    case K"::"(K"tuple"(_...), t)
        vst1_pparam_simple_tuple(vcx, st[1]) &
            vst1(with(vcx; readable_underscore=true), t)
    case K"tuple"(_...)
        vst1_pparam_simple_tuple(vcx, st)
    case _
        vst1_param(vcx, st)
end
vst1_pparam_simple_tuple_or_splat(vcx, st) = match st
    case K"..."(t)
        vst1_pparam_simple_tuple(vcx, t)
    case _
        vst1_pparam_simple_tuple(vcx, st)
end
# Similar to an assignment to a tuple LHS, but does not allow `::`.  Also should
# not allow ref, curly, or call, but flisp does, so we may need to change this.
vst1_pparam_simple_tuple(vcx, st) = match st
    case K"Identifier"()
        pass()
    case K"tuple"(K"parameters"(_, _...), _, _...)
        @fail(
            st[1], "cannot mix tuple `(a,b,c)` and named tuple `(;a,b,c)` syntax")
    case K"tuple"(K"parameters"(kws...))
        all(vst1_ident, vcx, kws; lhs=true)
    case K"tuple"(xs...)
        all(vst1_pparam_simple_tuple_or_splat, vcx, xs) &
            (count(kind(x)===K"..." for x in xs) <= 1 ? pass() :
            @fail(st, "multiple `...` in destructured parameter is ambiguous"))
    case K"::"(_...)
        @fail(st, "cannot have type in destructured argument")
    case _
        @fail(st, "expected identifier or tuple")
end

vst1_param(vcx, st) = match st
    case K"Identifier"()
        vst1_ident(vcx, st; lhs=true)
    case K"::"(id, t)
        vst1_ident(vcx, id; lhs=true) &
            vst1(with(vcx; readable_underscore=true), t)
    case K"::"(t)
        vst1(with(vcx; readable_underscore=true), t)
    case _
        @fail(st, "expected identifier or `identifier::type`")
end

# allow_val_splat=true when this is the final optional param (even if there are
# varargs after it).  See #50563
vst1_pparam_and_default(vcx, st; eq_is_kw, allow_val_splat) = match st
    case K"kw"(id, val)
        vst1_pparam_typed_tuple(vcx, id) &
            match val
            case K"..."(v)
                allow_val_splat ? vst1(vcx, v) :
                    @fail(val, "splat only allowed on final positional default arg")
            case _
                vst1(with(vcx; return_ok=true), val)
            end
    case K"="(id, val) if eq_is_kw
        vst1_pparam_typed_tuple(vcx, id) &
            match val
            case K"..."(v)
                allow_val_splat ? vst1(vcx, v) :
                    @fail(val, "splat only allowed on final positional default arg")
            case _
                vst1(with(vcx; return_ok=true), val)
            end
    case _
        @fail(st, "malformed optional positional parameter; expected `=`")
end

vst1_calldecl_kws(vcx, st) = match st
    case K"parameters"(kws..., last) if (varkw = strip_arg_meta(last);
                                         kind(varkw) === K"..." && numchildren(varkw) == 1)
        all(vst1_param_kw, vcx, kws) & vst1_param_varkw(vcx, varkw[1])
    case K"parameters"(kws...)
        all(vst1_param_kw, vcx, kws)
    case _
        @fail(st, "malformed keyword parameters")
end

vst1_param_varkw(vcx, st) = match st
    case K"Identifier"()
        vst1_ident(vcx, st; lhs=true)
    case K"::"(_...)
        @fail(st, "keyword parameter with `...` may not be given a type")
    case _
        @fail(st, "expected identifier")
end

# note no return_ok in default val, unlike positional defaults, due to bugs
vst1_param_kw(vcx, st) = match (st=strip_arg_meta(st))
    case K"kw"(id, val)
        vst1_param(vcx, id) & vst1(vcx, val)
    case K"..."(_...)
        @fail(st, "`...` may only be used for the final keyword parameter")
    case _
        vst1_param(vcx, st) |
            @fail(st, "malformed keyword parameter; expected identifier, `=`, or `::`")
end

vst1_typevar_decl(vcx, st) = match st
    case K"Identifier"()
        vst1_ident(vcx, st; lhs=true)
    case K"<:"(t, old)
        vst1_ident(vcx, t; lhs=true) & vst1(vcx, old)
    case K">:"(t, old)
        vst1_ident(vcx, t; lhs=true) & vst1(vcx, old)
    case K"comparison"(val_l, K"Identifier"(), t, K"Identifier"(), val_r) if (
            st[2].name_val===st[4].name_val && st[2].name_val in ("<:", ">:"))
        vst1(vcx, val_l) &
            vst1_ident(vcx, t; lhs=true) &
            vst1(vcx, val_r)
    case K"<:"(x, _)
        @fail(x, "expected type name")
    case K">:"(x, _)
        @fail(x, "expected type name")
    case K"comparison"(_...)
        @fail(st, "expected `lb <: type_name <: ub` or `ub >: type_name >: lb`")
    case _
        @fail(st, "expected type name or type bounds")
end

vst1_typesig(vcx, st) = match st
    case K"Identifier"()
        vst1_ident(vcx, st)
    case K"curly"(t, tvs...)
        vst1_ident(vcx, t) & all(vst1_typevar_decl, vcx, tvs)
    case K"<:"(K"curly"(t, tvs...), super)
        vst1_ident(vcx, t) & vst1(vcx, super) &
            all(vst1_typevar_decl, vcx, tvs)
    case K"<:"(t, super)
        vst1_ident(vcx, t) & vst1(vcx, super)
    case _
        @fail(st, "invalid type signature")
end

# normal, non-lhs curly may have implicit `(<: t)`
vst1_curly_typevar(vcx, st) = match st
    case K"<:"(t)
        vst1_splat_or_val(vcx, t)
    case K">:"(t)
        vst1_splat_or_val(vcx, t)
    case _
        vst1_splat_or_val(vcx, st)
end

# assignment should never be allowed, but flisp fails to check inside blocks or
# after anything that isn't a field.  See #62075.
function _struct_noassign(vcx, body::SyntaxList)
    for st in body
        if kind(st) === K"=" && vst1_struct_field(vcx, st[1]).ok
            return @fail(st, "assignment syntax in structure fields is reserved")
        elseif !vst1_struct_field(vcx, st).ok
            return pass()
        end
    end
    return pass()
end

vst1_struct_arg(vcx, st) = match st
    case K"block"(xs...)
        all(vst1_struct_arg, vcx, xs)
    case _
        vst1_struct_field(vcx, st) | vst1(vcx, st)
end

vst1_struct_field(vcx, st) = match st
    case K"Identifier"()
        pass()
    case K"::"(x, t)
        vst1_struct_field(vcx, x) & vst1(vcx, t)
    case K"const"(x)
        vst1_struct_field(vcx, x)
    case K"atomic"(x)
        vst1_struct_field(vcx, x)
    case _
        unknown()
end

# Messy: expr uses a different head for every op `(a op= b)` and `(a .op= b)`.
# RawGreenNode uses K"op=" and K".op=" with an extra argument specifying `op`.
# The tree we're matching stays one-to-one with Expr by using `K"unknown_head"`.
#
# Note simple `op` and `.op` are calls to (dotted) identifiers, so this special
# handling isn't necessary.
vst1_dotted_or_op_assign(vcx, st) = let op_s = get(st, :name_val, "")::String
    match st
    case K".="(l, r)
        vst1_dotassign_lhs(vcx, l) & vst1(vcx, r)
    case _ if (!Base.isoperator(op_s))
        unknown()
    case _ if (isempty(op_s) || op_s[end] !== '=')
        unknown()
    case K"unknown_head"(l, r) if (op_s[1] === '.')
        vst1_dotassign_lhs(vcx, l) & vst1(vcx, r)
    case K"unknown_head"(l, r)
        vst1_assign_lhs(vcx, l) & vst1(vcx, r)
    case _
        unknown()
    end
end

vst1_assign(vcx, st; in_const = false) = match st
    # This case handles a proper function declaration (= (call ...) ...) form.
    # `vst1_assign_lhs_nontuple` also accepts call forms, but that is a lowering
    # bug where the "function body" is evaluated immediately
    case K"="(l, r) if is_eventually_call(l)
        vst1_function(vcx, st)
    case K"="(l, r)
        vst1_assign_lhs(vcx, l; in_const) & vst1(vcx, r)
    case K"="(_...)
        @fail(st, "malformed assignment")
    case _
        unknown()
end

# TODO: We could do some destructuring checks here (e.g. fail `(a,b,c) = (1,2)`)
#
# syntax TODO:
# - call (only within a tuple using JuliaSyntax) can declare a function with
#   arguments, but can't use them on the rhs if in a tuple
# - in curly, typevars are checked for structure, but not used.
# - (local/global (= lhs rhs)) forms should probably reject the same
#   lhss as const (ref and .)
vst1_assign_lhs(vcx, st; in_const=false, in_tuple=false) = match st
    case K"tuple"(K"parameters"(xs...))
        all(vst1_symdecl, vcx, xs)
    case K"tuple"(xs...)
        all(vst1_assign_lhs, vcx, xs; in_const, in_tuple=true) &
            (count(kind(x)===K"..." for x in xs) <= 1 ? pass() :
            @fail(st, "multiple `...` in destructuring assignment are ambiguous"))
    # type-annotated tuple segfaults, haha
    # case K"::"(K"tuple"(_...), t) -> ???
    case K"..."(x)
        !in_tuple ?
            @fail(st, "splat on left side of assignment must be in a tuple") :
            vst1_assign_lhs_nontuple(vcx, x; in_const)
    case K"parameters"(_...) if in_tuple
        @fail(st, """
            property destructuring must use a single `;` before the property \
            names, e.g. `(; a, b) = rhs`""")
    case _
        vst1_assign_lhs_nontuple(vcx, st; in_const)
end
vst1_assign_lhs_nontuple(vcx, st; in_const=false, in_tuple=false) = match st
    case K"ssavalue"(K"Value"())
        in_const ? @fail(st, "cannot declare ssavalue const") : pass()
    case _ if (is_eventually_call(st))
        vst1_function_calldecl(vcx, st)
    case K"::"(x, t)
        vst1_assign_lhs(vcx, x; in_const, in_tuple) & vst1(vcx, t)
    case K"."(x, y)
        in_const ? @fail(st, "cannot declare this form constant") :
        kind(y) === K"tuple" ? @fail(st, "dotcall syntax not valid here") :
        vst1(vcx, x) & vst1(vcx, y)
    case K"ref"(x, is...)
        in_const ? @fail(st, "cannot declare this form constant") :
        vst1(vcx, x) & all(vst1_splat_or_val, vcx, is)
    case K"curly"(x, tvs...)
        vst1_ident(vcx, x; lhs=true) & all(vst1_typevar_decl, vcx, tvs)

    case K"typed_hcat"(_...)
        @fail(st, "invalid spacing in left side of indexed assignment")
    case K"typed_vcat"(_...)
        @fail(st, "unexpected `;` in left side of indexed assignment")
    case K"typed_ncat"(_...)
        @fail(st, "unexpected `;` in left side of indexed assignment")
    case _ if (kind(st) in KSet"vect hcat vcat ncat")
        @fail(st, "use `(a, b) = ...` to assign multiple values")
    case _
        @fail(st, "invalid syntax in left-hand side of assignment") |
            vst1_ident(vcx, st; lhs=true)
end

vst1_dotassign_lhs(vcx, st) = vst1_assign_lhs(vcx, st) | vst1(vcx, st)

# TODO: more validation is possible here, e.g. when row/nrow can show up in ncat
vst1_arraylike(vcx, st) = match st
    case K"vect"(xs...)
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case K"hcat"(xs...)
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case K"vcat"(xs...)
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case K"ncat"(K"Value"(), xs...)
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case K"ref"(x, is...)
        vst1(vcx, x) &
            no_assignment(is, "[ ... ]") & all(vst1_splat_or_val, vcx, is)
    case K"row"(xs...)
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case K"nrow"(K"Value"(), xs...)
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case K"typed_hcat"(t, xs...)
        vst1(vcx, t) &
            no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case K"typed_vcat"(t, xs...)
        vst1(vcx, t) &
            no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case K"typed_ncat"(t, xs...)
        vst1(vcx, t) &
            no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    case _
        unknown()
end

function no_assignment(sl, hint="this expression")
    for st in sl
        if kind(st) === K"="
            return @fail(st, string(
                "assignment is not allowed in ", hint))
        end
    end
    return pass()
end

# If there is both a min and a max, prefer a finite number of match cases
function minlen(err_st::SyntaxTree, sl::SyntaxList, n::Int)
    length(sl) >= n ? pass() :
        @fail(err_st, string(
            "expected at least ", n, " argument", (n === 1 ? "" : "s")))
end
function maxlen(err_st::SyntaxTree, sl::SyntaxList, n::Int)
    length(sl) <= n ? pass() :
        @fail(err_st, string(
            "expected at most ", n, " argument", (n === 1 ? "" : "s")))
end

vst1_splat_or_val(vcx, st) = match st
    case K"..."(x)
        vst1_splat_or_val(vcx, x)
    case K"..."(_...)
        @fail(st, "expected one argument to `...`")
    case _
        vst1(vcx, st)
end

vst1_generator(vcx, st) = let
    vcx = with(vcx; return_ok=false, toplevel=false, in_gscope=false)
    match st
    case K"generator"(_)
        @fail(st, "`generator` requires >=2 args")
    case K"generator"(val, K"filter"(cond, is...))
        vst1(vcx, val) &
            vst1(vcx, cond) &
            all(vst1_iter, vcx, is)
    case K"generator"(val, is...)
        vst1(vcx, val) & all(vst1_iter, vcx, is)
    case K"generator"(_...)
        @fail(st, "malformed `generator`")
    case _
        @fail(st, "expected `generator`")
    end
end

vst1_iter(vcx, st) = match st
    case K"="(K"outer"(i), v)
        vst1_assign_lhs(vcx, i) & vst1(vcx, v)
    # rare, malformed, happens to work in desugaring
    case K"="(i, K"..."(v))
        vst1_assign_lhs(vcx, i) & vst1(vcx, v)
    case K"="(i, v)
        vst1_assign_lhs(vcx, i) & vst1(vcx, v)
    case _
        @fail(st, "expected one of `=`, `in`, `∈`")
end

#-------------------------------------------------------------------------------
# Pre-macro-expansion (st0) is mostly a subset of st1, except with `macrocall`
# and `quote`.

"""
Assumes `st` is parsed from surface syntax, and not a partially-expanded tree.
"""
function valid_st0(st::SyntaxTree)
    DEBUG && assert_syntaxtree(st)
    vr = vst1(with(Validation1Context(), unexpanded=true), st)
    # hack: A macrocall can show up almost anywhere, so filter errors pointing
    # at macrocalls instead of adding cases to every function above.
    isnothing(vr.errors) && return vr.ok
    vr2_errors = filter(vr.errors) do err
        isempty(err.sts) || !(kind(err.sts[1]) === K"macrocall")
    end
    vr2 = ValidationResult(isempty(vr2_errors), vr2_errors)
    return vr2.ok
end

vst0(_, st) = vst1(with(Validation1Context(), unexpanded=true), st)

"""
TODO: While we can't validate any arguments to a macrocall in general, it would
make sense to check usage for things like @ccall and @doc.
"""
vst0_macrocall(vcx, st) = match st
    case _ if !vcx.unexpanded
        @fail(st, "macrocall not valid in AST after macro expansion")
    case K"macrocall"(name, K"Value"(), args...) if (
            typeof(st[2].value) in (LineNumberNode, MacroSource))
        pass()
    case K"macrocall"(_...)
        @fail(st, "expected (macrocall name linenode args...)")
    case _
        @fail(st, "invalid macrocall syntax")
end

vst0_quoted(vcx, st; quote_level) = match st
    case K"$"(x) if quote_level===1
        vst1_splat_or_val(vcx, x)
    case K"$"(x)
        vst0_quoted(vcx, x; quote_level=quote_level-1)
    case K"quote"(x)
        vst0_quoted(vcx, x; quote_level=quote_level+1)
    case _
        all(vst0_quoted, vcx, children(st); quote_level)
end

#-------------------------------------------------------------------------------
# Tree invariants assumed everywhere, including `show`, so fallback printing
# should be used on failure.  (These checks really belong in the type system,
# but failure should only be possible working on AST-internal functions.)

function assert_syntaxtree(st::SyntaxTree)
    vr = _assert_syntaxtree(st, NodeId[], pass())
    @jl_assert is_known(vr) st
    if !vr.ok
        msg = string("assert_syntaxtree failed: ", node_string(st), "\n")
        for err in vr.errors
            msg *= "node: " * node_string(only(err.sts)) *
                "\nreason: " * string(err.msgs)
        end
        throw(error(msg))
    end
    nothing
end

function _assert_syntaxtree(st::SyntaxTree, parents::Vector{NodeId}, vr)
    if st._id in parents
        err = "cycle detected: ["
        for p in parents
            err *= "\n" * node_string(SyntaxTree(st._graph, p))
        end
        return vr & @fail(st, err*"]")
    end
    for a in (:kind, :source)
        vr &= hasattr(st, a) ? pass() : @fail(st, string("needs attribute ", a))
    end
    if is_leaf(st)
        # Note some kinds can show up in non-leaves too
        required_attrs = match st
        case K"Identifier"()
            (:name_val,)
        case K"core"()
            (:name_val,)
        case K"top"()
            (:name_val,)
        case K"Symbol"()
            (:name_val,)
        case K"globalref"()
            (:name_val,:mod)
        case K"Placeholder"()
            ()
        case K"BindingId"()
            (:var_id,)
        case K"label"()
            (:id,)
        case K"symboliclabel"()
            (:name_val,)
        case K"symbolicgoto"()
            (:name_val,)
        case K"oldsymbolicgoto"()
            (:name_val,)
        case K"Value"()
            (:value,)
        case K"slot"()
            (:var_id,)
        case K"static_parameter"()
            (:var_id,)
        case K"SSAValue"()
            (:var_id,)
        case K"nothing"()
            ()
        case K"TOMBSTONE"()
            ()
        case K"SourceLocation"()
            ()
        case K"latestworld"()
            ()
        case K"latestworld_if_toplevel"()
            ()
        case _ if JuliaSyntax.is_literal(st)
            (:value,)
        case _ if JuliaSyntax.is_trivia(st)
            () # green tree only
        case _ if JuliaSyntax.is_operator(st)
            (:name_val,) # TODO: remove
        case _
            return vr & @fail(st, "unrecognized leaf kind")
        end
    else
        required_attrs = match st
        case K"code_info"(_...)
            (:slots, :is_toplevel_thunk)
        case K"scope_block"(_...)
            (:scope_type,)
        case K"unknown_head"(_...)
            (:name_val,)
        case _
            ()
        end
    end
    for a in required_attrs
        vr &= hasattr(st, a) ? pass() : @fail(st, string("needs attribute ", a))
    end
    # TODO: Proper traversal along .source and .macro_source (need to cache
    # results to avoid exponential repeated lookups, and figure out how these
    # edges may form cycles with child edges)
    st.source === st._id && (vr &= @fail(st, ".source equal to self ID"))
    get(st, :macro_source, nothing) === st._id &&
        (vr &= @fail(st, ".macro_source equal to self ID"))

    push!(parents, st._id)
    for c in children(st)
        vr &= _assert_syntaxtree(c, parents, vr)
    end
    pop!(parents)
    vr
end

#-------------------------------------------------------------------------------
# The post-desugaring tree "st2".  Failure shouldn't be reachable by user code;
# this is just for internal documentation and debugging purposes.

Base.@kwdef struct Validation2Context <: ValidationContext
    in_method_defs::Bool=false
end

function with(vcx::Validation2Context;
              in_method_defs = vcx.in_method_defs)
    Validation2Context(in_method_defs)
end

function valid_st2(st::SyntaxTree)
    assert_syntaxtree(st)
    vr = vst2(Validation2Context(), st)
    @jl_assert is_known(vr) st
    return vr
end

vst2(vcx::Validation2Context, st::SyntaxTree) = match st
    case _ if is_leaf(st)
        kind(st) in KSet"""
        Identifier BindingId Placeholder nothing static_parameter
        Bool Char Float Float32 BinInt OctInt HexInt Integer
        SourceLocation String Symbol Value core top
        latestworld latestworld_if_toplevel symbolicgoto oldsymbolicgoto symboliclabel TOMBSTONE
        """ ? pass() : @fail(st, "unrecognized leaf kind")

    case K"call"(K"static_eval"(cg), xs...)
        get(cg, :name_val, nothing) == "cglobal" ?
            all(vst2, vcx, xs) : @fail(st, "expected (call (static_eval cglobal) _...)")
    case K"call"(xs...)
        all(vst2, vcx, xs)
    case K"block"(xs...)
        all(vst2, vcx, xs)
    case K"scope_block"(xs...)
        all(vst2, vcx, xs)
    case K"="(l, r)
        vst2_ident_lhs(vcx, l) & vst2(vcx, r)
    case K"assign_or_constdecl_if_global"(l, r)
        vst2_ident_lhs(vcx, l) & vst2(vcx, r)
    case K"constdecl"(l)
        vst2_ident_lhs(vcx, l)
    case K"constdecl"(l, r)
        vst2_ident_lhs(vcx, l) & vst2(vcx, r)
    case K"global"(x)
        vst2_ident_lhs(vcx, x)
    case K"local"(x)
        vst2_ident_lhs(vcx, x)
    case K"decl"(x, t)
        vst2_ident(vcx, x) & vst2(vcx, t)
    case K"if"(cond, t)
        vst2(vcx, cond) & vst2(vcx, t)
    case K"if"(cond, t, f)
        vst2(vcx, cond) & vst2(vcx, t) & vst2_else(vcx, f)
    case K"&&"(xs...)
        all(vst2, vcx, xs)
    case K"||"(xs...)
        all(vst2, vcx, xs)
    case K"symbolicblock"(K"symboliclabel"(), body)
        vst2(vcx, body)
    case K"break"(K"symboliclabel"())
        pass()
    case K"break"(K"symboliclabel"(), x)
        vst2(vcx, x)
    case K"return"(x)
        vst2(vcx, x)
    case K"trycatchelse"(t, c)
        vst2(vcx, t) & vst2(vcx, c)
    case K"trycatchelse"(t, c, e)
        vst2(vcx, t) & vst2(vcx, c) & vst2(vcx, e)
    case K"tryfinally"(t, f)
        vst2(vcx, t) & vst2(vcx, f)
    case K"tryfinally"(t, f, scope)
        vst2(vcx, t) & vst2(vcx, f) & vst2(vcx, scope)
    case K"_opaque_closure"(id, argt, lb, ub, partial, nargs, isva, src, lam)
        vst2_ident(vcx, id) &
        all(vst2, vcx, children(st)[2:end-1]) &
        vst2_lam(vcx, lam)
    case K"_do_while"(body, cond)
        vst2(vcx, body) & vst2(vcx, cond)
    case K"_while"(cond, body)
        vst2(vcx, cond) & vst2(vcx, body)
    case K"inert"(_)
        pass()
    case K"inert_syntaxtree"(_)
        pass()
    case K"lambda"(_...)
        vst2_lam(vcx, st)
    case K"function_decl"(x)
        vst2_ident(vcx, x)
    case K"function_type"(x)
        vst2(vcx, x)
    case K"method"(name, meta, lam)
        !vcx.in_method_defs ?
            @fail(st, "method outside of method_defs") :
            (kind(name) === K"nothing" ? pass() : vst2_ident_val(vcx, name)) &
            vst2(vcx, meta) & vst2_lam(vcx, lam)
    case K"method_defs"(id, body)
        (kind(id) === K"nothing" ? pass() : vst2_ident_val(vcx, id)) &
        vst2(with(vcx; in_method_defs=true), body)
    case K"new"(t, args...)
        vst2(vcx, t) & all(vst2, vcx, args)
    case K"splatnew"(t, arg)
        vst2(vcx, t) & vst2(vcx, arg)
    case K"softscope"()
        pass()
    case K"softscope"(_)
        pass()
    case K"thisfunction"()
        pass()
    case K"gc_preserve_begin"(xs...)
        all(vst2_ident, vcx, xs)
    case K"gc_preserve_end"(xs...)
        minlen(st, xs, 1) & all(vst2_ident, vcx, xs)

    case K"meta"(xs...)
        all(vst2, vcx, xs) # TODO
    case K"loopinfo"(xs...)
        all(vst2, vcx, xs) # TODO
    case K"boundscheck"()
        pass()
    case K"inbounds_pop"()
        pass()
    case K"inbounds"(K"Value"()) if (st[1].value isa Bool)
        pass()
    case K"inline"(K"Value"()) if (st[1].value isa Bool)
        pass()
    case K"noinline"(K"Value"()) if (st[1].value isa Bool)
        pass()
    case K"purity"()
        pass()
    case K"purity"(_, _...)
        numchildren(st) == fieldcount(Base.EffectsOverride) ?
            pass() : @fail(st, "wrong number of args to `purity` expression")
    case K"aliasscope"()
        pass()
    case K"popaliasscope"()
        pass()

    case K"always_defined"(x)
        vst2_ident(vcx, x)
    case K"assert"(K"Symbol"(), x)
        vst2(vcx, x)
    case K"removable"(x)
        vst2(vcx, x)

    # Could be made stricter
    case K"foreigncall"(_, K"static_eval"(rt), K"static_eval"(at), cconv, roots_args...)
        vst2(vcx, rt) &
        vst2(vcx, at) &
        vst2(vcx, cconv) &
        all(vst2, vcx, roots_args)
    case K"foreignglobal"(_)
        pass()
    case K"cfunction"(K"Value"(), K"static_eval"(fptr), K"static_eval"(rt), K"static_eval"(at), K"Symbol"())
        vst2(vcx, fptr) & vst2(vcx, rt) & vst2(vcx, at)
    case K"cfunction"(K"Value"(), fptr, K"static_eval"(rt), K"static_eval"(at), K"Symbol"())
        vst2(vcx, fptr) & vst2(vcx, rt) & vst2(vcx, at)

    case K"isdefined"(x)
        vst2_ident_val(vcx, x)
    case K"isglobal"(K"Placeholder"())
        pass()
    case K"islocal"(K"Placeholder"())
        pass()
    case K"isglobal"(x)
        vst2_ident_val(vcx, x)
    case K"islocal"(x)
        vst2_ident_val(vcx, x)
    case K"locals"()
        pass()
    case _
        @fail(st, "unrecognized form out of desugaring")
end

vst2_ident_lhs(vcx, st) = match st
    case K"Identifier"()
        pass()
    case K"BindingId"()
        pass()
    case K"Placeholder"()
        pass()
    case _
        @fail(st, "expected identifier (lhs)")
end

vst2_ident(vcx, st) = match st
    case K"Identifier"()
        pass()
    case K"BindingId"()
        pass()
    case _
        @fail(st, "expected identifier or BindingId")
end

vst2_ident_val(vcx, st) = match st
    case K"Identifier"()
        pass()
    case K"BindingId"()
        pass()
    case K"core"()
        pass()
    case K"top"()
        pass()
    case _
        @fail(st, "expected identifier (val)")
end

vst2_lam(vcx, st) = match st
    case K"lambda"(K"block"(args...), K"block"(sps...), body)
        all(vst2_ident_lhs, vcx, args) &
        all(vst2_ident_lhs, vcx, sps) &
        vst2(vcx, body)
    case K"lambda"(K"block"(args...), K"block"(sps...), body, rett)
        all(vst2_ident_lhs, vcx, args) &
        all(vst2_ident_lhs, vcx, sps) &
        vst2(vcx, body) &
        vst2(vcx, rett)
    case _
        @fail(st, "malformed lambda")
end

vst2_else(vcx, st) = match st
    case K"elseif"(cond, t)
        vst2(vcx, cond) & vst2(vcx, t)
    case K"elseif"(cond, t, f)
        vst2(vcx, cond) & vst2(vcx, t) & vst2_else(vcx, f)
    case _
        vst2(vcx, st)
end
