struct ValidationDiagnostic
    sts::SyntaxList
    msgs::Vector{String}
    loc::LineNumberNode # for noting where failures come from in this file
end
ValidationDiagnostic(st::SyntaxTree, msg, loc) =
    ValidationDiagnostic(SyntaxList(st), String[msg], loc)

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
@noinline fail(st::SyntaxTree, msg="invalid syntax", loc=nothing) =
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

function Base.all(f::T, vcx::ValidationContext, itr; kws...) where {T<:Function}
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

    # fixme: flisp happens to allow reading of underscore sparam names if they
    # are used in function signature types or other sparam bounds.  See #60626.
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

vst1(vcx::Validation1Context, st::SyntaxTree)::ValidationResult = @stm st begin
    [:identifier] -> vst1_ident(vcx, st)
    [:value] -> pass()
    [:block xs...] -> all(vst1, vcx, xs)
    [:let [:block decls...] body] ->
        all(vst1_symdecl_or_assign, vcx, decls) &
        vst1(with(vcx; in_gscope=false), body)
    [:let decl body] ->
        vst1_symdecl_or_assign(vcx, decl) &
        vst1(with(vcx; in_gscope=false), body)
    [:if _...] -> let
        inner_vcx = vcx.toplevel ? with(vcx; inner_cond=true) : vcx
        @stm st begin
            [:if [:generated] t f] ->
                vst1(inner_vcx, t) & vst1(inner_vcx, f)
            [:if [:generated] _...] ->
                @fail(st, "if-generated requires both true and false cases")
            [:if cond t] ->
                vst1(vcx, cond) & vst1(inner_vcx, t)
            [:if cond t f] ->
                vst1(vcx, cond) & vst1(inner_vcx, t) & vst1(inner_vcx, f)
            _ -> @fail(st, "expected (if cond body) or (if cond body else)")
        end
    end
    [:elseif cond t] -> vst1(vcx, cond) & vst1(vcx, t)
    [:elseif cond t f] -> vst1(vcx, cond) & vst1(vcx, t) & vst1(vcx, f)
    [:try _...] -> vst1_try(vcx, st)
    [:function _...] -> vst1_function(vcx, st)
    [:call _...] -> vst1_call(vcx, st)
    [:var"'" x] -> vst1(vcx, x)
    [:. f [:tuple _...]] -> vst1_dotcall(vcx, st)
    [:. l r] -> vst1(vcx, l) & vst1_dot_getproperty_rhs(vcx, r)
    [:. x] -> vst1(vcx, x) # BroadcastFunction(x)
    [:do call lam] ->
        (vst1_call(vcx, call) | vst1_dotcall(vcx, call) | vst0_macrocall(vcx, call)) &
        vst1_lam(vcx, lam)
    [:(=) _...] -> vst1_assign(vcx, st)
    [:return val] -> vcx.return_ok ?
        vst1(vcx, val) :
        @fail(st, "`return` not allowed inside comprehension or generator")
    ([:continue], when=vcx.in_loop) -> pass()
    ([:continue lab], when=vcx.in_loop) -> vst1_ident(vcx, lab; lhs=true)
    # An unlabeled break is also allowed inside anonymous `@label` blocks;
    # breaking through a named block is rejected with a precise error during
    # linearization.
    ([:break], when=vcx.in_loop||vcx.in_symblock) -> pass()
    ([:break lab], when=vcx.in_loop||vcx.in_symblock) ->
        vst1_ident(vcx, lab; lhs=true)
    ([:break lab x], when=vcx.in_loop||vcx.in_symblock) ->
        vst1_ident(vcx, lab; lhs=true) & vst1(vcx, x)
    [:for [:block is...] body] ->
        all(vst1_iter, vcx, is) &
        vst1(with(vcx; in_loop=true, in_gscope=false), body)
    [:for iter1 body] ->
        vst1_iter(vcx, iter1) &
        vst1(with(vcx; in_loop=true, in_gscope=false), body)
    [:while cond body] ->
        vst1(vcx, cond) &
        vst1(with(vcx; in_loop=true, in_gscope=false), body)
    [:tuple _...] ->
        vst1_tuple(vcx, st)
    [:curly t tvs...] ->
        vst1(vcx, t) & no_assignment(tvs, "type parameter list") &
        all(vst1_curly_typevar, vcx, tvs)
    [:where t tds...] ->
        vst1(vcx, t) & all(vst1_typevar_decl, vcx, tds)
    [:string xs...] ->
        all(vst1_splat_or_val, vcx, xs)
    [:-> _...] ->
        vst1_lam(vcx, st)
    [:flatten g] -> vst1_generator(vcx, g)
    [:generator _...] -> vst1_generator(vcx, st)
    [:comprehension [:flatten g]] -> vst1(vcx, g)
    [:comprehension g] -> vst1(vcx, g)
    [:comprehension xs...] ->
        # HACK: We shouldn't be creating trees here, but this is extremely rare
        # (deprecated even in 2016)
        vst1_generator(vcx, @ast _ st [:generator xs...])
    [:typed_comprehension t [:flatten g]] ->
        vst1(vcx, t) & vst1(vcx, g)
    [:typed_comprehension t g] ->
        vst1(vcx, t) & vst1(vcx, g)
    [:comparison xs...] ->
        length(xs) < 3 || iseven(length(xs)) ?
        @fail(st, "`comparison` expects n>=3 args and odd n") :
        # TODO: can we restrict xs[2:2:end] to identifier or .identifier?
        all(vst1, vcx, xs[2:2:end]) &
        all(vst1, vcx, xs[1:2:end])
    [:<: xs...] -> all(vst1_call_arg, vcx, xs)
    [:>: xs...] -> all(vst1_call_arg, vcx, xs)
    [:--> xs...] -> all(vst1_call_arg, vcx, xs)
    [:(::) x y] -> vst1(vcx, x) & vst1(vcx, y)
    # TODO: inner_cond on args[2:end]
    [:&& xs...] -> all(vst1, vcx, xs)
    [:|| xs...] -> all(vst1, vcx, xs)
    [:.&& x y] -> vst1(vcx, x) & vst1(vcx, y)
    [:.|| x y] -> vst1(vcx, x) & vst1(vcx, y)
    (_, when=(vr=vst1_arraylike(vcx, st); is_known(vr))) -> vr
    # syntax TODO: disallow pre-desugared const, broken with complex rhs
    [:const l r] -> vst1_ident(vcx, l; lhs=true) & vst1(vcx, r)
    [:const [:global x]] -> !vcx.toplevel ?
        @fail(st, "unsupported `const` inside function") :
        vst1_const_assign(vcx, x)
    [:const x] ->  !vcx.toplevel ?
        @fail(st, "unsupported `const` inside function") :
        vst1_const_assign(vcx, x)
    [:global xs...] -> minlen(st, xs, 1) & all(vst1_global_arg, vcx, xs)
    [:local xs...] -> minlen(st, xs, 1) & all(vst1_local_arg, vcx, xs)
    (_, when=(vr=vst1_dotted_or_op_assign(vcx, st); is_known(vr))) -> vr
    [:macrocall _...] -> vst0_macrocall(vcx, st)
    [:quote x] -> vcx.unexpanded ? vst0_quoted(vcx, x; quote_level=1) :
        @fail(st, "interpolating quote not valid syntax after macro expansion")

    #---------------------------------------------------------------------------
    # Forms not produced by the parser
    [:ssavalue [:value]] -> pass()
    [:static_parameter [:value]] -> pass()
    [:inert _] -> pass()
    [:syntaxinert _] -> pass()
    [:core [:identifier]] -> pass()
    [:top [:identifier]] -> pass()
    [:meta _...] -> pass() # TODO
    [:toplevel xs...] -> pass() # this will be validated when we lower it
    [:opaque_closure argt lb ub bool lam] ->
        all(vst1, vcx, [argt, lb, ub, bool]) & vst1_lam(vcx, lam)
    [:symboliclabel lab] -> vst1_ident(vcx, lab; lhs=true)
    [:symbolicgoto lab] -> vst1_ident(vcx, lab; lhs=true)
    [:symbolicblock lab body] ->
        vst1_ident(vcx, lab; lhs=true) & vst1(with(vcx; in_symblock=true), body)
    [:gc_preserve x ids...] -> vst1(vcx, x) & all(vst1_ident, vcx, ids)
    # lowering TODO: 0 args segfaults
    [:gc_preserve_begin ids...] -> all(vst1_ident, vcx, ids)
    [:gc_preserve_end ids...] -> all(vst1_ident, vcx, ids)
    [:isdefined [:identifier]] -> pass()
    [:isdefined [:static_parameter [:value]]] -> pass()
    [:lambda _...] -> vst1_raw_lambda(vcx, st)
    [:var"with-static-parameters" lam sps...] ->
        vst1_raw_lambda(vcx, lam) & all(vst1_ident, vcx, sps; lhs=true)
    [:softscope _] -> pass()
    [:softscope] -> pass()
    [:generated] -> pass()
    [:foreigncall fname rt at cconv roots_args...] ->
        # TODO: could be stricter
        vst1(vcx, fname) &
        vst1(vcx, rt) &
        vst1(vcx, at) &
        vst1(vcx, cconv) &
        all(vst1, vcx, roots_args)
    [:foreignglobal fname] -> vst1(vcx, fname) # TODO: could be stricter
    [:cfunction [:value] f rt at [:inert [:identifier]]] ->
        vst1(vcx, f) & vst1(vcx, rt) & vst1(vcx, at)
    [:cconv tup nreq] -> (tup.value isa Tuple && nreq.value isa Int) ? pass() :
        @fail(st, "expected (cconv convention_tuple n_req_args)")
    [:tryfinally t f] -> vst1(vcx, t) & vst1(vcx, f)
    [:tryfinally t f scope] -> vst1(vcx, t) & vst1(vcx, f) & vst1(vcx, scope)
    [:loopinfo _...] -> pass() # TODO
    [:boundscheck] -> pass() # optional bool arg does nothing
    ([:boundscheck [:value]], when=(st[1].value isa Bool)) -> pass()
    ([:inbounds [:value]], when=(st[1].value isa Bool)) -> pass()
    ([:inbounds [:identifier]], when=(syntax_name(st[1]) == "pop")) -> pass()
    ([:inline [:value]], when=(st[1].value isa Bool)) -> pass()
    ([:noinline [:value]], when=(st[1].value isa Bool)) -> pass()
    [:purity] -> pass()
    [:purity _ _...] -> numchildren(st) == fieldcount(Base.EffectsOverride) ?
        pass() : @fail(st, "wrong number of args to `purity` expression")
    [:locals] -> pass()
    [:islocal _] -> pass()
    [:isglobal _] -> pass()
    [:copyast [:inert _]] -> pass()
    [:new t args...] -> vst1(vcx, t) & all(vst1, vcx, args)
    [:splatnew t arg] -> vst1(vcx, t) & vst1(vcx, arg)
    [:thisfunction] -> vcx.toplevel ?
        @fail(st, "can only be used inside a function") :
        !vcx.return_ok ?
        @fail(st, "current function not defined in comprehension or generator") : pass()
    [:var"latestworld-if-toplevel"] -> pass()
    [:var"scope-block" xs...] -> minlen(st, xs, 1) & maxlen(st, xs, 1) &
        all(vst1, with(vcx; in_gscope=false), xs)
    [:aliasscope] -> pass()
    [:popaliasscope] -> pass()

    #---------------------------------------------------------------------------
    # Invalid forms for which we want to produce detailed errors
    [:... _...] ->
        @fail(st, "unexpected `...`\nsplatting can only be done into a `call`, `tuple`, `curly`, or array-like expression")
    [:parameters _...] ->
        @fail(st, "unexpected semicolon")
    [:braces _...] ->
        @fail(st, "`{ }` outside of `where` is reserved for future use")
    [:bracescat _...] ->
        @fail(st, "`{ }` outside of `where` is reserved for future use")
    [:atomic _...] ->
        @fail(st, "unimplemented or unsupported `atomic` declaration")
    [:(::) x] ->
        @fail(st, "`::` must be written `value::type` outside function argument lists")
    # internal
    [:symbol] ->
        @fail(st, "`Symbol` kind not valid until desugaring")
    [:placeholder] ->
        @fail(st, "`Placeholder` kind not valid until desugaring")
    [:$ x] -> @fail(st, raw"`$` expression outside string or quote")
    [:continue _...] ->
        @fail(st, "`continue` outside of a `while` or `for` loop")
    [:break] ->
        @fail(st, "unlabeled `break` outside of a `while` or `for` loop")
    [:break _...] ->
        @fail(st, "labeled `break` outside of loop or symbolic block")
    _ -> let top_vr = vst1_toplevel_only(vcx, st)
        if vcx.toplevel
            top_vr
        else
            !top_vr.ok ? unknown() :
                @fail(st, "this syntax is only allowed at top level")
        end
    end | @fail(st, string(
        "invalid syntax: unknown form `", head(st),
        "` or number of arguments ", numchildren(st)))
end

vst1_toplevel_only(vcx, st) = @stm st begin
    # body will be validated when lowered
    [:module [:value] [:value] [:identifier] [:block xs...]] ->
        !(st[1].value isa VersionNumber) ? @fail(st[1], "expected version") :
        !(st[2].value isa Bool) ? @fail(st[2], "expected boolean bare flag") :
        pass()
    [:module [:value] [:identifier] [:block xs...]] ->
        !(st[1].value isa Bool) ? @fail(st[1], "expected boolean bare flag") :
        pass()
    [:macro _...] ->
        vst1_macro(vcx, st)
    [:struct [:value] sig [:block body...]] ->
        vst1_typesig(vcx, sig) & (
            !(st[1].value isa Bool) ? @fail(st[1], "expected mutable flag") :
                _struct_noassign(vcx, body) & all(vst1_struct_arg, vcx, body))
    [:abstract sig] ->
        vst1_typesig(vcx, sig)
    [:primitive sig n] ->
        vst1_typesig(vcx, sig) & vst1(vcx, n)
    [:import [:(:) p1 ps...]] ->
        (vst1_importpath(vcx, p1; dots_ok=true) &
        all(vst1_importpath, vcx, ps; dots_ok=false))
    [:using  [:(:) p1 ps...]] ->
        (vst1_importpath(vcx, p1; dots_ok=true) &
        all(vst1_importpath, vcx, ps; dots_ok=false))
    [:import ps...] ->
        minlen(st, ps, 1) & all(vst1_importpath, vcx, ps; dots_ok=true)
    [:using  ps...] ->
        minlen(st, ps, 1) & all(vst1_importpath, vcx, ps; dots_ok=true)
    [:public xs...] -> all(vst1_ident, vcx, xs)
    [:export xs...] -> all(vst1_ident, vcx, xs)
    [:latestworld] -> pass()
    [:typegroup [:block xs...]] -> all(vst1, vcx, xs) &
        # mostly to catch compiler bugs; semantically it would be fine to allow.
        (edition(st) >= JL_OLD_EDITION ?
            pass() : @fail(st, "typegroup not supported in this edition"))
    _ -> unknown()
end

#-------------------------------------------------------------------------------

vst1_local_arg(vcx, st) = @stm st begin
    [:function _...] -> vst1_function(vcx, st)
    _ -> vst1_symdecl_or_assign(vcx, st) | vst1_dotted_or_op_assign(vcx, st) |
        @fail(st, "invalid local declaration: expected identifier or assignment")
end

vst1_global_arg(vcx, st) = @stm st begin
    [:function _...] -> vcx.toplevel ?
        vst1_function(vcx, st) :
        @fail(st, "global function needs to be placed at top level, or use eval")
    _ -> vst1_symdecl_or_assign(vcx, st) | vst1_dotted_or_op_assign(vcx, st) |
        @fail(st, "invalid global declaration: expected identifier or assignment")
end

# @stm doesn't work so well with n dots and m identifiers
# one of:
# (as (importpath . . . x y z) ident)
#     (importpath . . . x y z)
# where y, z may be quoted (syntax TODO: require var"" for odd identifiers?)
function vst1_importpath(vcx, st; dots_ok)
    ok = pass()
    path_components = @stm st begin
        [:as [:. xs...] [:identifier]] -> xs
        [:as [:. xs...] x] -> (ok &= @fail(x, "expected identifier"); xs)
        [:. xs...] -> xs
        _ -> return @fail(st, "malformed import path")
    end
    seen_first = false
    for c in path_components
        if head(c) === :identifier && syntax_name(c) === "."
            if !dots_ok || seen_first
                ok &= @fail(c, "unexpected `.` in import path")
            end
            continue
        end
        if head(c) === :inert && numchildren(c) == 1
            c = c[1]
        end
        # syntax todo: lhs should probably not be true here
        ok = ok & (vst1_ident(vcx, c).ok ? pass() : vst1_ident(vcx, c; lhs=true))
        seen_first = true
    end
    return !seen_first ? @fail(st, "expected identifier in `importpath`") : ok
end

vst1_tuple(vcx, st) = @stm st begin
    [:tuple [:parameters kws...]] -> all(vst1_call_kwarg, vcx, kws)
    [:tuple [:parameters _ _...] _ _...] -> @fail(
        st[1], "cannot mix tuple `(a,b,c)` and named tuple `(;a,b,c)` syntax")
    ([:tuple args...], when=any(x->head(x)===:(=), args)) ->
        all(vst1_call_arg, vcx, args)
    [:tuple xs...] -> all(vst1_splat_or_val, vcx, xs)
    _ -> @fail(st, "malformed tuple")
end

# TODO: disallow (has-unmatched-symbolic-goto? tryb)
vst1_try(vcx, st) = @stm st begin
    [:try _] -> @fail(st, "try without catch or finally")
    [:try tryb cvar catchb] ->
        vst1(vcx, tryb) &
        vst1_try_catchvar(vcx, cvar) &
        vst1(vcx, catchb)
    [:try tryb cvar catchb finallyb] ->
        vst1(vcx, tryb) &
        vst1_try_catchvar(vcx, cvar) &
        vst1(vcx, catchb) &
        vst1(vcx, finallyb)
    [:try tryb cvar catchb finallyb elseb] ->
        vst1(vcx, tryb) &
        vst1_try_catchvar(vcx, cvar) &
        vst1(vcx, catchb) &
        vst1(vcx, finallyb) &
        vst1(vcx, elseb)
    _ -> @fail(st, "malformed `try` expression")
end

vst1_try_catchvar(_vcx, st) = @stm st begin
    [:identifier] -> pass()
    ([:value], when=st.value===false) -> pass()
end

# syntax TODO:
# - const is inoperative in the function case
# - single-arg const with no value (presumably to poison this name) was likely
#   not intended to work, and can only be produced by macros
vst1_const_assign(vcx, st) = @stm st begin
    [:(=) _ _] -> vst1_assign(vcx, st; in_const=true)
    [:identifier] -> pass()
    [:local _...] -> @fail(st, "unsupported `const local` declaration")
    _ -> @fail(st, "expected assignment after `const`")
end

# syntax TODO: all-underscore variables may be read from with dot syntax
vst1_dot_getproperty_rhs(vcx, st) = @stm st begin
    [:inert x] -> pass()
    [:syntaxinert x] -> pass()
    [:identifier] -> pass()
    [:value] -> pass()
    _ -> @fail(st, "invalid `.` syntax")
end

# We can't validate A.B in general (usually lowers to getproperty), but it shows
# up in a number of syntax special cases where we can. (flisp: sym-ref?)
vst1_calldecl_dot_name(vcx, st) = @stm st begin
    [:. l r] ->
        vst1_calldecl_dot_name(vcx, l) &
        vst1_calldecl_dot_name_rhs(vcx, r) |
        @fail(st, "invalid `.` form")
    [:value] -> pass()
    i -> vst1_ident(vcx, i)
end

vst1_calldecl_dot_name_rhs(vcx, st) = @stm st begin
    [:inert x] -> vst1_calldecl_dot_name_rhs(vcx, x)
    [:syntaxinert x] ->  vst1_calldecl_dot_name_rhs(vcx, x)
    [:identifier] -> vst1_ident(vcx, st; lhs=true)
    ([:value], when=st.value isa String) -> _ident_str(vcx, st, st.value; lhs=true)
    [:tuple _...] -> @fail(st, "dotcall syntax not valid here")
    _ -> @fail(st, "invalid `.` syntax")
end

vst1_symdecl_or_assign(vcx, st) =
    @fail(st, "expected identifier or assignment") |
    vst1_symdecl(vcx, st) | vst1_assign(vcx, st)

vst1_symdecl(vcx, st) = @stm st begin
    [:identifier] -> pass()
    [:(::) [:identifier] t] -> vst1(vcx, t)
    _ -> @fail(st, "expected identifier or `identifier::type`")
end

# TODO: globalref (identifier with .mod) might not be valid everywhere; check
# usage of this function
vst1_ident(vcx, st; lhs=false) = @stm st begin
    [:identifier] -> _ident_str(vcx, st, syntax_name(st); lhs)
    _ -> @fail(st, "expected identifier")
end
function _ident_str(vcx, st, s::String; lhs=false)
    if !lhs && (!vcx.readable_underscore || !is_flisp_compat(st)) &&
        is_writeonly_est_name(s)
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

vst1_call(vcx, st) = @stm st begin
    ([:call [:identifier] args...], when=syntax_name(st[1])==="cglobal") ->
        (1 <= length(args) <= 2 ? pass() :
            @fail(st, "cglobal must have one or two arguments")) &
        all(vst1_call_arg, vcx, args)
    [:call f [:parameters kwargs...] args...] ->
        (vst1_ident(vcx, f) | vst1(vcx, f)) &
        all(vst1_call_arg, vcx, args) &
        all(vst1_call_kwarg, vcx, kwargs)
    [:call f args...] ->
        (vst1_ident(vcx, f) | vst1(vcx, f)) &
        all(vst1_call_arg, vcx, args)
    [:call _...] -> @fail(st, "malformed `call`")
    _ -> unknown()
end

vst1_dotcall(vcx, st) = @stm st begin
    [:. f [:tuple [:parameters kwargs...] args...]] ->
        vst1(vcx, f) & all(vst1_call_kwarg, vcx, kwargs) &
        all(vst1_call_arg, vcx, args)
    [:. f [:tuple args...]] ->
        vst1(vcx, f) & all(vst1_call_arg, vcx, args)
    _ -> unknown()
end

# Arg to call (not function decl), pre-semicolon.  This can be anything, but
# additionally allow `kw` and `...` forms.
vst1_call_arg(vcx, st) = @stm st begin
    [:kw id val] -> vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    _ -> vst1_splat_or_val(vcx, st)
end

# Arg to `parameters` (post-semicolon) in a call (not function decl).  Stricter
# than `vst1_call_arg`.  `=` desugars to `kw`.
vst1_call_kwarg(vcx, st) = @stm st begin
    [:identifier] -> pass()
    [:kw id val] -> vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    [:(=) id val] -> vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    [:... x] -> vst1(vcx, x)
    [:. x [:inert id]] -> vst1(vcx, x) & vst1_ident(vcx, id; lhs=true)
    [:. x [:syntaxinert id]] -> vst1(vcx, x) & vst1_ident(vcx, id; lhs=true)
    ([:call [:identifier] symval v], when=(syntax_name(st[1])==="=>")) ->
        vst1(vcx, symval) & vst1(vcx, v)
    _ -> @fail(st, "expected identifier, `=`, or `...` after semicolon")
end

vst1_lam(vcx, st) = let
    f_vcx = with(vcx; return_ok=true, toplevel=false, in_gscope=false)
    @stm st begin
        [:-> l r] ->
            vst1_lam_lhs(with(f_vcx; return_ok=false), l) & vst1(f_vcx, r)
        _ -> @fail(st, "expected `->` expression")
    end
end

vst1_lam_lhs(vcx, st) = @stm st begin
    [:tuple [:parameters _...] ps...] ->
        _calldecl_positionals(vcx, ps, true) & vst1_calldecl_kws(vcx, st[1])
    [:tuple ps...] ->
        _calldecl_positionals(vcx, ps, true)
    [:where ps tds...] ->
        vst1_lam_lhs(vcx, ps) &
        all(vst1_typevar_decl, with(vcx; readable_underscore=true), tds)
    # syntax TODO: This is handled badly in the parser
    [:block] -> pass()
    [:block x] -> _calldecl_positionals(vcx, SyntaxList(x), true)
    [:block x p] -> _calldecl_positionals(vcx, SyntaxList(x), true) &
        @stm p begin
            [:(=) kw v] -> vst1_param(vcx, kw) & vst1(vcx, v)
            [:kw kw v] -> vst1_param(vcx, kw) & vst1(vcx, v)
            [:... kw] -> vst1_param_varkw(vcx, kw)
            _ -> vst1_param(vcx, p)
        end
    [:block _ _ _ _...] -> @fail(st, "more than one semicolon in signature")
    # unwrapped single arg
    _ -> let ps = SyntaxList(st)
        _calldecl_positionals(vcx, ps, true)
    end
end

vst1_function(vcx, st) = let
    f_vcx = with(vcx; return_ok=true, toplevel=false, in_gscope=false)
    # lowering TODO: conditional nested function definitions are known to be
    # broken, but are not disallowed, and can be found in stdlibs.
    # vcx.inner_cond && @fail(st, "conditional inner method definitions\
    #     are not supported; use `()->()` syntax instead")
    @stm st begin
        [:function name] -> vst1_ident(vcx, name)
        [:function callex body] ->
            vst1_function_calldecl(with(vcx; return_ok=false), callex) &
            vst1(f_vcx, body)
        [:(=) callex body] ->
            vst1_function_calldecl(with(vcx; return_ok=false), callex) &
            vst1(f_vcx, body)
        _ -> @fail(st, "malformed `function`")
    end
end

# Note that we consistently refer to children of a declaring call as
# "parameters" rather than arguments (and children of a :parameters block as
# "keyword args/params") so we don't mix them up with children to a real call,
# whose valid forms are subtly different.

vst1_function_calldecl(vcx, st) = @stm st begin
    [:where callex tds...] ->
        vst1_function_calldecl(vcx, callex) &
        all(vst1_typevar_decl, with(vcx; readable_underscore=true), tds)
    [:(::) callex rt] ->
        vst1_simple_calldecl(vcx, callex) & vst1(vcx, rt)
    _ -> vst1_simple_calldecl(vcx, st)
end

vst1_simple_calldecl(vcx, st) = @stm st begin
    [:call f [:parameters _...] ps...] ->
        vst1_calldecl_name(vcx, f) &
        _calldecl_positionals(vcx, ps, false) &
        vst1_calldecl_kws(vcx, st[2])
    [:call f ps...] -> vst1_calldecl_name(vcx, f) &
        _calldecl_positionals(vcx, ps, false)
    # anonymous function syntax `function (x); end` or `function (x...); end` is
    # subject to bad-arglist rules (block, etc.)
    _ ->  vst1_lam_lhs(vcx, st) | @fail(st, "malformed `call` in function decl")
end

vst1_macro(vcx, st) = @stm st begin
    [:macro m] -> vst1_ident(vcx, m; lhs=true) | vst1_ident(vcx, m; lhs=false)
    [:macro [:call _ [:parameters _...] _...] _...] ->
        @fail(st[1][end], "macros cannot accept keyword arguments")
    [:macro [:call m ps...] body] ->
        let vcx = with(vcx; return_ok=false, toplevel=false, in_gscope=false)
            vst1_macro_calldecl_name(vcx, m) &
                _calldecl_positionals(vcx, ps, false) &
                vst1(with(vcx; return_ok=true), body)
        end
    [:macro [:where _...] _...] ->
        @fail(st[1], "`where` not allowed in macro signatures")
    [:macro _...] -> @fail(st, "malformed `macro`")
    _ -> unknown()
end

# Macros may have either underscore or reserved (ccall, cglobal) names
vst1_macro_calldecl_name(vcx, st) = @stm st begin
    [:. _ _] -> vst1_calldecl_dot_name(vcx, st)
    m -> @fail(st, "invalid macro name") |
        vst1_ident(vcx, m; lhs=true) | vst1_ident(vcx, m; lhs=false)
end

vst1_calldecl_name(vcx, st) = @stm (st=strip_arg_meta(st)) begin
    [:identifier] -> vst1_ident(vcx, st; lhs=true) &
        (!is_dotted_operator(syntax_name(st)) ? pass() :
        @fail(st, "dotted operator is not a valid function name"))
    [:. _ _] ->
        vst1_calldecl_dot_name(vcx, st)
    [:curly t tvs...] ->
        vst1_calldecl_name(vcx, t) & all(vst1, vcx, tvs)
    [:value] ->
        pass() # GlobalRef works. Function? Type?
    ([:(::) _...], when=!vcx.toplevel) ->
        @fail(st, "adding methods to callable type only allowed at top level")
    [:(::) t] -> vst1(vcx, t)
    [:(::) x t] -> vst1_pparam_simple_tuple(vcx, x) & vst1(vcx, t)
    # TODO: @overlay broken in many cases, should be stricter
    [:overlay mt x] ->
        vst1(vcx, mt) & vst1_calldecl_name(vcx, x)

    [:where t tds...] ->
        vst1_calldecl_name(vcx, t) & all(vst1_typevar_decl, vcx, tds)
    _ -> @fail(st, "invalid function name")
end

strip_arg_meta(st) = @stm st begin
    [:meta s arg] -> let meta_s = est_syntax_name(s, "")
        meta_s isa String || return st
        head(arg) === :meta ? st :
            !(meta_s in ("specialize", "nospecialize")) ? st : arg
    end
    _ -> st
end

# Check mandatory and optional positional params:
# `[pparam* pparam_and_default* pparam_and_splatdefault? pparam_va?]`
# TODO: add list matching to @stm
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
        if head(p) === :kw || head(p) === :(=) && eq_is_kw
            require_assign = true
            allow_val_splat = i == lastindex(params)
            ok[] &= vst1_pparam_and_default(vcx, p; eq_is_kw, allow_val_splat)
        elseif head(p) === :...
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
vst1_pparam_va(vcx, st; eq_is_kw) = @stm st begin
    [:kw [:... va] val] ->
        vst1_pparam_typed_tuple(vcx, va) & vst1_splat_or_val(vcx, val)
    ([:(=) [:... va] val], when=eq_is_kw) ->
        vst1_pparam_typed_tuple(vcx, va) & vst1_splat_or_val(vcx, val)
    [:... va] -> vst1_pparam_typed_tuple(vcx, va)
    _ -> unknown()
end

# destructuring args: function f(a, (x, y)) ...  TODO: the strip_arg_meta call
# here corresponds to no-op nospecialize, and should ideally be removed.
vst1_pparam_typed_tuple(vcx, st) = @stm (st=strip_arg_meta(st)) begin
    [:(::) [:tuple _...] t] ->
        vst1_pparam_simple_tuple(vcx, st[1]) &
        vst1(with(vcx; readable_underscore=true), t)
    [:tuple _...] -> vst1_pparam_simple_tuple(vcx, st)
    _ -> vst1_param(vcx, st)
end
vst1_pparam_simple_tuple_or_splat(vcx, st) = @stm st begin
    [:... t] -> vst1_pparam_simple_tuple(vcx, t)
    t -> vst1_pparam_simple_tuple(vcx, t)
end
# Similar to an assignment to a tuple LHS, but does not allow `::`.  Also should
# not allow ref, curly, or call, but flisp does, so we may need to change this.
vst1_pparam_simple_tuple(vcx, st) = @stm st begin
    [:identifier] -> pass()
    [:tuple [:parameters _ _...] _ _...] -> @fail(
        st[1], "cannot mix tuple `(a,b,c)` and named tuple `(;a,b,c)` syntax")
    [:tuple [:parameters kws...]] -> all(vst1_ident, vcx, kws; lhs=true)
    [:tuple xs...] ->
        all(vst1_pparam_simple_tuple_or_splat, vcx, xs) &
        (count(head(x)===:... for x in xs) <= 1 ? pass() :
        @fail(st, "multiple `...` in destructured parameter is ambiguous"))
    [:(::) _...] -> @fail(st, "cannot have type in destructured argument")
    _ -> @fail(st, "expected identifier or tuple")
end

vst1_param(vcx, st) = @stm st begin
    [:identifier] -> vst1_ident(vcx, st; lhs=true)
    [:(::) id t] -> vst1_ident(vcx, id; lhs=true) &
        vst1(with(vcx; readable_underscore=true), t)
    [:(::) t] -> vst1(with(vcx; readable_underscore=true), t)
    _ -> @fail(st, "expected identifier or `identifier::type`")
end

# allow_val_splat=true when this is the final optional param (even if there are
# varargs after it).  See #50563
vst1_pparam_and_default(vcx, st; eq_is_kw, allow_val_splat) = @stm st begin
    [:kw id val] ->
        vst1_pparam_typed_tuple(vcx, id) & @stm val begin
            [:... v] -> allow_val_splat ? vst1(vcx, v) :
                @fail(val, "splat only allowed on final positional default arg")
            _ -> vst1(with(vcx; return_ok=true, toplevel=false, in_gscope=false), val)
        end
    ([:(=) id val], when=eq_is_kw) ->
        vst1_pparam_typed_tuple(vcx, id) & @stm val begin
            [:... v] -> allow_val_splat ? vst1(vcx, v) :
                @fail(val, "splat only allowed on final positional default arg")
            _ -> vst1(with(vcx; return_ok=true, toplevel=false, in_gscope=false), val)
        end
    _ -> @fail(st, "malformed optional positional parameter; expected `=`")
end

vst1_calldecl_kws(vcx, st) = @stm st begin
    ([:parameters kws... last],
     when=(varkw = strip_arg_meta(last);
           head(varkw) === :... && numchildren(varkw) == 1)) ->
         all(vst1_param_kw, vcx, kws) & vst1_param_varkw(vcx, varkw[1])
    [:parameters kws...] -> all(vst1_param_kw, vcx, kws)
    _ -> @fail(st, "malformed keyword parameters")
end

vst1_param_varkw(vcx, st) = @stm st begin
    [:identifier] -> vst1_ident(vcx, st; lhs=true)
    [:(::) _...] ->
        @fail(st, "keyword parameter with `...` may not be given a type")
    _ -> @fail(st, "expected identifier")
end

# note no return_ok in default val, unlike positional defaults, due to bugs
vst1_param_kw(vcx, st) = @stm (st=strip_arg_meta(st)) begin
    [:kw id val] ->
        vst1_param(vcx, id) & vst1(with(vcx; toplevel=false, in_gscope=false), val)
    [:... _...] ->
        @fail(st, "`...` may only be used for the final keyword parameter")
    _ -> vst1_param(vcx, st) |
        @fail(st, "malformed keyword parameter; expected identifier, `=`, or `::`")
end

vst1_typevar_decl(vcx, st) = @stm st begin
    [:identifier] -> vst1_ident(vcx, st; lhs=true)
    [:<: t old] ->
        vst1_ident(vcx, t; lhs=true) & vst1(vcx, old)
    [:>: t old] ->
        vst1_ident(vcx, t; lhs=true) & vst1(vcx, old)
    ([:comparison val_l [:identifier] t [:identifier] val_r],
     when=(syntax_name(st[2])===syntax_name(st[4]) && syntax_name(st[2]) in ("<:", ">:"))) ->
         vst1(vcx, val_l) &
         vst1_ident(vcx, t; lhs=true) &
         vst1(vcx, val_r)
    [:<: x _] ->
        @fail(x, "expected type name")
    [:>: x _] ->
        @fail(x, "expected type name")
    [:comparison _...] ->
        @fail(st, "expected `lb <: type_name <: ub` or `ub >: type_name >: lb`")
    _ -> @fail(st, "expected type name or type bounds")
end

vst1_typesig(vcx, st) = @stm st begin
    [:identifier] ->
        vst1_ident(vcx, st)
    [:curly t tvs...] ->
        vst1_ident(vcx, t) & all(vst1_typevar_decl, vcx, tvs)
    [:<: [:curly t tvs...] super] ->
        vst1_ident(vcx, t) & vst1(vcx, super) &
        all(vst1_typevar_decl, vcx, tvs)
    [:<: t super] ->
        vst1_ident(vcx, t) & vst1(vcx, super)
    _ -> @fail(st, "invalid type signature")
end

# normal, non-lhs curly may have implicit `(<: t)`
vst1_curly_typevar(vcx, st) = @stm st begin
    [:<: t] -> vst1_splat_or_val(vcx, t)
    [:>: t] -> vst1_splat_or_val(vcx, t)
    _ -> vst1_splat_or_val(vcx, st)
end

# assignment should never be allowed, but flisp fails to check inside blocks or
# after anything that isn't a field.  See #62075.
function _struct_noassign(vcx, body)
    for st in body
        if head(st) === :(=) && vst1_struct_field(vcx, st[1]).ok
            return @fail(st, "assignment syntax in structure fields is reserved")
        elseif !vst1_struct_field(vcx, st).ok
            return pass()
        end
    end
    return pass()
end

vst1_struct_arg(vcx, st) = @stm st begin
    [:block xs...] -> all(vst1_struct_arg, vcx, xs)
    _ -> vst1_struct_field(vcx, st) | vst1(vcx, st)
end

vst1_struct_field(vcx, st) = @stm st begin
    [:identifier] -> pass()
    [:(::) x t] -> vst1_struct_field(vcx, x) & vst1(vcx, t)
    [:const x] -> vst1_struct_field(vcx, x)
    [:atomic x] -> vst1_struct_field(vcx, x)
    _ -> unknown()
end

vst1_dotted_or_op_assign(vcx, st) = let
    @stm st begin
        [:.= l r] -> vst1_dotassign_lhs(vcx, l) & vst1(vcx, r)
        (_, when=(s = opeq_op(st); s !== nothing)) ->
            s[1] === '.' ?
            (vst1_dotassign_lhs(vcx, st[1]) & vst1(vcx, st[2])) :
            (vst1_assign_lhs(vcx, st[1]) & vst1(vcx, st[2]))
        _ -> unknown()
    end
end

vst1_assign(vcx, st; in_const = false) = @stm st begin
    # This case handles a proper function declaration (= (call ...) ...) form.
    # `vst1_assign_lhs_nontuple` also accepts call forms, but that is a lowering
    # bug where the "function body" is evaluated immediately
    ([:(=) l r], when=is_eventually_call(l)) -> vst1_function(vcx, st)
    [:(=) l r] -> vst1_assign_lhs(vcx, l; in_const) & vst1(vcx, r)
    [:(=) _...] -> @fail(st, "malformed assignment")
    _ -> unknown()
end

# TODO: We could do some destructuring checks here (e.g. fail `(a,b,c) = (1,2)`)
#
# syntax TODO:
# - call (only within a tuple using JuliaSyntax) can declare a function with
#   arguments, but can't use them on the rhs if in a tuple
# - in curly, typevars are checked for structure, but not used.
# - (local/global (= lhs rhs)) forms should probably reject the same
#   lhss as const (ref and .)
vst1_assign_lhs(vcx, st; in_const=false, in_tuple=false) = @stm st begin
    [:tuple [:parameters xs...]] -> all(vst1_symdecl, vcx, xs)
    [:tuple xs...] ->
        all(vst1_assign_lhs, vcx, xs; in_const, in_tuple=true) &
        (count(head(x)===:... for x in xs) <= 1 ? pass() :
        @fail(st, "multiple `...` in destructuring assignment are ambiguous"))
    # type-annotated tuple segfaults, haha
    # [:(::) [:tuple _...] t] -> ???
    [:... x] -> !in_tuple ?
        @fail(st, "splat on left side of assignment must be in a tuple") :
        vst1_assign_lhs_nontuple(vcx, x; in_const)
    ([:parameters _...], when=in_tuple) -> @fail(st, """
        property destructuring must use a single `;` before the property \
        names, e.g. `(; a, b) = rhs`""")
    _ -> vst1_assign_lhs_nontuple(vcx, st; in_const)
end
vst1_assign_lhs_nontuple(vcx, st; in_const=false, in_tuple=false) = @stm st begin
    [:ssavalue [:value]] -> in_const ? @fail(st, "cannot declare ssavalue const") : pass()
    (_, when=(is_eventually_call(st))) ->
        vst1_function_calldecl(vcx, st)
    [:(::) x t] ->
        vst1_assign_lhs(vcx, x; in_const, in_tuple) & vst1(vcx, t)
    [:. x y] ->
        in_const ? @fail(st, "cannot declare this form constant") :
        head(y) === :tuple ? @fail(st, "dotcall syntax not valid here") :
        vst1(vcx, x) & vst1(vcx, y)
    [:ref x is...] ->
        in_const ? @fail(st, "cannot declare this form constant") :
        vst1(vcx, x) & all(vst1_call_arg, vcx, is)
    [:curly x tvs...] ->
        vst1_ident(vcx, x; lhs=true) & all(vst1_typevar_decl, vcx, tvs)

    [:typed_hcat _...] ->
        @fail(st, "invalid spacing in left side of indexed assignment")
    [:typed_vcat _...] ->
        @fail(st, "unexpected `;` in left side of indexed assignment")
    [:typed_ncat _...] ->
        @fail(st, "unexpected `;` in left side of indexed assignment")
    (_, when=(head(st) in (:vect, :hcat, :vcat, :ncat))) ->
        @fail(st, "use `(a, b) = ...` to assign multiple values")
    _ -> @fail(st, "invalid syntax in left-hand side of assignment") |
        vst1_ident(vcx, st; lhs=true)
end

vst1_dotassign_lhs(vcx, st) = vst1_assign_lhs(vcx, st) | vst1(vcx, st)

# TODO: more validation is possible here, e.g. when row/nrow can show up in ncat
vst1_arraylike(vcx, st) = @stm st begin
    [:vect xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [:hcat xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [:vcat xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [:ncat [:value] xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [:ref x is...] -> vst1(vcx, x) & all(vst1_call_arg, vcx, is)
    [:row xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [:nrow [:value] xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [:typed_hcat t xs...] -> vst1(vcx, t) &
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [:typed_vcat t xs...] -> vst1(vcx, t) &
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [:typed_ncat t xs...] -> vst1(vcx, t) &
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    _ -> unknown()
end

function no_assignment(sl, hint="this expression")
    for st in sl
        if head(st) === :(=)
            return @fail(st, string(
                "assignment is not allowed in ", hint))
        end
    end
    return pass()
end

# If there is both a min and a max, prefer a finite number of match cases
function minlen(err_st::SyntaxTree, sl, n::Int)
    length(sl) >= n ? pass() :
        @fail(err_st, string(
            "expected at least ", n, " argument", (n === 1 ? "" : "s")))
end
function maxlen(err_st::SyntaxTree, sl, n::Int)
    length(sl) <= n ? pass() :
        @fail(err_st, string(
            "expected at most ", n, " argument", (n === 1 ? "" : "s")))
end

vst1_splat_or_val(vcx, st) = @stm st begin
    [:... x] -> vst1_splat_or_val(vcx, x)
    [:... _...] -> @fail(st, "expected one argument to `...`")
    _ -> vst1(vcx, st)
end

vst1_generator(vcx, st) = let
    vcx = with(vcx; return_ok=false, toplevel=false, in_gscope=false)
    @stm st begin
        [:generator _] -> @fail(st, "`generator` requires >=2 args")
        [:generator val [:filter cond is...]] ->
            vst1(vcx, val) &
            vst1(vcx, cond) &
            all(vst1_iter, vcx, is)
        [:generator val is...] ->
            vst1(with(vcx; readable_underscore=true), val) &
            all(vst1_iter, vcx, is)
        [:generator _...] -> @fail(st, "malformed `generator`")
        _ -> @fail(st, "expected `generator`")
    end
end

vst1_iter(vcx, st) = @stm st begin
    [:(=) [:outer i] v] -> vst1_assign_lhs(vcx, i) & vst1(vcx, v)
    # rare, malformed, happens to work in desugaring
    [:(=) i [:... v]] -> vst1_assign_lhs(vcx, i) & vst1(vcx, v)
    [:(=) i v] -> vst1_assign_lhs(vcx, i) & vst1(vcx, v)
    _ -> @fail(st, "expected one of `=`, `in`, `∈`")
end

vst1_raw_lambda(vcx, st) = @stm st begin
    [:lambda [:value] body] -> let args = st[1].value
        (args isa Vector && all(a->a isa Symbol, args) ? pass() :
        @fail(st[1], "expected Vector of Symbol")) &
        vst1(with(vcx; return_ok=true, toplevel=false, in_gscope=false), body)
    end
    [:lambda _...] -> @fail(st, "malformed `lambda`")
    _ -> @fail(st, "expected `lambda`")
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
        isempty(err.sts) || !(head(err.sts[1]) === :macrocall)
    end
    vr2 = ValidationResult(isempty(vr2_errors), vr2_errors)
    return vr2.ok
end

vst0(_, st) = vst1(with(Validation1Context(), unexpanded=true), st)

"""
TODO: While we can't validate any arguments to a macrocall in general, it would
make sense to check usage for things like @ccall and @doc.
"""
vst0_macrocall(vcx, st) = @stm st begin
    (_, when=!vcx.unexpanded) ->
        @fail(st, "macrocall not valid in AST after macro expansion")
    ([:macrocall name [:value] args...],
     when=(typeof(st[2].value) in (LineNumberNode, MacroSource))) ->
         pass()
    [:macrocall _...] ->
        @fail(st, "expected (macrocall name linenode args...)")
    _ -> @fail(st, "invalid macrocall syntax")
end

vst0_quoted(vcx, st; quote_level) = @stm st begin
    ([:$ x], when=quote_level===1) ->
        vst1_splat_or_val(vcx, x)
    [:$ x] ->
        vst0_quoted(vcx, x; quote_level=quote_level-1)
    [:quote x] ->
        vst0_quoted(vcx, x; quote_level=quote_level+1)
    _ -> all(vst0_quoted, vcx, children(st); quote_level)
end

#-------------------------------------------------------------------------------
# Tree invariants assumed everywhere, including `show`, so fallback printing
# should be used on failure.  (These checks really belong in the type system.)
# Failure should only be possible working on AST-internal functions.

function assert_syntaxtree(st::SyntaxTree, recursive=true)
    vr = recursive ? _assert_syntaxtree(st, SyntaxTree[], pass()) :
        _assert_syntaxtree_node(st)
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

function _assert_syntaxtree_node(st::SyntaxTree)
    vr = pass()
    # TODO: assert st has context (parser doesn't add any)
    if is_leaf(st)
        if head(st) === :globalref && st.mod === nothing
            vr &= @fail(st, "leaf globalref requires module in .mod")
        end
        (needs_val, valtype) = @stm st begin
            [:identifier] -> (true,String)
            [:value] -> (true,Any)
            [:core] -> (true,String)
            [:top] -> (true,String)
            [:symbol] -> (true,String)
            [:globalref] -> (true,String)
            [:placeholder] -> (false, Any)
            [:bindingid] -> (true,IdTag)
            [:label] -> (true,Int)
            [:symboliclabel] -> (true,String)
            [:symbolicgoto] -> (true,String)
            [:slot] -> (true,Int)
            [:static_parameter] -> (true,Int)
            [:ssavalue] -> (true,Int)
            [:nothing] -> (false, Any)
            [:tombstone] -> (false, Any)
            [:sourcelocation] -> (false, Any)
            [:latestworld] -> (false, Any)
            [:latestworld_if_toplevel] -> (false, Any)
            (_, when=JuliaSyntax.is_trivia(st)) -> (false, Any) # green tree only
            [:strmacroname] -> (true,String)
            [:cmdmacroname] -> (true,String)
            [:lambdabindings] -> (true,LambdaBindings)
            [:slots] -> (true,Vector{Slot})
            [:version] -> (true,VersionNumber)
            _ -> return vr & @fail(st, "unrecognized leaf kind $(head(st))")
        end
        if needs_val
            if !(st.value isa valtype)
                vr &= @fail(st, "needs value ::"*string(valtype))
            end
        end
    else
        # Note some kinds can show up as non-leaves too (mostly from Expr)
        if head(st) in (:identifier, :value, :placeholder, :bindingid, :label, :symbol,
                        :nothing, :tombstone, :sourcelocation,
                        :lambdabindings, :slots)
            vr &= @fail(st, "Found leaf-only kind with children")
        end
    end
    vr
end

function _assert_syntaxtree(st::SyntaxTree, parents::Vector{SyntaxTree}, vr)
    if st in parents
        err = "cycle detected: ["
        for p in parents
            err *= "\n" * node_string(p)
        end
        return vr & @fail(st, err*"]")
    end
    vr &= _assert_syntaxtree_node(st)
    # TODO: Proper traversal along .source and macro prov (need to cache results
    # to avoid exponential repeated lookups, and figure out how these edges may
    # form cycles with child edges)
    st.source === st && (vr &= @fail(st, ".source equal to self ID"))
    sc = st.context
    sc.unexpanded === st && (vr &= @fail(st, "unexpanded equal to self"))

    push!(parents, st)
    is_leaf(st) || for c in children(st)
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

vst2(vcx::Validation2Context, st::SyntaxTree) = @stm st begin
    (_, when=is_leaf(st)) -> head(st) in (
        :identifier, :bindingid, :placeholder, :nothing, :static_parameter,
        :sourcelocation, :symbol, :value, :core, :top,
        :latestworld, :latestworld_if_toplevel, :symbolicgoto, :symboliclabel,
        :tombstone) ? pass() : @fail(st, "unrecognized leaf kind $(head(st))")

    [:call [:static_eval cg] xs...] -> est_syntax_name(cg, "") === "cglobal" ?
        all(vst2, vcx, xs) : @fail(st, "expected (call (static_eval cglobal) _...)")
    [:call xs...] -> all(vst2, vcx, xs)
    [:block xs...] -> all(vst2, vcx, xs)
    [:scope_block [:neutral_scope] xs...] -> all(vst2, vcx, xs)
    [:scope_block [:hard_scope] xs...] -> all(vst2, vcx, xs)
    [:(=) l r] -> vst2_ident_lhs(vcx, l) & vst2(vcx, r)
    [:assign_or_constdecl_if_global l r] -> vst2_ident_lhs(vcx, l) & vst2(vcx, r)
    [:global_if_global x] -> vst2_ident_lhs(vcx, x)
    # declare undefined constant
    [:constdecl l] -> vst2_ident_lhs(vcx, l)
    # declare and assign constant
    [:constdecl l r] -> vst2_ident_lhs(vcx, l) & vst2(vcx, r)
    [:global x] -> vst2_ident_lhs(vcx, x)
    [:local x] -> vst2_ident_lhs(vcx, x)
    [:decl x t] -> vst2_ident(vcx, x) & vst2(vcx, t)
    [:if cond t] -> vst2(vcx, cond) & vst2(vcx, t)
    [:if cond t f] -> vst2(vcx, cond) & vst2(vcx, t) & vst2(vcx, f)
    [:elseif cond t] -> vst2(vcx, cond) & vst2(vcx, t)
    [:elseif cond t f] -> vst2(vcx, cond) & vst2(vcx, t) & vst2(vcx, f)
    [:&& xs...] -> all(vst2, vcx, xs)
    [:|| xs...] -> all(vst2, vcx, xs)
    [:symbolicblock [:symboliclabel] body] -> vst2(vcx, body)
    [:break [:symboliclabel]] -> pass()
    [:break [:symboliclabel] x] -> vst2(vcx, x)
    [:return x] -> vst2(vcx, x)
    [:trycatchelse t c] -> vst2(vcx, t) & vst2(vcx, c)
    [:trycatchelse t c e] -> vst2(vcx, t) & vst2(vcx, c) & vst2(vcx, e)
    [:tryfinally t f] -> vst2(vcx, t) & vst2(vcx, f)
    [:tryfinally t f scope] -> vst2(vcx, t) & vst2(vcx, f) & vst2(vcx, scope)
    [:_opaque_closure id argt lb ub partial nargs isva src lam] ->
        vst2_ident(vcx, id) &
        all(vst2, vcx, children(st)[2:end-1]) &
        vst2_lam(vcx, lam)
    [:_do_while body cond] -> vst2(vcx, body) & vst2(vcx, cond)
    [:_while cond body] -> vst2(vcx, cond) & vst2(vcx, body)
    [:inert _] -> pass()
    [:syntaxinert _] -> pass()
    [:lambda _...] -> vst2_lam(vcx, st)
    # Declare a zero-method generic function with global `name` or creates a
    # closure object and assigns it to the local `name`.
    [:function_decl x] -> vst2_ident(vcx, x)
    # Evaluates to the type of the function or closure with given `name`
    [:function_type x] -> vst2(vcx, x)
    [:method mtable argtypes lam] -> !vcx.in_method_defs ?
        @fail(st, "method outside of method_defs") :
        (head(mtable) === :nothing ? pass() : vst2(vcx, mtable)) &
        vst2(vcx, argtypes) & vst2_lam(vcx, lam)
    # The code in `body` defines methods for generic function `name`.  If
    # non-toplevel, all contained methods share a closure type.  `tvs` are
    # assigned-once top-level locals only referenced inside `:method`, but
    # outside of `:lambda`, since any reference inside a lambda should resolve
    # to the lambda's sparam shadowing it.
    [:method_defs id [:block tvs...] body] ->
        (head(id) === :nothing ? pass() : vst2_ident_val(vcx, id)) &
        all(vst2_typevar, vcx, tvs) & vst2(with(vcx; in_method_defs=true), body)
    # from `function f end`, tells closure conversion to give f its value
    # (usually done with method_defs)
    [:no_method_defs id] -> vst2_ident_val(vcx, id)
    [:new t args...] -> vst2(vcx, t) & all(vst2, vcx, args)
    [:splatnew t arg] -> vst2(vcx, t) & vst2(vcx, arg)
    [:softscope] -> pass()
    [:softscope _] -> pass()
    [:thisfunction] -> pass()
    [:gc_preserve_begin xs...] -> all(vst2_ident, vcx, xs)
    [:gc_preserve_end xs...] -> minlen(st, xs, 1) & all(vst2_ident, vcx, xs)

    [:meta xs...] -> all(vst2, vcx, xs) # TODO
    [:loopinfo xs...] -> all(vst2, vcx, xs) # TODO
    [:boundscheck] -> pass()
    [:inbounds_pop] -> pass()
    ([:inbounds [:value]], when=(st[1].value isa Bool)) -> pass()
    ([:inline [:value]], when=(st[1].value isa Bool)) -> pass()
    ([:noinline [:value]], when=(st[1].value isa Bool)) -> pass()
    [:purity] -> pass()
    [:purity _ _...] -> numchildren(st) == fieldcount(Base.EffectsOverride) ?
        pass() : @fail(st, "wrong number of args to `purity` expression")
    [:aliasscope] -> pass()
    [:popaliasscope] -> pass()

    # Note to variable analysis that x is always defined before use
    [:always_defined x] -> vst2_ident(vcx, x)
    # Lowering-internal assertion
    [:assert [:symbol] x] -> vst2(vcx, x)
    # The contained block of code causes no side effects and can be removed by a
    # later lowering pass if its value isn't used
    [:removable x] -> vst2(vcx, x)
    # `(relayered_global old::identifier)` is used to tell scope
    # resolution that any declaration conflicting with `(global old)`
    # should fail, even though `old` was never actually declared
    [:relayered_global [:identifier]] -> pass()

    # Could be made stricter
    [:foreigncall _ [:static_eval rt] [:static_eval at] cconv roots_args...] ->
         vst2(vcx, rt) &
         vst2(vcx, at) &
         vst2(vcx, cconv) &
         all(vst2, vcx, roots_args)
    [:foreignglobal _] -> pass()
    [:cfunction [:value] [:static_eval fptr] [:static_eval rt] [:static_eval at] [:symbol]] ->
         vst2(vcx, fptr) & vst2(vcx, rt) & vst2(vcx, at)
    [:cfunction [:value] fptr [:static_eval rt] [:static_eval at] [:symbol]] ->
         vst2(vcx, fptr) & vst2(vcx, rt) & vst2(vcx, at)

    [:isdefined x] -> vst2_ident_val(vcx, x)
    [:isglobal [:placeholder]] -> pass()
    [:islocal [:placeholder]] -> pass()
    [:isglobal x] -> vst2_ident_val(vcx, x)
    [:islocal x] -> vst2_ident_val(vcx, x)
    [:locals] -> pass()
    _ -> @fail(st, "unrecognized form out of desugaring")
end

vst2_ident_lhs(vcx, st) = @stm st begin
    [:identifier] -> pass()
    [:bindingid] -> pass()
    [:placeholder] -> pass()
    _ -> @fail(st, "expected identifier (lhs)")
end

vst2_ident(vcx, st) = @stm st begin
    [:identifier] -> pass()
    [:bindingid] -> pass()
    _ -> @fail(st, "expected identifier or BindingId")
end

vst2_ident_val(vcx, st) = @stm st begin
    [:identifier] -> pass()
    [:bindingid] -> pass()
    [:core] -> pass()
    [:top] -> pass()
    [:thisfunction] -> pass()
    [:static_parameter] -> pass()
    _ -> @fail(st, "expected identifier (val)")
end

vst2_lam(vcx, st) = @stm st begin
    [:lambda [:block args...] [:block sps...] body] ->
        all(vst2_ident_lhs, vcx, args) &
        all(vst2_ident_lhs, vcx, sps) &
        vst2(vcx, body)
    [:lambda [:block args...] [:block sps...] body rett] ->
        all(vst2_ident_lhs, vcx, args) &
        all(vst2_ident_lhs, vcx, sps) &
        vst2(vcx, body) &
        vst2(vcx, rett)
    _ -> @fail(st, "malformed lambda")
end

vst2_typevar(vcx, st) = @stm st begin
    [:typevar tv val] -> vst2_ident_lhs(vcx, tv) & vst2(vcx, val)
    _ -> @fail(st, "malformed sparam")
end
