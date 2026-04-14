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

vst1(vcx::Validation1Context, st::SyntaxTree)::ValidationResult = @stm st begin
    [K"Identifier"] -> vst1_ident(vcx, st)
    (_, when=is_expr_value(st)) -> pass()
    [K"block" xs...] -> all(vst1, vcx, xs)
    [K"let" [K"block" decls...] body] ->
        all(vst1_symdecl_or_assign, vcx, decls) &
        vst1(with(vcx; in_gscope=false), body)
    [K"let" decl body] ->
        vst1_symdecl_or_assign(vcx, decl) &
        vst1(with(vcx; in_gscope=false), body)
    [K"if" _...] -> let
        inner_vcx = vcx.toplevel ? with(vcx; inner_cond=true) : vcx
        @stm st begin
            [K"if" [K"generated"] t f] ->
                vst1(inner_vcx, t) & vst1_else(inner_vcx, f)
            [K"if" [K"generated"] _...] ->
                @fail(st, "if-generated requires both true and false cases")
            [K"if" cond t] ->
                vst1(vcx, cond) & vst1(inner_vcx, t)
            [K"if" cond t f] ->
                vst1(vcx, cond) & vst1(inner_vcx, t) & vst1_else(inner_vcx, f)
            _ -> @fail(st, "expected (if cond body) or (if cond body else)")
        end
    end
    [K"try" _...] -> vst1_try(vcx, st)
    [K"function" _...] -> vst1_function(vcx, st)
    [K"call" _...] -> vst1_call(vcx, st)
    [K"'" x] -> vst1(vcx, x)
    [K"." f [K"tuple" _...]] -> vst1_dotcall(vcx, st)
    [K"." l r] -> vst1(vcx, l) & vst1_dot_getproperty_rhs(vcx, r)
    [K"." x] -> vst1(vcx, x) # BroadcastFunction(x)
    [K"do" call lam] ->
        (vst1_call(vcx, call) | vst1_dotcall(vcx, call) | vst0_macrocall(vcx, call)) &
        vst1_lam(vcx, lam)
    [K"=" _...] -> vst1_assign(vcx, st)
    (_, when=(vr=vst1_dotted_or_op_assign(vcx, st); is_known(vr))) -> vr
    [K"return" val] -> vcx.return_ok ?
        vst1(vcx, val) :
        @fail(st, "`return` not allowed inside comprehension or generator")
    ([K"continue"], when=vcx.in_loop) -> pass()
    ([K"continue" lab], when=vcx.in_loop) -> vst1_ident(vcx, lab; lhs=true)
    ([K"break"], when=vcx.in_loop) -> pass()
    ([K"break" lab], when=vcx.in_loop||vcx.in_symblock) ->
        vst1_ident(vcx, lab; lhs=true)
    ([K"break" lab x], when=vcx.in_loop||vcx.in_symblock) ->
        vst1_ident(vcx, lab; lhs=true) & vst1(vcx, x)
    [K"for" [K"block" is...] body] ->
        all(vst1_iter, vcx, is) &
        vst1(with(vcx; in_loop=true, in_gscope=false), body)
    [K"for" iter1 body] ->
        vst1_iter(vcx, iter1) &
        vst1(with(vcx; in_loop=true, in_gscope=false), body)
    [K"while" cond body] ->
        vst1(vcx, cond) &
        vst1(with(vcx; in_loop=true, in_gscope=false), body)
    [K"tuple" _...] ->
        vst1_tuple(vcx, st)
    [K"curly" t tvs...] ->
        vst1(vcx, t) & no_assignment(tvs, "type parameter list") &
        all(vst1_curly_typevar, vcx, tvs)
    [K"where" t tds...] ->
        vst1(vcx, t) & all(vst1_typevar_decl, vcx, tds)
    [K"string" xs...] ->
        all(vst1_splat_or_val, vcx, xs)
    [K"->" _...] ->
        vst1_lam(vcx, st)
    [K"flatten" g] -> vst1_generator(vcx, g)
    [K"generator" _...] -> vst1_generator(vcx, st)
    [K"comprehension" [K"flatten" g]] -> vst1_generator(vcx, g)
    [K"comprehension" g] -> vst1_generator(vcx, g)
    [K"typed_comprehension" t [K"flatten" g]] ->
        vst1(vcx, t) & vst1_generator(vcx, g)
    [K"typed_comprehension" t g] ->
        vst1(vcx, t) & vst1_generator(vcx, g)
    [K"comparison" xs...] ->
        length(xs) < 3 || iseven(length(xs)) ?
        @fail(st, "`comparison` expects n>=3 args and odd n") :
        # TODO: can we restrict xs[2:2:end] to identifier or .identifier?
        all(vst1, vcx, xs[2:2:end]) &
        all(vst1, vcx, xs[1:2:end])
    [K"<:" x] -> vst1(vcx, x)
    [K">:" x] -> vst1(vcx, x)
    [K"<:" x y] -> vst1(vcx, x) & vst1(vcx, y)
    [K">:" x y] -> vst1(vcx, x) & vst1(vcx, y)
    [K"-->" xs...] -> all(vst1, vcx, xs)
    [K"::" x y] -> vst1(vcx, x) & vst1(vcx, y)
    # TODO: inner_cond on args[2:end]
    [K"&&" xs...] -> all(vst1, vcx, xs)
    [K"||" xs...] -> all(vst1, vcx, xs)
    [K".&&" x y] -> vst1(vcx, x) & vst1(vcx, y)
    [K".||" x y] -> vst1(vcx, x) & vst1(vcx, y)
    (_, when=(vr=vst1_arraylike(vcx, st); is_known(vr))) -> vr
    # syntax TODO: disallow pre-desugared const, broken with complex rhs
    [K"const" l r] -> vst1_ident(vcx, l; lhs=true) & vst1(vcx, r)
    [K"const" [K"global" x]] -> !vcx.toplevel ?
        @fail(st, "unsupported `const` inside function") :
        vst1_const_assign(vcx, x)
    [K"const" x] ->  !vcx.toplevel ?
        @fail(st, "unsupported `const` inside function") :
        vst1_const_assign(vcx, x)
    [K"global" xs...] -> minlen(st, xs, 1) & all(vst1_global_arg, vcx, xs)
    [K"local" xs...] -> minlen(st, xs, 1) & all(vst1_local_arg, vcx, xs)
    [K"macrocall" _...] -> vst0_macrocall(vcx, st)
    [K"quote" x] -> vcx.unexpanded ? vst0_quoted(vcx, x; quote_level=1) :
        @fail(st, "interpolating quote not valid syntax after macro expansion")

    #---------------------------------------------------------------------------
    # Forms not produced by the parser
    [K"ssavalue" [K"Value"]] -> pass()
    [K"inert" _] -> pass()
    [K"inert_syntaxtree" _] -> pass()
    [K"core"] -> st.name_val === "nothing" ? pass() :
        @fail(st, "zero-arg `core` is only used for `Core.nothing`")
    [K"core" [K"Identifier"]] -> pass()
    [K"top" [K"Identifier"]] -> pass()
    [K"meta" _...] -> pass() # TODO
    [K"toplevel" xs...] -> pass() # this will be validated when we lower it
    [K"opaque_closure" argt lb ub bool lam] ->
        all(vst1, vcx, [argt, lb, ub, bool]) & vst1_lam(vcx, lam)
    [K"symboliclabel" lab] -> vst1_ident(vcx, lab; lhs=true)
    [K"symbolicgoto" lab] -> vst1_ident(vcx, lab; lhs=true)
    [K"oldsymbolicgoto" lab] -> vst1_ident(vcx, lab; lhs=true)
    [K"symbolicblock" lab body] ->
        vst1_ident(vcx, lab; lhs=true) & vst1(with(vcx; in_symblock=true), body)
    [K"gc_preserve" x ids...] -> vst1(vcx, x) & all(vst1_ident, vcx, ids)
    # lowering TODO: 0 args segfaults
    [K"gc_preserve_begin" ids...] -> all(vst1_ident, vcx, ids)
    [K"gc_preserve_end" ids...] -> all(vst1_ident, vcx, ids)
    [K"isdefined" [K"Identifier"]] -> pass()
    [K"lambda" [K"block" b1...] [K"block" b2...] _] ->
        all(vst1_ident, vcx, b1) &
        all(vst1_ident, vcx, b2) &
        (kind(st[3]) === K"->" ? vst1_lam(vcx, st[3]) :
            vst1(with(vcx; return_ok=true, toplevel=false, in_gscope=false), st[3]))
    [K"softscope" _] -> pass()
    [K"softscope"] -> pass()
    [K"generated"] -> pass()
    [K"foreigncall" fname rt at cconv roots_args...] ->
        # TODO: could be stricter
        vst1(vcx, fname) &
        vst1(vcx, rt) &
        vst1(vcx, at) &
        vst1(vcx, cconv) &
        all(vst1, vcx, roots_args)
    [K"cfunction" [K"Value"] f rt at [K"inert" [K"Identifier"]]] ->
        vst1(vcx, f) & vst1(vcx, rt) & vst1(vcx, at)
    [K"cconv" tup nreq] -> (get(tup, :value, nothing) isa Tuple &&
        get(nreq, :value, nothing) isa Int) ? pass() :
        @fail(st, "expected (cconv convention_tuple n_req_args)")
    [K"tryfinally" t f] -> vst1(vcx, t) & vst1(vcx, f)
    [K"tryfinally" t f scope] -> vst1(vcx, t) & vst1(vcx, f) & vst1(vcx, scope)
    [K"inline" _] -> pass()
    [K"noinline" _] -> pass()
    [K"inbounds" _] -> pass()
    [K"boundscheck"] -> pass()
    [K"boundscheck" _] -> pass()
    [K"loopinfo" _...] -> pass()
    [K"locals"] -> pass()
    [K"islocal" _] -> pass()
    [K"isglobal" _] -> pass()
    [K"copyast" [K"inert" _]] -> pass()
    [K"new" t args...] -> vst1(vcx, t) & all(vst1, vcx, args)
    [K"splatnew" t arg] -> vst1(vcx, t) & vst1(vcx, arg)
    [K"thisfunction"] -> vcx.toplevel ?
        @fail(st, "can only be used inside a function") :
        !vcx.return_ok ?
        @fail(st, "current function not defined in comprehension or generator") : pass()
    [K"unknown_head"] -> let head = st.name_val
        head === "latestworld-if-toplevel" ? pass() :
            @fail(st, string("unknown expr head: ", head))
    end

    #---------------------------------------------------------------------------
    # Invalid forms for which we want to produce detailed errors
    [K"..." _...] ->
        @fail(st, "unexpected `...`\nsplatting can only be done into a `call`, `tuple`, `curly`, or array-like expression")
    [K"parameters" _...] ->
        @fail(st, "unexpected semicolon")
    [K"braces" _...] ->
        @fail(st, "`{ }` outside of `where` is reserved for future use")
    [K"bracescat" _...] ->
        @fail(st, "`{ }` outside of `where` is reserved for future use")
    [K"atomic" _...] ->
        @fail(st, "unimplemented or unsupported `atomic` declaration")
    [K"::" x] ->
        @fail(st, "`::` must be written `value::type` outside function argument lists")
    # internal
    [K"Symbol"] ->
        @fail(st, "`Symbol` kind not valid until desugaring")
    [K"Placeholder"] ->
        @fail(st, "`Placeholder` kind not valid until desugaring")
    [K"unknown_head" _...] ->
        @fail(st, string("unknown expr head: ", st.name_val))
    [K"$" x] -> @fail(st, raw"`$` expression outside string or quote")
    [K"continue" _...] ->
        @fail(st, "`continue` outside of a `while` or `for` loop")
    [K"break"] ->
        @fail(st, "unlabeled `break` outside of a `while` or `for` loop")
    [K"break" _...] ->
        @fail(st, "labeled `break` outside of loop or symbolic block")
    _ -> let top_vr = vst1_toplevel_only(vcx, st)
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

vst1_toplevel_only(vcx, st) = @stm st begin
    # body will be validated when lowered
    [K"module" [K"Value"] [K"Value"] [K"Identifier"] [K"block" xs...]] ->
        !(st[1].value isa VersionNumber) ? @fail(st[1], "expected version") :
        !(st[2].value isa Bool) ? @fail(st[2], "expected boolean bare flag") :
        pass()
    [K"module" [K"Value"] [K"Identifier"] [K"block" xs...]] ->
        !(st[1].value isa Bool) ? @fail(st[1], "expected boolean bare flag") :
        pass()
    [K"macro" _...] ->
        vst1_macro(vcx, st)
    [K"struct" [K"Value"] sig [K"block" body...]] ->
        vst1_typesig(vcx, sig) & (
            !(st[1].value isa Bool) ? @fail(st[1], "expected mutable flag") :
                all(vst1_struct_arg, vcx, body))
    [K"abstract" sig] ->
        vst1_typesig(vcx, sig)
    [K"primitive" sig n] ->
        vst1_typesig(vcx, sig) & vst1(vcx, n)
    [K"import" [K":" p1 ps...]] ->
        (vst1_importpath(vcx, p1; dots_ok=true) &
        all(vst1_importpath, vcx, ps; dots_ok=false))
    [K"using"  [K":" p1 ps...]] ->
        (vst1_importpath(vcx, p1; dots_ok=true) &
        all(vst1_importpath, vcx, ps; dots_ok=false))
    [K"import" ps...] ->
        minlen(st, ps, 1) & all(vst1_importpath, vcx, ps; dots_ok=true)
    [K"using"  ps...] ->
        minlen(st, ps, 1) & all(vst1_importpath, vcx, ps; dots_ok=true)
    [K"public" xs...] -> all(vst1_ident, vcx, xs)
    [K"export" xs...] -> all(vst1_ident, vcx, xs)
    [K"latestworld"] -> pass()
    [K"typegroup" [K"block" xs...]] ->
        all(vst1, vcx, xs)
    _ -> unknown()
end

#-------------------------------------------------------------------------------

vst1_local_arg(vcx, st) = @stm st begin
    [K"function" _...] -> vst1_function(vcx, st)
    _ -> vst1_symdecl_or_assign(vcx, st) | vst1_dotted_or_op_assign(vcx, st) |
        @fail(st, "invalid local declaration: expected identifier or assignment")
end

vst1_global_arg(vcx, st) = @stm st begin
    [K"function" _...] -> vcx.toplevel ?
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
        [K"as" [K"." xs...] [K"Identifier"]] -> xs
        [K"as" [K"." xs...] x] -> (ok &= @fail(x, "expected identifier"); xs)
        [K"." xs...] -> xs
    end
    seen_first = false
    for c in path_components
        if kind(c) === K"."
            if !dots_ok || seen_first
                ok &= @fail(c, "unexpected `.` in import path")
            end
            continue
        end
        ok = ok & vst1_ident(vcx, seen_first && kind(c) === K"quote" ? c[1] : c)
        seen_first = true
    end
    return !seen_first ?
        @fail(st, "expected identifier in `importpath`") : ok
end

vst1_tuple(vcx, st) = @stm st begin
    [K"tuple" [K"parameters" kws...]] -> all(vst1_call_kwarg, vcx, kws)
    [K"tuple" [K"parameters" _ _...] _ _...] -> @fail(
        st[1], "cannot mix tuple `(a,b,c)` and named tuple `(;a,b,c)` syntax")
    ([K"tuple" args...], when=any(x->kind(x)===K"=", args)) ->
        all(vst1_call_arg, vcx, args)
    [K"tuple" xs...] -> all(vst1_splat_or_val, vcx, xs)
    _ -> @fail(st, "malformed tuple")
end

vst1_else(vcx, st) = @stm st begin
    [K"elseif" cond t] -> vst1(vcx, cond) &
        vst1(vcx, t)
    [K"elseif" cond t f] -> vst1(vcx, cond) &
        vst1(vcx, t) &
        vst1_else(vcx, f)
    _ -> vst1(vcx, st)
end

# TODO: disallow (has-unmatched-symbolic-goto? tryb)
vst1_try(vcx, st) = @stm st begin
    [K"try" _] -> @fail(st, "try without catch or finally")
    [K"try" tryb cvar catchb] ->
        vst1(vcx, tryb) &
        vst1_try_catchvar(vcx, cvar) &
        vst1(vcx, catchb)
    [K"try" tryb cvar catchb finallyb] ->
        vst1(vcx, tryb) &
        vst1_try_catchvar(vcx, cvar) &
        vst1(vcx, catchb) &
        vst1(vcx, finallyb)
    [K"try" tryb cvar catchb finallyb elseb] ->
        vst1(vcx, tryb) &
        vst1_try_catchvar(vcx, cvar) &
        vst1(vcx, catchb) &
        vst1(vcx, finallyb) &
        vst1(vcx, elseb)
    _ -> @fail(st, "malformed `try` expression")
end

vst1_try_catchvar(_vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    ([K"Value"], when=st.value===false) -> pass()
end

# syntax TODO:
# - const is inoperative in the function case
# - single-arg const with no value (presumably to poison this name) was likely
#   not intended to work, and can only be produced by macros
vst1_const_assign(vcx, st) = @stm st begin
    [K"=" _ _] -> vst1_assign(vcx, st; in_const=true)
    [K"Identifier"] -> pass()
    [K"local" _...] -> @fail(st, "unsupported `const local` declaration")
    _ -> @fail(st, "expected assignment after `const`")
end

# syntax TODO: all-underscore variables may be read from with dot syntax
vst1_dot_getproperty_rhs(vcx, st) = @stm st begin
    [K"inert" x] -> pass()
    [K"inert_syntaxtree" x] -> pass()
    [K"Identifier"] -> pass()
    (_, when=is_expr_value(st)) -> pass()
    _ -> @fail(st, "invalid `.` syntax")
end

# We can't validate A.B in general (usually lowers to getproperty), but it shows
# up in a number of syntax special cases where we can. (flisp: sym-ref?)
vst1_calldecl_dot_name(vcx, st) = @stm st begin
    [K"." l r] ->
        vst1_calldecl_dot_name(vcx, l) &
        vst1_dot_definition_rhs(vcx, r) |
        @fail(st, "invalid `.` form")
    [K"Value"] -> pass()
    i -> vst1_ident(vcx, i)
end

vst1_dot_definition_rhs(vcx, st) = @stm st begin
    [K"inert" x] -> vst1_dot_definition_rhs(vcx, x)
    [K"inert_syntaxtree" x] ->  vst1_dot_definition_rhs(vcx, x)
    [K"Identifier"] -> vst1_ident(vcx, st; lhs=true)
    ([K"Value"], when=st.value isa String) -> _ident_str(vcx, st, st.value; lhs=true)
    [K"String"] -> _ident_str(vcx, st, st.value; lhs=true)
    [K"tuple" _...] -> @fail(st, "dotcall syntax not valid here")
    _ -> @fail(st, "invalid `.` syntax")
end

vst1_symdecl_or_assign(vcx, st) =
    @fail(st, "expected identifier or assignment") |
    vst1_symdecl(vcx, st) | vst1_assign(vcx, st)

vst1_symdecl(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"::" [K"Identifier"] t] -> vst1(vcx, t)
    _ -> @fail(st, "expected identifier or `identifier::type`")
end

# TODO: globalref (identifier with .mod) might not be valid everywhere; check
# usage of this function
vst1_ident(vcx, st; lhs=false) = @stm st begin
    [K"Identifier"] -> _ident_str(vcx, st, st.name_val; lhs)
    _ -> @fail(st, "expected identifier")
end
function _ident_str(vcx, st, s::String; lhs=false)
    if !lhs && all(==('_'), s) && !vcx.readable_underscore
        @fail(st, "all-underscore identifiers are write-only and their values cannot be used in expressions")
    elseif lhs && s in ("ccall", "cglobal")
        @fail(st, string(s, " is a reserved identifier"))
    else
        pass()
    end
end

vst1_call(vcx, st) = @stm st begin
    ([K"call" [K"Identifier"] args...], when=st[1].name_val==="cglobal") ->
        (1 <= length(args) <= 2 ? pass() :
            @fail(st, "cglobal must have one or two arguments")) &
        all(vst1_call_arg, vcx, args)
    [K"call" f [K"parameters" kwargs...] args...] ->
        (vst1_ident(vcx, f) | vst1(vcx, f)) &
        all(vst1_call_arg, vcx, args) &
        all(vst1_call_kwarg, vcx, kwargs)
    [K"call" f args...] ->
        (vst1_ident(vcx, f) | vst1(vcx, f)) &
        all(vst1_call_arg, vcx, args)
    [K"call" _...] -> @fail(st, "malformed `call`")
    _ -> unknown()
end

vst1_dotcall(vcx, st) = @stm st begin
    [K"." f [K"tuple" [K"parameters" kwargs...] args...]] ->
        vst1(vcx, f) & all(vst1_call_kwarg, vcx, kwargs) &
        all(vst1_call_arg, vcx, args)
    [K"." f [K"tuple" args...]] ->
        vst1(vcx, f) & all(vst1_call_arg, vcx, args)
    _ -> unknown()
end

# Arg to call (not function decl), pre-semicolon.  This can be anything, but
# additionally allow `kw` and `...` forms.
vst1_call_arg(vcx, st) = @stm st begin
    [K"kw" id val] -> vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    _ -> vst1_splat_or_val(vcx, st)
end

# Arg to `parameters` (post-semicolon) in a call (not function decl).  Stricter
# than `vst1_call_arg`.  `=` desugars to `kw`.
vst1_call_kwarg(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"kw" id val] -> vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    [K"=" id val] -> vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    [K"..." x] -> vst1(vcx, x)
    [K"." x [K"inert" id]] -> vst1(vcx, x) & vst1_ident(vcx, id; lhs=true)
    [K"." x [K"inert_syntaxtree" id]] -> vst1(vcx, x) & vst1_ident(vcx, id; lhs=true)
    ([K"call" [K"Identifier"] symval v], when=(st[1].name_val==="=>")) ->
        vst1(vcx, symval) & vst1(vcx, v)
    _ -> @fail(st, "expected identifier, `=`, or, `...` after semicolon")
end

vst1_lam(vcx, st) = let
    f_vcx = with(vcx; return_ok=true, toplevel=false, in_gscope=false)
    @stm st begin
        [K"->" l r] ->
            vst1_lam_lhs(with(f_vcx; return_ok=false), l) & vst1(f_vcx, r)
        _ -> @fail(st, "expected `->` expression")
    end
end

vst1_lam_lhs(vcx, st) = @stm st begin
    [K"tuple" [K"parameters" _...] ps...] ->
        _calldecl_positionals(vcx, ps, true) & vst1_calldecl_kws(vcx, st[1])
    [K"tuple" ps...] ->
        _calldecl_positionals(vcx, ps, true)
    [K"where" ps tds...] ->
        vst1_lam_lhs(vcx, ps) & all(vst1_typevar_decl, vcx, tds)
    # syntax TODO: This is handled badly in the parser
    [K"block"] -> pass()
    [K"block" x] -> _calldecl_positionals(vcx, SyntaxList(x), true)
    [K"block" x p] -> _calldecl_positionals(vcx, SyntaxList(x), true) &
        @stm p begin
            [K"=" kw v] -> vst1_param(vcx, kw) & vst1(vcx, v)
            [K"..." kw] -> vst1_param_varkw(vcx, kw)
            _ -> vst1_param(vcx, p)
        end
    [K"block" _ _ _ _...] -> @fail(st, "more than one semicolon in signature")
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
        [K"function" name] -> vst1_ident(vcx, name)
        [K"function" callex body] ->
            vst1_function_calldecl(with(f_vcx; return_ok=false), callex) &
            vst1(f_vcx, body)
        [K"=" callex body] ->
            vst1_function_calldecl(with(f_vcx; return_ok=false), callex) &
            vst1(f_vcx, body)
        _ -> @fail(st, "malformed `function`")
    end
end

# Note that we consistently refer to children of a declaring call as
# "parameters" rather than arguments (and children of a K"parameters" block as
# "keyword args/params") so we don't mix them up with children to a real call,
# whose valid forms are subtly different.

vst1_function_calldecl(vcx, st) = @stm st begin
    [K"where" callex tds...] ->
        vst1_function_calldecl(vcx, callex) & all(vst1_typevar_decl, vcx, tds)
    [K"::" callex rt] ->
        vst1_simple_calldecl(vcx, callex) &
        vst1(with(vcx, readable_underscore=true), rt)
    _ -> vst1_simple_calldecl(vcx, st)
end

vst1_simple_calldecl(vcx, st) = @stm st begin
    [K"call" f [K"parameters" _...] ps...] ->
        vst1_calldecl_name(vcx, f) &
        _calldecl_positionals(vcx, ps, false) &
        vst1_calldecl_kws(vcx, st[2])
    [K"call" f ps...] -> vst1_calldecl_name(vcx, f) &
        _calldecl_positionals(vcx, ps, false)
    # anonymous function syntax `function (x); end`  or `function (x...); end`
    [K"tuple" _...] -> vst1_lam_lhs(vcx, st)
    [K"..." va] -> vst1_pparam_typed_tuple(vcx, va)
    _ -> @fail(st, "malformed `call` in function decl")
end

vst1_macro(vcx, st) = @stm st begin
    [K"macro" m] -> vst1_ident(vcx, m; lhs=true) | vst1_ident(vcx, m; lhs=false)
    [K"macro" [K"call" _ [K"parameters" _...] _...] _...] ->
        @fail(st[1][end], "macros cannot accept keyword arguments")
    [K"macro" [K"call" m ps...] body] ->
        let vcx = with(vcx; return_ok=false, toplevel=false, in_gscope=false)
            vst1_macro_calldecl_name(vcx, m) &
                _calldecl_positionals(vcx, ps, false) &
                vst1(with(vcx; return_ok=true), body)
        end
    [K"macro" [K"where" _...] _...] ->
        @fail(st[1], "`where` not allowed in macro signatures")
    [K"macro" _...] -> @fail(st, "malformed `macro`")
    _ -> unknown()
end

# Macros may have either underscore or reserved (ccall, cglobal) names
vst1_macro_calldecl_name(vcx, st) = @stm st begin
    [K"." _ _] -> vst1_calldecl_dot_name(vcx, st)
    m -> @fail(st, "invalid macro name") |
        vst1_ident(vcx, m; lhs=true) | vst1_ident(vcx, m; lhs=false)
end

vst1_calldecl_name(vcx, st) = @stm st begin
    [K"Identifier"] -> vst1_ident(vcx, st; lhs=true) &
        (!is_dotted_operator(st.name_val::String) ? pass() :
        @fail(st, "dotted operator is not a valid function name"))
    [K"." _ _] ->
        vst1_calldecl_dot_name(vcx, st)
    [K"curly" t tvs...] ->
        vst1_calldecl_name(vcx, t) & all(vst1, vcx, tvs)
    [K"Value"] ->
        pass() # GlobalRef works. Function? Type?
    # callable type
    [K"::" t] -> vst1(vcx, t)
    [K"::" x t] -> vst1_pparam_simple_tuple(vcx, x) & vst1(vcx, t)
    # TODO: @overlay broken in many cases, should be stricter
    [K"overlay" mt x] ->
        vst1(vcx, mt) & vst1_calldecl_name(vcx, x)

    [K"where" t tds...] ->
        vst1_calldecl_name(vcx, t) & all(vst1_typevar_decl, vcx, tds)
    _ -> @fail(st, "invalid function name")
end

_is_arg_meta(st) = @stm st begin
    [K"meta" s arg] -> let meta_s = get(s, :name_val, "")::String
        kind(arg) === K"meta" ?
            @fail(st, "invalid nested annotation") :
        !(meta_s in ("specialize", "nospecialize")) ?
            @fail(st, "unrecognized meta function arg form") : pass()
    end
    _ -> unknown()
end

# Check mandatory and optional positional params:
# `[pparam* pparam_and_default* pparam_and_splatdefault? pparam_va?]`
# TODO: add list matching to @stm
function _calldecl_positionals(vcx, params_meta, eq_is_kw)
    isempty(params_meta) && return pass()
    ok = Ref(pass())
    params = map(params_meta) do meta_p
        @stm meta_p begin
            [K"meta" s p] -> begin
                ok[] &= _is_arg_meta(meta_p)
                p
            end
            p -> p
        end
    end
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
vst1_pparam_va(vcx, st; eq_is_kw) = @stm st begin
    [K"kw" [K"..." va] val] ->
        vst1_pparam_typed_tuple(vcx, va) & vst1_splat_or_val(vcx, val)
    ([K"=" [K"..." va] val], when=eq_is_kw) ->
        vst1_pparam_typed_tuple(vcx, va) & vst1_splat_or_val(vcx, val)
    [K"..." va] -> vst1_pparam_typed_tuple(vcx, va)
    _ -> unknown()
end

# destructuring args: function f(a, (x, y)) ...
vst1_pparam_typed_tuple(vcx, st) = @stm st begin
    [K"::" [K"tuple" _...] t] ->
        vst1_pparam_simple_tuple(vcx, st[1]) &
        vst1(with(vcx; readable_underscore=true), t)
    [K"tuple" _...] -> vst1_pparam_simple_tuple(vcx, st)
    _ -> vst1_param(vcx, st)
end
vst1_pparam_simple_tuple_or_splat(vcx, st) = @stm st begin
    [K"..." t] -> vst1_pparam_simple_tuple(vcx, t)
    t -> vst1_pparam_simple_tuple(vcx, t)
end
# Similar to an assignment to a tuple LHS, but does not allow `::`.  Also should
# not allow ref, curly, or call, but flisp does, so we may need to change this.
vst1_pparam_simple_tuple(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"tuple" [K"parameters" _ _...] _ _...] -> @fail(
        st[1], "cannot mix tuple `(a,b,c)` and named tuple `(;a,b,c)` syntax")
    [K"tuple" [K"parameters" kws...]] -> all(vst1_ident, vcx, kws; lhs=true)
    [K"tuple" xs...] ->
        all(vst1_pparam_simple_tuple_or_splat, vcx, xs) &
        (count(kind(x)===K"..." for x in xs) <= 1 ? pass() :
        @fail(st, "multiple `...` in destructured parameter is ambiguous"))
    [K"::" _...] -> @fail(st, "cannot have type in destructured argument")
    _ -> @fail(st, "expected identifier or tuple")
end

vst1_param(vcx, st) = @stm st begin
    [K"Identifier"] -> vst1_ident(vcx, st; lhs=true)
    [K"::" id t] -> vst1_ident(vcx, id; lhs=true) &
        vst1(with(vcx; readable_underscore=true), t)
    [K"::" t] -> vst1(with(vcx; readable_underscore=true), t)
    _ -> @fail(st, "expected identifier or `identifier::type`")
end

# allow_val_splat=true when this is the final optional param (even if there are
# varargs after it).  See #50563
vst1_pparam_and_default(vcx, st; eq_is_kw, allow_val_splat) = @stm st begin
    [K"kw" id val] ->
        vst1_pparam_typed_tuple(vcx, id) & @stm val begin
            [K"..." v] -> allow_val_splat ? vst1(vcx, v) :
                @fail(val, "splat only allowed on final positional default arg")
            _ -> vst1(vcx, val)
        end
    ([K"=" id val], when=eq_is_kw) ->
        vst1_pparam_typed_tuple(vcx, id) & @stm val begin
            [K"..." v] -> allow_val_splat ? vst1(vcx, v) :
                @fail(val, "splat only allowed on final positional default arg")
            _ -> vst1(vcx, val)
        end
    _ -> @fail(st, "malformed optional positional parameter; expected `=`")
end

vst1_calldecl_kws(vcx, st) = @stm st begin
    [K"parameters" kws... [K"..." varkw]] ->
        all(vst1_param_kw, vcx, kws) & vst1_param_varkw(vcx, varkw)
    [K"parameters" kws... [K"meta" _ [K"..." varkw]]] ->
        all(vst1_param_kw, vcx, kws) &
        _is_arg_meta(st[end]) &
        vst1_param_varkw(vcx, varkw)
    [K"parameters" kws...] -> all(vst1_param_kw, vcx, kws)
    _ -> @fail(st, "malformed keyword parameters")
end

vst1_param_varkw(vcx, st) = @stm st begin
    [K"Identifier"] -> vst1_ident(vcx, st; lhs=true)
    [K"::" _...] ->
        @fail(st, "keyword parameter with `...` may not be given a type")
    _ -> @fail(st, "expected identifier")
end

vst1_param_kw(vcx, st) = @stm st begin
    [K"kw" id val] ->
        vst1_param(vcx, id) & vst1(vcx, val)
    [K"meta" s x] -> _is_arg_meta(st) & vst1_param_kw(vcx, x)
    [K"..." _...] ->
        @fail(st, "`...` may only be used for the final keyword parameter")
    _ -> vst1_param(vcx, st) |
        @fail(st, "malformed keyword parameter; expected identifier, `=`, or `::`")
end

vst1_typevar_decl(vcx, st) = @stm st begin
    [K"Identifier"] -> vst1_ident(vcx, st; lhs=true)
    [K"<:" t old] ->
        vst1_ident(vcx, t; lhs=true) & vst1(vcx, old)
    [K">:" t old] ->
        vst1_ident(vcx, t; lhs=true) & vst1(vcx, old)
    ([K"comparison" val_l [K"Identifier"] t [K"Identifier"] val_r],
     when=(st[2].name_val===st[4].name_val && st[2].name_val in ("<:", ">:"))) ->
         vst1(vcx, val_l) &
         vst1_ident(vcx, t; lhs=true) &
         vst1(vcx, val_r)
    [K"<:" x _] ->
        @fail(x, "expected type name")
    [K">:" x _] ->
        @fail(x, "expected type name")
    [K"comparison" _...] ->
        @fail(st, "expected `lb <: type_name <: ub` or `ub >: type_name >: lb`")
    _ -> @fail(st, "expected type name or type bounds")
end

vst1_typesig(vcx, st) = @stm st begin
    [K"Identifier"] ->
        vst1_ident(vcx, st)
    [K"curly" t tvs...] ->
        vst1_ident(vcx, t) & all(vst1_typevar_decl, vcx, tvs)
    [K"<:" [K"curly" t tvs...] super] ->
        vst1_ident(vcx, t) & vst1(vcx, super) &
        all(vst1_typevar_decl, vcx, tvs)
    [K"<:" t super] ->
        vst1_ident(vcx, t) & vst1(vcx, super)
    _ -> @fail(st, "invalid type signature")
end

# normal, non-lhs curly may have implicit `(<: t)`
vst1_curly_typevar(vcx, st) = @stm st begin
    [K"<:" t] -> vst1_splat_or_val(vcx, t)
    [K">:" t] -> vst1_splat_or_val(vcx, t)
    _ -> vst1_splat_or_val(vcx, st)
end

vst1_struct_arg(vcx, st) =
    vst1_struct_special_form(vcx, st) | vst1(vcx, st)

vst1_struct_special_form(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"::" x t] -> vst1_struct_special_form(vcx, x) & vst1(vcx, t)
    [K"const" x] -> vst1_struct_special_form(vcx, x)
    [K"atomic" x] -> vst1_struct_special_form(vcx, x)
    _ -> unknown()
end

# Messy: expr uses a different head for every op `(a op= b)` and `(a .op= b)`.
# RawGreenNode uses K"op=" and K".op=" with an extra argument specifying `op`.
# The tree we're matching stays one-to-one with Expr by using `K"unknown_head"`.
#
# Note simple `op` and `.op` are calls to (dotted) identifiers, so this special
# handling isn't necessary.
vst1_dotted_or_op_assign(vcx, st) = let op_s = get(st, :name_val, "")::String
    @stm st begin
        [K".=" l r] -> vst1_dotassign_lhs(vcx, l) & vst1(vcx, r)
        (_, when=(!Base.isoperator(op_s))) -> unknown()
        (_, when=(isempty(op_s) || op_s[end] !== '=')) -> unknown()
        ([K"unknown_head" l r], when=(op_s[1] === '.')) ->
             vst1_dotassign_lhs(vcx, l) & vst1(vcx, r)
        ([K"unknown_head" l r]) ->
             vst1_assign_lhs(vcx, l) & vst1(vcx, r)
        _ -> unknown()
    end
end

vst1_assign(vcx, st; in_const = false) = @stm st begin
    # This case handles a proper function declaration (= (call ...) ...) form.
    # `vst1_assign_lhs_nontuple` also accepts call forms, but this is a lowering
    # bug where the "function body" is evaluated immediately
    ([K"=" l r], when=is_eventually_call(l)) -> vst1_function(vcx, st)
    [K"=" l r] -> vst1_assign_lhs(vcx, l; in_const) & vst1(vcx, r)
    [K"=" _...] -> @fail(st, "malformed assignment")
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
    [K"tuple" [K"parameters" xs...]] -> all(vst1_symdecl, vcx, xs)
    [K"tuple" xs...] ->
        all(vst1_assign_lhs, vcx, xs; in_const, in_tuple=true) &
        (count(kind(x)===K"..." for x in xs) <= 1 ? pass() :
        @fail(st, "multiple `...` in destructuring assignment are ambiguous"))
    # type-annotated tuple segfaults, haha
    # [K"::" [K"tuple" _...] t] -> ???
    [K"..." x] -> !in_tuple ?
        @fail(st, "splat on left side of assignment must be in a tuple") :
        vst1_assign_lhs_nontuple(vcx, x; in_const)
    ([K"parameters" _...], when=in_tuple) -> @fail(st, """
        property destructuring must use a single `;` before the property \
        names, e.g. `(; a, b) = rhs`""")
    _ -> vst1_assign_lhs_nontuple(vcx, st; in_const)
end
vst1_assign_lhs_nontuple(vcx, st; in_const=false, in_tuple=false) = @stm st begin
    [K"ssavalue" [K"Value"]] -> in_const ? @fail(st, "cannot declare ssavalue const") : pass()
    (_, when=(is_eventually_call(st))) ->
        vst1_function_calldecl(vcx, st)
    [K"::" x t] ->
        vst1_assign_lhs(vcx, x; in_const, in_tuple) & vst1(vcx, t)
    [K"." x y] ->
        in_const ? @fail(st, "cannot declare this form constant") :
        vst1(vcx, x) & vst1_dot_definition_rhs(vcx, y)
    [K"ref" x is...] ->
        in_const ? @fail(st, "cannot declare this form constant") :
        vst1(vcx, x) & all(vst1_splat_or_val, vcx, is)
    [K"curly" [K"Identifier"] tvs...] ->
        all(vst1_typevar_decl, vcx, tvs)

    [K"typed_hcat" _...] ->
        @fail(st, "invalid spacing in left side of indexed assignment")
    [K"typed_vcat" _...] ->
        @fail(st, "unexpected `;` in left side of indexed assignment")
    [K"typed_ncat" _...] ->
        @fail(st, "unexpected `;` in left side of indexed assignment")
    (_, when=(kind(st) in KSet"vect hcat vcat ncat")) ->
        @fail(st, "use `(a, b) = ...` to assign multiple values")
    _ -> @fail(st, "invalid syntax in left-hand side of assignment") |
        vst1_ident(vcx, st; lhs=true)
end

vst1_dotassign_lhs(vcx, st) = vst1_assign_lhs(vcx, st) | vst1(vcx, st)

# TODO: more validation is possible here, e.g. when row/nrow can show up in ncat
vst1_arraylike(vcx, st) = @stm st begin
    [K"vect" xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [K"hcat" xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [K"vcat" xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [K"ncat" [K"Value"] xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [K"ref" x is...] -> vst1(vcx, x) &
        no_assignment(is, "[ ... ]") & all(vst1_splat_or_val, vcx, is)
    [K"row" xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [K"nrow" [K"Value"] xs...] ->
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [K"typed_hcat" t xs...] -> vst1(vcx, t) &
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [K"typed_vcat" t xs...] -> vst1(vcx, t) &
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    [K"typed_ncat" t xs...] -> vst1(vcx, t) &
        no_assignment(xs, "array expression") & all(vst1_splat_or_val, vcx, xs)
    _ -> unknown()
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

vst1_splat_or_val(vcx, st) = @stm st begin
    [K"..." x] -> vst1_splat_or_val(vcx, x)
    [K"..." _...] -> @fail(st, "expected one argument to `...`")
    _ -> vst1(vcx, st)
end

vst1_generator(vcx, st) = let
    vcx = with(vcx; return_ok=false, toplevel=false, in_gscope=false)
    @stm st begin
        [K"generator" _] -> @fail(st, "`generator` requires >=2 args")
        [K"generator" val [K"filter" cond is...]] ->
            vst1(vcx, val) &
            vst1(vcx, cond) &
            all(vst1_iter, vcx, is)
        [K"generator" val is...] ->
            vst1(vcx, val) & all(vst1_iter, vcx, is)
        [K"generator" _...] -> @fail(st, "malformed `generator`")
        _ -> unknown()
    end
end

vst1_iter(vcx, st) = @stm st begin
    [K"=" [K"outer" i] v] -> vst1_assign_lhs(vcx, i) & vst1(vcx, v)
    [K"=" i v] -> vst1_assign_lhs(vcx, i) & vst1(vcx, v)
    _ -> @fail(st, "expected one of `=`, `in`, `∈`")
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
vst0_macrocall(vcx, st) = @stm st begin
    (_, when=!vcx.unexpanded) ->
        @fail(st, "macrocall not valid in AST after macro expansion")
    ([K"macrocall" name [K"Value"] args...],
     when=(typeof(st[2].value) in (LineNumberNode, MacroSource))) ->
         pass()
    [K"macrocall" _...] ->
        @fail(st, "expected (macrocall name linenode args...)")
    _ -> @fail(st, "invalid macrocall syntax")
end

vst0_quoted(vcx, st; quote_level) = @stm st begin
    ([K"$" x], when=quote_level===1) ->
        vst1_splat_or_val(vcx, x)
    [K"$" x] ->
        vst0_quoted(vcx, x; quote_level=quote_level-1)
    [K"quote" x] ->
        vst0_quoted(vcx, x; quote_level=quote_level+1)
    _ -> all(vst0_quoted, vcx, children(st); quote_level)
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
        required_attrs = @stm st begin
            [K"Identifier"] -> (:name_val,)
            [K"core"] -> (:name_val,)
            [K"top"] -> (:name_val,)
            [K"Symbol"] -> (:name_val,)
            [K"globalref"] -> (:name_val,:mod)
            [K"Placeholder"] -> ()
            [K"BindingId"] -> (:var_id,)
            [K"label"] -> (:id,)
            [K"symboliclabel"] -> (:name_val,)
            [K"symbolicgoto"] -> (:name_val,)
            [K"oldsymbolicgoto"] -> (:name_val,)
            [K"Value"] -> (:value,)
            [K"slot"] -> (:var_id,)
            [K"static_parameter"] -> (:var_id,)
            [K"SSAValue"] -> (:var_id,)
            [K"TOMBSTONE"] -> ()
            [K"SourceLocation"] -> ()
            [K"latestworld"] -> ()
            [K"latestworld_if_toplevel"] -> ()
            (_, when=JuliaSyntax.is_literal(st)) -> (:value,)
            (_, when=JuliaSyntax.is_trivia(st)) -> () # green tree only
            (_, when=JuliaSyntax.is_operator(st)) -> (:name_val) # TODO: remove
            _ -> return vr & @fail(st, "unrecognized leaf kind")
        end
    else
        required_attrs = @stm st begin
            [K"code_info" _...] -> (:slots, :is_toplevel_thunk)
            [K"scope_block" _...] -> (:scope_type,)
            [K"unknown_head" _...] -> (:name_val,)
            _ -> ()
        end
    end
    for a in required_attrs
        vr &= hasattr(st, a) ? pass() : @fail(st, string("needs attribute ", a))
    end
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

vst2(vcx::Validation2Context, st::SyntaxTree) = @stm st begin
    (_, when=is_leaf(st)) -> kind(st) in KSet"""
    Identifier BindingId Placeholder
    Bool Char Float Float32 BinInt OctInt HexInt Integer
    SourceLocation String Symbol Value core top
    latestworld latestworld_if_toplevel symbolicgoto oldsymbolicgoto symboliclabel TOMBSTONE
    """ ? pass() : @fail(st, "unrecognized leaf kind")

    [K"call" [K"static_eval" cg] xs...] -> get(cg, :name_val, nothing) == "cglobal" ?
        all(vst2, vcx, xs) : @fail(st, "expected (call (static_eval cglobal) _...)")
    [K"call" xs...] -> all(vst2, vcx, xs)
    [K"block" xs...] -> all(vst2, vcx, xs)
    [K"scope_block" xs...] -> all(vst2, vcx, xs)
    [K"=" l r] -> vst2_ident_lhs(vcx, l) & vst2(vcx, r)
    [K"assign_or_constdecl_if_global" l r] -> vst2_ident(vcx, l) & vst2(vcx, r)
    [K"constdecl" l] -> vst2_ident_lhs(vcx, l)
    [K"constdecl" l r] -> vst2_ident_lhs(vcx, l) & vst2(vcx, r)
    [K"global" x] -> vst2_ident_lhs(vcx, x)
    [K"local" x] -> vst2_ident_lhs(vcx, x)
    [K"decl" x t] -> vst2_ident(vcx, x) & vst2(vcx, t)
    [K"if" cond t] -> vst2(vcx, cond) & vst2(vcx, t)
    [K"if" cond t f] ->  vst2(vcx, cond) & vst2(vcx, t) & vst2_else(vcx, f)
    [K"&&" xs...] -> all(vst2, vcx, xs)
    [K"||" xs...] -> all(vst2, vcx, xs)
    [K"symbolicblock" [K"symboliclabel"] body] -> vst2(vcx, body)
    [K"break" [K"symboliclabel"]] -> pass()
    [K"break" [K"symboliclabel"] x] -> vst2(vcx, x)
    [K"return" x] -> vst2(vcx, x)
    [K"trycatchelse" t c] -> vst2(vcx, t) & vst2(vcx, c)
    [K"trycatchelse" t c e] -> vst2(vcx, t) & vst2(vcx, c) & vst2(vcx, e)
    [K"tryfinally" t f] -> vst2(vcx, t) & vst2(vcx, f)
    [K"tryfinally" t f scope] -> vst2(vcx, t) & vst2(vcx, f) & vst2(vcx, scope)
    [K"_opaque_closure" id argt lb ub partial nargs isva src lam] ->
        all(vst2, vcx, children(st)[2:end]) & vst2_lam(vcx, lam)
    [K"_do_while" body cond] -> vst2(vcx, body) & vst2(vcx, cond)
    [K"_while" cond body] -> vst2(vcx, cond) & vst2(vcx, body)
    [K"inert" _] -> pass()
    [K"inert_syntaxtree" _] -> pass()
    [K"lambda" _...] -> vst2_lam(vcx, st)
    [K"function_decl" x] -> vst2_ident(vcx, x)
    [K"function_type" x] -> vst2(vcx, x)
    [K"method" name meta lam] -> !vcx.in_method_defs ?
        @fail(st, "method outside of method_defs") :
        vst2_ident_val(vcx, name) & vst2(vcx, meta) & vst2_lam(vcx, lam)
    [K"method_defs" id body] ->
        vst2_ident_val(vcx, id) & vst2(with(vcx; in_method_defs=true), body)
    [K"new" t args...] -> vst2(vcx, t) & all(vst2, vcx, args)
    [K"splatnew" t arg] -> vst2(vcx, t) & vst2(vcx, arg)
    [K"softscope"] -> pass()
    [K"softscope" _] -> pass()
    [K"thisfunction"] -> pass()
    [K"gc_preserve_begin" xs...] -> all(vst2_ident, vcx, xs)
    [K"gc_preserve_end" xs...] -> minlen(st, xs, 1) & all(vst2_ident, vcx, xs)

    [K"meta" xs...] -> all(vst2, vcx, xs) # TODO
    [K"loopinfo" xs...] -> all(vst2, vcx, xs) # TODO
    [K"boundscheck" xs...] -> all(vst2, vcx, xs) # TODO

    [K"always_defined" x] -> vst2_ident(vcx, x)
    [K"assert" [K"Symbol"] x] -> vst2(vcx, x)
    [K"removable" x] -> vst2(vcx, x)

    # Could be made stricter
    [K"foreigncall" _ [K"static_eval" rt] [K"static_eval" at] cconv roots_args...] ->
         vst2(vcx, rt) &
         vst2(vcx, at) &
         vst2(vcx, cconv) &
         all(vst2, vcx, roots_args)
    [K"cfunction" [K"Value"] fptr [K"static_eval" rt] [K"static_eval" at] [K"Symbol"]] ->
         vst2(vcx, fptr) & vst2(vcx, rt) & vst2(vcx, at)

    [K"isdefined" x] -> vst2_ident_val(vcx, x)
    [K"isglobal" [K"Placeholder"]] -> pass()
    [K"islocal" [K"Placeholder"]] -> pass()
    [K"isglobal" x] -> vst2_ident_val(vcx, x)
    [K"islocal" x] -> vst2_ident_val(vcx, x)
    [K"locals"] -> pass()
    _ -> @fail(st, "unrecognized form out of desugaring")
end

vst2_ident_lhs(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"BindingId"] -> pass()
    [K"Placeholder"] -> pass()
    _ -> @fail(st, "expected identifier (lhs)")
end

vst2_ident(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"BindingId"] -> pass()
    _ -> @fail(st, "expected identifier or BindingId")
end

vst2_ident_val(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"BindingId"] -> pass()
    [K"core"] -> pass()
    [K"top"] -> pass()
    _ -> @fail(st, "expected identifier (val)")
end

vst2_lam(vcx, st) = @stm st begin
    [K"lambda" [K"block" args...] [K"block" sps...] body] ->
        all(vst2_ident_lhs, vcx, args) &
        all(vst2_ident_lhs, vcx, sps) &
        vst2(vcx, body)
    [K"lambda" [K"block" args...] [K"block" sps...] body rett] ->
        all(vst2_ident_lhs, vcx, args) &
        all(vst2_ident_lhs, vcx, sps) &
        vst2(vcx, body) &
        vst2(vcx, rett)
    _ -> @fail(st, "malformed lambda")
end

vst2_else(vcx, st) = @stm st begin
    [K"elseif" cond t] -> vst2(vcx, cond) & vst2(vcx, t)
    [K"elseif" cond t f] -> vst2(vcx, cond) & vst2(vcx, t) & vst2_else(vcx, f)
    _ -> vst2(vcx, st)
end
