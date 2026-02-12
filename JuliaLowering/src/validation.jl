struct ValidationDiagnostic
    sts::SyntaxList
    msg::String
    loc::LineNumberNode # for noting where failures come from in this file
end
ValidationDiagnostic(st::SyntaxTree, msg, loc) =
    ValidationDiagnostic(
        SyntaxList(syntax_graph(st), NodeId[st._id]), msg, loc)

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

# TODO: pretty-printing
function showerrors(vr::ValidationResult)
    isnothing(vr.errors) && return
    for (i, e) in enumerate(vr.errors)
        printstyled("error $i:\n"; color=:red)
        showerror(stdout, LoweringError(e.sts[1], e.msg))
        for st_ in e.sts[2:end]; show(st_); end
        show(e.loc)
        printstyled("\n---------------------------------\n"; color=:red)
    end
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
    in_param_t::Bool=false

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
              in_param_t   =vcx.in_param_t,
              unexpanded   =vcx.unexpanded)
    Validation1Context(
        toplevel, in_gscope, in_loop, in_symblock, inner_cond, return_ok,
        in_param_t, unexpanded)
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
  kinds are leaves (or not), and that syntax flags are valid per kind.  Those
  should be checked before this validation in a linear pass over the nodes.
- Scope issues are caught later in lowering, e.g. declaring something local and
  global.
- Checking that certain forms don't appear in value position is also handled
  later in lowering.
"""
function valid_st1(st::SyntaxTree)
    vr = vst1(Validation1Context(), st)
    @assert is_known(vr)
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
    [K"do" [K"macrocall" _...] lam] ->
        vst1_macrocall(vcx, call) & vst1_lam(vcx, lam)
    [K"do" call lam] -> vst1_call(vcx, call) & vst1_lam(vcx, lam)

    ([K"=" call _], when=(is_eventually_call(call))) ->
        vst1_function(vcx, st)
    [K"=" l r] ->
        vst1_assign_lhs(vcx, l) & vst1(vcx, r)
    [K".=" l r] ->
        vst1_dotassign_lhs(vcx, l) & vst1(vcx, r)
    ([K"unknown_head" l r], when=Base.isoperator(st.name_val)) ->
        vst1_op_assign(vcx, st)
    [K"unknown_head"] -> let head = st.name_val
        head === "latestworld-if-toplevel" ?
            pass() : @fail(st, string("unknown expr head: ", head))
    end
    ([K"." l r], when=kind(r)!==K"tuple") ->
        vst1(vcx, l) & vst1_simple_dot_rhs(vcx, r)
    [K"." x] ->
        vst1(vcx, x) # BroadcastFunction(x)
    [K"return" val] -> vcx.return_ok ?
        vst1(vcx, val) :
        @fail(st, "`return` not allowed inside comprehension or generator")
    [K"return"] -> vcx.return_ok ? pass() :
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
        vst1(vcx, t) & all(vst1_curly_typevar, vcx, tvs)
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
    [K"-->" x y] -> vst1(vcx, x) & vst1(vcx, y)
    [K"::" x y] -> vst1(vcx, x) & vst1(vcx, y)
    # TODO: inner_cond on args[2:end]
    [K"&&" xs...] -> all(vst1, vcx, xs)
    [K"||" xs...] -> all(vst1, vcx, xs)
    [K".&&" x y] -> vst1(vcx, x) & vst1(vcx, y)
    [K".||" x y] -> vst1(vcx, x) & vst1(vcx, y)
    (_, when=(vr=vst1_arraylike(vcx, st); is_known(vr))) -> vr
    [K"const" [K"global" x]] -> !vcx.toplevel ?
        @fail(st, "unsupported `const` inside function") :
        vst1_const_assign(vcx, x)
    [K"const" x] ->  !vcx.toplevel ?
        @fail(st, "unsupported `const` inside function") :
        vst1_const_assign(vcx, x)
    [K"global" xs...] -> all(vst1_global_arg, vcx, xs)
    [K"local" xs...] -> all(vst1_local_arg, vcx, xs)
    [K"macrocall" _...] -> vst0_macrocall(vcx, st)
    [K"quote" x] -> vcx.unexpanded ? vst0_quoted(vcx, x; quote_level=1) :
        @fail(st, "interpolating quote not valid syntax after macro expansion")

    #---------------------------------------------------------------------------
    # Forms not produced by the parser
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
    [K"symbolicblock" lab body] ->
        vst1_ident(vcx, lab; lhs=true) & vst1(with(vcx; in_symblock=true), body)
    [K"gc_preserve" x ids...] -> vst1(vcx, x) & all(vst1_ident, vcx, ids)
    [K"gc_preserve_begin" ids...] -> all(vst1_ident, vcx, ids)
    [K"gc_preserve_end" ids...] -> all(vst1_ident, vcx, ids)
    [K"isdefined" [K"Identifier"]] -> pass()
    [K"lambda" [K"block" b1...] [K"block" b2...] [K"->" _...]] ->
        all(vst1_ident, vcx, b1) &
        all(vst1_ident, vcx, b2) &
        vst1_lam(vcx, st[3])
    [K"softscope" _] -> pass()
    [K"softscope"] -> pass()
    [K"generated"] -> pass()
    [K"generated_function" callex gen nongen] ->
        vst1_function_calldecl(vcx, callex) &
        vst1(vcx, gen) & vst1(vcx, nongen)
    [K"foreigncall" _...] -> pass() # TODO (ccall also?)
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
    [K"import" ps...] -> all(vst1_importpath, vcx, ps; dots_ok=true)
    [K"using"  ps...] -> all(vst1_importpath, vcx, ps; dots_ok=true)
    [K"public" xs...] -> all(vst1_ident, vcx, xs)
    [K"export" xs...] -> all(vst1_ident, vcx, xs)
    [K"latestworld"] -> pass()
    _ -> unknown()
end

#-------------------------------------------------------------------------------

vst1_local_arg(vcx, st) = @stm st begin
    [K"function" _...] -> vst1_function(vcx, st)
    _ -> vst1_symdecl_or_assign(vcx, st) | vst1_op_assign(vcx, st) |
        @fail(st, "invalid local declaration: expected identifier or assignment")
end

vst1_global_arg(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    ([K"Value"], when=(st.value isa GlobalRef)) -> pass
    [K"=" l r] ->
        vst1_assign_lhs(vcx, l; disallow_type=!vcx.toplevel) &
        vst1(vcx, r)
    [K"function" _...] -> vcx.toplevel ?
        vst1_function(vcx, st) :
        @fail(st, "global function needs to be placed at top level, or use eval")
    [K"::" [K"Identifier"] t] -> vcx.toplevel ?
        vst1(vcx, t) :
        @fail(st, "type declarations for globals cannot be inside a function")
    (_, when=vst1_op_assign(vcx, st).ok) -> pass()
    _ -> @fail(st, "invalid global declaration: expected identifier or assignment")
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

vst1_try_catchvar(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    ([K"Value"], when=st.value===false) -> pass()
end

# syntax TODO:
# - const is inoperative in the function case
# - single-arg const with no value (presumably to poison this name) was likely
#   not intended to work, and can only be produced by macros
vst1_const_assign(vcx, st) = @stm st begin
    [K"=" l r] -> vst1_assign_lhs(vcx, l; in_const=true) & vst1(vcx, r)
    [K"Identifier"] -> pass()
    [K"local" _...] -> @fail(st, "unsupported `const local` declaration")
    _ -> @fail(st, "expected assignment after `const`")
end

# We can't validate A.B in general (usually lowers to getproperty), but it shows
# up in a number of syntax special cases where we can.
vst1_dotsym(vcx, st) = @stm st begin
    [K"." l r] -> vst1_dotsym(vcx, l) & vst1_simple_dot_rhs(vcx, r) |
        @fail(st, "invalid `.` form")
    [K"Value"] -> typeof(st.value) in (Module, GlobalRef) ? pass() :
        @fail(st, "expected `.` or identifier")
    i -> vst1_ident(vcx, i)
end

# syntax TODO: all-underscore variables may be read from with dot syntax
# syntax TODO: disallow string
vst1_simple_dot_rhs(vcx, st; lhs=false) = @stm st begin
    [K"inert" x] -> vst1_simple_dot_rhs(vcx, x)
    [K"Identifier"] -> pass()
    [K"String"] -> pass()
    [K"tuple" _...] -> @fail(st, "dotcall syntax not valid here")
    _ -> @fail(st, "invalid `.` syntax")
end

vst1_symdecl_or_assign(vcx, st) = @stm st begin
    [K"=" l r] -> vst1_assign_lhs(vcx, l) & vst1(vcx, r)
    _ -> vst1_symdecl(vcx, st)
end

vst1_symdecl(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"::" [K"Identifier"] t] -> vst1(vcx, t)
    _ -> @fail(st, "expected identifier or `identifier::type`")
end

# TODO: globalref might not be valid everywhere; check usage of this function
vst1_ident(vcx, st; lhs=false) = @stm st begin
    ([K"Identifier"], when=(s=st.name_val; true)) -> if all(==('_'), s)
        lhs || vcx.in_param_t ? pass() :
            @fail(st, "all-underscore identifiers are write-only and their values cannot be used in expressions")
    elseif s in ("ccall", "cglobal")
        # TODO
        pass()
    else
        pass()
    end
    ([K"Value"], when=(st.value isa GlobalRef)) -> pass()
    _ -> @fail(st, "expected identifier")
end

vst1_call(vcx, st) = @stm st begin
    [K"call" f [K"parameters" kwargs...] args...] ->
        vst1(vcx, f) &
        all(vst1_call_arg, vcx, args) &
        all(vst1_call_kwarg, vcx, kwargs)
    [K"call" f args...] ->
        vst1(vcx, f) &
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
# than `vst1_call_arg`.
vst1_call_kwarg(vcx, st) = @stm st begin
    [K"Identifier"] -> pass()
    [K"kw" id val] -> vst1_ident(vcx, id; lhs=true) & vst1(vcx, val)
    [K"..." x] -> vst1(vcx, x)
    [K"." x [K"inert" id]] -> vst1(vcx, x) & vst1_ident(vcx, id; lhs=true)
    ([K"call" [K"Identifier"] symval v], when=(st[1].name_val==="=>")) ->
        vst1(vcx, symval) & vst1(vcx, v)
    _ -> @fail(st, "expected identifier, `=`, or, `...` after semicolon")
end

vst1_lam(vcx, st) = let
    f_vcx = with(vcx; return_ok=true, toplevel=false, in_gscope=false)
    @stm st begin
        [K"->" l r] ->
            vst1_lam_lhs(with(f_vcx; return_ok=false), l) & vst1(f_vcx, r)
        _ -> unknown()
    end
end

vst1_lam_lhs(vcx, st) = @stm st begin
    [K"tuple" [K"parameters" _...] ps...] ->
        _calldecl_positionals(vcx, ps, K"=") & vst1_calldecl_kws(vcx, st[1])
    [K"tuple" ps...] ->
        _calldecl_positionals(vcx, ps, K"=")
    [K"where" ps tds...] ->
        vst1_lam_lhs(vcx, ps) & all(vst1_typevar_decl, vcx, tds)
    # syntax TODO: This is handled badly in the parser
    [K"block" p1 p2] -> pass()
    # unwrapped single arg
    _ -> let ps = SyntaxList(st._graph, tree_ids(st))
        _calldecl_positionals(vcx, ps, K"=")
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
        vst1(with(vcx, in_param_t=true), rt)
    _ -> vst1_simple_calldecl(vcx, st)
end

vst1_simple_calldecl(vcx, st; in_macro=false) = @stm st begin
    [K"call" f [K"parameters" _...] ps...] ->
        vst1_calldecl_name(vcx, f) &
        _calldecl_positionals(vcx, ps, K"kw") &
        vst1_calldecl_kws(vcx, st[2])
    [K"call" f ps...] -> vst1_calldecl_name(vcx, f) &
        _calldecl_positionals(vcx, ps, K"kw")
    # anonymous function syntax `function (x); end`  or `function (x...); end`
    [K"tuple" _...] -> vst1_lam_lhs(vcx, st)
    [K"..." va] -> vst1_pparam_typed_tuple(vcx, va)
    _ -> @fail(st, "malformed `call` in function decl")
end

vst1_macro(vcx, st) = @stm st begin
    [K"macro" m] -> vst1_ident(vcx, m)
    [K"macro" [K"call" _ [K"parameters" _...] _...] _...] ->
        @fail(st[1][end], "macros cannot accept keyword arguments")
    [K"macro" [K"call" m ps...] body] ->
        let vcx = with(vcx; return_ok=false, toplevel=false, in_gscope=false)
            vst1_macro_calldecl_name(vcx, m) &
                _calldecl_positionals(vcx, ps, K"kw") &
                vst1(with(vcx; return_ok=true), body)
        end
    [K"macro" [K"where" _...] _...] ->
        @fail(st[1], "`where` not allowed in macro signatures")
    _ -> unknown()
end

vst1_macro_calldecl_name(vcx, st) = @stm st begin
    (_, when=vst1_ident(vcx, st).ok) -> pass()
    [K"." _ _] -> vst1_dotsym(vcx, st)
    _ -> @fail(st, "invalid macro name")
end

vst1_calldecl_name(vcx, st) = @stm st begin
    [K"." _ _] ->
        vst1_dotsym(vcx, st)
    [K"curly" t tvs...] ->
        vst1_calldecl_name(vcx, t) & all(vst1, vcx, tvs)
    [K"Value"] ->
        pass() # GlobalRef works. Function? Type?

    # callable type
    [K"::" t] ->
        vst1(vcx, t)
    [K"::" x t] ->
        vst1_pparam_simple_tuple(vcx, x) & vst1(vcx, t)

    [K"where" t tds...] ->
        vst1(vcx, t) & all(vst1_typevar_decl, vcx, tds)
    _ -> @fail(st, "invalid function name") | vst1_ident(vcx, st)
end

# Check mandatory and optional positional params:
# `[pparam* pparam_and_default* pparam_and_splatdefault? pparam_va?]`
# TODO: add list matching to @stm
function _calldecl_positionals(vcx, params_meta, kw_kind)
    isempty(params_meta) && return pass()
    ok = Ref(pass())
    params = map(params_meta) do meta_p
        @stm meta_p begin
            [K"meta" s p] -> let meta_s = get(s, :name_val, "")
                if !(meta_s in ("specialize", "nospecialize"))
                    ok[] &= @fail(p, "unrecognized meta function arg form")
                end
                p
            end
            p -> p
        end
    end
    va_ok = vst1_pparam_va(vcx, params[end]; kw_kind)
    if is_known(va_ok)
        params = params[1:end-1]
        ok[] &= va_ok
    end
    require_assign = false
    for p in params
        if kind(p) === kw_kind
            require_assign = true
            if p == params[end]
                ok[] &= vst1_pparam_and_default(vcx, p; kw_kind, allow_val_splat=true)
            else
                ok[] &= vst1_pparam_and_default(vcx, p; kw_kind, allow_val_splat=false)
            end
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

vst1_pparam_va(vcx, st; kw_kind) = @stm st begin
    ([K"kw" [K"..." va] val], when=kw_kind===K"kw") ->
        vst1_pparam_typed_tuple(vcx, va) & vst1_splat_or_val(vcx, val)
    ([K"=" [K"..." va] val], when=kw_kind===K"=") ->
        vst1_pparam_typed_tuple(vcx, va) & vst1_splat_or_val(vcx, val)
    [K"..." va] -> vst1_pparam_typed_tuple(vcx, va)
    _ -> unknown()
end

# destructuring args: function f(a, (x, y)) ...
vst1_pparam_typed_tuple(vcx, st) = @stm st begin
    [K"::" [K"tuple" _...] t] ->
        vst1_pparam_simple_tuple(vcx, st[1]) &
        vst1(with(vcx; in_param_t=true), t)
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
    [K"Identifier"] -> pass()
    [K"::" [K"Identifier"] t] -> vst1(with(vcx; in_param_t=true), t)
    [K"::" t] -> vst1(with(vcx; in_param_t=true), t)
    _ -> @fail(st, "expected identifier or `identifier::type`")
end

# allow_val_splat=true when this is the final optional param (even if there are
# varargs after it).  See #50563
vst1_pparam_and_default(vcx, st; kw_kind, allow_val_splat) = @stm st begin
    ([K"kw" id val], when=(kw_kind===K"kw")) ->
        vst1_pparam_typed_tuple(vcx, id) & @stm val begin
            [K"..." v] -> allow_val_splat ? vst1(vcx, v) :
                @fail(val, "splat only allowed on final positional default arg")
            _ -> vst1(vcx, val)
        end
    ([K"=" id val], when=(kw_kind===K"=")) ->
        vst1_pparam_typed_tuple(vcx, id) & @stm val begin
            [K"..." v] -> allow_val_splat ? vst1(vcx, v) :
                @fail(val, "splat only allowed on final positional default arg")
            _ -> vst1(vcx, val)
        end
    _ -> @fail(st, "malformed optional positional parameter; expected `=`")
end

vst1_calldecl_kws(vcx, st) = @stm st begin
    [K"parameters" kws... [K"..." varkw]] ->
        all(vst1_param_kw, vcx, kws) & @stm varkw begin
            [K"Identifier"] -> pass()
            [K"::" _...] ->
                @fail(varkw, "keyword parameter with `...` may not be given a type")
        end
    [K"parameters" kws...] -> all(vst1_param_kw, vcx, kws)
    _ -> unknown()
end

vst1_param_kw(vcx, st) = @stm st begin
    [K"kw" id val] ->
        vst1_param(vcx, id) & vst1(vcx, val)
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
vst1_op_assign(vcx, st) = let op_s = get(st, :name_val, "")
    @stm st begin
        (_, when=(!Base.isoperator(op_s))) -> unknown()
        (_, when=(isempty(op_s) || op_s[end] !== '=')) ->
            @fail(st, "expected op= or .op=")
        ([K"unknown_head" l r], when=(op_s[1] === '.')) ->
             vst1_dotassign_lhs(vcx, l) & vst1(vcx, r)
        ([K"unknown_head" l r]) ->
             vst1_assign_lhs(vcx, l) & vst1(vcx, r)
        _ -> unknown()
    end
end

# TODO: We could do some destructuring checks here (e.g. fail `(a,b,c) = (1,2)`)
#
# syntax TODO:
# - call (only within a tuple using JuliaSyntax) can declare a function with
#   arguments, but can't use them on the rhs if in a tuple
# - in curly, typevars are checked for structure, but not used.
# - (local/global (= lhs rhs)) forms should probably reject the same
#   lhss as const (ref and .)
vst1_assign_lhs(
    vcx, st; in_const=false, in_tuple=false, disallow_type=false
) = @stm st begin
    [K"tuple" [K"parameters" xs...]] -> all(vst1_symdecl, vcx, xs)
    [K"tuple" xs...] ->
        all(vst1_assign_lhs, vcx, xs; in_const, in_tuple=true) &
        (count(kind(x)===K"..." for x in xs) <= 1 ? pass() :
        @fail(st, "multiple `...` in destructuring assignment are ambiguous"))
    # type-annotated tuple segfaults, haha
    # [K"::" [K"tuple" _...] t] -> ???
    [K"..." x] -> !in_tuple ?
        @fail(st, "splat on left side of assignment must be in a tuple") :
        vst1_assign_lhs_nontuple(vcx, x; in_const, disallow_type)
    ([K"parameters" _...], when=in_tuple) -> @fail(st, """
        property destructuring must use a single `;` before the property \
        names, e.g. `(; a, b) = rhs`""")
    _ -> vst1_assign_lhs_nontuple(vcx, st; in_const)
end
vst1_assign_lhs_nontuple(
    vcx, st; in_const=false, in_tuple=false, disallow_type=false
) = @stm st begin
    [K"Identifier"] -> pass()
    ([K"Value"], when=(st.value isa GlobalRef)) -> pass()
    (_, when=(is_eventually_call(st))) ->
        vst1_function_calldecl(vcx, st)
    [K"::" x t] -> disallow_type ?
        @fail(st, "type declarations for globals cannot be inside a function") :
        vst1_assign_lhs(vcx, x; in_const, in_tuple) & vst1(vcx, t)
    [K"." x y] ->
        in_const ? @fail(st, "cannot declare this form constant") :
        vst1(vcx, x) & vst1_simple_dot_rhs(vcx, y)
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
    _ -> @fail(st, "invalid syntax in left-hand side of assignment")
end

vst1_dotassign_lhs(vcx, st) = vst1_assign_lhs(vcx, st) | vst1(vcx, st)

# TODO: more validation is possible here, e.g. when row/nrow can show up in ncat
vst1_arraylike(vcx, st) = @stm st begin
    [K"vect" xs...] -> all(vst1_splat_or_val, vcx, xs)
    [K"hcat" xs...] -> all(vst1_splat_or_val, vcx, xs)
    [K"vcat" xs...] -> all(vst1_splat_or_val, vcx, xs)
    [K"ncat" [K"Value"] xs...] -> all(vst1_splat_or_val, vcx, xs)
    [K"ref" x is...] -> vst1(vcx, x) & all(vst1_splat_or_val, vcx, is)
    [K"row" xs...] -> all(vst1_splat_or_val, vcx, xs)
    [K"nrow" [K"Value"] xs...] -> all(vst1_splat_or_val, vcx, xs)
    [K"typed_hcat" t xs...] ->
        vst1(vcx, t) & all(vst1_splat_or_val, vcx, xs)
    [K"typed_vcat" t xs...] ->
        vst1(vcx, t) & all(vst1_splat_or_val, vcx, xs)
    [K"typed_ncat" t xs...] ->
        vst1(vcx, t) & all(vst1_splat_or_val, vcx, xs)
    _ -> unknown()
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
    vr = vst1(with(Validation1Context(), unexpanded=true), st)
    # hack: A macrocall can show up almost anywhere, so filter errors pointing
    # at macrocalls instead of adding cases to every function above.
    isnothing(vr.errors) && return vr.ok
    vr2_errors = filter(vr.errors) do err
        isempty(err.sts) || !(kind(err.sts[1]) === K"macrocall")
    end
    vr2 = ValidationResult(isempty(vr2_errors), vr2_errors)
    showerrors(vr2)
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
     when=(typeof(st[2].value) in (LineNumberNode, Core.MacroSource))) ->
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
