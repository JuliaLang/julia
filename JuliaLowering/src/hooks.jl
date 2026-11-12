"""
Becomes `Core._lower()` upon activating JuliaLowering.

Returns an svec with the lowered code (usually expr) as its first element, and
(until integration is less experimental) whatever we want after it
"""
function core_lowering_hook(mod::Module, @nospecialize(code), world::UInt,
                            warn::Bool, loc::LineNumberNode)
    return __script_exit_invoke_in_lowering_world(
        _core_lowering_hook, mod, code, world, warn, loc)
end

function _core_lowering_hook(mod::Module, @nospecialize(code), world, warn, loc)
    if !(code isa SyntaxTree || code isa Expr)
        return Core.svec(code)
    end
    local st0, st1 = nothing, nothing
    try
        st0 = code isa Expr ? expr_to_est(code, loc) : code
        if head(st0) in (:toplevel, :module, :error, :incomplete)
            return Core.svec(code)
        end
        st0 = rebase_layers(st0, mod)
        st1 = expand_forms_1(st0, world, true)
        ctx2, st2 = expand_forms_2(st1, world)
        ctx3, st3 = resolve_scopes(ctx2, st2, warn, false)
        ctx4, st4 = convert_closures(ctx3, st3)
        ctx5, st5 = linearize_ir(ctx4, st4)
        ex = to_lowered_expr(st5)
        return Core.svec(ex, st5, ctx5)
    catch exc
        if exc isa LoweringError && !exc.internal
            return Core.svec(Expr(:error, sprint(showerror, exc)))
        else
            rethrow(to_compatible_exception(exc))
        end
    end
end

@fzone "JL: eval" function __script_exit_eval(mod::Module, @nospecialize(st))
    if !(st isa SyntaxTree)
        st = expr_to_est(st)
    end
    return _eval(mod, lower_init(st), true, false)
end

# TODO: JL tests expect @eval to run with JL as the active lowerer.  Once it is
# the only lowerer, this can be replaced with `Core.eval`.
function at_eval_hook(mod::Module, st::SyntaxTree)
    ccall(:jl_check_top_level_effect, Cvoid, (Any, Cstring), mod, "eval")
    return __script_exit_eval(mod, st)
end

const _has_v1_13_hooks = isdefined(Core, :_lower)

function activate!(enable=true; freeze_world_age=true)
    if !_has_v1_13_hooks
        error("Cannot use JuliaLowering without `Core._lower` binding or in $VERSION < 1.13")
    end

    _lowering_world[] = (enable && freeze_world_age) ? Base.get_world_counter() : UInt(0)
    if enable
        Core._setlowerer!(core_lowering_hook)
        Core._set_toplevel_eval!(__script_exit_eval)
    else
        Core._setlowerer!(Base.fl_lower)
        Core._set_toplevel_eval!(Base.fl_toplevel_eval)
    end
end
