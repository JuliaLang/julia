# TODO: Allow `soft_scope::Union{Nothing,Bool}` to be passed through `jl_lower` C API

"""
Becomes `Core._lower()` upon activating JuliaLowering.

Returns an svec with the lowered code (usually expr) as its first element, and
(until integration is less experimental) whatever we want after it
"""
function core_lowering_hook(@nospecialize(code), mod::Module, file::String="none",
                            line::Int=0, world::UInt=typemax(Csize_t), _warn::Bool=false)
    return invoke_in_lowering_world(_core_lowering_hook, code, mod, file, line, world, _warn)
end

function _core_lowering_hook(@nospecialize(code), mod::Module, file::String,
                             line::Int, world::UInt, _warn::Bool)
    if !(code isa SyntaxTree || code isa Expr)
        # e.g. LineNumberNode, integer...
        return Core.svec(code)
    end

    local st0, st1 = nothing, nothing
    try
        st0 = code isa Expr ? expr_to_est(code, LineNumberNode(line, file)) : code
        if head(st0) === :toplevel || head(st0) === :module
            return Core.svec(code)
        end
        st0 = rebase_layers(st0, mod)
        st1 = expand_forms_1(st0, world, true)
        ctx2, st2 = expand_forms_2(st1, world)
        ctx3, st3 = resolve_scopes(ctx2, st2)
        ctx4, st4 = convert_closures(ctx3, st3)
        ctx5, st5 = linearize_ir(ctx4, st4)
        ex = to_lowered_expr(st5)
        return Core.svec(ex, st5, ctx5)
    catch exc
        @info("JuliaLowering threw given input:", code=code, file=file,
              line=line, mod=mod, st0=st0, st1=st1)
        if exc isa LoweringError && !exc.internal
            return Core.svec(Expr(:error, sprint(
                (io,err)->showerror(io,err; show_detail=false), exc)))
        else
            rethrow(exc)
        end

        # TODO: Re-enable flisp fallback once we're done collecting errors
        # @error("JuliaLowering failed — falling back to flisp!",
        #        exception=(exc,catch_backtrace()),
        #        code=code, file=file, line=line, mod=mod)
        # return Base.fl_lower(code, mod, file, line, world, warn)
    end
end

const _has_v1_13_hooks = isdefined(Core, :_lower)

function activate!(enable=true; freeze_world_age=true)
    if !_has_v1_13_hooks
        error("Cannot use JuliaLowering without `Core._lower` binding or in $VERSION < 1.13")
    end

    _lowering_world[] = (enable && freeze_world_age) ? Base.get_world_counter() : UInt(0)
    if enable
        Core._setlowerer!(core_lowering_hook)
        Core._set_toplevel_eval!(JuliaLowering.eval)
    else
        Core._setlowerer!(Base.fl_lower)
        Core._set_toplevel_eval!(_fl_toplevel_eval)
    end
end

function _fl_toplevel_eval(mod::Module, @nospecialize(x))
    ex = if x isa SyntaxTree
        Expr(:toplevel, first_linenode(x), est_to_expr(x))
    else
        x
    end
    ccall(:jl_toplevel_eval, Any, (Any, Any), mod, ex)
end

function __init__()
    _lowering_world[] = 0
end
