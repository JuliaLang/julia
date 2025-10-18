"""
Becomes `Core._lower()` upon activating JuliaLowering.

Returns an svec with the lowered code (usually expr) as its first element, and
(until integration is less experimental) whatever we want after it
"""
function core_lowering_hook(@nospecialize(code), mod::Module,
                            file="none", line=0, world=typemax(Csize_t), warn=false)
    if !(code isa SyntaxTree || code isa Expr)
        # e.g. LineNumberNode, integer...
        return Core.svec(code)
    end

    # TODO: fix in base
    file = file isa Ptr{UInt8} ? unsafe_string(file) : file
    line = !(line isa Int64) ? Int64(line) : line

    local st0 = nothing
    try
        st0 = code isa Expr ? expr_to_syntaxtree(code, LineNumberNode(line, file)) : code
        if kind(st0) in KSet"toplevel module"
            return Core.svec(code)
        elseif kind(st0) === K"doc" && numchildren(st0) >= 2 && kind(st0[2]) === K"module"
            # TODO: this ignores module docstrings for now
            return Core.svec(Expr(st0[2]))
        end
        ctx1, st1 = expand_forms_1(  mod,  st0, true, world)
        ctx2, st2 = expand_forms_2(  ctx1, st1)
        ctx3, st3 = resolve_scopes(  ctx2, st2)
        ctx4, st4 = convert_closures(ctx3, st3)
        ctx5, st5 = linearize_ir(    ctx4, st4)
        ex = to_lowered_expr(st5)
        return Core.svec(ex, st5, ctx5)
    catch exc
        @info("JuliaLowering threw given input:", code=code, st0=st0, file=file, line=line, mod=mod)
        rethrow(exc)

        # TODO: Re-enable flisp fallback once we're done collecting errors
        # @error("JuliaLowering failed — falling back to flisp!",
        #        exception=(exc,catch_backtrace()),
        #        code=code, file=file, line=line, mod=mod)
        # return Base.fl_lower(code, mod, file, line, world, warn)
    end
end

# TODO: Write a parser hook here.  The input to `core_lowering_hook` should
# eventually be a (convertible to) SyntaxTree, but we need to make updates to
# the parsing API to include a parameter for AST type.

const _has_v1_13_hooks = isdefined(Core, :_lower)

function activate!(enable=true)
    if !_has_v1_13_hooks
        error("Cannot use JuliaLowering without `Core._lower` binding or in $VERSION < 1.13")
    end

    if enable
        Core._setlowerer!(core_lowering_hook)
    else
        Core._setlowerer!(Base.fl_lower)
    end
end
