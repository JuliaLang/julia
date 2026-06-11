# TODO: Allow `soft_scope::Union{Nothing,Bool}` to be passed through `jl_lower` C API

"""
Becomes `Core._lower()` upon activating JuliaLowering.

Returns an svec with the lowered code (usually expr) as its first element, and
(until integration is less experimental) whatever we want after it
"""
function core_lowering_hook(@nospecialize(code), mod::Module, file::Union{String,Ptr{UInt8}}="none",
                            line::Integer=0, world::UInt=typemax(Csize_t), _warn::Bool=false)
    if !(code isa SyntaxTree || code isa Expr)
        # e.g. LineNumberNode, integer...
        return Core.svec(code)
    end

    # TODO: fix in base
    file = file isa Ptr{UInt8} ? unsafe_string(file) : file
    line = !(line isa Int) ? Int(line) : line

    local st0, st1 = nothing, nothing
    try
        st0 = code isa Expr ? expr_to_est(code, LineNumberNode(line, file)) : code
        # export/public pass through unlowered: the runtime's toplevel
        # machinery evaluates them directly from the post-lowering head check
        # (like flisp, which never lowers them). This matters during
        # bootstrap, where boot.jl's own `export` runs before any runtime
        # support functions exist.
        st0u = st0
        wrappers = Any[]
        while kind(st0u) == K"hygienic-scope" && numchildren(st0u) >= 3
            push!(wrappers, (st0u[2].value, st0u[3].value))
            st0u = st0u[1]
        end
        if kind(st0u) == K"toplevel" && !isempty(wrappers)
            # A macro-produced `toplevel` form: defer each chunk, rewrapped
            # in its hygienic context so that escapes inside chunks resolve
            # when the runtime re-lowers them one by one.
            out = Expr(:toplevel)
            for c in children(st0u)
                ec = est_to_expr(c)
                for (m, l) in wrappers
                    ec = Expr(Symbol("hygienic-scope"), ec, m, l)
                end
                push!(out.args, ec)
            end
            return Core.svec(out)
        end
        if kind(st0u) in KSet"export public" && st0u !== st0
            # macro-produced export/public: the runtime requires plain
            # symbols, so resolve away the hygiene wrappers
            out = Expr(kind(st0u) == K"export" ? :export : :public)
            for c in children(st0u)
                cu = c
                while kind(cu) in KSet"escape hygienic-scope" && numchildren(cu) >= 1
                    cu = cu[1]
                end
                push!(out.args, Symbol(cu.name_val::String))
            end
            return Core.svec(out)
        end
        if kind(st0u) in KSet"toplevel module export public"
            # n.b. for other toplevel-only forms wrapped in hygienic-scope
            # (e.g. a macro-produced module), the hygiene context of the
            # form's own head is dropped, matching the (admittedly poor)
            # flisp handling
            return Core.svec(st0u === st0 ? code : est_to_expr(st0u))
        elseif kind(st0) === K"doc" && numchildren(st0) >= 2 && kind(st0[2]) === K"module"
            # TODO: this ignores module docstrings for now
            return Core.svec(est_to_expr(st0[2]))
        end
        ctx1, st1 = expand_forms_1(  mod,  st0, true, world)
        ctx2, st2 = expand_forms_2(  ctx1, st1)
        ctx3, st3 = resolve_scopes(  ctx2, st2)
        ctx4, st4 = convert_closures(ctx3, st3)
        ctx5, st5 = linearize_ir(    ctx4, st4)
        ex = to_lowered_expr(st5)
        return Core.svec(ex, st5, ctx5)
    catch exc
        @info("JuliaLowering threw given input:", code=code, st0=st0, st1=st1, file=file, line=line, mod=mod)
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
