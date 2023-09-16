# TODO use PrecompileTools as a stdlib

function precompile_mi(mi)
    precompile(mi.specTypes) # TODO: Julia should allow one to pass `mi` directly (would handle `invoke` properly)
    return
end

"""
    check_edges(node)

Recursively ensure that all callees of `node` are precompiled. This is (rarely) necessary
because sometimes there is no backedge from callee to caller (xref https://github.com/JuliaLang/julia/issues/49617),
and `staticdata.c` relies on the backedge to trace back to a MethodInstance that is tagged `mi.precompiled`.
"""
function check_edges(node)
    parentmi = node.mi_info.mi
    for child in node.children
        childmi = child.mi_info.mi
        if !(isdefined(childmi, :backedges) && parentmi ∈ childmi.backedges)
            precompile_mi(childmi)
        end
        check_edges(child)
    end
end

function precompile_roots(roots)
    for child in roots
        precompile_mi(child.mi_info.mi)
        check_edges(child)
    end
end

"""
    @compile_workload f(args...)

`precompile` (and save in the compile_workload file) any method-calls that occur inside the expression. All calls (direct or indirect) inside a
`@compile_workload` block will be cached.

`@compile_workload` has three key features:

1. code inside runs only when the package is being precompiled (i.e., a `*.ji`
   precompile compile_workload file is being written)
2. the interpreter is disabled, ensuring your calls will be compiled
3. both direct and indirect callees will be precompiled, even for methods defined in other packages
   and even for runtime-dispatched callees (requires Julia 1.8 and above).

!!! note
    For comprehensive precompilation, ensure the first usage of a given method/argument-type combination
    occurs inside `@compile_workload`.

    In detail: runtime-dispatched callees are captured only when type-inference isc executed, and they
    are inferred only on first usage. Inferrable calls that trace back to a method defined in your package,
    and their *inferrable* callees, will be precompiled regardless of "ownership" of the callees
    (Julia 1.8 and higher).

    Consequently, this recommendation matters only for:

        - direct calls to methods defined in Base or other packages OR
        - indirect runtime-dispatched calls to such methods.
"""
macro compile_workload(ex::Expr)
    local iscompiling = Base.generating_output()
    ex = quote
        begin
            Base.Experimental.@force_compile
            $(esc(ex))
        end
    end
    ex = quote
        Core.Compiler.Timings.reset_timings()
        Core.Compiler.__set_measure_typeinf(true)
        try
            $ex
        finally
            Core.Compiler.__set_measure_typeinf(false)
            Core.Compiler.Timings.close_current_timer()
        end
        $(precompile_roots)(Core.Compiler.Timings._timings[1].children)
    end

    return quote
        if $iscompiling
            $ex
        end
    end
end

"""
    @setup_workload begin
        vars = ...
        ⋮
    end

Run the code block only during package precompilation. `@setup_workload` is often used in combination
with [`@compile_workload`](@ref), for example:

    @setup_workload begin
        vars = ...
        @compile_workload begin
            y = f(vars...)
            g(y)
            ⋮
        end
    end

`@setup_workload` does not force compilation (though it may happen anyway) nor intentionally capture
runtime dispatches (though they will be precompiled anyway if the runtime-callee is for a method belonging
to your package).
"""
macro setup_workload(ex::Expr)
    local iscompiling = Base.generating_output()
    # Ideally we'd like a `let` around this to prevent namespace pollution, but that seem to
    # trigger inference & codegen in undesirable ways (see #16).
    return quote
        if $iscompiling
            $(esc(ex))
        end
    end
end