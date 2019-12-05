module Snoop

const __inf_logging__ = Tuple{Float64,Core.MethodInstance}[]

function typeinf_ext_logged(linfo::Core.MethodInstance, params::Core.Compiler.Params)
    tstart = time()
    ret = Core.Compiler.typeinf_ext(linfo, params)
    tstop = time()
    push!(__inf_logging__, (tstop-tstart, linfo))
    return ret
end
function typeinf_ext_logged(linfo::Core.MethodInstance, world::UInt)
    tstart = time()
    ret = Core.Compiler.typeinf_ext(linfo, world)
    tstop = time()
    push!(__inf_logging__, (tstop-tstart, linfo))
    return ret
end

@noinline start_logging() = ccall(:jl_set_typeinf_func, Cvoid, (Any,), typeinf_ext_logged)
@noinline stop_logging() = ccall(:jl_set_typeinf_func, Cvoid, (Any,), Core.Compiler.typeinf_ext)

macro snoopi(cmd)
    body = Expr(:meta, :waitcompile, cmd)
    quote
        empty!(__inf_logging__)
        start_logging()
        try
            $(esc(body))
        finally
            stop_logging()
        end
        __inf_logging__
    end
end

macro badsnoopi(cmd)
    body = Expr(:meta, :waitcompile, cmd)
    somecode = quote
        ss = 0
        for i = 1:5
            global ss
            ss += i
        end
    end
    quote
        empty!(__inf_logging__)
        start_logging()
        $(esc(somecode))
        try
            $(esc(body))
        finally
            stop_logging()
        end
        __inf_logging__
    end
end

function __init__()
    # typeinf_ext_logged must be compiled before it gets run
    # We do this in __init__ to make sure it gets compiled to native code
    # (the *.ji file stores only the inferred code)
    precompile(typeinf_ext_logged, (Core.MethodInstance, Core.Compiler.Params))
    precompile(typeinf_ext_logged, (Core.MethodInstance, UInt))
    precompile(start_logging, ())
    precompile(stop_logging, ())
    nothing
end

end # Snoop

ret2() = 2
# A loop would normally force the entire body to be compiled, thus
# failing to turn on inference-snooping before compilation.
# Consequently this tests the :waitcompile mechanism.
tinf = Snoop.@snoopi begin
    s = 0
    for i = 1:5
        global s   # is this undesirable?
        s += ret2()
    end
    s
end
@test last(tinf[1]).def == first(methods(ret2))

ret3() = 3
tinf = Snoop.@badsnoopi begin
    s = 0
    for i = 1:2
        global s
        s += ret3()
    end
    s
end
@test isempty(tinf)  # the loop forced it to compile before snooping was turned on
@test ss == sum(1:5)
