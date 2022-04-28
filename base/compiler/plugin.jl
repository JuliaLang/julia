# this simple code assumes a plugin AbstractInterpreter manages its own code cache
# in a way that it is totally separated from the native code cache

# TODO more composable way
global PLUGIN_INTERPRETER::AbstractInterpreter = NativeInterpreter()
isplugin(interp::AbstractInterpreter) = let
    global PLUGIN_INTERPRETER
    interp === PLUGIN_INTERPRETER
end

function new_opaque_closure(src::CodeInfo, nargs::Int, @nospecialize(rt),
    @nospecialize(env...))
    @assert src.inferred "unoptimized IR unsupported"
    argt = argtypes_to_type((src.slottypes::Vector{Any})[2:nargs+1])

    # M = src.parent.def
    # sig = Base.tuple_type_tail(src.parent.specTypes)

    return ccall(:jl_new_opaque_closure_from_code_info, Any,
        (Any, Any, Any, Any, Any, Cint, Any, Cint, Cint, Any),
        argt, Union{}, rt, @__MODULE__, src, 0, nothing, nargs, false, env)
end

function execute_with_plugin(f, args...;
    world::UInt = get_world_counter(),
    interp::AbstractInterpreter = PLUGIN_INTERPRETER)
    global PLUGIN_INTERPRETER = interp
    tt = Tuple{Core.Typeof(f), Any[Core.Typeof(args[i]) for i = 1:length(args)]...}
    matches = _methods_by_ftype(tt, -1, world)
    matches !== nothing || throw(MethodError(f, args, world))
    length(matches) ≠ 1 && throw(MethodError(f, args, world))
    m = first(matches)::MethodMatch
    src, rt = typeinf_code(interp,
        m.method, m.spec_types, m.sparams, true)
    if src === nothing
        # builtin, or bad generated function
        return f(args...) # XXX
    end
    # TODO support varargs
    oc = new_opaque_closure(src, length(args), rt)
    return oc(args...)
end

module Plugin
import ..execute_with_plugin
export execute_with_plugin
end
