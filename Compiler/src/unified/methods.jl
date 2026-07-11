# Method-level plumbing: get lowered UnifiedIR for existing methods (entry
# converter) and define runtime methods from UnifiedIR bodies (exit
# converter) — the runtime-interaction boundary in both directions.

"""
    lowered_ir(f, argtypes::Type{<:Tuple}; world) -> UnifiedIR.IR

Look up the method matching `f(argtypes...)`, fetch its uncompressed lowered
source, and entry-convert to UnifiedIR (cfg-wrap mode).
"""
function lowered_ir(@nospecialize(f), @nospecialize(argtypes);
                    world::UInt = Base.get_world_counter())
    tt = Base.signature_type(f, argtypes)
    match = Base._which(tt; world)
    mi = Compiler.specialize_method(match)
    ci = Base.uncompressed_ir(mi.def::Method)
    ir = codeinfo_to_ir(ci; nargs = Int((mi.def::Method).nargs), name = (mi.def::Method).name)
    ir.meta[:method_instance] = mi
    ir.meta[:slotnames] = ci.slotnames
    ir.sptypes = Any[t for t in mi.sparam_vals]
    ir.meta[:sptypes_lat] = sptypes_lattice(mi)
    return ir
end

"""
    define_ir_method!(mod::Module, name::Symbol, nargs::Int, ir) -> function

Exit-convert `ir` and define it as a runtime method `mod.name` taking
`nargs-1` untyped arguments (arg 1 is `#self#`). Returns the callable.
"""
function define_ir_method!(mod::Module, name::Symbol, nargs::Int, ir::UnifiedIR.IR)
    ci = ir_to_codeinfo(ir; name)
    # a fresh generic function + jl_method_def, mirroring lowered :method exprs
    f = Core.eval(mod, :(function $name end))
    atypes = Core.svec(typeof(f), (Any for _ in 2:nargs)...)
    argdata = Core.svec(atypes, Core.svec(), LineNumberNode(0, :unified_ir))
    ccall(:jl_method_def, Any, (Any, Ptr{Cvoid}, Any, Any),
          argdata, C_NULL, ci, mod)
    return f
end

"""
    roundtrip_codeinfo(f, argtypes) -> (original::CodeInfo, round::CodeInfo, ir)

Differential-harness helper: lowered source → UnifiedIR → lowered source.
"""
function roundtrip_codeinfo(@nospecialize(f), @nospecialize(argtypes);
                            world::UInt = Base.get_world_counter())
    tt = Base.signature_type(f, argtypes)
    match = Base._which(tt; world)
    mi = Compiler.specialize_method(match)
    m = mi.def::Method
    ci = Base.uncompressed_ir(m)
    ir = codeinfo_to_ir(ci; nargs = Int(m.nargs), name = m.name)
    ir.meta[:slotnames] = ci.slotnames
    ci2 = ir_to_codeinfo(ir; name = m.name)
    return (ci, ci2, ir)
end

"""
    redefine_through_ir(f, argtypes; mod) -> callable

Round-trip `f`'s lowered code through UnifiedIR and define the result as a
fresh function; the differential harness compares behaviors.
"""
function redefine_through_ir(@nospecialize(f), @nospecialize(argtypes);
                             mod::Module = @__MODULE__)
    ci, ci2, ir = roundtrip_codeinfo(f, argtypes)
    nargs = Int(ci.nargs)
    name = gensym(nameof(f))
    return define_ir_method!(mod, name, nargs, ir)
end
