using InteractiveUtils: code_typed, gen_call_with_extracted_types_and_kwargs

function ircode_tapir(f, types)
    @nospecialize
    (ci,), = code_typed(f, types)
    mch = Base._which(Base.tuple_type_cons(typeof(f), types))
    mi = Core.Compiler.specialize_method(mch.method, mch.spec_types, mch.sparams)
    return Core.Compiler.lower_tapir_to_ircode(mi, ci)
end

"""
    @ircode_tapir f(args...)

Get `IRCode` after processed for lowering to Tapir.
"""
macro ircode_tapir(args...)
    gen_call_with_extracted_types_and_kwargs(__module__, ircode_tapir, args)
end
