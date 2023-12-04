module Stubs

macro stub(old, mod)
    dep_message = """: `$old` has been moved to the standard library package `$mod`.
                        Add `using $mod` to your imports."""
    return Expr(:toplevel,
        quote
            import Base: $old
            function $(esc(old))(args...)
                Base.depwarn($dep_message, $(QuoteNode(old)))
                Base.invoke_in_world($(esc(:delay_initialize))(), $(esc(old)), args...)
            end
            Base.Docs.getdoc(::typeof($(esc(old)))) = ($(esc(:delay_initialize))(); nothing)
        end)
end

module Random
    import ..Stubs: @stub
    let Random_PkgID = Base.PkgId(Base.UUID(0x9a3f8284_a2c9_5f02_9a11_845980a1fd5c), "Random")
        RANDOM_MODULE_REF = Ref{Module}()

        global delay_initialize
        function delay_initialize()
            if !isassigned(RANDOM_MODULE_REF)
                RANDOM_MODULE_REF[] = Base.require(Random_PkgID)
            end
            return ccall(:jl_module_world, Csize_t, (Any,), RANDOM_MODULE_REF[])
        end
    end
    @stub rand Random
    @stub randn Random
end

function delete_stubs(mod)
    for name in names(mod, imported=true)
        if name == :delay_initialize
            continue
        end
        obj = getglobal(mod, name)
        if obj isa Function
            ms = Base.methods(obj, mod)
            for m in ms
                ccall(:jl_push_newly_deleted, Cvoid, (Any,), m)
                ccall(:jl_method_table_disable_incremental, Cvoid, (Any, Any), Base.get_methodtable(m), m)
            end
        end
    end
end

end
