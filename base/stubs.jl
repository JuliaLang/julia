module Stubs

module Random
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

    import Base: rand, randn
    function rand(args...)
        Base.invoke_in_world(delay_initialize(), rand, args...)
    end

    function randn(args...)
        Base.invoke_in_world(delay_initialize(), randn, args...)
    end
end

Base.Docs.getdoc(::typeof(Base.rand)) = (Random.delay_initialize(); nothing)
Base.Docs.getdoc(::typeof(Base.randn)) = (Random.delay_initialize(); nothing)

function delete_stubs(mod)
    for name in names(mod, imported=true)
        if name == :delay_initialize
            continue
        end
        obj = getglobal(mod, name)
        if obj isa Function
            ms = Base.methods(obj, mod)
            for m in ms
                Base.delete_method(m)
            end
        end
    end
end

end
