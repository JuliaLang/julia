module M
    using JuliaLowering: JuliaLowering, @ast, @chk, adopt_scope
    using JuliaSyntax

    # Introspection
    macro __MODULE__()
        __context__.scope_layer.mod
    end

    macro __FILE__()
        JuliaLowering.filename(__context__.macroname)
    end

    macro __LINE__()
        JuliaLowering.source_location(__context__.macroname)[1]
    end

    # Macro with local variables
    JuliaLowering.include(M, "demo_include_2.jl")

    someglobal = "global in module M"

    # Macro with local variables
    macro foo(ex)
        quote
            x = "`x` from @foo"
            (x, someglobal, A.@bar $ex)
            #(x, someglobal, $ex, A.@bar($ex), A.@bar(x))
        end
    end

    macro set_a_global(val)
        quote
            global a_global = $val
        end
    end

    macro set_global_in_parent(ex)
        e1 = adopt_scope(:(sym_introduced_from_M), __context__)
        quote
            $e1 = $ex
        end
    end

    macro baz(ex)
        quote
            let $ex = 10
                $ex
            end
        end
    end

    macro make_module()
        :(module X
              blah = 10
          end)
    end

    macro return_a_value()
        42
    end

    macro nested_return_a_value()
        :(
            @return_a_value
        )
    end

    macro inner()
        :(2)
    end

    macro outer()
        :((1, @inner))
    end
end
