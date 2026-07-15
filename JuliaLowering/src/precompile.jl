@static if Base.get_bool_env("JULIA_LOWERING_PRECOMPILE", true)
    # Exercise lowering directly so this also works on runtimes where evaluating
    # JuliaLowering output through `include_string` is not yet compatible.
    thunks = String[
        """
        function foo(xxx, yyy)
            @nospecialize xxx
            return Pair{Any,Any}(typeof(xxx), typeof(yyy))
        end
        """

        """
        struct Foo
            x::Int
            Foo(x::Int) = new(x)
            # Foo() = new()
        end
        """
    ]
    for thunk in thunks
        stream = JuliaSyntax.ParseStream(thunk)
        JuliaSyntax.parse!(stream; rule=:all)
        st0 = JuliaSyntax.build_tree(SyntaxTree, stream; filename=@__FILE__)
        lwrst = lower(@__MODULE__, st0[1])
        lwr = to_lowered_expr(lwrst)
        @assert Meta.isexpr(lwr, :thunk) && only(lwr.args) isa Core.CodeInfo
    end

    @static if VERSION >= v"1.14.0-DEV.2635"
        workload = raw"""
        _precompile_kwf(x; y=1, z=2) = x + y + z

        function _precompile_destr(t)
            (a, b) = t
            a + b
        end

        macro _precompile_plus1(ex)
            :($(esc(ex)) + 1)
        end
        _precompile_usemac(x) = @_precompile_plus1(x)

        @generated function _precompile_genf(x)
            :(x + 1)
        end

        # Fire everything so inference and the generator run during the build.
        _precompile_kwf(1; y = 2)
        _precompile_destr((1, 2))
        _precompile_usemac(3)
        _precompile_genf(1.0)
        """
        include_string(@__MODULE__, workload, @__FILE__; expr_compat_mode=true)
    end
end
