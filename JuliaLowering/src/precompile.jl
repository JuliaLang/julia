# exercise the whole lowering pipeline
if Base.get_bool_env("JULIA_LOWERING_PRECOMPILE", true)
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
        lwr = to_lowered_expr(@__MODULE__, lwrst)
        @assert Meta.isexpr(lwr, :thunk) && only(lwr.args) isa Core.CodeInfo
    end
end
