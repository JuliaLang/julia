
# @macroexpand tests
macro seven_dollar(ex)
    # simonbyrne example 18240
    isa(ex,Expr) && ex.head == :$ ? 7 : ex
end

let
    @test (@macroexpand @macroexpand x) == macroexpand(:(@macroexpand x))
    @test (@macroexpand  :(1+$y) ) == macroexpand(:( :(1+ $y)))
    @test (@macroexpand @fastmath 1+2    ) == :(Base.FastMath.add_fast(1,2))
    @test (@macroexpand @fastmath +      ) == :(Base.FastMath.add_fast)
    @test (@macroexpand @fastmath min(1) ) == :(Base.FastMath.min_fast(1))
    @test (@macroexpand @doc "" f() = @x) == Expr(:error, UndefVarError(Symbol("@x")))
    @test (@macroexpand @seven_dollar $bar) == 7
    x = 2
    @test (@macroexpand @seven_dollar 1+$x) == :(1 + $(Expr(:$, :x)))
end
