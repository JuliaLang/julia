# This file is a part of Julia. License is MIT: https://julialang.org/license

# this tests that Expr(:top) is resolved correctly

module TestTop
    string(x...) = Base.string("TestTop: ", x...)
    module NewTop
        Base.@topmodule
        import ..string
        hello(x) = "hello world \"$x\""
        module Sub
            hello(x) = "goodnight world \"$x\""
        end
    end
    hello(x) = "bonjour world \"$x\""
end

hello(x) = "ciao world \"$x\""
@test hello("user") == "ciao world \"user\""
@test TestTop.hello("user") == "bonjour world \"user\""
@test TestTop.NewTop.hello("user") == "TestTop: hello world \"user\""
@test TestTop.NewTop.Sub.hello("user") == "TestTop: goodnight world \"user\""
@test 25 < length(names(Base.TopModule)) < 75 # currently about 40
