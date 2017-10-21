using Test

module DeprecationTests # to test @deprecate
    f() = true

    # test the Symbol path of @deprecate
    @deprecate f1 f
    @deprecate f2 f false # test that f2 is not exported

    # test the Expr path of @deprecate
    @deprecate f3() f()
    @deprecate f4() f() false # test that f4 is not exported
    @deprecate f5(x::T) where T f()

    # test deprecation of a constructor
    struct A{T} end
    @deprecate A{T}(x::S) where {T, S} f()

    # test that @deprecate_moved can be overridden by an import
    Base.@deprecate_moved foo1234 "Foo"
    Base.@deprecate_moved bar "Bar" false
end # module
module Foo1234
    export foo1234
    foo1234(x) = x+1
end

# issue #21972
struct T21972
    @noinline function T21972()
        Base.depwarn("something", :T21972)
        new()
    end
end

@testset "@deprecate" begin
    using .DeprecationTests
    using .Foo1234
    @test foo1234(3) == 4
    @test_throws ErrorException DeprecationTests.bar(3)

    # 22845
    ex = :(module M22845; import ..DeprecationTests: bar;
                          bar(x::Number) = x + 3; end)
    @test_warn "importing deprecated binding" eval(ex)
    @test @test_nowarn(DeprecationTests.bar(4)) == 7

    # enable when issue #22043 is fixed
    # @test @test_warn "f1 is deprecated, use f instead." f1()
    # @test @test_nowarn f1()

    # @test_throws UndefVarError f2() # not exported
    # @test @test_warn "f2 is deprecated, use f instead." DeprecationTests.f2()
    # @test @test_nowarn DeprecationTests.f2()

    # @test @test_warn "f3() is deprecated, use f() instead." f3()
    # @test @test_nowarn f3()

    # @test_throws UndefVarError f4() # not exported
    # @test @test_warn "f4() is deprecated, use f() instead." DeprecationTests.f4()
    # @test @test_nowarn DeprecationTests.f4()

    # @test @test_warn "f5(x::T) where T is deprecated, use f() instead." f5(1)
    # @test @test_nowarn f5(1)

    # @test @test_warn "A{T}(x::S) where {T, S} is deprecated, use f() instead." A{Int}(1.)
    # @test @test_nowarn A{Int}(1.)

    # issue #21972
    @noinline function f21972()
        T21972()
    end
    @test_warn "deprecated" f21972()
    @test_nowarn f21972()
end
