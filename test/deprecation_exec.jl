# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for deprecated functionality.
#
# These can't be run with --depwarn=error, so currently require special
# treatment when run inside the test system.

using Test
using Logging

using Base: remove_linenums!

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
    @test_deprecated "something" f21972()
end

f24658() = depwarn24658()

depwarn24658() = Base.firstcaller(backtrace(), :_func_not_found_)

@testset "firstcaller" begin
    # issue #24658
    @test eval(:(if true; f24658(); end)) == (Ptr{Cvoid}(0),StackTraces.UNKNOWN)
end

# issue #25130
f25130() = Base.depwarn("f25130 message", :f25130)
# The following test is for the depwarn behavior of expressions evaluated at
# top-level, so we can't use the usual `collect_test_logs()` / `with_logger()`
testlogger = Test.TestLogger()
prev_logger = global_logger(testlogger)
# Each call at top level should be distinct. This won't be true if they're
# attributed to internal C frames (including generic dispatch machinery)
f25130()
f25130()
testlogs = testlogger.logs
@test length(testlogs) == 2
@test testlogs[1].id != testlogs[2].id
@test testlogs[1].kwargs[:caller].func == Symbol("top-level scope")
@test all(l.message == "f25130 message" for l in testlogs)
global_logger(prev_logger)


#-------------------------------------------------------------------------------
# BEGIN 0.7 deprecations

@testset "parser syntax deprecations" begin
    # #15524
    # @test (@test_deprecated Meta.parse("for a=b f() end")) == :(for a=b; f() end)
    @test_broken length(Test.collect_test_logs(()->Meta.parse("for a=b f() end"))[1]) > 0
end

# END 0.7 deprecations
