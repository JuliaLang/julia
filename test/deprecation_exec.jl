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
    # Test empty logs for meta.parse depwarn argument.
    @test_logs Meta.parse("1.+2", depwarn=false)

    # #19089
    @test (@test_deprecated Meta.parse("1.+2")) == :(1 .+ 2)

    # #16356
    @test (@test_deprecated Meta.parse("0xapi")) == :(0xa * pi)

    # #22523 #22712
    @test (@test_deprecated Meta.parse("a?b:c"))    == :(a ? b : c)
    @test (@test_deprecated Meta.parse("a ?b:c"))   == :(a ? b : c)
    @test (@test_deprecated Meta.parse("a ? b:c"))  == :(a ? b : c)
    @test (@test_deprecated Meta.parse("a ? b :c")) == :(a ? b : c)
    @test (@test_deprecated Meta.parse("?")) == Symbol("?")

    # #13079
    @test (@test_deprecated Meta.parse("1<<2*3")) == :(1<<(2*3))

    # ([#19157], [#20418]).
    @test remove_linenums!(@test_deprecated Meta.parse("immutable A; end")) ==
          remove_linenums!(:(struct A; end))
    @test remove_linenums!(@test_deprecated Meta.parse("type A; end")) ==
          remove_linenums!(:(mutable struct A; end))

    # #19987
    @test remove_linenums!(@test_deprecated Meta.parse("try ; catch f() ; end")) ==
          remove_linenums!(:(try ; catch; f() ; end))

    # #15524
    # @test (@test_deprecated Meta.parse("for a=b f() end")) == :(for a=b; f() end)
    @test_broken length(Test.collect_test_logs(()->Meta.parse("for a=b f() end"))[1]) > 0

    # #23076
    @test (@test_deprecated Meta.parse("[a,b;]")) == :([a;b])

    # #24452
    @test (@test_deprecated Meta.parse("(a...)")) == :((a...,))
end


@testset "lowering syntax deprecations" begin
    # #16295
    @test_deprecated Meta.lower(@__MODULE__, :(A.(:+)(a,b) = 1))

    # #11310
    @test_deprecated r"parametric method syntax" Meta.lower(@__MODULE__, :(f{T}(x::T) = 1))

    # #17623
    @test_deprecated r"Deprecated syntax `function .+(...)`" Meta.lower(@__MODULE__, :(function .+(a,b) ; end))

    # #21774 (more uniform let expressions)
    @test_deprecated Meta.lower(@__MODULE__, Expr(:let, :a))
    @test_deprecated Meta.lower(@__MODULE__, Expr(:let, :a, :(a=1), :(b=1)))

    # #23157 (Expression heads for types renamed)
    @test_deprecated Meta.lower(@__MODULE__, Expr(:type, true, :A, Expr(:block)))
    @test_deprecated Meta.lower(@__MODULE__, Expr(:bitstype, 32, :A))

    # #15032
    @test_deprecated Meta.lower(@__MODULE__, :(a.(b) = 1))

    # #5332
    @test_deprecated Meta.lower(@__MODULE__, :(a.'))

    # #19324
    @test_deprecated r"implicit assignment to global" eval(
           :(module M19324
                 x=1
                 for i=1:10
                     x += i
                 end
             end))

    # #24221
    @test_deprecated r"underscores as an rvalue" Meta.lower(@__MODULE__, :(a=_))

    # #22314
    @test_deprecated r"Use of final value of loop variable `i`.*is deprecated. In the future the variable will be local to the loop instead." Meta.lower(@__MODULE__, :(
        function f()
            i=0
            for i=1:10
            end
            i
        end))
    @test_deprecated r"Loop variable `i` overwrites a variable in an enclosing scope" eval(:(
        module M22314
            i=10
            for i=1:10
            end
        end))

    # #6080
    @test_deprecated r"Syntax `&argument`.*is deprecated" Meta.lower(@__MODULE__, :(ccall(:a, Cvoid, (Cint,), &x)))

end

module LogTest
    function bar(io)
        info(io,"barinfo")
        warn(io,"barwarn")
        Base.display_error(io,"barerror",backtrace())
    end
    function pooh(io)
        info(io,"poohinfo")
        warn(io,"poohwarn")
        Base.display_error(io,"pooherror",backtrace())
    end
end
function foo(io)
    info(io,"fooinfo")
    warn(io,"foowarn")
    Base.display_error(io,"fooerror",backtrace())
end

# Silence the flurry of depwarns for now.
with_logger(NullLogger()) do

@testset "Deprecated logging" begin

# Test info
@test contains(sprint(info, "test"), "INFO:")
@test contains(sprint(info, "test"), "INFO: test")
@test contains(sprint(info, "test ", 1, 2, 3), "INFO: test 123")
@test contains(sprint(io->info(io,"test", prefix="MYINFO: ")), "MYINFO: test")

# Test warn
@test contains(sprint(Base.warn_once, "test"), "WARNING: test")
@test isempty(sprint(Base.warn_once, "test"))

@test contains(sprint(warn), "WARNING:")
@test contains(sprint(warn, "test"), "WARNING: test")
@test contains(sprint(warn, "test ", 1, 2, 3), "WARNING: test 123")
@test contains(sprint(io->warn(io, "test", prefix="MYWARNING: ")), "MYWARNING: test")
@test contains(sprint(io->warn(io, "testonce", once=true)), "WARNING: testonce")
@test isempty(sprint(io->warn(io, "testonce", once=true)))
@test !isempty(sprint(io->warn(io, "testonce", once=true, key=hash("testonce",hash("testanother")))))
let bt = backtrace()
    ws = split(chomp(sprint(io->warn(io, "test", bt = bt))), '\n')
    bs = split(chomp(sprint(Base.show_backtrace, bt)), '\n')
    @test contains(ws[1],"WARNING: test")
    for (l,b) in zip(ws[2:end],bs[2:end])
        @test contains(l, b)
    end
end

# PR #16213
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))


logging(devnull, LogTest, :bar;  kind=:info)
@test all(contains.(sprint(LogTest.bar), ["WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(devnull, LogTest;  kind=:info)
@test all(contains.(sprint(LogTest.bar), ["WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(devnull;  kind=:info)
@test all(contains.(sprint(LogTest.bar), ["WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(kind=:info)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))


logging(devnull, LogTest, :bar;  kind=:warn)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(devnull, LogTest;  kind=:warn)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(devnull;  kind=:warn)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "ERROR: \"fooerror\""]))

logging(kind=:warn)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))


logging(devnull, LogTest, :bar;  kind=:error)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "WARNING: barwarn"]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(devnull, LogTest;  kind=:error)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "WARNING: barwarn"]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn"]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(devnull;  kind=:error)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "WARNING: barwarn"]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn"]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn"]))

logging(kind=:error)
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))


logging(devnull, LogTest, :bar)
@test sprint(LogTest.bar) == ""
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(devnull, LogTest)
@test sprint(LogTest.bar) == ""
@test sprint(LogTest.pooh) == ""
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

logging(devnull)
@test sprint(LogTest.bar) == ""
@test sprint(LogTest.pooh) == ""
@test sprint(foo) == ""

logging()
@test all(contains.(sprint(LogTest.bar), ["INFO: barinfo", "WARNING: barwarn", "ERROR: \"barerror\""]))
@test all(contains.(sprint(LogTest.pooh), ["INFO: poohinfo", "WARNING: poohwarn", "ERROR: \"pooherror\""]))
@test all(contains.(sprint(foo), ["INFO: fooinfo", "WARNING: foowarn", "ERROR: \"fooerror\""]))

end # @testset

end

# END 0.7 deprecations
