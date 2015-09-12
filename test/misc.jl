# This file is a part of Julia. License is MIT: http://julialang.org/license

# Tests that do not really go anywhere else

# Test info
@test contains(sprint(io->info(io,"test")), "INFO:")
@test contains(sprint(io->info(io,"test")), "INFO: test")
@test contains(sprint(io->info(io,"test ",1,2,3)), "INFO: test 123")
@test contains(sprint(io->info(io,"test", prefix="MYINFO: ")), "MYINFO: test")

# Test warn
@test contains(sprint(io->Base.warn_once(io,"test")), "WARNING: test")
@test isempty(sprint(io->Base.warn_once(io,"test")))

@test contains(sprint(io->warn(io)), "WARNING:")
@test contains(sprint(io->warn(io, "test")), "WARNING: test")
@test contains(sprint(io->warn(io, "test ",1,2,3)), "WARNING: test 123")
@test contains(sprint(io->warn(io, "test", prefix="MYWARNING: ")), "MYWARNING: test")
@test contains(sprint(io->warn(io, "testonce", once=true)), "WARNING: testonce")
@test isempty(sprint(io->warn(io, "testonce", once=true)))
@test !isempty(sprint(io->warn(io, "testonce", once=true, key=hash("testonce",hash("testanother")))))
let bt = backtrace()
    ws = split(chomp(sprint(io->warn(io,"test", bt))), '\n')
    bs = split(chomp(sprint(io->Base.show_backtrace(io,bt))), '\n')
    @test contains(ws[1],"WARNING: test")
    for (l,b) in zip(ws[2:end],bs)
        @test contains(l, b)
    end
end

# test assert() method
@test_throws AssertionError assert(false)
let res = assert(true)
    @test res === nothing
end
let
    ex = @test_throws AssertionError begin
        assert(false)
        error("unexpected")
    end
    @test isempty(ex.msg)
end

# test @assert macro
@test_throws AssertionError (@assert 1 == 2)
@test_throws AssertionError (@assert false)
@test_throws AssertionError (@assert false "this is a test")
@test_throws AssertionError (@assert false "this is a test" "another test")
@test_throws AssertionError (@assert false :a)
let
    ex = @test_throws AssertionError begin
        @assert 1 == 2
        error("unexpected")
    end
    @test contains(ex.msg, "1 == 2")
end
# test @assert message
let
    ex = @test_throws AssertionError begin
        @assert 1 == 2 "this is a test"
        error("unexpected")
    end
    @test ex.msg == "this is a test"
end
# @assert only uses the first message string
let
    ex = @test_throws AssertionError begin
        @assert 1 == 2 "this is a test" "this is another test"
        error("unexpected")
    end
    @test ex.msg == "this is a test"
end
# @assert calls string() on second argument
let
    ex = @test_throws AssertionError begin
        @assert 1 == 2 :random_object
        error("unexpected")
    end
    @test !contains(ex.msg,  "1 == 2")
    @test contains(ex.msg, "random_object")
end
# if the second argument is an expression, c
let deepthought(x, y) = 42
    ex = @test_throws AssertionError begin
        @assert 1 == 2 string("the answer to the ultimate question: ",
                              deepthought(6, 9))
    end
    @test ex.msg == "the answer to the ultimate question: 42"
end

let # test the process title functions, issue #9957
    oldtitle = Sys.get_process_title()
    Sys.set_process_title("julia0x1")
    @test Sys.get_process_title() == "julia0x1"
    Sys.set_process_title(oldtitle)
    @test Sys.get_process_title() == oldtitle
end


# test gc_enable/disable
@test gc_enable(true)
@test gc_enable(false)
@test gc_enable(false) == false
@test gc_enable(true) == false
@test gc_enable(true)

# test methodswith
immutable NoMethodHasThisType end
@test isempty(methodswith(NoMethodHasThisType))
@test !isempty(methodswith(Int))

# PR #10984
# Disable on windows because of issue (missing flush) when redirecting STDERR.
let
    redir_err = "redirect_stderr(STDOUT)"
    exename = joinpath(JULIA_HOME, Base.julia_exename())
    script = "$redir_err; f(a::Number, b...) = 1;f(a, b::Number) = 1"
    warning_str = readall(`$exename --startup-file=no -e $script`)
    @test contains(warning_str, "f(Any, Number)")
    @test contains(warning_str, "f(Number, Any...)")
    @test contains(warning_str, "f(Number, Number)")

    script = "$redir_err; module A; f() = 1; end; A.f() = 1"
    warning_str = readall(`$exename --startup-file=no -e $script`)
    @test contains(warning_str, "f()")
end

# lock / unlock
let l = ReentrantLock()
    lock(l)
    unlock(l)
    @test_throws ErrorException unlock(l)
end

# timing macros

# test that they don't introduce global vars
global v11801, t11801, names_before_timing
names_before_timing = names(current_module(), true)

let t = @elapsed 1+1
    @test isa(t, Real) && t >= 0
end

let
    val, t = @timed sin(1)
    @test val == sin(1)
    @test isa(t, Real) && t >= 0
end

# problem after #11801 - at global scope
t11801 = @elapsed 1+1
@test isa(t11801,Real) && t11801 >= 0
v11801, t11801 = @timed sin(1)
@test v11801 == sin(1)
@test isa(t11801,Real) && t11801 >= 0

@test names(current_module(), true) == names_before_timing

# interactive utilities

import Base.summarysize
@test summarysize(Core) > summarysize(Core.Inference) > Core.sizeof(Core)
@test summarysize(Base) > 10_000*sizeof(Int)
module _test_whos_
export x
x = 1.0
end
@test sprint(whos, Main, r"^$") == ""
let v = sprint(whos, _test_whos_)
    @test contains(v, "x      8 bytes  Float64 : 1.0")
end

# issue #13021
let ex = try
    Main.x13021 = 0
    nothing
catch ex
    ex
end
    @test isa(ex, ErrorException) && ex.msg == "cannot assign variables in other modules"
end
