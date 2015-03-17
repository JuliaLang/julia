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
    try
        assert(false)
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test isempty(ex.msg)
    end
end
let
    try
        assert(false, "this is a test")
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "this is a test"
    end
end

# test @assert macro
@test_throws AssertionError (@assert 1 == 2)
@test_throws AssertionError (@assert false)
@test_throws AssertionError (@assert false "this is a test")
@test_throws AssertionError (@assert false "this is a test" "another test")
@test_throws AssertionError (@assert false :a)
let
    try
        @assert 1 == 2
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test contains(ex.msg, "1 == 2")
    end
end
# test @assert message
let
    try
        @assert 1 == 2 "this is a test"
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "this is a test"
    end
end
# @assert only uses the first message string
let
    try
        @assert 1 == 2 "this is a test" "this is another test"
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "this is a test"
    end
end
# @assert calls string() on second argument
let
    try
        @assert 1 == 2 :random_object
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test !contains(ex.msg,  "1 == 2")
        @test contains(ex.msg, "random_object")
    end
end
# if the second argument is an expression, c
let deepthought(x, y) = 42
    try
        @assert 1 == 2 string("the answer to the ultimate question: ", deepthought(6,9))
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "the answer to the ultimate question: 42"
    end
end

let # test the process title functions, issue #9957
    oldtitle = Sys.get_process_title()
    Sys.set_process_title("julia0x1")
    @test Sys.get_process_title() == "julia0x1"
    Sys.set_process_title(oldtitle)
    @test Sys.get_process_title() == oldtitle
end


# test gc_enable/disable
@test gc_enable()
@test gc_disable()
@test gc_disable() == false
@test gc_enable() == false
@test gc_enable()
