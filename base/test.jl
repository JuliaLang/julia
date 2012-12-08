module Test
export @test, @test_fails

abstract Result
type Success <: Result
    expr
end
type Failure <: Result
    expr
end
type Error <: Result
    expr
    err
end

default_handler(r::Success) = nothing
default_handler(r::Failure) = error("test failed: $(r.expr)")
default_handler(r::Error)   = error("test error during $(r.expr)\n$(r.err)")

const handlers = [default_handler]

function do_test(thk, qex)
    handlers[end](try
        thk() ? Success(qex) : Failure(qex)
    catch err
        Error(qex,err)
    end)
end

function do_test_fails(thk, qex)
    handlers[end](try
        thk()
        Failure(qex)
    catch err
        Success(qex)
    end)
end

macro test(ex)
    :(do_test(()->($(esc(ex))),$(expr(:quote,ex))))
end

macro test_fails(ex)
    :(do_test_fails(()->($(esc(ex))),$(expr(:quote,ex))))
end

end # module
