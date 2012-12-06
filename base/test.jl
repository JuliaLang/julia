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

macro test(ex)
    qex = expr(:quote,ex)
    quote
        handlers[length(handlers)](try
            $(esc(ex)) ? Success($qex) : Failure($qex)
        catch err
            Error($qex,err)
        end)
    end
end

macro test_fails(ex)
    qex = expr(:quote,ex)
    quote
        handlers[length(handlers)](try
            $(esc(ex))
            Failure($qex)
        catch
            Success($qex)
        end)
    end
end

end # module
