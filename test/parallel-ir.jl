# TODO:
# - `Expr(:spawn)` should return a Task
# - Extend `Expr(:spawn, ssize, sticky, parallel)`
# - syncregion undefined
# - Codegen to setjmp
# - Slot transformation
# - Task analysis

macro syncregion()
    Expr(:syncregion)
end

macro spawn(token, expr)
    Expr(:spawn, esc(token), esc(expr))
end

macro sync_end(token)
    Expr(:sync, esc(token))
end

macro loopinfo(args...)
    Expr(:loopinfo, args...)
end

const tokenname = gensym(:token)
macro sync(block)
    var = esc(tokenname)
    quote
        let $var = @syncregion()
            $(esc(block))
            @sync_end($var)
        end
    end
end

macro spawn(expr)
    var = esc(tokenname)
    quote
        @spawn $var $(esc(expr))
    end
end

macro par(expr)
    @assert expr.head === :for
    token = gensym(:token)
    body = expr.args[2]
    lhs = expr.args[1].args[1]
    range = expr.args[1].args[2]
    quote
        let $token = @syncregion()
            for $(esc(lhs)) = $(esc(range))
                @spawn $token $(esc(body))
            end
            @sync_end $token
        end
    end
end

function f()
    let token = @syncregion()
        @spawn token begin
            1 + 1
        end
        @sync_end token
    end
end

# In a perfect world we want `Expr(:spawn)` to return
# a Task
function g()
    T = @spawn begin
        1 + 1
    end
    @show typeof(T)
end

function taskloop(N)
    let token = @syncregion()
        for i in 1:N
            @spawn token begin
                1 + 1
            end
        end
        @sync_end token
    end
end

function taskloop2(N)
    @sync for i in 1:N
        @spawn begin
            1 + 1
        end
    end
end

function taskloop3(N)
    @par for i in 1:N
        1+1
    end
end

function vecadd(out, A, B)
    @assert length(out) == length(A) == length(B)
    @inbounds begin
        @par for i in 1:length(out)
            out[i] = A[i] + B[i]
        end
    end
    return out
end

function fib(N)
    if N <= 1
        return N
    end
    token = @syncregion()
    x1 = Ref{Int64}()
    @spawn token begin
        x1[]  = fib(N-1)
    end
    x2 = fib(N-2)
    @sync_end token
    return x1[] + x2
end

###
# Interesting corner cases and broken IR
###

##
# Parallel regions with errors are tricky
# #1  detach within %sr, #2, #3
# #2  ...
#     unreachable()
#     reattach within %sr, #3
# #3  sync within %sr
#
# Normally a unreachable get's turned into a ReturnNode(),
# but that breaks the CFG. So we need to detect that we are
# in a parallel region.
#
# Question:
#   - Can we elimante a parallel region that throws?
#     Probably if the sync is dead as well. We could always
#     use the serial projection and serially execute the region.

function vecadd_err(out, A, B)
    @assert length(out) == length(A) == length(B)
    @inbounds begin
        @par for i in 1:length(out)
            out[i] = A[i] + B[i]
            error()
        end
    end
    return out
end

# This function is broken due to the PhiNode
@noinline function fib2(N)
    if N <= 1
        return N
    end
    token = @syncregion()
    x1 = 0
    @spawn token begin
        x1  = fib2(N-1)
    end
    # PHI-node for x1 doesn't work.
    x2 = fib2(N-2)
    @sync_end token
    return x1 + x2
end
