module Tapir

# As stdlib for now until https://github.com/JuliaLang/julia/pull/51514 is fully fixed.

function __init__()
    bcfile = joinpath(dirname(pathof(Tapir)), "..", "res", "tapir.ll")
    args = ["", "-tapir-runtime-bc-path=$bcfile"]
    ccall((:LLVMParseCommandLineOptions, Base.libllvm_path()), Cvoid, (Cint, Ptr{Cstring}, Cstring), length(args), args, C_NULL)
end

macro sync_end(token)
    Expr(:sync, esc(token))
end

macro syncregion()
    Expr(:syncregion)
end

const tokenname = gensym(:token)

"""
    Tapir.@sync block
"""
macro sync(block)
    var = esc(tokenname)
    quote
        let $var = @syncregion()
            v = $(esc(block))
            @sync_end($var)
            v
        end
    end
end

"""
    Tapir.@spawn expression
"""
macro spawn(expr)
    Expr(:spawn, esc(tokenname), esc(Expr(:block, expr)))
end

###
# Runtime
###

import Base: @ccallable

const TaskGroup = Channel{Any}
taskgroup() = Channel{Any}(Inf)::TaskGroup

struct StackFrame
    tg::TaskGroup
end

mutable struct TapirTask
   ptr::Ptr{Cvoid}
   data::Vector{UInt8}
end
(obj::TapirTask)() = ccall(obj.ptr, Cvoid, (Ptr{UInt8},), obj.data) 

@ccallable function __rts_enter_frame(sf::Ptr{StackFrame})::Cvoid
  tg = taskgroup()
  GC.@preserve tg begin
    Base.unsafe_store!(sf, StackFrame(tg))
  end
  # This is potentially fishy for GC.
  return nothing
end

@ccallable function __rts_leave_frame(sf::Ptr{StackFrame})::Cvoid
  tg = Base.unsafe_load(sf).tg::TaskGroup
  close(tg)
  return nothing
end

@ccallable function __rts_sync(sf::Ptr{StackFrame})::Cvoid
  tg = Base.unsafe_load(sf).tg::TaskGroup
  Base.sync_end(tg)
  return nothing
end

@ccallable function __rts_spawn(sf::Ptr{StackFrame}, func::Ptr{Cvoid}, data::Ptr{Cvoid}, sz::Int, align::Int)::Cvoid
  tg = Base.unsafe_load(sf).tg::TaskGroup
  buf = ccall(:aligned_alloc, Ptr{Cvoid}, (Csize_t, Csize_t), align, sz)
  Base.Libc.memcpy(buf, data, sz)
  buf = Base.unsafe_wrap(Vector{UInt8}, Base.unsafe_convert(Ptr{UInt8}, buf), sz; own = true)
  t = Base.Task(TapirTask(func, buf))
  t.sticky = false
  Base.yield(t) # unfair form of schedule; child-first
  push!(tg, t)
  return nothing
end

@ccallable function __rts_loop_grainsize_64(len::Int64)::Int64
  return 8
end
@ccallable function __rts_loop_grainsize_32(len::Int32)::Int32
  return 16
end
@ccallable function __rts_loop_grainsize_16(len::Int16)::Int16
  return 32
end
@ccallable function __rts_loop_grainsize_8(len::Int8)::Int8
  return 64
end

###
# Parallel constructs
###

loop_spawning_strategy() = (Symbol("tapir.loop.spawn.strategy"), 2)
loop_grainsize(n) = (Symbol("tapir.loop.grainsize"), convert(Int, n))
simd() = Symbol("julia.simdloop")
ivdep() = Symbol("julia.ivdep")

@eval function foreach(f::F, iterator) where F
    @sync for I in iterator
        @spawn @inline f(I)
        $(Expr(:loopinfo, simd(), ivdep(), loop_spawning_strategy(), loop_grainsize(1)))
    end
    return nothing
end

end
