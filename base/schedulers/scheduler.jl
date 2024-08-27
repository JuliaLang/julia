# This file is a part of Julia. License is MIT: https://julialang.org/license

module Scheduler

cong(max::UInt32) = iszero(max) ? UInt32(0) : ccall(:jl_rand_ptls, UInt32, (UInt32,), max) + UInt32(1)
include("schedulers/partr.jl")
include("schedulers/workstealing.jl")

const ChosenScheduler = Workstealing



# Scheduler interface:
    # enqueue! which pushes a runnable Task into it
    # dequeue! which pops a runnable Task from it
    # checktaskempty which returns true if the scheduler has no available Tasks

enqueue!(t::Task) = ChosenScheduler.enqueue!(t)
dequeue!() = ChosenScheduler.dequeue!()
checktaskempty() = ChosenScheduler.checktaskempty()

end