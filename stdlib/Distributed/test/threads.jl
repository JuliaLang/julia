using Test
using Distributed, Base.Threads
using Base.Iterators: product

exeflags = ("--startup-file=no",
            "--check-bounds=yes",
            "--depwarn=error",
            "--threads=2")
procs_added = addprocs(2; exeflags)

@everywhere procs_added using Base.Threads

function call_on(f, wid, tid)
  remotecall(wid) do
    t = Task(f)
    ccall(:jl_set_task_tid, Cvoid, (Any, Cint), t, tid-1)
    schedule(t)
    @assert threadid(t) == tid
    t
  end
end

# Run function on process holding the data to only serialize the result of f.
# This becomes useful for things that cannot be serialized (e.g. running tasks)
# or that would be unnecessarily big if serialized.
fetch_from_owner(f, rr) = remotecall_fetch(f∘fetch, rr.where, rr)

isdone(rr) = fetch_from_owner(istaskdone, rr)
isfailed(rr) = fetch_from_owner(istaskfailed, rr)

@testset "RemoteChannel allows put!/take! from thread other than 1" begin
  ps = product(procs_added, procs_added)
  ts = product(1:2, 1:2)
  chan_id = first(procs_added)
  timeout = 1.0
  @testset "from process $p1 to $p2 via $chan_id" for (p1, p2) in ps
    @testset "from thread $p1.$t1 to $p2.$t2" for (t1, t2) in ts
      chan = RemoteChannel(chan_id)
      send = call_on(p1, t1) do
        put!(chan, nothing)
      end
      recv = call_on(p2, t2) do
        take!(chan)
      end
      timedwait(() -> isdone(send) && isdone(recv), timeout)
      @test isdone(send)
      @test isdone(recv)
      @test !isfailed(send)
      @test !isfailed(recv)
    end
  end
  rmprocs(procs_added)
end
