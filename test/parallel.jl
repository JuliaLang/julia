# NOTE: worker processes cannot add more workers, only the client process can. 
require("testdefs.jl")

if nprocs() < 2
    remotecall_fetch(1, () -> addprocs(1))
end

id_me = myid()
id_other = filter(x -> x != id_me, procs())[rand(1:(nprocs()-1))]

@test fetch(@spawnat id_other myid()) == id_other
@test @fetchfrom id_other begin myid() end == id_other
@fetch begin myid() end

d = drand((200,200), [id_me, id_other])
s = convert(Array, d[1:150, 1:150])
a = convert(Array, d)
@test a[1:150,1:150] == s

@test fetch(@spawnat id_me localpart(d)[1,1]) == d[1,1]
@test fetch(@spawnat id_other localpart(d)[1,1]) == d[1,101]

# Test @parallel load balancing - all processors should get either M or M+1
# iterations out of the loop range for some M.
if nprocs() < 4
    remotecall_fetch(1, () -> addprocs(4 - nprocs()))
end
workloads = hist(@parallel((a,b)->[a,b], for i=1:7; myid(); end), nprocs())[2]
@test max(workloads) - min(workloads) <= 1

# @parallel reduction should work even with very short ranges
@test @parallel(+, for i=1:2; i; end) == 3

# Testing timedwait on multiple RemoteRefs
rr1 = RemoteRef()
rr2 = RemoteRef()
rr3 = RemoteRef()

@async begin sleep(0.5); put(rr1, :ok) end
@async begin sleep(1.0); put(rr2, :ok) end
@async begin sleep(2.0); put(rr3, :ok) end

tic()
timedwait(1.0) do
    all(map(isready, [rr1, rr2, rr3]))
end
et=toq()

# assuming that 0.5 seconds is a good enough buffer on a typical modern CPU
@test (et >= 1.0) && (et <= 1.5)
@test isready(rr1)
@test !isready(rr3)
