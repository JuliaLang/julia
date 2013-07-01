# NOTE: worker processes cannot add more workers, only the client process can. 
# Make sure to update runtests.jl with the mnimum number of 
# workers required whenever the requirement here changes.

if nprocs() < 2
    addprocs(1)
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
    addprocs(4 - nprocs())
end
workloads = hist(@parallel((a,b)->[a,b], for i=1:7; myid(); end), nprocs())[2]
@test max(workloads) - min(workloads) <= 1

# @parallel reduction should work even with very short ranges
@test @parallel(+, for i=1:2; i; end) == 3
