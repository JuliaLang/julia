include("UnixDomainCM.jl")

addprocs(UnixDomainCM(parseint(ARGS[1])))
resp = pmap(x -> myid() *2, [1:nworkers()])
println(resp)
