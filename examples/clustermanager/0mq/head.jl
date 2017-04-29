# This file is a part of Julia. License is MIT: http://julialang.org/license

include("ZMQCM.jl")

# @spawn run(`julia broker.jl`)

start_master(parse(Int,ARGS[1]))

resp = pmap(x -> myid() *2, [1:nworkers()])

println(resp)
