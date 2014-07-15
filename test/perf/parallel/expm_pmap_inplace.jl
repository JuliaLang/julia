#Simple parallel expm using @parallel
t = 10^5 #Number of trials
n = 3    #Size of matrices

function map!(f, Ms)
    for i=1:size(Ms, 3)
        Ms[:,:,i] = f(Ms[:,:,i])
    end
end

function pmap!(f, Ms)
    #Simple static scheduler
    workids = 1:size(Ms, 3)
    nwork = length(workids)
    np = nprocs()
    #nwork % np == 0 || error("amount of work=$nwork doesn't evenly divide nprocs()=$np; not implemented") 
    nchunk = nwork÷np
    rrefs = Array(RemoteRef, np)
    for procid=1:np
	rrefs[procid] = @spawn begin
	    localworkids = workids[(procid-1)*nchunk+1:(procid==1?procid*nchunk:end)]
	    localt = length(localworkids)
	    localMs= Array(eltype(Ms),size(Ms,1),size(Ms,2),localt)
	    for (i,t) in enumerate(localworkids)
                localMs[:,:,i] = f(Ms[:,:,t])
	    end
	    localMs
        end
    end

    for procid=1:np
	localworkids = workids[(procid-1)*nchunk+1:(procid==1?procid*nchunk:end)]
        Ms[:,:,localworkids] = fetch(rrefs[procid])
    end
end

Ms_ser = randn(n,n,t)
Ms_par = copy(Ms_ser)

#Precompile
expm(randn(n,n))
map!(identity, randn(n,n,nprocs()))
pmap!(identity, randn(n,n,nprocs()))

@time map!(expm, Ms_ser)
@time @sync pmap!(expm, Ms_par)

@assert Ms_ser==Ms_par

