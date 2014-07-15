#Simple parallel expm using pmap
t = 10^5          #Number of trials
@everywhere n = 3 #Size of matrices
Ms = pmap(_->randn(n,n), 1:t)

#Precompile
expm(randn(n,n))
map(identity, [randn(n,n)])
pmap(identity, [randn(n,n)])

@time expMs_ser = map(expm, Ms)
@time expMs_par = pmap(expm, Ms)

@assert expMs_ser == expMs_par
