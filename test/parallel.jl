addprocs_local(1)

@test myid() == 1
@test fetch(@spawnat 2 myid()) == 2

d = drand(200,200)
s = convert(Array, d[1:150, 1:150])
a = convert(Array, d)
@test a[1:150,1:150] == s
