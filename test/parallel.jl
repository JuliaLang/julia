if nprocs() < 2
    addprocs_local(1)
end

id_me = myid()
id_other = id_me==1 ? 2 : 1

@test fetch(@spawnat id_other myid()) == id_other

d = drand((200,200), [id_me, id_other])
s = convert(Array, d[1:150, 1:150])
a = convert(Array, d)
@test a[1:150,1:150] == s

@test fetch(@spawnat id_me localpart(d)[1,1]) == d[1,1]
@test fetch(@spawnat id_other localpart(d)[1,1]) == d[1,101]
