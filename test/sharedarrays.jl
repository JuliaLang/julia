if nprocs() < 4
    addprocs(4-nprocs())
end

@everywhere using SharedArrays

@everywhere function fillme!(A::AbstractArray)
    B = myarray(A)
    ind = myindexes(A)
    B[ind...] = myid()
    A
end

S = SharedArray(Int, (5, 8))

# Create the result we expect to get
n = size(S,2)
target = zeros(Int, size(S,1), n)
k = nchunks(S)
for i = 0:k-1
    ind = iround(i*n/k)+1:iround((i+1)*n/k)
    target[:,ind] = i+1
end

ret, rr = pcall(fillme!, S)
@assert ret == target
@assert S == target
fill!(S, 14)  # so we can check that the next runs properly
bw = sharedsync()
ret, rr = pcall_bw(fillme!, bw, S)
@assert ret == target
@assert S == target

@time pcall(fillme!, S)
@time pcall_bw(fillme!, bw, S)

# Test that nothing breaks when the number of array elements is smaller
# than the number of processes
S = SharedArray(Float64, (2,))
pcall(fillme!, S)
