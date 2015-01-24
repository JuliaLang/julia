ccall_test_func(x) = ccall((:testUcharX, "./libccalltest"), Int32, (UInt8,), x % UInt8)
@test ccall_test_func(3) == 1
@test ccall_test_func(259) == 1

ccall_echo_func{T,U}(x, ::Type{T}, ::Type{U}) = ccall((:test_echo_p, "./libccalltest"), T, (U,), x)

type IntLike
    x::Int
end
@test unsafe_load(ccall_echo_func(132, Ptr{Int}, Ref{Int})) === 132
@test unsafe_load(ccall_echo_func(Ref(921), Ptr{Int}, Ref{Int})) === 921
@test unsafe_load(ccall_echo_func(IntLike(993), Ptr{Int}, Ref{IntLike})) === 993
@test unsafe_load(ccall_echo_func(IntLike(881), Ptr{IntLike}, Ref{IntLike})).x === 881
@test ccall_echo_func(532, Int, Int) === 532
@test ccall_echo_func(164, IntLike, Int).x === 164
@test ccall_echo_func(IntLike(828), Int, IntLike) === 828
@test ccall_echo_func(913, Any, Any) === 913
@test unsafe_pointer_to_objref(ccall_echo_func(553, Ptr{Any}, Any)) === 553
@test ccall_echo_func(124, Ref{Int}, Any) === 124
@test unsafe_load(ccall_echo_func(422, Ptr{Any}, Ref{Any})) === 422

@test unsafe_load(ccall_echo_func([383], Ptr{Int}, Ref{Int})) === 383
@test unsafe_load(ccall_echo_func(Ref([144,172],2), Ptr{Int}, Ref{Int})) === 172
#@test unsafe_load(ccall_echo_func(Ref([8],1,1), Ptr{Int}, Ref{Int})) === 8
