## Memory management/Memory locking ##
function allow_swapping{T}(src::Array{T})
    status = ccall(:jl_munlock,Int,(Any,),src)
    println("Unlocked with status $status")
    return status
end

type ArrayLockHolder{T}
    var::Array{T}
end
allow_swapping{T}(lh::ArrayLockHolder{T}) = allow_swapping(lh.var)

function prevent_swapping{T}(src::Array{T})
    mlock = ArrayLockHolder(src)
    finalizer(mlock,allow_swapping)
    status = ccall(:jl_mlock,Int,(Any,),src)
    return status, mlock
end
