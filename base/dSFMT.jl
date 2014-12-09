module dSFMT

export DSFMT_state, dsfmt_get_min_array_size, dsfmt_get_idstring,
       dsfmt_init_gen_rand, dsfmt_init_by_array, dsfmt_gv_init_by_array,
       dsfmt_fill_array_close_open!, dsfmt_fill_array_close1_open2!,
       win32_SystemFunction036!

type DSFMT_state
    val::Vector{Int32}
    DSFMT_state() = new(Array(Int32, 770))
end

function dsfmt_get_idstring()
    idstring = ccall((:dsfmt_get_idstring,:libdSFMT),
                     Ptr{UInt8},
                     ())
    return bytestring(idstring)
end

function dsfmt_get_min_array_size()
    min_array_size = ccall((:dsfmt_get_min_array_size,:libdSFMT),
                           Int32,
                           ())
end

const dsfmt_min_array_size = dsfmt_get_min_array_size()

function dsfmt_init_gen_rand(s::DSFMT_state, seed::UInt32)
    ccall((:dsfmt_init_gen_rand,:libdSFMT),
          Void,
          (Ptr{Void}, UInt32,),
          s.val, seed)
end

function dsfmt_init_by_array(s::DSFMT_state, seed::Vector{UInt32})
    ccall((:dsfmt_init_by_array,:libdSFMT),
          Void,
          (Ptr{Void}, Ptr{UInt32}, Int32),
          s.val, seed, length(seed))
end

function dsfmt_gv_init_by_array(seed::Vector{UInt32})
    ccall((:dsfmt_gv_init_by_array,:libdSFMT),
          Void,
          (Ptr{UInt32}, Int32),
          seed, length(seed))
end

function dsfmt_fill_array_close1_open2!(s::DSFMT_state, A::Ptr{Float64}, n::Int)
    @assert Csize_t(A) % 16 == 0 # the underlying C array must be 16-byte aligned
    @assert dsfmt_min_array_size <= n && iseven(n)
    ccall((:dsfmt_fill_array_close1_open2,:libdSFMT),
          Void,
          (Ptr{Void}, Ptr{Float64}, Int),
          s.val, A, n)
end

function dsfmt_fill_array_close_open!(s::DSFMT_state, A::Ptr{Float64}, n::Int)
    @assert Csize_t(A) % 16 == 0 # the underlying C array must be 16-byte aligned
    @assert dsfmt_min_array_size <= n && iseven(n)
    ccall((:dsfmt_fill_array_close_open,:libdSFMT),
          Void,
          (Ptr{Void}, Ptr{Float64}, Int),
          s.val, A, n)
end

## Windows entropy

@windows_only begin
    function win32_SystemFunction036!{T}(a::Array{T})
        ccall((:SystemFunction036,:Advapi32),stdcall,UInt8,(Ptr{Void},UInt32),a,sizeof(a))
    end
end

end # module
