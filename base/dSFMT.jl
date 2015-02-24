module dSFMT

export DSFMT_state, dsfmt_get_min_array_size, dsfmt_get_idstring,
       dsfmt_init_gen_rand, dsfmt_gv_init_gen_rand,
       dsfmt_init_by_array, dsfmt_gv_init_by_array,
       dsfmt_genrand_close1_open2, dsfmt_gv_genrand_close1_open2,
       dsfmt_genrand_close_open, dsfmt_gv_genrand_close_open,
       dsfmt_genrand_uint32, dsfmt_gv_genrand_uint32,
       win32_SystemFunction036!

type DSFMT_state
    val::Vector{Int32}
    DSFMT_state() = new(Array(Int32, 770))
end

function dsfmt_get_idstring()
    idstring = ccall((:dsfmt_get_idstring,:libdSFMT),
                     Ptr{Uint8},
                     ())
    return bytestring(idstring)
end

function dsfmt_get_min_array_size()
    min_array_size = ccall((:dsfmt_get_min_array_size,:libdSFMT),
                           Int32,
                           ())
end

const dsfmt_min_array_size = dsfmt_get_min_array_size()

function dsfmt_init_gen_rand(s::DSFMT_state, seed::Uint32)
    ccall((:dsfmt_init_gen_rand,:libdSFMT),
          Void,
          (Ptr{Void}, Uint32,),
          s.val, seed)
end

function dsfmt_gv_init_gen_rand(seed::Uint32)
    ccall((:dsfmt_gv_init_gen_rand,:libdSFMT),
          Void,
          (Uint32,),
          seed)
end

function dsfmt_init_by_array(s::DSFMT_state, seed::Vector{Uint32})
    ccall((:dsfmt_init_by_array,:libdSFMT),
          Void,
          (Ptr{Void}, Ptr{Uint32}, Int32),
          s.val, seed, length(seed))
end

function dsfmt_gv_init_by_array(seed::Vector{Uint32})
    ccall((:dsfmt_gv_init_by_array,:libdSFMT),
        Void,
        (Ptr{Uint32}, Int32),
        seed, length(seed))
end

function dsfmt_genrand_close_open(s::DSFMT_state)
    ccall((:dsfmt_genrand_close_open, :libdSFMT),
    Float64,
    (Ptr{Void},),
    s.val)
end

function dsfmt_gv_genrand_close_open()
    ccall((:dsfmt_gv_genrand_close_open, :libdSFMT),
    Float64,
    ())
end

function dsfmt_genrand_close1_open2(s::DSFMT_state)
    ccall((:dsfmt_genrand_close1_open2, :libdSFMT),
    Float64,
    (Ptr{Void},),
    s.val)
end

function dsfmt_gv_genrand_close1_open2()
    ccall((:dsfmt_gv_genrand_close1_open2, :libdSFMT),
    Float64,
    ())
end

function dsfmt_genrand_uint32(s::DSFMT_state)
    ccall((:dsfmt_genrand_uint32,:libdSFMT),
          Uint32,
          (Ptr{Void},),
          s.val)
end

function dsfmt_gv_genrand_uint32()
    ccall((:dsfmt_gv_genrand_uint32,:libdSFMT),
          Uint32,
          ())
end

## Windows entropy

@windows_only begin
    function win32_SystemFunction036!(a::Array{Uint32})
        ccall((:SystemFunction036,:Advapi32),stdcall,Uint8,(Ptr{Void},Uint32),a,length(a)*sizeof(eltype(a)))
    end
end

end # module
