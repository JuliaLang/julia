module LibRandom

import Base.*
import Main
import Main.*

export dsfmt_get_min_array_size, dsfmt_gv_init_gen_rand, dsfmt_gv_init_by_array,
       dsfmt_gv_genrand_close_open, dsfmt_gv_genrand_uint32, dsfmt_gv_fill_array_close_open!, 
       randmtzig_create_ziggurat_tables, randmtzig_randn, randmtzig_fill_randn!, 
       randmtzig_exprnd, randmtzig_fill_exprnd!,
       win32_SystemFunction036!

## DSFMT

function dsfmt_get_min_array_size()
    min_array_size = ccall(dlsym(Main.librandom, :dsfmt_get_min_array_size), 
                           Int32, 
                           ())
end

function dsfmt_gv_init_gen_rand(seed::Uint32)
    ccall(dlsym(Main.librandom, :dsfmt_gv_init_gen_rand),
          Void, 
          (Uint32,), 
          seed)
end

function dsfmt_gv_init_by_array(seed::Vector{Uint32})
  ccall(dlsym(Main.librandom, :dsfmt_gv_init_by_array),
        Void, 
        (Ptr{Uint32}, Int32), 
        seed, length(seed))
end

function dsfmt_gv_genrand_close_open()
    r = ccall(dlsym(Main.librandom, :dsfmt_gv_genrand_close_open), 
              Float64, 
              ())
end

function dsfmt_gv_genrand_uint32()
    r = ccall(dlsym(Main.librandom, :dsfmt_gv_genrand_uint32), 
              Uint32, 
              ())
end

function dsfmt_gv_fill_array_close_open!(A::Array{Float64})
    ccall(dlsym(Main.librandom, :dsfmt_gv_fill_array_close_open),
          Void, 
          (Ptr{Void}, Int32), 
          A, n)
    return A
end

## randmtzig

function randmtzig_create_ziggurat_tables()
    ccall(dlsym(Main.librandom, :randmtzig_create_ziggurat_tables), 
          Void,
          ())
end

function randmtzig_randn()
    call(dlsym(Main.librandom, :randmtzig_randn), 
         Float64,
         ())
end

function randmtzig_fill_randn!(A)
    ccall(dlsym(Main.librandom, :randmtzig_fill_randn),
          Void,
          (Ptr{Float64}, Uint32), 
          A, numel(A))
end

function randmtzig_exprnd()
    ccall(dlsym(Main.librandom, :randmtzig_exprnd),
          Float64, 
          ())
end

function randmtzig_fill_exprnd!(A)
   ccall(dlsym(Main.librandom, :randmtzig_fill_exprnd),
         Void,
         (Ptr{Float64}, Uint32), 
         A, numel(A))
end

## Windows entropy

@windows_only begin
    function win32_SystemFunction036!(a::Array{Uint32})
        ccall(dlsym(advapi32,:SystemFunction036),stdcall,Uint8,(Ptr{Void},Uint64),convert(Ptr{Void},a),8)
    end
end

end # module
