module LibRandom

export DSFMT_state, dsfmt_get_min_array_size, dsfmt_get_idstring,
       dsfmt_init_gen_rand, dsfmt_gv_init_gen_rand, 
       dsfmt_init_by_array, dsfmt_gv_init_by_array,
       dsfmt_genrand_close1_open2, dsfmt_gv_genrand_close1_open2,
       dsfmt_genrand_close_open, dsfmt_gv_genrand_close_open, 
       dsfmt_genrand_open_close, dsfmt_gv_genrand_open_close, 
       dsfmt_genrand_open_open, dsfmt_gv_genrand_open_open, 
       dsfmt_fill_array_close1_open2!, dsfmt_gv_fill_array_close1_open2!,
       dsfmt_fill_array_close_open!, dsfmt_gv_fill_array_close_open!, 
       dsfmt_fill_array_open_close!, dsfmt_gv_fill_array_open_close!, 
       dsfmt_fill_array_open_open!, dsfmt_gv_fill_array_open_open!, 
       dsfmt_genrand_uint32, dsfmt_gv_genrand_uint32, 
       randmtzig_create_ziggurat_tables, randmtzig_randn, randmtzig_fill_randn!, 
       randmtzig_exprnd, randmtzig_fill_exprnd!,
       win32_SystemFunction036!

## DSFMT

type DSFMT_state
    val::Vector{Int32}
    DSFMT_state() = new(Array(Int32, 770))
end

function dsfmt_get_idstring()
    idstring = ccall((:dsfmt_get_idstring,:librandom),
                     Ptr{Uint8},
                     ())
    return bytestring(idstring)
end

function dsfmt_get_min_array_size()
    min_array_size = ccall((:dsfmt_get_min_array_size,:librandom), 
                           Int32, 
                           ())
end

const dsfmt_min_array_size = dsfmt_get_min_array_size()

function dsfmt_init_gen_rand(s::DSFMT_state, seed::Uint32)
    ccall((:dsfmt_init_gen_rand,:librandom),
          Void, 
          (Ptr{Void}, Uint32,), 
          s.val, seed)
end

function dsfmt_gv_init_gen_rand(seed::Uint32)
    ccall((:dsfmt_gv_init_gen_rand,:librandom),
          Void,
          (Uint32,),
          seed)
end

function dsfmt_init_by_array(s::DSFMT_state, seed::Vector{Uint32})
    ccall((:dsfmt_init_by_array,:librandom),
          Void, 
          (Ptr{Void}, Ptr{Uint32}, Int32), 
          s.val, seed, length(seed))
end

function dsfmt_gv_init_by_array(seed::Vector{Uint32})
  ccall((:dsfmt_gv_init_by_array,:librandom),
        Void, 
        (Ptr{Uint32}, Int32), 
        seed, length(seed))
end

for (genrand, gv_genrand) in 
    ((:dsfmt_genrand_close1_open2,    :dsfmt_gv_genrand_close1_open2),
     (:dsfmt_genrand_close_open,      :dsfmt_gv_genrand_close_open),
     (:dsfmt_genrand_open_close,      :dsfmt_gv_genrand_open_close),
     (:dsfmt_genrand_open_open,       :dsfmt_gv_genrand_open_open))
    @eval begin
     
        function ($genrand)(s::DSFMT_state)
            ccall(($(string(genrand)),:librandom),
                  Float64,
                  (Ptr{Void},),
                  s.val)
        end

        function ($gv_genrand)()
            ccall(($(string(gv_genrand)),:librandom),
                  Float64,
                  ())
        end
        
    end
end

for (genrand_fill, gv_genrand_fill, genrand_fill_name, gv_genrand_fill_name) in
    ((:dsfmt_fill_array_close1_open2,  :dsfmt_gv_fill_array_close1_open2,
      :dsfmt_fill_array_close1_open2!, :dsfmt_gv_fill_array_close1_open2!),
     (:dsfmt_fill_array_close_open,    :dsfmt_gv_fill_array_close_open,
      :dsfmt_fill_array_close_open!,   :dsfmt_gv_fill_array_close_open!),
     (:dsfmt_fill_array_open_close,    :dsfmt_gv_fill_array_open_close,
      :dsfmt_fill_array_open_close!,   :dsfmt_gv_fill_array_open_close!),
     (:dsfmt_fill_array_open_open,     :dsfmt_gv_fill_array_open_open,
      :dsfmt_fill_array_open_open!,    :dsfmt_gv_fill_array_open_open!))
    @eval begin

        function ($genrand_fill_name)(s::DSFMT_state, A::Array{Float64})
            n = numel(A)
            if n <= dsfmt_min_array_size
                for i = 1:n
                    A[i] = rand()
                end
            else
                ccall(($(string(genrand_fill)),:librandom),
                      Void,
                      (Ptr{Void}, Ptr{Float64}, Int32),
                      s.val, A, n & 0xfffffffe)
                if isodd(n)
                    A[n] = rand()
                end
            end
            return A
        end
        
        function ($gv_genrand_fill_name)(A::Array{Float64})
            n = numel(A)
            if n <= dsfmt_min_array_size
                for i = 1:n
                    A[i] = rand()
                end
            else
                ccall(($(string(gv_genrand_fill)),:librandom),
                      Void,
                      (Ptr{Void}, Int32),
                      A, n & 0xfffffffe)
                if isodd(n)
                    A[n] = rand()
                end
            end
            return A
        end

    end
end

function dsfmt_genrand_uint32(s::DSFMT_state)
    ccall((:dsfmt_genrand_uint32,:librandom), 
          Uint32,
          (Ptr{Void},),
          s.val)
end

function dsfmt_gv_genrand_uint32()
    ccall((:dsfmt_gv_genrand_uint32,:librandom), 
          Uint32,
          ())
end

## randmtzig

function randmtzig_create_ziggurat_tables()
    ccall((:randmtzig_create_ziggurat_tables,:librandom), 
          Void,
          ())
end

function randmtzig_randn()
    ccall((:randmtzig_randn,:librandom), 
          Float64,
          ())
end

function randmtzig_fill_randn!(A)
    ccall((:randmtzig_fill_randn,:librandom),
          Void,
          (Ptr{Float64}, Int), 
          A, numel(A))
    return A
end

function randmtzig_exprnd()
    ccall((:randmtzig_exprnd,:librandom),
          Float64, 
          ())
end

function randmtzig_fill_exprnd!(A)
    ccall((:randmtzig_fill_exprnd,:librandom),
          Void,
          (Ptr{Float64}, Int), 
          A, numel(A))
    return A
end

## Windows entropy

@windows_only begin
    function win32_SystemFunction036!(a::Array{Uint32})
        ccall((:SystemFunction036,:Advapi32),stdcall,Uint8,(Ptr{Void},Uint64),convert(Ptr{Void},a),8)
    end
end

end # module
