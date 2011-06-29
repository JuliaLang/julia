#libdsfmt = dlopen("libdSFMT")

rand()     = ccall(:rand_double,   Float64, ())
randf()    = ccall(:rand_float,    Float32, ())
randui32() = ccall(:genrand_int32, Uint32,  ())
randn()    = ccall(:randn,         Float64, ())
srand(s::Union(Int32,Uint32)) = ccall(:randomseed32, Void, (Uint32,), uint32(s))
srand(s::Union(Int64,Uint64)) = ccall(:randomseed64, Void, (Uint64,), uint64(s))

macro rand_matrix_builder(t, f)
    quote

        function ($f)(dims::Dims)
            A = Array($t, dims)
            for i = 1:numel(A)
                A[i] = ($f)()
            end
            return A
        end

        ($f)(dims::Size...) = ($f)(dims)

    end # quote
end # macro

@rand_matrix_builder Float64 rand
@rand_matrix_builder Float32 randf
@rand_matrix_builder Float64 randn
@rand_matrix_builder Uint32 randui32

