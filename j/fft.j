libfftw = dlopen("libfftw3")

FFTW_FORWARD = int32(-1)
FFTW_ESTIMATE = uint32(64)

fftw_execute(plan) = ccall(dlsym(libfftw, "fftw_execute"), Void, (Ptr{Void},), plan)
fftw_destroy_plan(plan) = ccall(dlsym(libfftw, "fftw_destroy_plan"), Void, (Ptr{Void},), plan)

macro fftw_fft1d(plan_name, in_type, out_type)
    quote
        function fft(X::DenseVector{$in_type})
            
            Y = similar(X, $out_type)
            
            plan = ccall(dlsym(libfftw, $plan_name),
                         Ptr{Void}, 
                         (Int32, Ptr{$in_type}, Ptr{$out_type}, Int32, Uint32, ),
                         length(X), X, Y, FFTW_FORWARD, FFTW_ESTIMATE)
            
            fftw_execute(plan)
            fftw_destroy_plan(plan)
            
            return Y
        end
    end
end

@fftw_fft1d :fftw_plan_dft_r2c_1d Float64 Complex128
@fftw_fft1d :fftw_plan_dft_1d Complex128 Complex128
