libfftw = dlopen("libfftw3")

FFTW_FORWARD = int32(-1)
FFTW_ESTIMATE = uint32(64)

function fft(X::Vector{Complex128})

    Y = copy(X)

    plan = ccall(dlsym(libfftw, "fftw_plan_dft_1d"),
              Ptr{Void}, 
              (Int32, Ptr{Complex128}, Ptr{Complex128}, Int32, Uint32, ),
              length(X), X, Y, FFTW_FORWARD, FFTW_ESTIMATE)

    ccall(dlsym(libfftw, "fftw_execute"),
          Void,
          (Ptr{Void},),
          plan)

    ccall(dlsym(libfftw, "fftw_destroy_plan"),
          Void,
          (Ptr{Void},),
          plan)

    return Y
    
end
