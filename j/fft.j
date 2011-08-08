## FFT: Implement fft by calling fftw.

libfftw = dlopen("libfftw3")
libfftwf = dlopen("libfftw3f")

## Direction of FFT

FFTW_FORWARD = int32(-1)
FFTW_BACKWARD = int32(1)

## FFTW Flags from fftw3.h

FFTW_MEASURE         = uint32(0)
FFTW_DESTROY_INPUT   = uint32(1 << 0)
FFTW_UNALIGNED       = uint32(1 << 1)
FFTW_CONSERVE_MEMORY = uint32(1 << 2)
FFTW_EXHAUSTIVE      = uint32(1 << 3)   # NO_EXHAUSTIVE is default
FFTW_PRESERVE_INPUT  = uint32(1 << 4)   # cancels FFTW_DESTROY_INPUT
FFTW_PATIENT         = uint32(1 << 5)   # IMPATIENT is default
FFTW_ESTIMATE        = uint32(1 << 6)

## Julia wrappers around FFTW functions

# Execute

jl_fftw_execute(precision::Union(Type{Float64}, Type{Complex128}), plan) =
    ccall(dlsym(libfftw, :fftw_execute), Void, (Ptr{Void},), plan)

jl_fftw_execute(precision::Union(Type{Float32}, Type{Complex64}), plan) =
    ccall(dlsym(libfftwf, :fftwf_execute), Void, (Ptr{Void},), plan)

# Destroy plan

jl_fftw_destroy_plan(precision::Union(Type{Float64}, Type{Complex128}), plan) =
    ccall(dlsym(libfftw, :fftw_destroy_plan), Void, (Ptr{Void},), plan)

jl_fftw_destroy_plan(precision::Union(Type{Float32}, Type{Complex64}), plan) =
    ccall(dlsym(libfftwf, :fftwf_destroy_plan), Void, (Ptr{Void},), plan)

# Create 1d plan

macro jl_fftw_plan_dft_1d_macro(libname, fname_complex, fname_real, T_in, T_out)
    quote

        function jl_fftw_plan_dft(X::Vector{$T_out}, Y::Vector{$T_out}, direction::Int)
            plan = ccall(dlsym($libname, $fname_complex),
                         Ptr{Void},
                         (Int32, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                         int32(length(X)), X, Y, int32(direction), FFTW_ESTIMATE)
            return plan
        end

        function jl_fftw_plan_dft(X::Vector{$T_in}, Y::Vector{$T_out})
            plan = ccall(dlsym($libname, $fname_real),
                         Ptr{Void},
                         (Int32, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                         int32(length(X)), X, Y, FFTW_ESTIMATE)
            return plan
        end

    end
end

@jl_fftw_plan_dft_1d_macro libfftw  :fftw_plan_dft_1d  :fftw_plan_dft_r2c_1d  Float64 Complex128
@jl_fftw_plan_dft_1d_macro libfftwf :fftwf_plan_dft_1d :fftwf_plan_dft_r2c_1d Float32 Complex64

# Create 2d plan

macro jl_fftw_plan_dft_2d_macro(libname, fname_complex, fname_real, T_in, T_out)
    quote

        function jl_fftw_plan_dft(X::Matrix{$T_out}, Y::Matrix{$T_out}, direction::Int)
            plan = ccall(dlsym($libname, $fname_complex),
                         Ptr{Void},
                         (Int32, Int32, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                         int32(size(X,2)), int32(size(X,1)), X, Y, int32(direction), FFTW_ESTIMATE)
            return plan
        end

        function jl_fftw_plan_dft(X::Matrix{$T_in}, Y::Matrix{$T_out})
            plan = ccall(dlsym($libname, $fname_real),
                         Ptr{Void},
                         (Int32, Int32, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                         int32(size(X,2)), int32(size(X,1)), X, Y, FFTW_ESTIMATE)
            return plan
        end

    end
end

@jl_fftw_plan_dft_2d_macro libfftw  :fftw_plan_dft_2d  :fftw_plan_dft_r2c_2d  Float64 Complex128 
@jl_fftw_plan_dft_2d_macro libfftwf :fftwf_plan_dft_2d :fftwf_plan_dft_r2c_2d Float32 Complex64 

# Create 3d plan

macro jl_fftw_plan_dft_3d_macro(libname, fname_complex, fname_real, T_in, T_out)
    quote

        function jl_fftw_plan_dft(X::Array{$T_out,3}, Y::Array{$T_out,3}, direction::Int)
            plan = ccall(dlsym($libname, $fname_complex),
                         Ptr{Void},
                         (Int32, Int32, Int32, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                         int32(size(X,3)), int32(size(X,2)), int32(size(X,1)), X, Y, int32(direction), FFTW_ESTIMATE)
            return plan
        end

        function jl_fftw_plan_dft(X::Array{$T_in,3}, Y::Array{$T_out,3})
            plan = ccall(dlsym($libname, $fname_real),
                         Ptr{Void},
                         (Int32, Int32, Int32, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                         int32(size(X,3)), int32(size(X,2)), int32(size(X,1)), X, Y, FFTW_ESTIMATE)
            return plan
        end

    end
end

@jl_fftw_plan_dft_3d_macro libfftw  :fftw_plan_dft_2d  :fftw_plan_dft_r2c_2d  Float64 Complex128 
@jl_fftw_plan_dft_3d_macro libfftwf :fftwf_plan_dft_2d :fftwf_plan_dft_r2c_2d Float32 Complex64 

# Create nd plan

macro jl_fftw_plan_dft_nd_macro(libname, fname_complex, fname_real, T_in, T_out)
    quote

        function jl_fftw_plan_dft(X::Array{$T_out}, Y::Array{$T_out}, direction::Int)
            plan = ccall(dlsym($libname, $fname_complex),
                         Ptr{Void},
                         (Int32, Ptr{Int32}, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                         int32(ndims(X)), int32([size(X)...]), X, Y, int32(direction), FFTW_ESTIMATE)
            return plan
        end

        function jl_fftw_plan_dft(X::Array{$T_in}, Y::Array{$T_out})
            plan = ccall(dlsym($libname, $fname_real),
                         Ptr{Void},
                         (Int32, Ptr{Int32}, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                         int32(ndims(X)), int32([size(X)...]), X, Y, FFTW_ESTIMATE)
            return plan
        end

    end
end

@jl_fftw_plan_dft_nd_macro libfftw  :fftw_plan_dft  :fftw_plan_dft_r2c  Float64 Complex128 
@jl_fftw_plan_dft_nd_macro libfftwf :fftwf_plan_dft :fftwf_plan_dft_r2c Float32 Complex64 

# Complex inputs

function fftn{T<:Union(Complex128,Complex64)}(X::Array{T})
    Y = similar(X, T)
    plan = jl_fftw_plan_dft(X, Y, FFTW_FORWARD)
    jl_fftw_execute(T, plan)
    jl_fftw_destroy_plan(T, plan)
    return Y
end

function ifftn{T<:Union(Complex128,Complex64)}(X::Array{T})
    Y = similar(X, T)
    plan = jl_fftw_plan_dft(X, Y, FFTW_BACKWARD)
    jl_fftw_execute(T, plan)
    jl_fftw_destroy_plan(T, plan)
    return Y
end

# Real inputs

# This definition can be generalized to fftn, once the code to 
# compute the conjugate parts for the nd case is implemented.
function fftn{T<:Union(Float64,Float32)}(X::Vector{T})
    T_out = Complex128
    if is(T, Float32)
        T_out = Complex64
    end

    Y = similar(X, T_out)
    plan = jl_fftw_plan_dft(X, Y)
    jl_fftw_execute(T, plan)
    jl_fftw_destroy_plan(T, plan)

    n = length(Y)
    nconj = long(length(X)/2 - 1)
    for i=n:-1:(n-nconj)
        Y[i] = conj(Y[n-i+2])
    end

    return Y
end

# TODO: Can be computed efficiently without converting to complex
# if the conjugate pairs are checked for.
ifftn{T<:Union(Float64,Float32)}(X::Array{T}) = ifftn(complex(X))

# Convenience functions
fft2{T<:Union(Float64,Float32)}(X::Matrix{T}) = fftn(complex(X))
fft3{T<:Union(Float64,Float32)}(X::Array{T})  = fftn(complex(X))

fft2{T<:Union(Complex128,Complex64)}(X::Matrix{T}) = fftn(X)
fft3{T<:Union(Complex128,Complex64)}(X::Array{T})  = fftn(X)

ifft{T<:Union(Float64,Float32,Complex128,Complex64)}(X::Vector{T})  = ifftn(X)
ifft2{T<:Union(Float64,Float32,Complex128,Complex64)}(X::Matrix{T}) = ifftn(X)
ifft3{T<:Union(Float64,Float32,Complex128,Complex64)}(X::Array{T})  = ifftn(X)

# Compute fft and ifft of slices of arrays

fft(X) = fft(X, (), 1)
ifft(X) = ifft(X, (), 1)

# TODO: This is inefficient. Advanced interfaces of FFTW should be used
macro jl_fft_ifft_macro(fname, fname_compute)
    quote 
        function ($fname){T<:Union(Float64,Float32,Complex128,Complex64),n}(X::Array{T,n}, 
                                                                            npoints, dim::Int)

            if npoints != (); error("The npoints option is not yet supported"); end
            if dim > 2; error("Only 2d arrays are supported for now"); end

            if n == 1; return fftn(X); end

            if is(T, Float64) || is(T, Complex128); Y = similar(X, Complex128); end
            if is(T, Float32) || is(T, Complex64);  Y = similar(X, Complex64);  end

            if dim == 2; X = X.'; Y = reshape(Y, size(X)); end

            for i=1:size(X,1):numel(X)
                R = i:(i+size(X,1)-1)
                Y[R] = ($fname_compute)(X[R])
            end

            if dim == 2; Y = Y.'; end

            return Y
        end

    end
end

@jl_fft_ifft_macro fft fftn
@jl_fft_ifft_macro ifft ifftn
