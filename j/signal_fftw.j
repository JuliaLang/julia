## FFT: Implement fft by calling fftw.

_jl_libfftw = dlopen("libfftw3")
_jl_libfftwf = dlopen("libfftw3f")

## Direction of FFT

const _jl_FFTW_FORWARD = int32(-1)
const _jl_FFTW_BACKWARD = int32(1)

## _jl_FFTW Flags from fftw3.h

const _jl_FFTW_MEASURE         = uint32(0)
const _jl_FFTW_DESTROY_INPUT   = uint32(1 << 0)
const _jl_FFTW_UNALIGNED       = uint32(1 << 1)
const _jl_FFTW_CONSERVE_MEMORY = uint32(1 << 2)
const _jl_FFTW_EXHAUSTIVE      = uint32(1 << 3)   # NO_EXHAUSTIVE is default
const _jl_FFTW_PRESERVE_INPUT  = uint32(1 << 4)   # cancels _jl_FFTW_DESTROY_INPUT
const _jl_FFTW_PATIENT         = uint32(1 << 5)   # IMPATIENT is default
const _jl_FFTW_ESTIMATE        = uint32(1 << 6)

## R2R transform kinds

const _jl_FFTW_R2HC    = int32(0)
const _jl_FFTW_HC2R    = int32(1)
const _jl_FFTW_DHT     = int32(2)
const _jl_FFTW_REDFT00 = int32(3)
const _jl_FFTW_REDFT01 = int32(4)
const _jl_FFTW_REDFT10 = int32(5)
const _jl_FFTW_REDFT11 = int32(6)
const _jl_FFTW_RODFT00 = int32(7)
const _jl_FFTW_RODFT01 = int32(8)
const _jl_FFTW_RODFT10 = int32(9)
const _jl_FFTW_RODFT11 = int32(10)

## Julia wrappers around FFTW functions

# Execute

_jl_fftw_execute(precision::Union(Type{Float64}, Type{Complex128}), plan) =
    ccall(dlsym(_jl_libfftw, :fftw_execute), Void, (Ptr{Void},), plan)

_jl_fftw_execute(precision::Union(Type{Float32}, Type{Complex64}), plan) =
    ccall(dlsym(_jl_libfftwf, :fftwf_execute), Void, (Ptr{Void},), plan)

# Destroy plan

_jl_fftw_destroy_plan(precision::Union(Type{Float64}, Type{Complex128}), plan) =
    ccall(dlsym(_jl_libfftw, :fftw_destroy_plan), Void, (Ptr{Void},), plan)

_jl_fftw_destroy_plan(precision::Union(Type{Float32}, Type{Complex64}), plan) =
    ccall(dlsym(_jl_libfftwf, :fftwf_destroy_plan), Void, (Ptr{Void},), plan)

# Create 1d plan

macro _jl_fftw_plan_dft_1d_macro(libname, fname_complex, fname_real, T_in, T_out)
    quote
        function _jl_fftw_plan_dft(X::Vector{$T_out}, Y::Vector{$T_out}, direction::Integer)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                  int32(length(X)), X, Y, int32(direction), _jl_FFTW_ESTIMATE)
        end
        function _jl_fftw_plan_dft(X::Vector{$T_in}, Y::Vector{$T_out})
            ccall(dlsym($libname, $fname_real),
                  Ptr{Void},
                  (Int32, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                  int32(length(X)), X, Y, _jl_FFTW_ESTIMATE)
        end
    end
end

@_jl_fftw_plan_dft_1d_macro _jl_libfftw  :fftw_plan_dft_1d  :fftw_plan_dft_r2c_1d  Float64 Complex128
@_jl_fftw_plan_dft_1d_macro _jl_libfftwf :fftwf_plan_dft_1d :fftwf_plan_dft_r2c_1d Float32 Complex64

# Create 2d plan

macro _jl_fftw_plan_dft_2d_macro(libname, fname_complex, fname_real, T_in, T_out)
    quote
        function _jl_fftw_plan_dft(X::Matrix{$T_out}, Y::Matrix{$T_out}, direction::Integer)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Int32, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                  int32(size(X,2)), int32(size(X,1)), X, Y, int32(direction), _jl_FFTW_ESTIMATE)
        end
        function _jl_fftw_plan_dft(X::Matrix{$T_in}, Y::Matrix{$T_out})
            ccall(dlsym($libname, $fname_real),
                  Ptr{Void},
                  (Int32, Int32, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                  int32(size(X,2)), int32(size(X,1)), X, Y, _jl_FFTW_ESTIMATE)
        end
    end
end

@_jl_fftw_plan_dft_2d_macro _jl_libfftw  :fftw_plan_dft_2d  :fftw_plan_dft_r2c_2d  Float64 Complex128 
@_jl_fftw_plan_dft_2d_macro _jl_libfftwf :fftwf_plan_dft_2d :fftwf_plan_dft_r2c_2d Float32 Complex64 

# Create 3d plan

macro _jl_fftw_plan_dft_3d_macro(libname, fname_complex, fname_real, T_in, T_out)
    quote
        function _jl_fftw_plan_dft(X::Array{$T_out,3}, Y::Array{$T_out,3}, direction::Integer)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Int32, Int32, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                  int32(size(X,3)), int32(size(X,2)), int32(size(X,1)), X, Y, int32(direction), _jl_FFTW_ESTIMATE)
        end
        function _jl_fftw_plan_dft(X::Array{$T_in,3}, Y::Array{$T_out,3})
            ccall(dlsym($libname, $fname_real),
                  Ptr{Void},
                  (Int32, Int32, Int32, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                  int32(size(X,3)), int32(size(X,2)), int32(size(X,1)), X, Y, _jl_FFTW_ESTIMATE)
        end
    end
end

@_jl_fftw_plan_dft_3d_macro _jl_libfftw  :fftw_plan_dft_2d  :fftw_plan_dft_r2c_2d  Float64 Complex128 
@_jl_fftw_plan_dft_3d_macro _jl_libfftwf :fftwf_plan_dft_2d :fftwf_plan_dft_r2c_2d Float32 Complex64 

# Create nd plan

macro _jl_fftw_plan_dft_nd_macro(libname, fname_complex, fname_real, T_in, T_out)
    quote
        function _jl_fftw_plan_dft(X::Array{$T_out}, Y::Array{$T_out}, direction::Integer)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                  int32(ndims(X)), int32([size(X)...]), X, Y, int32(direction), _jl_FFTW_ESTIMATE)
        end
        function _jl_fftw_plan_dft(X::Array{$T_in}, Y::Array{$T_out})
            ccall(dlsym($libname, $fname_real),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                  int32(ndims(X)), int32([size(X)...]), X, Y, _jl_FFTW_ESTIMATE)
        end
    end
end

@_jl_fftw_plan_dft_nd_macro _jl_libfftw  :fftw_plan_dft  :fftw_plan_dft_r2c  Float64 Complex128 
@_jl_fftw_plan_dft_nd_macro _jl_libfftwf :fftwf_plan_dft :fftwf_plan_dft_r2c Float32 Complex64 

# Complex inputs

function fftn{T<:Union(Complex128,Complex64)}(X::Array{T})
    Y = similar(X, T)
    plan = _jl_fftw_plan_dft(X, Y, _jl_FFTW_FORWARD)
    _jl_fftw_execute(T, plan)
    _jl_fftw_destroy_plan(T, plan)
    return Y
end

function ifftn{T<:Union(Complex128,Complex64)}(X::Array{T})
    Y = similar(X, T)
    plan = _jl_fftw_plan_dft(X, Y, _jl_FFTW_BACKWARD)
    _jl_fftw_execute(T, plan)
    _jl_fftw_destroy_plan(T, plan)
    return Y
end

# Real inputs

# This definition can be generalized to fftn, once the code to 
# compute the conjugate parts for the nd case is implemented.
function fftn{T<:Union(Float64,Float32)}(X::Vector{T})
    T_out = is(T, Float32) ? Complex64 : Complex128

    Y = similar(X, T_out)
    plan = _jl_fftw_plan_dft(X, Y)
    _jl_fftw_execute(T, plan)
    _jl_fftw_destroy_plan(T, plan)

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

# TODO: This is inefficient. Advanced interfaces of _jl_FFTW should be used
macro jl_fft_ifft_macro(fname, fname_compute)
    quote 
        function ($fname){T<:Union(Float64,Float32,Complex128,Complex64),n}(
            X::Array{T,n}, npoints, dim::Integer
        )
            if npoints != (); error("the npoints option is not yet supported"); end
            if dim > 2; error("only 2d arrays are supported for now"); end

            if n == 1; return fftn(X); end

            Y = similar(X, (is(T,Float32)||is(T,Complex64)) ? Complex64 : Complex128)

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

# Transpose
# NOTE: Using _jl_FFTW_MEASURE and _jl_FFTW_PATIENT zeros out the input the 
# first time it is used for a particular size. Use _jl_FFTW_ESTIMATE

macro jl_transpose_real_macro(libname, fname, eltype)
    quote
        function _jl_fftw_transpose(X::Matrix{$eltype})
            (n1, n2) = size(X)
            P = similar(X, n2, n1)
            plan = ccall(dlsym($libname, $fname), Ptr{Void},
                         (Int32, Ptr{Int32}, Int32, Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Uint32),
                         int32(0), C_NULL, int32(2),int32([n1,n2,1,n2,1,n1]), X, P, [_jl_FFTW_HC2R], _jl_FFTW_ESTIMATE | _jl_FFTW_PRESERVE_INPUT)
            _jl_fftw_execute($eltype, plan)
            _jl_fftw_destroy_plan($eltype, plan)
            return P
        end
    end
end

@jl_transpose_real_macro _jl_libfftw  :fftw_plan_guru_r2r  Float64
@jl_transpose_real_macro _jl_libfftwf :fftwf_plan_guru_r2r Float32

macro jl_transpose_complex_macro(libname, fname, celtype)
    quote
        function _jl_fftw_transpose(X::Matrix{$celtype})
            (n1, n2) = size(X)
            P = similar(X, n2, n1)
            plan = ccall(dlsym($libname, $fname), Ptr{Void},
                         (Int32, Ptr{Int32}, Int32, Ptr{Int32}, Ptr{$celtype}, Ptr{$celtype}, Int32, Uint32),
                         int32(0), C_NULL, int32(2),int32([n1,n2,1,n2,1,n1]), X, P, _jl_FFTW_FORWARD, _jl_FFTW_ESTIMATE | _jl_FFTW_PRESERVE_INPUT)
            _jl_fftw_execute($celtype, plan)
            _jl_fftw_destroy_plan($celtype, plan)
            return P
        end
    end
end

@jl_transpose_complex_macro _jl_libfftw  :fftw_plan_guru_dft  Complex128
@jl_transpose_complex_macro _jl_libfftwf :fftwf_plan_guru_dft Complex64
