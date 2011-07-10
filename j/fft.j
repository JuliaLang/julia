## FFT

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

jl_fftw_plan_dft_1d(X::DenseVector{Complex128}, Y::DenseVector{Complex128}, direction::Int32) =
    ccall(dlsym(libfftw, :fftw_plan_dft_1d),
          Ptr{Void},
          (Int32, Ptr{Complex128}, Ptr{Complex128}, Int32, Uint32, ),
          length(X), X, Y, direction, FFTW_ESTIMATE)

jl_fftw_plan_dft_1d(X::DenseVector{Complex64}, Y::DenseVector{Complex64}, direction::Int32) =
    ccall(dlsym(libfftwf, :fftwf_plan_dft_1d),
          Ptr{Void},
          (Int32, Ptr{Complex64}, Ptr{Complex64}, Int32, Uint32, ),
          length(X), X, Y, direction, FFTW_ESTIMATE)

jl_fftw_plan_dft_r2c_1d(X::DenseVector{Float64}, Y::DenseVector{Complex128}) =
    ccall(dlsym(libfftw, :fftw_plan_dft_r2c_1d),
          Ptr{Void},
          (Int32, Ptr{Float64}, Ptr{Complex128}, Uint32, ),
          length(X), X, Y, FFTW_ESTIMATE)

jl_fftw_plan_dft_r2c_1d(X::DenseVector{Float32}, Y::DenseVector{Complex64}) =
    ccall(dlsym(libfftw, :fftwf_plan_dft_r2c_1d),
          Ptr{Void},
          (Int32, Ptr{Float32}, Ptr{Complex64}, Uint32, ),
          length(X), X, Y, FFTW_ESTIMATE)

# Create 2d plan

jl_fftw_plan_dft_2d(X::DenseMatrix{Complex128}, Y::DenseMatrix{Complex128}, direction::Int32) =
    ccall(dlsym(libfftw, :fftw_plan_dft_2d),
          Ptr{Void},
          (Int32, Int32, Ptr{Complex128}, Ptr{Complex128}, Int32, Uint32, ),
          size(X,2), size(X,1), X, Y, direction, FFTW_ESTIMATE)

jl_fftw_plan_dft_2d(X::DenseMatrix{Complex64}, Y::DenseMatrix{Complex64}, direction::Int32) =
    ccall(dlsym(libfftwf, :fftwf_plan_dft_2d),
          Ptr{Void},
          (Int32, Int32, Ptr{Complex64}, Ptr{Complex64}, Int32, Uint32, ),
          size(X,2), size(X,1), X, Y, direction, FFTW_ESTIMATE)

# Create 3d plan

jl_fftw_plan_dft_3d(X::Array{Complex128,3}, Y::Array{Complex128,3}, direction::Int32) =
    ccall(dlsym(libfftw, :fftw_plan_dft_3d),
          Ptr{Void},
          (Int32, Int32, Int32, Ptr{Complex128}, Ptr{Complex128}, Int32, Uint32, ),
          size(X,3), size(X,2), size(X,1), X, Y, direction, FFTW_ESTIMATE)

jl_fftw_plan_dft_3d(X::Array{Complex64,3}, Y::Array{Complex64,3}, direction::Int32) =
    ccall(dlsym(libfftwf, :fftwf_plan_dft_3d),
          Ptr{Void},
          (Int32, Int32, Int32, Ptr{Complex64}, Ptr{Complex64}, Int32, Uint32, ),
          size(X,3), size(X,2), size(X,1), X, Y, direction, FFTW_ESTIMATE)

# Create nd plan

jl_fftw_plan_dft(X::Array{Complex128}, Y::Array{Complex128}, direction::Int32) =
    ccall(dlsym(libfftw, :fftw_plan_dft),
          Ptr{Void},
          (Int32, Ptr{Int32}, Ptr{Complex128}, Ptr{Complex128}, Int32, Uint32, ),
          ndims(X), [size(X)...], X, Y, direction, FFTW_ESTIMATE)

jl_fftw_plan_dft(X::Array{Complex64}, Y::Array{Complex64}, direction::Int32) =
    ccall(dlsym(libfftwf, :fftwf_plan_dft),
          Ptr{Void},
          (Int32, Ptr{Int32}, Ptr{Complex64}, Ptr{Complex64}, Int32, Uint32, ),
          ndims(X), [size(X)...], X, Y, direction, FFTW_ESTIMATE)

# Complex inputs

macro fftw_fftn(fname, array_type, in_type, plan_name, direction)
    quote

        function ($fname)(X::($array_type){$in_type})
            Y = similar(X, $in_type)
            plan = ($plan_name)(X, Y, $direction)
            jl_fftw_execute($in_type, plan)
            jl_fftw_destroy_plan($in_type, plan)
            return Y
        end

    end
end

@fftw_fftn fft   DenseVector         Complex128 jl_fftw_plan_dft_1d  FFTW_FORWARD
@fftw_fftn ifft  DenseVector         Complex128 jl_fftw_plan_dft_1d  FFTW_BACKWARD

@fftw_fftn fft   DenseVector         Complex64  jl_fftw_plan_dft_1d  FFTW_FORWARD
@fftw_fftn ifft  DenseVector         Complex64  jl_fftw_plan_dft_1d  FFTW_BACKWARD

@fftw_fftn fft2  DenseMatrix         Complex128 jl_fftw_plan_dft_2d  FFTW_FORWARD
@fftw_fftn ifft2 DenseMatrix         Complex128 jl_fftw_plan_dft_2d  FFTW_BACKWARD

@fftw_fftn fft2  DenseMatrix         Complex64  jl_fftw_plan_dft_2d  FFTW_FORWARD
@fftw_fftn ifft2 DenseMatrix         Complex64  jl_fftw_plan_dft_2d  FFTW_BACKWARD

@fftw_fftn fft3  Array{Complex128,3} Complex128 jl_fftw_plan_dft_3d  FFTW_FORWARD
@fftw_fftn ifft3 Array{Complex128,3} Complex128 jl_fftw_plan_dft_3d  FFTW_BACKWARD

@fftw_fftn fft3  Array{Complex64,3}  Complex64  jl_fftw_plan_dft_3d  FFTW_FORWARD
@fftw_fftn ifft3 Array{Complex64,3}  Complex64  jl_fftw_plan_dft_3d  FFTW_BACKWARD

@fftw_fftn fftn  Array               Complex128 jl_fftw_plan_dft     FFTW_FORWARD
@fftw_fftn ifftn Array               Complex128 jl_fftw_plan_dft     FFTW_BACKWARD

@fftw_fftn fftn  Array               Complex64  jl_fftw_plan_dft     FFTW_FORWARD
@fftw_fftn ifftn Array               Complex64  jl_fftw_plan_dft     FFTW_BACKWARD

macro fftw_fftn_real(fname, array_type, in_type, out_type, plan_name)
    quote

        function ($fname)(X::($array_type){$in_type})
            Y = similar(X, $out_type)
            plan = ($plan_name)(X, Y)
            jl_fftw_execute($in_type, plan)
            jl_fftw_destroy_plan($in_type, plan)

            n = length(Y)
            nconj = int32(length(X)/2 - 1)
            for i=n:-1:(nconj-n)
                Y[i] = Y[n-i+2]
            end

            return Y
        end

    end
end

@fftw_fftn_real fft DenseVector Float64 Complex128 jl_fftw_plan_dft_r2c_1d
@fftw_fftn_real fft DenseVector Float32 Complex64  jl_fftw_plan_dft_r2c_1d

# TODO: Implement efficient operations such as the vector case later
fft2(X::DenseMatrix) = fft2(complex(X))
fft3(X::Array) = fft3(complex(X))
fftn(X::Array) = fftn(complex(X))

# TODO: Compute fft and ifft of slices of arrays
