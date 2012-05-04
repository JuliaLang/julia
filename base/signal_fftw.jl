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

for (libname, fname_complex, fname_real, T_in, T_out) in
    ((:_jl_libfftw,"fftw_plan_dft_1d","fftw_plan_dft_r2c_1d",:Float64,:Complex128),
     (:_jl_libfftwf,"fftwf_plan_dft_1d","fftwf_plan_dft_r2c_1d",:Float32,:Complex64))
    @eval begin
        function _jl_fftw_plan_dft(X::Vector{$T_out}, Y::Vector{$T_out}, direction::Integer)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                  length(X), X, Y, direction, _jl_FFTW_ESTIMATE)
        end
        function _jl_fftw_plan_dft(X::Vector{$T_in}, Y::Vector{$T_out})
            ccall(dlsym($libname, $fname_real),
                  Ptr{Void},
                  (Int32, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                  length(X), X, Y, _jl_FFTW_ESTIMATE)
        end
    end
end

# Create 2d plan

for (libname, fname_complex, fname_real, T_in, T_out) in
    ((:_jl_libfftw,"fftw_plan_dft_2d","fftw_plan_dft_r2c_2d",:Float64,:Complex128),
     (:_jl_libfftwf,"fftwf_plan_dft_2d","fftwf_plan_dft_r2c_2d",:Float32,:Complex64))
    @eval begin
        function _jl_fftw_plan_dft(X::Matrix{$T_out}, Y::Matrix{$T_out}, direction::Integer)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Int32, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                  size(X,2), size(X,1), X, Y, direction, _jl_FFTW_ESTIMATE)
        end
        function _jl_fftw_plan_dft(X::Matrix{$T_in}, Y::Matrix{$T_out})
            ccall(dlsym($libname, $fname_real),
                  Ptr{Void},
                  (Int32, Int32, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                  size(X,2), size(X,1), X, Y, _jl_FFTW_ESTIMATE)
        end
    end
end

# Create nd plan

for (libname, fname_complex, fname_real, T_in, T_out) in
    ((:_jl_libfftw,"fftw_plan_dft","fftw_plan_dft_r2c",:Float64,:Complex128),
     (:_jl_libfftwf,"fftwf_plan_dft","fftwf_plan_dft_r2c",:Float32,:Complex64))
    @eval begin
        function _jl_fftw_plan_dft(X::Array{$T_out}, Y::Array{$T_out}, direction::Integer)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                  ndims(X), int32(reverse([size(X)...])), X, Y, direction, _jl_FFTW_ESTIMATE)
        end
        function _jl_fftw_plan_dft(X::Array{$T_in}, Y::Array{$T_out})
            ccall(dlsym($libname, $fname_real),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                  ndims(X), int32(reverse([size(X)...])), X, Y, _jl_FFTW_ESTIMATE)
        end
    end
end

# Guru plans

for (libname, fname_complex, fname_real, T_in, T_out) in
    ((:_jl_libfftw,"fftw_plan_guru_dft","fftw_plan_guru_dft_r2c",:Float64,:Complex128),
     (:_jl_libfftwf,"fftwf_plan_guru_dft","fftwf_plan_guru_dft_r2c",:Float32,:Complex64))
    @eval begin
        function _jl_fftw_plan_guru_dft(dims::Array{Int,2}, howmany::Array{Int,2},
            X::Array{$T_out}, Y::Array{$T_out}, direction::Int32)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Int32, Ptr{Int32},
                   Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32),
                  size(dims,2), int32(dims), size(howmany,2), int32(howmany),
                  X, Y, direction, _jl_FFTW_ESTIMATE)
        end
        function _jl_fftw_plan_guru_dft(dims::Array{Int,2}, howmany::Array{Int,2},
            X::Array{$T_in}, Y::Array{$T_out})
            ccall(dlsym($libname, $fname_real),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Int32, Ptr{Int32},
                   Ptr{$T_in}, Ptr{$T_out}, Uint32),
                  size(dims,2), int32(dims), size(howmany,2), int32(howmany),
                  X, Y, _jl_FFTW_ESTIMATE)
        end
    end
end

# fftn/ifftn

for (fname,direction) in ((:fftn,:_jl_FFTW_FORWARD),(:ifftn,:_jl_FFTW_BACKWARD))
    @eval begin
        function ($fname){T<:Union(Complex128,Complex64)}(X::Array{T})
            Y = similar(X, T)
            plan = _jl_fftw_plan_dft(X, Y, $direction)
            _jl_fftw_execute(T, plan)
            _jl_fftw_destroy_plan(T, plan)
            return Y
        end

        function ($fname){T<:Union(Float64,Float32)}(X::Array{T})
            Y = complex(X) # in-place transform
            plan = _jl_fftw_plan_dft(Y, Y, $direction)
            _jl_fftw_execute(T, plan)
            _jl_fftw_destroy_plan(T, plan)
            return Y
        end
    end
end

# Convenience functions

fft2{T}(X::Matrix{T}) = fftn(X)
fft3{T}(X::Array{T,3}) = fftn(X)
ifft2{T}(X::Matrix{T}) = ifftn(X)
ifft3{T}(X::Array{T,3}) = ifftn(X)

# Compute fft and ifft of slices of arrays

fft(X) = fft(X, 1)
ifft(X) = ifft(X, 1)

for (fname,direction) in ((:fft,:_jl_FFTW_FORWARD),(:ifft,:_jl_FFTW_BACKWARD))
    @eval begin
        function ($fname){T<:Union(Complex128,Complex64)}(X::Array{T}, dim::Int)
            s = [size(X)...]
            strides = [ prod(s[1:i-1]) for i=1:length(s) ]
            dims = [s[dim],strides[dim],strides[dim]]''
            del(s, dim)
            del(strides, dim)
            howmany = [s strides strides]'
            Y = similar(X, T)
            plan = _jl_fftw_plan_guru_dft(dims, howmany, X, Y, $direction)
            _jl_fftw_execute(T, plan)
            _jl_fftw_destroy_plan(T, plan)
            return Y
        end

        function ($fname){T<:Union(Float64,Float32)}(X::Array{T}, dim::Int)
            s = [size(X)...]
            strides = [ prod(s[1:i-1]) for i=1:length(s) ]
            n = s[dim]
            dims = [n,strides[dim],strides[dim]]''
            del(s, dim)
            del(strides, dim)
            howmany = [s strides strides]'
            Y = complex(X) # in-place transform
            plan = _jl_fftw_plan_guru_dft(dims, howmany, Y, Y, $direction)
            _jl_fftw_execute(T, plan)
            _jl_fftw_destroy_plan(T, plan)
            return Y
        end
    end
end

# rfft/rfftn

rfft(X) = rfft(X, 1)
for (Tr,Tc) in ((:Float32,:Complex64),(:Float64,:Complex128))
    @eval begin
        function rfftn(X::Array{$Tr})
            osize = [size(X)...]
            osize[1] = ifloor(osize[1]/2) + 1
            Y = Array($Tc, osize...)
            plan = _jl_fftw_plan_dft(X, Y)
            _jl_fftw_execute($Tr, plan)
            _jl_fftw_destroy_plan($Tr, plan)
            return Y
        end

        function rfft(X::Array{$Tr}, dim::Int)
            isize = [size(X)...]
            osize = [size(X)...]
            osize[dim] = ifloor(osize[dim]/2) + 1
            istrides = [ prod(isize[1:i-1]) | i=1:length(isize) ]
            ostrides = [ prod(osize[1:i-1]) | i=1:length(osize) ]
            Y = Array($Tc, osize...)
            dims = [isize[dim],istrides[dim],ostrides[dim]]''
            del(isize, dim)
            del(istrides, dim)
            del(ostrides, dim)
            howmany = [isize istrides ostrides]'
            plan = _jl_fftw_plan_guru_dft(dims, howmany, X, Y)
            _jl_fftw_execute($Tr, plan)
            _jl_fftw_destroy_plan($Tr, plan)
            return Y
        end
    end
end

# Transpose
# NOTE: Using _jl_FFTW_MEASURE and _jl_FFTW_PATIENT zeros out the input the 
# first time it is used for a particular size. Use _jl_FFTW_ESTIMATE

for (libname, fname, elty) in ((:_jl_libfftw ,"fftw_plan_guru_r2r",:Float64),
                               (:_jl_libfftwf,"fftwf_plan_guru_r2r",:Float32))
    @eval begin
        function _jl_fftw_transpose(X::Matrix{$elty})
            P = similar(X)
            (n1, n2) = size(X)
            plan = ccall(dlsym($libname, $fname), Ptr{Void},
                         (Int32, Ptr{Int32}, Int32, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Uint32),
                         0, C_NULL, 2, int32([n1,n2,1,n2,1,n1]), X, P, [_jl_FFTW_HC2R], _jl_FFTW_ESTIMATE | _jl_FFTW_PRESERVE_INPUT)
            _jl_fftw_execute($elty, plan)
            _jl_fftw_destroy_plan($elty, plan)
            return P
        end
    end
end

for (libname, fname, celty) in ((:_jl_libfftw ,"fftw_plan_guru_dft",:Complex128),
                                (:_jl_libfftwf,"fftwf_plan_guru_dft",:Complex64))
    @eval begin
        function _jl_fftw_transpose(X::Matrix{$celty})
            P = similar(X)
            (n1, n2) = size(X)
            plan = ccall(dlsym($libname, $fname), Ptr{Void},
                         (Int32, Ptr{Int32}, Int32, Ptr{Int32}, Ptr{$celty}, Ptr{$celty}, Int32, Uint32),
                         0, C_NULL, 2, int32([n1,n2,1,n2,1,n1]), X, P, _jl_FFTW_FORWARD, _jl_FFTW_ESTIMATE | _jl_FFTW_PRESERVE_INPUT)
            _jl_fftw_execute($celty, plan)
            _jl_fftw_destroy_plan($celty, plan)
            return P
        end
    end
end
