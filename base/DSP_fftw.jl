module FFTW

import Base.*

global transpose
export bfft, bfftn, brfft, brfftn, fft, fft2, fft3, fftn,
       ifft, ifft2, ifft3, ifftn, irfft, irfftn, rfft, rfftn

## FFT: Implement fft by calling fftw.

## Direction of FFT

const FORWARD = int32(-1)
const BACKWARD = int32(1)

## _jl_FFTW Flags from fftw3.h

const MEASURE         = uint32(0)
const DESTROY_INPUT   = uint32(1 << 0)
const UNALIGNED       = uint32(1 << 1)
const CONSERVE_MEMORY = uint32(1 << 2)
const EXHAUSTIVE      = uint32(1 << 3)   # NO_EXHAUSTIVE is default
const PRESERVE_INPUT  = uint32(1 << 4)   # cancels DESTROY_INPUT
const PATIENT         = uint32(1 << 5)   # IMPATIENT is default
const ESTIMATE        = uint32(1 << 6)

## R2R transform kinds

const R2HC    = int32(0)
const HC2R    = int32(1)
const DHT     = int32(2)
const REDFT00 = int32(3)
const REDFT01 = int32(4)
const REDFT10 = int32(5)
const REDFT11 = int32(6)
const RODFT00 = int32(7)
const RODFT01 = int32(8)
const RODFT10 = int32(9)
const RODFT11 = int32(10)

## Julia wrappers around FFTW functions

# Wisdom

function import_64bit_wisdom(filename::String)
    stat = ccall(dlsym(Base.libfftw,:fftw_import_wisdom_from_filename),
        Int32, (Ptr{Uint8},), bytestring(filename))
    if stat == 0
        error("failed to import wisdom from $filename")
    end
end

function import_32bit_wisdom(filename::String)
    stat = ccall(dlsym(Base.libfftwf,:fftwf_import_wisdom_from_filename),
        Int32, (Ptr{Uint8},), bytestring(filename))
    if stat == 0
        error("failed to import wisdom from $filename")
    end
end

function forget_wisdom()
    ccall(dlsym(Base.libfftw,:fftw_forget_wisdom), Void, ())
    ccall(dlsym(Base.libfftwf,:fftwf_forget_wisdom), Void, ())
end

# Threads

let initialized = false
    global num_threads
    function num_threads(nthreads::Integer)
        if !initialized
            stat = ccall(dlsym(Base.libfftw,:fftw_init_threads), Int32, ())
            statf = ccall(dlsym(Base.libfftwf,:fftwf_init_threads), Int32, ())
            if stat == 0 || statf == 0
                error("could not initialize fft threads")
            end
            initialized = true
        end
        ccall(dlsym(Base.libfftw,:fftw_plan_with_nthreads), Void, (Int32,), nthreads)
        ccall(dlsym(Base.libfftwf,:fftwf_plan_with_nthreads), Void, (Int32,), nthreads)
    end
end

# Execute

execute(precision::Union(Type{Float64}, Type{Complex128}), plan) =
    ccall(dlsym(Base.libfftw, :fftw_execute), Void, (Ptr{Void},), plan)

execute(precision::Union(Type{Float32}, Type{Complex64}), plan) =
    ccall(dlsym(Base.libfftwf, :fftwf_execute), Void, (Ptr{Void},), plan)

# Destroy plan

destroy_plan(precision::Union(Type{Float64}, Type{Complex128}), plan) =
    ccall(dlsym(Base.libfftw, :fftw_destroy_plan), Void, (Ptr{Void},), plan)

destroy_plan(precision::Union(Type{Float32}, Type{Complex64}), plan) =
    ccall(dlsym(Base.libfftwf, :fftwf_destroy_plan), Void, (Ptr{Void},), plan)

# Create nd plan

for (libname, fname_complex, fname_r2c, fname_c2r, T_in, T_out) in
    ((:(Base.libfftw),"fftw_plan_dft","fftw_plan_dft_r2c","fftw_plan_dft_c2r",:Float64,:Complex128),
     (:(Base.libfftwf),"fftwf_plan_dft","fftwf_plan_dft_r2c","fftwf_plan_dft_c2r",:Float32,:Complex64))
    @eval begin
        function plan_dft(X::Array{$T_out}, Y::Array{$T_out}, direction::Integer)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32, ),
                  ndims(X), int32(reverse([size(X)...])), X, Y, direction, ESTIMATE)
        end
        function plan_dft(X::Array{$T_in}, Y::Array{$T_out})
            ccall(dlsym($libname, $fname_r2c),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Ptr{$T_in}, Ptr{$T_out}, Uint32, ),
                  ndims(X), int32(reverse([size(X)...])), X, Y, ESTIMATE)
        end
        function plan_dft(X::Array{$T_out}, Y::Array{$T_in})
            ccall(dlsym($libname, $fname_c2r),
                  Ptr{Void},
                  (Int32, Ptr{Int32}, Ptr{$T_out}, Ptr{$T_in}, Uint32),
                  ndims(Y), int32(reverse([size(Y)...])), X, Y, ESTIMATE)
        end
    end
end

# Guru plans

for (libname, fname_complex, fname_r2c, fname_c2r, T_in, T_out) in
    ((:(Base.libfftw),"fftw_plan_guru64_dft","fftw_plan_guru64_dft_r2c","fftw_plan_guru64_dft_c2r",:Float64,:Complex128),
     (:(Base.libfftwf),"fftwf_plan_guru64_dft","fftwf_plan_guru64_dft_r2c","fftwf_plan_guru64_dft_c2r",:Float32,:Complex64))
    @eval begin
        function plan_guru_dft(dims::Array{Int,2}, howmany::Array{Int,2},
            X::Array{$T_out}, Y::Array{$T_out}, direction::Int32)
            ccall(dlsym($libname, $fname_complex),
                  Ptr{Void},
                  (Int32, Ptr{Int}, Int32, Ptr{Int},
                   Ptr{$T_out}, Ptr{$T_out}, Int32, Uint32),
                  size(dims,2), dims, size(howmany,2), howmany,
                  X, Y, direction, ESTIMATE)
        end
        function plan_guru_dft(dims::Array{Int,2}, howmany::Array{Int,2},
            X::Array{$T_in}, Y::Array{$T_out})
            ccall(dlsym($libname, $fname_r2c),
                  Ptr{Void},
                  (Int32, Ptr{Int}, Int32, Ptr{Int},
                   Ptr{$T_in}, Ptr{$T_out}, Uint32),
                  size(dims,2), dims, size(howmany,2), howmany,
                  X, Y, ESTIMATE)
        end
        function plan_guru_dft(dims::Array{Int,2}, howmany::Array{Int,2},
            X::Array{$T_out}, Y::Array{$T_in})
            ccall(dlsym($libname, $fname_c2r),
                  Ptr{Void},
                  (Int32, Ptr{Int}, Int32, Ptr{Int},
                   Ptr{$T_out}, Ptr{$T_in}, Uint32),
                  size(dims,2), dims, size(howmany,2), howmany,
                  X, Y, ESTIMATE)
        end
    end
end

# fftn/ifftn

for (fname,direction) in ((:fftn,:FORWARD),(:bfftn,:BACKWARD))
    @eval begin
        function ($fname){T<:Union(Complex128,Complex64)}(X::Array{T})
            Y = similar(X, T)
            plan = plan_dft(X, Y, $direction)
            execute(T, plan)
            destroy_plan(T, plan)
            return Y
        end

        function ($fname){T<:Union(Float64,Float32)}(X::Array{T})
            Y = complex(X) # in-place transform
            plan = plan_dft(Y, Y, $direction)
            execute(T, plan)
            destroy_plan(T, plan)
            return Y
        end
    end
end

ifftn(X) = bfftn(X)./length(X)

# Convenience functions

fft2{T}(X::Matrix{T}) = fftn(X)
fft3{T}(X::Array{T,3}) = fftn(X)
ifft2{T}(X::Matrix{T}) = ifftn(X)
ifft3{T}(X::Array{T,3}) = ifftn(X)

# Compute fft and ifft of slices of arrays

fft(X) = fft(X, 1)
ifft(X) = ifft(X, 1)
ifft(X,dim) = bfft(X,dim)./size(X,dim)

for (fname,direction) in ((:fft,:FORWARD),(:bfft,:BACKWARD))
    @eval begin
        function ($fname){T<:Union(Complex128,Complex64)}(X::Array{T}, dim::Int)
            s = [size(X)...]
            strides = [ prod(s[1:i-1]) for i=1:length(s) ]
            dims = [s[dim],strides[dim],strides[dim]]''
            del(s, dim)
            del(strides, dim)
            howmany = [s strides strides]'
            Y = similar(X, T)
            plan = plan_guru_dft(dims, howmany, X, Y, $direction)
            execute(T, plan)
            destroy_plan(T, plan)
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
            plan = plan_guru_dft(dims, howmany, Y, Y, $direction)
            execute(T, plan)
            destroy_plan(T, plan)
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
            plan = plan_dft(X, Y)
            execute($Tr, plan)
            destroy_plan($Tr, plan)
            return Y
        end

        function rfft(X::Array{$Tr}, dim::Int)
            isize = [size(X)...]
            osize = [size(X)...]
            osize[dim] = ifloor(osize[dim]/2) + 1
            istrides = [ prod(isize[1:i-1]) for i=1:length(isize) ]
            ostrides = [ prod(osize[1:i-1]) for i=1:length(osize) ]
            Y = Array($Tc, osize...)
            dims = [isize[dim],istrides[dim],ostrides[dim]]''
            del(isize, dim)
            del(istrides, dim)
            del(ostrides, dim)
            howmany = [isize istrides ostrides]'
            plan = plan_guru_dft(dims, howmany, X, Y)
            execute($Tr, plan)
            destroy_plan($Tr, plan)
            return Y
        end

        function brfftn(X::Array{$Tc}, d::Int)
            osize = [size(X)...]
            @assert osize[1] == ifloor(d/2) + 1
            osize[1] = d
            Y = Array($Tr, osize...)
            plan = plan_dft(X, Y)
            execute($Tr, plan)
            destroy_plan($Tr, plan)
            return Y
        end

        function brfft(X::Array{$Tc}, d::Int, dim::Int)
            isize = [size(X)...]
            osize = [size(X)...]
            @assert osize[dim] == ifloor(d/2) + 1
            osize[dim] = d
            istrides = [ prod(isize[1:i-1]) for i=1:length(isize) ]
            ostrides = [ prod(osize[1:i-1]) for i=1:length(osize) ]
            Y = Array($Tr, osize...)
            dims = [osize[dim],istrides[dim],ostrides[dim]]''
            del(osize, dim)
            del(istrides, dim)
            del(ostrides, dim)
            howmany = [osize istrides ostrides]'
            plan = plan_guru_dft(dims, howmany, X, Y)
            execute($Tr, plan)
            destroy_plan($Tr, plan)
            return Y
        end
    end
end

brfft(X,d) = brfft(X,d,1)
irfft(X,d) = irfft(X,d,1)
irfft(X,d,dim) = brfft(X,d,dim)./d
irfftn(X,d) = (Y=brfftn(X,d); Y./length(Y))

# Transpose
# NOTE: Using MEASURE and PATIENT zeros out the input the 
# first time it is used for a particular size. Use ESTIMATE

for (libname, fname, elty) in ((:(Base.libfftw) ,"fftw_plan_guru_r2r",:Float64),
                               (:(Base.libfftwf),"fftwf_plan_guru_r2r",:Float32))
    @eval begin
        function transpose(X::Matrix{$elty})
            P = similar(X)
            (n1, n2) = size(X)
            plan = ccall(dlsym($libname, $fname), Ptr{Void},
                         (Int32, Ptr{Int32}, Int32, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Uint32),
                         0, C_NULL, 2, int32([n1,n2,1,n2,1,n1]), X, P, [HC2R], ESTIMATE | PRESERVE_INPUT)
            execute($elty, plan)
            destroy_plan($elty, plan)
            return P
        end
    end
end

for (libname, fname, celty) in ((:(Base.libfftw) ,"fftw_plan_guru_dft",:Complex128),
                                (:(Base.libfftwf),"fftwf_plan_guru_dft",:Complex64))
    @eval begin
        function transpose(X::Matrix{$celty})
            P = similar(X)
            (n1, n2) = size(X)
            plan = ccall(dlsym($libname, $fname), Ptr{Void},
                         (Int32, Ptr{Int32}, Int32, Ptr{Int32}, Ptr{$celty}, Ptr{$celty}, Int32, Uint32),
                         0, C_NULL, 2, int32([n1,n2,1,n2,1,n1]), X, P, FORWARD, ESTIMATE | PRESERVE_INPUT)
            execute($celty, plan)
            destroy_plan($celty, plan)
            return P
        end
    end
end

end # module
