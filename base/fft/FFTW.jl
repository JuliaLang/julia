# This file is a part of Julia. License is MIT: http://julialang.org/license

module FFTW

import ..DFT: fft, bfft, ifft, rfft, brfft, irfft, plan_fft, plan_bfft, plan_ifft, plan_rfft, plan_brfft, plan_irfft, fft!, bfft!, ifft!, plan_fft!, plan_bfft!, plan_ifft!, Plan, rfft_output_size, brfft_output_size, plan_inv, normalization, ScaledPlan

import Base: show, *, convert, unsafe_convert, size, strides, ndims, pointer, A_mul_B!

export r2r, r2r!, plan_r2r, plan_r2r!

export export_wisdom, import_wisdom, import_system_wisdom, forget_wisdom,
       MEASURE, DESTROY_INPUT, UNALIGNED, CONSERVE_MEMORY, EXHAUSTIVE,
       PRESERVE_INPUT, PATIENT, ESTIMATE, WISDOM_ONLY, NO_TIMELIMIT,
       R2HC, HC2R, DHT, REDFT00, REDFT01, REDFT10, REDFT11,
       RODFT00, RODFT01, RODFT10, RODFT11,
       fftwNumber, fftwReal, fftwComplex, flops

## FFT: Implement fft by calling fftw.

const libfftw = Base.libfftw_name
const libfftwf = Base.libfftwf_name

const version = convert(VersionNumber, split(unsafe_string(cglobal((:fftw_version,Base.DFT.FFTW.libfftw), UInt8)), ['-', ' '])[2])

## Direction of FFT

const FORWARD = -1
const BACKWARD = 1

## FFTW Flags from fftw3.h

const MEASURE         = UInt32(0)
const DESTROY_INPUT   = UInt32(1 << 0)
const UNALIGNED       = UInt32(1 << 1)
const CONSERVE_MEMORY = UInt32(1 << 2)
const EXHAUSTIVE      = UInt32(1 << 3)   # NO_EXHAUSTIVE is default
const PRESERVE_INPUT  = UInt32(1 << 4)   # cancels DESTROY_INPUT
const PATIENT         = UInt32(1 << 5)   # IMPATIENT is default
const ESTIMATE        = UInt32(1 << 6)
const WISDOM_ONLY     = UInt32(1 << 21)
const NO_SIMD = UInt32(1 << 17) # disable SIMD, useful for benchmarking

## R2R transform kinds

const R2HC    = 0
const HC2R    = 1
const DHT     = 2
const REDFT00 = 3
const REDFT01 = 4
const REDFT10 = 5
const REDFT11 = 6
const RODFT00 = 7
const RODFT01 = 8
const RODFT10 = 9
const RODFT11 = 10

let k2s = Dict(R2HC => "R2HC", HC2R => "HC2R", DHT => "DHT", REDFT00 => "REDFT00", REDFT01 => "REDFT01", REDFT10 => "REDFT10", REDFT11 => "REDFT11", RODFT00 => "RODFT00", RODFT01 => "RODFT01", RODFT10 => "RODFT10", RODFT11 => "RODFT11")
    global kind2string
    kind2string(k::Integer) = k2s[Int(k)]
end

# FFTW floating-point types:

typealias fftwNumber Union{Float64,Float32,Complex128,Complex64}
typealias fftwReal Union{Float64,Float32}
typealias fftwComplex Union{Complex128,Complex64}
typealias fftwDouble Union{Float64,Complex128}
typealias fftwSingle Union{Float32,Complex64}
typealias fftwTypeDouble Union{Type{Float64},Type{Complex128}}
typealias fftwTypeSingle Union{Type{Float32},Type{Complex64}}

# For ESTIMATE plans, FFTW allows one to pass NULL for the array pointer,
# since it is not written to.  Hence, it is convenient to create an
# array-like type that carries a size and a stride like a "real" array
# but which is converted to C_NULL as a pointer.
immutable FakeArray{T, N} <: DenseArray{T, N}
    sz::NTuple{N, Int}
    st::NTuple{N, Int}
end
size(a::FakeArray) = a.sz
strides(a::FakeArray) = a.st
unsafe_convert{T}(::Type{Ptr{T}}, a::FakeArray{T}) = convert(Ptr{T}, C_NULL)
pointer{T}(a::FakeArray{T}) = convert(Ptr{T}, C_NULL)
FakeArray{T, N}(::Type{T}, sz::NTuple{N, Int}) =
    FakeArray{T, N}(sz, colmajorstrides(sz))
FakeArray{T}(::Type{T}, sz::Int...) = FakeArray(T, sz)
fakesimilar(flags, X, T) = flags & ESTIMATE != 0 ? FakeArray(T, size(X)) : Array{T}(size(X))
alignment_of(A::FakeArray) = Int32(0)

## Julia wrappers around FFTW functions

# Wisdom

# Import and export wisdom to/from a single file for all precisions,
# which is more user-friendly than requiring the user to call a
# separate routine depending on the fp precision of the plans.  This
# requires a bit of trickness since we have to (a) use the libc file
# I/O routines with fftw_export_wisdom_to_file/import_wisdom_from_file
# (b) we need 256 bytes of space padding between the wisdoms to work
# around FFTW's internal file i/o buffering [see the BUFSZ constant in
# FFTW's api/import-wisdom-from-file.c file].

function export_wisdom(fname::AbstractString)
    f = ccall(:fopen, Ptr{Void}, (Cstring,Cstring), fname, :w)
    systemerror("could not open wisdom file $fname for writing", f == C_NULL)
    ccall((:fftw_export_wisdom_to_file,libfftw), Void, (Ptr{Void},), f)
    ccall(:fputs, Int32, (Ptr{UInt8},Ptr{Void}), " "^256, f) # no NUL, hence no Cstring
    ccall((:fftwf_export_wisdom_to_file,libfftwf), Void, (Ptr{Void},), f)
    ccall(:fclose, Void, (Ptr{Void},), f)
end

function import_wisdom(fname::AbstractString)
    f = ccall(:fopen, Ptr{Void}, (Cstring,Cstring), fname, :r)
    systemerror("could not open wisdom file $fname for reading", f == C_NULL)
    if ccall((:fftw_import_wisdom_from_file,libfftw),Int32,(Ptr{Void},),f)==0||
       ccall((:fftwf_import_wisdom_from_file,libfftwf),Int32,(Ptr{Void},),f)==0
        error("failed to import wisdom from $fname")
    end
    ccall(:fclose, Void, (Ptr{Void},), f)
end

function import_system_wisdom()
    if ccall((:fftw_import_system_wisdom,libfftw), Int32, ()) == 0 ||
       ccall((:fftwf_import_system_wisdom,libfftwf), Int32, ()) == 0
        error("failed to import system wisdom")
    end
end

function forget_wisdom()
    ccall((:fftw_forget_wisdom,libfftw), Void, ())
    ccall((:fftwf_forget_wisdom,libfftwf), Void, ())
end

# Threads

let initialized = false
    global set_num_threads
    function set_num_threads(nthreads::Integer)
        if !initialized
            # must re-initialize FFTW if any FFTW routines have been called
            ccall((:fftw_cleanup,libfftw), Void, ())
            ccall((:fftwf_cleanup,libfftwf), Void, ())
            stat = ccall((:fftw_init_threads,libfftw), Int32, ())
            statf = ccall((:fftwf_init_threads,libfftwf), Int32, ())
            if stat == 0 || statf == 0
                error("could not initialize FFTW threads")
            end
            initialized = true
        end
        ccall((:fftw_plan_with_nthreads,libfftw), Void, (Int32,), nthreads)
        ccall((:fftwf_plan_with_nthreads,libfftwf), Void, (Int32,), nthreads)
    end
end

# pointer type for fftw_plan (opaque pointer)

immutable fftw_plan_struct end
typealias PlanPtr Ptr{fftw_plan_struct}

# Planner timelimits

const NO_TIMELIMIT = -1.0 # from fftw3.h

set_timelimit(precision::fftwTypeDouble,seconds) =
    ccall((:fftw_set_timelimit,libfftw), Void, (Float64,), seconds)

set_timelimit(precision::fftwTypeSingle,seconds) =
    ccall((:fftwf_set_timelimit,libfftwf), Void, (Float64,), seconds)

# Array alignment mod 16:
#   FFTW plans may depend on the alignment of the array mod 16 bytes,
#   i.e. the address mod 16 of the first element of the array, in order
#   to exploit SIMD operations.  Julia arrays are, by default, aligned
#   to 16-byte boundaries (address mod 16 == 0), but this may not be
#   true for data imported from external C code, or for SubArrays.
#   Use the undocumented routine fftw_alignment_of to determine the
#   alignment of a given pointer modulo whatever FFTW needs; this
#   function will be documented in FFTW 3.3.4.


if Base.libfftw_name == "libmkl_rt"
    alignment_of{T<:fftwDouble}(A::StridedArray{T}) =
        convert(Int32, convert(Int64, pointer(A)) % 16)
    alignment_of{T<:fftwSingle}(A::StridedArray{T}) =
        convert(Int32, convert(Int64, pointer(A)) % 16)
else
    alignment_of{T<:fftwDouble}(A::StridedArray{T}) =
        ccall((:fftw_alignment_of, libfftw), Int32, (Ptr{T},), A)
    alignment_of{T<:fftwSingle}(A::StridedArray{T}) =
        ccall((:fftwf_alignment_of, libfftwf), Int32, (Ptr{T},), A)
end

# FFTWPlan (low-level)

# low-level storage of the FFTW plan, along with the information
# needed to determine whether it is applicable.   We need to put
# this into a type to support a finalizer on the fftw_plan.
# K is FORWARD/BACKWARD for forward/backward or r2c/c2r plans, respectively.
# For r2r plans, K is a tuple of the transform kinds along each dimension.
abstract FFTWPlan{T<:fftwNumber,K,inplace} <: Plan{T}
for P in (:cFFTWPlan, :rFFTWPlan, :r2rFFTWPlan) # complex, r2c/c2r, and r2r
    @eval begin
        type $P{T<:fftwNumber,K,inplace,N} <: FFTWPlan{T,K,inplace}
            plan::PlanPtr
            sz::NTuple{N, Int} # size of array on which plan operates (Int tuple)
            osz::NTuple{N, Int} # size of output array (Int tuple)
            istride::NTuple{N, Int} # strides of input
            ostride::NTuple{N, Int} # strides of output
            ialign::Int32 # alignment mod 16 of input
            oalign::Int32 # alignment mod 16 of input
            flags::UInt32 # planner flags
            region::Any # region (iterable) of dims that are transormed
            pinv::ScaledPlan
            function $P{T,K,inplace,N}(plan::PlanPtr, flags::Integer, R::Any,
                                       X::StridedArray{T, N}, Y::StridedArray) where {T<:fftwNumber,K,inplace,N}
                p = new(plan, size(X), size(Y), strides(X), strides(Y),
                        alignment_of(X), alignment_of(Y), flags, R)
                finalizer(p, destroy_plan)
                p
            end
        end
    end
end

size(p::FFTWPlan) = p.sz

unsafe_convert(::Type{PlanPtr}, p::FFTWPlan) = p.plan

destroy_plan{T<:fftwDouble}(plan::FFTWPlan{T}) =
    ccall((:fftw_destroy_plan,libfftw), Void, (PlanPtr,), plan)

destroy_plan{T<:fftwSingle}(plan::FFTWPlan{T}) =
    ccall((:fftwf_destroy_plan,libfftwf), Void, (PlanPtr,), plan)

cost{T<:fftwDouble}(plan::FFTWPlan{T}) =
    ccall((:fftw_cost,libfftw), Float64, (PlanPtr,), plan)
cost{T<:fftwSingle}(plan::FFTWPlan{T}) =
    ccall((:fftwf_cost,libfftwf), Float64, (PlanPtr,), plan)

function arithmetic_ops{T<:fftwDouble}(plan::FFTWPlan{T})
    # Change to individual Ref after we can allocate them on stack
    ref = Ref{NTuple{3, Float64}}()
    ptr = Ptr{Float64}(Base.unsafe_convert(Ptr{NTuple{3, Float64}}, ref))
    ccall((:fftw_flops,libfftw), Void,
          (PlanPtr,Ptr{Float64},Ptr{Float64},Ptr{Float64}),
          plan, ptr, ptr + 8, ptr + 16)
    (round(Int64, ref[][1]), round(Int64, ref[][2]), round(Int64, ref[][3]))
end
function arithmetic_ops{T<:fftwSingle}(plan::FFTWPlan{T})
    # Change to individual Ref after we can allocate them on stack
    ref = Ref{NTuple{3, Float64}}()
    ptr = Ptr{Float64}(Base.unsafe_convert(Ptr{NTuple{3, Float64}}, ref))
    ccall((:fftwf_flops,libfftwf), Void,
          (PlanPtr,Ptr{Float64},Ptr{Float64},Ptr{Float64}),
          plan, ptr, ptr + 8, ptr + 16)
    (round(Int64, ref[][1]), round(Int64, ref[][2]), round(Int64, ref[][3]))
end
flops(plan::FFTWPlan) = let ops = arithmetic_ops(plan)
    ops[1] + ops[2] + 2 * ops[3] # add + mul + 2*fma
end

# Pretty-printing plans

function showfftdims(io, sz::Dims, istride::Dims, T)
    if isempty(sz)
        print(io, "0-dimensional")
    elseif length(sz) == 1
        print(io, sz[1], "-element")
    else
        print(io, join(sz, "×"))
    end
    if istride == colmajorstrides(sz)
        print(io, " array of ", T)
    else
        print(io, " $istride-strided array of ", T)
    end
end

# The sprint_plan function was released in FFTW 3.3.4
sprint_plan_{T<:fftwDouble}(plan::FFTWPlan{T}) =
    ccall((:fftw_sprint_plan,libfftw), Ptr{UInt8}, (PlanPtr,), plan)
sprint_plan_{T<:fftwSingle}(plan::FFTWPlan{T}) =
    ccall((:fftwf_sprint_plan,libfftwf), Ptr{UInt8}, (PlanPtr,), plan)
function sprint_plan(plan::FFTWPlan)
    p = sprint_plan_(plan)
    str = unsafe_string(p)
    Libc.free(p)
    return str
end

function show{T,K,inplace}(io::IO, p::cFFTWPlan{T,K,inplace})
    print(io, inplace ? "FFTW in-place " : "FFTW ",
          K < 0 ? "forward" : "backward", " plan for ")
    showfftdims(io, p.sz, p.istride, T)
    version >= v"3.3.4" && print(io, "\n", sprint_plan(p))
end

function show{T,K,inplace}(io::IO, p::rFFTWPlan{T,K,inplace})
    print(io, inplace ? "FFTW in-place " : "FFTW ",
          K < 0 ? "real-to-complex" : "complex-to-real",
          " plan for ")
    showfftdims(io, p.sz, p.istride, T)
    version >= v"3.3.4" && print(io, "\n", sprint_plan(p))
end

function show{T,K,inplace}(io::IO, p::r2rFFTWPlan{T,K,inplace})
    print(io, inplace ? "FFTW in-place r2r " : "FFTW r2r ")
    if isempty(K)
        print(io, "0-dimensional")
    elseif K == ntuple(i -> K[1], length(K))
        print(io, kind2string(K[1]))
        if length(K) > 1
            print(io, "^", length(K))
        end
    else
        print(io, join(map(kind2string, K), "×"))
    end
    print(io, " plan for ")
    showfftdims(io, p.sz, p.istride, T)
    version >= v"3.3.4" && print(io, "\n", sprint_plan(p))
end

# Check whether a FFTWPlan is applicable to a given input array, and
# throw an informative error if not:
function assert_applicable{T}(p::FFTWPlan{T}, X::StridedArray{T})
    if size(X) != p.sz
        throw(ArgumentError("FFTW plan applied to wrong-size array"))
    elseif strides(X) != p.istride
        throw(ArgumentError("FFTW plan applied to wrong-strides array"))
    elseif alignment_of(X) != p.ialign || p.flags & UNALIGNED != 0
        throw(ArgumentError("FFTW plan applied to array with wrong memory alignment"))
    end
end

function assert_applicable{T,K,inplace}(p::FFTWPlan{T,K,inplace}, X::StridedArray{T}, Y::StridedArray)
    assert_applicable(p, X)
    if size(Y) != p.osz
        throw(ArgumentError("FFTW plan applied to wrong-size output"))
    elseif strides(Y) != p.ostride
        throw(ArgumentError("FFTW plan applied to wrong-strides output"))
    elseif alignment_of(Y) != p.oalign || p.flags & UNALIGNED != 0
        throw(ArgumentError("FFTW plan applied to output with wrong memory alignment"))
    elseif inplace != (pointer(X) == pointer(Y))
        throw(ArgumentError(string("FFTW ",
                                   inplace ? "in-place" : "out-of-place",
                                   " plan applied to ",
                                   inplace ? "out-of-place" : "in-place",
                                   " data")))
    end
end

# strides for a column-major (Julia-style) array of size == sz
colmajorstrides(sz) = isempty(sz) ? () : (1,cumprod(Int[sz[1:end-1]...])...)

# Execute

unsafe_execute!{T<:fftwDouble}(plan::FFTWPlan{T}) =
    ccall((:fftw_execute,libfftw), Void, (PlanPtr,), plan)

unsafe_execute!{T<:fftwSingle}(plan::FFTWPlan{T}) =
    ccall((:fftwf_execute,libfftwf), Void, (PlanPtr,), plan)

unsafe_execute!{T<:fftwDouble}(plan::cFFTWPlan{T},
                               X::StridedArray{T}, Y::StridedArray{T}) =
    ccall((:fftw_execute_dft,libfftw), Void,
          (PlanPtr,Ptr{T},Ptr{T}), plan, X, Y)

unsafe_execute!{T<:fftwSingle}(plan::cFFTWPlan{T},
                               X::StridedArray{T}, Y::StridedArray{T}) =
    ccall((:fftwf_execute_dft,libfftwf), Void,
          (PlanPtr,Ptr{T},Ptr{T}), plan, X, Y)

unsafe_execute!(plan::rFFTWPlan{Float64,FORWARD},
                X::StridedArray{Float64}, Y::StridedArray{Complex128}) =
    ccall((:fftw_execute_dft_r2c,libfftw), Void,
          (PlanPtr,Ptr{Float64},Ptr{Complex128}), plan, X, Y)

unsafe_execute!(plan::rFFTWPlan{Float32,FORWARD},
                X::StridedArray{Float32}, Y::StridedArray{Complex64}) =
    ccall((:fftwf_execute_dft_r2c,libfftwf), Void,
          (PlanPtr,Ptr{Float32},Ptr{Complex64}), plan, X, Y)

unsafe_execute!(plan::rFFTWPlan{Complex128,BACKWARD},
                X::StridedArray{Complex128}, Y::StridedArray{Float64}) =
    ccall((:fftw_execute_dft_c2r,libfftw), Void,
          (PlanPtr,Ptr{Complex128},Ptr{Float64}), plan, X, Y)

unsafe_execute!(plan::rFFTWPlan{Complex64,BACKWARD},
                X::StridedArray{Complex64}, Y::StridedArray{Float32}) =
    ccall((:fftwf_execute_dft_c2r,libfftwf), Void,
          (PlanPtr,Ptr{Complex64},Ptr{Float32}), plan, X, Y)

unsafe_execute!{T<:fftwDouble}(plan::r2rFFTWPlan{T},
                               X::StridedArray{T}, Y::StridedArray{T}) =
    ccall((:fftw_execute_r2r,libfftw), Void,
          (PlanPtr,Ptr{T},Ptr{T}), plan, X, Y)

unsafe_execute!{T<:fftwSingle}(plan::r2rFFTWPlan{T},
                               X::StridedArray{T}, Y::StridedArray{T}) =
    ccall((:fftwf_execute_r2r,libfftwf), Void,
          (PlanPtr,Ptr{T},Ptr{T}), plan, X, Y)

# NOTE ON GC (garbage collection):
#    The FFTWPlan has a finalizer so that gc will destroy the plan,
#    which is necessary for gc to work with plan_fft.  However,
#    even when we are creating a single-use FFTWPlan [e.g. for fftn(x)],
#    we intentionally do NOT call destroy_plan explicitly, and instead
#    wait for garbage collection.  The reason is that, in the common
#    case where the user calls fft(x) a second time soon afterwards,
#    if destroy_plan has not yet been called then FFTW will internally
#    re-use the table of trigonometric constants from the first plan.

# Compute dims and howmany for FFTW guru planner
function dims_howmany(X::StridedArray, Y::StridedArray,
                      sz::Array{Int,1}, region)
    reg = [region...]
    if length(unique(reg)) < length(reg)
        throw(ArgumentError("each dimension can be transformed at most once"))
    end
    ist = [strides(X)...]
    ost = [strides(Y)...]
    dims = [sz[reg] ist[reg] ost[reg]]'
    oreg = [1:ndims(X);]
    oreg[reg] = 0
    oreg = filter(d -> d > 0, oreg)
    howmany = [sz[oreg] ist[oreg] ost[oreg]]'
    return (dims, howmany)
end

# check & convert kinds into int32 array with same length as region
function fix_kinds(region, kinds)
    if length(kinds) != length(region)
        if length(kinds) > length(region)
            throw(ArgumentError("too many transform kinds"))
        else
            if isempty(kinds)
                throw(ArgumentError("must supply a transform kind"))
            end
            k = Array{Int32}(length(region))
            k[1:length(kinds)] = [kinds...]
            k[length(kinds)+1:end] = kinds[end]
            kinds = k
        end
    else
        kinds = Int32[kinds...]
    end
    for i = 1:length(kinds)
        if kinds[i] < 0 || kinds[i] > 10
            throw(ArgumentError("invalid transform kind"))
        end
    end
    return kinds
end

# low-level FFTWPlan creation (for internal use in FFTW module)

for (Tr,Tc,fftw,lib) in ((:Float64,:Complex128,"fftw",libfftw),
                         (:Float32,:Complex64,"fftwf",libfftwf))
    @eval function (::Type{cFFTWPlan{$Tc,K,inplace,N}}){K,inplace,N}(X::StridedArray{$Tc,N},
                                                                     Y::StridedArray{$Tc,N},
                                                                     region, flags::Integer, timelimit::Real)
        direction = K
        set_timelimit($Tr, timelimit)
        R = isa(region, Tuple) ? region : copy(region)
        dims, howmany = dims_howmany(X, Y, [size(X)...], R)
        plan = ccall(($(string(fftw,"_plan_guru64_dft")),$lib),
                     PlanPtr,
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tc}, Ptr{$Tc}, Int32, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, direction, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        return cFFTWPlan{$Tc,K,inplace,N}(plan, flags, R, X, Y)
    end

    @eval function (::Type{rFFTWPlan{$Tr,$FORWARD,inplace,N}}){inplace,N}(X::StridedArray{$Tr,N},
                                                                          Y::StridedArray{$Tc,N},
                                                                          region, flags::Integer, timelimit::Real)
        R = isa(region, Tuple) ? region : copy(region)
        region = circshift([region...],-1) # FFTW halves last dim
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(X)...], region)
        plan = ccall(($(string(fftw,"_plan_guru64_dft_r2c")),$lib),
                     PlanPtr,
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tr}, Ptr{$Tc}, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        return rFFTWPlan{$Tr,$FORWARD,inplace,N}(plan, flags, R, X, Y)
    end

    @eval function (::Type{rFFTWPlan{$Tc,$BACKWARD,inplace,N}}){inplace,N}(X::StridedArray{$Tc,N},
                                                                           Y::StridedArray{$Tr,N},
                                                                           region, flags::Integer, timelimit::Real)
        R = isa(region, Tuple) ? region : copy(region)
        region = circshift([region...],-1) # FFTW halves last dim
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(Y)...], region)
        plan = ccall(($(string(fftw,"_plan_guru64_dft_c2r")),$lib),
                     PlanPtr,
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tc}, Ptr{$Tr}, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        return rFFTWPlan{$Tc,$BACKWARD,inplace,N}(plan, flags, R, X, Y)
    end

    @eval function (::Type{r2rFFTWPlan{$Tr,ANY,inplace,N}}){inplace,N}(X::StridedArray{$Tr,N},
                                                                       Y::StridedArray{$Tr,N},
                                                                       region, kinds, flags::Integer,
                                                                       timelimit::Real)
        R = isa(region, Tuple) ? region : copy(region)
        knd = fix_kinds(region, kinds)
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(X)...], region)
        plan = ccall(($(string(fftw,"_plan_guru64_r2r")),$lib),
                     PlanPtr,
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tr}, Ptr{$Tr}, Ptr{Int32}, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, knd, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        r2rFFTWPlan{$Tr,(map(Int,knd)...),inplace,N}(plan, flags, R, X, Y)
    end

    # support r2r transforms of complex = transforms of real & imag parts
    @eval function (::Type{r2rFFTWPlan{$Tc,ANY,inplace,N}}){inplace,N}(X::StridedArray{$Tc,N},
                                                                       Y::StridedArray{$Tc,N},
                                                                       region, kinds, flags::Integer,
                                                                       timelimit::Real)
        R = isa(region, Tuple) ? region : copy(region)
        knd = fix_kinds(region, kinds)
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(X)...], region)
        dims[2:3, 1:size(dims,2)] *= 2
        howmany[2:3, 1:size(howmany,2)] *= 2
        howmany = [howmany [2,1,1]] # append loop over real/imag parts
        plan = ccall(($(string(fftw,"_plan_guru64_r2r")),$lib),
                     PlanPtr,
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tc}, Ptr{$Tc}, Ptr{Int32}, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, knd, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        r2rFFTWPlan{$Tc,(map(Int,knd)...),inplace,N}(plan, flags, R, X, Y)
    end

end

# Convert arrays of numeric types to FFTW-supported packed complex-float types
# (FIXME: is there a way to use the Julia promotion rules more cleverly here?)
fftwcomplex{T<:fftwComplex}(X::StridedArray{T}) = X
fftwcomplex{T<:fftwReal}(X::AbstractArray{T}) =
    copy!(Array{typeof(complex(zero(T)))}(size(X)), X)
fftwcomplex{T<:Real}(X::AbstractArray{T}) = copy!(Array{Complex128}(size(X)),X)
fftwcomplex{T<:Complex}(X::AbstractArray{T}) =
    copy!(Array{Complex128}(size(X)), X)
fftwfloat{T<:fftwReal}(X::StridedArray{T}) = X
fftwfloat{T<:Real}(X::AbstractArray{T}) = copy!(Array{Float64}(size(X)), X)
fftwfloat{T<:Complex}(X::AbstractArray{T}) = fftwcomplex(X)

for (f,direction) in ((:fft,FORWARD), (:bfft,BACKWARD))
    plan_f = Symbol("plan_",f)
    plan_f! = Symbol("plan_",f,"!")
    idirection = -direction
    @eval begin
        function $plan_f{T<:fftwComplex,N}(X::StridedArray{T,N}, region;
                                           flags::Integer=ESTIMATE,
                                           timelimit::Real=NO_TIMELIMIT)
            cFFTWPlan{T,$direction,false,N}(X, fakesimilar(flags, X, T),
                                            region, flags, timelimit)
        end

        function $plan_f!{T<:fftwComplex,N}(X::StridedArray{T,N}, region;
                                            flags::Integer=ESTIMATE,
                                            timelimit::Real=NO_TIMELIMIT)
            cFFTWPlan{T,$direction,true,N}(X, X, region, flags, timelimit)
        end
        $plan_f{T<:fftwComplex}(X::StridedArray{T}; kws...) =
            $plan_f(X, 1:ndims(X); kws...)
        $plan_f!{T<:fftwComplex}(X::StridedArray{T}; kws...) =
            $plan_f!(X, 1:ndims(X); kws...)

        function plan_inv{T<:fftwComplex,N,inplace}(p::cFFTWPlan{T,$direction,inplace,N})
            X = Array{T}(p.sz)
            Y = inplace ? X : fakesimilar(p.flags, X, T)
            ScaledPlan(cFFTWPlan{T,$idirection,inplace,N}(X, Y, p.region,
                                                          p.flags, NO_TIMELIMIT),
                       normalization(X, p.region))
        end
    end
end

function A_mul_B!{T}(y::StridedArray{T}, p::cFFTWPlan{T}, x::StridedArray{T})
    assert_applicable(p, x, y)
    unsafe_execute!(p, x, y)
    return y
end

function *{T,K,N}(p::cFFTWPlan{T,K,false}, x::StridedArray{T,N})
    assert_applicable(p, x)
    y = Array{T}(p.osz)::Array{T,N}
    unsafe_execute!(p, x, y)
    return y
end

function *{T,K}(p::cFFTWPlan{T,K,true}, x::StridedArray{T})
    assert_applicable(p, x)
    unsafe_execute!(p, x, x)
    return x
end

# rfft/brfft and planned variants.  No in-place version for now.

for (Tr,Tc) in ((:Float32,:Complex64),(:Float64,:Complex128))
    # Note: use $FORWARD and $BACKWARD below because of issue #9775
    @eval begin
        function plan_rfft{N}(X::StridedArray{$Tr,N}, region;
                              flags::Integer=ESTIMATE,
                              timelimit::Real=NO_TIMELIMIT)
            osize = rfft_output_size(X, region)
            Y = flags&ESTIMATE != 0 ? FakeArray($Tc,osize...) : Array{$Tc}(osize...)
            rFFTWPlan{$Tr,$FORWARD,false,N}(X, Y, region, flags, timelimit)
        end

        function plan_brfft{N}(X::StridedArray{$Tc,N}, d::Integer, region;
                               flags::Integer=ESTIMATE,
                               timelimit::Real=NO_TIMELIMIT)
            osize = brfft_output_size(X, d, region)
            Y = flags&ESTIMATE != 0 ? FakeArray($Tr,osize...) : Array{$Tr}(osize...)

            # FFTW currently doesn't support PRESERVE_INPUT for
            # multidimensional out-of-place c2r transforms, so
            # we have to handle 1d and >1d cases separately with a copy.  Ugh.
            if length(region) <= 1
                rFFTWPlan{$Tc,$BACKWARD,false,N}(X, Y, region,
                                                 flags | PRESERVE_INPUT,
                                                 timelimit)
            else
                rFFTWPlan{$Tc,$BACKWARD,false,N}(copy(X), Y, region, flags,
                                                 timelimit)
            end
        end

        plan_rfft(X::StridedArray{$Tr};kws...)=plan_rfft(X,1:ndims(X);kws...)
        plan_brfft(X::StridedArray{$Tr};kws...)=plan_brfft(X,1:ndims(X);kws...)

        function plan_inv{N}(p::rFFTWPlan{$Tr,$FORWARD,false,N})
            X = Array{$Tr}(p.sz)
            Y = p.flags&ESTIMATE != 0 ? FakeArray($Tc,p.osz) : Array{$Tc}(p.osz)
            ScaledPlan(rFFTWPlan{$Tc,$BACKWARD,false,N}(Y, X, p.region,
                                                        length(p.region) <= 1 ?
                                                        p.flags | PRESERVE_INPUT :
                                                        p.flags, NO_TIMELIMIT),
                       normalization(X, p.region))
        end

        function plan_inv{N}(p::rFFTWPlan{$Tc,$BACKWARD,false,N})
            X = Arra{$Tc}(p.sz)
            Y = p.flags&ESTIMATE != 0 ? FakeArray($Tr,p.osz) : Array{$Tr}(p.osz)
            ScaledPlan(rFFTWPlan{$Tr,$FORWARD,false,N}(Y, X, p.region,
                                                       p.flags, NO_TIMELIMIT),
                       normalization(Y, p.region))
        end

        function A_mul_B!(y::StridedArray{$Tc}, p::rFFTWPlan{$Tr,$FORWARD}, x::StridedArray{$Tr})
            assert_applicable(p, x, y)
            unsafe_execute!(p, x, y)
            return y
        end
        function A_mul_B!(y::StridedArray{$Tr}, p::rFFTWPlan{$Tc,$BACKWARD}, x::StridedArray{$Tc})
            assert_applicable(p, x, y)
            unsafe_execute!(p, x, y) # note: may overwrite x as well as y!
            return y
        end

        function *{N}(p::rFFTWPlan{$Tr,$FORWARD,false}, x::StridedArray{$Tr,N})
            assert_applicable(p, x)
            y = Array{$Tc}(p.osz)::Array{$Tc,N}
            unsafe_execute!(p, x, y)
            return y
        end

        function *{N}(p::rFFTWPlan{$Tc,$BACKWARD,false}, x::StridedArray{$Tc,N})
            if p.flags & PRESERVE_INPUT != 0
                assert_applicable(p, x)
                y = Array{$Tr}(p.osz)::Array{$Tr,N}
                unsafe_execute!(p, x, y)
            else # need to make a copy to avoid overwriting x
                xc = copy(x)
                assert_applicable(p, xc)
                y = Array{$Tr}(p.osz)::Array{$Tr,N}
                unsafe_execute!(p, xc, y)
            end
            return y
        end
    end
end

"""
    plan_rfft(A [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Pre-plan an optimized real-input FFT, similar to [`plan_fft`](@ref) except for
[`rfft`](@ref) instead of [`fft`](@ref). The first two arguments, and the
size of the transformed result, are the same as for [`rfft`](@ref).
"""
plan_rfft

"""
    plan_brfft(A, d [, dims]; flags=FFTW.ESTIMATE;  timelimit=Inf)

Pre-plan an optimized real-input unnormalized transform, similar to
[`plan_rfft`](@ref) except for [`brfft`](@ref) instead of
[`rfft`](@ref). The first two arguments and the size of the transformed result, are
the same as for [`brfft`](@ref).
"""
plan_brfft

# FFTW r2r transforms (low-level interface)

for f in (:r2r, :r2r!)
    pf = Symbol("plan_", f)
    @eval begin
        $f{T<:fftwNumber}(x::AbstractArray{T}, kinds) = $pf(x, kinds) * x
        $f{T<:fftwNumber}(x::AbstractArray{T}, kinds, region) = $pf(x, kinds, region) * x
        $pf(x::AbstractArray, kinds; kws...) = $pf(x, kinds, 1:ndims(x); kws...)
        $f{T<:Real}(x::AbstractArray{T}, kinds, region=1:ndims(x)) = $f(fftwfloat(x), kinds, region)
        $pf{T<:Real}(x::AbstractArray{T}, kinds, region; kws...) = $pf(fftwfloat(x), kinds, region; kws...)
        $f{T<:Complex}(x::AbstractArray{T}, kinds, region=1:ndims(x)) = $f(fftwcomplex(x), kinds, region)
        $pf{T<:Complex}(x::AbstractArray{T}, kinds, region; kws...) = $pf(fftwcomplex(x), kinds, region; kws...)
    end
end

function plan_r2r{T<:fftwNumber,N}(X::StridedArray{T,N}, kinds, region;
                                   flags::Integer=ESTIMATE,
                                   timelimit::Real=NO_TIMELIMIT)
    r2rFFTWPlan{T,ANY,false,N}(X, fakesimilar(flags, X, T), region, kinds,
                               flags, timelimit)
end

function plan_r2r!{T<:fftwNumber,N}(X::StridedArray{T,N}, kinds, region;
                                    flags::Integer=ESTIMATE,
                                    timelimit::Real=NO_TIMELIMIT)
    r2rFFTWPlan{T,ANY,true,N}(X, X, region, kinds, flags, timelimit)
end

"""
    r2r(A, kind [, dims])

Performs a multidimensional real-input/real-output (r2r) transform
of type `kind` of the array `A`, as defined in the FFTW manual.
`kind` specifies either a discrete cosine transform of various types
(`FFTW.REDFT00`, `FFTW.REDFT01`, `FFTW.REDFT10`, or
`FFTW.REDFT11`), a discrete sine transform of various types
(`FFTW.RODFT00`, `FFTW.RODFT01`, `FFTW.RODFT10`, or
`FFTW.RODFT11`), a real-input DFT with halfcomplex-format output
(`FFTW.R2HC` and its inverse `FFTW.HC2R`), or a discrete
Hartley transform (`FFTW.DHT`).  The `kind` argument may be
an array or tuple in order to specify different transform types
along the different dimensions of `A`; `kind[end]` is used
for any unspecified dimensions.  See the FFTW manual for precise
definitions of these transform types, at http://www.fftw.org/doc.

The optional `dims` argument specifies an iterable subset of
dimensions (e.g. an integer, range, tuple, or array) to transform
along. `kind[i]` is then the transform type for `dims[i]`,
with `kind[end]` being used for `i > length(kind)`.

See also [`plan_r2r`](@ref) to pre-plan optimized r2r transforms.
"""
FFTW.r2r

"""
    r2r!(A, kind [, dims])

Same as [`r2r`](@ref), but operates in-place on `A`, which must be
an array of real or complex floating-point numbers.
"""
FFTW.r2r!

"""
    plan_r2r!(A, kind [, dims [, flags [, timelimit]]])

Similar to [`plan_fft`](@ref), but corresponds to [`r2r!`](@ref).
"""
FFTW.plan_r2r!

"""
    plan_r2r(A, kind [, dims [, flags [, timelimit]]])

Pre-plan an optimized r2r transform, similar to [`plan_fft`](@ref)
except that the transforms (and the first three arguments)
correspond to [`r2r`](@ref) and [`r2r!`](@ref), respectively.
"""
FFTW.plan_r2r

# mapping from r2r kind to the corresponding inverse transform
const inv_kind = Dict{Int,Int}(R2HC => HC2R, HC2R => R2HC, DHT => DHT,
                               REDFT00 => REDFT00,
                               REDFT01 => REDFT10, REDFT10 => REDFT01,
                               REDFT11 => REDFT11,
                               RODFT00 => RODFT00,
                               RODFT01 => RODFT10, RODFT10 => RODFT01,
                               RODFT11 => RODFT11)

# r2r inverses are normalized to 1/N, where N is a "logical" size
# the transform with length n and kind k:
function logical_size(n::Integer, k::Integer)
    k <= DHT && return n
    k == REDFT00 && return 2(n-1)
    k == RODFT00 && return 2(n+1)
    return 2n
end

function plan_inv{T<:fftwNumber,K,inplace,N}(p::r2rFFTWPlan{T,K,inplace,N})
    X = Array{T}(p.sz)
    iK = fix_kinds(p.region, [inv_kind[k] for k in K])
    Y = inplace ? X : fakesimilar(p.flags, X, T)
    ScaledPlan(r2rFFTWPlan{T,ANY,inplace,N}(X, Y, p.region, iK,
                                            p.flags, NO_TIMELIMIT),
               normalization(real(T),
                             map(logical_size, [p.sz...][[p.region...]], iK),
                             1:length(iK)))
end

function A_mul_B!{T}(y::StridedArray{T}, p::r2rFFTWPlan{T}, x::StridedArray{T})
    assert_applicable(p, x, y)
    unsafe_execute!(p, x, y)
    return y
end

function *{T,K,N}(p::r2rFFTWPlan{T,K,false}, x::StridedArray{T,N})
    assert_applicable(p, x)
    y = Array{T}(p.osz)::Array{T,N}
    unsafe_execute!(p, x, y)
    return y
end

function *{T,K}(p::r2rFFTWPlan{T,K,true}, x::StridedArray{T})
    assert_applicable(p, x)
    unsafe_execute!(p, x, x)
    return x
end

include("dct.jl")

end # module
