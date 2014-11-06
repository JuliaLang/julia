module FFTW

export fft, bfft, ifft, rfft, brfft, irfft,
       plan_fft, plan_bfft, plan_ifft, plan_rfft, plan_brfft, plan_irfft,
       fft!, bfft!, ifft!, plan_fft!, plan_bfft!, plan_ifft!,
       r2r, r2r!, plan_r2r, plan_r2r!,
       export_wisdom, import_wisdom, import_system_wisdom, forget_wisdom,
       MEASURE, DESTROY_INPUT, UNALIGNED, CONSERVE_MEMORY, EXHAUSTIVE,
       PRESERVE_INPUT, PATIENT, ESTIMATE, WISDOM_ONLY, NO_TIMELIMIT,
       R2HC, HC2R, DHT, REDFT00, REDFT01, REDFT10, REDFT11,
       RODFT00, RODFT01, RODFT10, RODFT11,
       fftwNumber, fftwReal, fftwComplex

## FFT: Implement fft by calling fftw.

const libfftw = Base.libfftw_name
const libfftwf = Base.libfftwf_name

## Direction of FFT

const FORWARD = int32(-1)
const BACKWARD = int32(1)

## FFTW Flags from fftw3.h

const MEASURE         = uint32(0)
const DESTROY_INPUT   = uint32(1 << 0)
const UNALIGNED       = uint32(1 << 1)
const CONSERVE_MEMORY = uint32(1 << 2)
const EXHAUSTIVE      = uint32(1 << 3)   # NO_EXHAUSTIVE is default
const PRESERVE_INPUT  = uint32(1 << 4)   # cancels DESTROY_INPUT
const PATIENT         = uint32(1 << 5)   # IMPATIENT is default
const ESTIMATE        = uint32(1 << 6)
const WISDOM_ONLY     = uint32(1 << 21)

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

# FFTW floating-point types:

typealias fftwNumber Union(Float64,Float32,Complex128,Complex64)
typealias fftwReal Union(Float64,Float32)
typealias fftwComplex Union(Complex128,Complex64)
typealias fftwDouble Union(Float64,Complex128)
typealias fftwSingle Union(Float32,Complex64)
typealias fftwTypeDouble Union(Type{Float64},Type{Complex128})
typealias fftwTypeSingle Union(Type{Float32},Type{Complex64})

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
    f = ccall(:fopen, Ptr{Void}, (Ptr{UInt8},Ptr{UInt8}), fname, "w")
    systemerror("could not open wisdom file $fname for writing", f == C_NULL)
    ccall((:fftw_export_wisdom_to_file,libfftw), Void, (Ptr{Void},), f)
    ccall(:fputs, Int32, (Ptr{UInt8},Ptr{Void}), " "^256, f)
    ccall((:fftwf_export_wisdom_to_file,libfftwf), Void, (Ptr{Void},), f)
    ccall(:fclose, Void, (Ptr{Void},), f)
end

function import_wisdom(fname::AbstractString)
    f = ccall(:fopen, Ptr{Void}, (Ptr{UInt8},Ptr{UInt8}), fname, "r")
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

# Execute

execute(precision::fftwTypeDouble, plan) =
    ccall((:fftw_execute,libfftw), Void, (Ptr{Void},), plan)

execute(precision::fftwTypeSingle, plan) =
    ccall((:fftwf_execute,libfftwf), Void, (Ptr{Void},), plan)

execute(plan, X::StridedArray{Complex128}, Y::StridedArray{Complex128}) =
    ccall((:fftw_execute_dft,libfftw), Void,
          (Ptr{Void},Ptr{Complex128},Ptr{Complex128}), plan, X, Y)

execute(plan, X::StridedArray{Complex64}, Y::StridedArray{Complex64}) =
    ccall((:fftwf_execute_dft,libfftwf), Void,
          (Ptr{Void},Ptr{Complex64},Ptr{Complex64}), plan, X, Y)

execute(plan, X::StridedArray{Float64}, Y::StridedArray{Complex128}) =
    ccall((:fftw_execute_dft_r2c,libfftw), Void,
          (Ptr{Void},Ptr{Float64},Ptr{Complex128}), plan, X, Y)

execute(plan, X::StridedArray{Float32}, Y::StridedArray{Complex64}) =
    ccall((:fftwf_execute_dft_r2c,libfftwf), Void,
          (Ptr{Void},Ptr{Float32},Ptr{Complex64}), plan, X, Y)

execute(plan, X::StridedArray{Complex128}, Y::StridedArray{Float64}) =
    ccall((:fftw_execute_dft_c2r,libfftw), Void,
          (Ptr{Void},Ptr{Complex128},Ptr{Float64}), plan, X, Y)

execute(plan, X::StridedArray{Complex64}, Y::StridedArray{Float32}) =
    ccall((:fftwf_execute_dft_c2r,libfftwf), Void,
          (Ptr{Void},Ptr{Complex64},Ptr{Float32}), plan, X, Y)

execute_r2r{T<:fftwDouble}(plan, X::StridedArray{T}, Y::StridedArray{T}) =
    ccall((:fftw_execute_r2r,libfftw), Void,
          (Ptr{Void},Ptr{T},Ptr{T}), plan, X, Y)

execute_r2r{T<:fftwSingle}(plan, X::StridedArray{T}, Y::StridedArray{T}) =
    ccall((:fftwf_execute_r2r,libfftwf), Void,
          (Ptr{Void},Ptr{T},Ptr{T}), plan, X, Y)

execute{T<:fftwReal}(plan, X::StridedArray{T}, Y::StridedArray{T}) =
    execute_r2r(plan, X, Y)

# Destroy plan

destroy_plan(precision::fftwTypeDouble, plan) =
    ccall((:fftw_destroy_plan,libfftw), Void, (Ptr{Void},), plan)

destroy_plan(precision::fftwTypeSingle, plan) =
    ccall((:fftwf_destroy_plan,libfftwf), Void, (Ptr{Void},), plan)

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
#   alignment of a given pointer modulo whatever FFTW needs.

if Base.libfftw_name == "libmkl_rt"

    alignment_of{T<:fftwDouble}(A::StridedArray{T}) =
        convert(Int32, convert(Int64, pointer(A)) % 16)

    alignment_of{T<:fftwSingle}(A::StridedArray{T}) =
        convert(Int32, convert(Int64, pointer(A)) % 16)

else

    alignment_of{T<:fftwDouble}(A::StridedArray{T}) =
        ccall((:fftw_alignment_of, libfftw), Int32, (Ptr{Void},), A)

    alignment_of{T<:fftwSingle}(A::StridedArray{T}) =
        ccall((:fftwf_alignment_of, libfftwf), Int32, (Ptr{Void},), A)

end

# Plan (low-level)

# low-level storage of the FFTW plan, along with the information
# needed to determine whether it is applicable.   We need to put
# this into a type to support a finalizer on the fftw_plan.
type Plan{T<:fftwNumber}
    plan::Ptr{Void}
    sz::Dims # size of array on which plan operates (Int tuple)
    istride::Dims # strides of input
    ialign::Int32 # alignment mod 16 of input
    function Plan(plan::Ptr{Void}, sz::Dims, istride::Dims, ialign::Int32)
        p = new(plan,sz,istride,ialign)
        finalizer(p, p -> destroy_plan(T, p.plan))
        return p
    end
end
Plan{T<:fftwNumber}(plan::Ptr{Void}, X::StridedArray{T}) = Plan{T}(plan, size(X), strides(X), alignment_of(X))

# Check whether a Plan is applicable to a given input array, and
# throw an informative error if not:
function assert_applicable{T<:fftwNumber}(p::Plan{T}, X::StridedArray{T})
    if size(X) != p.sz
        throw(ArgumentError("FFTW plan applied to wrong-size array"))
    elseif strides(X) != p.istride
        throw(ArgumentError("FFTW plan applied to wrong-strides array"))
    elseif alignment_of(X) != p.ialign
        throw(ArgumentError("FFTW plan applied to array with wrong memory alignment"))
    end
end

# NOTE ON GC (garbage collection):
#    The Plan has a finalizer so that gc will destroy the plan,
#    which is necessary for gc to work with plan_fft.  However,
#    even when we are creating a single-use Plan [e.g. for fftn(x)],
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
    oreg = [1:ndims(X)]
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
            if length(kinds) == 0
                throw(ArgumentError("must supply a transform kind"))
            end
            k = Array(Int32, length(region))
            k[1:length(kinds)] = [kinds...]
            k[length(kinds)+1:end] = kinds[end]
            kinds = k
        end
    else
        kinds = int32([kinds...])
    end
    for i = 1:length(kinds)
        if kinds[i] < 0 || kinds[i] > 10
            throw(ArgumentError("invalid transform kind"))
        end
    end
    return kinds
end

# low-level Plan creation (for internal use in FFTW module)

for (Tr,Tc,fftw,lib) in ((:Float64,:Complex128,"fftw",libfftw),
                         (:Float32,:Complex64,"fftwf",libfftwf))

    @eval function Plan(X::StridedArray{$Tc}, Y::StridedArray{$Tc},
                        region, direction::Integer,
                        flags::Unsigned, timelimit::Real)
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(X)...], region)
        plan = ccall(($(string(fftw,"_plan_guru64_dft")),$lib),
                     Ptr{Void},
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tc}, Ptr{$Tc}, Int32, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, direction, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        return Plan(plan, X)
    end

    @eval function Plan(X::StridedArray{$Tr}, Y::StridedArray{$Tc},
                        region, flags::Unsigned, timelimit::Real)
        region = circshift([region...],-1) # FFTW halves last dim
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(X)...], region)
        plan = ccall(($(string(fftw,"_plan_guru64_dft_r2c")),$lib),
                     Ptr{Void},
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tr}, Ptr{$Tc}, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        return Plan(plan, X)
    end

    @eval function Plan(X::StridedArray{$Tc}, Y::StridedArray{$Tr},
                        region, flags::Unsigned, timelimit::Real)
        region = circshift([region...],-1) # FFTW halves last dim
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(Y)...], region)
        plan = ccall(($(string(fftw,"_plan_guru64_dft_c2r")),$lib),
                     Ptr{Void},
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tc}, Ptr{$Tr}, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        return Plan(plan, X)
    end

    @eval function Plan_r2r(X::StridedArray{$Tr}, Y::StridedArray{$Tr},
                            region, kinds,
                            flags::Unsigned, timelimit::Real)
        kinds = fix_kinds(region, kinds)
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(X)...], region)
        plan = ccall(($(string(fftw,"_plan_guru64_r2r")),$lib),
                     Ptr{Void},
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tr}, Ptr{$Tr}, Ptr{Int32}, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, kinds, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        return Plan(plan, X)
    end

    # support r2r transforms of complex = transforms of real & imag parts
    @eval function Plan_r2r(X::StridedArray{$Tc}, Y::StridedArray{$Tc},
                            region, kinds,
                            flags::Unsigned, timelimit::Real)
        kinds = fix_kinds(region, kinds)
        set_timelimit($Tr, timelimit)
        dims, howmany = dims_howmany(X, Y, [size(X)...], region)
        dims[2:3, 1:size(dims,2)] *= 2
        howmany[2:3, 1:size(howmany,2)] *= 2
        howmany = [howmany [2,1,1]] # append loop over real/imag parts
        plan = ccall(($(string(fftw,"_plan_guru64_r2r")),$lib),
                     Ptr{Void},
                     (Int32, Ptr{Int}, Int32, Ptr{Int},
                      Ptr{$Tc}, Ptr{$Tc}, Ptr{Int32}, UInt32),
                     size(dims,2), dims, size(howmany,2), howmany,
                     X, Y, kinds, flags)
        set_timelimit($Tr, NO_TIMELIMIT)
        if plan == C_NULL
            error("FFTW could not create plan") # shouldn't normally happen
        end
        return Plan(plan, X)
    end

end

# Convert arrays of numeric types to FFTW-supported packed complex-float types
# (FIXME: is there a way to use the Julia promotion rules more cleverly here?)
complexfloat{T<:fftwComplex}(X::StridedArray{T}) = X
complexfloat{T<:fftwReal}(X::StridedArray{T}) = complex(X)
complexfloat{T<:Real}(X::StridedArray{T}) = complex128(X)
complexfloat{T<:Complex}(X::StridedArray{T}) = complex128(X)

# In the Julia interface, a "plan" is just a function that executes
# an efficient FFT of fixed size/strides/alignment.  For each FFT function
# (fft, bfft, ifft, rfft, ...), we have at least two interfaces:
#
#   fft(x [, region]) - FFT of x, creating and destroying a plan,
#                       optionally acting only on a subset of the dimensions
#   p = plan_fft(x, [, region [, flags [, timelimit]]])
#       -- returns a function p(x) that performs efficient FFTs
#          of a given size (on given dimensions) with variants
#          to specify the FFTW planner flags (default is ESTIMATE) and
#          timelimit (default: NO_TIMELIMIT).
#
# along with in-place variants fft! and plan_fft! if feasible.

for (f,direction) in ((:fft,:FORWARD), (:bfft,:BACKWARD))
    f! = symbol(string(f,"!"))
    plan_f = symbol(string("plan_",f))
    plan_f! = symbol(string("plan_",f,"!"))
    @eval begin
        function $f{T<:fftwComplex}(X::StridedArray{T}, region)
            Y = similar(X, T)
            p = Plan(X, Y, region, $direction, ESTIMATE, NO_TIMELIMIT)
            execute(T, p.plan)
            # do not destroy_plan ... see above note on gc
            return Y
        end

        # in-place version
        function $f!{T<:fftwComplex}(X::StridedArray{T},region)
            p = Plan(X, X, region, $direction, ESTIMATE, NO_TIMELIMIT)
            execute(T, p.plan)
            # do not destroy_plan ... see above note on gc
            return X
        end

        function $f{T<:Number}(X::StridedArray{T}, region)
            Y = complexfloat(X) # in-place transform
            return $f!(Y, region)
        end

        function $plan_f{T<:fftwComplex}(X::StridedArray{T},
                                                         region,
                                                         flags::Unsigned,
                                                         tlim::Real)
            Y = similar(X, T)
            p = Plan(X, Y, region, $direction, flags, tlim)
            return Z::StridedArray{T} -> begin
                assert_applicable(p, Z)
                W = similar(Z, T)
                execute(p.plan, Z, W)
                return W
            end
        end

        function $plan_f{T<:Number}(X::StridedArray{T}, region,
                                    flags::Unsigned, tlim::Real)
            Y = complexfloat(X) # in-place transform
            p = Plan(Y, Y, region, $direction, flags, tlim)
            return Z::StridedArray{T} -> begin
                W = complexfloat(Z) # in-place transform
                assert_applicable(p, W)
                execute(p.plan, W, W)
                return W
            end
        end

        function $plan_f!{T<:fftwComplex}(X::StridedArray{T},
                                                          region,
                                                          flags::Unsigned,
                                                          tlim::Real)
            p = Plan(X, X, region, $direction, flags, tlim)
            return Z::StridedArray{T} -> begin
                assert_applicable(p, Z)
                execute(p.plan, Z, Z)
                return Z
            end
        end

        $f(X::StridedArray) = $f(X, 1:ndims(X))
        $f!(X::StridedArray) = $f!(X, 1:ndims(X))
    end
    for pf in (plan_f, plan_f!)
        @eval begin
            $pf{T<:Number}(X::StridedArray{T}, region, flags::Unsigned) =
                $pf(X, region, flags, NO_TIMELIMIT)
            $pf{T<:Number}(X::StridedArray{T}, region) =
                $pf(X, region, ESTIMATE, NO_TIMELIMIT)
            $pf{T<:Number}(X::StridedArray{T}) =
                $pf(X, 1:ndims(X), ESTIMATE, NO_TIMELIMIT)
        end
    end
end

# Normalization for ifft

normalization(X::StridedArray, region) = 1 / prod([size(X)...][[region...]])
normalization(X::StridedArray) = 1 / length(X)

# Normalized ifft inverse transforms:

for (f,fb) in ((:ifft,:bfft), (:ifft!,:bfft!))
    pf = symbol(string("plan_", f))
    pfb = symbol(string("plan_", fb))
    @eval begin
        $f(X, region) = scale!($fb(X, region), normalization(X, region))
        $f(X) = scale!($fb(X), normalization(X))

        function $pf(X, region, flags, tlim)
            nrm = normalization(X, region)
            p = $pfb(X, region, flags, tlim)
            return Z -> scale!(p(Z), nrm)
        end
        $pf(X, region, flags) = $pf(X, region, flags, NO_TIMELIMIT)
        $pf(X, region) = $pf(X, region, ESTIMATE, NO_TIMELIMIT)
        function $pf(X)
            nrm = normalization(X)
            p = $pfb(X)
            return Z -> scale!(p(Z), nrm)
        end
    end
end

# rfft/brfft and planned variants.  No in-place version for now.

for (Tr,Tc) in ((:Float32,:Complex64),(:Float64,:Complex128))
    @eval begin
        function rfft(X::StridedArray{$Tr}, region)
            d1 = region[1]
            osize = [size(X)...]
            osize[d1] = osize[d1]>>1 + 1
            Y = Array($Tc, osize...)
            p = Plan(X, Y, region, ESTIMATE, NO_TIMELIMIT)
            execute($Tr, p.plan)
            # do not destroy_plan ... see above note on gc
            return Y
        end

        function rfft{T<:Real}(X::StridedArray{T}, region)
            Xr = float(X)
            return rfft(Xr, region)
        end

        function plan_rfft(X::StridedArray{$Tr}, region,
                           flags::Unsigned, tlim::Real)
            d1 = region[1]
            osize = [size(X)...]
            osize[d1] = osize[d1]>>1 + 1
            Y = Array($Tc, osize...)
            p = Plan(X, Y, region, flags, tlim)
            return Z::StridedArray{$Tr} -> begin
                assert_applicable(p, Z)
                W = Array($Tc, osize...)
                execute(p.plan, Z, W)
                return W
            end
        end

        # FFTW currently doesn't support PRESERVE_INPUT for
        # multidimensional out-of-place c2r transforms, so
        # we have to handle 1d and >1d cases separately.  Ugh.

        function brfft(X::StridedArray{$Tc}, d::Integer, region::Integer)
            osize = [size(X)...]
            @assert osize[region] == d>>1 + 1
            osize[region] = d
            Y = Array($Tr, osize...)
            p = Plan(X, Y, region, ESTIMATE | PRESERVE_INPUT, NO_TIMELIMIT)
            execute($Tr, p.plan)
            # do not destroy_plan ... see above note on gc
            return Y
        end

        # variant that destroys input X
        function brfftd(X::StridedArray{$Tc}, d::Integer, region)
            d1 = region[1]
            osize = [size(X)...]
            @assert osize[d1] == d>>1 + 1
            osize[d1] = d
            Y = Array($Tr, osize...)
            p = Plan(X, Y, region, ESTIMATE, NO_TIMELIMIT)
            execute($Tr, p.plan)
            # do not destroy_plan ... see above note on gc
            return Y
        end

        function brfft(X::StridedArray{$Tc}, d::Integer, region)
            if length(region) == 1
                return brfft(X, d, convert(Int, region[1]))
            end
            X = copy(X) # TODO: work in-place instead?
            return brfftd(X, d, region)
        end

        function brfft{T<:Number}(X::StridedArray{T}, d::Integer, region)
            Xc = complexfloat(X)
            return brfftd(Xc, d, region)
        end

        function plan_brfft(X::StridedArray{$Tc}, d::Integer, region::Integer,
                            flags::Unsigned, tlim::Real)
            osize = [size(X)...]
            @assert osize[region] == d>>1 + 1
            osize[region] = d
            Y = Array($Tr, osize...)
            p = Plan(X, Y, region, flags | PRESERVE_INPUT, tlim)
            return Z::StridedArray{$Tc} -> begin
                assert_applicable(p, Z)
                W = Array($Tr, osize...)
                execute(p.plan, Z, W)
                return W
            end
        end

        function plan_brfft(X::StridedArray{$Tc}, d::Integer, region,
                            flags::Unsigned, tlim::Real)
            if length(region) == 1
                return plan_brfft(X, d, convert(Int, region[1]), flags, tlim)
            end
            d1 = region[1]
            osize = [size(X)...]
            @assert osize[d1] == d>>1 + 1
            osize[d1] = d
            Y = Array($Tr, osize...)
            X = copy(X)
            p = Plan(X, Y, region, flags, tlim)
            return Z::StridedArray{$Tc} -> begin
                assert_applicable(p, Z)
                Z = copy(Z)
                W = Array($Tr, osize...)
                execute(p.plan, Z, W)
                return W
            end
        end

        rfft(X::StridedArray) = rfft(X, 1:ndims(X))
        brfft(X::StridedArray,d) = brfft(X, d, 1:ndims(X))

        plan_rfft(X::StridedArray, region, flags) =
            plan_rfft(X, region, flags, NO_TIMELIMIT)
        plan_rfft(X::StridedArray, region) =
            plan_rfft(X, region, ESTIMATE, NO_TIMELIMIT)
        plan_rfft(X::StridedArray) =
            plan_rfft(X, 1:ndims(X), ESTIMATE, NO_TIMELIMIT)

        plan_brfft(X::StridedArray, d,  region, flags) =
            plan_brfft(X, d, region, flags, NO_TIMELIMIT)
        plan_brfft(X::StridedArray, d,  region) =
            plan_brfft(X, d, region, ESTIMATE, NO_TIMELIMIT)
        plan_brfft(X::StridedArray, d) =
            plan_brfft(X, d, 1:ndims(X), ESTIMATE, NO_TIMELIMIT)
    end
end

# Normalized rfft inverse transforms:

function irfft(X, d, region)
    Y = brfft(X, d, region)
    return scale!(Y, normalization(Y, region))
end

function irfft(X, d)
    Y = brfft(X, d)
    return scale!(Y, normalization(Y))
end

function plan_irfft(X::StridedArray, d::Integer, region, flags, tlim)
    p = plan_brfft(X, d, region, flags, tlim)
    d1 = region[1]
    osize = [size(X)...]
    osize[d1] = d
    nrm = 1 / prod(osize[[region...]])
    return Z -> scale!(p(Z), nrm)
end

plan_irfft(X, d, region, flags) = plan_irfft(X, d, region, flags, NO_TIMELIMIT)
plan_irfft(X, d, region) = plan_irfft(X, d, region, ESTIMATE, NO_TIMELIMIT)
plan_irfft(X, d) = plan_irfft(X, d, 1:ndims(X), ESTIMATE, NO_TIMELIMIT)

# A DFT is unambiguously defined as just the identity operation for scalars
fft(x::Number) = x
ifft(x::Number) = x
bfft(x::Number) = x
rfft(x::Real) = x
irfft(x::Number, d::Integer) = d == 1 ? real(x) : throw(BoundsError())
brfft(x::Number, d::Integer) = d == 1 ? real(x) : throw(BoundsError())
fft(x::Number, dims) = length(dims) == 0 || dims[1] == 1 ? x : throw(BoundsError())
ifft(x::Number, dims) = length(dims) == 0 || dims[1] == 1 ? x : throw(BoundsError())
bfft(x::Number, dims) = length(dims) == 0 || dims[1] == 1 ? x : throw(BoundsError())
fft(x::Number, dims) = length(dims) == 0 || dims[1] == 1 ? x : throw(BoundsError())
rfft(x::Real, dims) = dims[1] == 1 ? x : throw(BoundsError())
irfft(x::Number, d::Integer, dims) = d == 1 && dims[1] == 1 ? real(x) : throw(BoundsError())
brfft(x::Number, d::Integer, dims) = d == 1 && dims[1] == 1 ? real(x) : throw(BoundsError())

plan_fft(x::Number) = x -> x
plan_ifft(x::Number) = x -> x
plan_bfft(x::Number) = x -> x
plan_rfft(x::Real) = x -> x
plan_irfft(x::Number, d::Integer) = (irfft(x,d); x -> real(x))
plan_brfft(x::Number, d::Integer) = (brfft(x,d); x -> real(x))

plan_fft(x::Number, dims) = (fft(x,dims); x -> x)
plan_ifft(x::Number, dims) = (ifft(x,dims); x -> x)
plan_bfft(x::Number, dims) = (bfft(x,dims); x -> x)
plan_rfft(x::Real, dims) = (rfft(x,dims); x -> x)
plan_irfft(x::Number, d::Integer, dims) = (irfft(x,d,dims); x -> real(x))
plan_brfft(x::Number, d::Integer, dims) = (brfft(x,d,dims); x -> real(x))

plan_fft(x::Number, dims, flags) = plan_fft(x, dims)
plan_ifft(x::Number, dims, flags) = plan_ifft(x, dims)
plan_bfft(x::Number, dims, flags) = plan_bfft(x, dims)
plan_rfft(x::Real, dims, flags) = plan_rfft(x, dims)
plan_irfft(x::Number, d::Integer, dims, flags) = plan_irfft(x, d, dims)
plan_brfft(x::Number, d::Integer, dims, flags) = plan_brfft(x, d, dims)

plan_fft(x::Number, dims, flags, tlim) = plan_fft(x, dims)
plan_ifft(x::Number, dims, flags, tlim) = plan_ifft(x, dims)
plan_bfft(x::Number, dims, flags, tlim) = plan_bfft(x, dims)
plan_rfft(x::Real, dims, flags, tlim) = plan_rfft(x, dims)
plan_irfft(x::Number, d::Integer, dims, flags, tlim) = plan_irfft(x, d, dims)
plan_brfft(x::Number, d::Integer, dims, flags, tlim) = plan_brfft(x, d, dims)

# FFTW r2r transforms (low-level interface)

function r2r{T<:fftwNumber}(X::StridedArray{T}, kinds, region)
    Y = similar(X, T)
    p = Plan_r2r(X, Y, region, kinds, ESTIMATE, NO_TIMELIMIT)
    execute(T, p.plan)
    # do not destroy_plan ... see above note on gc
    return Y
end

function r2r!{T<:fftwNumber}(X::StridedArray{T}, kinds, region)
    p = Plan_r2r(X, X, region, kinds, ESTIMATE, NO_TIMELIMIT)
    execute(T, p.plan)
    # do not destroy_plan ... see above note on gc
    return X
end

r2r{T<:Real}(X::StridedArray{T}, kinds, region) = r2r!(float(X), kinds, region)
r2r{T<:Complex}(X::StridedArray{T}, kinds, region) = r2r!(complexfloat(X), kinds, region)

for f in (:r2r, :r2r!)
    @eval $f(X, kinds) = $f(X, kinds, 1:ndims(X))
end

function plan_r2r{T<:fftwNumber}(
    X::StridedArray{T}, kinds, region, flags::Unsigned, tlim::Real)
    Y = similar(X, T)
    p = Plan_r2r(X, Y, region, kinds, flags, tlim)
    return Z::StridedArray{T} -> begin
        assert_applicable(p, Z)
        W = similar(Z, T)
        execute_r2r(p.plan, Z, W)
        return W
    end
end

function plan_r2r{T<:Number}(X::StridedArray{T}, kinds, region,
                             flags::Unsigned, tlim::Real)
    Y = T<:Complex ? complexfloat(X) : float(X)
    p = Plan_r2r(Y, Y, region, kinds, flags, tlim)
    return Z::StridedArray{T} -> begin
        W = T<:Complex ? complexfloat(Z) : float(Z)
        execute_r2r(p.plan, W, W)
        return W
    end
end

function plan_r2r!{T<:fftwNumber}(
    X::StridedArray{T}, kinds,region,flags::Unsigned, tlim::Real)
    p = Plan_r2r(X, X, region, kinds, flags, tlim)
    return Z::StridedArray{T} -> begin
        assert_applicable(p, Z)
        execute_r2r(p.plan, Z, Z)
        return Z
    end
end

for f in (:plan_r2r, :plan_r2r!)
    @eval begin
        $f(X, kinds, region, flags) = $f(X, kinds, region, flags, NO_TIMELIMIT)
        $f(X, kinds, region) = $f(X, kinds, region, ESTIMATE, NO_TIMELIMIT)
        $f(X, kinds) = $f(X, kinds, 1:ndims(X), ESTIMATE, NO_TIMELIMIT)
    end
end

end # module
