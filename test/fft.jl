# This file is a part of Julia. License is MIT: http://julialang.org/license

# fft
a = rand(8) + im*rand(8)
@test norm(ifft(fft(a)) - a) < 1e-8
@test norm(ifft(fft(a,1),1) - a) < 1e-8
@test norm(ifft(fft(a,[1]),[1]) - a) < 1e-8
@test norm(ifft(fft(a,(1,)),(1,)) - a) < 1e-8
a = rand(-10:10, 8) + im*rand(-10:10, 8)
@test norm(ifft(fft(a)) - a) < 1e-8

m4 = [16.    2     3    13;
    5    11    10     8;
    9     7     6    12;
    4    14    15     1]

true_fft_m4 = [
    34.            34.            34.            34.;
     7. - 1.0im  -5. + 3.0im  -3. + 5.0im   1. - 7.0im;
    16.           -16.           -16.            16.;
     7. + 1.0im  -5. - 3.0im  -3. - 5.0im   1. + 7.0im ]

true_fftn_m4 = [
 136.        0          0         0 ;
   0.       20          8 + 8im   0 - 12im ;
   0.       32 + 32im   0        32 - 32im ;
   0.        0 + 12im   8 - 8im  20 ]

true_fftd2_m4 = [
   34.   13 + 11im    4   13 - 11im ;
   34.   -5 -  3im   -4   -5 +  3im ;
   34.    3 +  5im   -4    3 -  5im ;
   34.  -11 - 13im    4  -11 + 13im ]

b = rand(17,14)
b[3:6,9:12] = m4
sm4 = view(b,3:6,9:12)

m3d = map(Float32,copy(reshape(1:5*3*2, 5, 3, 2)))
true_fftd3_m3d = Array{Float32}(5, 3, 2)
true_fftd3_m3d[:,:,1] = 17:2:45
true_fftd3_m3d[:,:,2] = -15

# use invoke to force usage of CTPlan versions even if FFTW is present
for A in (Array,SubArray)
    for f in (:fft,:ifft,:plan_fft,:plan_ifft)
        f_ = Symbol(f, "_")
        @eval begin
            $f_{T,N}(x::$A{T,N}) = invoke($f, Tuple{AbstractArray{T,N}}, x)
            $f_{T,N,R}(x::$A{T,N},r::R) = invoke($f,Tuple{AbstractArray{T,N},R},x,r)
        end
    end
end

for (f,fi,pf,pfi) in ((fft,ifft,plan_fft,plan_ifft),
                      (fft_,ifft_,plan_fft_,plan_ifft_))
    pm4 = pf(m4,1)

    fft_m4 = f(m4,1)
    fftd2_m4 = f(m4,2)
    ifft_fft_m4 = fi(f(m4,1),1)
    fftn_m4 = f(m4)
    ifftn_fftn_m4 = fi(f(m4))

    fft!_m4 = complex(m4); fft!(fft!_m4,1)
    fft!d2_m4 = complex(m4); fft!(fft!d2_m4,2)
    ifft!_fft_m4 = f(m4,1); ifft!(ifft!_fft_m4,1)
    fft!n_m4 = complex(m4); fft!(fft!n_m4)
    ifft!n_fftn_m4 = f(m4); ifft!(ifft!n_fftn_m4)

    pfft_m4 = pf(m4,1)*m4
    pfftd2_m4 = pf(m4,2)*m4
    pifft_fft_m4 = pfi(fft_m4,1)*fft_m4
    pfftn_m4 = pf(m4)*m4
    pifftn_fftn_m4 = pfi(fftn_m4)*fftn_m4

    pfft!_m4 = complex(m4); plan_fft!(pfft!_m4,1)*pfft!_m4
    pfft!d2_m4 = complex(m4); plan_fft!(pfft!d2_m4,2)*pfft!d2_m4
    pifft!_fft_m4 = f(m4,1); plan_ifft!(pifft!_fft_m4,1)*pifft!_fft_m4
    pfft!n_m4 = complex(m4); plan_fft!(pfft!n_m4)*pfft!n_m4
    pifft!n_fftn_m4 = f(m4); plan_ifft!(pifft!n_fftn_m4)*pifft!n_fftn_m4

    sfftn_m4 = f(sm4)
    psfftn_m4 = pf(sm4)*sm4
    sfft!n_b = map(Complex128,b)
    sfft!n_m4 = view(sfft!n_b,3:6,9:12); fft!(sfft!n_m4)
    psfft!n_b = map(Complex128,b)
    psfft!n_m4 = view(psfft!n_b,3:6,9:12); plan_fft!(psfft!n_m4)*psfft!n_m4

    for i = 1:length(m4)
        @test fft_m4[i] ≈ true_fft_m4[i]
        @test fftd2_m4[i] ≈ true_fftd2_m4[i]
        @test ifft_fft_m4[i] ≈ m4[i]
        @test fftn_m4[i] ≈ true_fftn_m4[i]
        @test ifftn_fftn_m4[i] ≈ m4[i]

        @test fft!_m4[i] ≈ true_fft_m4[i]
        @test fft!d2_m4[i] ≈ true_fftd2_m4[i]
        @test ifft!_fft_m4[i] ≈ m4[i]
        @test fft!n_m4[i] ≈ true_fftn_m4[i]
        @test ifft!n_fftn_m4[i] ≈ m4[i]

        @test pfft_m4[i] ≈ true_fft_m4[i]
        @test pfftd2_m4[i] ≈ true_fftd2_m4[i]
        @test pifft_fft_m4[i] ≈ m4[i]
        @test pfftn_m4[i] ≈ true_fftn_m4[i]
        @test pifftn_fftn_m4[i] ≈ m4[i]

        @test pfft!_m4[i] ≈ true_fft_m4[i]
        @test pfft!d2_m4[i] ≈ true_fftd2_m4[i]
        @test pifft!_fft_m4[i] ≈ m4[i]
        @test pfft!n_m4[i] ≈ true_fftn_m4[i]
        @test pifft!n_fftn_m4[i] ≈ m4[i]

        @test sfftn_m4[i] ≈ true_fftn_m4[i]
        @test sfft!n_m4[i] ≈ true_fftn_m4[i]
        @test psfftn_m4[i] ≈ true_fftn_m4[i]
        @test psfft!n_m4[i] ≈ true_fftn_m4[i]
    end

    ifft!(sfft!n_m4)
    plan_ifft!(psfft!n_m4)*psfft!n_m4
    @test norm(sfft!n_m4 - m4) < 1e-8
    @test norm(psfft!n_m4 - m4) < 1e-8

    # The following capabilities are FFTW only.
    # They are not available in MKL, and hence do not test them.
    if Base.fftw_vendor() != :mkl
        ifft3_fft3_m3d = fi(f(m3d))

        fftd3_m3d = f(m3d,3)
        ifftd3_fftd3_m3d = fi(fftd3_m3d,3)

        fft!d3_m3d = complex(m3d); fft!(fft!d3_m3d,3)
        ifft!d3_fftd3_m3d = copy(fft!d3_m3d); ifft!(ifft!d3_fftd3_m3d,3)

        pfftd3_m3d = pf(m3d,3)*m3d
        pifftd3_fftd3_m3d = pfi(fftd3_m3d,3)*fftd3_m3d

        pfft!d3_m3d = complex(m3d); plan_fft!(pfft!d3_m3d,3)*pfft!d3_m3d
        pifft!d3_fftd3_m3d = copy(fft!d3_m3d); plan_ifft!(pifft!d3_fftd3_m3d,3)*pifft!d3_fftd3_m3d

        @test isa(fftd3_m3d, Array{Complex64,3})
        @test isa(ifftd3_fftd3_m3d, Array{Complex64,3})
        @test isa(fft!d3_m3d, Array{Complex64,3})
        @test isa(ifft!d3_fftd3_m3d, Array{Complex64,3})
        @test isa(pfftd3_m3d, Array{Complex64,3})
        @test isa(pifftd3_fftd3_m3d, Array{Complex64,3})
        @test isa(pfft!d3_m3d, Array{Complex64,3})
        @test isa(pifft!d3_fftd3_m3d, Array{Complex64,3})

        for i = 1:length(m3d)
            @test fftd3_m3d[i] ≈ true_fftd3_m3d[i]
            @test ifftd3_fftd3_m3d[i] ≈ m3d[i]
            @test ifft3_fft3_m3d[i] ≈ m3d[i]

            @test fft!d3_m3d[i] ≈ true_fftd3_m3d[i]
            @test ifft!d3_fftd3_m3d[i] ≈ m3d[i]

            @test pfftd3_m3d[i] ≈ true_fftd3_m3d[i]
            @test pifftd3_fftd3_m3d[i] ≈ m3d[i]
            @test pfft!d3_m3d[i] ≈ true_fftd3_m3d[i]
            @test pifft!d3_fftd3_m3d[i] ≈ m3d[i]
        end
    end  # if fftw_vendor() != :mkl

    # rfft/rfftn

    rfft_m4 = rfft(m4,1)
    rfftd2_m4 = rfft(m4,2)
    rfftn_m4 = rfft(m4)

    prfft_m4 = plan_rfft(m4,1)*m4
    prfftd2_m4 = plan_rfft(m4,2)*m4
    prfftn_m4 = plan_rfft(m4)*m4

    srfftn_m4 = rfft(sm4)
    psrfftn_m4 = plan_rfft(sm4)*sm4

    for i = 1:3, j = 1:4
        @test rfft_m4[i,j] ≈ true_fft_m4[i,j]
        @test rfftd2_m4[j,i] ≈ true_fftd2_m4[j,i]
        @test rfftn_m4[i,j] ≈ true_fftn_m4[i,j]

        @test prfft_m4[i,j] ≈ true_fft_m4[i,j]
        @test prfftd2_m4[j,i] ≈ true_fftd2_m4[j,i]
        @test prfftn_m4[i,j] ≈ true_fftn_m4[i,j]

        @test srfftn_m4[i,j] ≈ true_fftn_m4[i,j]
        @test psrfftn_m4[i,j] ≈ true_fftn_m4[i,j]
    end

    irfft_rfft_m4 = irfft(rfft_m4,size(m4,1),1)
    irfft_rfftd2_m4 = irfft(rfftd2_m4,size(m4,2),2)
    irfftn_rfftn_m4 = irfft(rfftn_m4,size(m4,1))

    pirfft_rfft_m4 = plan_irfft(rfft_m4,size(m4,1),1)*rfft_m4
    pirfft_rfftd2_m4 = plan_irfft(rfftd2_m4,size(m4,2),2)*rfftd2_m4
    pirfftn_rfftn_m4 = plan_irfft(rfftn_m4,size(m4,1))*rfftn_m4

    for i = 1:length(m4)
        @test irfft_rfft_m4[i] ≈ m4[i]
        @test irfft_rfftd2_m4[i] ≈ m4[i]
        @test irfftn_rfftn_m4[i] ≈ m4[i]

        @test pirfft_rfft_m4[i] ≈ m4[i]
        @test pirfft_rfftd2_m4[i] ≈ m4[i]
        @test pirfftn_rfftn_m4[i] ≈ m4[i]
    end

    if Base.fftw_vendor() != :mkl
        rfftn_m3d = rfft(m3d)
        rfftd3_m3d = rfft(m3d,3)
        @test size(rfftd3_m3d) == size(fftd3_m3d)
        irfft_rfftd3_m3d = irfft(rfftd3_m3d,size(m3d,3),3)
        irfftn_rfftn_m3d = irfft(rfftn_m3d,size(m3d,1))
        for i = 1:length(m3d)
            @test rfftd3_m3d[i] ≈ true_fftd3_m3d[i]
            @test irfft_rfftd3_m3d[i] ≈ m3d[i]
            @test irfftn_rfftn_m3d[i] ≈ m3d[i]
        end

        fftn_m3d = fft(m3d)
        @test size(fftn_m3d) == (5,3,2)
        rfftn_m3d = rfft(m3d)
        @test size(rfftn_m3d) == (3,3,2)
        for i = 1:3, j = 1:3, k = 1:2
            @test rfftn_m3d[i,j,k] ≈ fftn_m3d[i,j,k]
        end
    end # !mkl
end

# FFT self-test algorithm (for unscaled 1d forward FFTs):
#   Funda Ergün, "Testing multivariate linear functions: Overcoming
#   the generator bottleneck," Proc. 27th ACM Symposium on the Theory
#   of Computing, pp. 407-416 (1995).
# Check linearity, impulse-response, and time-shift properties.
function fft_test{T<:Complex}(p::Base.DFT.Plan{T}, ntrials=4,
                              tol=1e5 * eps(real(T)))
    ndims(p) == 1 || throw(ArgumentError("not a 1d FFT"))
    n = length(p)
    twopi_i = (-2 * convert(real(T), π)/n * (0:n-1)) * im
    for trial = 1:ntrials
        # linearity:
        x = rand(T, n)
        y = rand(T, n)
        α = rand(T)
        β = rand(T)
        X = p * (α*x + β*y)
        err = norm(α * (p*x) + β * (p*y) - X, Inf) / norm(X, Inf)
        err <= tol || error("linearity error $err in $p")

        # impulse-response:
        z = zeros(T, n)
        i = rand(0:n-1)
        z[i+1] = 1
        X = exp.(twopi_i*i)
        err = norm(p*z - X, Inf) / norm(X, Inf)
        err <= tol || error("impulse-response error $err in $p")

        # time-shift:
        if n > 1
            s = rand(1:n-1)
            X = (p*x).*exp.(twopi_i*s)
            err = norm(p*circshift(x,s) - X, Inf) / norm(X, Inf)
            err <= tol || error("time-shift error $err in $p")
        end
    end
end

for T in (Complex64, Complex128)
    for n in [1:100; 121; 143; 1000; 1024; 1031; 2000; 2048]
        x = zeros(T, n)
        fft_test(plan_fft(x))
        fft_test(plan_fft_(x))
    end
end

# test inversion, scaling, and pre-allocated variants
for T in (Complex64, Complex128)
    for x in (T[1:100;], copy(reshape(T[1:200;], 20,10)))
        y = similar(x)
        for planner in (plan_fft, plan_fft_, plan_ifft, plan_ifft_)
            p = planner(x)
            pi = inv(p)
            p3 = 3*p
            p3i = inv(p3)
            @test eltype(p) == eltype(pi) == eltype(p3) == eltype(p3i) == T
            @test vecnorm(x - p3i * (p * 3x)) < eps(real(T)) * 10000
            @test vecnorm(3x - pi * (p3 * x)) < eps(real(T)) * 10000
            A_mul_B!(y, p, x)
            @test y == p * x
            A_ldiv_B!(y, p, x)
            @test y == p \ x
        end
    end
end

let
    plan32 = plan_fft([1.0:2048.0;])
    plan64 = plan_fft([1f0:2048f0;])
    FFTW.flops(plan32)
    FFTW.flops(plan64)
end

# issue #9772
for x in (randn(10),randn(10,12))
    z = complex(x)
    y = rfft(x)
    @inferred rfft(x)
    @inferred brfft(x,18)
    @inferred brfft(y,10)
    for f in (plan_bfft!, plan_fft!, plan_ifft!,
              plan_bfft, plan_fft, plan_ifft,
              fft, bfft, fft_, ifft)
        p = @inferred f(z)
        if isa(p, FFTW.Plan)
            @inferred FFTW.plan_inv(p)
        end
    end
    for f in (plan_bfft, plan_fft, plan_ifft,
              plan_rfft, fft, bfft, fft_, ifft)
        p = @inferred f(x)
        if isa(p, FFTW.Plan)
            @inferred FFTW.plan_inv(p)
        end
    end
    # note: inference doesn't work for plan_fft_ since the
    #       algorithm steps are included in the CTPlan type
end

# issue #17896
a = rand(5)
@test  fft(a) ==  fft(view(a,:)) ==  fft(view(a, 1:5)) ==  fft(view(a, [1:5;]))
@test rfft(a) == rfft(view(a,:)) == rfft(view(a, 1:5)) == rfft(view(a, [1:5;]))
a16 = convert(Vector{Float16}, a)
@test  fft(a16) ==  fft(view(a16,:)) ==  fft(view(a16, 1:5)) ==  fft(view(a16, [1:5;]))
@test rfft(a16) == rfft(view(a16,:)) == rfft(view(a16, 1:5)) == rfft(view(a16, [1:5;]))
