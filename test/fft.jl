# fft
a = rand(8) + im*rand(8)
@test norm(ifft(fft(a)) - a) < 1e-8

m4 = [16.    2     3    13;
    5    11    10     8;
    9     7     6    12;
    4    14    15     1]

b = rand(17,14)
b[3:6,9:12] = m4
sm4 = slice(b,3:6,9:12)

pm4 = plan_fft(m4,1)

fft_m4 = fft(m4,1)
fftd2_m4 = fft(m4,2)
ifft_fft_m4 = ifft(fft(m4,1),1)
fftn_m4 = fft(m4)
ifftn_fftn_m4 = ifft(fft(m4))

fft!_m4 = complex(m4); fft!(fft!_m4,1)
fft!d2_m4 = complex(m4); fft!(fft!d2_m4,2)
ifft!_fft_m4 = fft(m4,1); ifft!(ifft!_fft_m4,1)
fft!n_m4 = complex(m4); fft!(fft!n_m4)
ifft!n_fftn_m4 = fft(m4); ifft!(ifft!n_fftn_m4)

pfft_m4 = plan_fft(m4,1)(m4)
pfftd2_m4 = plan_fft(m4,2)(m4)
pifft_fft_m4 = plan_ifft(fft_m4,1)(fft_m4)
pfftn_m4 = plan_fft(m4)(m4)
pifftn_fftn_m4 = plan_ifft(fftn_m4)(fftn_m4)

pfft!_m4 = complex(m4); plan_fft!(pfft!_m4,1)(pfft!_m4)
pfft!d2_m4 = complex(m4); plan_fft!(pfft!d2_m4,2)(pfft!d2_m4)
pifft!_fft_m4 = fft(m4,1); plan_ifft!(pifft!_fft_m4,1)(pifft!_fft_m4)
pfft!n_m4 = complex(m4); plan_fft!(pfft!n_m4)(pfft!n_m4)
pifft!n_fftn_m4 = fft(m4); plan_ifft!(pifft!n_fftn_m4)(pifft!n_fftn_m4)

sfftn_m4 = fft(sm4)
psfftn_m4 = plan_fft(sm4)(sm4)
sfft!n_b = complex128(b)
sfft!n_m4 = slice(sfft!n_b,3:6,9:12); fft!(sfft!n_m4)
psfft!n_b = complex128(b)
psfft!n_m4 = slice(psfft!n_b,3:6,9:12); plan_fft!(psfft!n_m4)(psfft!n_m4)

true_fft_m4 = [
    34.            34.            34.            34.;
     7. - 1.im  -5. + 3.im  -3. + 5.im   1. - 7.im;
    16.           -16.           -16.            16.;
     7. + 1.im  -5. - 3.im  -3. - 5.im   1. + 7.im ]

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

for i = 1:length(m4)
    @test_approx_eq fft_m4[i] true_fft_m4[i]
    @test_approx_eq fftd2_m4[i] true_fftd2_m4[i]
    @test_approx_eq ifft_fft_m4[i] m4[i]
    @test_approx_eq fftn_m4[i] true_fftn_m4[i]
    @test_approx_eq ifftn_fftn_m4[i] m4[i]

    @test_approx_eq fft!_m4[i] true_fft_m4[i]
    @test_approx_eq fft!d2_m4[i] true_fftd2_m4[i]
    @test_approx_eq ifft!_fft_m4[i] m4[i]
    @test_approx_eq fft!n_m4[i] true_fftn_m4[i]
    @test_approx_eq ifft!n_fftn_m4[i] m4[i]

    @test_approx_eq pfft_m4[i] true_fft_m4[i]
    @test_approx_eq pfftd2_m4[i] true_fftd2_m4[i]
    @test_approx_eq pifft_fft_m4[i] m4[i]
    @test_approx_eq pfftn_m4[i] true_fftn_m4[i]
    @test_approx_eq pifftn_fftn_m4[i] m4[i]

    @test_approx_eq pfft!_m4[i] true_fft_m4[i]
    @test_approx_eq pfft!d2_m4[i] true_fftd2_m4[i]
    @test_approx_eq pifft!_fft_m4[i] m4[i]
    @test_approx_eq pfft!n_m4[i] true_fftn_m4[i]
    @test_approx_eq pifft!n_fftn_m4[i] m4[i]

    @test_approx_eq sfftn_m4[i] true_fftn_m4[i]
    @test_approx_eq sfft!n_m4[i] true_fftn_m4[i]
    @test_approx_eq psfftn_m4[i] true_fftn_m4[i]
    @test_approx_eq psfft!n_m4[i] true_fftn_m4[i]
end

ifft!(sfft!n_m4)
plan_ifft!(psfft!n_m4)(psfft!n_m4)
@test norm(sfft!n_m4 - m4) < 1e-8
@test norm(psfft!n_m4 - m4) < 1e-8

m3d = float32(reshape(1:5*3*2, 5, 3, 2))
ifft3_fft3_m3d = ifft(fft(m3d))

fftd3_m3d = fft(m3d,3)
ifftd3_fftd3_m3d = ifft(fftd3_m3d,3)

fft!d3_m3d = complex(m3d); fft!(fft!d3_m3d,3)
ifft!d3_fftd3_m3d = copy(fft!d3_m3d); ifft!(ifft!d3_fftd3_m3d,3)

pfftd3_m3d = plan_fft(m3d,3)(m3d)
pifftd3_fftd3_m3d = plan_ifft(fftd3_m3d,3)(fftd3_m3d)

pfft!d3_m3d = complex(m3d); plan_fft!(pfft!d3_m3d,3)(pfft!d3_m3d)
pifft!d3_fftd3_m3d = copy(fft!d3_m3d); plan_ifft!(pifft!d3_fftd3_m3d,3)(pifft!d3_fftd3_m3d)

@test isa(fftd3_m3d, Array{Complex64,3})
@test isa(ifftd3_fftd3_m3d, Array{Complex64,3})
@test isa(fft!d3_m3d, Array{Complex64,3})
@test isa(ifft!d3_fftd3_m3d, Array{Complex64,3})
@test isa(pfftd3_m3d, Array{Complex64,3})
@test isa(pifftd3_fftd3_m3d, Array{Complex64,3})
@test isa(pfft!d3_m3d, Array{Complex64,3})
@test isa(pifft!d3_fftd3_m3d, Array{Complex64,3})

true_fftd3_m3d = Array(Float32, 5, 3, 2)
true_fftd3_m3d[:,:,1] = 17:2:45
true_fftd3_m3d[:,:,2] = -15

for i = 1:length(m3d)
    @test_approx_eq fftd3_m3d[i] true_fftd3_m3d[i]
    @test_approx_eq ifftd3_fftd3_m3d[i] m3d[i]
    @test_approx_eq ifft3_fft3_m3d[i] m3d[i]

    @test_approx_eq fft!d3_m3d[i] true_fftd3_m3d[i]
    @test_approx_eq ifft!d3_fftd3_m3d[i] m3d[i]

    @test_approx_eq pfftd3_m3d[i] true_fftd3_m3d[i]
    @test_approx_eq pifftd3_fftd3_m3d[i] m3d[i]
    @test_approx_eq pfft!d3_m3d[i] true_fftd3_m3d[i]
    @test_approx_eq pifft!d3_fftd3_m3d[i] m3d[i]
end

# rfft/rfftn

rfft_m4 = rfft(m4,1)
rfftd2_m4 = rfft(m4,2)
rfftn_m4 = rfft(m4)

prfft_m4 = plan_rfft(m4,1)(m4)
prfftd2_m4 = plan_rfft(m4,2)(m4)
prfftn_m4 = plan_rfft(m4)(m4)

srfftn_m4 = rfft(sm4)
psrfftn_m4 = plan_rfft(sm4)(sm4)

for i = 1:3, j = 1:4
    @test_approx_eq rfft_m4[i,j] true_fft_m4[i,j]
    @test_approx_eq rfftd2_m4[j,i] true_fftd2_m4[j,i]
    @test_approx_eq rfftn_m4[i,j] true_fftn_m4[i,j]

    @test_approx_eq prfft_m4[i,j] true_fft_m4[i,j]
    @test_approx_eq prfftd2_m4[j,i] true_fftd2_m4[j,i]
    @test_approx_eq prfftn_m4[i,j] true_fftn_m4[i,j]

    @test_approx_eq srfftn_m4[i,j] true_fftn_m4[i,j]
    @test_approx_eq psrfftn_m4[i,j] true_fftn_m4[i,j]
end

irfft_rfft_m4 = irfft(rfft_m4,size(m4,1),1)
irfft_rfftd2_m4 = irfft(rfftd2_m4,size(m4,2),2)
irfftn_rfftn_m4 = irfft(rfftn_m4,size(m4,1))

pirfft_rfft_m4 = plan_irfft(rfft_m4,size(m4,1),1)(rfft_m4)
pirfft_rfftd2_m4 = plan_irfft(rfftd2_m4,size(m4,2),2)(rfftd2_m4)
pirfftn_rfftn_m4 = plan_irfft(rfftn_m4,size(m4,1))(rfftn_m4)

for i = 1:length(m4)
    @test_approx_eq irfft_rfft_m4[i] m4[i]
    @test_approx_eq irfft_rfftd2_m4[i] m4[i]
    @test_approx_eq irfftn_rfftn_m4[i] m4[i]

    @test_approx_eq pirfft_rfft_m4[i] m4[i]
    @test_approx_eq pirfft_rfftd2_m4[i] m4[i]
    @test_approx_eq pirfftn_rfftn_m4[i] m4[i]
end

rfftn_m3d = rfft(m3d)
rfftd3_m3d = rfft(m3d,3)
@test size(rfftd3_m3d) == size(fftd3_m3d)
irfft_rfftd3_m3d = irfft(rfftd3_m3d,size(m3d,3),3)
irfftn_rfftn_m3d = irfft(rfftn_m3d,size(m3d,1))
for i = 1:length(m3d)
    @test_approx_eq rfftd3_m3d[i] true_fftd3_m3d[i]
    @test_approx_eq irfft_rfftd3_m3d[i] m3d[i]
    @test_approx_eq irfftn_rfftn_m3d[i] m3d[i]
end

fftn_m3d = fft(m3d)
@test size(fftn_m3d) == (5,3,2)
rfftn_m3d = rfft(m3d)
@test size(rfftn_m3d) == (3,3,2)
for i = 1:3, j = 1:3, k = 1:2
    @test_approx_eq rfftn_m3d[i,j,k] fftn_m3d[i,j,k]
end
