# fft
a = rand(8) + im*rand(8)
@test norm(ifft(fft(a)) - a) < 1e-8

m4 = [16.    2     3    13;
    5    11    10     8;
    9     7     6    12;
    4    14    15     1]

fft_m4 = fft(m4)
fft2_m4 = fft2(m4)
fftd2_m4 = fft(m4,2)
ifft_fft_m4 = ifft(fft(m4))
fftn_m4 = fftn(m4)
ifftn_fftn_m4 = ifftn(fftn(m4))

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
    @assert_approx_eq fft_m4[i] true_fft_m4[i]
    @assert_approx_eq fft2_m4[i] true_fftn_m4[i]
    @assert_approx_eq fftd2_m4[i] true_fftd2_m4[i]
    @assert_approx_eq ifft_fft_m4[i] m4[i]
    @assert_approx_eq fftn_m4[i] true_fftn_m4[i]
    @assert_approx_eq ifftn_fftn_m4[i] m4[i]
end

m3d = float32(reshape(1:5*3*2, 5, 3, 2))
ifft3_fft3_m3d = ifft3(fft3(m3d))
fftd3_m3d = fft(m3d,3)
ifftd3_fftd3_m3d = ifft(fftd3_m3d,3)
@test isa(fftd3_m3d, Array{Complex64,3})

true_fftd3_m3d = Array(Float32, 5, 3, 2)
true_fftd3_m3d[:,:,1] = 17:2:45
true_fftd3_m3d[:,:,2] = -15

for i = 1:length(m3d)
    @assert_approx_eq fftd3_m3d[i] true_fftd3_m3d[i]
    @assert_approx_eq ifftd3_fftd3_m3d[i] m3d[i]
    @assert_approx_eq ifft3_fft3_m3d[i] m3d[i]
end

# rfft/rfftn

rfft_m4 = rfft(m4)
rfftd2_m4 = rfft(m4,2)
rfftn_m4 = rfftn(m4)

for i = 1:3, j = 1:4
    @assert_approx_eq rfft_m4[i,j] true_fft_m4[i,j]
    @assert_approx_eq rfftd2_m4[j,i] true_fftd2_m4[j,i]
    @assert_approx_eq rfftn_m4[i,j] true_fftn_m4[i,j]
end

irfft_rfft_m4 = irfft(rfft_m4,size(m4,1))
irfft_rfftd2_m4 = irfft(rfftd2_m4,size(m4,2),2)
irfftn_rfftn_m4 = irfftn(rfftn_m4,size(m4,1))
for i = 1:length(m4)
    @assert_approx_eq irfft_rfft_m4[i] m4[i]
    @assert_approx_eq irfft_rfftd2_m4[i] m4[i]
    @assert_approx_eq irfftn_rfftn_m4[i] m4[i]
end

rfftn_m3d = rfftn(m3d)
rfftd3_m3d = rfft(m3d,3)
@test size(rfftd3_m3d) == size(fftd3_m3d)
irfft_rfftd3_m3d = irfft(rfftd3_m3d,size(m3d,3),3)
irfftn_rfftn_m3d = irfftn(rfftn_m3d,size(m3d,1))
for i = 1:length(m3d)
    @assert_approx_eq rfftd3_m3d[i] true_fftd3_m3d[i]
    @assert_approx_eq irfft_rfftd3_m3d[i] m3d[i]
    @assert_approx_eq irfftn_rfftn_m3d[i] m3d[i]
end

fftn_m3d = fftn(m3d)
@test size(fftn_m3d) == (5,3,2)
rfftn_m3d = rfftn(m3d)
@test size(rfftn_m3d) == (3,3,2)
for i = 1:3, j = 1:3, k = 1:2
    @assert_approx_eq rfftn_m3d[i,j,k] fftn_m3d[i,j,k]
end
