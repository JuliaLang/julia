# fft
a = rand(8) + im*rand(8)
@assert norm((1/length(a))*ifft(fft(a)) - a) < 1e-8

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
    @assert_approx_eq ifft_fft_m4[i] m4[i]*4
    @assert_approx_eq fftn_m4[i] true_fftn_m4[i]
    @assert_approx_eq ifftn_fftn_m4[i] m4[i]*16
end
