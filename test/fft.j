# fft
a = rand(8) + im*rand(8)
@assert norm((1/length(a))*ifft(fft(a)) - a) < 1e-8

