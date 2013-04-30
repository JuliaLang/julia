# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Based on C/Scala implementations
function mandel(n)
    f = open("mandelbrot-output.txt","w")
	bit_num = 0
	byte_acc = 0
	write(f,"P4\n$n $n\n")
	for y = 0:n-1, x = 0:n-1
		zr = zi = 0.0 
		tr = ti = 0.0
		cr = (2.0*x/n - 1.5)
		ci = (2.0*y/n - 1.0)
		for i = 1:50
			zi = 2.0*zr*zi + ci
			zr = tr - ti + cr
			tr = zr*zr
			ti = zi*zi
			tr+ti > 4.0 && break
		end
		byte_acc <<= 1
		tr+ti <= 4.0 && (byte_acc += 1)
		bit_num += 1
		if x == n-1
			byte_acc <<= 8
			bit_num = 8
		end
		if bit_num == 8
			write(f,char(byte_acc))
			byte_acc = 0
			bit_num = 0
		end
	end
	close(f)
end
mandel(int(ARGS[1]))
#@time mandel(1000)
#@time mandel(4000)
#@time mandel(16000)
