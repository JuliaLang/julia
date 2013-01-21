# Sample of strpack.jl: Reading a .png header

load("strpack")
fpng = open(ARGS[1])

# check the signature
signature = unpack(fpng, s"BcccBBBB")
@assert isequal(signature, struct(s"BcccBBBB", 0x89, 'P', 'N', 'G', 0x0D, 0x0A, 0x1A, 0x0A))

# read the header
header = unpack(fpng, s"
                > # big endian
                i # length of this chunk
                cccc # IHDR in this case
                [size]2I
                bbbbb
                ")
close(fpng)

println("PNG File info for $(ARGS[1]):")
# check we got the header correctly
@assert isequal(header[1], 0x0D)
@assert isequal(header[2:5], ['I', 'H', 'D', 'R'])
println("Width: $(header.size[1]), Height: $(header.size[2])")
println("Bit depth: $(header[7])")
println("Color type: $(header[8])")
println("Compression method: $(header[9])")
println("Filter method: $(header[10])")
println("Interlace method: $(header[11])")
