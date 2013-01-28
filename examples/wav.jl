require("sound")
using Sound

in_filename = ARGS[1]

y, Fs, nbits, opts = wavread(in_filename)
wavwrite(y, Fs, nbits, string("out-", in_filename))
