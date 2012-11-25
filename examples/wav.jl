require("sound")
import Sound.*

in_filename = ARGS[1]

y, Fs, nbits, opts = wavread(in_filename)
wavwrite(y, Fs, nbits, strcat("out-", in_filename))
