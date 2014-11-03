include("../perfutil.jl")

#If running from .. directory, include "shootout/" path to data file
rpath(filename::AbstractString) = string(contains(pwd(), "shootout") ? "" : "shootout/", filename)

include("binary_trees.jl")
@timeit binary_trees(10) "binary_trees" "Allocate and deallocate many many binary trees"

include("fannkuch.jl")
@timeit fannkuch(7) "fannkuch" "Indexed-access to tiny integer-sequence"

include("fasta.jl")
@timeit fasta(100) "fasta" "Generate and write random DNA sequences"

include("k_nucleotide.jl")
@timeit k_nucleotide(rpath("knucleotide-input.txt")) "k_nucleotide" "Hashtable update and k-nucleotide strings"

include("mandelbrot.jl")
@timeit mandelbrot(200, rpath("mandelbrot-output-julia.txt")) "mandelbrot" "Generate Mandelbrot set portable bitmap file"

include("meteor_contest.jl")
@timeit meteor_contest() "meteor_contest" "Search for solutions to shape packing puzzle"

include("nbody.jl")
using NBody
@timeit NBody.nbody() "nbody" "Double-precision N-body simulation"

include("nbody_vec.jl")
using NBodyVec
@timeit NBodyVec.nbody_vec() "nbody_vec" "A vectorized double-precision N-body simulation"

include("pidigits.jl")
@assert pidigits(1000) == 9216420198
@timeit pidigits(1000) "pidigits" "Streaming arbitrary-precision arithmetic"

include("regex_dna.jl")
@timeit regex_dna(rpath("regexdna-input.txt")) "regex_dna" "Match DNA 8-mers and substitute nucleotides for IUB codes"

include("revcomp.jl")
@timeit revcomp(rpath("revcomp-input.txt")) "revcomp" "Read DNA sequences - write their reverse-complement"

include("spectralnorm.jl")
@timeit spectralnorm() "spectralnorm" "Eigenvalue using the power method"
