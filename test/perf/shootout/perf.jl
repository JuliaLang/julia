include("../perfutil.jl")

include("binary_trees.jl")
@timeit binary_trees(10) "binary_trees"

include("fannkuch.jl")
@timeit fannkuch(7) "fannkuch"

include("fasta.jl")
n = 100
@timeit begin
    repeat_fasta(alu, 2n)
    random_fasta(iub[1], iub[2], 3n)
    random_fasta(homosapiens[1], homosapiens[2], 5n)
end "fasta"

include("k_nucleotide.jl")
@timeit k_nucleotide("shootout/knucleotide-input.txt") "k_nucleotide"

include("mandelbrot.jl")
@timeit mandelbrot(200, "shootout/mandelbrot-output-julia.txt") "mandelbrot"

include("meteor_contest.jl")
@timeit1 meteor_contest() "meteor_contest"

include("nbody.jl")
using NBody
@timeit NBody.nbody() "nbody"

include("nbody_vec.jl")
using NBodyVec
@timeit NBodyVec.nbody_vec() "nbody_vec"

include("pidigits.jl")
@assert pidigits(1000) == 9216420198
@timeit pidigits(1000) "pidigits"

include("regex_dna.jl")
@timeit regex_dna("shootout/regexdna-input.txt") "regex_dna"

include("revcomp.jl")
@timeit revcomp("shootout/revcomp-input.txt") "revcomp"

include("spectralnorm.jl")
@timeit spectralnorm() "spectralnorm"
