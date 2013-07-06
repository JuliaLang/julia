include("../perfutil.jl")

include("binary_trees.jl")
@timeit1 binary_trees(10) "binary_trees"

include("fannkuch.jl")
@timeit1 fannkuch(7) "fannkuch" 

include("fasta.jl")
n = 100
@timeit1 begin
    repeat_fasta(alu, 2n)
    random_fasta(iub[1], iub[2], 3n)
    random_fasta(homosapiens[1], homosapiens[2], 5n)
end "fasta"

include("k_nucleotide.jl")
@timeit1 k_nucleotide("shootout/knucleotide-input.txt") "k_nucleotide"

include("mandelbrot.jl")
@timeit1 mandel("shootout/mandelbrot-output-julia.txt") "mandelbrot"

include("meteor_contest.jl")
@timeit1 meteor_contest() "meteor_contest"

include("nbody.jl")
using NBody
@timeit1 NBody.nbody() "nbody"

include("nbody_vec.jl")
using NBodyVec
@timeit1 NBodyVec.nbody_vec() "nbody_vec"

include("pidigits.jl")
#@assert pidigits(1000) == 9216420198
#@timeit1 pidigits(1000) "pidigits"

include("regex_dna.jl")
@timeit1 regex_dna("shootout/regexdna-input.txt") "regex_dna"

include("revcomp.jl")
@timeit1 revcomp("shootout/revcomp-input.txt") "revcomp"

include("spectralnorm.jl")
@timeit1 spectralnorm() "spectralnorm"
