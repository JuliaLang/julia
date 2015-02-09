const line_width = 60

const alu = string(
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA",
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT",
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA",
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA")

const iub1 = b"acgtBDHKMNRSVWY"
const iub2 = [0.27, 0.12, 0.12, 0.27, 0.02,0.02, 0.02, 0.02, 0.02, 0.02,0.02, 0.02, 0.02, 0.02, 0.02]

const homosapiens1 = b"acgt"
const homosapiens2 = [0.3029549426680, 0.1979883004921,0.1975473066391, 0.3015094502008]

const IM  = 139968.0
const IA  =   3877.0
const IC  =  29573.0

function gen_random()
    global rng_state::Float64 = ((rng_state::Float64 * IA + IC) % IM) / IM
end
function repeat_fasta(src, n)
    k = length(src)
    return string(src, src, src[1:n % k])
end
function choose_char(cs)
    k = length(cs)
    r = gen_random()
    r < cs[1] && return 1
    a = 1
    b = k
    while b > a + 1
        c = fld(a + b, 2)
        if r < cs[c]; b = c; else a = c; end
    end
    b
end
function random_fasta(symb, pr, n)
    cs = cumsum(pr)
    line = Array(UInt8, line_width)
    k = n
    while k > 0
        m = min(k, line_width)
        for i = 1:m
            line[i] = symb[choose_char(cs)]
        end
        k -= line_width
    end
end

rng_state = 42.0
function fasta(n=25000000)
  repeat_fasta(alu, 2n)
  random_fasta(iub1, iub2, 3n)
  random_fasta(homosapiens1, homosapiens2, 5n)
end
