
const line_width = 60

const alu = string( 
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA",
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT",
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA",
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA")

const iub =
    (b"acgtBDHKMNRSVWY", 
     [0.27, 0.12, 0.12, 0.27, 0.02,
      0.02, 0.02, 0.02, 0.02, 0.02,
      0.02, 0.02, 0.02, 0.02, 0.02])

const homosapiens =
    (b"acgt",
     [0.3029549426680, 0.1979883004921,
      0.1975473066391, 0.3015094502008])



const IM  = 139968
const IA  =   3877
const IC  =  29573
const IMf = float64(IM)
rng_state = 42

function gen_random(max::Float64)
    global rng_state = (rng_state * IA + IC) % IM
    max * float(rng_state) / IMf
end


function repeat_fasta(src, n)
    k = length(src)
    s = string(src, src, src[1:n % k])

    for j = 0:div(n, line_width) - 1
        i = j * line_width % k
        println(s[i + 1:i + line_width])
    end

    if n % line_width > 0
        println(s[end - (n % line_width) + 1:])
    end
end


function choose_char(cs)
    k = length(cs)
    r = gen_random(1.0)

    if r < cs[1]; return 1; end

    a::Uint = 1
    b::Uint = k

    while b > a + 1
        c = div(a + b, 2)
        if r < cs[c]; b = c; else a = c; end
    end

    b
end


function random_fasta(symb, pr, n)
    cs = cumsum(pr)
    line = Array(Uint8, line_width)
    k = n
    while k > 0
        m = min(k, line_width)
        for i = 1:m
            line[i] = symb[choose_char(cs)]
        end
        write(line[1:m]); println()
        k -= line_width
    end
end



const n = int(ARGS[1])

println(">ONE Homo sapiens alu")
repeat_fasta(alu, 2n)

println(">TWO IUB ambiguity codes")
random_fasta(iub[1], iub[2], 3n)

println(">THREE Homo sapiens frequency")
random_fasta(homosapiens[1], homosapiens[2], 5n)

