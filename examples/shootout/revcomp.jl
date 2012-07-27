# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by David Campbell

# FIXME(davekong) Is there support in Julia for doing more efficient IO and
# handling of byte arrays?

const revcomp = {
   'A'=> 'T', 'a'=> 'T',
   'C'=> 'G', 'c'=> 'G',
   'G'=> 'C', 'g'=> 'C',
   'T'=> 'A', 't'=> 'A',
   'U'=> 'A', 'u'=> 'A',
   'M'=> 'K', 'm'=> 'K',
   'R'=> 'Y', 'r'=> 'Y',
   'W'=> 'W', 'w'=> 'W',
   'S'=> 'S', 's'=> 'S',
   'Y'=> 'R', 'y'=> 'R',
   'K'=> 'M', 'k'=> 'M',
   'V'=> 'B', 'v'=> 'B',
   'H'=> 'D', 'h'=> 'D',
   'D'=> 'H', 'd'=> 'H',
   'B'=> 'V', 'b'=> 'V',
   'N'=> 'N', 'n'=> 'N',
}

function print_buff(b)
    br = join(reverse(b))
    for i = 1:60:length(br)
        if i+59 > length(br)
            println(br[i:end])
        else
            println(br[i:i+59])
        end
    end
end

function main()
    buff = Array(Char, 0)
    while true
        line = readline(stdin_stream)
        if line == ""
            print_buff(buff)
            return
        elseif line[1] == '>'
            print_buff(buff)
            buff = Array(Char, 0)
            print(line)
        else
            buff = append(buff, [revcomp[x] for x in line[1:end-1]])
        end
    end
end

main()
