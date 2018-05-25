# This file is a part of Julia. License is MIT: https://julialang.org/license

# script to generate tables of common symbols from a system image
# steps to rerun this:
#   1. empty the contents of common_symbols*.inc
#   2. modify dump.c to write symbols in a clearly marked format (#define GEN_SYMTAB_MODE)
#   3. build sys.ji
#   4. cd src && ../julia gen_sysimg_symtab.jl ../usr/lib/julia/sys.ji

fname = ARGS[1]

io,_ = open(pipeline(`strings -n 4 $fname`,
                     `tr -d "() \t"`,
                     `grep JJJ`,
                     `sort`, `uniq -c`, `sort -g -r`,
                     `head -n 354`))  # 102 + 252

function outputline(io, line)
    row = split(line, " ", keep=false)
    println(io, "jl_symbol(\"", row[2][4:end], "\"),")
end

lines = eachline(io)

open(f->foreach(l->outputline(f,l), Base.Iterators.take(lines,102)), "common_symbols1.inc", "w")
open(f->foreach(l->outputline(f,l), lines), "common_symbols2.inc", "w")
