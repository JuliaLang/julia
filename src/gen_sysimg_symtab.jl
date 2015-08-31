# This file is a part of Julia. License is MIT: http://julialang.org/license

# script to generate tables of common symbols from a system image
# steps to rerun this:
#   1. empty the contents of common_symbols*.inc
#   2. modify dump.c to write and read a '\0' after every symbol name
#   3. build sys.ji
#   4. cd src && ../julia gen_sysimg_symtab.jl ../usr/lib/julia/sys.ji

fname = ARGS[1]

io,_ = open(pipe(`strings -n 3 $fname`,
                 `tr -d "() \t+-"`,
                 `sort`, `uniq -c`, `sort -g -r`,
                 `grep -v Main`,  # for some reason Main breaks things
                 `head -n 315`))  # 63 + 252

function outputline(io, line)
    row = split(chomp(line), " ", keep=false)
    println(io, "jl_symbol(\"", row[2], "\"),")
end

foreach(f, it) = for x in it; f(x); end

lines = eachline(io)

open(f->foreach(l->outputline(f,l), take(lines,63)), "common_symbols1.inc", "w")
open(f->foreach(l->outputline(f,l), lines), "common_symbols2.inc", "w")
