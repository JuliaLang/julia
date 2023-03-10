using BenchmarkTools
using JuliaSyntax

include("test_utils.jl")

function concat_base()
    basedir = joinpath(Sys.BINDIR, "..", "share", "julia", "base")
    io = IOBuffer()
    for f in find_source_in_path(basedir)
        write(io, read(f, String))
        println(io)
    end
    return String(take!(io))
end

all_base_code = concat_base()

b_ParseStream = @benchmark JuliaSyntax.parse!(JuliaSyntax.ParseStream(all_base_code), rule=:toplevel)
b_GreenNode   = @benchmark JuliaSyntax.parseall(JuliaSyntax.GreenNode, all_base_code)
b_SyntaxNode  = @benchmark JuliaSyntax.parseall(JuliaSyntax.SyntaxNode, all_base_code)
b_Expr        = @benchmark JuliaSyntax.parseall(Expr, all_base_code)

@info "Benchmarks" ParseStream=b_ParseStream GreenNode=b_GreenNode SyntaxNode=b_SyntaxNode Expr=b_Expr


# Allocation profiling
#
# using Profile.Allocs
# using PProf
# Allocs.clear()
# stream = JuliaSyntax.ParseStream(text);
# JuliaSyntax.peek(stream);
# Allocs.@profile sample_rate=1 JuliaSyntax.parse(stream)
# PProf.Allocs.pprof()
