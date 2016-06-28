# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "workspace" begin
script = """
# Issue #11948
f(x) = x+1
workspace()
@assert !isdefined(:f)
LastMain.f(2)

# PR #12990
io = IOBuffer()
show(io, Pair)
@assert takebuf_string(io) == "Pair{A,B}"
@assert !Base.inbase(LastMain)
"""
exename = Base.julia_cmd()
run(`$exename --startup-file=no -e $script`)

end
