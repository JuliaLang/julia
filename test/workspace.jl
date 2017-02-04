# This file is a part of Julia. License is MIT: http://julialang.org/license

script = """
# Issue #11948
f(x) = x+1
workspace()
@assert !isdefined(:f)
LastMain.f(2)

# PR #12990
io = IOBuffer()
show(io, Pair)
@assert String(take!(io)) == "Pair"
@assert !Base.inbase(LastMain)
"""
exename = Base.julia_cmd()
run(`$exename --startup-file=no -e $script`)

# issue #17764
script2 = """
type Foo end
workspace()
type Foo end
@assert Tuple{Type{LastMain.Foo}} !== Tuple{Type{Main.Foo}}
"""
run(`$exename --startup-file=no -e $script2`)
