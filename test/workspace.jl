# This file is a part of Julia. License is MIT: http://julialang.org/license

# Issue #11948
script = """
f(x) = x+1
workspace()
@assert !isdefined(:f)
LastMain.f(2)
"""
exename = joinpath(JULIA_HOME, Base.julia_exename())
run(`$exename -f -e $script`)
