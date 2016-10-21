# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test prettyprint_getunits
@test Base.prettyprint_getunits(0, length(Base._mem_units), 1024) == (0, 1)
@test Base.prettyprint_getunits(1, length(Base._mem_units), 1024) == (1, 1)
@test Base.prettyprint_getunits(1023, length(Base._mem_units), 1024)[2] == 1
@test Base.prettyprint_getunits(1024, length(Base._mem_units), 1024) == (1, 2)
@test Base.prettyprint_getunits(1025, length(Base._mem_units), 1024)[2] == 2