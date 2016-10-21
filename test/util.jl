# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test prettyprint_getunits
@test Base.prettyprint_getunits(0, 6, 1024) == (0, 1)
@test Base.prettyprint_getunits(1, 6, 1024) == (1, 1)
@test Base.prettyprint_getunits(1023, 6, 1024)[2] == 1
@test Base.prettyprint_getunits(1024, 6, 1024) == (1, 2)
@test Base.prettyprint_getunits(1025, 6, 1024)[2] == 2
@test Base.prettyprint_getunits(1024^5 - 1, 6, 1024)[2] == 5
@test Base.prettyprint_getunits(1024^5, 6, 1024) == (1, 6)
@test Base.prettyprint_getunits(1024^5 + 1, 6, 1024)[2] == 6
@test Base.prettyprint_getunits(1024^5, 5, 1024) == (1024, 5)
