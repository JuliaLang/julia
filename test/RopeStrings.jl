# This file is a part of Julia. License is MIT: http://julialang.org/license

@test sizeof(RopeString("abc","def")) == 6
