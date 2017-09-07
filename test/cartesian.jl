# This file is a part of Julia. License is MIT: https://julialang.org/license

@test Base.Cartesian.exprresolve(:(1 + 3)) == 4
ex = Base.Cartesian.exprresolve(:(if 5 > 4; :x; else :y; end))
@test ex.args[2] == QuoteNode(:x)

@test Base.Cartesian.lreplace!("val_col", Base.Cartesian.LReplace{String}(:col, "col", 1)) == "val_1"