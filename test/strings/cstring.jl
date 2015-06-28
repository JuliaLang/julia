# This file is a part of Julia. License is MIT: http://julialang.org/license

# Wstring
u8 = "\U10ffff\U1d565\U1d7f6\U00066\U2008a"
w = wstring(u8)
@test length(w) == 5 && utf8(w) == u8 && collect(u8) == collect(w)
@test u8 == WString(w.data)
