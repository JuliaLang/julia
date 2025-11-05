module GensymTestPkg

s1 = gensym()
f() = gensym() != s1

export f

end # module GensymTestPkg
