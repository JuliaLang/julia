function optest_loose(x,o::Options)
    checkflag = ischeck(o)
    paramtable = fill_defaults(o,:scale,2)
    scale = paramtable[:scale]
    # Do the computation
    y = scale*x
    return y
end
optest_loose(x) = optest_loose(x,Options())

function optest_fussy(x,o::Options)
    checkflag = ischeck(o)
    paramtable = fill_defaults(o,:scale,2)
    scale = paramtable[:scale]
    # Do the computation
    y = scale*x
    # Check that all options were used
    docheck(o,checkflag)
    return y
end
optest_fussy(x) = optest_fussy(x,Options())    

ops = Options(:dummy,5.2,:scale,3)
x = [5,2]
println(optest_loose(x,ops))

ops = Options(:dummy,5.2,:scale,3)
println(optest_fussy(x,ops))
