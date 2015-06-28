# This file is a part of Julia. License is MIT: http://julialang.org/license

# issue #7764
let
    srep = RepString("Σβ",2)
    s="Σβ"
    ss=SubString(s,1,endof(s))

    @test ss^2 == "ΣβΣβ"
    @test RepString(ss,2) == "ΣβΣβ"

    @test endof(srep) == 7

    @test next(srep, 3) == ('β',5)
    @test next(srep, 7) == ('β',9)

    @test srep[7] == 'β'
    @test_throws BoundsError srep[8]
end
