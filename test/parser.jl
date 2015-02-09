# issue #9684
let
    for (ex1, ex2) in [("5.≠x", "5.!=x"),
                       ("5.≥x", "5.>=x"),
                       ("5.≤x", "5.<=x")]
        ex1 = parse(ex1); ex2 = parse(ex2)
        @test ex1.head === :comparison && (ex1.head === ex2.head)
        @test ex1.args[1] === 5 && ex2.args[1] === 5
        @test is(eval(Main, ex1.args[2]), eval(Main, ex2.args[2]))
        @test ex1.args[3] === :x && (ex1.args[3] === ex2.args[3])
    end
end

# issue #9704
let a = :a
    @test :(try
            catch $a
            end) == :(try
                      catch a
                      end)
    @test :(module $a
            end) == :(module a
                      end)
end
