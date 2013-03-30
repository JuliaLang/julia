using Test

let
    ENV["JULIA ENV TEST VALUE"] = x = randstring(1000)
    @test ENV["JULIA ENV TEST VALUE"] == x
    @test_fails ENV["julia env test value"]
    @test has(ENV,"JULIA ENV TEST VALUE")
    @test delete!(ENV,"JULIA ENV TEST VALUE") == x
    @test !has(ENV,"JULIA ENV TEST VALUE")
    @test_fails ENV["JULIA ENV TEST VALUE"]
    @test_fails delete!(ENV,"JULIA ENV TEST VALUE")
    count = 0
    for (k,v) in ENV
        count += 1
    end
    @test count == length(ENV)
    @test get(ENV, "JULIA ENV TEST VALUE", nothing) == nothing
end

let
    key = "JULIA ENV TEST VALUE = 2"
    try delete!(CONFIG,key,:entry) end
    CONFIG[key] = x = (randstring(1000),100,randstring)
    @test CONFIG[key] == x
    getcnt = 0
    setcnt = 0
    deletedcnt = 0
    expect = x
    CONFIG[key, :default, :getter, :setter, :delete] = (42,
            (k,v)->(@test(k==key); getcnt+=1; (v,true)),
            (k,v,hadv,newv)->(@test(k==key && newv==expect); setcnt+=1; newv),
            (k,v)->(@test(k==key); deletedcnt+=1; return true))
    @test getcnt == 0
    @test setcnt == 1
    @test deletedcnt == 0
    expect = 42
    @test delete!(CONFIG,key) == (x,true)
    @test getcnt == 1
    @test setcnt == 2
    @test deletedcnt == 1
    @test CONFIG[key] == (42,true)
    @test_fails delete!(CONFIG,key)
    @test delete!(CONFIG,key,:default) == (42,true)
    @test getcnt == 3
    @test setcnt == 2
    @test deletedcnt == 2
    expect = 1
    CONFIG[key, :default] = 1
    expect = 2
    CONFIG[key] = 2
    @test getcnt == 3
    @test setcnt == 4
    @test deletedcnt == 2
    @test delete!(CONFIG,key,:default) == (2,true)
    @test getcnt == 4
    @test setcnt == 4
    @test deletedcnt == 2
    @test CONFIG[key] == (2,true)
    delete!(CONFIG,key,:getter,:setter,:delete,:value,:entry)
    @test getcnt == 5
    @test setcnt == 4
    @test deletedcnt == 2
end
