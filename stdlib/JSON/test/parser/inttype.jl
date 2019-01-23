@testset for T in [Int32, Int64, Int128, BigInt]
    val = JSON.parse("{\"x\": 3}", inttype=T)
    @test isa(val, Dict{String, Any})
    @test length(val) == 1
    key = collect(keys(val))[1]
    @test string(key) == "x"
    value = val[key]
    @test value == 3
    @test typeof(value) == T
end

@testset begin
    teststr = """{"201736327611975630": 18005722827070440994}"""
    val = JSON.parse(teststr, inttype=Int128)
    @test val == Dict{String,Any}("201736327611975630"=> 18005722827070440994)
end
