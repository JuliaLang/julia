test21 = "[\r\n{\r\n\"a\": 1,\r\n\"b\": 2\r\n},\r\n{\r\n\"a\": 3,\r\n\"b\": 4\r\n}\r\n]"
a = JSON.parse(test21)
@test isa(a, Vector{Any})
@test length(a) == 2
