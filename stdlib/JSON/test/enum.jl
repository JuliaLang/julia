@enum Animal zebra aardvark horse
@test json(zebra) == "\"zebra\""
@test json([aardvark, horse, Dict("z" => zebra)]) ==
    "[\"aardvark\",\"horse\",{\"z\":\"zebra\"}]"
