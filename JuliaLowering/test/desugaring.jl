@testset "Desugaring" begin

test_mod = Module(:TestMod)

# @test desugar(test_mod, """
# let
#     y = 0
#     x = 1
#     let x = x + 1
#         y = x
#     end
#     (x, y)
# end
# """) ~ @ast_ [K"block"
#     [K"block"
#         [K"="
#             "y"::K"Identifier"
#             0::K"Integer"
#         ]
#         [K"="
#             "x"::K"Identifier"
#             1::K"Integer"
#         ]
#         [K"block"
#             [K"="
#                 1::K"BindingId"
#                 [K"call"
#                     "+"::K"Identifier"
#                     "x"::K"Identifier"
#                     1::K"Integer"
#                 ]
#             ]
#             [K"block"
#                 [K"local_def"
#                     "x"::K"Identifier"
#                 ]
#                 [K"="
#                     "x"::K"Identifier"
#                     1::K"BindingId"
#                 ]
#                 [K"block"
#                     [K"="
#                         "y"::K"Identifier"
#                         "x"::K"Identifier"
#                     ]
#                 ]
#             ]
#         ]
#         [K"call"
#             "tuple"::K"core"
#             "x"::K"Identifier"
#             "y"::K"Identifier"
#         ]
#     ]
# ]

end
