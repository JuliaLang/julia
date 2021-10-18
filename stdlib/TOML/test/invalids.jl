@testset "errors" begin

str = """
[foo]
bar = 3

[foo]
quiz = 3
"""

err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrDuplicatedKey

str = """
[[foo.bar]]

[foo]
bar = 2
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrKeyAlreadyHasValue

str = """
[[foo.bar]]

[foo.bar]
q = 3
"""
err = tryparse(str)
@test err isa ParserError

end
