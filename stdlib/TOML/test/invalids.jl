# This file is a part of Julia. License is MIT: https://julialang.org/license

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

for str in ("a = {}\n[[a.b]]", "a = [{}]\n[[a.b]]", "a = {b = {}}\n[[a.b.c]]")
    err = tryparse(str)
    @test err isa ParserError
    @test err.type == Internals.ErrAddKeyToInlineTable
end
err = tryparse("a = 1\n[[a.b]]")
@test err isa ParserError
@test err.type == Internals.ErrKeyAlreadyHasValue

str = """
[a.b.c]
x = 1

[a]
b = 2
"""
err = tryparse(str)
@test err isa ParserError
@test err.type == Internals.ErrDuplicatedKey

end
