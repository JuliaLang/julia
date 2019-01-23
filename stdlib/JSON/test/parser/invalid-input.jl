const FAILURES = [
    # Unexpected character in array
    "[1,2,3/4,5,6,7]",
    # Unexpected character in object
    "{\"1\":2, \"2\":3 _ \"4\":5}",
    # Invalid escaped character
    "[\"alpha\\α\"]",
    "[\"\\u05AG\"]",
    # Invalid 'simple' and 'unknown value'
    "[tXXe]",
    "[fail]",
    "∞",
    # Invalid number
    "[5,2,-]",
    "[5,2,+β]",
    # Incomplete escape
    "\"\\",
    # Control character
    "\"\0\"",
    # Issue #99
    "[\"🍕\"_\"🍕\"",
    # Issue #260
    "1997-03-03",
    "1997.1-",
]

@testset for fail in FAILURES
    # Test memory parser
    @test_throws ErrorException JSON.parse(fail)

    # Test streaming parser
    @test_throws ErrorException JSON.parse(IOBuffer(fail))
end
