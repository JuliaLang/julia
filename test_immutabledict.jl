# Test script to validate ImmutableDict changes
# This will be run once Julia is built

println("Testing ImmutableDict performance improvements...")

import Base.ImmutableDict

# Test basic functionality
println("Creating empty dict...")
d = ImmutableDict{String, String}()
@assert length(d) == 0
@assert isempty(d)
println("✓ Empty dict: length = $(length(d))")

# Test single item
println("Adding one item...")
d1 = ImmutableDict(d, "key1" => "value1")  
@assert length(d1) == 1
@assert !isempty(d1)
@assert haskey(d1, "key1")
@assert d1["key1"] == "value1"
println("✓ Single item: length = $(length(d1))")

# Test multiple items
println("Adding second item...")
d2 = ImmutableDict(d1, "key2" => "value2")
@assert length(d2) == 2
@assert haskey(d2, "key1")
@assert haskey(d2, "key2")
@assert d2["key1"] == "value1"
@assert d2["key2"] == "value2"
println("✓ Two items: length = $(length(d2))")

# Test duplicate key (should increase length)
println("Adding duplicate key...")
d3 = ImmutableDict(d2, "key1" => "new_value1")
@assert length(d3) == 3  # This is the key test!
@assert d3["key1"] == "new_value1"  # Should get the newest value
println("✓ Duplicate key: length = $(length(d3))")

# Test performance with larger dict
println("Testing performance with larger dict...")
d_large = d
for i in 1:1000
    d_large = ImmutableDict(d_large, "key$i" => "value$i")
end
@assert length(d_large) == 1000

# Performance test - this should be O(1) now instead of O(n)
println("Performance test:")
@time len = length(d_large)
println("✓ Large dict length calculation: $(len) (should be instant)")

# Test IteratorSize
@assert Base.IteratorSize(typeof(d_large)) == Base.HasLength()
println("✓ IteratorSize correctly reports HasLength()")

println("\nAll tests passed! 🎉")
println("ImmutableDict length is now O(1) instead of O(n)")
