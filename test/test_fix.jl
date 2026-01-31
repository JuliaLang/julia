# test_fix.jl

# 1. Test the Boolean Array case (The original bug)
v = [true, false]
result = +v
println("Original Bug Test:")
println("Input: $v (Eltype: $(eltype(v)))")
println("Output: $result (Eltype: $(eltype(result)))")

if eltype(result) == Int
    println("✅ SUCCESS: Bools turned into Ints!")
else
    println("❌ FAIL: Still Bools.")
end

println("-"^20)

# 2. Test the Consistency case (Mutation check)
v2 = [1, 2]
result2 = +v2
println("Consistency Test:")
println("Is the result the EXACT same object? ", result2 === v2)

if result2 !== v2
    println("✅ SUCCESS: Result is a copy (not the same object).")
else
    println("❌ FAIL: Result is the same object (Identity).")
end