#!project begin
#!project end

using Test

# Some actual Julia code
function bar()
    println("hello")
end

#!manifest begin
#!manifest end

bar()
