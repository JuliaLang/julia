function simplefun(x,opts::Options)
    @defaults opts a=3 b=2 c=2*b
    println(a)
    println(b)
    println(c)
    @check_options_used opts
end


function complexfun(x,opts::Options)
    @defaults opts parent=3 both=7
    println(parent)
    println(both)
    subfun1(x,opts)
    subfun2(x,opts)
    @check_options_used opts
end

function subfun1(x,opts::Options)
    @defaults opts sub1="Default sub1" both=0
    println(sub1)
    println(both)
    @check_options_used opts
end

function subfun2(x,opts::Options)
    @defaults opts sub2=-5 both=22
    println(sub2)
    println(both)
    @check_options_used opts
end


# Testing
println("Simple cases:\n")
println("Substitute 5 for a:")
o = @options a=5
simplefun(77,copy(o))
println(o)

println("\n\nAdditionally, substitute 7 for b:")
@add_options o b=7
simplefun(77,copy(o))

println("\n\nAdd an unrecognized option d:")
@add_options o d=99
try
    simplefun(77,copy(o))
    println("This should not have succeeded!!")
catch err
    println("This failed, as it should have. Here is the error message:")
    println(err)
end

println(o)

println("\n\nNow it should just yield a warning:")
o.check_behavior = OPTIONS_WARN
simplefun(77,copy(o))

println("\n\nThis should run silently:")
o.check_behavior = OPTIONS_NONE
simplefun(77,copy(o))



println("\n\nA more complex case:\n")
o = @options parent="Parent" both="both" sub2="sub2 override"
complexfun(55,copy(o))
println("Note this worked even though the parent function doesn't check sub2")
println("\nIn contrast, this should fail:")
o = @options parent="Parent" both="both" sub2wrong="sub2 override"
try
    complexfun(55,copy(o))
    println("This should not have succeeded!!")
catch err
    println("This failed, as it should have. Here is the error message:")
    println(err)
end
