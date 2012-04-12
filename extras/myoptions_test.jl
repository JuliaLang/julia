function simplefun(x,funcopts::Options)
#    @defaults funcopts a=3 b=2 c=2*b
    @defaults funcopts a=3 b=2
    println(a)
    println(b)
#    println(c)
    @check_options_used funcopts
end


function complexfun(x,opts::Options)
    println("Parent opts (before default):",opts)
    @defaults opts parent=3 both=7
    println("Parent opts (after default):",opts)
    println("Parent checkflag: ",_opts_checkflag)
    println(parent)
    println(both)
    subfun1(x,opts)
    println("Parent checkflag, round 2: ",_opts_checkflag)
    subfun2(x,opts)
    println("Parent opts (before check):",opts)
    @check_options_used opts
end

function subfun1(x,sub1opts::Options)
    println("sub1 opts (before default):",sub1opts)
    @defaults sub1opts sub1="sub1 default" both=0
    println("sub1 opts (after default):",sub1opts)
    println("sub1 checkflag: ",_sub1opts_checkflag)
    println(sub1)
    println(both)
    @check_options_used sub1opts
end

function subfun2(x,sub2opts::Options)
    println("sub2 opts (before default):",sub2opts)
    @defaults sub2opts sub2="sub2 default" both=22
    println("sub2 opts (after default):",sub2opts)
    println("sub2 checkflag: ",_sub2opts_checkflag)
    println(sub2)
    println(both)
    @check_options_used sub2opts
end

function twinopts(x,plotopts::Options,calcopts::Options)
    @defaults plotopts linewidth=1
    @defaults calcopts n_iter=100
    println(linewidth)
    println(n_iter)
    @check_options_used plotopts
    @check_options_used calcopts
end



# Testing
println("Simple cases:\n")
println("Substitute 5 for a:")
o = @options a=5
simplefun(77,o)
println(o)

println("\n\nAdditionally, substitute 7 for b:")
@add_options o b=7
simplefun(77,o)

println("\n\nAdd an unrecognized option d:")
@add_options o d=99
try
    simplefun(77,o)
    println("This should not have succeeded!!")
catch err
    println("This failed, as it should have. Here is the error message:")
    println(err)
end

println(o)

println("\n\nNow it should just yield a warning:")
o.check_behavior = OPTIONS_WARN
simplefun(77,o)

println("\n\nThis should run silently:")
o.check_behavior = OPTIONS_NONE
simplefun(77,o)



println("\n\nA more complex case:\n")
o = @options parent="Parent" both="both" sub2="sub2 override"
complexfun(55,o)
println("Note this worked even though the parent function doesn't check sub2")
println("\nIn contrast, this should fail:")
o = @options parent="Parent" both="both" sub2wrong="sub2 override"
println(o)
try
    complexfun(55,o)
    println("This should not have succeeded!!")
catch err
    println("This failed, as it should have. Here is the error message:")
    println(err)
end



println("\n\nA case with two options arguments:\n")
oplot = @options linewidth=2
ocalc = @options n_iter=1000
twinopts(99,oplot,ocalc)
println("This should fail:")
try
    twinopts(99,ocalc,oplot)
    println("This should not have succeeded!")
catch err
    println("This failed, as it should have. Here is the error message:",err)
end

