#! /bin/bash
# Instructions
# Create a .jl file that looks like this:
#        function test()
#           do some stuff, perhaps return a value
#        end
#        @assert test() == 5   # 5 is the correct answer if all is working
#        exit()
# Then provide the name of that function after the "julia" call below
make clean   # or cleanall if necessary
make
julia /tmp/test.jl
exit $?
