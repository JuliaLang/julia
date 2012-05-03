## Copyright (c) 2012 Miguel Bazdresch
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.

# load files
load("gaston_types.jl")
load("gaston_aux.jl")
load("gaston_lowlvl.jl")
load("gaston_midlvl.jl")
load("gaston_hilvl.jl")
load("gaston_config.jl")

# set up global variables
# global variable that stores gnuplot's state
gnuplot_state = GnuplotState(false,0,0,strcat("/tmp/gaston-",getenv("USER"),
    "-",randstring(5),"/"),[])
# when gnuplot_state goes out of scope, close the pipe
finalizer(gnuplot_state,gnuplot_exit)

# global variable that stores Gaston's configuration
gaston_config = GastonConfig()
