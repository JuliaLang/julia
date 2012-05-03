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

# write commands to gnuplot's pipe
function gnuplot_send(s::String)
    fid = gnuplot_state.fid
    err = ccall(:fputs, Int, (Ptr{Uint8},Ptr), strcat(s,"\n"), fid)
    # fputs returns a positive number if everything worked all right
    if err < 0
        println("Something went wrong writing to the gnuplot pipe.")
        return
    end
    err = ccall(:fflush, Int, (Ptr,), fid)
    ## fflush returns 0 if everything worked all right
    if err != 0
        println("Something went wrong writing to the gnuplot pipe.")
        return
    end
end
