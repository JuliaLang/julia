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

# initialize
# The way to interface to gnuplot is by setting up a pipe that gnuplot
# reads commands from. I don't see how to create such a 'persistent'
# pipe from Julia, so we have to use libc's 'popen' call.
function gnuplot_init()
    global gnuplot_state
    f = ccall(:popen, Ptr, (Ptr{Uint8},Ptr{Uint8}), "gnuplot" ,"w")
    if f == 0
        println("There was a problem starting up gnuplot.")
        return
    else
        gnuplot_state.running = true
        gnuplot_state.fid = f
    end
end

# close gnuplot pipe
function gnuplot_exit(x...)
    global gnuplot_state
    if gnuplot_state.running
        # close pipe
        err = ccall(:pclose, Int, (Ptr,), gnuplot_state.fid)
        # err should be zero
        if err != 0
            println("Gnuplot may not have closed correctly.");
        end
    end
    # reset gnuplot_state
    gnuplot_state.running = false
    gnuplot_state.current = 0
    gnuplot_state.fid = 0
    gnuplot_state.figs = []
end

# return a random string (for filenames)
function randstring(len::Int)
    const cset = char([0x30:0x39,0x41:0x5a,0x61:0x7a])
    const strset = convert(String,strcat(cset...))
    index = int(ceil(strlen(strset)*rand(len)))
    s = strset[index]
    return s
end

# Return index to figure with handle 'c'. If no such figure exists, returns 0.
function findfigure(c)
    global gnuplot_state
    i = 0
    for j = 1:length(gnuplot_state.figs)
        if gnuplot_state.figs[j].handle == c
            i = j
            break
        end
    end
    return i
end

# convert marker string description to gnuplot's expected number
function pointtype(x::String)
    if x == "+"
        return 1
    elseif x == "x"
        return 2
    elseif x == "*"
        return 3
    elseif x == "esquare"
        return 4
    elseif x == "fsquare"
        return 5
    elseif x == "ecircle"
        return 6
    elseif x == "fcircle"
        return 7
    elseif x == "etrianup"
        return 8
    elseif x == "ftrianup"
        return 9
    elseif x == "etriandn"
        return 10
    elseif x == "ftriandn"
        return 11
    elseif x == "edmd"
        return 12
    elseif x == "fdmd"
        return 13
    end
    return 1
end

# return configuration string for a single plot
function linestr_single(conf::CurveConf)
    s = ""
    if conf.legend != ""
        s = strcat(s, " title '", conf.legend, "' ")
    else
        s = strcat(s, "notitle ")
    end
    s = strcat(s, " with ", conf.plotstyle, " ")
    if conf.color != ""
        s = strcat(s, "linecolor rgb '", conf.color, "' ")
    end
    s = strcat(s, "lw ", string(conf.linewidth), " ")
    # some plotstyles don't allow point specifiers
    cp = conf.plotstyle
    if cp != "lines" && cp != "impulses" && cp != "pm3d" && cp != "image" &&
        cp != "rgbimage" && cp != "boxes"
        if conf.marker != ""
            s = strcat(s, "pt ", string(pointtype(conf.marker)), " ")
        end
        s = strcat(s, "ps ", string(conf.pointsize), " ")
    end
    return s
end

# build a string with plot commands according to configuration
function linestr(curves::Vector{CurveData}, cmd::String, file::String,
    postcmd::String)
    # We have to insert "," between plot commands. One easy way to do this
    # is create the first plot command, then the rest
    # We also need to keep track of the current index (starts at zero)
    index = 0
    s = strcat(cmd, " '", file, "' ", postcmd, " i 0 ",
        linestr_single(curves[1].conf))
    if length(curves) > 1
        for i in curves[2:end]
            index += 1
            s = strcat(s, ", '", file, "' ", postcmd, " i ", string(index)," ",
                linestr_single(i.conf))
        end
    end
    return s
end

# create a Z-coordinate matrix from x, y coordinates and a function
function meshgrid(x,y,f)
    Z = zeros(length(x),length(y))
    for k = 1:length(x)
        Z[k,:] = [ f(i,j) | i=x[k], j=y ]
    end
    return Z
end

# create x,y coordinates for a histogram, from a sample vector, using a number
# of bins
function histdata(s,bins)
    # When adding an element s to a bin, we use an iequality m < s <= M.
    # In order to account for elements s==m, we need to artificially reduce
    # m a tiny bit. Note that this cannot be changed by using other
    # inequalities:
    # m <= s < M -- we'd have to increase M to account for s==M
    # m <= s <= M -- we'd risk adding s to more than one bin
    ms = min(s)-eps()
    Ms = max(s)
    delta = (Ms-ms)/bins
    x = ms:delta:Ms
    y = zeros(numel(x))
    for i in 1:numel(x)-1
        y[i] = sum(x[i] < s <= x[i+1])
    end
    # We want the left bin to start at ms and the right bin to end at Ms
    x = x+delta/2
    return x,y
end

# dereference CurveConf, by adding a method to copy()
function copy(conf::CurveConf)
    new = CurveConf()
    new.legend = conf.legend
    new.plotstyle = conf.plotstyle
    new.color = conf.color
    new.marker = conf.marker
    new.linewidth = conf.linewidth
    new.pointsize = conf.pointsize
    return new
end

# dereference AxesConf
function copy(conf::AxesConf)
    new = AxesConf()
    new.title = conf.title
    new.xlabel = conf.xlabel
    new.ylabel = conf.ylabel
    new.zlabel = conf.zlabel
    new.box = conf.box
    new.axis = conf.axis
    return new
end

# Build a "set term" string appropriate for the terminal type
function termstring(term::String)
    global gnuplot_state
    global gaston_config

    c = findfigure(gnuplot_state.current)
    if is_term_screen(term)
        ts = strcat("set term ", term, " ", string(c))
    else
        ts = strcat("set term ", term, "\nset output ", "\"",
            gaston_config.outputfile, "\"")
    end
    return ts
end

# Validation functions.
# These functions validate that configuration parameters are valid and
# supported. They return true iff the argument validates.

# Validate terminal type.
function validate_terminal(s::String)
    supp_terms = ["wxt", "x11", "svg", "gif"]
    if contains(supp_terms, s)
        return true
    end
    return false
end

# Identify terminal by type: file or screen
function is_term_screen(s::String)
    screenterms = ["wxt", "x11"]
    if contains(screenterms, s)
        return true
    end
    return false
end

function is_term_file(s::String)
    screenterms = ["svg", "gif"]
    if contains(screenterms, s)
        return true
    end
    return false
end

# Valid plotstyles supported by gnuplot's plot
function validate_2d_plotstyle(s::String)
    valid = ["lines", "linespoints", "points", "impulses", "boxes",
        "errorlines", "errorbars"]
    if contains(valid, s)
        return true
    end
    return false
end

# Valid plotstyles supported by gnuplot's splot
function validate_3d_plotstyle(s::String)
    valid = ["lines", "linespoints", "points", "impulses", "pm3d"]
    if contains(valid, s)
        return true
    end
    return false
end

# Valid plotstyles supported by gnuplot's plot with images
function validate_image_plotstyle(s::String)
    valid = ["image", "rgbimage"]
    if contains(valid, s)
        return true
    end
    return false
end

# Valid axis types
function validate_axis(s::String)
    valid = ["", "normal", "semilogx", "semilogy", "loglog"]
    if contains(valid,s)
        return true
    end
    return false
end

# Valid markers supported by Gaston
function validate_marker(s::String)
    valid = ["", "+", "x", "*", "esquare", "fsquare", "ecircle", "fcircle",
    "etrianup", "ftrianup", "etriandn", "ftriandn", "edmd", "fdmd"]
    if contains(valid, s)
        return true
    end
    return false
end
