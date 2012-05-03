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

## This file contains "high-level" plotting functions, similar to Octave's.

# figure handling
# Close a figure, or current figure.
# Returns the handle of the figure that was closed.
function closefigure(x...)
    global gnuplot_state
    global gaston_config

    term = gaston_config.terminal
    # create vector of handles
    handles = []
    if gnuplot_state.current != 0
        for i in gnuplot_state.figs
            handles = [handles, i.handle]
        end
    end
    if isempty(x)
        # close current figure
        h = gnuplot_state.current
    else
        h = x[1]
    end
    if contains(handles,h)
        # only care about closing windows if term type is screen
        if is_term_screen(term)
            if gnuplot_state.running
                gnuplot_send(
                    strcat("set term ", term, " ", string(h), " close"))
            end
        end
        # delete all data related to this figure
        _figs = []
        for i in gnuplot_state.figs
            if i.handle != h
                _figs = [_figs, i]
            end
        end
        gnuplot_state.figs = _figs
        # update state
        if isempty(gnuplot_state.figs)
            # we just closed the last figure
            gnuplot_state.current = 0
        else
            # select the most-recently created figure
            gnuplot_state.current = gnuplot_state.figs[end].handle
        end
    else
        println("No such figure exists");
        h = 0
    end
    return h
end

# close all figures
function closeall()
    global gnuplot_state

    try
        for i in gnuplot_state.figs
            closefigure()
        end
    catch
    end
end

# Select or create a figure. When called with no arguments, create a new
# figure. Figure handles must be natural numbers.
# Returns the current figure handle.
function figure(x...)
    global gnuplot_state
    global gaston_config

    term = gaston_config.terminal

    # check arguments
    if !isempty(x)
        # assert x[1] is a natural integer
        assert((x[1] > 0) && (isa(x[1],Int)),
            "Figure handle must be a natural number.")
        # assert x contains a single value
        assert(length(x) == 1,"figure() argument must be a single number")
    end

    # see if we need to set up gnuplot
    if gnuplot_state.running == false
        gnuplot_init();
    end
    # create vector of handles, needed later
    handles = []
    for i in gnuplot_state.figs
        handles = [handles, i.handle]
    end
    # determine figure handle
    if gnuplot_state.current == 0
        if isempty(x)
            h = 1
        else
            h = x[1]
        end
    else
        if isempty(x)
            # use lowest numbered handle available
            for i = 1:max(handles)+1
                if !contains(handles,i)
                    h = i
                    break
                end
            end
        else
            h = x[1]
        end
    end
    # if figure with handle h exists, replot it; otherwise create it
    ts = termstring(gaston_config.terminal)
    gnuplot_send(ts)
    gnuplot_state.current = h
    if !contains(handles,h)
        gnuplot_state.figs = [gnuplot_state.figs, Figure(h)]
    else
        llplot()
    end
    return h
end

# 2-d plots
function plot(args...)
    global gnuplot_state
    # if args[1] is an integer, it's the function handle.
    if isa(args[1], Int)
        h = args[1]
        args = args[2:end]   # argument parsing starts with 1 (eases debug)
    else
        h = gnuplot_state.current
    end
    if h == 0
        h = figure()     # new figure
    else
        closefigure(h)
        figure(h)    # overwrite specific figure
    end
    # parse arguments
    state = "SINI"
    la = length(args)
    while(true)
        if state == "SINI"
            i = 1
            cc = CurveConf()
            ac = AxesConf()
            state = "S1"
        elseif state == "S1"
            if i > la
                state = "SERROR"
                continue
            end
            y = args[i]
            i = i+1
            state = "S2"
        elseif state == "S2"
            if i > la
                addcoords(y,cc)
                state = "SEND"
                continue
            end
            if isa(args[i], String)
                x = 1:length(y)
                state = "S4"
            else
                x = y
                y = args[i]
                i = i+1
                state = "S3"
            end
        elseif state == "S3"
            if i > la
                addcoords(x,y,cc)
                state = "SEND"
                continue
            end
            if isa(args[i], String)
                state = "S4"
            else
                addcoords(x,y,cc)
                cc = CurveConf()
                y = args[i]
                i = i+1
                state = "S2"
            end
        elseif state == "S4"
            if i+1 > la
                state = "SERROR"
                continue
            end
            ai = args[i]; ai1 = args[i+1]
            if ai == "legend"
                cc.legend = ai1
            elseif ai == "plotstyle"
                cc.plotstyle = ai1
            elseif ai == "color"
                cc.color = ai1
            elseif ai == "marker"
                cc.marker = ai1
            elseif ai == "linewidth"
                cc.linewidth = ai1
            elseif ai == "pointsize"
                cc.pointsize = ai1
            elseif ai == "title"
                ac.title = ai1
            elseif ai == "xlabel"
                ac.xlabel = ai1
            elseif ai == "ylabel"
                ac.ylabel = ai1
            elseif ai == "box"
                ac.box = ai1
            elseif ai == "axis"
                ac.axis = ai1
            else
                error("Invalid property specified")
            end
            i = i+2
            state = "S3"
        elseif state == "SEND"
            addconf(ac)
            llplot()
            break
        elseif state == "SERROR"
            error("Invalid arguments")
        else
            error("Unforseen situation, bailing out")
        end
    end
    return h
end

function histogram(args...)
    global gnuplot_state
    # if args[1] is an integer, it's the function handle.
    if isa(args[1], Int)
        h = args[1]
        args = args[2:end]   # argument parsing starts with 1 (eases debug)
    else
        h = gnuplot_state.current
    end
    if h == 0
        h = figure()     # new figure
    else
        closefigure(h)
        figure(h)    # overwrite specific figure
    end
    # parse arguments
    state = "SINI"
    la = length(args)
    while(true)
        if state == "SINI"
            i = 1
            bins::Int = 10
            norm::Int = 0
            cc = CurveConf()
            cc.plotstyle = "boxes"
            ac = AxesConf()
            state = "S1"
        elseif state == "S1"
            if i > la
                state = "SERROR"
                continue
            end
            y = args[i]
            i = i+1
            state = "S2"
        elseif state == "S2"
            if i > la
                # validate bins and norm
                if bins <= 0 || norm < 0
                    state = "SERROR"
                    continue
                end
                (x,y) = histdata(y,bins)
                if norm != 0
                    delta = x[2] - x[1]
                    y = norm*y/(delta*sum(y))
                end
                addcoords(x,y,cc)
                state = "SEND"
                continue
            end
            state = "S3"
        elseif state == "S3"
            if i+1 > la
                state = "SERROR"
                continue
            end
            ai = args[i]; ai1 = args[i+1]
            if ai == "legend"
                cc.legend = ai1
            elseif ai == "color"
                cc.color = ai1
            elseif ai == "linewidth"
                cc.linewidth = ai1
            elseif ai == "bins"
                bins = ai1
            elseif ai == "norm"
                norm = ai1
            elseif ai == "title"
                ac.title = ai1
            elseif ai == "xlabel"
                ac.xlabel = ai1
            elseif ai == "ylabel"
                ac.ylabel = ai1
            elseif ai == "box"
                ac.box = ai1
            else
                error("Invalid property specified")
            end
            i = i+2
            state = "S2"
        elseif state == "SEND"
            addconf(ac)
            llplot()
            break
        elseif state == "SERROR"
            error("Invalid arguments")
        else
            error("Unforseen situation, bailing out")
        end
    end
    return h
end
