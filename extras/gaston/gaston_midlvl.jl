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


# append x,y,z coordinates and configuration to current figure
function addcoords(x::Coord,y::Coord,Z::Array,conf::CurveConf)
    global gnuplot_state

    # check that at least one figure has been setup
    if gnuplot_state.current == 0
        figure(1)
    end

    # check coordinates: dimensions, sizes and types
    nex = !isempty(x); ney = !isempty(y); neZ = !isempty(Z)
    # check types
    if nex
        assert(eltype(x)<:Real,"Invalid coordinates")
    end
    if ney
        assert(eltype(y)<:Real,"Invalid coordinates")
    end
    if neZ
        assert(eltype(Z)<:Real,"Invalid coordinates")
        assert(1 < ndims(Z) < 4,"Invalid coordinates")
    end
    # valid combinations of x,y,Z, where 0 means empty:
    #  x  y  Z
    #  1  1  0   # 2-d plot
    #  1  1  1   # 3-d plot
    #  0  0  1   # image
    assert((nex && ney) || (!nex && !ney && neZ), "Invalid coordinates")
    # if x,y are matrices, convert to vectors
    if nex
        if isa(x,Matrix)
            s = size(x)
            if s[1] == 1 || s[2] == 1
                x = squeeze(x)
            else
                error("Invalid abscissa coordinates")
            end
        elseif isa(x,Range1) || isa(x,Range)
            x = [x]
        end
    end
    if ney
        if isa(y,Matrix)
            s = size(y)
            if s[1] == 1 || s[2] == 1
                y = squeeze(y)
            else
                error("Invalid abscissa coordinates")
            end
        elseif isa(y,Range1) || isa(y,Range)
            y = [y]
        end
    end
    # check number of elements
    if nex && neZ
        assert(size(Z,1) == length(x),
        "Number of columns in 3-d coordinates must match length of abscissa")
        assert(size(Z,2) == length(y),
        "Number of rows in 3-d coordinates must match length of ordinate")
    end
    if nex && !neZ
        assert(length(x) == length(y),
        "Abscissa and ordinate must have the same number of elements")
    end

    # check curve configuration: property names
    # TODO: figure out how to check valid color names -- gnuplot supports
    #  112 different color names.
    # check valid values of marker
    assert(validate_marker(conf.marker), "Invalid mark name specified")
    if nex && !neZ ## 2-d plot
        # check valid values of plotstyle
        assert(validate_2d_plotstyle(conf.plotstyle),
            "Invalid plotstyle specified")
    elseif nex && neZ ## 3-d plot
        assert(validate_3d_plotstyle(conf.plotstyle),
            "Invalid plotstyle specified")
    else ## image
        assert(validate_image_plotstyle(conf.plotstyle),
            "Invalid plotstyle specified")
    end

    conf = copy(conf)       # we need to dereference conf
    # append data to figure
    c = findfigure(gnuplot_state.current)
    figs = gnuplot_state.figs
    if isempty(figs[c].curves[1].x)
        # figure() creates a structure with one empty curve; we want to
        # overwrite it with the first actual curve
        figs[c].curves[1] = CurveData(x,y,Z,conf)
    else
        figs[c].curves = [figs[c].curves, CurveData(x,y,Z,conf)]
    end
    gnuplot_state.figs = figs
end
addcoords(y) = addcoords(1:length(y),y,[],CurveConf())
addcoords(y,c::CurveConf) = addcoords(1:length(y),y,[],c)
addcoords(x,y) = addcoords(x,y,[],CurveConf())
addcoords(x,y,Z) = addcoords(x,y,Z,CurveConf())
addcoords(x,y,c::CurveConf) = addcoords(x,y,[],c)
# X, Y data in matrix columns
function addcoords(X::Matrix,Y::Matrix,conf::CurveConf)
    for i = 1:size(X,2)
        addcoords(X[:,i],Y[:,i],[],conf)
    end
end
function addcoords(Y::Matrix,conf::CurveConf)
    x = 1:size(Y,1)
    X = zeros(size(Y))
    for i = 1:size(Y,2)
        X[:,i] = x
    end
    addcoords(X,Y,conf)
end
addcoords(X::Matrix, Y::Matrix) = addcoords(X,Y,CurveConf())
addcoords(Y::Matrix) = addcoords(Y,CurveConf())

# append error data to current set of coordinates
function adderror(yl::Coord,yh::Coord)
    global gnuplot_state
    # check that at least one figure has been setup
    if gnuplot_state.current == 0
        figure(1)
    end
    c = findfigure(gnuplot_state.current)

    # check arguments and convert to vectors
    if isempty(yl)
        error("Invalid error data")
    else
        assert(eltype(yl)<:Real,"Invalid error data")
        if isa(yl,Matrix)
            s = size(yl)
            if s[1] == 1 || s[2] == 1
                yl = squeeze(yl)
            else
                error("Invalid error data")
            end
        elseif isa(yl,Range1) || isa(yl,Range)
            yl = [yl]
        end
    end
    if !isempty(yh)
        assert(eltype(yh)<:Real,"Invalid error data")
        if isa(yl,Matrix)
            s = size(yh)
            if s[1] == 1 || s[2] == 1
                yh = squeeze(yh)
            else
                error("Invalid error data")
            end
        elseif isa(yh,Range1) || isa(yh,Range)
            yh = [yh]
        end
    end
    # verify vector sizes -- this also implies that x,y coordinates must be
    # added to figure, before error data can be attached to it
    assert(length(gnuplot_state.figs[c].curves[end].x) == length(yl),
        "Error data vector must be of same size as abscissa")
    if !isempty(yh)
        assert(length(yh) == length(yl),
            "Error data vectors must be of same size")
    end

    # set fields in current curve
    gnuplot_state.figs[c].curves[end].ylow = yl
    gnuplot_state.figs[c].curves[end].yhigh = yh

end
adderror(ydelta) = adderror(ydelta,[])

# add axes configuration to current figure
function addconf(conf::AxesConf)
    global gnuplot_state
    # check that at least one figure has been setup
    if gnuplot_state.current == 0
        figure(1)
    end

    # Perform argument validation
    # TODO: find a way to validate the box argument
    # validate axis type
    assert(validate_axis(conf.axis),"Invalid axis type specified")

    conf = copy(conf)
    # select current plot
    c = findfigure(gnuplot_state.current)
    gnuplot_state.figs[c].conf = conf
end

# llplot() is our workhorse plotting function
function llplot()
    global gnuplot_state
    global gaston_config

    # select current plot
    c = findfigure(gnuplot_state.current)
    if c == 0
        println("No current figure")
        return
    end
    figs = gnuplot_state.figs
    config = figs[c].conf

    # Build terminal setup string and send it to gnuplot
    ts = termstring(gaston_config.terminal)
    gnuplot_send(ts)

    gnuplot_send("set autoscale")
    # legend box
    if config.box != ""
        gnuplot_send(strcat("set key ",config.box))
    end
    # plot title
    if config.title != ""
        gnuplot_send(strcat("set title '",config.title,"' "))
    end
    # xlabel
    if config.xlabel != ""
        gnuplot_send(strcat("set xlabel '",config.xlabel,"' "))
    end
    # ylabel
    if config.ylabel != ""
        gnuplot_send(strcat("set ylabel '",config.ylabel,"' "))
    end
    # zlabel
    if config.zlabel != ""
        gnuplot_send(strcat("set zlabel '",config.zlabel,"' "))
    end
    # axis log scale
    if config.axis != "" || config.axis != "normal"
        if config.axis == "semilogx"
            gnuplot_send("set logscale x")
        end
        if config.axis == "semilogy"
            gnuplot_send("set logscale y")
        end
        if config.axis == "loglog"
            gnuplot_send("set logscale xy")
        end
    end
    # datafile filename
    filename = strcat(string(gnuplot_state.tmpdir),"figure",string(c),".dat")
    # send coordinates, checking each special case
    # first check whether we are doing 2-d, 3-d or image plots
    # 2-d plot: x is not empty, Z is empty
    if isempty(figs[c].curves[1].Z) && !isempty(figs[c].curves[1].x)
        # create data file
        f = open(filename,"w")
        for i in figs[c].curves
            tmp = i.conf.plotstyle
            if tmp == "errorbars" || tmp == "errorlines"
                if isempty(i.yhigh)
                    # ydelta (single error coordinate)
                    dlmwrite(f,[i.x i.y i.ylow],' ')
                else
                    # ylow, yhigh (double error coordinate)
                    dlmwrite(f,[i.x i.y i.ylow i.yhigh],' ')
                end
            else
                dlmwrite(f,[i.x i.y],' ')
            end
            write(f,"\n\n")
        end
        close(f)
        # send command to gnuplot
        gnuplot_send(linestr(figs[c].curves, "plot", filename,""))
        # 3-d plot: x is not empty, Z is not empty
    elseif !isempty(figs[c].curves[1].Z) && !isempty(figs[c].curves[1].x)
        # create data file
        f = open(filename,"w")
        for i in figs[c].curves
            # nonuniform matrix -- see gnuplot 4.6 manual, p. 169
            write(f,"0 ")
            dlmwrite(f,[i.x]',' ');
            for y = 1:length(i.y)
                dlmwrite(f,[i.y[y] i.Z[:,y]'],' ')
            end
            write(f,"\n\n")
        end
        close(f)
        # send command to gnuplot
        gnuplot_send(linestr(figs[c].curves, "splot",filename,
            "nonuniform matrix"))
        # image plot: plotstyle is "image" or "rgbimage"
    elseif figs[c].curves[1].conf.plotstyle == "image" ||
            figs[c].curves[1].conf.plotstyle == "rgbimage"
        # create data file
        f = open(filename,"w")
        # assume there is only one image per figure
        if figs[c].curves[1].conf.plotstyle == "image"
            # output matrix
            dlmwrite(f,figs[c].curves[1].Z,' ')
            close(f)
            # send command to gnuplot
            gnuplot_send("set yrange [*:*] reverse")  # flip y axis
            gnuplot_send(linestr(figs[c].curves,"plot",filename,"matrix"))
        end
        if figs[c].curves[1].conf.plotstyle == "rgbimage"
            # output matrix
            Z = figs[c].curves[1].Z
            y = 1.0:size(Z,2)
            for i = 1:size(Z,1)
                c1 = i*ones(length(y))
                r = slicedim(Z,3,1)[i,:]
                g = slicedim(Z,3,2)[i,:]
                b = slicedim(Z,3,3)[i,:]
                dlmwrite(f,hcat(c1,y,r',g',b'),' ')
            end
            close(f)
            # send command to gnuplot
            gnuplot_send("set yrange [*:*] reverse")  # flip y axis
            gnuplot_send(linestr(figs[c].curves,"plot",filename,""))
        end
    end
    gnuplot_send("reset")
end
