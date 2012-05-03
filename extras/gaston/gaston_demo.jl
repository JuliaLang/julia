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

function demo()
    closeall()

    t = -2:0.01:2
    st = sin(10pi*t)
    ct = cos(10pi*t)
    et = exp(abs(t/10))

    # simplest figure
    figure(1)
    addcoords(st)
    llplot()

    # with x coordinates
    figure(2)
    addcoords(t,st)
    llplot()

    # plot configuration
    figure(3)
    c = CurveConf()
    c.legend = "Sinusoidal"
    c. plotstyle = "points"
    c.color = "blue"
    c.marker = "fdmd"
    c.pointsize = 2
    c.linewidth = 1.5
    addcoords(t,st,c)
    llplot()

    # figure configuration
    figure(4)
    a = AxesConf()
    a.title = "Example of figure configuration"
    a.xlabel = "Time (s)"
    a.ylabel = "Amplitude"
    a.box = "bottom left"
    a.axis = "semilogx"
    c.plotstyle = "linespoints"
    addcoords(t,st,c)
    addconf(a)
    llplot()

    # multiple plots
    figure(5)
    c = CurveConf()
    c.legend = "Sin"
    c.color = "black"
    addcoords(t,st,c)
    c.legend = "Cos"
    c.color = "magenta"
    c.plotstyle = "impulses"
    c.linewidth = 0.4
    addcoords(t,ct,c)
    c.legend = "Exp"
    c.color = "red"
    c.plotstyle = "linespoints"
    addcoords(t,et,c)
    a = AxesConf()
    a.xlabel = "Time (s)"
    a.ylabel = "Amplitude"
    a.title = "Multiple plots in the same figure"
    a.box = "outside top horizontal box"
    addconf(a)
    llplot()

    # error bars with ydelta
    y = exp(-(1:.1:4.9))
    figure(6)
    c = CurveConf()
    c.legend = "Random"
    c.plotstyle = "errorbars"
    addcoords(1:40,y,c)
    adderror(0.1*rand(40))
    a = AxesConf()
    a.title = "Error bars (ydelta)"
    addconf(a)
    llplot()

    # error bars with ylow, yhigh
    figure(7)
    c = CurveConf()
    c.legend = "Random"
    c.plotstyle = "errorbars"
    ylow = y - 0.05*rand(40);
    yhigh = y + 0.05*rand(40);
    addcoords(1:40,y,c)
    ylow = y - 0.05*rand(40);
    yhigh = y + 0.05*rand(40);
    adderror(ylow,yhigh)
    a = AxesConf()
    a.title = "Error bars (ylow, yhigh)"
    addconf(a)
    llplot()

    # error lines
    figure(8)
    c = CurveConf()
    c.legend = "Random"
    c.plotstyle = "errorlines"
    addcoords(1:40,y,c)
    adderror(0.1*rand(40))
    a = AxesConf()
    a.title = "Error lines (ydelta)"
    addconf(a)
    llplot()

    # plotting columns of matrices
    figure(9)
    Y = hcat(st, ct, et)
    X = hcat(t, t, t)
    addcoords(X,Y)
    a = AxesConf()
    a.title = "Plotting matrix columns"
    addconf(a)
    llplot()

    # simple 3-D plot with default config
    figure(10)
    x=[0,1,2,3]
    y=[0,1,2]
    Z=[10 10 10; 10 5 10;10 1 10; 10 0 10]
    addcoords(x,y,Z)
    a = AxesConf()
    a.title = "3D: Valley of the Gnu from gnuplot manual"
    addconf(a)
    llplot()

    # same plot with colored surfaces
    figure(11)
    c = CurveConf()
    c.plotstyle = "pm3d"
    addcoords(x,y,Z,c)
    a = AxesConf()
    a.title = "3D: Valley of the Gnu with pm3d"
    addconf(a)
    llplot()

    # sombrero
    figure(12)
    c = CurveConf()
    c.plotstyle = "pm3d"
    x = -15:0.33:15
    y = -15:0.33:15
    Z = meshgrid(x,y,(x,y)->sin(sqrt(x.*x+y.*y))/sqrt(x.*x+y.*y))
    addcoords(x,y,Z,c)
    a = AxesConf()
    a.title = "3D: Sombrero"
    addconf(a)
    llplot()

    # simple image
    figure(13)
    c = CurveConf()
    c.plotstyle = "image"
    Z = [5 4 3 1 0; 2 2 0 0 1; 0 0 0 1 0; 0 1 2 4 3]
    addcoords([],[],Z,c)
    a = AxesConf()
    a.title = "Image"
    addconf(a)
    llplot()

    # rgb image
    figure(14)
    c = CurveConf()
    c.plotstyle = "rgbimage"
    R = [ x+y | x=0:5:120, y=0:5:120]
    G = [ x+y | x=0:5:120, y=120:-5:0]
    B = [ x+y | x=120:-5:0, y=0:5:120]
    Z = zeros(25,25,3)
    Z[:,:,1] = R
    Z[:,:,2] = G
    Z[:,:,3] = B
    addcoords([],[],Z,c)
    a = AxesConf()
    a.title = "RGB Image"
    addconf(a)
    llplot()

    # histograms
    figure(15)
    c = CurveConf()
    c.plotstyle = "boxes"
    c.color = "blue"
    y = [1 2 3 4 5 6 7 8 9 10]
    (x,y) = histdata(y,10)
    addcoords(x,y,c)
    a = AxesConf()
    a.title = "Simple histogram test"
    addconf(a)
    llplot()

    figure(16)
    c = CurveConf()
    c.plotstyle = "boxes"
    c.color = "blue"
    c.legend = "1000 samples"
    (x,y) = histdata(randn(1000),25)
    delta = x[2]-x[1]
    y = y/(delta*sum(y))  # normalization
    addcoords(x,y,c)
    x = -5:0.05:5
    y = 1/sqrt(2pi)*exp((-x.^2)/2)
    c = CurveConf()
    c.plotstyle = "lines"
    c.color = "black"
    c.legend = "Theoretical"
    addcoords(x,y,c)
    a = AxesConf()
    a.title = "Histogram"
    addconf(a)
    llplot()

end
