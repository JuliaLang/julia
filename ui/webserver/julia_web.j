###########################################
# plotting functions
###########################################

# number of points to plot for functions
__PLOT_POINTS = 450

# the aspect ratio of plots
__PLOT_ASPECT_RATIO = 1.95

# plot an array (window determined manually)
function plot(x, y, xmin, xmax, ymin, ymax)
    # make sure there are the same number of x and y coordinates
    if length(x) != length(y)
        return error("size of x and y arrays must be equal")
    end

    # make sure there is enough data to plot
    if length(x) < 1
        return error("at least two data points required for plot")
    end

    # make the plot
    __write_message(__Message(__MSG_OUTPUT_PLOT, {
        "line",
        strcat("[", join([string(float64(i)) | i=x], ","), "]"),
        strcat("[", join([string(float64(i)) | i=y], ","), "]"),
        string(float64(xmin)),
        string(float64(xmax)),
        string(float64(ymin)),
        string(float64(ymax))
    }))
end

# plot an array (window determined automatically)
function plot(x, y)
    # make sure there are the same number of x and y coordinates
    if length(x) != length(y)
        return error("size of x and y arrays must be equal")
    end

    # make sure there is enough data to plot
    if length(x) < 1
        return error("at least two data points required for plot")
    end

    # make the plot
    xmin = min(x)
    xmax = max(x)
    ymin = min(y)
    ymax = max(y)
    if abs((ymax-ymin)/(xmax-xmin)-1) < 0.05
        cx = (xmax+xmin)/2.0
        cy = (ymax+ymin)/2.0
        w = (ymax-ymin)/1.95
        plot(x, y, cx-w*__PLOT_ASPECT_RATIO, cx+w*__PLOT_ASPECT_RATIO, cy-w, cy+w)
    else
        plot(x, y, xmin, xmax, ymin-(ymax-ymin)*0.05, ymax+(ymax-ymin)*0.05)
    end
end

# plot an array (window determined automatically)
function plot(y)
    # make sure there is enough data to plot
    if length(y) < 1
        return error("at least two data points required for plot")
    end

    # make the plot
    x = [float64(i-1)/(length(y)-1) | i=1:length(y)]
    xmin = 0.0
    xmax = 1.0
    ymin = min(y)
    ymax = max(y)
    if abs((ymax-ymin)/(xmax-xmin)-1) < 0.05
        cx = (xmax+xmin)/2.0
        cy = (ymax+ymin)/2.0
        w = (ymax-ymin)/1.95
        plot(x, y, cx-w*__PLOT_ASPECT_RATIO, cx+w*__PLOT_ASPECT_RATIO, cy-w, cy+w)
    else
        plot(x, y, xmin, xmax, ymin-(ymax-ymin)*0.05, ymax+(ymax-ymin)*0.05)
    end
end

# plot a function (vertical window determined automatically)
function plot(f, xmin, xmax)
    # make the range
    x = [xmin+float64(i-1)*xmax/(__PLOT_POINTS-1) | i=1:__PLOT_POINTS]
    y = [f(i) | i=x]

    # make the plot
    plot(x, y)
end

# plot a function (window determined manually)
function plot(f, xmin, xmax, ymin, ymax)
    # make the range
    x = [xmin+float64(i-1)*xmax/(__PLOT_POINTS-1) | i=1:__PLOT_POINTS]
    y = [f(i) | i=x]

    # make the plot
    plot(x, y, xmin, xmax, ymin, ymax)
end