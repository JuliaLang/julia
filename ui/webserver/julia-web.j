###########################################
# protocol
###########################################

###### the julia<-->server protocol #######

# the message type is sent as a byte
# the next byte indicates how many arguments there are
# each argument is four bytes indicating the size of the argument, then the data for that argument

###### the server<-->browser protocol #####

# messages are sent as json arrays
# [message_type:number, arg0:string, arg1:string, ...]

# import the message types
load("./ui/webserver/message_types.h")

###########################################
# set up the socket connection
###########################################

# open a socket on any port
__ports = [int16(4444)]
__sockfd = ccall(:open_any_tcp_port, Int32, (Ptr{Int16},), __ports)
if __sockfd == -1
    # couldn't open the socket
    println("could not open server socket on port 4444.")
    exit()
end

# print the socket number so the server knows what it is
println(__ports[1])

# wait for the server to connect to the socket
__connectfd = ccall(dlsym(libc, :accept), Int32, (Int32, Ptr{Void}, Ptr{Void}), __sockfd, C_NULL, C_NULL)

# create an io object from the file descriptor
__io = fdio(__connectfd)

###########################################
# protocol implementation
###########################################

# a message
type __Message
    msg_type::Uint8
    args::Array{Any, 1}
end

# read a message
function __read_message()
    msg_type = read(__io, Uint8)
    args = {}
    num_args = read(__io, Uint8)
    for i=1:num_args
        arg_length = read(__io, Uint32)
        arg = ASCIIString(read(__io, Uint8, arg_length))
        push(args, arg)
    end
    return __Message(msg_type, args)
end

# send a message
function __write_message(msg)
    write(__io, uint8(msg.msg_type))
    write(__io, uint8(length(msg.args)))
    for arg=msg.args
        write(__io, uint32(length(arg)))
        write(__io, arg)
    end
    flush(__io)
end

# print a message (useful for debugging)
function __print_message(msg)
    print(msg.msg_type)
    print(": [ ")
    for arg=msg.args
        print("\"")
        print(arg)
        print("\" ")
    end
    println("]")
end

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
    plot(x, y, 0, 1, ymin-(ymax-ymin)*0.05, ymax+(ymax-ymin)*0.05)
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

###########################################
# input event handler
###########################################

# store the result of the previous input
ans = nothing

# callback for that event handler
function __socket_callback(fd)
    # keep track of the previous result
    global ans

    # read the message
    __msg = __read_message()

    # MSG_INPUT_EVAL
    if __msg.msg_type == __MSG_INPUT_EVAL
        # try to parse it
        __expr = parse_input_line(__msg.args[1])

        # if there was nothing to parse, send nothing back
        if __expr == nothing
            __write_message(__Message(__MSG_OUTPUT_PARSE_COMPLETE, {}))
            return __write_message(__Message(__MSG_OUTPUT_EVAL_RESULT, {""}))
        end
        
        # check if there was a parsing error
        if __expr.head == :error
            return __write_message(__Message(__MSG_OUTPUT_PARSE_ERROR, {__expr.args[1]}))
        end

        # check if the expression was incomplete
        if __expr.head == :continue
            return __write_message(__Message(__MSG_OUTPUT_PARSE_INCOMPLETE, {}))
        end
        
        # tell the browser that we're now evaluating the expression
        __write_message(__Message(__MSG_OUTPUT_PARSE_COMPLETE, {}))

        # evaluate the expression and print any exceptions that occurred
        local __result
        try
            __result = eval(__expr)
        catch __error
            return __write_message(__Message(__MSG_OUTPUT_EVAL_ERROR, {print_to_string(show, __error)}))
        end

        # if nothing was returned, send nothing back
        if __result == nothing
            ans = nothing
            return __write_message(__Message(__MSG_OUTPUT_EVAL_RESULT, {""}))
        end

        # otherwise, send back the result
        ans = __result
        return __write_message(__Message(__MSG_OUTPUT_EVAL_RESULT, {print_to_string(show, __result)}))
    end
end

# event handler for socket input
add_fd_handler(__connectfd, __socket_callback)

###########################################
# wait forever while asynchronous processing happens
###########################################

# this is better than an infinite loop because it doesn't consume the cpu
wait(RemoteRef())