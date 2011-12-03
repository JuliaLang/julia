###########################################
# protocol
###########################################

###### the julia<-->server protocol #######

# the message type is sent as a byte
# the next byte indicates how many arguments there are
# each argument is four bytes indicating the size of the argument, then the data for that argument

###### the server<-->browser protocol #####

# messages are sent as arrays of arrays (json)
# the outer array is an "array of messages"
# each message is itself an array:
# [message_type::number, arg0::string, arg1::string, ...]

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
# standard web library
###########################################

# load the special functions available to the web repl
load("./ui/webserver/julia_web.j")

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