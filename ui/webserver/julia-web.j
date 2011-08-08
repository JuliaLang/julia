###########################################
# protocol
###########################################

###### the julia<-->server protocol #######

# the message type is sent as a byte
# the next four bytes indicates how many arguments there are
# each argument is four bytes indicating the size of the argument, then the data for that argument

###### the server<-->browser protocol #####

# messages are sent as json arrays
# [message_type:number, arg1:string, arg2:string, ...]

###########################################
# input messages
###########################################

# new session (this message is intercepted in the SCGI server and never reaches here)
# arguments: {}
MSG_INPUT_START = 0

# poll the server (this message is intercepted in the SCGI server and never reaches here)
# arguments: {}
MSG_INPUT_POLL = 1

# evaluate an expression
# arguments: {expression}
MSG_INPUT_EVAL = 2

###########################################
# output messages
###########################################

# message
# arguments: {message}
MSG_OUTPUT_MESSAGE = 0

# error message
# arguments: {message}
MSG_OUTPUT_ERROR = 1

# fatal error message (terminates session)
# arguments: {message}
MSG_OUTPUT_FATAL_ERROR = 2

# incomplete expression
# arguments: {}
MSG_OUTPUT_EVAL_INCOMPLETE = 3

# expression result
# arguments: {result}
MSG_OUTPUT_EVAL_RESULT = 4

###########################################
# set up the socket connection
###########################################

# open a socket on port 4444
ports = [int16(4444)]
sockfd = ccall(:open_any_tcp_port, Int32, (Ptr{Int16},), ports)
if sockfd == -1 || ports[1] != 4444
    # couldn't open the socket
    println("could not open server socket on port 4444.")
    exit()
end
println("server listening on port 4444.")

# wait for the server to connect to the socket
connectfd = ccall(dlsym(libc, :accept), Int32, (Int32, Ptr{Void}, Ptr{Void}), sockfd, C_NULL, C_NULL)

# create an io object from the file descriptor
io = fdio(connectfd)

###########################################
# protocol implementation
###########################################

# a message
type Message
    msg_type::Uint8
    args::Array{Any,1}
end

# read a message
function read_message()
    msg_type = read(io, Uint8)
    args = {}
    num_args = read(io, Uint8)
    for i=1:num_args
        arg_length = read(io, Uint8)
        arg = ASCIIString(read(io, Uint8, arg_length))
        push(args, arg)
    end

    # print the message
    print("received message:  ")
    print_message(Message(msg_type, args))

    return Message(msg_type, args)
end

# send a message
function write_message(msg)
    write(io, uint8(msg.msg_type))
    write(io, uint8(length(msg.args)))
    for arg=msg.args
        write(io, uint8(length(arg)))
        write(io, arg)
    end
    flush(io)

    # print the message
    print("sent message:  ")
    print_message(msg)
end

# print a message (useful for debugging)
function print_message(msg)
    print(msg.msg_type)
    print(":  ")
    for arg=msg.args
        print("\"")
        print(arg)
        print("\" ")
    end
    println("")
end

###########################################
# protocol helpers
###########################################

# send a message
function send_message(msg)
    write_message(Message(MSG_OUTPUT_MESSAGE, [msg]))
end

# send an error message
function send_error(msg)
    write_message(Message(MSG_OUTPUT_ERROR, [msg]))
end

# send a fatal error message
function send_fatal_error(msg)
    write_message(Message(MSG_OUTPUT_FATAL_ERROR, [msg]))
end

# send an incomplete expression message
function send_eval_incomplete(msg)
    write_message(Message(MSG_OUTPUT_EVAL_INCOMPLETE, [msg]))
end

# send an expression result message
function send_eval_result(msg)
    write_message(Message(MSG_OUTPUT_EVAL_RESULT, [msg]))
end

###########################################
# input event handler
###########################################

# callback for that event handler
function socket_callback(fd)
    # read the message
    msg = read_message()

    # MSG_INPUT_EVAL
    if msg.msg_type == MSG_INPUT_EVAL
        # try to evaluate it
        expr_c_str = cstring(msg.args[1])
        expr = ccall(:jl_parse_input_line, Any, (Ptr{Uint8},), expr_c_str)

        # check if there was an error
        if expr.head == :error
            return send_error(expr.args[1])
        end

        # check if the expression was incomplete
        if expr.head == :continue
            return send_eval_incomplete()
        end

        # evaluate the expression
        local result
        try
            result = eval(expr)
        catch error
            return send_error("There was an error!")
        end
        return send_eval_result(string(result))
    end
end

# event handler for socket input
add_fd_handler(connectfd, socket_callback)

###########################################
# asynchronous stuff
###########################################

# do asynchronous stuff
yield()