###########################################
# protocol
###########################################

###### the julia<-->server protocol #######

# the message type is sent as a byte
# the next four bytes indicates how many arguments there are
# each argument is four bytes indicating the size of the argument, then the data for that argument

###### the server<-->browser protocol #####

# messages are sent as json arrays
# [message_type:number, arg0:string, arg1:string, ...]

###########################################
# input messages
###########################################

# null message (should be ignored)
# arguments: {}
MSG_INPUT_NULL = 0

# new session (this message is intercepted in the SCGI server and never reaches here)
# arguments: {}
MSG_INPUT_START = 1

# poll the server (this message is intercepted in the SCGI server and never reaches here)
# arguments: {}
MSG_INPUT_POLL = 2

# evaluate an expression
# arguments: {expression}
MSG_INPUT_EVAL = 3

###########################################
# output messages
###########################################

# null message (should be ignored)
# arguments: {}
MSG_OUTPUT_NULL = 0

# message
# arguments: {message}
MSG_OUTPUT_MESSAGE = 1

# error message
# arguments: {message}
MSG_OUTPUT_ERROR = 2

# fatal error message (terminates session)
# arguments: {message}
MSG_OUTPUT_FATAL_ERROR = 3

# incomplete expression
# arguments: {}
MSG_OUTPUT_EVAL_INCOMPLETE = 4

# expression result
# arguments: {result}
MSG_OUTPUT_EVAL_RESULT = 5

# other output (not sent directly from julia)
# arguments: {message}
MSG_OUTPUT_OTHER = 6

###########################################
# set up the socket connection
###########################################

# get the port number
print("enter the port number:\n")
port_num_str = readline(stdin_stream)
port_num = int16(port_num_str[1:length(port_num_str)-1])

# connect to localhost on that port
connectfd = ccall(:connect_to_host, Int32, (Ptr{Uint8}, Int16), "127.0.0.1", port_num)
if connectfd == -1
    # couldn't open the socket
    println("could not connect to socket on localhost.")
    exit()
end
println("client connected.")

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
# input event handler
###########################################

# callback for that event handler
function socket_callback(fd)
    # read the message
    msg = read_message()
end

# event handler for socket input
add_fd_handler(connectfd, socket_callback)

###########################################
# main program loop
###########################################

write_message(Message(MSG_INPUT_EVAL, ["3+5/2"]))

# do asynchronous stuff
wait(RemoteRef())