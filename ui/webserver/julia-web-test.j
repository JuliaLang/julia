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

# connect to localhost on 4444
connectfd = ccall(:connect_to_host, Int32, (Ptr{Uint8}, Int16), "127.0.0.1", int16(4444))
if connectfd == -1
    # couldn't open the socket
    println("could not connect to socket on port 4444.")
    exit()
end
println("client connected to port 4444.")

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

# send a start message
write_message(Message(MSG_INPUT_START, {}))

while true
    # send an expression to evaluate
    write_message(Message(MSG_INPUT_EVAL, ["5"]))

    # wait a second before repeating
    sleep(1)
end