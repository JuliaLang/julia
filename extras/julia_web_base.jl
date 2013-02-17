## Julia Webrepl ##
const DEBUG = false
import Base.show

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
include("webrepl_msgtypes_h.jl")

###########################################
# set up the socket connection
###########################################

# open a socket on any port
(port,sock) = Base.open_any_tcp_port(4444)

# print the socket number so the server knows what it is
println(STDOUT,int16(port))

# wait for the server to connect to the socket
__io = Base.wait_accept(sock)
Base.start_reading(__io)

###########################################
# protocol implementation
###########################################

# a message
type __Message
    msg_type::Uint8
    args::Array{Any, 1}
end
show(m::__Message) = __print_message(m)

# read a message
function __read_message()
    msg_type = read(__io, Uint8)
    args = {}
    num_args = read(__io, Uint8)
    for i=1:num_args
        arg_length = read(__io, Uint32)
        arg = ASCIIString(read(__io, Uint8, arg_length))
        push!(args, arg)
    end
    return __Message(msg_type, args)
end

# send a message
const __io_out_buf = PipeString()
function __write_message(msg)
    if DEBUG show(msg); println() end
    write(__io, uint8(msg.msg_type))
    write(__io, uint8(length(msg.args)))
    for arg=msg.args
        write(__io_out_buf, arg)
        data = takebuf_array(__io_out_buf)
        write(__io, uint32(length(data)))
        write(__io, data)
    end
    #flush(__io)
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
include("julia_web.jl")

###########################################
# input event handler
###########################################

# store the result of the previous input
ans = nothing

# callback for that event handler
function __socket_callback(fd)
    # read the message
    __msg = __read_message()
    if DEBUG show(__msg); println() end
    
    # MSG_INPUT_EVAL
    if __msg.msg_type == __MSG_INPUT_EVAL && length(__msg.args) == 3
        # parse the arguments
        __user_name = __msg.args[1]
        __user_id = __msg.args[2]
        __input = __msg.args[3]

        # split the input into lines
        __lines = split(__input, '\n')

        # try to parse each line incrementally
        __parsed_exprs = {}
        __input_so_far = ""
        __all_nothing = true

        for i=1:length(__lines)
            # add the next line of input
            __input_so_far = string(__input_so_far, __lines[i], "\n")

            # try to parse it
            __expr = Base.parse_input_line(__input_so_far)
            
            # if there was nothing to parse, just keep going
            if __expr == nothing
                continue
            end
            __all_nothing = false
            __expr_multitoken = isa(__expr, Expr)

            # stop now if there was a parsing error
            if __expr_multitoken && __expr.head == :error
                # send everyone the input
                __write_message(__Message(__MSG_OUTPUT_EVAL_INPUT, {__user_id, __user_name, __input}))
                return __write_message(__Message(__MSG_OUTPUT_EVAL_ERROR, {__user_id, __expr.args[1]}))
            end
            
            # if the expression was incomplete, just keep going
            if __expr_multitoken && __expr.head == :continue
                continue
            end

            # add the parsed expression to the list
            __input_so_far = ""
            __parsed_exprs = [__parsed_exprs, {(__user_id, __expr)}]
        end

        # if the input was empty, stop early
        if __all_nothing
            # send everyone the input
            __write_message(__Message(__MSG_OUTPUT_EVAL_INPUT, {__user_id, __user_name, __input}))
            return __write_message(__Message(__MSG_OUTPUT_EVAL_RESULT, {__user_id, ""}))
        end

        # tell the browser if we didn't get a complete expression
        if length(__parsed_exprs) == 0
            return __write_message(__Message(__MSG_OUTPUT_EVAL_INCOMPLETE, {__user_id}))
        end

        # send everyone the input
        __write_message(__Message(__MSG_OUTPUT_EVAL_INPUT, {__user_id, __user_name, __input}))

        put(__eval_channel, __parsed_exprs)
    end
end

# event handler for socket input
enq_work(@task while true __socket_callback(__io) end)

web_show(user_id, ans) =
    __Message(__MSG_OUTPUT_EVAL_RESULT, {user_id, sprint(repl_show, ans)})

function __eval_exprs(__parsed_exprs)
    global ans
    user_id = ""

    # try to evaluate the expressions
    for i=1:length(__parsed_exprs)
        # evaluate the expression and stop if any exceptions happen
        user_id = __parsed_exprs[i][1]
        try
            ans = eval(__parsed_exprs[i][2])
        catch __error
            return __write_message(__Message(__MSG_OUTPUT_EVAL_ERROR, {user_id, sprint(show, __error)}))
        end
    end
    
    # send the result of the last expression
    if isa(ans,Nothing)
        return __write_message(__Message(__MSG_OUTPUT_EVAL_RESULT, {user_id, ""}))
    else
        return __write_message(web_show(user_id, ans))
    end
end

# print version info
println("Julia ", Base.version_string)
println(Base.commit_string, "\n")

# work around bug displaying "\n "
#print("  ",replace(Base.banner_plain, "\n", "\n  "))

###########################################
# wait forever while asynchronous processing happens
###########################################

__eval_channel = RemoteRef()

while true
    __eval_exprs(take(__eval_channel))
end
