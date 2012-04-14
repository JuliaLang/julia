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

#macro debug_only(x); x; end
macro debug_only(x); end

###########################################
# set up the socket connection
###########################################

# open a socket on any port
function connect_cb(accept_fd::Ptr,status::Int32)
    global __client
    if(status == -1)
        error("An error occured during the creation of the server")
    end
    client = TcpSocket(_jl_tcp_init(globalEventLoop()))
    __client = client
    err = _jl_tcp_accept(box(Ptr{Void},unbox(Int,accept_fd)),client.handle)
    if err!=0
        print("accept error: ", _uv_lasterror(globalEventLoop()), "\n")
    else
        p=__PartialMessageBuffer()
        add_io_handler(client,make_callback((args...)->__socket_callback(client,p,args...)))
    end
end

(port,sock) = open_any_tcp_port(4444,make_callback(connect_cb))

# print the socket number so the server knows what it is
println(int16(port))

###########################################
# protocol implementation
###########################################

# a message
type __Message
    msg_type::Uint8
    args::Array{Any, 1}
    __Message(msg_type::Uint8,args::Array{Any,1})=new(msg_type,args)
    __Message() = new(-1,cell(0))
end

type __PartialMessageBuffer
    current::__Message
    num_args::Uint8
    curArg::ASCIIString
    curArgLength::Int32
    curArgHeaderByteNum::Uint8
    curArgPos::Int32
    __PartialMessageBuffer()=new(__Message(),255,"",0,0,1)
end

# send a message
function __write_message(client::TcpSocket,msg)
    write(client, uint8(msg.msg_type))
    write(client, uint8(length(msg.args)))
    for arg=msg.args
        write(client, uint32(length(arg)))
        write(client, arg)
    end
end
__write_message(msg) = __write_message(__client,msg)

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
load("./ui/webserver/julia_web.jl")

###########################################
# input event handler
###########################################

# store the result of the previous input
ans = nothing


# callback for that event handler
function __socket_callback(client::TcpSocket,p::__PartialMessageBuffer,handle::Ptr,nread::Int,base::Ptr,len::Int32)
    if(nread <= 0)
        return
    end
    arr = ccall(:jl_pchar_to_array,Any,(Ptr,Int),base,nread)::Array{Uint8}
    @debug_only println("Callback: ",arr)
    pos = 0
    while(pos<nread)
        pos+=1
        b=arr[pos]
        if(p.current.msg_type == 255)
            p.current.msg_type = b
            @debug_only println("Message type: ",b)
        elseif(p.num_args == 255)
            if(b==255)
                error("Number of arguments for a message must not exceed 254")
            end
            p.num_args = b
            @debug_only println("Number of arguments: ",b)
        elseif(p.curArgHeaderByteNum<4)
            p.curArgLength|=int32(b)<<8*p.curArgHeaderByteNum
            p.curArgHeaderByteNum += 1
            @debug_only println("received header: ",b)
        elseif(nread-pos<p.curArgLength-p.curArgPos)
            append!(p.curArg.data,arr[pos:nread])
            set_current_output_stream(STDERR)
            @debug_only begin
                println("message body incomplete")
                println(nread)
                println(pos)
                println(p.curArgLength)
                println(p.curArgPos)
                println(p.current.msg_type)
                println(p.num_args)
            p.curArgPos=nread-pos
            end
            break
        else
            append!(p.curArg.data,arr[pos:(pos+p.curArgLength-p.curArgPos)])
            pos+=p.curArgLength-p.curArgPos;
            push(p.current.args,p.curArg)
            p.curArg=""
            @debug_only begin
                println("message body complete")
                println(p.num_args)
                println(p.current.args)
            end
            if(numel(p.current.args)>=p.num_args)
                __msg=p.current
                p.current=__Message()
                p.num_args=255
                p.curArgLength=0
                p.curArgHeaderByteNum=0
                p.curArgPos=1

                if __msg.msg_type == __MSG_INPUT_EVAL
                    # split the input into lines
                    __lines = split(__msg.args[1], '\n')

                    # try to parse each line incrementally
                    __parsed_exprs = {}
                    __input_so_far = ""
                    __all_nothing = true
                    breakLoop=false
                    for i=1:length(__lines)
                         #add the next line of input
                         __input_so_far = strcat(__input_so_far, __lines[i], "\n")

                         # try to parse it
                         __expr = parse_input_line(__input_so_far)

                         # if there was nothing to parse, just keep going
                         if __expr == nothing
                           continue
                         end

                         __all_nothing = false

                         # stop now if there was a parsing error
                         if __expr.head == :error
                            __write_message(client,__Message(__MSG_OUTPUT_PARSE_ERROR, {__expr.args[1]}))
                            breakLoop=true
                            break
                         elseif __expr.head == :continue
                            continue
                         end

                         # add the parsed expression to the list
                         __input_so_far = ""
                         __parsed_exprs = [__parsed_exprs, {__expr}]
                    end
                    if(breakLoop)
                        continue
                    end

                    # if the input was empty, stop early
                    if __all_nothing
                        __write_message(client,__Message(__MSG_OUTPUT_PARSE_COMPLETE, {}))
                        __write_message(client,__Message(__MSG_OUTPUT_EVAL_RESULT, {""}))
                        continue
                    end

                    # tell the browser if we didn't get a complete expression
                    if length(__parsed_exprs) == 0
                        __write_message(client,__Message(__MSG_OUTPUT_PARSE_INCOMPLETE, {}))
                        continue
                    end

                    # tell the browser all the lines were parsed
                    __write_message(client,__Message(__MSG_OUTPUT_PARSE_COMPLETE, {}))

                    __eval_exprs(client,__parsed_exprs)
                end
            end
        end
    end
end

function __eval_exprs(client,__parsed_exprs)
    global ans
    # try to evaluate the expressions
    for i=1:length(__parsed_exprs)
        # evaluate the expression and stop if any exceptions happen
        try
            ans = eval(__parsed_exprs[i])
        catch __error
            return __write_message(client,__Message(__MSG_OUTPUT_EVAL_ERROR, {print_to_string(show, __error)}))
        end
    end
    
    # send the result of the last expression
    if ans == nothing
        return __write_message(client,__Message(__MSG_OUTPUT_EVAL_RESULT, {""}))
    else
        return __write_message(client,__Message(__MSG_OUTPUT_EVAL_RESULT, {print_to_string(show, ans)}))
    end
end

###########################################
# wait forever while asynchronous processing happens
###########################################

set_current_output_stream(STDERR)
while true
try
    run_event_loop(globalEventLoop())
catch(err)
    set_current_output_stream(STDERR)
    print(err)
end
end
