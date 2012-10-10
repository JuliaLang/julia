import Base.*

##########################################
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
load("webrepl_msgtypes_h.jl")

#macro debug_only(x); x; end
macro debug_only(x); end

###########################################
# set up the socket connection
###########################################

# open a socket on any port
function connect_cb(server::AsyncStream,status::Int32)
    @debug_only println(STDERR,"Julia: Client instance connected")
    global __client
    if(status == -1)
        error("An error occured during the creation of the server")
    end
    client = TcpSocket()
    __client = client
    err = accept(server,client)
    if err!=0
        print("accept error: ", Base._uv_lasterror(Base.globalEventLoop()), "\n")
    else
        p=__PartialMessageBuffer()
		client.readcb = (args...)->__socket_callback(client,p,args...)
        Base.start_reading(client)
    end
end

(port,sock) = Base.open_any_tcp_port(4444,connect_cb)

# print the socket number so the server knows what it is
println(STDOUT,int16(port))

@debug_only println(STDERR,"Julia Instance Started")

###########################################
# protocol implementation
###########################################

# a message
type __Message
    msg_type::Uint8
    args::Array{Any, 1}
    __Message(msg_type::Uint8,args::Array{Any,1})=new(msg_type,args)
    __Message() = new(255,cell(0))
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
    @debug_only __print_message(msg)
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
    println(STDERR,"Writing message: ",msg.msg_type)
    println(STDERR,"Number of arguments: ",length(msg.args))
    show(STDERR,msg.args)
    for arg=msg.args
        println(STDERR,"Argument Length: ",length(arg))
    end
end

###########################################
# standard web library
###########################################

# load the special functions available to the web repl
load("julia_web.jl")

###########################################
# input event handler
###########################################

# store the result of the previous input
ans = nothing


# callback for that event handler
function __socket_callback(client::TcpSocket,p::__PartialMessageBuffer,stream::TcpSocket)
    @debug_only println(STDERR,"received")
    arr = stream.buffer.data
    @debug_only println(STDERR,"Callback: ",arr)
    pos = 0
	nread = stream.buffer.ptr-1
    while(pos<nread)
        pos+=1
        @debug_only println(STDERR,"loop at pos ",pos," of ",length(arr)," with message type ",p.current.msg_type)
        b=arr[pos]
        if(p.current.msg_type == 255)
            p.current.msg_type = b
			if(b==255)
                error("Message type must not exceed 254")
            end
            @debug_only println(STDERR,"Message type: ",b)
            continue
        elseif(p.num_args == 255)
            if(b==255)
                error("Number of arguments for a message must not exceed 254")
            end
            p.num_args = b
            @debug_only println(STDERR,"Number of arguments: ",b)
            continue
        elseif(p.curArgHeaderByteNum<4)
            p.curArgLength|=int32(b)<<8*p.curArgHeaderByteNum
            p.curArgHeaderByteNum += 1
            @debug_only println(STDERR,"received header: ",b," ",p.curArgHeaderByteNum)
            if(p.curArgHeaderByteNum != 4 || p.curArgLength != 0)
                continue
            end
         elseif(nread-pos<p.curArgLength-p.curArgPos)
            append!(p.curArg.data,arr[pos:nread])
            @debug_only begin
                println(STDERR,"message body incomplete")
                println(STDERR,nread)
                println(STDERR,pos)
                println(STDERR,p.curArgLength)
                println(STDERR,p.curArgPos)
                println(STDERR,p.current.msg_type)
                println(STDERR,p.num_args)
            end
            p.curArgPos=nread-pos
            break
        else
            append!(p.curArg.data,arr[pos:(pos+p.curArgLength-p.curArgPos)])
        end
        @debug_only println(STDERR,"argument of length ",p.curArgLength," at pos ",p.curArgPos," complete");
        pos+=p.curArgLength-p.curArgPos;
        push(p.current.args,p.curArg)
        p.curArg=ASCIIString(Array(Uint8,0))
        @debug_only begin
            println(STDERR,"message body complete")
            println(STDERR,p.num_args)
            println(STDERR,p.current.args)
        end
        p.curArgLength=0
        p.curArgHeaderByteNum=0
        p.curArgPos=1
        if(numel(p.current.args)>=p.num_args)
            __msg=p.current
            p.current=__Message()
            p.num_args=255

            # MSG_INPUT_EVAL
            if __msg.msg_type == __MSG_INPUT_EVAL && length(__msg.args) == 3
                                    @debug_only println(STDERR,"Evaluating input")
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
                    __input_so_far = strcat(__input_so_far, __lines[i], "\n")

                    # try to parse it
                    __expr = parse_input_line(__input_so_far)

                    # if there was nothing to parse, just keep going
                    if __expr == nothing
                            continue
                    end
                    __all_nothing = false
                    __expr_multitoken = isa(__expr, Expr)

                    # stop now if there was a parsing error
                    if __expr_multitoken && __expr.head == :error
                        # send everyone the input
                        __write_message(client,__Message(__MSG_OUTPUT_EVAL_INPUT, {__user_id, __user_name, __input}))
                        __write_message(client,__Message(__MSG_OUTPUT_EVAL_ERROR, {__user_id, __expr.args[1]}))
						stream.buffer.ptr = 1
						return true
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
                    __write_message(client,__Message(__MSG_OUTPUT_EVAL_INPUT, {__user_id, __user_name, __input}))
                    __write_message(client,__Message(__MSG_OUTPUT_EVAL_RESULT, {__user_id, ""}))
					stream.buffer.ptr = 1
					return true
                end

                # tell the browser if we didn't get a complete expression
                if length(__parsed_exprs) == 0
                    __write_message(client,__Message(__MSG_OUTPUT_EVAL_INCOMPLETE, {__user_id}))
					stream.buffer.ptr = 1
					return true
                end

                # send everyone the input
                __write_message(client,__Message(__MSG_OUTPUT_EVAL_INPUT, {__user_id, __user_name, __input}))

                __eval_exprs(client, __parsed_exprs)
            end
        end
    end
	stream.buffer.ptr=1
	return true
end


web_show(user_id, ans) =
    __Message(__MSG_OUTPUT_EVAL_RESULT, {user_id, sprint(repl_show, ans)})

function __eval_exprs(client,__parsed_exprs)    global ans
    user_id = ""

    # try to evaluate the expressions
    for i=1:length(__parsed_exprs)
        # evaluate the expression and stop if any exceptions happen
        user_id = __parsed_exprs[i][1]
        try
            ans = eval(__parsed_exprs[i][2])
        catch __error
            return __write_message(client,__Message(__MSG_OUTPUT_EVAL_ERROR, {user_id,sprint(repl_show, __error)}))
        end
    end

    # send the result of the last expression
    if isa(ans,Nothing)
        return __write_message(client,__Message(__MSG_OUTPUT_EVAL_RESULT, {user_id,""}))
    else
        return __write_message(client, web_show(user_id, ans))
    end
end

# print version info
println("Julia ", Base._jl_version_string)
println(Base._jl_commit_string, "\n")

# work around bug displaying "\n "
#print("  ",replace(Base._jl_banner_plain, "\n", "\n  "))

###########################################
# wait forever while asynchronous processing happens
###########################################

while true
try
    Base.run_event_loop()
catch(err)
    print(err)
end
end
