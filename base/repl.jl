# called to show a REPL result
repl_show(v::ANY) = repl_show(OUTPUT_STREAM, v)
function repl_show(io, v::ANY)
    if !(isa(v,Function) && isgeneric(v))
        if isa(v,AbstractVector) && !isa(v,Ranges)
            print(io, summary(v))
            if !isempty(v)
                println(io, ":")
                print_matrix(io, reshape(v,(length(v),1)))
            end
        else
            show(io, v)
        end
    end
    if isgeneric(v) && !isa(v,CompositeKind)
        show(io, v.env)
    end
end

# showing exception objects as descriptive error messages

error_show(io::IO, e) = show(io, e)

function error_show(io::IO, e::TypeError)
    ctx = isempty(e.context) ? "" : "in $(e.context), "
    if e.expected === Bool
        print(io, "type: non-boolean ($(typeof(e.got))) ",
                  "used in boolean context")
    elseif e.expected === Function && e.func === :apply && isa(e.got,AbstractKind)
        print(io, "type: cannot instantiate abstract type $(e.got.name)")
    else
        if isa(e.got,Type)
            tstr = "Type{$(e.got)}"
        else
            tstr = string(typeof(e.got))
        end
        print(io, "type: $(e.func): ",
                  "$(ctx)expected $(e.expected), ",
                  "got $tstr")
    end
end

function error_show(io::IO, e, bt)
    error_show(io, e)
    show_backtrace(io, bt)
end

error_show(io::IO, e::LoadError) = error_show(io, e, {})
function error_show(io::IO, e::LoadError, bt)
    error_show(io, e.error, bt)
    print(io, "\nat $(e.file):$(e.line)")
end

error_show(io::IO, e::SystemError) = print(io, "$(e.prefix): $(strerror(e.errnum))")
error_show(io::IO, ::DivideByZeroError) = print(io, "integer divide by zero")
error_show(io::IO, ::StackOverflowError) = print(io, "stack overflow")
error_show(io::IO, ::UndefRefError) = print(io, "access to undefined reference")
error_show(io::IO, ::EOFError) = print(io, "read: end of file")
error_show(io::IO, e::ErrorException) = print(io, e.msg)
error_show(io::IO, e::KeyError) = print(io, "key not found: $(e.key)")
error_show(io::IO, e::InterruptException) = print(io, "interrupt")

function error_show(io::IO, e::MethodError)
    name = e.f.env.name
    if is(e.f,convert) && length(e.args)==2
        print(io, "no method $(name)(Type{$(e.args[1])},$(typeof(e.args[2])))")
    else
        print(io, "no method $(name)$(typeof(e.args))")
    end
end

function show_backtrace(io::IO, t)
    # we may not declare :eval_user_input
    # directly so that we get a compile error
    # in case its name changes in the future
    const eval_function = try
            symbol(string(eval_user_input))
        catch
            :(:) #for when client.jl is not yet defined
        end
    for i = 1:3:length(t)
        if i == 1 && t[i] == :error; continue; end
        if t[i] == eval_function; break; end
        print(io, "\n")
        lno = t[i+2]
        print(io, " in ", t[i], " at ", t[i+1])
        if lno >= 1
            try
                print(io, ":", lno)
            catch
                print(io, '?') #for when dec is not yet defined
            end
        end
    end
end
