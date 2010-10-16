# showing fundamental objects

print(x) = show(x)
print(x...) = (for i=x; print(i); end)

function show_comma_array(ar, open, close)
    print(open)
    for i=1:length(ar)
        show(ar[i])
        if i < length(ar)
            print(",")
        end
    end
    print(close)
end

function show(e::Expr)
    hd = e.head
    if is(hd,`call)
        print(e.args[1])
        show_comma_array(e.args[2:],"(",")")
    elseif is(hd,`=)
        print(e.args[1], " = ", e.args[2])
    elseif is(hd,`quote)
        print("`", e.args[1])
    elseif is(hd,`null)
        print("()")
    elseif is(hd,`goto)
        print("goto ", e.args[1])
    elseif is(hd,`gotoifnot)
        print("unless ", e.args[1], " goto ", e.args[2])
    elseif is(hd,`label)
        print(e.args[1],": ")
    elseif is(hd,symbol("return"))
        print("return ", e.args[1])
    elseif is(hd,`string)
        print("\"", e.args[1], "\"")
    elseif is(hd,symbol("::"))
        print(e.args[1], "::", e.args[2])
    elseif is(hd,`symbol)
        print(e.args[1])
    elseif is(hd,`body) || is(hd,`block)
        print("\nbegin\n")
        for a=e.args
            print("  ", a, "\n")
        end
        print("end\n")
    else
        print(hd)
        show_comma_array(e.args,"(",")")
    end
    if !is(e.type, Any)
        if isa(e.type, FuncKind)
            print("::F")
        elseif is(e.type, IntrinsicFunction)
            print("::I")
        else
            print("::", e.type)
        end
    end
end

function show(e::TypeError)
    ctx = (e.context=="") ? "" : strcat("in ", e.context, ", ")
    print("type error: ", e.func, ": ", ctx, "expected ", e.expected, ", got ",
          typeof(e.got), "\n")
end

function show(e::LoadError)
    show(e.error)
    print(" ", e.file, ":", e.line, "\n")
end

show(e::DivideByZeroError) = print("error: integer divide by zero\n")

show(e::StackOverflowError) = print("error: stack overflow\n")

show(e::ErrorException) = print(e.msg, "\n")

dump(t::Type) = print(t)
dump(t::Tuple) = print(t)

function dump{T}(x::T)
    print(T,"(")
    for field = T.names
        print(field, "=")
        show(getfield(x, field))
        print(",")
    end
    print(")\n")
end
