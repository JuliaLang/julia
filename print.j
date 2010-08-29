# printing fundamental objects

print(x...) = for i=x; print(i); end

function print_comma_array(ar, open, close)
    print(open)
    for i=1:length(ar)
        print(ar[i])
        if i < length(ar)
            print(",")
        end
    end
    print(close)
end

function print(e::Expr)
    hd = e.head
    if is(hd,`call)
        print(e.args[1])
        print_comma_array(e.args[2:],"(",")")
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
        print_comma_array(e.args,"(",")")
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

inspect(x) = print(x)
