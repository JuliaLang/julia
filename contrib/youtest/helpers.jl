##################################
#### bootstrap testing helper ####
##################################

macro throws(a,b)
    ExceptionType, code = isa(a, Symbol) ? (a,b) : (b,a)
    quote
        thrown = false
        try
            $(esc(code))
        catch exception
            if isa(exception, $(esc(ExceptionType)))
                thrown = true
            else
                println("Expecting ", $(esc(ExceptionType)), " exception. Got ",
                        typeof(exception), " instead.")
                @assert false
            end
        end
        if !thrown
            println("Expecting ", $(esc(ExceptionType)), " exception, but no exception was raised.")
            @assert false
        end
    end
end


# Helps to understand what is going on macros
macro show(es...)
    global e
    e = es
    println("==================================================")
    println(es)
    println("--------------------------------------------------")
    pprint_expression(es)
    nothing
end

pprint_expression(e) = pprint_expression(e, "")
function pprint_expression(e::Expr, indent)
    println(indent, "Expr", " ", e.head)
    pprint_expression(e.args, strcat(indent, "  "))
end
function pprint_expression(s::Symbol, indent)
    println(indent, s)
end
function pprint_expression(iterable, indent)
    next_indent = strcat(indent, "    ")
    for item in iterable
        pprint_expression(item, next_indent)
    end
end
function pprint_expression(l::LineNumberNode, indent)
    println(indent, "- - - - line ", l.line, " - - - - - - -")
end
pprint_expression(i::Int, indent) = println(indent, i)
pprint_expression(i::Bool, indent) = println(indent, i)
pprint_expression(s::String, indent) = println(indent, "\"", s, "\"")

#@show fail 1==1 (a;b;c) 2==2

