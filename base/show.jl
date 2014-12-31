show(x) = show(STDOUT::IO, x)

print(io::IO, s::Symbol) = (write(io,s);nothing)

function show(io::IO, x::ANY)
    t = typeof(x)::DataType
    show(io, t)
    print(io, '(')
    if t.names !== () || t.size==0
        recorded = false
        oid = object_id(x)
        shown_set = get(task_local_storage(), :SHOWNSET, nothing)
        if shown_set == nothing
            shown_set = Set()
            task_local_storage(:SHOWNSET, shown_set)
        end

        try
            if oid in shown_set
                print(io, "#= circular reference =#")
            else
                push!(shown_set, oid)
                recorded = true

                n = length(t.names)
                for i=1:n
                    f = t.names[i]
                    if !isdefined(x, f)
                        print(io, undef_ref_str)
                    else
                        show(io, x.(f))
                    end
                    if i < n
                        print(io, ',')
                    end
                end
            end
        catch e
            rethrow(e)

        finally
            if recorded delete!(shown_set, oid) end
        end
    else
        nb = t.size
        print(io, "0x")
        p = pointer_from_objref(x) + sizeof(Ptr{Void})
        for i=nb-1:-1:0
            print(io, hex(unsafe_load(convert(Ptr{UInt8}, p+i)), 2))
        end
    end
    print(io,')')
end

function show(io::IO, f::Function)
    if isgeneric(f)
        print(io, f.env.name)
    elseif isdefined(f, :env) && isa(f.env,Symbol)
        print(io, f.env)
    else
        print(io, "(anonymous function)")
    end
end

function show(io::IO, x::IntrinsicFunction)
    print(io, "(intrinsic function #", box(Int32,unbox(IntrinsicFunction,x)), ")")
end

function show(io::IO, x::UnionType)
    if is(x,Top)
        print(io, "Top")
    else
        print(io, "Union", x.types)
    end
end

show(io::IO, x::TypeConstructor) = show(io, x.body)

function show(io::IO, x::DataType)
    if isvarargtype(x)
        print(io, x.parameters[1], "...")
    else
        show(io, x.name)
        if length(x.parameters) > 0
            show_comma_array(io, x.parameters, '{', '}')
        end
    end
end

showcompact(io::IO, x) = show(io, x)
showcompact(x) = showcompact(STDOUT::IO, x)

showcompact_lim(io, x) = _limit_output ? showcompact(io, x) : show(io, x)
showcompact_lim(io, x::Number) = _limit_output ? showcompact(io, x) : print(io, x)

macro show(exs...)
    blk = Expr(:block)
    for ex in exs
        push!(blk.args, :(println($(sprint(show_unquoted,ex)*" = "),
                                  repr(begin value=$(esc(ex)) end))))
    end
    if !isempty(exs); push!(blk.args, :value); end
    return blk
end

function show(io::IO, tn::TypeName)
    if tn.module == Core || tn.module == Base || tn.module == Main
        print(io, tn.name)
    else
        print(io, tn.module, '.', tn.name)
    end
end

show(io::IO, ::Void) = print(io, "nothing")
show(io::IO, b::Bool) = print(io, b ? "true" : "false")
show(io::IO, n::Signed) = (write(io, dec(n)); nothing)
show(io::IO, n::Unsigned) = print(io, "0x", hex(n,sizeof(n)<<1))
print(io::IO, n::Unsigned) = print(io, dec(n))

show{T}(io::IO, p::Ptr{T}) = print(io, typeof(p), " @0x$(hex(unsigned(p), WORD_SIZE>>2))")

function show(io::IO, p::Pair)
    show(io, p.first)
    print(io, "=>")
    show(io, p.second)
end

function show(io::IO, m::Module)
    if is(m,Main)
        print(io, "Main")
    else
        print(io, join(fullname(m),"."))
    end
end

function show(io::IO, l::LambdaStaticData)
    print(io, "AST(")
    show(io, uncompressed_ast(l))
    print(io, ")")
end

function show_delim_array(io::IO, itr::AbstractArray, op, delim, cl, delim_one, compact=false)
    print(io, op)
    newline = true
    first = true
    i = 1
    l = length(itr)
    if l > 0
        while true
            if !isassigned(itr, i)
                print(io, undef_ref_str)
                multiline = false
            else
                x = itr[i]
                multiline = isa(x,AbstractArray) && ndims(x)>1 && length(x)>0
                newline && multiline && println(io)
                compact ? showcompact_lim(io, x) : show(io, x)
            end
            i += 1
            if i > l
                delim_one && first && print(io, delim)
                break
            end
            first = false
            print(io, delim)
            if multiline
                println(io); println(io)
                newline = false
            else
                newline = true
            end
        end
    end
    print(io, cl)
end

function show_delim_array(io::IO, itr, op, delim, cl, delim_one)
    print(io, op)
    state = start(itr)
    newline = true
    first = true
    if !done(itr,state)
        while true
            x, state = next(itr,state)
            multiline = isa(x,AbstractArray) && ndims(x)>1 && length(x)>0
            newline && multiline && println(io)
            show(io, x)
            if done(itr,state)
                delim_one && first && print(io, delim)
                break
            end
            first = false
            print(io, delim)
            if multiline
                println(io); println(io)
                newline = false
            else
                newline = true
            end
        end
    end
    print(io, cl)
end

show_comma_array(io::IO, itr, o, c) = show_delim_array(io, itr, o, ',', c, false)
show(io::IO, t::Tuple) = show_delim_array(io, t, '(', ',', ')', true)

show(io::IO, s::Symbol) = show_unquoted(io, QuoteNode(s))

## Abstract Syntax Tree (AST) printing ##

# Summary:
#   print(io, ex) defers to show_unquoted(io, ex)
#   show(io, ex) defers to show_unquoted(io, QuoteNode(ex))
#   show_unquoted(io, ex) does the heavy lifting
#
# AST printing should follow two rules:
#   1. parse(string(ex)) == ex
#   2. eval(parse(repr(ex))) == ex
#
# Rule 1 means that printing an expression should generate Julia code which
# could be reparsed to obtain the original expression. This code should be
# unambiguous and as readable as possible.
#
# Rule 2 means that showing an expression should generate a quoted version of
# print’s output. Parsing and then evaling this output should return the
# original expression.
#
# This is consistent with many other show methods, i.e.:
#   show(Set([1,2,3]))                # ==> "Set{Int64}([2,3,1])"
#   eval(parse("Set{Int64}([2,3,1])”) # ==> An actual set
# While this isn’t true of ALL show methods, it is of all ASTs.

typealias ExprNode Union(Expr, QuoteNode, SymbolNode, LineNumberNode,
                         LabelNode, GotoNode, TopNode)
print        (io::IO, ex::ExprNode)    = show_unquoted(io, ex)
show         (io::IO, ex::ExprNode)    = show_unquoted(io, QuoteNode(ex))
show_unquoted(io::IO, ex)              = show_unquoted(io, ex, 0, 0)
show_unquoted(io::IO, ex, indent::Int) = show_unquoted(io, ex, indent, 0)
show_unquoted(io::IO, ex, ::Int,::Int) = show(io, ex)

## AST printing constants ##

const indent_width = 4
const quoted_syms = Set{Symbol}([:(:),:(::),:(:=),:(=),:(==),:(===),:(=>)])
const uni_ops = Set{Symbol}([:(+), :(-), :(!), :(¬), :(~), :(<:), :(>:), :(√), :(∛), :(∜)])
const expr_infix_wide = Set([:(=), :(+=), :(-=), :(*=), :(/=), :(\=), :(&=),
    :(|=), :($=), :(>>>=), :(>>=), :(<<=), :(&&), :(||), :(<:), :(=>)])
const expr_infix = Set([:(:), :(->), symbol("::")])
const expr_infix_any = union(expr_infix, expr_infix_wide)
const expr_calls  = Dict(:call =>('(',')'), :calldecl =>('(',')'), :ref =>('[',']'), :curly =>('{','}'))
const expr_parens = Dict(:tuple=>('(',')'), :vcat=>('[',']'), :cell1d=>("Any[","]"),
                         :hcat =>('[',']'), :row =>('[',']'))

## AST decoding helpers ##

is_id_start_char(c::Char) = ccall(:jl_id_start_char, Cint, (UInt32,), c) != 0
is_id_char(c::Char) = ccall(:jl_id_char, Cint, (UInt32,), c) != 0
function isidentifier(s::AbstractString)
    i = start(s)
    done(s, i) && return false
    (c, i) = next(s, i)
    is_id_start_char(c) || return false
    while !done(s, i)
        (c, i) = next(s, i)
        is_id_char(c) || return false
    end
    return true
end

isoperator(s::Symbol) = ccall(:jl_is_operator, Cint, (Ptr{UInt8},), s) != 0
operator_precedence(s::Symbol) = int(ccall(:jl_operator_precedence,
                                           Cint, (Ptr{UInt8},), s))
operator_precedence(x::Any) = 0 # fallback for generic expression nodes
const prec_power = operator_precedence(:(^))

is_expr(ex, head::Symbol)         = (isa(ex, Expr) && (ex.head == head))
is_expr(ex, head::Symbol, n::Int) = is_expr(ex, head) && length(ex.args) == n

is_linenumber(ex::LineNumberNode) = true
is_linenumber(ex::Expr)           = is(ex.head, :line)
is_linenumber(ex)                 = false

is_quoted(ex)            = false
is_quoted(ex::QuoteNode) = true
is_quoted(ex::Expr)      = is_expr(ex, :quote, 1)

unquoted(ex::QuoteNode)  = ex.value
unquoted(ex::Expr)       = ex.args[1]

## AST printing helpers ##

const indent_width = 4
show_expr_type_emphasize = false

function show_expr_type(io::IO, ty)
    if is(ty, Function)
        print(io, "::F")
    elseif is(ty, IntrinsicFunction)
        print(io, "::I")
    else
        if show_expr_type_emphasize::Bool && !isleaftype(ty)
            emphasize(io, "::$ty")
        else
            if !is(ty, Any)
                print(io, "::$ty")
            end
        end
    end
end

emphasize(io, str::AbstractString) = have_color ? print_with_color(:red, io, str) : print(io, uppercase(str))

show_linenumber(io::IO, line)       = print(io," # line ",line,':')
show_linenumber(io::IO, line, file) = print(io," # ",file,", line ",line,':')

# show a block, e g if/for/etc
function show_block(io::IO, head, args::Vector, body, indent::Int)
    print(io, head, ' ')
    show_list(io, args, ", ", indent)

    ind = is(head, :module) || is(head, :baremodule) ? indent : indent + indent_width
    exs = (is_expr(body, :block) || is_expr(body, :body)) ? body.args : Any[body]
    for ex in exs
        if !is_linenumber(ex); print(io, '\n', " "^ind); end
        show_unquoted(io, ex, ind)
    end
    print(io, '\n', " "^indent)
end
show_block(io::IO,head,    block,i::Int) = show_block(io,head, [], block,i)
function show_block(io::IO, head, arg, block, i::Int)
    if is_expr(arg, :block)
        show_block(io, head, arg.args, block, i)
    else
        show_block(io, head, Any[arg], block, i)
    end
end

# show an indented list
function show_list(io::IO, items, sep, indent::Int, prec::Int=0, enclose_operators::Bool=false)
    n = length(items)
    if n == 0; return end
    indent += indent_width
    first = true
    for item in items
        !first && print(io, sep)
        parens = enclose_operators && isa(item,Symbol) && isoperator(item)
        parens && print(io, '(')
        show_unquoted(io, item, indent, prec)
        parens && print(io, ')')
        first = false
    end
end
# show an indented list inside the parens (op, cl)
function show_enclosed_list(io::IO, op, items, sep, cl, indent, prec=0, encl_ops=false)
    print(io, op); show_list(io, items, sep, indent, prec, encl_ops); print(io, cl)
end

# show a normal (non-operator) function call, e.g. f(x,y) or A[z]
function show_call(io::IO, head, func, func_args, indent)
    op, cl = expr_calls[head]
    if isa(func, Symbol) || (isa(func, Expr) && func.head == :.)
        show_unquoted(io, func, indent)
    else
        print(io, '(')
        show_unquoted(io, func, indent)
        print(io, ')')
    end
    if !isempty(func_args) && isa(func_args[1], Expr) && func_args[1].head === :parameters
        print(io, op)
        show_list(io, func_args[2:end], ',', indent, 0)
        print(io, "; ")
        show_list(io, func_args[1].args, ',', indent, 0)
        print(io, cl)
    else
        show_enclosed_list(io, op, func_args, ",", cl, indent)
    end
end

## AST printing ##

show_unquoted(io::IO, sym::Symbol, ::Int, ::Int)        = print(io, sym)
show_unquoted(io::IO, ex::LineNumberNode, ::Int, ::Int) = show_linenumber(io, ex.line)
show_unquoted(io::IO, ex::LabelNode, ::Int, ::Int)      = print(io, ex.label, ": ")
show_unquoted(io::IO, ex::GotoNode, ::Int, ::Int)       = print(io, "goto ", ex.label)
show_unquoted(io::IO, ex::TopNode, ::Int, ::Int)        = print(io,"top(",ex.name,')')

function show_unquoted(io::IO, ex::SymbolNode, ::Int, ::Int)
    print(io, ex.name)
    show_expr_type(io, ex.typ)
end

show_unquoted(io::IO, ex::QuoteNode, indent::Int, prec::Int) =
    show_unquoted_quote_expr(io, ex.value, indent, prec)

function show_unquoted_quote_expr(io::IO, value, indent::Int, prec::Int)
    if isa(value, Symbol) && !(value in quoted_syms)
        s = string(value)
        if isidentifier(s) || isoperator(value)
            print(io, ":")
            print(io, value)
        else
            print(io, "symbol(\"", escape_string(s), "\")")
        end
    else
        if isa(value,Expr) && value.head === :block
            show_block(io, "quote", value, indent)
            print(io, "end")
        else
            print(io, ":(")
            show_unquoted(io, value, indent+indent_width, 0)
            print(io, ")")
        end
    end
end

# TODO: implement interpolated strings
function show_unquoted(io::IO, ex::Expr, indent::Int, prec::Int)
    head, args, nargs = ex.head, ex.args, length(ex.args)
    show_type = true
    global show_expr_type_emphasize
    state = show_expr_type_emphasize::Bool

    # dot (i.e. "x.y")
    if is(head, :(.))
        show_unquoted(io, args[1], indent + indent_width)
        print(io, '.')
        if is_quoted(args[2])
            show_unquoted(io, unquoted(args[2]), indent + indent_width)
        else
            print(io, '(')
            show_unquoted(io, args[2], indent + indent_width)
            print(io, ')')
        end

    # infix (i.e. "x<:y" or "x = y")
    elseif (head in expr_infix_any && nargs==2) || (is(head,:(:)) && nargs==3)
        func_prec = operator_precedence(head)
        head_ = head in expr_infix_wide ? " $head " : head
        if func_prec < prec
            show_enclosed_list(io, '(', args, head_, ')', indent, func_prec, true)
        else
            show_list(io, args, head_, indent, func_prec, true)
        end

    # list (i.e. "(1,2,3)" or "[1,2,3]")
    elseif haskey(expr_parens, head)               # :tuple/:vcat/:cell1d
        op, cl = expr_parens[head]
        if head === :vcat && !isempty(args) && is_expr(args[1], :row)
            sep = ";"
        elseif head === :hcat || head === :row
            sep = " "
        else
            sep = ","
        end
        head !== :row && print(io, op)
        show_list(io, args, sep, indent)
        if is(head, :tuple) && nargs == 1; print(io, ','); end
        head !== :row && print(io, cl)

    # function declaration (like :call but always printed with parens)
    # (:calldecl is a "fake" expr node created when we find a :function expr)
    elseif head == :calldecl && nargs >= 1
        show_call(io, head, args[1], args[2:end], indent)

    # function call
    elseif haskey(expr_calls, head) && nargs >= 1  # :call/:ref/:curly
        func = args[1]
        func_prec = operator_precedence(func)
        func_args = args[2:end]

        if in(ex.args[1], (:box, TopNode(:box), :throw)) || ismodulecall(ex)
            show_expr_type_emphasize::Bool = show_type = false
        end

        # scalar multiplication (i.e. "100x")
        if (func == :(*) && length(func_args)==2 &&
            isa(func_args[1], Real) && isa(func_args[2], Symbol))
            if func_prec <= prec
                show_enclosed_list(io, '(', func_args, "", ')', indent, func_prec)
            else
                show_list(io, func_args, "", indent, func_prec)
            end

        # unary operator (i.e. "!z")
        elseif func in uni_ops && length(func_args) == 1
            show_unquoted(io, func, indent)
            if isa(func_args[1], Expr) || length(func_args) > 1
                show_enclosed_list(io, '(', func_args, ",", ')', indent, func_prec)
            else
                show_unquoted(io, func_args[1])
            end

        # binary operator (i.e. "x + y")
        elseif func_prec > 0 # is a binary operator
            if length(func_args) > 1
                sep = " $func "
                if func_prec <= prec
                    show_enclosed_list(io, '(', func_args, sep, ')', indent, func_prec, true)
                else
                    show_list(io, func_args, sep, indent, func_prec, true)
                end
            else
                # 1-argument call to normally-binary operator
                op, cl = expr_calls[head]
                print(io, "(")
                show_unquoted(io, func, indent)
                print(io, ")")
                show_enclosed_list(io, op, func_args, ",", cl, indent)
            end

        # normal function (i.e. "f(x,y)" or "A[x,y]")
        else
            show_call(io, head, func, func_args, indent)
        end

    # comprehensions
    elseif (head === :typed_comprehension || head === :typed_dict_comprehension) && length(args) == 3
        isdict = (head === :typed_dict_comprehension)
        isdict && print(io, '(')
        show_unquoted(io, args[1], indent)
        isdict && print(io, ')')
        print(io, '[')
        show_unquoted(io, args[2], indent)
        print(io, " for ")
        show_unquoted(io, args[3], indent)
        print(io, ']')

    elseif (head === :comprehension || head === :dict_comprehension) && length(args) == 2
        print(io, '[')
        show_unquoted(io, args[1], indent)
        print(io, " for ")
        show_unquoted(io, args[2], indent)
        print(io, ']')

    elseif is(head, :ccall)
        show_unquoted(io, :ccall, indent)
        show_enclosed_list(io, '(', args, ",", ')', indent)

    # comparison (i.e. "x < y < z")
    elseif is(head, :comparison) && nargs >= 3 && (nargs&1==1)
        comp_prec = minimum(operator_precedence, args[2:2:end])
        if comp_prec <= prec
            show_enclosed_list(io, '(', args, " ", ')', indent, comp_prec)
        else
            show_list(io, args, " ", indent, comp_prec)
        end

    # function calls need to transform the function from :call to :calldecl
    # so that operators are printed correctly
    elseif head == :function && nargs==2 && is_expr(args[1], :call)
        show_block(io, head, Expr(:calldecl, args[1].args...), args[2], indent)
        print(io, "end")

    # block with argument
    elseif head in (:for,:while,:function,:if) && nargs==2
        show_block(io, head, args[1], args[2], indent); print(io, "end")

    elseif is(head, :module) && nargs==3 && isa(args[1],Bool)
        show_block(io, args[1] ? :module : :baremodule, args[2], args[3], indent); print(io, "end")

    # type declaration
    elseif is(head, :type) && nargs==3
        show_block(io, args[1] ? :type : :immutable, args[2], args[3], indent)
        print(io, "end")

    # empty return (i.e. "function f() return end")
    elseif is(head, :return) && nargs == 1 && is(args[1], nothing)
        print(io, head)

    # type annotation (i.e. "::Int")
    elseif is(head, symbol("::")) && nargs == 1
        print(io, "::")
        show_unquoted(io, args[1], indent)

    # var-arg declaration or expansion
    # (i.e. "function f(L...) end" or "f(B...)")
    elseif is(head, :(...)) && nargs == 1
        show_unquoted(io, args[1], indent)
        print(io, "...")

    elseif (nargs == 1 && head in (:return, :abstract, :const)) ||
                          head in (:local,  :global, :export)
        print(io, head, ' ')
        show_list(io, args, ", ", indent)

    elseif is(head, :macrocall) && nargs >= 1
        show_list(io, args, ' ', indent)

    elseif is(head, :typealias) && nargs == 2
        print(io, "typealias ")
        show_list(io, args, ' ', indent)

    elseif is(head, :line) && 1 <= nargs <= 2
        show_type = false
        show_linenumber(io, args...)

    elseif is(head, :if) && nargs == 3     # if/else
        show_block(io, "if",   args[1], args[2], indent)
        show_block(io, "else", args[3], indent)
        print(io, "end")

    elseif is(head, :try) && 3 <= nargs <= 4
        show_block(io, "try", args[1], indent)
        if is_expr(args[3], :block)
            show_block(io, "catch", is(args[2], false) ? Any[] : args[2], args[3], indent)
        end
        if nargs >= 4 && is_expr(args[4], :block)
            show_block(io, "finally", Any[], args[4], indent)
        end
        print(io, "end")

    elseif is(head, :let) && nargs >= 1
        show_block(io, "let", args[2:end], args[1], indent); print(io, "end")

    elseif is(head, :block) || is(head, :body)
        show_block(io, "begin", ex, indent); print(io, "end")

    elseif is(head, :quote) && nargs == 1
        show_unquoted_quote_expr(io, args[1], indent, 0)

    elseif is(head, :gotoifnot) && nargs == 2
        print(io, "unless ")
        show_list(io, args, " goto ", indent)

    elseif is(head, :string) && nargs == 1 && isa(args[1], AbstractString)
        show(io, args[1])

    elseif is(head, :null)
        print(io, "nothing")

    elseif is(head, :kw) && length(args)==2
        show_unquoted(io, args[1], indent+indent_width)
        print(io, '=')
        show_unquoted(io, args[2], indent+indent_width)

    elseif is(head, :string)
        a = map(args) do x
            if !isa(x,AbstractString)
                if isa(x,Symbol) && !(x in quoted_syms)
                    string("\$", x)
                else
                    string("\$(", sprint(show_unquoted,x), ")")
                end
            else
                sprint(print_escaped, x, "\"\$")
            end
        end
        print(io, '"', a..., '"')

    elseif is(head, :&) && length(args) == 1
        print(io, '&')
        show_unquoted(io, args[1])

    # transpose
    elseif (head === symbol('\'') || head === symbol(".'")) && length(args) == 1
        show_unquoted(io, args[1])
        print(io, head)

    elseif is(head, :import) || is(head, :importall) || is(head, :using)
        print(io, head)
        first = true
        for a = args
            if first
                print(io, ' ')
                first = false
            else
                print(io, '.')
            end
            if !is(a, :.)
                print(io, a)
            end
        end

    # print anything else as "Expr(head, args...)"
    else
        show_type = false
        show_expr_type_emphasize::Bool = in(ex.head, (:lambda, :method))
        print(io, "\$(Expr(")
        show(io, ex.head)
        for arg in args
            print(io, ", ")
            show(io, arg)
        end
        print(io, "))")
    end

    if ex.head == :(=)
        show_type = show_type_assignment(ex.args[2])
    elseif in(ex.head, (:boundscheck, :gotoifnot, :return))
        show_type = false
    end

    if show_type
        show_expr_type(io, ex.typ)
    end

    show_expr_type_emphasize::Bool = state
end

show_type_assignment(::Number) = false
show_type_assignment(::(Number...)) = false
show_type_assignment(::Expr) = false
show_type_assignment(::SymbolNode) = false
show_type_assignment(::LambdaStaticData) = false
show_type_assignment(a) = true

function ismodulecall(ex::Expr)
    ex.head == :call && ex.args[1] == TopNode(:getfield) &&
        isa(ex.args[2], Symbol) &&
        isdefined(current_module(), ex.args[2]) &&
        isa(getfield(current_module(), ex.args[2]), Module)
end

# dump & xdump - structured tree representation like R's str()
# - dump is for the user-facing structure
# - xdump is for the internal structure
#
# x is the object
# n is the depth of traversal in nested types (5 is the default)
# indent is a character string of spaces that is incremented at
# each descent.
#
# Package writers may overload dump for other nested types like lists
# or DataFrames. If overloaded, check the nesting level (n), and if
# n > 0, dump each component. Limit to the first 10 entries. When
# dumping components, decrement n, and add two spaces to indent.
#
# Package writers should not overload xdump.

function xdump(fn::Function, io::IO, x, n::Int, indent)
    T = typeof(x)
    print(io, T, " ")
    if isa(T, DataType) && length(T.names) > 0
        println(io)
        if n > 0
            for field in T.names
                if field != symbol("")    # prevents segfault if symbol is blank
                    print(io, indent, "  ", field, ": ")
                    if isdefined(x,field)
                        fn(io, getfield(x, field), n - 1, string(indent, "  "))
                    else
                        println(io, undef_ref_str)
                    end
                end
            end
        end
    else
        println(io, x)
    end
end
function xdump(fn::Function, io::IO, x::Module, n::Int, indent)
    print(io, Module, " ")
    println(io, x)
end
function xdump_elts(fn::Function, io::IO, x::Array{Any}, n::Int, indent, i0, i1)
    for i in i0:i1
        print(io, indent, "  ", i, ": ")
        if !isdefined(x,i)
            println(io, undef_ref_str)
        else
            fn(io, x[i], n - 1, string(indent, "  "))
        end
    end
end
function xdump(fn::Function, io::IO, x::Array{Any}, n::Int, indent)
    println(io, "Array($(eltype(x)),$(size(x)))")
    if n > 0
        xdump_elts(fn, io, x, n, indent, 1, (length(x) <= 10 ? length(x) : 5))
        if length(x) > 10
            println(io, indent, "  ...")
            xdump_elts(fn, io, x, n, indent, length(x)-4, length(x))
        end
    end
end
xdump(fn::Function, io::IO, x::Symbol, n::Int, indent) = println(io, typeof(x), " ", x)
xdump(fn::Function, io::IO, x::Function, n::Int, indent) = println(io, x)
xdump(fn::Function, io::IO, x::Array, n::Int, indent) =
               (print(io, "Array($(eltype(x)),$(size(x))) ");
                show(io, x); println(io))

# Types
xdump(fn::Function, io::IO, x::UnionType, n::Int, indent) = println(io, x)
function xdump(fn::Function, io::IO, x::DataType, n::Int, indent)
    println(io, x, "::", typeof(x), " ", " <: ", super(x))
    if n > 0
        for idx in 1:min(10,length(x.names))
            if x.names[idx] != symbol("")    # prevents segfault if symbol is blank
                print(io, indent, "  ", x.names[idx], "::")
                if isa(x.types[idx], DataType)
                    xdump(fn, io, x.types[idx], n - 1, string(indent, "  "))
                else
                    println(io, x.types[idx])
                end
            end
        end
        if length(x.names) > 10
            println(io, indent, "  ...")
        end
    end
end

# dumptype is for displaying abstract type hierarchies like Jameson
# Nash's wiki page: https://github.com/JuliaLang/julia/wiki/Types-Hierarchy

function dumptype(io::IO, x, n::Int, indent)
    # based on Jameson Nash's examples/typetree.jl
    println(io, x)
    if n == 0   # too deeply nested
        return
    end
    typargs(t) = split(string(t), "{")[1]
    # todo: include current module?
    for m in (Core, Base)
        for s in names(m)
            if isdefined(m,s)
                t = eval(m,s)
                if isa(t, TypeConstructor)
                    if string(x.name) == typargs(t) ||
                        ("Union" == split(string(t), "(")[1] &&
                         any(map(tt -> string(x.name) == typargs(tt), t.body.types)))
                        targs = join(t.parameters, ",")
                        println(io, indent, "  ", s,
                                length(t.parameters) > 0 ? "{$targs}" : "",
                                " = ", t)
                    end
                elseif isa(t, UnionType)
                    if any(tt -> string(x.name) == typargs(tt), t.types)
                        println(io, indent, "  ", s, " = ", t)
                    end
                elseif isa(t, DataType) && super(t).name == x.name
                    # type aliases
                    if string(s) != string(t.name)
                        println(io, indent, "  ", s, " = ", t.name)
                    elseif t != Any
                        print(io, indent, "  ")
                        dump(io, t, n - 1, string(indent, "  "))
                    end
                end
            end
        end
    end
end

# For abstract types, use _dumptype only if it's a form that will be called
# interactively.
xdump(fn::Function, io::IO, x::DataType) = x.abstract ? dumptype(io, x, 5, "") : xdump(fn, io, x, 5, "")
xdump(fn::Function, io::IO, x::DataType, n::Int) = x.abstract ? dumptype(io, x, n, "") : xdump(fn, io, x, n, "")

# defaults:
xdump(fn::Function, io::IO, x) = xdump(xdump, io, x, 5, "")  # default is 5 levels
xdump(fn::Function, io::IO, x, n::Int) = xdump(xdump, io, x, n, "")
xdump(fn::Function, io::IO, args...) = error("invalid arguments to xdump")
xdump(fn::Function, args...) = xdump(fn, STDOUT::IO, args...)
xdump(io::IO, args...) = xdump(xdump, io, args...)
xdump(args...) = with_output_limit(()->xdump(xdump, STDOUT::IO, args...), true)

# Here are methods specifically for dump:
dump(io::IO, x, n::Int) = dump(io, x, n, "")
dump(io::IO, x) = dump(io, x, 5, "")  # default is 5 levels
dump(io::IO, x::AbstractString, n::Int, indent) =
               (print(io, typeof(x), " ");
                show(io, x); println(io))
dump(io::IO, x, n::Int, indent) = xdump(dump, io, x, n, indent)
dump(io::IO, args...) = error("invalid arguments to dump")
dump(args...) = with_output_limit(()->dump(STDOUT::IO, args...), true)

function dump(io::IO, x::Dict, n::Int, indent)
    println(io, typeof(x), " len ", length(x))
    if n > 0
        i = 1
        for (k,v) in x
            print(io, indent, "  ", k, ": ")
            dump(io, v, n - 1, string(indent, "  "))
            if i > 10
                println(io, indent, "  ...")
                break
            end
            i += 1
        end
    end
end

# More generic representation for common types:
dump(io::IO, x::DataType, n::Int, indent) = println(io, x.name)
dump(io::IO, x::DataType, n::Int) = dump(io, x, n, "")
dump(io::IO, x::DataType) = dump(io, x, 5, "")
dump(io::IO, x::TypeVar, n::Int, indent) = println(io, x.name)


alignment(x::Any) = (0, length(sprint(showcompact_lim, x)))
alignment(x::Number) = (length(sprint(showcompact_lim, x)), 0)
alignment(x::Integer) = (length(sprint(showcompact_lim, x)), 0)
function alignment(x::Real)
    m = match(r"^(.*?)((?:[\.eE].*)?)$", sprint(showcompact_lim, x))
    m == nothing ? (length(sprint(showcompact_lim, x)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
function alignment(x::Complex)
    m = match(r"^(.*[\+\-])(.*)$", sprint(showcompact_lim, x))
    m == nothing ? (length(sprint(showcompact_lim, x)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
function alignment(x::Rational)
    m = match(r"^(.*?/)(/.*)$", sprint(showcompact_lim, x))
    m == nothing ? (length(sprint(showcompact_lim, x)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end

const undef_ref_str = "#undef"
const undef_ref_alignment = (3,3)

function alignment(
    X::AbstractVecOrMat,
    rows::AbstractVector, cols::AbstractVector,
    cols_if_complete::Integer, cols_otherwise::Integer, sep::Integer
)
    a = []
    for j in cols
        l = r = 0
        for i in rows
            if isassigned(X,i,j)
                aij = alignment(X[i,j])
            else
                aij = undef_ref_alignment
            end
            l = max(l, aij[1])
            r = max(r, aij[2])
        end
        push!(a, (l, r))
        if length(a) > 1 && sum(map(sum,a)) + sep*length(a) >= cols_if_complete
            pop!(a)
            break
        end
    end
    if 1 < length(a) < size(X,2)
        while sum(map(sum,a)) + sep*length(a) >= cols_otherwise
            pop!(a)
        end
    end
    return a
end

function print_matrix_row(io::IO,
    X::AbstractVecOrMat, A::Vector,
    i::Integer, cols::AbstractVector, sep::AbstractString
)
    for k = 1:length(A)
        j = cols[k]
        if isassigned(X,i,j)
            x = X[i,j]
            a = alignment(x)
            sx = sprint(showcompact_lim, x)
        else
            a = undef_ref_alignment
            sx = undef_ref_str
        end
        l = repeat(" ", A[k][1]-a[1])
        r = repeat(" ", A[k][2]-a[2])
        print(io, l, sx, r)
        if k < length(A); print(io, sep); end
    end
end

function print_matrix_vdots(io::IO,
    vdots::AbstractString, A::Vector, sep::AbstractString, M::Integer, m::Integer
)
    for k = 1:length(A)
        w = A[k][1] + A[k][2]
        if k % M == m
            l = repeat(" ", max(0, A[k][1]-length(vdots)))
            r = repeat(" ", max(0, w-length(vdots)-length(l)))
            print(io, l, vdots, r)
        else
            print(io, repeat(" ", w))
        end
        if k < length(A); print(io, sep); end
    end
end

function print_matrix(io::IO, X::AbstractVecOrMat,
                      sz::(Integer, Integer) = (s = tty_size(); (s[1]-4, s[2])),
                      pre::AbstractString = " ",
                      sep::AbstractString = "  ",
                      post::AbstractString = "",
                      hdots::AbstractString = "  \u2026  ",
                      vdots::AbstractString = "\u22ee",
                      ddots::AbstractString = "  \u22f1  ",
                      hmod::Integer = 5, vmod::Integer = 5)
    rows, cols = sz
    cols -= length(pre) + length(post)
    presp = repeat(" ", length(pre))
    postsp = ""
    @assert strwidth(hdots) == strwidth(ddots)
    ss = length(sep)
    m, n = size(X,1), size(X,2)
    if m <= rows # rows fit
        A = alignment(X,1:m,1:n,cols,cols,ss)
        if n <= length(A) # rows and cols fit
            for i = 1:m
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,A,i,1:n,sep)
                print(io, i == m ? post : postsp)
                if i != m; println(io, ); end
            end
        else # rows fit, cols don't
            c = div(cols-length(hdots)+1,2)+1
            R = reverse(alignment(X,1:m,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - length(hdots)
            L = alignment(X,1:m,1:n,c,c,ss)
            for i = 1:m
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,L,i,1:length(L),sep)
                print(io, i % hmod == 1 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,R,i,n-length(R)+1:n,sep)
                print(io, i == m ? post : postsp)
                if i != m; println(io, ); end
            end
        end
    else # rows don't fit
        t = div(rows,2)
        I = [1:t; m-div(rows-1,2)+1:m]
        A = alignment(X,I,1:n,cols,cols,ss)
        if n <= length(A) # rows don't fit, cols do
            for i in I
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,A,i,1:n,sep)
                print(io, i == m ? post : postsp)
                if i != I[end]; println(io, ); end
                if i == t
                    print(io, i == 1 ? pre : presp)
                    print_matrix_vdots(io, vdots,A,sep,vmod,1)
                    println(io, i == m ? post : postsp)
                end
            end
        else # neither rows nor cols fit
            c = div(cols-length(hdots)+1,2)+1
            R = reverse(alignment(X,I,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - length(hdots)
            L = alignment(X,I,1:n,c,c,ss)
            r = mod((length(R)-n+1),vmod)
            for i in I
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,L,i,1:length(L),sep)
                print(io, i % hmod == 1 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,R,i,n-length(R)+1:n,sep)
                print(io, i == m ? post : postsp)
                if i != I[end]; println(io, ); end
                if i == t
                    print(io, i == 1 ? pre : presp)
                    print_matrix_vdots(io, vdots,L,sep,vmod,1)
                    print(io, ddots)
                    print_matrix_vdots(io, vdots,R,sep,vmod,r)
                    println(io, i == m ? post : postsp)
                end
            end
        end
    end
end

summary(x) = string(typeof(x))

dims2string(d) = length(d) == 0 ? "0-dimensional" :
                 length(d) == 1 ? "$(d[1])-element" :
                 join(map(string,d), 'x')

summary(a::AbstractArray) =
    string(dims2string(size(a)), " ", typeof(a))

function show_nd(io::IO, a::AbstractArray, limit, print_matrix, label_slices)
    if isempty(a)
        return
    end
    tail = size(a)[3:end]
    nd = ndims(a)-2
    function print_slice(idxs...)
        if limit
            for i = 1:nd
                ii = idxs[i]
                if size(a,i+2) > 10
                    if ii == 4 && all(x->x==1,idxs[1:i-1])
                        for j=i+1:nd
                            szj = size(a,j+2)
                            if szj>10 && 3 < idxs[j] <= szj-3
                                return
                            end
                        end
                        #println(io, idxs)
                        print(io, "...\n\n")
                        return
                    end
                    if 3 < ii <= size(a,i+2)-3
                        return
                    end
                end
            end
        end
        if label_slices
            print(io, "[:, :, ")
            for i = 1:(nd-1); print(io, "$(idxs[i]), "); end
            println(io, idxs[end], "] =")
        end
        slice = sub(a, 1:size(a,1), 1:size(a,2), idxs...)
        print_matrix(io, slice)
        print(io, idxs == tail ? "" : "\n\n")
    end
    cartesianmap(print_slice, tail)
end

function whos(m::Module, pattern::Regex)
    for v in sort(names(m))
        s = string(v)
        if isdefined(m,v) && ismatch(pattern, s)
            println(rpad(s, 30), summary(eval(m,v)))
        end
    end
end
whos() = whos(r"")
whos(m::Module) = whos(m, r"")
whos(pat::Regex) = whos(current_module(), pat)

# global flag for limiting output
# TODO: this should be replaced with a better mechanism. currently it is only
# for internal use in showing arrays.
_limit_output = false

function print_matrix_repr(io, X::AbstractArray)
    compact, prefix = array_eltype_show_how(X)
    prefix *= "["
    ind = " "^length(prefix)
    print(io, prefix)
    for i=1:size(X,1)
        i > 1 && print(io, ind)
        for j=1:size(X,2)
            j > 1 && print(io, " ")
            if !isassigned(X,i,j)
                print(io, undef_ref_str)
            else
                el = X[i,j]
                compact ? showcompact_lim(io, el) : show(io, el)
            end
        end
        if i < size(X,1)
            println(io)
        else
            print(io, "]")
        end
    end
end

# NOTE: this is a possible, so-far-unexported function, providing control of
# array output. Not sure I want to do it this way.
showarray(X::AbstractArray; kw...) = showarray(STDOUT, X; kw...)
function showarray(io::IO, X::AbstractArray;
                   header::Bool=true, limit::Bool=_limit_output,
                   sz = (s = tty_size(); (s[1]-4, s[2])), repr=false)
    rows, cols = sz
    header && print(io, summary(X))
    if !isempty(X)
        header && println(io, ":")
        if ndims(X) == 0
            if isassigned(X)
                return showcompact_lim(io, X[])
            else
                return print(io, undef_ref_str)
            end
        end
        if !limit
            rows = cols = typemax(Int)
        end
        if repr
            if ndims(X)<=2
                print_matrix_repr(io, X)
            else
                show_nd(io, X, limit, print_matrix_repr, false)
            end
        else
            punct = (" ", "  ", "")
            if ndims(X)<=2
                print_matrix(io, X, sz, punct...)
            else
                show_nd(io, X, limit,
                        (io,slice)->print_matrix(io,slice,sz,punct...),
                        !repr)
            end
        end
    end
end

show(io::IO, X::AbstractArray) = showarray(io, X, header=_limit_output, repr=!_limit_output)

function with_output_limit(thk, lim=true)
    global _limit_output
    last = _limit_output
    _limit_output = lim
    try
        thk()
    finally
        _limit_output = last
    end
end

showall(x) = showall(STDOUT, x)
function showall(io::IO, x)
    if _limit_output==false
        show(io, x)
    else
        with_output_limit(false) do
            show(io, x)
        end
    end
end

showlimited(x) = showlimited(STDOUT, x)
function showlimited(io::IO, x)
    if _limit_output==true
        show(io, x)
    else
        with_output_limit(true) do
            show(io, x)
        end
    end
end

# returns compact, prefix
function array_eltype_show_how(X)
    e = eltype(X)
    leaf = isleaftype(e)
    plain = e<:Number || e<:AbstractString
    if isa(e,DataType) && e === e.name.primary
        str = string(e.name)
    else
        str = string(e)
    end
    leaf&&plain, (!isempty(X) && (e===Float64 || e===Int || (leaf && !plain)) ? "" : str)
end

function show_vector(io::IO, v, opn, cls)
    compact, prefix = array_eltype_show_how(v)
    print(io, prefix)
    if _limit_output && length(v) > 20
        show_delim_array(io, sub(v,1:10), opn, ",", "", false, compact)
        print(io, "  \u2026  ")
        n = length(v)
        show_delim_array(io, sub(v,(n-9):n), "", ",", cls, false, compact)
    else
        show_delim_array(io, v, opn, ",", cls, false)
    end
end

show(io::IO, v::AbstractVector) = show_vector(io, v, "[", "]")

# printing BitArrays

# (following functions not exported - mainly intended for debug)

function print_bit_chunk(io::IO, c::UInt64, l::Integer)
    for s = 0 : l - 1
        d = (c >>> s) & 1
        print(io, "01"[d + 1])
        if (s + 1) & 7 == 0
            print(io, " ")
        end
    end
end

print_bit_chunk(io::IO, c::UInt64) = print_bit_chunk(io, c, 64)

print_bit_chunk(c::UInt64, l::Integer) = print_bit_chunk(STDOUT, c, l)
print_bit_chunk(c::UInt64) = print_bit_chunk(STDOUT, c)

function bitshow(io::IO, B::BitArray)
    if length(B) == 0
        return
    end
    for i = 1 : length(B.chunks) - 1
        print_bit_chunk(io, B.chunks[i])
        print(io, ": ")
    end
    l = (@_mod64 (length(B)-1)) + 1
    print_bit_chunk(io, B.chunks[end], l)
end
bitshow(B::BitArray) = bitshow(STDOUT, B)

bitstring(B::BitArray) = sprint(bitshow, B)
