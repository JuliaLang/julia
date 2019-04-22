is_func_call(x) = false
is_func_call(x::EXPR{Call}) = true
is_func_call(x::EXPR{InvisBrackets}) = is_func_call(x.args[2])
is_func_call(x::UnaryOpCall) = true
is_func_call(x::BinaryOpCall) = true
function is_func_call(x::BinarySyntaxOpCall)
    if is_decl(x.op)
        return is_func_call(x.arg1)
    else
        return false
    end
end
function is_func_call(x::WhereOpCall)
    return is_func_call(x.arg1)
end

is_assignment(x) = x isa BinarySyntaxOpCall && is_eq(x.op)

# OPERATOR
is_exor(x) = x isa OPERATOR && x.kind == Tokens.EX_OR && x.dot == false
is_decl(x) = x isa OPERATOR && x.kind == Tokens.DECLARATION
is_issubt(x) = x isa OPERATOR && x.kind == Tokens.ISSUBTYPE
is_issupt(x) = x isa OPERATOR && x.kind == Tokens.ISSUPERTYPE
is_and(x) = x isa OPERATOR && x.kind == Tokens.AND && x.dot == false
is_not(x) = x isa OPERATOR && x.kind == Tokens.NOT && x.dot == false
is_plus(x) = x isa OPERATOR && x.kind == Tokens.PLUS && x.dot == false
is_minus(x) = x isa OPERATOR && x.kind == Tokens.MINUS && x.dot == false
is_star(x) = x isa OPERATOR && x.kind == Tokens.STAR && x.dot == false
is_eq(x) = x isa OPERATOR && x.kind == Tokens.EQ && x.dot == false
is_dot(x) = x isa OPERATOR && x.kind == Tokens.DOT
is_ddot(x) = x isa OPERATOR && x.kind == Tokens.DDOT
is_dddot(x) = x isa OPERATOR && x.kind == Tokens.DDDOT
is_pairarrow(x) = x isa OPERATOR && x.kind == Tokens.PAIR_ARROW && x.dot == false
is_in(x) = x isa OPERATOR && x.kind == Tokens.IN && x.dot == false
is_elof(x) = x isa OPERATOR && x.kind == Tokens.ELEMENT_OF && x.dot == false
is_colon(x) = x isa OPERATOR && x.kind == Tokens.COLON
is_prime(x) = x isa OPERATOR && x.kind == Tokens.PRIME
is_cond(x) = x isa OPERATOR && x.kind == Tokens.CONDITIONAL
is_where(x) = x isa OPERATOR && x.kind == Tokens.WHERE
is_anon_func(x) = x isa OPERATOR && x.kind == Tokens.ANON_FUNC

# PUNCTUATION
is_punc(x) = x isa PUNCTUATION && 
    x.kind == Tokens.COMMA && 
    x.kind == Tokens.LPAREN &&
    x.kind == Tokens.RPAREN &&
    x.kind == Tokens.LBRACE &&
    x.kind == Tokens.RBRACE &&
    x.kind == Tokens.LSQUARE &&
    x.kind == Tokens.RSQUARE
is_comma(x) = x isa PUNCTUATION && x.kind == Tokens.COMMA
is_lparen(x) = x isa PUNCTUATION && x.kind == Tokens.LPAREN
is_rparen(x) = x isa PUNCTUATION && x.kind == Tokens.RPAREN
is_lbrace(x) = x isa PUNCTUATION && x.kind == Tokens.LBRACE
is_rbrace(x) = x isa PUNCTUATION && x.kind == Tokens.RBRACE
is_lsquare(x) = x isa PUNCTUATION && x.kind == Tokens.LSQUARE
is_rsquare(x) = x isa PUNCTUATION && x.kind == Tokens.RSQUARE

# KEYWORD
is_if(x) = x isa KEYWORD && x.kind == Tokens.IF
is_module(x) = x isa KEYWORD && x.kind == Tokens.MODULE
is_import(x) = x isa KEYWORD && x.kind == Tokens.IMPORT
is_importall(x) = x isa KEYWORD && x.kind == Tokens.IMPORTALL


# Literals

is_lit_string(x) = x isa LITERAL && (x.kind == Tokens.STRING || x.kind == Tokens.TRIPLE_STRING)

is_valid_iterator(x) = false
is_valid_iterator(x::BinarySyntaxOpCall) = is_eq(x.op) 
is_valid_iterator(x::BinaryOpCall) = is_in(x.op) || is_elof(x.op)

_arg_id(x) = x
_arg_id(x::IDENTIFIER) = x
_arg_id(x::EXPR{Quotenode}) = x.args[1]
_arg_id(x::EXPR{Curly}) = _arg_id(x.args[1])
_arg_id(x::EXPR{Kw}) = _arg_id(x.args[1])


function _arg_id(x::UnarySyntaxOpCall)
    if is_dddot(x.arg2)
        return _arg_id(x.arg1)
    else
        return x
    end
end

function _arg_id(x::BinarySyntaxOpCall)
    if is_decl(x.op)
        return _arg_id(x.arg1)
    else
        return x
    end
end
function _arg_id(x::WhereOpCall)
    return _arg_id(x.arg1)
end

function get_where_params(x, params = [])
    return params
end

function get_where_params(x::WhereOpCall, params = [])
    for i = 1:length(x.args)
        a = x.args[i]
        if !(a isa PUNCTUATION)
            param = rem_subtype(a)
            param = rem_curly(param)
            push!(params, str_value(param))
        end
    end
    return params
end

function get_curly_params(x, params = [])
    return params
end
function get_curly_params(x::EXPR{Curly}, params = [])
    for i = 2:length(x.args)
        a = x.args[i]
        if !(a isa PUNCTUATION)
            param = rem_subtype(a)
            push!(params, str_value(param))
        end
    end
    return params
end



function get_sig_params(x, params = [])
    get_where_params(x, params)
    if x isa WhereOpCall && x.arg1 isa WhereOpCall
        get_where_params(x.arg1, params)
    end
    x = rem_where(x)
    x = rem_call(x)
    get_curly_params(x, params)
    return params
end


function rem_subtype(x)
    if x isa BinarySyntaxOpCall && x.op isa OPERATOR && x.op.kind == Tokens.ISSUBTYPE
        return x.arg1
    else
        return x
    end
end

function rem_decl(x)
    if x isa BinarySyntaxOpCall && is_decl(x.op)
        return x.arg1
    else
        return x
    end
end

function rem_curly(x)
    if x isa EXPR{Curly}
        return x.args[1]
    else
        return x
    end
end

function rem_call(x)
    if x isa EXPR{Call}
        return x.args[1]
    else
        return x
    end
end

function rem_where(x)
    if x isa WhereOpCall
        return rem_where(x.arg1)
    else
        return x
    end
end

function rem_invis(x)
    if x isa EXPR{InvisBrackets}
        return x.args[2]
    else
        return x
    end
end

function rem_dddot(x)
    if x isa UnarySyntaxOpCall && is_dddot(x.arg2)
        return x.arg1
    else
        return x
    end
end

function rem_kw(x)
    if x isa EXPR{Kw}
        return x.args[1]
    else
        return x
    end
end

# Definitions
defines_function(x) = false
defines_function(x::EXPR{FunctionDef}) = true
function defines_function(x::BinarySyntaxOpCall)
    if is_eq(x.op)
        sig = x.arg1
        while true
            if sig isa EXPR{Call}
                return true
            elseif sig isa BinarySyntaxOpCall && is_decl(sig.op) || sig isa WhereOpCall
                sig = sig.arg1
            elseif sig isa UnaryOpCall || sig isa UnarySyntaxOpCall# && sig.arg isa BinarySyntaxOpCall && is_decl(sig.arg.op) && sig.arg.arg1 isa EXPR{InvisBrackets}
                return true
            elseif sig isa BinaryOpCall || (sig isa BinarySyntaxOpCall && !(sig.op.kind == Tokens.DOT))
                return true
            else
                return false
            end
        end
    end
    return false
end

defines_macro(x) = false
defines_macro(x::EXPR{Macro}) = true


function defines_datatype(x)
    defines_struct(x) || defines_abstract(x) || defines_primitive(x)
end

defines_struct(x) = x isa EXPR{Struct} || defines_mutable(x)
defines_mutable(x) = x isa EXPR{Mutable}
defines_abstract(x) = x isa EXPR{Abstract}
function defines_primitive(x) 
    x isa EXPR{Primitive}
end

defines_module(x) = x isa EXPR{ModuleH} || x isa EXPR{BareModule}

defines_anon_function(x) = x isa BinarySyntaxOpCall && is_anon_func(x.op)

function has_sig(x)
    defines_datatype(x) || defines_function(x) || defines_macro(x) || defines_anon_function(x)
end


"""
    get_sig(x)

Returns the full signature of function, macro and datatype definitions. 
Should only be called when has_sig(x) == true.
"""
get_sig(x::EXPR{Struct}) = x.args[2]
get_sig(x::EXPR{Mutable}) = x.args[3]
get_sig(x::EXPR{Abstract}) = length(x.args) == 4 ? x.args[3] : x.args[2]
get_sig(x::EXPR{Primitive}) = x.args[3]
get_sig(x::EXPR{FunctionDef}) = x.args[2]
get_sig(x::EXPR{Macro}) = x.args[2]
function get_sig(x::BinarySyntaxOpCall)
    return x.arg1
end

function get_name(x::EXPR{T}) where T <: Union{Struct,Mutable,Abstract,Primitive}
    sig = get_sig(x)
    sig = rem_where(sig)
    sig = rem_subtype(sig)
    sig = rem_curly(sig)
    return sig
end

function get_name(x::EXPR{T}) where T <: Union{ModuleH,BareModule}
    x.args[2]
end

function get_name(sig)
    sig = rem_where(sig)
    sig = rem_decl(sig)
    sig = rem_call(sig)
    sig = rem_curly(sig)
    sig = rem_invis(sig)
    return sig
end

function get_name(x::EXPR{FunctionDef})
    sig = get_sig(x)
    sig = rem_where(sig)
    sig = rem_decl(sig)
    sig = rem_call(sig)
    sig = rem_curly(sig)
    sig = rem_invis(sig)
    return sig
end

function get_name(x::EXPR{Macro})
    sig = get_sig(x)
    sig = rem_call(sig)
    sig = rem_invis(sig)
    return sig
end


function get_name(x::BinarySyntaxOpCall)
    sig = x.arg1
    if sig isa UnaryOpCall 
        return sig.op
    end
    sig = rem_where(sig)
    sig = rem_decl(sig)
    sig = rem_call(sig)
    sig = rem_curly(sig)
    sig = rem_invis(sig)
end

function get_args(x::IDENTIFIER)
    return []
end

function get_args(x)
    if defines_anon_function(x) && !(x.arg1 isa EXPR{TupleH})
        arg = x.arg1
        arg = rem_invis(arg)
        arg = get_arg_name(arg)
        return [arg]
    end
    sig = get_sig(x)
    sig = rem_where(sig)
    sig = rem_decl(sig)
    return get_args(sig)
end

function get_args(sig::EXPR{TupleH})
    args = []
    for i = 2:length(sig.args)
        arg = sig.args[i]
        arg isa PUNCTUATION && continue
        arg isa EXPR{Parameters} && continue
        arg_name = get_arg_name(arg)
        push!(args, arg_name)
    end
    return args
end


function get_args(x::EXPR{Do})
    args = []
    for i = 1:length(x.args[3].args)
        arg = x.args[3].args[i]
        arg isa PUNCTUATION && continue
        arg isa EXPR{Parameters} && continue
        arg_name = get_arg_name(arg)
        push!(args, arg_name)
    end
    return args
end

function get_args(sig::EXPR{Call})
    args = []
    sig = rem_where(sig)
    sig = rem_decl(sig)
    if sig isa EXPR{Call}
        for i = 2:length(sig.args)
            arg = sig.args[i]
            arg isa PUNCTUATION && continue
            if arg isa EXPR{Parameters}
                for j = 1:length(arg.args)
                    parg = arg.args[j]
                    parg isa PUNCTUATION && continue
                    parg_name = get_arg_name(parg)
                    push!(args, parg_name)
                end
            else
                arg_name = get_arg_name(arg)
                push!(args, arg_name)
            end
        end
    else
        error("not sig: $sig")
    end
    return args
end

function get_args(x::EXPR{Struct})
    args = []
    for arg in x.args[3]
        if !defines_function(arg)
            arg = rem_decl(arg)
            push!(args, arg)
        end
    end
    return args
end

function get_args(x::EXPR{Mutable})
    args = []
    for arg in x.args[4]
        if !defines_function(arg)
            arg = rem_decl(arg)
            push!(args, arg)
        end
    end
    return args
end

function get_args(x::EXPR{Flatten})
    get_args(x.args[1])
end

function get_args(x::EXPR{T}) where T <: Union{Generator,Filter}
    args = []
    if x.args[1] isa EXPR{Flatten} || x.args[1] isa EXPR{Generator}
        append!(args, get_args(x.args[1]))
    end

    if x.args[3] isa EXPR{Filter}
        return get_args(x.args[3])
    else
        for i = 3:length(x.args)
            arg = x.args[i]
            if is_valid_iterator(arg)
                arg = rem_decl(arg.arg1)
                arg = flatten_tuple(arg)
                arg = rem_decl.(arg)
                append!(args, arg)
            end
        end
        return args
    end
end


function get_arg_name(arg)
    arg = rem_kw(arg)
    arg = rem_dddot(arg)
    arg = rem_where(arg)
    arg = rem_decl(arg)
    arg = rem_subtype(arg)
    arg = rem_curly(arg)
    arg = rem_invis(arg)
end



function get_arg_type(arg)
    if arg isa BinarySyntaxOpCall && is_decl(arg.op)
        return Expr(arg.arg2)
    else
        return :Any
    end
end

get_body(x::EXPR{ModuleH}) = x.args[3]
get_body(x::EXPR{BareModule}) = x.args[3]
get_body(x::EXPR{If}) = x.args[3]
get_body(x::EXPR{For}) = x.args[3]
get_body(x::EXPR{While}) = x.args[3]
get_body(x::EXPR{FunctionDef}) = x.args[3]
get_body(x::EXPR{Macro}) = x.args[3]
get_body(x::EXPR{Struct}) = x.args[3]
get_body(x::EXPR{Mutable}) = x.args[4]


flatten_tuple(x::EXPR{InvisBrackets}, out = []) = flatten_tuple(x.args[2], out)
function flatten_tuple(x, out = [])
    if x isa EXPR{TupleH}
        for arg in x
            arg isa PUNCTUATION && continue    
            flatten_tuple(arg, out)
        end
    else
        push!(out, x)
    end
    return out
end

"""
    get_id(x)

Get the IDENTIFIER name of a variable, possibly in the presence of 
type declaration operators.
"""
function get_id(x::BinarySyntaxOpCall)
    if is_issubt(x.op) || is_decl(x.op)
        return get_id(x.arg1)
    else
        return x
    end
end

function get_id(x::WhereOpCall)
    return get_id(x.arg1)
end

function get_id(x::UnarySyntaxOpCall)
    if is_dddot(x.arg2)
        return get_id(x.arg1)
    else
        return x
    end
end

get_id(x::EXPR{Curly}) = get_id(x.args[1])
get_id(x::EXPR{InvisBrackets}) = get_id(x.args[2])
get_id(x) = x



"""
    get_t(x)

Basic inference in the presence of type declarations.
"""
get_t(x) = :Any
function get_t(x::BinarySyntaxOpCall) 
    if is_decl(x.op)
        return Expr(x.arg2)
    else
        return :Any
    end
end


infer_t(x) = :Any
function infer_t(x::LITERAL)
    if x.kind == Tokens.INTEGER
        return :Int
    elseif x.kind == Tokens.FLOAT
        return :Float64
    elseif x.kind == Tokens.STRING
        return :String
    elseif x.kind == Tokens.TRIPLE_STRING
        return :String
    elseif x.kind == Tokens.CHAR
        return :Char
    elseif x.kind == Tokens.TRUE || x.kind == Tokens.FALSE
        return :Bool
    elseif x.kind == Tokens.CMD
        return :Cmd
    end
end

infer_t(x::EXPR{Vect}) = :(Array{Any,1})
infer_t(x::EXPR{Vcat}) = :(Array{Any,N})
infer_t(x::EXPR{TypedVcat}) = :(Array{$(Expr(x.args[1])),N})
infer_t(x::EXPR{Hcat}) = :(Array{Any,2})
infer_t(x::EXPR{TypedHcat}) = :(Array{$(Expr(x.args[1])),2})
infer_t(x::EXPR{Quote}) = :Expr
infer_t(x::EXPR{StringH}) = :String
infer_t(x::EXPR{Quotenode}) = :QuoteNode


"""
    contributes_scope(x)
Checks whether the body of `x` is included in the toplevel namespace.
"""
contributes_scope(x) = false
contributes_scope(x::EXPR{FileH}) = true
contributes_scope(x::EXPR{Begin}) = true
contributes_scope(x::EXPR{Block}) = true
contributes_scope(x::EXPR{Const}) = true
contributes_scope(x::EXPR{Global}) = true
contributes_scope(x::EXPR{Local}) = true
contributes_scope(x::EXPR{If}) = true
contributes_scope(x::EXPR{MacroCall}) = true
contributes_scope(x::EXPR{TopLevel}) = true
