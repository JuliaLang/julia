
precedence(op::Int) = op < Tokens.end_assignments ?  AssignmentOp :
                       op < Tokens.end_pairarrow ? 2 :
                       op < Tokens.end_conditional ? ConditionalOp :
                       op < Tokens.end_arrow ?       ArrowOp :
                       op < Tokens.end_lazyor ?      LazyOrOp :
                       op < Tokens.end_lazyand ?     LazyAndOp :
                       op < Tokens.end_comparison ?  ComparisonOp :
                       op < Tokens.end_pipe ?        PipeOp :
                       op < Tokens.end_colon ?       ColonOp :
                       op < Tokens.end_plus ?        PlusOp :
                       op < Tokens.end_bitshifts ?   BitShiftOp :
                       op < Tokens.end_times ?       TimesOp :
                       op < Tokens.end_rational ?    RationalOp :
                       op < Tokens.end_power ?       PowerOp :
                       op < Tokens.end_decl ?        DeclarationOp :
                       op < Tokens.end_where ?       WhereOp : DotOp

precedence(kind::Tokens.Kind) = kind == Tokens.DDDOT ? DddotOp :
                        kind < Tokens.begin_assignments ? 0 :
                        kind < Tokens.end_assignments ?   AssignmentOp :
                        kind < Tokens.end_pairarrow ?   2 :
                       kind < Tokens.end_conditional ?    ConditionalOp :
                       kind < Tokens.end_arrow ?          ArrowOp :
                       kind < Tokens.end_lazyor ?         LazyOrOp :
                       kind < Tokens.end_lazyand ?        LazyAndOp :
                       kind < Tokens.end_comparison ?     ComparisonOp :
                       kind < Tokens.end_pipe ?           PipeOp :
                       kind < Tokens.end_colon ?          ColonOp :
                       kind < Tokens.end_plus ?           PlusOp :
                       kind < Tokens.end_bitshifts ?      BitShiftOp :
                       kind < Tokens.end_times ?          TimesOp :
                       kind < Tokens.end_rational ?       RationalOp :
                       kind < Tokens.end_power ?          PowerOp :
                       kind < Tokens.end_decl ?           DeclarationOp :
                       kind < Tokens.end_where ?          WhereOp :
                       kind < Tokens.end_dot ?            DotOp :
                       kind == Tokens.ANON_FUNC ? AnonFuncOp :
                       kind == Tokens.PRIME ?             PrimeOp : 20

precedence(x) = 0
precedence(x::AbstractToken) = precedence(x.kind)
precedence(x::OPERATOR) = precedence(x.kind)


isoperator(kind) = Tokens.begin_ops < kind < Tokens.end_ops
isoperator(t::AbstractToken) = isoperator(t.kind)


isunaryop(op) = false
isunaryop(op::OPERATOR) = isunaryop(op.kind)
isunaryop(t::AbstractToken) = isunaryop(t.kind)
isunaryop(kind::Tokens.Kind) = kind == Tokens.ISSUBTYPE ||
                  kind == Tokens.ISSUPERTYPE ||
                  kind == Tokens.PLUS ||
                  kind == Tokens.MINUS ||
                  kind == Tokens.NOT ||
                  kind == Tokens.APPROX ||
                  kind == Tokens.NOT_SIGN ||
                  kind == Tokens.AND ||
                  kind == Tokens.SQUARE_ROOT ||
                  kind == Tokens.CUBE_ROOT ||
                  kind == Tokens.QUAD_ROOT ||
                  kind == Tokens.DECLARATION ||
                  kind == Tokens.EX_OR ||
                  kind == Tokens.COLON

isunaryandbinaryop(t) = false
isunaryandbinaryop(t::AbstractToken) = isunaryandbinaryop(t.kind)
isunaryandbinaryop(kind::Tokens.Kind) = kind == Tokens.PLUS ||
                           kind == Tokens.MINUS ||
                           kind == Tokens.EX_OR ||
                           kind == Tokens.ISSUBTYPE ||
                           kind == Tokens.ISSUPERTYPE ||
                           kind == Tokens.AND ||
                           kind == Tokens.APPROX ||
                           kind == Tokens.DECLARATION ||
                           kind == Tokens.COLON

isbinaryop(op) = false
isbinaryop(op::OPERATOR) = isbinaryop(op.kind)
isbinaryop(t::AbstractToken) = isbinaryop(t.kind)
isbinaryop(kind::Tokens.Kind) = isoperator(kind) &&
                    !(kind == Tokens.SQUARE_ROOT ||
                    kind == Tokens.CUBE_ROOT ||
                    kind == Tokens.QUAD_ROOT ||
                    kind == Tokens.NOT ||
                    kind == Tokens.NOT_SIGN)

isassignment(t::AbstractToken) = Tokens.begin_assignments < t.kind < Tokens.end_assignments

function non_dotted_op(t::AbstractToken)
    k = t.kind
    return (k == Tokens.COLON_EQ ||
            k == Tokens.PAIR_ARROW ||
            k == Tokens.EX_OR_EQ ||
            k == Tokens.CONDITIONAL ||
            k == Tokens.LAZY_OR ||
            k == Tokens.LAZY_AND ||
            k == Tokens.ISSUBTYPE ||
            k == Tokens.ISSUPERTYPE ||
            k == Tokens.LPIPE ||
            k == Tokens.RPIPE ||
            k == Tokens.EX_OR ||
            k == Tokens.COLON ||
            k == Tokens.DECLARATION ||
            k == Tokens.IN ||
            k == Tokens.ISA ||
            k == Tokens.WHERE ||
            (isunaryop(k) && !isbinaryop(k) && !(k == Tokens.NOT)))
end


issyntaxcall(op) = false
function issyntaxcall(op::OPERATOR)
    K = op.kind
    P = precedence(K)
    P == AssignmentOp && !(K == Tokens.APPROX || K == Tokens.PAIR_ARROW) ||
    K == Tokens.RIGHT_ARROW ||
    P == LazyOrOp ||
    P == LazyAndOp ||
    K == Tokens.ISSUBTYPE ||
    K == Tokens.ISSUPERTYPE ||
    K == Tokens.COLON ||
    K == Tokens.DECLARATION ||
    K == Tokens.DOT ||
    K == Tokens.DDDOT ||
    K == Tokens.PRIME ||
    K == Tokens.WHERE
end


issyntaxunarycall(op) = false
function issyntaxunarycall(op::OPERATOR)
    K = op.kind
    !op.dot && (K == Tokens.EX_OR ||
    K == Tokens.AND ||
    K == Tokens.DECLARATION ||
    K == Tokens.ISSUBTYPE ||
    K == Tokens.ISSUPERTYPE)
end



LtoR(prec::Int) = AssignmentOp ≤ prec ≤ LazyAndOp || prec == PowerOp


"""
    parse_unary(ps)

Having hit a unary operator at the start of an expression return a call.
"""
function parse_unary(ps::ParseState, op::OPERATOR)
    K,dot = op.kind, op.dot
    if op isa OPERATOR && op.kind == Tokens.COLON
        ret = parse_unary_colon(ps, op)
    elseif (is_plus(op) || is_minus(op)) && (ps.nt.kind == Tokens.INTEGER || ps.nt.kind == Tokens.FLOAT) && isemptyws(ps.ws) && ps.nnt.kind!=Tokens.CIRCUMFLEX_ACCENT
        arg = LITERAL(next(ps))
        ret = LITERAL(op.fullspan + arg.fullspan, (op.fullspan + arg.span), string(is_plus(op) ? "+" : "-" , val(ps.t, ps)), ps.t.kind)
    else
        P = precedence(K)
        prec = P == DeclarationOp ? DeclarationOp :
                    K == Tokens.AND ? DeclarationOp :
                    K == Tokens.EX_OR ? 20 : PowerOp
        arg = @closer ps unary @precedence ps prec parse_expression(ps)
        if issyntaxunarycall(op)
            ret = UnarySyntaxOpCall(op, arg)
        else
            ret = UnaryOpCall(op, arg)
        end
    end

    return ret
end

function parse_unary_colon(ps::ParseState, op::OPERATOR)
    if Tokens.begin_keywords < ps.nt.kind < Tokens.end_keywords
        ret = EXPR{Quotenode}(Any[op, IDENTIFIER(next(ps))])
    elseif Tokens.begin_literal < ps.nt.kind < Tokens.end_literal ||
        isoperator(ps.nt.kind) || ps.nt.kind == Tokens.IDENTIFIER
        ret = EXPR{Quotenode}(Any[op, INSTANCE(next(ps))])
    elseif closer(ps)
        ret = op
    else
        arg = @precedence ps 20 parse_expression(ps)
        ret = EXPR{Quote}(Any[op, arg])
    end
    return ret
end

function parse_operator_eq(ps::ParseState, @nospecialize(ret), op)
    nextarg = @precedence ps AssignmentOp - LtoR(AssignmentOp) parse_expression(ps)

    if is_func_call(ret) && !(nextarg isa EXPR{Begin} || (nextarg isa EXPR{InvisBrackets} && nextarg.args[2] isa EXPR{Block}))
        nextarg = EXPR{Block}(Any[nextarg])
    end
    return BinarySyntaxOpCall(ret, op, nextarg)
end

# Parse conditionals
function parse_operator_cond(ps::ParseState, @nospecialize(ret), op)
    nextarg = @closer ps ifop parse_expression(ps)
    op2 = OPERATOR(next(ps))
    nextarg2 = @closer ps comma @precedence ps 0 parse_expression(ps)

    return ConditionalOpCall(ret, op, nextarg, op2, nextarg2)
end

# Parse comparisons
function parse_comp_operator(ps::ParseState, @nospecialize(ret), op)
    nextarg = @precedence ps ComparisonOp - LtoR(ComparisonOp) parse_expression(ps)

    if ret isa EXPR{Comparison}
        push!(ret, op)
        push!(ret, nextarg)
    elseif ret isa BinaryOpCall && precedence(ret.op) == ComparisonOp
        ret = EXPR{Comparison}(Any[ret.arg1, ret.op, ret.arg2, op, nextarg])
    elseif ret isa BinarySyntaxOpCall && (is_issubt(ret.op) || is_issupt(ret.op))
        ret = EXPR{Comparison}(Any[ret.arg1, ret.op, ret.arg2, op, nextarg])
    elseif (is_issubt(op) || is_issupt(op))
        ret = BinarySyntaxOpCall(ret, op, nextarg)
    else
        ret = BinaryOpCall(ret, op, nextarg)
    end
    return ret
end

# Parse ranges
function parse_operator_colon(ps::ParseState, @nospecialize(ret), op)
    nextarg = @precedence ps ColonOp - LtoR(ColonOp) parse_expression(ps)

    if ret isa BinaryOpCall && is_colon(ret.op)
        ret = EXPR{ColonOpCall}(Any[ret.arg1, ret.op, ret.arg2, op, nextarg])
    else
        ret = BinaryOpCall(ret, op, nextarg)
    end
    return ret
end




# Parse power (special case for preceding unary ops)
function parse_operator_power(ps::ParseState, @nospecialize(ret), op)
    nextarg = @precedence ps PowerOp - LtoR(PowerOp) @closer ps inwhere parse_expression(ps)
    
    if ret isa UnaryOpCall
        nextarg = BinaryOpCall(ret.arg, op, nextarg)
        ret = UnaryOpCall(ret.op, nextarg)
    else
        ret = BinaryOpCall(ret, op, nextarg)
    end
    return ret
end


# parse where
function parse_operator_where(ps::ParseState, @nospecialize(ret), op)
    nextarg = @precedence ps LazyAndOp @closer ps inwhere parse_expression(ps)
    
    if nextarg isa EXPR{Braces}
        args = nextarg.args
    else
        args = Any[nextarg]
    end
    return WhereOpCall(ret, op, args)
end

function parse_operator_dot(ps::ParseState, @nospecialize(ret), op)
    if ps.nt.kind == Tokens.LPAREN
        @static if VERSION > v"1.1-"
            iserred = ps.ws.kind != Tokens.EMPTY_WS
            sig = @default ps parse_call(ps, ret)
            nextarg = EXPR{TupleH}(sig.args[2:end])
            if iserred
                nextarg = ErrorToken(nextarg)
            end
        else
            sig = @default ps parse_call(ps, ret)
            nextarg = EXPR{TupleH}(sig.args[2:end])
        end
    elseif iskw(ps.nt) || ps.nt.kind == Tokens.IN || ps.nt.kind == Tokens.ISA || ps.nt.kind == Tokens.WHERE
        nextarg = IDENTIFIER(next(ps))
    elseif ps.nt.kind == Tokens.COLON
        op2 = OPERATOR(next(ps))
        if ps.nt.kind == Tokens.LPAREN
            nextarg = @closeparen ps @precedence ps DotOp - LtoR(DotOp) parse_expression(ps)
            nextarg = EXPR{Quote}(Any[op2, nextarg])
        else    
            nextarg = @precedence ps DotOp - LtoR(DotOp) parse_unary(ps, op2)
        end
    elseif ps.nt.kind == Tokens.EX_OR && ps.nnt.kind == Tokens.LPAREN
        op2 = OPERATOR(next(ps))
        nextarg = parse_call(ps, op2)
    else
        nextarg = @precedence ps DotOp - LtoR(DotOp) parse_expression(ps)
    end

    if nextarg isa IDENTIFIER || nextarg isa EXPR{Vect} || (nextarg isa UnarySyntaxOpCall && is_exor(nextarg.arg1))
        ret = BinarySyntaxOpCall(ret, op, Quotenode(nextarg))
    elseif nextarg isa EXPR{MacroCall}
        mname = BinarySyntaxOpCall(ret, op, Quotenode(nextarg.args[1]))
        ret = EXPR{MacroCall}(Any[mname])
        for i = 2:length(nextarg.args)
            push!(ret, nextarg.args[i])
        end
    else
        ret = BinarySyntaxOpCall(ret, op, nextarg)
    end
    return ret
end

function parse_operator_anon_func(ps::ParseState, @nospecialize(ret), op)
    arg = @closer ps comma @precedence ps 0 parse_expression(ps)
    
    if !(arg isa EXPR{Begin} || (arg isa EXPR{InvisBrackets} && arg.args[2] isa EXPR{Block}))
        arg = EXPR{Block}(Any[arg])
    end
    return BinarySyntaxOpCall(ret, op, arg)
end

function parse_operator(ps::ParseState, @nospecialize(ret), op)
    K,dot = op.kind, op.dot
    P = precedence(K)

    if ret isa EXPR{ChainOpCall} && (is_star(op) || is_plus(op)) && op.kind == ret.args[2].kind
        nextarg = @precedence ps P - LtoR(P) parse_expression(ps)
        push!(ret, op)
        push!(ret, nextarg)
        ret = ret
    elseif ret isa BinaryOpCall && (is_star(op) || is_plus(op)) && op.kind == ret.op.kind && !ret.op.dot && ret.op.span > 0
        nextarg = @precedence ps P - LtoR(P) parse_expression(ps)
        ret = EXPR{ChainOpCall}(Any[ret.arg1, ret.op, ret.arg2, op, nextarg])
    elseif is_eq(op)
        ret = parse_operator_eq(ps, ret, op)
    elseif is_cond(op)
        ret = parse_operator_cond(ps, ret, op)
    elseif is_colon(op)
        ret = parse_operator_colon(ps, ret, op)
    elseif is_where(op)
        ret = parse_operator_where(ps, ret, op)
    elseif is_anon_func(op)
        ret = parse_operator_anon_func(ps, ret, op)
    elseif is_dot(op)
        ret = parse_operator_dot(ps, ret, op)
    elseif is_dddot(op) || is_prime(op)
        ret = UnarySyntaxOpCall(ret, op)
    elseif P == ComparisonOp
        ret = parse_comp_operator(ps, ret, op)
    elseif P == PowerOp
        ret = parse_operator_power(ps, ret, op)
    else
        ltor = K == Tokens.LPIPE ? true : LtoR(P)
        nextarg = @precedence ps P - ltor parse_expression(ps)
        
        if issyntaxcall(op)
            ret = BinarySyntaxOpCall(ret, op, nextarg)
        else
            ret = BinaryOpCall(ret, op, nextarg)
        end
    end
    return ret
end
