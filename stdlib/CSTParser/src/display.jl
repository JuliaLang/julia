function Base.show(io::IO, x::EXPR{T}, d = 0, er = false) where T
    printstyled(io, " "^d, T.name.name, "  ", x.fullspan, " (", x.span, ")\n", color = er ? :red : :normal)
    for a in x.args
        show(io, a, d + 1, er)
    end
end
function Base.show(io::IO, x::EXPR{ErrorToken}, d = 0, er = false)
    if isempty(x.args)
        printstyled(io, " "^d, "ErrorToken\n", color = :red )
    else
        for a in x.args
            show(io, a, d, true)
        end
    end
end


function Base.show(io::IO, x::T, d = 0, er = false) where T <: Union{BinaryOpCall,BinarySyntaxOpCall,UnaryOpCall,UnarySyntaxOpCall,ConditionalOpCall,WhereOpCall}
    printstyled(io, " "^d, T.name.name, "  ", x.fullspan, " (", x.span, ")\n", color = er ? :red : :normal)
    for a in x
        show(io, a, d + 1, er)
    end
end

function Base.show(io::IO, x::IDENTIFIER, d = 0, er = false) 
    printstyled(io, " "^d, "ID: ", x.val, "  ", x.fullspan, " (", x.span, ")\n", color = er ? :red : :normal)
end

function Base.show(io::IO, x::PUNCTUATION, d = 0, er = false) 
    c =  er ? :red : :normal
    if x.kind == Tokens.LPAREN
        printstyled(io, " "^d, "(\n", color = c)
    elseif x.kind == Tokens.RPAREN
        printstyled(io, " "^d, ")\n", color = c)
    elseif x.kind == Tokens.LSQUARE
        printstyled(io, " "^d, "[\n", color = c)
    elseif x.kind == Tokens.RSQUARE
        printstyled(io, " "^d, "]\n", color = c)
    elseif x.kind == Tokens.COMMA
        printstyled(io, " "^d, ",\n", color = c)
    else
        printstyled(io, " "^d, "PUNC: ", x.kind, "  ", x.fullspan, " (", x.span, ")\n", color = c)
    end
end

function Base.show(io::IO, x::OPERATOR, d = 0, er = false) 
    printstyled(io, " "^d, "OP: ", x.kind, "  ", x.fullspan, " (", x.span, ")\n", color = er ? :red : :normal)
end

function Base.show(io::IO, x::LITERAL, d = 0, er = false) 
    printstyled(io, " "^d, "LITERAL: ", x.val, "  ", x.fullspan, " (", x.span, ")\n", color = er ? :red : :normal)
end

function Base.show(io::IO, x::KEYWORD, d = 0, er = false) 
    printstyled(io, " "^d, x.kind, "  ", x.fullspan, " (", x.span, ")\n", color = er ? :red : :normal)
end