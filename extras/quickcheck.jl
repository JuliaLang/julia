# A Julia implementation of QuickCheck, a specification-based tester
#
# QuickCheck was originally written for Haskell by Koen Claessen and John Hughes
# http://www.cse.chalmers.se/~rjmh/QuickCheck/

module QuickCheck

export property
export condproperty
export quantproperty

import Base.*

function lambda_arg_types(f::Function)
    if !isa(f.code, LambdaStaticData)
        error("Property must be expressed as an anonymous function")
    end
    [eval(var.args[2]) for var in f.code.ast.args[1]]
end

# Simple properties
function property(f::Function, ntests)
    typs = lambda_arg_types(f)
    arggens = [size -> generator(typ, size) for typ in typs]
    quantproperty(f, ntests, arggens...)
end
property(f::Function) = property(f, 100)

# Conditional properties
function condproperty(f::Function, ntests, maxtests, argconds...)
    typs = lambda_arg_types(f)
    arggens = [size -> generator(typ, size) for typ in typs]
    check_property(f, arggens, argconds, ntests, maxtests)
end

# Quantified properties (custom generators)
function quantproperty(f::Function, ntests, arggens...)
    if !isa(f.code, LambdaStaticData)
        error("Property must be expressed as an anonymous function")
    end
    argconds = [_->true for n in f.code.ast.args[1]]
    check_property(f, arggens, argconds, ntests, ntests)
end

function check_property(f::Function, arggens, argconds, ntests, maxtests)
    totalTests = 0
    for i in 1:ntests
        goodargs = false
        args = {}
        while !goodargs
            totalTests += 1
            if totalTests > maxtests
                println("Arguments exhausted after $i tests.")
                return
            end
            args = [arggen(div(i,2)+3) for arggen in arggens]
            goodargs = all([apply(x[1], tuple(x[2])) for x in zip(argconds, args)])
        end
        if !f(args...)
            error("Falsifiable, after $i tests:\n$args")
        end
    end
    println("OK, passed $ntests tests.")
end

# Default generators for primitive types
generator{T<:Unsigned}(::Type{T}, size) = convert(T, randi(size))
generator{T<:Signed}(::Type{T}, size) = convert(T, randi((-size, size)))
generator{T<:FloatingPoint}(::Type{T}, size) = convert(T, (rand()-0.5).*size)
# This won't generate interesting UTF-8, but doing that is a Hard Problem
generator{T<:String}(::Type{T}, size) = convert(T, randstring(size))
generator(::Type{Any}, size) = error("Property variables cannot by typed Any.")

# Generator for array types
function generator{T,n}(::Type{Array{T,n}}, size)
    reshape([generator(T, size) for x in 1:(size^n)], [size for i in 1:n]...)
end

# Generator for composite types
function generator{C}(::Type{C}, size)
    if !isa(C, CompositeKind)
        error("Type $C is not a composite type.")
    end
    C([generator(T, size) for T in C.types]...)
end

end