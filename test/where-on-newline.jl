using Test

function f(x::I) where I <: Integer
    return x
end

@test f(3) == 3

function g(x::I)
         where I <: Integer
    return x
end

@test g(3) == 3

function h()
    where where where # an expression equal to Any
end

@test h() === Any

function j(x::I) where(Union{} <: I <: Any)
    return x
end

@test j(3) == 3

function k(x::I)
        where (Union{} <: I <: Any)
    return x
end

@test k(3) == 3

function where(x)
    return x
end

function l0(x::I) where(I)
end

@test l0(3) == nothing

function l(x::Integer)
    where(x)
end

@test l(3) == 3

function m(x::I) where I
    where(Union{} <: I <: Any) # a function call: where(true)
end

@test m(3) == true

function n(x::I) where J
    where (Union{} <: I <: Any) # a continuation of the where clause
end

@test n(3) === nothing

function o(where::Integer)
    where
end

@test o(3) == 3

function o2(where::Integer)
    where + 1
end

@test o2(3) == 4

function o3(where::Integer)
    where+1
end

@test o3(3) == 4

function p(integer::where) where where
    where
end

@test p(3) == Int

function q(x::I) where I <:
                       Integer
   return x
end

@test q(3) == 3

function r(x::I) where
                 I <: Integer
   return x
end

@test r(3) == 3

function s0(integer::where)
    where where    # where clause
    where          # expression
end

@test s0(3) == Int

function t0(integer::where) where where; where; end # where clause
function t1(integer::Int);  where where  where; end # where where where expression

@test t0(3) == Int
@test t1(3) == Any

function f(x::I, y::J)
         # I represents the type of x
         where I <: Integer
         # J represents the type of y
         where J <: Real
    return x * y
end

@test f(3, 4) == 12

function u0(integer::A)
    where where where A       # where clause
    where where where where A # expression
    A
end

@test u0(3) == Int

function v(integer::whereA)
    where whereA                   # where clause
    where where whereA where where # expression
    whereA
end

@test v(3) == Int

# -----------------------------------------
#
# if we ever want to enable where-on-next-line for expressions:
#
# -----------------------------------------

#function s1(integer::where)
#    where          # expression
#    where where    # where clause for the expression on the previous line
#end
#
#@test s1(3) == Any

#function u1(integer::where)
#    where where A  # expression
#    where where    # where clause for the expression on the previous line
#end
#
#@test u1(3) == Any

