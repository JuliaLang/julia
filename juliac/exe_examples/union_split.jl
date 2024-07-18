module Main2
const N = 9 # Number of subtypes/methods

abstract type MyAbstractType end

for i in 1:N
    @eval begin
        struct $(Symbol("MyType$i")) <: MyAbstractType
        end
        union_split_baz(::$(Symbol("MyType$i"))) = $i
    end
end

const arr = MyAbstractType[]

@noinline function union_split_baz(@nospecialize(x::MyAbstractType))
    67
end

@noinline function foo()
    for i in 1:N
        push!(arr,MyType1())
    end
end

@noinline function bar()
    out = Int[]
    for i in 1:N
        push!(out,union_split_baz(arr[i])::Int)
    end
    out
end

Base.@ccallable function main()::Cint
    foo()
    ccall(:jl_, Cvoid, (Any,) ,bar())
    return 0
end
end
