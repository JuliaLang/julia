using Test

incomplete type Bar; end
@test Bar.incomplete
struct Foo
    x::Bar
end
@test !Foo.incomplete
struct Bar
    x::Foo
end
@test !Bar.incomplete
@test Bar.types[1] === Foo
@test Foo.types[1] === Bar

@test_throws ErrorException Core.eval(Module(), quote
    incomplete type Bar{A}; end
    struct Bar; end
end)

@test_throws ErrorException Core.eval(Module(), quote
    incomplete type Bar{A}; end
    struct Bar{A,B}; end
end)

@test_throws ErrorException Core.eval(Module(), quote
    abstract type Foo; end
    incomplete type Bar <: Foo ; end
    struct Bar; end
end)

@test_throws ErrorException Core.eval(Module(), quote
    abstract type Foo1; end
    abstract type Foo2; end
    incomplete type Bar <: Foo1; end
    incomplete type Bar <: Foo2; end
end)

incomplete type FieldNamesTest; end
struct FieldNamesTest
    a::Int64
    b::Int64
end
@test fieldnames(FieldNamesTest) == (:a, :b)
