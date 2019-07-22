using Test

macro placeholder(sym)
    :(const $(esc(sym)) = $(Expr(:new, Core.Placeholder, :(Any[]))))
end

@placeholder Bar1
struct Foo1
    x::Bar1
end
@test length(methods(Foo1)) == 0
@test Foo1.incomplete
struct Bar1
    x::Foo1
end
@test length(methods(Bar1)) == 2
@test length(methods(Foo1)) == 2


@placeholder Bar2
@placeholder Baz2
struct Foo2
    x::Bar2
    y::Baz2
end
@test Foo2.incomplete
struct Bar2
    x::Foo2
end
@test length(methods(Bar2)) == 0
@test Bar2.incomplete
struct Baz2; end
@test !Foo2.incomplete && !Bar2.incomplete
