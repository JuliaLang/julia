using Test

load_path = first(Base.LOAD_PATH)

write(joinpath(load_path, "Foo.jl"),
    """
    module Foo
    Base.Experimental.@MethodTable(mt)
    Base.Experimental.@overlay mt sin(x::Int) = 1
    end
    """)

# precompiling Foo serializes the overlay method through the `mt` binding in the module
Foo = Base.require(Main, :Foo)
@test length(Foo.mt) == 1

write(joinpath(load_path, "Bar.jl"),
    """
    module Bar
    Base.Experimental.@MethodTable(mt)
    end
    """)

write(joinpath(load_path, "Baz.jl"),
    """
    module Baz
    using Bar
    Base.Experimental.@overlay Bar.mt sin(x::Int) = 1
    end
    """)

# when referring an method table in another module,
# the overlay method needs to be discovered explicitly
Bar = Base.require(Main, :Bar)
@test length(Bar.mt) == 0
Baz = Base.require(Main, :Baz)
@test length(Bar.mt) == 1