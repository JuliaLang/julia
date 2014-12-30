# Encapsulate tests in a module, to avoid any scoping problems
# All test code is run in the model root scope
module CartesianTests

using Base.Cartesian, Base.Test

ex = macroexpand(:(@nifs 3 d->(d in dims) d->(println(d))))

@test ex.head == :block
@test length(ex.args) == 3
for i in 1:3
    x = ex.args[i]
    @test x.head == :if
    @test length(x.args) == 2
    @test x.args[1] == :($i in dims)
    @test x.args[2] == :(println($i))
end

ex = macroexpand(:(@nifs 2 d->(d in dims) d->(println(d)) d->println("not "*string(d))))
@test ex.head == :block
@test length(ex.args) == 2
for i in 1:2
    x = ex.args[i]
    @test x.head == :if
    @test length(x.args) == 3
    @test x.args[1] == :($i in dims)
    @test x.args[2] == :(println($i))
    @test x.args[3] == :(println("not "*string($i)))
end

end
