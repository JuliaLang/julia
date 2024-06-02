struct ToRedefine1
	a::Int
end

previously_defined_method() = ToRedefine1(1)
const old_reference = previously_defined_method()
const old_world_age = Base.get_world_counter()

struct ToRedefine1
	a::Int
	b::Int
end
@test_throws previously_defined_method()

# Test that the binding rename worked
@test !isa(old_reference, ToRedefine1)
@test isa(old_reference, @world(ToRedefine1, old_world_age))

# Test that the lowering of the inner constructor references the type by identity,
# not binding
@test isa(@world(ToRedefine1, old_world_age)(1), @world(ToRedefine1, old_world_age))
