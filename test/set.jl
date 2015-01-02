x=Set(1,2,3)
y=Set(1,4,5,6)
z=Set(1,2,3,4,5,6)

#Testing the basic functionality of the set type
@test intersect(x,y)==Set(1)
@test union(x,y)==z
@test isempty(x)==false
@test isempty(Set())
@test length(x)==3
@test length(Set())==0
@test in(1,x)
@test in(5,x)==false
@test setdiff(z,x)==Set(4,5,6)
@test union!(copy(x),[4,5,6])==z
@test setdiff!(z,[4,5,6])==Set(1,2,3)
@test unique([1,2,3,4,1,2,3])==[1,2,3,4]

#Testing the filter functions
function iseven(x)
	if(x%2==0)
		return true
	else
		return false
	end
end
@test filter!(iseven,x)==Set(2)
@test filter(iseven,x)==Set(2)

