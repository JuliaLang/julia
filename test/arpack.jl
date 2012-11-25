cd("../extras") do
require("arpack")

# arpack
begin
local n,a,asym,d,v
n = 10
a = rand(n,n)
asym = a+a'+n*eye(n)

(d,v) = eigs(asym, 3)
@assert sum(asym*v[:,1]-d[1]*v[:,1]) < 1e-8

(d,v) = eigs(a,3)
@assert abs(sum(a*v[:,2]-d[2]*v[:,2])) < 1e-8
end

end # cd
