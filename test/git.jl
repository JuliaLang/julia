import Base.Git

dir = string("tmp.",randstring())
@test !ispath(dir)
mkdir(dir)
@test isdir(dir)
try # ensure directory removal
cd(dir) do

run(`git init -q`)
run(`git commit -q --allow-empty -m "initial empty commit"`)

for a=["unchanged","changed","removed"], b=["tracked","untracked"]
	run(`echo -n before` > "$a.$b")
	b == "tracked" && run(`git add $a.$b`)
end

try Git.transact() do
	for a=["added","changed"], b=["tracked","untracked"]
		run(`rm -f $a.$b`) # FIXME: delete this pending #2640
		run(`echo -n after` > "$a.$b")
		@test isfile("$a.$b")
		@test readall("$a.$b") == "after"
	end
	for b=["tracked","untracked"]
		run(`rm removed.$b`)
		@test !ispath("removed.$b")
	end
	run(`git add added.tracked`)
	throw(nothing)
end catch x
	is(x,nothing) || rethrow()
end

for a=["unchanged","changed","removed"], b=["tracked","untracked"]
	@test isfile("$a.$b")
	@test readall("$a.$b") == "before"
end
for b=["tracked","untracked"]
	@test !ispath("added.$b")
end

end # cd
finally
run(`rm -rf $dir`)
end
