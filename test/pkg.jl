ENV["JULIA_PKGDIR"] = string("tmp.",randstring())
@test !isdir(Pkg.dir())
try # ensure directory removal
	Pkg.init()
	@test isdir(Pkg.dir())
	Pkg.resolve()

	@test isempty(Pkg.installed())
	Pkg.add("Example")
	@test [keys(Pkg.installed())...] == ["Example"]
	Pkg.rm("Example")
	@test isempty(Pkg.installed())
finally
	run(`rm -rf $(Pkg.dir())`)
end
