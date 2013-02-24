ENV["JULIA_PKGDIR"] = string("tmp.",randstring())
@test !isdir(Pkg.dir())
try # ensure directory removal

@test_fails Pkg.cd_pkgdir()
Pkg.init()
@test isdir(Pkg.dir())
Pkg.resolve()

@test length(Pkg.required()) == 0
Pkg.add("Example")
@test length(Pkg.required()) == 1
@test Pkg.required()[1].package == "Example"
Pkg.rm("Example")
@test length(Pkg.required()) == 0

# delete temporary Pkg directory
finally
run(`rm -rf $(Pkg.dir())`)
end
