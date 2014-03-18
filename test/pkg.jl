using Base.Test

function temp_pkg_dir(fn::Function)
  # Used in tests below to setup and teardown a sandboxed package directory
  const tmpdir = ENV["JULIA_PKGDIR"] = string("tmp.",randstring())
  @test !isdir(Pkg.dir())
  try
    Pkg.init()
    @test isdir(Pkg.dir())
    Pkg.resolve()

    fn()
  finally
    run(`rm -rf $tmpdir`)
  end
end

# Test adding a removing a package
temp_pkg_dir() do
	@test isempty(Pkg.installed())
	Pkg.add("Example")
	@test [keys(Pkg.installed())...] == ["Example"]
	Pkg.rm("Example")
	@test isempty(Pkg.installed())
end

# testing a package with @test dependencies causes them to be installed
temp_pkg_dir() do
	Pkg.generate("PackageWithTestDependencies", "MIT")
	@test [keys(Pkg.installed())...] == ["PackageWithTestDependencies"]

  mkdir(Pkg.dir("PackageWithTestDependencies","test"))
  open(Pkg.dir("PackageWithTestDependencies","test","REQUIRE"),"a") do f
    println(f,"Example")
  end

  open(Pkg.dir("PackageWithTestDependencies","test","runtests.jl"),"w") do f
    println(f,"")
  end

  Pkg.resolve()
  @test [keys(Pkg.installed())...] == ["PackageWithTestDependencies"]

  Pkg.test("PackageWithTestDependencies")
  @test sort([keys(Pkg.installed())...]) == sort(["PackageWithTestDependencies", "Example"])
end

# testing a package with no run_test.jl errors
temp_pkg_dir() do
	Pkg.generate("PackageWithNoTests", "MIT")

	try
    Pkg.test("PackageWithNoTests")
  catch err
    @test err.msg == "PackageWithNoTests did not provide a test/runtests.jl file"
  end
end

# testing a package with failing tests errors
temp_pkg_dir() do
	Pkg.generate("PackageWithFailingTests", "MIT")

  mkdir(Pkg.dir("PackageWithFailingTests","test"))
  open(Pkg.dir("PackageWithFailingTests", "test", "runtests.jl"),"w") do f
    println(f,"using Base.Test")
    println(f,"@test false")
  end
 
	try
    Pkg.test("PackageWithFailingTests")
  catch err
    @test err.msg == "PackageWithFailingTests had test errors"
  end
end

