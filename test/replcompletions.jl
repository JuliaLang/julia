# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.REPLCompletions

module CompletionFoo
    type Test_y
        yy
    end
    type Test_x
        xx :: Test_y
    end
    type_test = Test_x(Test_y(1))
    module CompletionFoo2

    end
    const bar = 1
    foo() = bar
    macro foobar()
        :()
    end

    test{T<:Real}(x::T, y::T) = pass
    test(x::Real, y::Real) = pass
    test{T<:Real}(x::AbstractArray{T}, y) = pass
    test(args...) = pass

    test1(x::Type{Float64}) = pass

    test2(x::AbstractString) = pass
    test2(x::Char) = pass

    test3(x::AbstractArray{Int}, y::Int) = pass
    test3(x::AbstractArray{Float64}, y::Float64) = pass

    array = [1, 1]
end

function temp_pkg_dir(fn::Function)
  # Used in tests below to setup and teardown a sandboxed package directory
  const tmpdir = ENV["JULIA_PKGDIR"] = joinpath(tempdir(),randstring())
  @test !isdir(Pkg.dir())
  try
    mkpath(Pkg.dir())
    @test isdir(Pkg.dir())
    fn()
  finally
    rm(tmpdir, recursive=true)
  end
end

test_complete(s) = completions(s,endof(s))
test_scomplete(s) = shell_completions(s,endof(s))
test_bslashcomplete(s) = bslash_completions(s,endof(s))[2]

s = ""
c,r = test_complete(s)
@test "CompletionFoo" in c
@test isempty(r)
@test s[r] == ""

s = "Comp"
c,r = test_complete(s)
@test "CompletionFoo" in c
@test r == 1:4
@test s[r] == "Comp"

s = "Main.Comp"
c,r = test_complete(s)
@test "CompletionFoo" in c
@test r == 6:9
@test s[r] == "Comp"

s = "Main.CompletionFoo."
c,r = test_complete(s)
@test "bar" in c
@test r == 20:19
@test s[r] == ""

s = "Main.CompletionFoo.f"
c,r = test_complete(s)
@test "foo" in c
@test r == 20:20
@test s[r] == "f"
@test !("foobar" in c)

# issue #6424
s = "Main.CompletionFoo.@f"
c,r = test_complete(s)
@test "@foobar" in c
@test r == 20:21
@test s[r] == "@f"
@test !("foo" in c)

s = "Main.CompletionFoo.type_test.x"
c,r = test_complete(s)
@test "xx" in c
@test r == 30:30
@test s[r] == "x"

# To complete on a variable of a type, the type T of the variable
# must be a concrete type, hence Base.isstructtype(T) returns true,
# for the completion to succeed. That why `xx :: Test_y` of `Test_x`.
s = "Main.CompletionFoo.type_test.xx.y"
c,r = test_complete(s)
@test "yy" in c
@test r == 33:33
@test s[r] == "y"

# issue #6333
s = "Base.return_types(getin"
c,r = test_complete(s)
@test "getindex" in c
@test r == 19:23
@test s[r] == "getin"

# inexistent completion inside a string
s = "Pkg.add(\"lol"
c,r,res = test_complete(s)
@test res == false

# test latex symbol completions
s = "\\alpha"
c,r = test_bslashcomplete(s)
@test c[1] == "α"
@test r == 1:length(s)
@test length(c) == 1

# test latex symbol completions after unicode #9209
s = "α\\alpha"
c,r = test_bslashcomplete(s)
@test c[1] == "α"
@test r == 3:sizeof(s)
@test length(c) == 1

# test emoji symbol completions
s = "\\:koala:"
c,r = test_bslashcomplete(s)
@test c[1] == "🐨"
@test r == 1:sizeof(s)
@test length(c) == 1

# test emoji symbol completions after unicode #9209
s = "α\\:koala:"
c,r = test_bslashcomplete(s)
@test c[1] == "🐨"
@test r == 3:sizeof(s)
@test length(c) == 1

# test latex symbol completions in strings should not work when there
# is a backslash in front of `\alpha` because it interferes with path completion on windows
s = "cd(\"path_to_an_empty_folder_should_not_complete_latex\\\\\\alpha"
c,r,res = test_complete(s)
@test length(c) == 0

# test latex symbol completions in strings
s = "\"C:\\\\ \\alpha"
c,r,res = test_complete(s)
@test c[1] == "α"
@test r == 7:12
@test length(c) == 1

s = "\\a"
c, r, res = test_complete(s)
"\\alpha" in c
@test r == 1:2
@test s[r] == "\\a"

# `cd("C:\U should not make the repl crash due to escaping see comment #9137
s = "cd(\"C:\\U"
c,r,res = test_complete(s)

# Test method completions
s = "max("
c, r, res = test_complete(s)
@test !res
@test c[1] == string(start(methods(max)))
@test r == 1:3
@test s[r] == "max"

# Test completion of methods with input args
s = "CompletionFoo.test(1,1, "
c, r, res = test_complete(s)
@test !res
@test c[1] == string(methods(CompletionFoo.test, Tuple{Int, Int})[1])
@test length(c) == 3
@test r == 1:18
@test s[r] == "CompletionFoo.test"

s = "CompletionFoo.test(CompletionFoo.array,"
c, r, res = test_complete(s)
@test !res
@test c[1] == string(methods(CompletionFoo.test, Tuple{Array{Int, 1}, Any})[1])
@test length(c) == 2
@test r == 1:18
@test s[r] == "CompletionFoo.test"

s = "CompletionFoo.test(1,1,1,"
c, r, res = test_complete(s)
@test !res
@test c[1] == string(methods(CompletionFoo.test, Tuple{Any, Any, Any})[1])
@test r == 1:18
@test s[r] == "CompletionFoo.test"

s = "CompletionFoo.test1(Int,"
c, r, res = test_complete(s)
@test !res
@test length(c) == 0
@test r == 1:19
@test s[r] == "CompletionFoo.test1"

s = "CompletionFoo.test1(Float64,"
c, r, res = test_complete(s)
@test !res
@test length(c) == 1
@test r == 1:19
@test s[r] == "CompletionFoo.test1"

s = "prevind(\"θ\",1,"
c, r, res = test_complete(s)
@test c[1] == string(methods(prevind, Tuple{UTF8String, Int})[1])
@test r == 1:7
@test s[r] == "prevind"

for (T, arg) in [(ASCIIString,"\")\""),(Char, "')'")]
    s = "(1, CompletionFoo.test2($arg,"
    c, r, res = test_complete(s)
    @test length(c) == 1
    @test c[1] == string(methods(CompletionFoo.test2, Tuple{T,})[1])
    @test r == 5:23
    @test s[r] == "CompletionFoo.test2"
end

s = "CompletionFoo.test3([1.,2.],"
c, r, res = test_complete(s)
@test !res
@test length(c) == 2

s = "CompletionFoo.test3([1.,2.], 1.,"
c, r, res = test_complete(s)
@test !res
@test c[1] == string(methods(CompletionFoo.test3, Tuple{Array{Float64, 1}, Float64})[1])
@test r == 1:19
@test s[r] == "CompletionFoo.test3"

# Test completion in multi-line comments
s = "#=\n\\alpha"
c, r, res = test_complete(s)
@test c[1] == "α"
@test r == 4:9
@test length(c) == 1

# Test that completion do not work in multi-line comments
s = "#=\nmax"
c, r, res = test_complete(s)
@test length(c) == 0

# Test completion of packages
mkp(p) = ((@assert !isdir(p)); mkpath(p))
temp_pkg_dir() do
    # Complete <Mod>/src/<Mod>.jl and <Mod>.jl/src/<Mod>.jl
    # but not <Mod>/ if no corresponding .jl file is found
    pkg_dir = Pkg.dir("CompletionFooPackage", "src")
    mkp(pkg_dir)
    touch(joinpath(pkg_dir, "CompletionFooPackage.jl"))

    pkg_dir = Pkg.dir("CompletionFooPackage2.jl", "src")
    mkp(pkg_dir)
    touch(joinpath(pkg_dir, "CompletionFooPackage2.jl"))

    touch(Pkg.dir("CompletionFooPackage3.jl"))

    mkp(Pkg.dir("CompletionFooPackageNone"))
    mkp(Pkg.dir("CompletionFooPackageNone2.jl"))

    s = "using Completion"
    c,r = test_complete(s)
    @test "CompletionFoo" in c #The module
    @test "CompletionFooPackage" in c #The package
    @test "CompletionFooPackage2" in c #The package
    @test "CompletionFooPackage3" in c #The package
    @test !("CompletionFooPackageNone" in c) #The package
    @test !("CompletionFooPackageNone2" in c) #The package
    @test s[r] == "Completion"
end

path = joinpath(tempdir(),randstring())
push!(LOAD_PATH, path)
try
    # Should not throw an error even though the path do no exist
    test_complete("using ")
    Pack_folder = joinpath(path, "Test_pack")
    mkpath(Pack_folder)

    Pack_folder2 = joinpath(path, "Test_pack2", "src")
    mkpath(Pack_folder2)
    touch(joinpath(Pack_folder2, "Test_pack2.jl"))

    # Test it completes on folders
    c,r,res = test_complete("using Test_p")
    @test !("Test_pack" in c)
    @test "Test_pack2" in c

    # Test that it also completes on .jl files in pwd()
    cd(Pack_folder) do
        open("Text.txt","w") do f end
        open("Pack.jl","w") do f end
        c,r,res = test_complete("using ")
        @test "Pack" in c
        @test !("Text.txt" in c)
    end
finally
    @test pop!(LOAD_PATH) == path
    rm(path, recursive=true)
end

# Test $ in shell-mode
s = "cd \$(max"
c, r, res = test_scomplete(s)
@test "max" in c
@test r == 6:8
@test s[r] == "max"

# The return type is of importance, before #8995 it would return nothing
# which would raise an error in the repl code.
@test (UTF8String[], 0:-1, false) == test_scomplete("\$a")

@unix_only begin
    #Assume that we can rely on the existence and accessibility of /tmp

    # Tests path in Julia code and closing " if it's a file
    # Issue #8047
    s = "@show \"/dev/nul"
    c,r = test_complete(s)
    @test "null\"" in c
    @test r == 13:15
    @test s[r] == "nul"

    # Tests path in Julia code and not closing " if it's a directory
    # Issue #8047
    s = "@show \"/tm"
    c,r = test_complete(s)
    @test "tmp/" in c
    @test r == 9:10
    @test s[r] == "tm"

    # Tests path in Julia code and not double-closing "
    # Issue #8047
    s = "@show \"/dev/nul\""
    c,r = completions(s, 15)
    @test "null" in c
    @test r == 13:15
    @test s[r] == "nul"

    s = "/t"
    c,r = test_scomplete(s)
    @test "tmp/" in c
    @test r == 2:2
    @test s[r] == "t"

    s = "/tmp"
    c,r = test_scomplete(s)
    @test "tmp/" in c
    @test r == 2:4
    @test s[r] == "tmp"

    # This should match things that are inside the tmp directory
    if !isdir("/tmp/tmp")
        s = "/tmp/"
        c,r = test_scomplete(s)
        @test !("tmp/" in c)
        @test r == 6:5
        @test s[r] == ""
    end

    s = "cd \$(Pk"
    c,r = test_scomplete(s)
    @test "Pkg" in c
    @test r == 6:7
    @test s[r] == "Pk"

    # Tests homedir expansion
    let
        path = homedir()
        dir = joinpath(path, "tmpfoobar")
        mkdir(dir)
        s = "\"~/tmpfoob"
        c,r = test_complete(s)
        @test "tmpfoobar/" in c
        @test r == 4:10
        @test s[r] == "tmpfoob"
        rm(dir)
    end

    # Tests detecting of files in the env path (in shell mode)
    let
        oldpath = ENV["PATH"]
        path = tempdir()
        ENV["PATH"] = path
        file = joinpath(path, "tmp-executable")
        touch(file)
        chmod(file, 0o755)
        s = "tmp-execu"
        c,r = test_scomplete(s)
        @test "tmp-executable" in c
        @test r == 1:9
        @test s[r] == "tmp-execu"
        rm(file)
        ENV["PATH"] = oldpath
    end

end

let #test that it can auto complete with spaces in file/path
    path = tempdir()
    space_folder = randstring() * " α"
    dir = joinpath(path, space_folder)
    dir_space = replace(space_folder, " ", "\\ ")
    mkdir(dir)
    cd(path) do
        open(joinpath(space_folder, "space .file"),"w") do f
            s = @windows? "rm $dir_space\\\\space" : "cd $dir_space/space"
            c,r = test_scomplete(s)
            @test r == endof(s)-4:endof(s)
            @test "space\\ .file" in c

            s = @windows? "cd(\"β $dir_space\\\\space" : "cd(\"β $dir_space/space"
            c,r = test_complete(s)
            @test r == endof(s)-4:endof(s)
            @test "space\\ .file\"" in c
        end
        # Test for issue #10324
        s = "cd(\"$dir_space"
        c,r = test_complete(s)
        @test r == 5:15
        @test s[r] ==  dir_space
    end
    rm(dir, recursive=true)
end

@windows_only begin
    tmp = tempname()
    path = dirname(tmp)
    file = basename(tmp)
    temp_name = basename(path)
    cd(path) do
        s = "cd ..\\\\"
        c,r = test_scomplete(s)
        @test r == length(s)+1:length(s)
        @test temp_name * "\\\\" in c

        s = "ls $(file[1:2])"
        c,r = test_scomplete(s)
        @test r == length(s)-1:length(s)
        @test file in c

        s = "cd(\"..\\"
        c,r = test_complete(s)
        @test r == length(s)+1:length(s)
        @test temp_name * "\\\\" in c

        s = "cd(\"$(file[1:2])"
        c,r = test_complete(s)
        @test r == length(s) - 1:length(s)
        @test file  in c
    end
    rm(tmp)
end
