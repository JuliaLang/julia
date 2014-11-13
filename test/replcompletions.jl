using Base.REPLCompletions

module CompletionFoo
    module CompletionFoo2

    end
    const bar = 1
    foo() = bar
    macro foobar()
        :()
    end
end

test_complete(s) = completions(s,endof(s))
test_scomplete(s) = shell_completions(s,endof(s))
test_latexcomplete(s) = latex_completions(s,endof(s))[2]

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
c,r = test_latexcomplete(s)
@test c[1] == "α"
@test r == 1:length(s)
@test length(c) == 1

## Test completion of packages
#mkp(p) = ((@assert !isdir(p)); mkdir(p))
#temp_pkg_dir() do
#    mkp(Pkg.dir("MyAwesomePackage"))
#    mkp(Pkg.dir("CompletionFooPackage"))
#
#    s = "using MyAwesome"
#    c,r = test_complete(s)
#    @test "MyAwesomePackage" in c
#    @test s[r] == "MyAwesome"
#
#    s = "using Completion"
#    c,r = test_complete(s)
#    @test "CompletionFoo" in c #The module
#    @test "CompletionFooPackage" in c #The package
#    @test s[r] == "Completion"
#
#    s = "using CompletionFoo.Completion"
#    c,r = test_complete(s)
#    @test "CompletionFoo2" in c
#    @test s[r] == "Completion"
#
#    rm(Pkg.dir("MyAwesomePackage"))
#    rm(Pkg.dir("CompletionFooPackage"))
#end

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
