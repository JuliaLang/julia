using Base.REPLCompletions

module CompletionFoo
    module CompletionFoo2

    end
    const bar = 1
    foo() = bar
end

test_complete(s) = completions(s,endof(s))
test_scomplete(s) = shell_completions(s,endof(s))

s = ""
c,r = test_complete(s)
@test in("CompletionFoo",c)
@test isempty(r)
@test s[r] == ""

s = "Comp"
c,r = test_complete(s)
@test in("CompletionFoo",c)
@test r == 1:4
@test s[r] == "Comp"

s = "Main.Comp"
c,r = test_complete(s)
@test in("CompletionFoo",c)
@test r == 6:9
@test s[r] == "Comp"

s = "Main.CompletionFoo."
c,r = test_complete(s)
@test in("bar",c)
@test r == 20:19
@test s[r] == ""

s = "Main.CompletionFoo.f"
c,r = test_complete(s)
@test in("foo",c)
@test r == 20:20
@test s[r] == "f"

# issue #6333
s = "Base.return_types(getin"
c,r = test_complete(s)
@test in("getindex",c)
@test r == 19:23
@test s[r] == "getin"

## Test completion of packages
#mkp(p) = ((@assert !isdir(p)); mkdir(p))
#temp_pkg_dir() do
#    mkp(Pkg.dir("MyAwesomePackage"))
#    mkp(Pkg.dir("CompletionFooPackage"))
#
#    s = "using MyAwesome"
#    c,r = test_complete(s)
#    @test in("MyAwesomePackage",c)
#    @test s[r] == "MyAwesome"
#
#    s = "using Completion"
#    c,r = test_complete(s)
#    @test in("CompletionFoo",c) #The module
#    @test in("CompletionFooPackage",c) #The package
#    @test s[r] == "Completion"
#
#    s = "using CompletionFoo.Completion"
#    c,r = test_complete(s)
#    @test in("CompletionFoo2",c)
#    @test s[r] == "Completion"
#
#    rmdir(Pkg.dir("MyAwesomePackage"))
#    rmdir(Pkg.dir("CompletionFooPackage"))
#end

@unix_only begin
    #Assume that we can rely on the existence and accessibility of /tmp
    s = "/t"
    c,r = test_scomplete("/t")
    @test in("tmp/",c)
    @test r == 2:2
    @test s[r] == "t"

    s = "/tmp"
    c,r = test_scomplete(s)
    @test in("tmp/",c)
    @test r == 2:4
    @test s[r] == "tmp"

    # This should match things that are inside the tmp directory
    if !isdir("/tmp/tmp")
        s = "/tmp/"
        c,r = test_scomplete(s)
        @test !in("tmp/",c)
        @test r == 6:5
        @test s[r] == ""
    end

    s = "cd \$(Pk"
    c,r = test_scomplete(s)
    @test in("Pkg",c)
    @test r == 6:7
    @test s[r] == "Pk"
end
