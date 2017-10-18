# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.REPLCompletions

let ex = quote
        module CompletionFoo
        mutable struct Test_y
            yy
        end
        mutable struct Test_x
            xx :: Test_y
        end
        type_test = Test_x(Test_y(1))
        (::Test_y)() = "", ""
        module CompletionFoo2

        end
        const bar = 1
        foo() = bar
        macro foobar()
            :()
        end

        # Support non-Dict Associatives, #19441
        mutable struct CustomDict{K, V} <: Associative{K, V}
            mydict::Dict{K, V}
        end

        Base.keys(d::CustomDict) = collect(keys(d.mydict))
        Base.length(d::CustomDict) = length(d.mydict)

        test(x::T, y::T) where {T<:Real} = pass
        test(x::Real, y::Real) = pass
        test(x::AbstractArray{T}, y) where {T<:Real} = pass
        test(args...) = pass

        test1(x::Type{Float64}) = pass

        test2(x::AbstractString) = pass
        test2(x::Char) = pass
        test2(x::Cmd) = pass

        test3(x::AbstractArray{Int}, y::Int) = pass
        test3(x::AbstractArray{Float64}, y::Float64) = pass

        test4(x::AbstractString, y::AbstractString) = pass
        test4(x::AbstractString, y::Regex) = pass

        test5(x::Array{Bool,1}) = pass
        test5(x::BitArray{1}) = pass
        test5(x::Float64) = pass
        const a=x->x
        test6()=[a, a]
        test7() = rand() > 0.5 ? 1 : 1.0
        test8() = Any[1][1]
        kwtest(; x=1, y=2, w...) = pass

        array = [1, 1]
        varfloat = 0.1

        const tuple = (1, 2)

        test_y_array=[CompletionFoo.Test_y(rand()) for i in 1:10]
        test_dict = Dict("abc"=>1, "abcd"=>10, :bar=>2, :bar2=>9, Base=>3,
                         contains=>4, `ls`=>5, 66=>7, 67=>8, ("q",3)=>11,
                         "α"=>12, :α=>13)
        test_customdict = CustomDict(test_dict)

        macro teststr_str(s) end
        macro tϵsτstρ_str(s) end
        macro testcmd_cmd(s) end
        macro tϵsτcmδ_cmd(s) end

        end
        test_repl_comp_dict = CompletionFoo.test_dict
        test_repl_comp_customdict = CompletionFoo.test_customdict
        test_dict_ℂ = Dict(1=>2)
    end
    ex.head = :toplevel
    eval(Main, ex)
end

function temp_pkg_dir_noinit(fn::Function)
    # Used in tests below to set up and tear down a sandboxed package directory
    # Unlike the version in test/pkg.jl, this does not run Pkg.init so does not
    # clone METADATA (only pkg and libgit2-online tests should need internet access)
    tmpdir = joinpath(tempdir(),randstring())
    withenv("JULIA_PKGDIR" => tmpdir) do
        @test !isdir(Pkg.dir())
        try
            mkpath(Pkg.dir())
            @test isdir(Pkg.dir())
            fn()
        finally
            rm(tmpdir, recursive=true)
        end
    end
end

test_complete(s) = completions(s,endof(s))
test_scomplete(s) = shell_completions(s,endof(s))
test_bslashcomplete(s) = bslash_completions(s,endof(s))[2]

let s = ""
    c, r = test_complete(s)
    @test "CompletionFoo" in c
    @test isempty(r)
    @test s[r] == ""
end

let s = "Comp"
    c, r = test_complete(s)
    @test "CompletionFoo" in c
    @test r == 1:4
    @test s[r] == "Comp"
end

let s = "Main.Comp"
    c, r = test_complete(s)
    @test "CompletionFoo" in c
    @test r == 6:9
    @test s[r] == "Comp"
end

let s = "Main.CompletionFoo."
    c, r = test_complete(s)
    @test "bar" in c
    @test r == 20:19
    @test s[r] == ""
end

let s = "Main.CompletionFoo.f"
    c, r = test_complete(s)
    @test "foo" in c
    @test r == 20:20
    @test s[r] == "f"
    @test !("foobar" in c)
end

# issue #6424
let s = "Main.CompletionFoo.@f"
    c, r = test_complete(s)
    @test "@foobar" in c
    @test r == 20:21
    @test s[r] == "@f"
    @test !("foo" in c)
end

let s = "Main.CompletionFoo.type_test.x"
    c, r = test_complete(s)
    @test "xx" in c
    @test r == 30:30
    @test s[r] == "x"
end

let s = "Main.CompletionFoo.bar.no_val_available"
    c, r = test_complete(s)
    @test length(c)==0
end

#cannot do dot completion on infix operator
let s = "+."
    c, r = test_complete(s)
    @test length(c)==0
end

# To complete on a variable of a type, the type T of the variable
# must be a concrete type, hence Base.isstructtype(T) returns true,
# for the completion to succeed. That why `xx :: Test_y` of `Test_x`.
let s = "Main.CompletionFoo.type_test.xx.y"
    c, r = test_complete(s)
    @test "yy" in c
    @test r == 33:33
    @test s[r] == "y"
end

# issue #6333
let s = "Base.return_types(getin"
    c, r = test_complete(s)
    @test "getindex" in c
    @test r == 19:23
    @test s[r] == "getin"
end

# issue #23193: after `using`, identifiers can be prefixed by module names
let s = "using Test, Base.Random"
    c, r = test_complete(s)
    @test !("RandomDevice" in c)
end

# issue #23226: identifiers must be separated by a comma (not a newline)
let s = "using Base\nusi"
    c, r = test_complete(s)
    @test "using" in c
end

# issue 23292
let
    @test_nowarn test_complete("test7().")
    c, r = test_complete("test8().")
    @test isempty(c)
end

# inexistent completion inside a string
let s = "Pkg.add(\"lol"
    c, r, res = test_complete(s)
    @test res == false
end

# test latex symbol completions
let s = "\\alpha"
    c, r = test_bslashcomplete(s)
    @test c[1] == "α"
    @test r == 1:length(s)
    @test length(c) == 1
end

# test latex symbol completions after unicode #9209
let s = "α\\alpha"
    c, r = test_bslashcomplete(s)
    @test c[1] == "α"
    @test r == 3:sizeof(s)
    @test length(c) == 1
end

# test emoji symbol completions
let s = "\\:koala:"
    c, r = test_bslashcomplete(s)
    @test c[1] == "🐨"
    @test r == 1:sizeof(s)
    @test length(c) == 1
end

let s = "\\:ko"
    c, r = test_bslashcomplete(s)
    @test "\\:koala:" in c
end

# test emoji symbol completions after unicode #9209
let s = "α\\:koala:"
    c, r = test_bslashcomplete(s)
    @test c[1] == "🐨"
    @test r == 3:sizeof(s)
    @test length(c) == 1
end

# test latex symbol completions in strings should not work when there
# is a backslash in front of `\alpha` because it interferes with path completion on windows
let s = "cd(\"path_to_an_empty_folder_should_not_complete_latex\\\\\\alpha"
    c, r, res = test_complete(s)
    @test length(c) == 0
end

# test latex symbol completions in strings
let s = "\"C:\\\\ \\alpha"
    c, r, res = test_complete(s)
    @test c[1] == "α"
    @test r == 7:12
    @test length(c) == 1
end

let s = "\\a"
    c, r, res = test_complete(s)
    "\\alpha" in c
    @test r == 1:2
    @test s[r] == "\\a"
end

# `cd("C:\U should not make the repl crash due to escaping see comment #9137
let s = "cd(\"C:\\U"
    c, r, res = test_complete(s)
end

# Test method completions
let s = "max("
    c, r, res = test_complete(s)
    @test !res
    @test let found = false
        for m in methods(max)
            if !found
                found = (c[1] == string(m))
            end
        end
        found
    end
    @test r == 1:3
    @test s[r] == "max"
end

# Test completion of methods with input concrete args and args where typeinference determine their type
let s = "CompletionFoo.test(1,1, "
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test, Tuple{Int, Int})))
    @test length(c) == 3
    @test r == 1:18
    @test s[r] == "CompletionFoo.test"
end

let s = "CompletionFoo.test(CompletionFoo.array,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test, Tuple{Array{Int, 1}, Any})))
    @test length(c) == 2
    @test r == 1:18
    @test s[r] == "CompletionFoo.test"
end

let s = "CompletionFoo.test(1,1,1,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test, Tuple{Any, Any, Any})))
    @test r == 1:18
    @test s[r] == "CompletionFoo.test"
end

let s = "CompletionFoo.test1(Int,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 0
    @test r == 1:19
    @test s[r] == "CompletionFoo.test1"
end

let s = "CompletionFoo.test1(Float64,"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test r == 1:19
    @test s[r] == "CompletionFoo.test1"
end

let s = "prevind(\"θ\",1,"
    c, r, res = test_complete(s)
    @test c[1] == string(first(methods(prevind, Tuple{String, Int})))
    @test r == 1:7
    @test s[r] == "prevind"
end

for (T, arg) in [(String,"\")\""),(Char, "')'")]
    s = "(1, CompletionFoo.test2($arg,"
    c, r, res = test_complete(s)
    @test length(c) == 1
    @test c[1] == string(first(methods(Main.CompletionFoo.test2, Tuple{T,})))
    @test r == 5:23
    @test s[r] == "CompletionFoo.test2"
end

let s = "(1, CompletionFoo.test2(`')'`,"
    c, r, res = test_complete(s)
    @test c[1] == string(first(methods(Main.CompletionFoo.test2, Tuple{Cmd})))
    @test length(c) == 1
end

let s = "CompletionFoo.test3([1, 2] + CompletionFoo.varfloat,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test3, Tuple{Array{Float64, 1}, Float64})))
    @test length(c) == 1
end

let s = "CompletionFoo.test3([1.,2.], 1.,"
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test3, Tuple{Array{Float64, 1}, Float64})))
    @test r == 1:19
    @test length(c) == 1
    @test s[r] == "CompletionFoo.test3"
end

let s = "CompletionFoo.test4(\"e\",r\" \","
    c, r, res = test_complete(s)
    @test !res
    @test c[1] == string(first(methods(Main.CompletionFoo.test4, Tuple{String, Regex})))
    @test r == 1:19
    @test length(c) == 1
    @test s[r] == "CompletionFoo.test4"
end

# (As discussed in #19829, the Base.REPLCompletions.get_type function isn't
#  powerful enough to analyze anonymous functions.)
let s = "CompletionFoo.test5(broadcast((x,y)->x==y, push!(Base.split(\"\",' '),\"\",\"\"), \"\"),"
    c, r, res = test_complete(s)
    @test !res
    @test_broken length(c) == 1
    @test_broken c[1] == string(first(methods(Main.CompletionFoo.test5, Tuple{BitArray{1}})))
end

# test partial expression expansion
let s = "CompletionFoo.test5(Bool[x==1 for x=1:4],"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test c[1] == string(first(methods(Main.CompletionFoo.test5, Tuple{Array{Bool,1}})))
end

let s = "CompletionFoo.test4(CompletionFoo.test_y_array[1]()[1], CompletionFoo.test_y_array[1]()[2], "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test c[1] == string(first(methods(Main.CompletionFoo.test4, Tuple{String, String})))
end

# Test that string escaption is handled correct
let s = """CompletionFoo.test4("\\"","""
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
end

########## Test where the current inference logic fails ########
# Fails due to inferrence fails to determine a concrete type for arg 1
# But it returns AbstractArray{T,N} and hence is able to remove test5(x::Float64) from the suggestions
let s = "CompletionFoo.test5(AbstractArray[[]][1],"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
end

# equivalent to above but due to the time macro the completion fails to find the concrete type
let s = "CompletionFoo.test3(@time([1, 2] + CompletionFoo.varfloat),"
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 2
end
#################################################################

let s = "CompletionFoo.kwtest( "
    c, r, res = test_complete(s)
    @test !res
    @test length(c) == 1
    @test contains(c[1], "x, y, w...")
end

# Test of inference based getfield completion
let s = "(1+2im)."
    c,r = test_complete(s)
    @test length(c) == 2
    @test r == (endof(s) + 1):endof(s)
    @test c == ["im", "re"]
end

let s = "((1+2im))."
    c, r = test_complete(s)
    @test length(c) == 2
    @test r == (endof(s) + 1):endof(s)
    @test c == ["im", "re"]
end

let s = "CompletionFoo.test_y_array[1]."
    c, r = test_complete(s)
    @test length(c) == 1
    @test r == (endof(s) + 1):endof(s)
    @test c[1] == "yy"
end

let s = "CompletionFoo.Test_y(rand()).y"
    c, r = test_complete(s)
    @test length(c) == 1
    @test r == endof(s):endof(s)
    @test c[1] == "yy"
end

let s = "CompletionFoo.test6()[1](CompletionFoo.Test_y(rand())).y"
    c, r = test_complete(s)
    @test length(c) == 1
    @test r == endof(s):endof(s)
    @test c[1] == "yy"
end

# Test completion in multi-line comments
let s = "#=\n\\alpha"
    c, r, res = test_complete(s)
    @test c[1] == "α"
    @test r == 4:9
    @test length(c) == 1
end

# Test that completion do not work in multi-line comments
let s = "#=\nmax"
    c, r, res = test_complete(s)
    @test length(c) == 0
end

# Test completion of packages
mkp(p) = ((@assert !isdir(p)); mkpath(p))
temp_pkg_dir_noinit() do
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
    c, r, res = test_complete("using Test_p")
    @test !("Test_pack" in c)
    @test "Test_pack2" in c

    # Test that it also completes on .jl files in pwd()
    cd(Pack_folder) do
        open("Text.txt","w") do f end
        open("Pack.jl","w") do f end
        c, r, res = test_complete("using ")
        @test "Pack" in c
        @test !("Text.txt" in c)
    end
finally
    @test pop!(LOAD_PATH) == path
    rm(path, recursive=true)
end

# Test $ in shell-mode
let s = "cd \$(max"
    c, r, res = test_scomplete(s)
    @test "max" in c
    @test r == 6:8
    @test s[r] == "max"
end

# The return type is of importance, before #8995 it would return nothing
# which would raise an error in the repl code.
@test (String[], 0:-1, false) == test_scomplete("\$a")

if Sys.isunix()
let s, c, r
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

    # Pressing tab after having entered "/tmp " should not
    # attempt to complete "/tmp" but rather work on the current
    # working directory again.
    let s, c, r, file
        file = joinpath(path, "repl completions")
        s = "/tmp "
        c,r = test_scomplete(s)
        @test r == 6:5
    end

    # Test completing paths with an escaped trailing space
    let s, c, r, file
        file = joinpath(tempdir(), "repl completions")
        touch(file)
        s = string(tempdir(), "/repl\\ ")
        c,r = test_scomplete(s)
        @test ["repl\\ completions"] == c
        @test s[r] == "repl\\ "
        rm(file)
    end

    # Tests homedir expansion
    let path, s, c, r
        path = homedir()
        dir = joinpath(path, "tmpfoobar")
        mkdir(dir)
        s = "\"" * path * "/tmpfoob"
        c,r = test_complete(s)
        @test "tmpfoobar/" in c
        l = 3 + length(path)
        @test r == l:l+6
        @test s[r] == "tmpfoob"
        s = "\"~"
        @test "tmpfoobar/" in c
        c,r = test_complete(s)
        rm(dir)
    end

    # Tests detecting of files in the env path (in shell mode)
    let path, s, c, r, file
        oldpath = ENV["PATH"]
        path = tempdir()
        # PATH can also contain folders which we aren't actually allowed to read.
        unreadable = joinpath(tempdir(), "replcompletion-unreadable")
        ENV["PATH"] = string(path, ":", unreadable)

        file = joinpath(path, "tmp-executable")
        touch(file)
        chmod(file, 0o755)
        mkdir(unreadable)
        chmod(unreadable, 0o000)

        s = "tmp-execu"
        c,r = test_scomplete(s)
        @test "tmp-executable" in c
        @test r == 1:9
        @test s[r] == "tmp-execu"

        rm(file)
        rm(unreadable)
        ENV["PATH"] = oldpath
    end

    # Make sure completion results are unique in case things are in the env path twice.
    let
        file0 = joinpath(tempdir(), "repl-completion")
        dir = joinpath(tempdir(), "repl-completion-subdir")
        file1 = joinpath(dir, "repl-completion")

        try
            # Create /tmp/repl-completion and /tmp/repl-completion-subdir/repl-completion
            mkdir(dir)
            touch(file0)
            touch(file1)

            withenv("PATH" => string(tempdir(), ":", dir)) do
                s = string("repl-completio")
                c,r = test_scomplete(s)
                @test ["repl-completion"] == c
                @test s[r] == "repl-completio"
            end

        finally
            rm(file0)
            rm(file1)
            rm(dir)
        end
    end
end
end

#test that it can auto complete with spaces in file/path
let path = tempdir(),
    space_folder = randstring() * " α",
    dir = joinpath(path, space_folder),
    dir_space = replace(space_folder, " ", "\\ ")

    mkdir(dir)
    cd(path) do
        open(joinpath(space_folder, "space .file"),"w") do f
            s = Sys.iswindows() ? "rm $dir_space\\\\space" : "cd $dir_space/space"
            c,r = test_scomplete(s)
            @test r == endof(s)-4:endof(s)
            @test "space\\ .file" in c

            s = Sys.iswindows() ? "cd(\"β $dir_space\\\\space" : "cd(\"β $dir_space/space"
            c,r = test_complete(s)
            @test r == endof(s)-4:endof(s)
            @test "space\\ .file\"" in c
        end
        # Test for issue #10324
        s = "cd(\"$dir_space"
        c,r = test_complete(s)
        @test r == 5:15
        @test s[r] ==  dir_space

        #Test for #18479
        for c in "'`@\$;&"
            test_dir = "test$(c)test"
            mkdir(joinpath(path, test_dir))
            try
                if !(c in ['\'','$']) # As these characters hold special meaning
                    # in shell commands the shell path completion cannot complete
                    # paths with these characters
                    c,r,res = test_scomplete(test_dir)
                    @test c[1] == test_dir*(Sys.iswindows() ? "\\\\" : "/")
                    @test res
                end
                c,r,res  = test_complete("\""*test_dir)
                @test c[1] == test_dir*(Sys.iswindows() ? "\\\\" : "/")
                @test res
            finally
                rm(joinpath(path, test_dir), recursive=true)
            end
        end
    end
    rm(dir, recursive=true)
end

# Test tilde path completion
let (c, r, res) = test_complete("\"~/julia")
    if !Sys.iswindows()
        @test res && c == String[homedir() * "/julia"]
    else
        @test !res
    end

    c, r, res = test_complete("\"foo~bar")
    @test !res
end

# Test the completion returns nothing when the folder do not exist
let (c, r) = test_complete("cd(\"folder_do_not_exist_77/file")
    @test length(c) == 0
end

if Sys.iswindows()
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
        @test (length(c) > 1 && file in c) || (["$file\""] == c)
    end
    rm(tmp)
end

# auto completions of true and false... issue #14101
let s = "tru"
    c, r, res = test_complete(s)
    @test "true" in c
end

let s = "fals"
    c, r, res = test_complete(s)
    @test "false" in c
end

# Don't crash when attempting to complete a tuple, #15329
let s = "CompletionFoo.tuple."
    c, r, res = test_complete(s)
    @test isempty(c)
end

# test Dicts
function test_dict_completion(dict_name)
    s = "$dict_name[\"ab"
    c, r = test_complete(s)
    @test c == Any["\"abc\"", "\"abcd\""]
    s = "$dict_name[\"abcd"
    c, r = test_complete(s)
    @test c == Any["\"abcd\"]"]
    s = "$dict_name[ \"abcd"  # leading whitespace
    c, r = test_complete(s)
    @test c == Any["\"abcd\"]"]
    s = "$dict_name[\"abcd]"  # trailing close bracket
    c, r = completions(s, endof(s) - 1)
    @test c == Any["\"abcd\""]
    s = "$dict_name[:b"
    c, r = test_complete(s)
    @test c == Any[":bar", ":bar2"]
    s = "$dict_name[:bar2"
    c, r = test_complete(s)
    @test c == Any[":bar2]"]
    s = "$dict_name[Ba"
    c, r = test_complete(s)
    @test c == Any["Base]"]
    s = "$dict_name[co"
    c, r = test_complete(s)
    @test c == Any["contains]"]
    s = "$dict_name[`l"
    c, r = test_complete(s)
    @test c == Any["`ls`]"]
    s = "$dict_name[6"
    c, r = test_complete(s)
    @test c == Any["66", "67"]
    s = "$dict_name[66"
    c, r = test_complete(s)
    @test c == Any["66]"]
    s = "$dict_name[("
    c, r = test_complete(s)
    @test c == Any["(\"q\", 3)]"]
    s = "$dict_name[\"\\alp"
    c, r = test_complete(s)
    @test c == String["\\alpha"]
    s = "$dict_name[\"\\alpha"
    c, r = test_complete(s)
    @test c == String["α"]
    s = "$dict_name[\"α"
    c, r = test_complete(s)
    @test c == Any["\"α\"]"]
    s = "$dict_name[:\\alp"
    c, r = test_complete(s)
    @test c == String["\\alpha"]
    s = "$dict_name[:\\alpha"
    c, r = test_complete(s)
    @test c == String["α"]
    s = "$dict_name[:α"
    c, r = test_complete(s)
    @test c == Any[":α]"]
end
test_dict_completion("CompletionFoo.test_dict")
test_dict_completion("CompletionFoo.test_customdict")
test_dict_completion("test_repl_comp_dict")
test_dict_completion("test_repl_comp_customdict")

# Issue #23004: this should not throw:
@test REPLCompletions.dict_identifier_key("test_dict_ℂ[\\", :other) isa Tuple

@testset "completion of string/cmd macros (#22577)" begin
    c, r, res = test_complete("ra")
    @test "raw\"" in c
    c, r, res = test_complete("CompletionFoo.tests")
    @test "teststr\"" in c
    c, r, res = test_complete("CompletionFoo.tϵsτs")
    @test "tϵsτstρ\"" in c
    c, r, res = test_complete("CompletionFoo.testc")
    @test "testcmd`" in c
    c, r, res = test_complete("CompletionFoo.tϵsτc")
    @test "tϵsτcmδ`" in c
end
