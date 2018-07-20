# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "basic path functions for string type $S" for S in (String, GenericString)
    dir = pwd()
    sep = Base.Filesystem.path_separator
    @testset "isabspath and abspath" begin
        @test abspath(S("foo")) == "$dir$(sep)foo"
        @test abspath(S("foo"), S("bar")) == "$dir$(sep)foo$(sep)bar"
        @test isabspath(S(homedir()))
        @test !isabspath(S("foo"))
    end
    @test basename(S("foo$(sep)bar")) == "bar"
    @test dirname(S("foo$(sep)bar")) == "foo"

    @testset "expanduser" begin
        @test expanduser(S("")) == ""
        @test expanduser(S("x")) == "x"
        @test expanduser(S("~")) == (Sys.iswindows() ? "~" : homedir())
    end
    @testset "Base.contractuser" begin
        @test Base.contractuser(S(homedir())) == (Sys.iswindows() ? homedir() : "~")
        @test Base.contractuser(S(joinpath(homedir(), "x"))) ==
              (Sys.iswindows() ? joinpath(homedir(), "x") : "~$(sep)x")
        @test Base.contractuser(S("/foo/bar")) == "/foo/bar"
    end
    @testset "isdirpath" begin
        @test !isdirpath(S("foo"))
        @test isdirpath(S("foo$sep"))
        @test isdirpath(S(""))
        @test isdirpath(S("."))
        @test isdirpath(S(".."))
    end
    @testset "joinpath" begin
        @test joinpath(S("foo")) == "foo"
        @test joinpath(S("foo"), S("bar")) == "foo$(sep)bar"
        @test joinpath(S("foo"), S(homedir())) == homedir()
        @test joinpath(S(abspath("foo")), S(homedir())) == homedir()

        if Sys.iswindows()
            @test joinpath(S("foo"),S("bar:baz")) == "bar:baz"
            @test joinpath(S("C:"),S("foo"),S("D:"),S("bar")) == "D:bar"
            @test joinpath(S("C:"),S("foo"),S("D:bar"),S("baz")) == "D:bar$(sep)baz"
        elseif Sys.isunix()
            @test joinpath(S("foo"),S("bar:baz")) == "foo$(sep)bar:baz"
            @test joinpath(S("C:"),S("foo"),S("D:"),S("bar")) == "C:$(sep)foo$(sep)D:$(sep)bar"
            @test joinpath(S("C:"),S("foo"),S("D:"),S("bar"),S("baz")) == "C:$(sep)foo$(sep)D:$(sep)bar$(sep)baz"
        end
    end
    @testset "normpath" begin
        @test normpath(S(joinpath("."))) == "."
        @test normpath(S(joinpath(".."))) == ".."
        @test normpath(S(joinpath("..","."))) == ".."
        @test normpath(S(joinpath(".",".."))) == ".."
        @test normpath(S(joinpath("..",".."))) == "..$(sep).."
        @test normpath(S(joinpath(".","..",".."))) == "..$(sep).."
        @test normpath(S(joinpath("..",".",".."))) == "..$(sep).."
        @test normpath(S(joinpath("..","..","."))) == "..$(sep).."

        @test normpath(S(joinpath("foo","."))) == "foo$sep"
        @test normpath(S(joinpath("foo",".."))) == "."
        @test normpath(S(joinpath("foo","..","."))) == "."
        @test normpath(S(joinpath("foo",".",".."))) == "."
        @test normpath(S(joinpath("foo","..",".."))) == ".."
        @test normpath(S(joinpath("foo",".","..",".."))) == ".."
        @test normpath(S(joinpath("foo","..",".",".."))) == ".."
        @test normpath(S(joinpath("foo","..","..","."))) == ".."

        @test normpath(S(joinpath(".","bar"))) == "bar"
        @test normpath(S(joinpath("..","bar"))) == "..$(sep)bar"
        @test normpath(S(joinpath("..",".","bar"))) == "..$(sep)bar"
        @test normpath(S(joinpath(".","..","bar"))) == "..$(sep)bar"
        @test normpath(S(joinpath("..","..","bar"))) == "..$(sep)..$(sep)bar"
        @test normpath(S(joinpath(".","..","..","bar"))) == "..$(sep)..$(sep)bar"
        @test normpath(S(joinpath("..",".","..","bar"))) == "..$(sep)..$(sep)bar"
        @test normpath(S(joinpath("..","..",".","bar"))) == "..$(sep)..$(sep)bar"

        @test normpath(S(joinpath("foo",".","bar"))) == "foo$(sep)bar"
        @test normpath(S(joinpath("foo","..","bar"))) == "bar"
        @test normpath(S(joinpath("foo","..",".","bar"))) == "bar"
        @test normpath(S(joinpath("foo",".","..","bar"))) == "bar"
        @test normpath(S(joinpath("foo","..","..","bar"))) == "..$(sep)bar"
        @test normpath(S(joinpath("foo",".","..","..","bar"))) == "..$(sep)bar"
        @test normpath(S(joinpath("foo","..",".","..","bar"))) == "..$(sep)bar"
        @test normpath(S(joinpath("foo","..","..",".","bar"))) == "..$(sep)bar"
    end
    @test relpath(S(joinpath("foo","bar")), S("foo")) == "bar"

    @testset "splitpath" begin
        @test splitpath(joinpath("a","b","c")) == ["a", "b", "c"]
        @test splitpath("") == [""]

        @test splitpath(joinpath("cats are", "gr8t")) == ["cats are", "gr8t"]
        @test splitpath(joinpath("  ", " ")) == ["  ", " "]

        # Unix-style paths are understood by all systems.
        @test splitpath("/a/b") == ["/", "a", "b"]
        @test splitpath("/") == ["/"]
        @test splitpath("a/") == ["a"]
        @test splitpath("a/b/") == ["a", "b"]
        @test splitpath("a.dir/b.txt") == ["a.dir", "b.txt"]
        @test splitpath("///") == ["/"]
        @test splitpath("///a///b///") == ["/", "a", "b"]

        if Sys.iswindows()
            @test splitpath("C:\\\\a\\b\\c") == ["C:\\", "a", "b", "c"]
            @test splitpath("C:\\\\") == ["C:\\"]
            @test splitpath("J:\\") == ["J:\\"]
            @test splitpath("C:") == ["C:"]
            @test splitpath("C:a") == ["C:a"]
            @test splitpath("C:a\\b") == ["C:a", "b"]

            @test splitpath("a\\") == ["a"]
            @test splitpath("a\\\\b\\\\") == ["a","b"]
            @test splitpath("a.dir\\b.txt") == ["a.dir", "b.txt"]
            @test splitpath("\\a\\b\\") == ["\\", "a","b"]
            @test splitpath("\\\\a\\b") == ["\\\\a\\b"]  # This is actually a valid drive name in windows.

            @test splitpath("/a/b\\c/d\\\\e") == ["/", "a", "b", "c", "d", "e"]
            @test splitpath("/\\/\\") == ["/"]
            @test splitpath("\\/\\a/\\//b") == ["\\","a","b"]
        end
    end

    @testset "splitting" begin
        @test joinpath(splitdir(S(homedir()))...) == homedir()
        @test string(splitdrive(S(homedir()))...) == homedir()

        if Sys.iswindows()
            @test splitdrive(S("\\\\servername\\hello.world\\filename.ext")) ==
                ("\\\\servername\\hello.world","\\filename.ext")
            @test splitdrive(S("\\\\servername.com\\hello.world\\filename.ext")) ==
                ("\\\\servername.com\\hello.world","\\filename.ext")
            @test splitdrive(S("C:\\foo\\bar")) ==
                ("C:","\\foo\\bar")
        end

        @test splitext(S("")) == ("", "")
        @test splitext(S(".")) == (".", "")
        @test_broken splitext(S("..")) == ("..", "")
        @test_broken splitext(S("...")) == ("...", "")
        @test splitext(S("foo")) == ("foo", "")
        @test splitext(S("foo.")) == ("foo", ".")
        @test_broken splitext(S("foo..")) == ("foo", "..")
        @test_broken splitext(S("foo...")) == ("foo", "...")
        @test splitext(S("foo.bar")) == ("foo", ".bar")
        @test splitext(S(".foo")) == (".foo", "")
        @test splitext(S(".foo.")) == (".foo", ".")
        @test_broken splitext(S(".foo..")) == (".foo", "..")
        @test_broken splitext(S(".foo...")) == (".foo", "...")
        @test splitext(S(".foo.bar")) == (".foo", ".bar")
    end
end

@testset "isabspath" begin
    @test isabspath("~") == false
    @test isabspath("/") == true # on windows, this is relatively absolute
    @test isabspath("A:/") == Sys.iswindows()
    @test isabspath("B:\\") == Sys.iswindows()
    @test isabspath("./") == false
    @test isabspath("C:") == false
    @test isabspath("C:.") == false
    @test isabspath("α:/") == false
    @test isabspath(".:/") == false
    #@test isabspath("_:/") == false # FIXME?
    #@test isabspath("AB:/") == false # FIXME?
    @test isabspath("\\\\") == Sys.iswindows()
    if Sys.isunix()
        @test isabspath(expanduser("~")) == true
        @test startswith(expanduser("~"), homedir())
    else
        @test expanduser("~") == "~"
    end
end

@testset "relpath" begin
    function test_relpath()
        sep = Base.Filesystem.path_separator
        filepaths = [
            "$(sep)home$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)Test1.md",
            "$(sep)home$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)lib$(sep)file1.md",
            "$(sep)home$(sep)user$(sep).julia$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "$(sep)home$(sep)user$(sep)dir_withendsep$(sep)",
            "$(sep)home$(sep)dir2_withendsep$(sep)",
            "$(sep)home$(sep)test.md",
            "$(sep)home",
            # Special cases
            "$(sep)",
            "$(sep)home$(sep)$(sep)$(sep)"
        ]
        startpaths = [
            "$(sep)home$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)genindex.md",
            "$(sep)multi_docs$(sep)genindex.md",
            "$(sep)home$(sep)user$(sep)dir_withendsep$(sep)",
            "$(sep)home$(sep)dir2_withendsep$(sep)",
            "$(sep)home$(sep)test.md",
            "$(sep)home",
            # Special cases
            "$(sep)",
            "$(sep)home$(sep)$(sep)$(sep)"
        ]
        relpath_expected_results = [
            "..$(sep)Test1.md",
            "..$(sep)..$(sep)home$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)Test1.md",
            "..$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)Test1.md",
            "..$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)Test1.md",
            "..$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)Test1.md",
            "user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)Test1.md",
            "home$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)Test1.md",
            "user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)Test1.md",
            "..$(sep)lib$(sep)file1.md",
            "..$(sep)..$(sep)home$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)lib$(sep)file1.md",
            "..$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)lib$(sep)file1.md",
            "..$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)lib$(sep)file1.md",
            "..$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)lib$(sep)file1.md",
            "user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)lib$(sep)file1.md",
            "home$(sep)user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)lib$(sep)file1.md",
            "user$(sep).julia$(sep)Test1$(sep)docs$(sep)api$(sep)lib$(sep)file1.md",
            "..$(sep)..$(sep)..$(sep)..$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "..$(sep)..$(sep)home$(sep)user$(sep).julia$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "..$(sep).julia$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "..$(sep)user$(sep).julia$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "..$(sep)user$(sep).julia$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "user$(sep).julia$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "home$(sep)user$(sep).julia$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "user$(sep).julia$(sep)测试2$(sep)docs$(sep)api$(sep)测试2.md",
            "..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)dir_withendsep",
            "..$(sep)..$(sep)home$(sep)user$(sep)dir_withendsep",".","..$(sep)user$(sep)dir_withendsep",
            "..$(sep)user$(sep)dir_withendsep","user$(sep)dir_withendsep",
            "home$(sep)user$(sep)dir_withendsep","user$(sep)dir_withendsep",
            "..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)dir2_withendsep",
            "..$(sep)..$(sep)home$(sep)dir2_withendsep","..$(sep)..$(sep)dir2_withendsep",".",
            "..$(sep)dir2_withendsep","dir2_withendsep","home$(sep)dir2_withendsep","dir2_withendsep",
            "..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)test.md","..$(sep)..$(sep)home$(sep)test.md",
            "..$(sep)..$(sep)test.md","..$(sep)test.md",".","test.md","home$(sep)test.md","test.md",
            "..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)..","..$(sep)..$(sep)home","..$(sep)..",
            "..","..",".","home",".","..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)..","..$(sep)..",
            "..$(sep)..$(sep)..","..$(sep)..","..$(sep)..","..",".","..",
            "..$(sep)..$(sep)..$(sep)..$(sep)..$(sep)..","..$(sep)..$(sep)home","..$(sep)..",
            "..","..",".","home","."
        ]
        idx = 0
        for filep in filepaths
            for startp in startpaths
                res = relpath(filep, startp)
                idx += 1
                @test res == relpath_expected_results[idx]
            end
        end
        # Additional cases
        @test_throws ArgumentError relpath("$(sep)home$(sep)user$(sep)dir_withendsep$(sep)", "")
        @test_throws ArgumentError relpath("", "$(sep)home$(sep)user$(sep)dir_withendsep$(sep)")
    end
    test_relpath()
end
@testset "type stability" begin
    @test isa(joinpath("a", "b"), String)
    @test isa(joinpath(abspath("a"), "b"), String)
end
@testset "homedir" begin
    var = Sys.iswindows() ? "USERPROFILE" : "HOME"
    MAX_PATH = Sys.iswindows() ? 240 : 1020
    for i = 0:9
        local home = " "^MAX_PATH * "123456789"[1:i]
        @test withenv(var => home) do
            homedir()
        end == home
    end
    @test isabspath(withenv(homedir, var => nothing))
end
