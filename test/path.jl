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
    if Sys.iswindows()
        @testset "issue #38491" begin
            pwd_drive = uppercase(splitdrive(pwd())[1])
            drive = (pwd_drive == "X:") ? "Y:" : "X:"
            @test abspath("$(lowercase(drive))a\\b\\c") == "$(lowercase(drive))\\a\\b\\c"
            @test abspath("$(uppercase(drive))a\\b\\c") == "$(uppercase(drive))\\a\\b\\c"
            @test abspath("$(lowercase(drive))a") == "$(lowercase(drive))\\a"
            @test abspath("$(uppercase(drive))a") == "$(uppercase(drive))\\a"
            @test abspath(lowercase(drive)) == "$(lowercase(drive))\\"
            @test abspath(uppercase(drive)) == "$(uppercase(drive))\\"

            @test lowercase(abspath("$(pwd_drive)a\\b\\c")) == lowercase(joinpath(pwd(), "a\\b\\c"))
            @test lowercase(abspath("$(pwd_drive)a")) == lowercase(joinpath(pwd(), "a"))
            @test lowercase(abspath(lowercase(pwd_drive))) == lowercase("$(pwd())\\")
            @test lowercase(abspath(uppercase(pwd_drive))) == lowercase("$(pwd())\\")
        end
    end
    @test basename(S("foo$(sep)bar")) == "bar"
    @test dirname(S("foo$(sep)bar")) == "foo"

    @testset "expanduser" begin
        @test expanduser(S("")) == ""
        @test expanduser(S("x")) == "x"
        @test expanduser(S("~")) == (Sys.iswindows() ? "~" : homedir())
    end
    @testset "contractuser" begin
        @test contractuser(S(homedir())) == (Sys.iswindows() ? homedir() : "~")
        @test contractuser(S(joinpath(homedir(), "x"))) ==
              (Sys.iswindows() ? joinpath(homedir(), "x") : "~$(sep)x")
        @test contractuser(S("/foo/bar")) == "/foo/bar"
    end
    @testset "isdirpath" begin
        @test !isdirpath(S("foo"))
        @test isdirpath(S("foo$sep"))
        @test isdirpath(S(""))
        @test isdirpath(S("."))
        @test isdirpath(S(".."))
    end
    @testset "joinpath" begin
        @test joinpath(S("")) == ""
        @test joinpath(S("foo")) == "foo"
        @test joinpath(S("foo"), S("bar")) == "foo$(sep)bar"
        @test joinpath(S("foo"), S("bar"), S("baz")) == "foo$(sep)bar$(sep)baz"
        @test joinpath(S("foo"), S(""), S("baz")) == "foo$(sep)baz"
        @test joinpath(S("foo"), S(""), S("")) == "foo$(sep)"
        @test joinpath(S("foo"), S(""), S(""), S("bar")) == "foo$(sep)bar"

        @test joinpath(S("foo"), S(homedir())) == homedir()
        @test joinpath(S(abspath("foo")), S(homedir())) == homedir()

        for str in map(S, [sep, "a$(sep)b", "a$(sep)b$(sep)c", "a$(sep)b$(sep)c$(sep)d"])
            @test str == joinpath(splitpath(str))
            @test joinpath(splitpath(str)) == joinpath(splitpath(str)...)
        end

        if Sys.iswindows()
            @test joinpath(S("foo"),S("bar:baz")) == "bar:baz"
            @test joinpath(S("C:"),S("foo"),S("D:"),S("bar")) == "D:bar"
            @test joinpath(S("C:"),S("foo"),S("D:bar"),S("baz")) == "D:bar$(sep)baz"

            # relative folders and case-insensitive drive letters
            @test joinpath(S("C:\\a\\b"), S("c:c\\e")) == "C:\\a\\b\\c\\e"

            # UNC paths
            @test joinpath(S("\\\\server"), S("share")) == "\\\\server\\share"
            @test joinpath(S("\\\\server"), S("share"), S("a")) == "\\\\server\\share\\a"
            @test joinpath(S("\\\\server\\"), S("share"), S("a")) == "\\\\server\\share\\a"
            @test joinpath(S("\\\\server"), S("share"), S("a"), S("b")) == "\\\\server\\share\\a\\b"
            @test joinpath(S("\\\\server\\share"),S("a")) == "\\\\server\\share\\a"
            @test joinpath(S("\\\\server\\share\\"), S("a")) == "\\\\server\\share\\a"

            for str in map(S, ["c:\\", "c:\\a", "c:\\a\\b", "c:\\a\\b\\c", "c:\\a\\b\\c\\d"])
                @test str == joinpath(splitpath(str))
                @test joinpath(splitpath(str)) == joinpath(splitpath(str)...)
            end

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
        @test splitpath(S(joinpath("a","b","c"))) == ["a", "b", "c"]
        @test splitpath("") == [""]

        @test splitpath(S(joinpath("cats are", "gr8t"))) == ["cats are", "gr8t"]
        @test splitpath(S(joinpath("  ", " "))) == ["  ", " "]

        # Unix-style paths are understood by all systems.
        @test splitpath(S("/a/b")) == ["/", "a", "b"]
        @test splitpath(S("/")) == ["/"]
        @test splitpath(S("a/")) == ["a"]
        @test splitpath(S("a/b/")) == ["a", "b"]
        @test splitpath(S("a.dir/b.txt")) == ["a.dir", "b.txt"]
        @test splitpath(S("///")) == ["/"]
        @test splitpath(S("///a///b///")) == ["/", "a", "b"]

        if Sys.iswindows()
            @test splitpath(S("C:\\\\a\\b\\c")) == ["C:\\", "a", "b", "c"]
            @test splitpath(S("C:\\\\")) == ["C:\\"]
            @test splitpath(S("J:\\")) == ["J:\\"]
            @test splitpath(S("C:")) == ["C:"]
            @test splitpath(S("C:a")) == ["C:a"]
            @test splitpath(S("C:a\\b")) == ["C:a", "b"]

            @test splitpath(S("a\\")) == ["a"]
            @test splitpath(S("a\\\\b\\\\")) == ["a","b"]
            @test splitpath(S("a.dir\\b.txt")) == ["a.dir", "b.txt"]
            @test splitpath(S("\\a\\b\\")) == ["\\", "a","b"]
            @test splitpath(S("\\\\a\\b")) == ["\\\\a\\b"]  # This is actually a valid drive name in windows.

            @test splitpath(S("/a/b\\c/d\\\\e")) == ["/", "a", "b", "c", "d", "e"]
            @test splitpath(S("/\\/\\")) == ["/"]
            @test splitpath(S("\\/\\a/\\//b")) == ["\\","a","b"]
        end
    end

    @testset "splitdir, splitdrive" begin
        @test joinpath(splitdir(S(homedir()))...) == homedir()
        @test string(splitdrive(S(homedir()))...) == homedir()
        @test splitdrive("a\nb") == ("", "a\nb")

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

    @testset "isabspath" begin
        @test isabspath(S("~")) == false
        @test isabspath(S("/")) == true # on windows, this is relatively absolute
        @test isabspath(S("A:/")) == Sys.iswindows()
        @test isabspath(S("B:\\")) == Sys.iswindows()
        @test isabspath(S("./")) == false
        @test isabspath(S("C:")) == false
        @test isabspath(S("C:.")) == false
        @test isabspath(S("α:/")) == false
        @test isabspath(S(".:/")) == false
        #@test isabspath(S("_:/")) == false # FIXME?
        #@test isabspath(S("AB:/")) == false # FIXME?
        @test isabspath(S("\\\\")) == Sys.iswindows()
        if Sys.isunix()
            @test isabspath(S(expanduser("~"))) == true
            @test startswith(expanduser(S("~")), homedir())
        else
            @test expanduser(S("~")) == "~"
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
                    if Sys.iswindows()
                        @test relpath("e:$filep", "e:$startp") == relpath_expected_results[idx]
                        @test relpath("e:$filep", "E:$startp") == relpath_expected_results[idx]
                        @test relpath("E:$filep", "e:$startp") == relpath_expected_results[idx]
                        @test relpath("E:$filep", "E:$startp") == relpath_expected_results[idx]
                    end
                end
            end
            # Additional cases
            @test_throws ArgumentError relpath(S("$(sep)home$(sep)user$(sep)dir_withendsep$(sep)"), "")
            @test_throws ArgumentError relpath(S(""), S("$(sep)home$(sep)user$(sep)dir_withendsep$(sep)"))

            # issue 40237
            path = "..$(sep)a$(sep)b$(sep)c"
            @test relpath(abspath(path)) == path
        end
        test_relpath()
    end

    if Sys.iswindows()
        @testset "issue #23646" begin
            @test lowercase(relpath("E:\\a\\b", "C:\\c")) == "e:\\a\\b"
            @test lowercase(relpath("E:\\a\\b", "c:\\c")) == "e:\\a\\b"
            @test lowercase(relpath("e:\\a\\b", "C:\\c")) == "e:\\a\\b"
            @test lowercase(relpath("e:\\a\\b", "c:\\c")) == "e:\\a\\b"

            @test relpath("C:\\a\\b", "c:\\a\\b") == "."
            @test relpath("c:\\a\\b", "C:\\a\\b") == "."
            @test lowercase(relpath("C:\\a\\b", "c:\\c\\d")) == "..\\..\\a\\b"
            @test lowercase(relpath("c:\\a\\b", "C:\\c\\d")) == "..\\..\\a\\b"
        end
    end

    @testset "type stability" begin
        @test isa(joinpath(S("a"), S("b")), String)
        @test isa(joinpath(S(abspath("a")), S("b")), String)
    end
end

@testset "homedir" begin
    var = Sys.iswindows() ? "USERPROFILE" : "HOME"
    AVG_PATH = Base.Filesystem.AVG_PATH - 1 # null-termination character
    for i = 0:9
        local home = " "^AVG_PATH * "123456789"[1:i]
        @test withenv(var => home) do
            homedir()
        end == home
    end
    @test isabspath(withenv(homedir, var => nothing))
end
