# This file is a part of Julia. License is MIT: https://julialang.org/license

for S in (String, GenericString)
    dir = pwd()
    sep = Base.Filesystem.path_separator

    @test abspath(S("foo")) == "$dir$(sep)foo"
    @test abspath(S("foo"), S("bar")) == "$dir$(sep)foo$(sep)bar"

    @test basename(S("foo$(sep)bar")) == "bar"
    @test dirname(S("foo$(sep)bar")) == "foo"

    @test expanduser(S("x")) == "x"
    @test expanduser(S("~")) == (Sys.iswindows() ? "~" : homedir())

    @test isabspath(S(homedir()))
    @test !isabspath(S("foo"))

    @test !isdirpath(S("foo"))
    @test isdirpath(S("foo$sep"))
    @test isdirpath(S(""))
    @test isdirpath(S("."))
    @test isdirpath(S(".."))

    @test joinpath(S("foo")) == "foo"
    @test joinpath(S("foo"), S("bar")) == "foo$(sep)bar"
    @test joinpath(S("foo"), S(homedir())) == homedir()
    @test joinpath(S(abspath("foo")), S(homedir())) == homedir()

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

    @test relpath(S(joinpath("foo","bar")), S("foo")) == "bar"

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

############################################
# This section tests relpath computation. #
###########################################
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

# Test type stability
@test isa(joinpath("a", "b"), String)
@test isa(joinpath(abspath("a"), "b"), String)

# homedir
let var = Sys.iswindows() ? "USERPROFILE" : "HOME",
    MAX_PATH = Sys.iswindows() ? 240 : 1020
    for i = 0:9
        local home = " "^MAX_PATH * "123456789"[1:i]
        @test withenv(var => home) do
            homedir()
        end == home
    end
    @test isabspath(withenv(homedir, var => nothing))
end
