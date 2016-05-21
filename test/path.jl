# This file is a part of Julia. License is MIT: http://julialang.org/license

@test isabspath("~") == false
@test isabspath("/") == true # on windows, this is relatively absolute
@test isabspath("A:/") == is_windows()
@test isabspath("B:\\") == is_windows()
@test isabspath("./") == false
@test isabspath("C:") == false
@test isabspath("C:.") == false
@test isabspath("α:/") == false
@test isabspath(".:/") == false
#@test isabspath("_:/") == false # FIXME?
#@test isabspath("AB:/") == false # FIXME?
@test isabspath("\\\\") == is_windows()
if is_unix()
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
