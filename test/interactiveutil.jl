
file, line = functionloc(workspace)
func, str = Base.editcmd(file, line)
@test func == run || func == spawn
@test contains(string(str), file)


@unix_only begin
    old = ""
    try
        old = clipboard()
        clipboard("Clipboard Test")
        @test clipboard() == "Clipboard Test"
    catch exc
        @test isa(exc, ErrorException)
        @test contains(exc.msg, "no clipboard command found, please install xsel or xclip")
    finally
        if length(old) > 0
            clipboard(old)
        end
    end
end

# Not sure how detailed the versioninfo check needs to be
vinfo = IOBuffer()
versioninfo(vinfo, true)
str = takebuf_string(vinfo)
@test contains(str, "Julia Version")
@test contains(str, "Commit")
@test contains(str, "Load Avg")
@test contains(str, "Package Directory")
@test contains(str, "PATH")

versioninfo(vinfo, false)
str = takebuf_string(vinfo)
@test contains(str, "Julia Version")
@test contains(str, "Commit")
@test !contains(str, "Load Avg")
@test !contains(str, "Package Directory")
@test !contains(str, "PATH")

abstract Foo
@test length(methodswith(Expr)) > 0
@test length(methodswith(Expr, Base)) > 0
@test length(methodswith(Expr, Core)) == 0
@test length(methodswith(Foo, Base)) == 0

# Issue #11948
# originally in test/workspace.jl
script = """
f(x) = x+1
workspace()
@assert !isdefined(:f)
LastMain.f(2)
"""
exename = joinpath(JULIA_HOME, Base.julia_exename())
# If we're running this with code coverage run the script with
# coverage on, but inlining and precompilation off.
if Base.JLOptions().code_coverage > 0
    run(`$exename --precompiled=no --inline=no --code-coverage=all -f -e $script`)
else
    run(`$exename -f -e $script`)
end
