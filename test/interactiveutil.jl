
file, line = functionloc(workspace)
func, str = editcmd(file, line)
@test func == run || func == spawn
@test contains(string(str), file)


@unix_only begin
    try
        clipboard("Clipboard Test")
        @test clipboard() == "Clipboard Test"
    catch exc
        @test isa(exc, ErrorException)
        @test contains(exc.msg, "no clipboard command found, please install xsel or xclip")
    end
end

# Not sure how detailed the versioninfo check needs to be
versioninfo(true)
versioninfo(false)

abstract Foo
@test length(methodswith(Expr)) > 0
@test length(methodswith(Expr, Base)) > 0
@test length(methodswith(Expr, Core)) == 0
@test length(methodswith(Foo, Base)) == 0

