@unix_only @test expanduser("~")[1] != ENV["HOME"]

@unix_only @test isabspath("/") == true
@test isabspath("~") == false
@unix_only @test isabspath(expanduser("~")) == true
