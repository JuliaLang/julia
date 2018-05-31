## Secure strings ##

@testset "SecureString" begin
    @testset "wipe original" begin
        str = String("foobar")
        secure = SecureString(str)
        @test str == secure

        # Securely wiping the SecureString will also wipe out the original source
        shred!(secure) === secure
        @test secure != "foobar"
        @test str != "foobar"
    end

    @testset "finalizer" begin
        # Note: Using `@test_warn` ends up messing up the test
        finalizer_test() = begin
            str = "foobar"
            expected = deepcopy(str)
            secure_a = SecureString(str)
            secure_b = secure_a

            secure_a = nothing
            gc()

            @test str == expected  # Still retaining a reference to the original SecureString

            secure_b = nothing
            gc()

            @test str != expected
        end

        @test_warn "SecureString \"foobar\" not explicitly shredded" finalizer_test()
    end

    @testset "deepcopy" begin
        secure_a = SecureString("foo")
        secure_b = deepcopy(secure_a)
        shred!(secure_a)

        @test secure_a != "foo"
        @test secure_b == "foo"
    end
end
