# This file is a part of Julia. License is MIT: https://julialang.org/license

mktempdir() do temp_dir
    # Download a file
    file = joinpath(temp_dir, "ip")
    @test download("http://httpbin.org/ip", file) == file
    @test isfile(file)
    @test !isempty(read(file))

    # Download a file and check its SHA-256 hash
    file = joinpath(temp_dir, "html")
    @test file == download("http://httpbin.org/html", file,
          sha="3f324f9914742e62cf082861ba03b207282dba781c3349bee9d7c1b5ef8e0bfe")
    @test_throws ErrorException file == download("http://httpbin.org/html", file,
        sha="3f324f9914742e62cf082861ba03b207282dba781c3349bee9d7c1b5ef8e0000")
    @test_throws ArgumentError file == download("http://httpbin.org/html", file,
        sha="3f324f9914742e62cf082861ba03b207282dba781c3349bee9d7c1b5ef8e0xxx")
    @test_throws ArgumentError file == download("http://httpbin.org/html", file,
        sha="3f324f9914742e62cf082861ba03b207282dba781c3349bee9d7c1b5ef8e0")

    # Download an empty file
    empty_file = joinpath(temp_dir, "empty")
    @test download("http://httpbin.org/status/200", empty_file) == empty_file

    # Windows and older versions of curl do not create the empty file (https://github.com/curl/curl/issues/183)
    @test !isfile(empty_file) || isempty(read(empty_file))

    # Make sure that failed downloads do not leave files around
    missing_file = joinpath(temp_dir, "missing")
    @test_throws ErrorException download("http://httpbin.org/status/404", missing_file)
    @test !isfile(missing_file)

    # Make sure we properly handle metachar '
    metachar_file = joinpath(temp_dir, "metachar")
    download("https://httpbin.org/get?test='^'", metachar_file)
    metachar_string = read(metachar_file, String)
    m = match(r"\"url\"\s*:\s*\"(.*)\"", metachar_string)
    @test m.captures[1] == "https://httpbin.org/get?test='^'"

    # Use a TEST-NET (192.0.2.0/24) address which shouldn't be bound
    invalid_host_file = joinpath(temp_dir, "invalid_host")
    @test_throws ErrorException download("http://192.0.2.1", invalid_host_file)
    @test !isfile(invalid_host_file)
end
