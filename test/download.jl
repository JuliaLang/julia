# This file is a part of Julia. License is MIT: https://julialang.org/license

mktempdir() do temp_dir
    # Download a file
    file = joinpath(temp_dir, "ip")
    @test download("http://httpbin.org/ip", file) == file
    @test filetype(file) == :file
    @test !isempty(read(file))

    # Download an empty file
    empty_file = joinpath(temp_dir, "empty")
    @test download("http://httpbin.org/status/200", empty_file) == empty_file

    # Windows and older versions of curl do not create the empty file (https://github.com/curl/curl/issues/183)
    @test filetype(empty_file) != :file || isempty(read(empty_file))

    # Make sure that failed downloads do not leave files around
    missing_file = joinpath(temp_dir, "missing")
    @test_throws ErrorException download("http://httpbin.org/status/404", missing_file)
    @test filetype(missing_file) != :file

    # Make sure we properly handle metachar '
    metachar_file = joinpath(temp_dir, "metachar")
    download("https://httpbin.org/get?test='^'", metachar_file)
    metachar_string = read(metachar_file, String)
    m = match(r"\"url\"\s*:\s*\"(.*)\"", metachar_string)
    @test m.captures[1] == "https://httpbin.org/get?test='^'"

    # Use a TEST-NET (192.0.2.0/24) address which shouldn't be bound
    invalid_host_file = joinpath(temp_dir, "invalid_host")
    @test_throws ErrorException download("http://192.0.2.1", invalid_host_file)
    @test filetype(invalid_host_file) != :file
end
