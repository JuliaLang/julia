# This file is a part of Julia. License is MIT: https://julialang.org/license

# Test that `Base.download_url()` is altered by `Base.DOWNLOAD_HOOKS`.
let urls = ["http://httpbin.julialang.org/ip", "https://httpbin.julialang.org/ip"]
    for url in urls
        @test Base.download_url(url) == url
    end
    push!(Base.DOWNLOAD_HOOKS, url->replace(url, r"^http://" => "https://"))
    for url in urls
        @test Base.download_url(url) == urls[end]
    end
    pop!(Base.DOWNLOAD_HOOKS)
    for url in urls
        @test Base.download_url(url) == url
    end
end

mktempdir() do temp_dir
    # Download a file
    file = joinpath(temp_dir, "ip")
    @test download("https://httpbin.julialang.org/ip", file) == file
    @test isfile(file)
    @test !isempty(read(file))
    ip = read(file, String)

    # Test download rewrite hook
    push!(Base.DOWNLOAD_HOOKS, url->replace(url, r"/status/404$" => "/ip"))
    @test download("https://httpbin.julialang.org/status/404", file) == file
    @test isfile(file)
    @test !isempty(read(file))
    @test ip == read(file, String)
    pop!(Base.DOWNLOAD_HOOKS)

    # Download an empty file
    empty_file = joinpath(temp_dir, "empty")
    @test download("https://httpbin.julialang.org/status/200", empty_file) == empty_file

    # Windows and older versions of curl do not create the empty file (https://github.com/curl/curl/issues/183)
    @test !isfile(empty_file) || isempty(read(empty_file))

    # Make sure that failed downloads do not leave files around
    missing_file = joinpath(temp_dir, "missing")
    @test_throws ProcessFailedException download("https://httpbin.julialang.org/status/404", missing_file)
    @test !isfile(missing_file)

    # Make sure we properly handle metachar ' on windows with ^ escaping
    if Sys.iswindows()
        metachar_file = joinpath(temp_dir, "metachar")
        Base.download_powershell("https://httpbin.julialang.org/get?test='^'", metachar_file)
        metachar_string = read(metachar_file, String)
        m = match(r"\"test\"\s*:\s*\"(.*)\"", metachar_string)
        @test m.captures[1] == "'^'"
    end

    # Use a TEST-NET (192.0.2.0/24) address which shouldn't be bound
    invalid_host_file = joinpath(temp_dir, "invalid_host")
    @test_throws ProcessFailedException download("http://192.0.2.1", invalid_host_file)
    @test !isfile(invalid_host_file)
end
