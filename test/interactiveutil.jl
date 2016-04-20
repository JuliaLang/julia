### Testing download(url,[localfile],[silent=false])

"Runs a function, and returns stdout, stderr, and the result."
function check_output(func::Function, args...; kwargs...)
    originalSTDOUT = STDOUT
    originalSTDERR = STDERR
    (errRead, errWrite) = redirect_stderr()
    (outRead, outWrite) = redirect_stdout()

    try
        result = func(args...; kwargs...)
        # Ensure both streams are written to or readavailable waits forever.
        write(STDOUT, "\n")
        write(STDERR, "\n")

        errstr = UTF8String(readavailable(errRead))
        outstr = UTF8String(readavailable(outRead))
        # Remove the manually added \n from stdout and stderr
        return (chomp(outstr), chomp(errstr), result)
    catch exception
        # Immediately restore output so errors are printed.
        redirect_stderr(originalSTDERR)
        redirect_stdout(originalSTDOUT)
        println("Error checking output of $func: args=$args, kwargs=$kwargs - $exception")
        rethrow(exception)
    finally
        redirect_stderr(originalSTDERR)
        redirect_stdout(originalSTDOUT)
        close(outRead)
        close(errRead)
        close(errWrite)
        close(outWrite)
    end
end

port = 2000 #Changes to next unused port after this

function start_server(html::AbstractString)
    global server_running, port
    (port, server) = listenany(port)
    html_len = length(html)
    response = """
    HTTP/1.1 200 OK
    Content-Length: $html_len
    Content-Type: text/html
    Connection: Closed

    $html"""

    server_running = true
    @async begin
        while server_running
            sock = accept(server)
            readline(sock)
            write(sock, response)
            close(sock)
        end
    end
end

function stop_server()
    global server_running
    server_running = false
end

function run_test()
    global port
    html = "<html>success</html>"
    port = 2000
    start_server(html)

    address = @windows ? "http://localhost:$port" : "localhost:$port"

    downloader = :unknown
    @unix_only for checkcmd in (:curl, :wget, :fetch)
        if success(`which $checkcmd`)
            downloader = checkcmd
            break
        end
    end

    # Test verbose download, no specified file
    outstr, errstr, result = check_output(download, address)
    downloaded_html = readstring(result)
    @test downloaded_html == html
    if downloader in (:curl, :wget)
        # Progress appears on stderr for curl and wget
        @test length(errstr) > 50
        @test length(outstr) == 0
    else
        @test length(errstr) == 0
        @test length(outstr) == 0
    end

    # Test silent download, no specified file
    outstr, errstr, result = check_output(download, address; silent=true)
    downloaded_html = readstring(result)
    @test downloaded_html == html
    @test length(errstr) == 0
    @test length(outstr) == 0

    output_file = tempname()

    # Test verbose, specified file
    outstr, errstr, result = check_output(download, address, output_file)
    downloaded_html = readstring(output_file)
    @test downloaded_html == html
    if downloader in (:curl, :wget)
        # Progress appears on stderr for curl and wget
        @test length(errstr) > 50
        @test length(outstr) == 0
    else
        @test length(errstr) == 0
        @test length(outstr) == 0
    end

    # Test silent download, specified file
    outstr, errstr, result = check_output(download, address, output_file; silent=true)
    downloaded_html = readstring(output_file)
    @test downloaded_html == html
    @test length(errstr) == 0
    @test length(outstr) == 0

    stop_server()
end

# Test cannot currently run on windows, since it requires @async support
@unix_only run_test()
