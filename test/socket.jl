@test ip"127.0.0.1" == IPv4(127,0,0,1)
@test ip"192.0" == IPv4(192,0,0,0)
@test ip"192.0xFFF" == IPv4(192,0,15,255)
@test ip"192.0xFFFF" == IPv4(192,0,255,255)
@test ip"192.0xFFFFF" == IPv4(192,15,255,255)
@test ip"192.0xFFFFFF" == IPv4(192,255,255,255)
@test_throws Base.parseipv4("192.0xFFFFFFF")
@test ip"022.0.0.1" == IPv4(18,0,0,1)

@test_throws Base.parseipv4("192.0xFFFFFFFFF")
@test_throws Base.parseipv4("192.")

@test ip"::1" == IPv6(1)
@test ip"2605:2700:0:3::4713:93e3" == IPv6(parseint(Uint128,"260527000000000300000000471393e3",16))

@test ip"2001:db8:0:0:0:0:2:1" == ip"2001:db8::2:1" == ip"2001:db8::0:2:1"

@test ip"0:0:0:0:0:ffff:127.0.0.1" == IPv6(0xffff7f000001)

# RFC 5952 Compliance

@test repr(ip"2001:db8:0:0:0:0:2:1") == "ip\"2001:db8::2:1\""
@test repr(ip"2001:0db8::0001") == "ip\"2001:db8::1\""
@test repr(ip"2001:db8::1:1:1:1:1") == "ip\"2001:db8:0:1:1:1:1:1\""
@test repr(ip"2001:db8:0:0:1:0:0:1") == "ip\"2001:db8::1:0:0:1\""
@test repr(ip"2001:0:0:1:0:0:0:1") == "ip\"2001:0:0:1::1\""

c = Base.Condition()
@async begin
	s = listen(2134)
	Base.notify(c)
	sock = accept(s)
	write(sock,"Hello World\n")
	close(s)
	close(sock)
end
wait(c)
@test readall(connect(2134)) == "Hello World\n"

socketname = @windows ? "\\\\.\\pipe\\uv-test" : "testsocket"
@unix_only isfile(socketname) && Base.FS.unlink(socketname)
@async begin
	s = listen(socketname)
	Base.notify(c)
	sock = accept(s)
	write(sock,"Hello World\n")
	close(s)
	close(sock)
end
wait(c)
@test readall(connect(socketname)) == "Hello World\n"

try 
    getaddrinfo(".invalid")
catch e
    @test typeof(e) == Base.UVError # E.g. not method error
end

try
    # This should be an invalid port
    connect("localhost",21452)
    @test false
catch e
    @test typeof(e) == Base.UVError
end

server = listen(2134)
@async @test_throws accept(server);
sleep(0.1)
close(server)

server = listen(2134)
@async connect("localhost",2134)
s1 = accept(server)
@test_throws accept(server,s1)
close(server)

try 
    connect(".invalid",80)
catch e
    @test typeof(e) == Base.UVError
end
