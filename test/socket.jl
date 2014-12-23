@test ip"127.0.0.1" == IPv4(127,0,0,1)
@test ip"192.0" == IPv4(192,0,0,0)
@test ip"192.0xFFF" == IPv4(192,0,15,255)
@test ip"192.0xFFFF" == IPv4(192,0,255,255)
@test ip"192.0xFFFFF" == IPv4(192,15,255,255)
@test ip"192.0xFFFFFF" == IPv4(192,255,255,255)
@test_throws ErrorException Base.parseipv4("192.0xFFFFFFF")
@test ip"022.0.0.1" == IPv4(18,0,0,1)

@test_throws ErrorException Base.parseipv4("192.0xFFFFFFFFF")
@test_throws ErrorException Base.parseipv4("192.")

@test ip"::1" == IPv6(1)
@test ip"2605:2700:0:3::4713:93e3" == IPv6(parseint(UInt128,"260527000000000300000000471393e3",16))

@test ip"2001:db8:0:0:0:0:2:1" == ip"2001:db8::2:1" == ip"2001:db8::0:2:1"

@test ip"0:0:0:0:0:ffff:127.0.0.1" == IPv6(0xffff7f000001)

# isless and comparisons
@test ip"1.2.3.4" < ip"1.2.3.7" < ip"2.3.4.5"
@test ip"1.2.3.4" >= ip"1.2.3.4" >= ip"1.2.3.1"
@test isless(ip"1.2.3.4", ip"1.2.3.5")
@test_throws MethodError sort[ip"2.3.4.5", ip"1.2.3.4", ip"2001:1:2::1"]

# RFC 5952 Compliance

@test repr(ip"2001:db8:0:0:0:0:2:1") == "ip\"2001:db8::2:1\""
@test repr(ip"2001:0db8::0001") == "ip\"2001:db8::1\""
@test repr(ip"2001:db8::1:1:1:1:1") == "ip\"2001:db8:0:1:1:1:1:1\""
@test repr(ip"2001:db8:0:0:1:0:0:1") == "ip\"2001:db8::1:0:0:1\""
@test repr(ip"2001:0:0:1:0:0:0:1") == "ip\"2001:0:0:1::1\""

c = Base.Condition()
port = rand(2000:4000)
@async begin
    s = listen(port)
    Base.notify(c)
    sock = accept(s)
    write(sock,"Hello World\n")
    close(s)
    close(sock)
end
wait(c)
@test readall(connect(port)) == "Hello World\n"

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

@test_throws Base.UVError getaddrinfo(".invalid")
@test_throws Base.UVError connect("localhost", 21452)

server = listen(port)
@async @test_throws ErrorException accept(server)
sleep(0.1)
close(server)

server = listen(port)
@async connect("localhost",port)
s1 = accept(server)
@test_throws ErrorException accept(server,s1)
close(server)

@test_throws Base.UVError connect(".invalid",80)

a = UDPSocket()
b = UDPSocket()
bind(a,ip"127.0.0.1",port)
bind(b,ip"127.0.0.1",port+1)

c = Condition()
@async begin
    @test bytestring(recv(a)) == "Hello World"
    # Issue 6505
    @async begin
        @test bytestring(recv(a)) == "Hello World"
        notify(c)
    end
    send(b,ip"127.0.0.1",port,"Hello World")
end
send(b,ip"127.0.0.1",port,"Hello World")
wait(c)

@test_throws MethodError bind(UDPSocket(),port)

close(a)
close(b)
