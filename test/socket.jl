@test ip"127.0.0.1" == IPv4(127,0,0,1)
@test ip"192.0" == IPv4(192,0,0,0)
@test ip"192.0xFFF" == IPv4(192,0,15,255)
@test ip"192.0xFFFF" == IPv4(192,0,255,255)
@test ip"192.0xFFFFF" == IPv4(192,15,255,255)
@test ip"192.0xFFFFFF" == IPv4(192,255,255,255)
@test ip"022.0.0.1" == IPv4(18,0,0,1)

@test UInt(IPv4(0x01020304)) == 0x01020304
@test Int(IPv4("1.2.3.4")) == Int(0x01020304) == Int32(0x01020304)
@test Int128(IPv6("2001:1::2")) == 42540488241204005274814694018844196866
@test_throws InexactError Int16(IPv4("1.2.3.4"))
@test_throws InexactError Int64(IPv6("2001:1::2"))

let ipv = parseip("127.0.0.1")
    @test isa(ipv, IPv4)
    @test ipv == ip"127.0.0.1"
end

@test_throws ArgumentError Base.parseipv4("192.0xFFFFFFF")
@test_throws ArgumentError IPv4(192,255,255,-1)
@test_throws ArgumentError IPv4(192,255,255,256)

@test_throws ArgumentError Base.parseipv4("192.0xFFFFFFFFF")
@test_throws ArgumentError Base.parseipv4("192.")

@test ip"::1" == IPv6(1)
@test ip"2605:2700:0:3::4713:93e3" == IPv6(parse(UInt128,"260527000000000300000000471393e3",16))

@test ip"2001:db8:0:0:0:0:2:1" == ip"2001:db8::2:1" == ip"2001:db8::0:2:1"

@test ip"0:0:0:0:0:ffff:127.0.0.1" == IPv6(0xffff7f000001)

let ipv = parseip("0:0:0:0:0:ffff:127.0.0.1")
    @test isa(ipv, IPv6)
    @test ipv == ip"0:0:0:0:0:ffff:127.0.0.1"
end

@test_throws ArgumentError IPv6(1,1,1,1,1,1,1,-1)
@test_throws ArgumentError IPv6(1,1,1,1,1,1,1,typemax(UInt16)+1)

# test InetAddr constructor
let inet = Base.InetAddr(IPv4(127,0,0,1), 1024)
    @test inet.host == ip"127.0.0.1"
    @test inet.port == 1024
end
# test InetAddr invalid port
@test_throws ArgumentError Base.InetAddr(IPv4(127,0,0,1), -1)
@test_throws ArgumentError Base.InetAddr(IPv4(127,0,0,1), typemax(UInt16)+1)

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

port = RemoteRef()
c = Base.Condition()
defaultport = rand(2000:4000)
tsk = @async begin
    p, s = listenany(defaultport)
    put!(port, p)
    Base.notify(c)
    sock = accept(s)
    write(sock,"Hello World\n")
    close(s)
    close(sock)
end
wait(c)
@test readall(connect(fetch(port))) == "Hello World\n"
wait(tsk)

socketname = (@windows ? "\\\\.\\pipe\\uv-test" : "testsocket") * "-" * randstring(6)
@unix_only isfile(socketname) && Base.FS.unlink(socketname)
for T in (ASCIIString, UTF8String, UTF16String) # test for issue #9435
    tsk = @async begin
        s = listen(T(socketname))
        Base.notify(c)
        sock = accept(s)
        write(sock,"Hello World\n")
        close(s)
        close(sock)
    end
    wait(c)
    @test readall(connect(socketname)) == "Hello World\n"
    wait(tsk)
end

@test_throws Base.UVError getaddrinfo(".invalid")
@test_throws Base.UVError connect("localhost", 21452)

# test invalid port
@test_throws ArgumentError connect(ip"127.0.0.1",-1)
@test_throws ArgumentError connect(ip"127.0.0.1", typemax(UInt16)+1)
@test_throws ArgumentError connect(ip"0:0:0:0:0:ffff:127.0.0.1", -1)
@test_throws ArgumentError connect(ip"0:0:0:0:0:ffff:127.0.0.1", typemax(UInt16)+1)

p, server = listenany(defaultport)
r = RemoteRef()
tsk = @async begin
    put!(r, :start)
    @test_throws Base.UVError accept(server)
end
@test fetch(r) === :start
close(server)
wait(tsk)

port, server = listenany(defaultport)
@async connect("localhost",port)
s1 = accept(server)
@test_throws ErrorException accept(server,s1)
@test_throws Base.UVError listen(port)
port2, server2 = listenany(port)
@test port != port2
close(server)
close(server2)

@test_throws Base.UVError connect(".invalid",80)

begin
    a = UDPSocket()
    b = UDPSocket()
    bind(a,ip"127.0.0.1",port)
    bind(b,ip"127.0.0.1",port+1)

    c = Condition()
    tsk = @async begin
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
    wait(tsk)

    tsk = @async begin
        @test begin
            (addr,data) = recvfrom(a)
            addr == ip"127.0.0.1" && bytestring(data) == "Hello World"
        end
    end
    send(b, ip"127.0.0.1",port,"Hello World")
    wait(tsk)

    @test_throws MethodError bind(UDPSocket(),port)

    close(a)
    close(b)
end
@non_windowsxp_only begin
    a = UDPSocket()
    b = UDPSocket()
    bind(a, ip"::1", UInt16(port))
    bind(b, ip"::1", UInt16(port+1))

    tsk = @async begin
        @test begin
            (addr, data) = recvfrom(a)
            addr == ip"::1" && bytestring(data) == "Hello World"
        end
    end
    send(b, ip"::1", port, "Hello World")
    wait(tsk)
end
