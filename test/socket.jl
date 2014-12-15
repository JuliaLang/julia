
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
