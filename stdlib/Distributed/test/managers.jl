using Test
using Distributed
using Sockets
using Distributed: parse_machine, bind_client_port, SSHManager, LocalManager

@test parse_machine("127.0.0.1") == ("127.0.0.1", nothing)
@test parse_machine("127.0.0.1:80") == ("127.0.0.1", 80)
@test parse_machine("[2001:db8::1]") == ("2001:db8::1", nothing)
@test parse_machine("[2001:db8::1]:443") == ("2001:db8::1", 443)

@test parse_machine("127.0.0.1:90") == ("127.0.0.1", 90)
@test parse_machine("127.0.0.1:1") == ("127.0.0.1", 1)
@test parse_machine("127.0.0.1:65535") == ("127.0.0.1", 65535)
@test_throws ArgumentError parse_machine("127.0.0.1:-1")
@test_throws ArgumentError parse_machine("127.0.0.1:0")
@test_throws ArgumentError parse_machine("127.0.0.1:65536")

for ip in (IPv4(0), IPv6(0))
    sock = TCPSocket()
    @test bind_client_port(sock, typeof(ip)) === sock
    addr, port = getsockname(sock)
    @test addr === ip
    @test port::UInt16 === Distributed.client_port[] != 0
end

@test occursin(r"^SSHManager\(machines=.*\)$",
               sprint((t,x) -> show(t, "text/plain", x), SSHManager("127.0.0.1")))
@test sprint((t,x) -> show(t, "text/plain", x), LocalManager(1, true)) == "LocalManager()"
