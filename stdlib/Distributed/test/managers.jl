using Test
using Distributed
using Sockets
using Distributed: parse_machine, bind_client_port

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

sock = bind_client_port(TCPSocket(), typeof(IPv4(0)))
addr, port = getsockname(sock)
@test addr == ip"0.0.0.0"

sock = bind_client_port(TCPSocket(), typeof(IPv6(0)))
addr, port = getsockname(sock)
@test addr == ip"::"