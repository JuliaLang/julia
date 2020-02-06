using Test

using Distributed: bind_client_port
using Sockets

sock = bind_client_port(TCPSocket(), typeof(IPv4(0)))
addr, port = getsockname(sock)
@test addr == ip"0.0.0.0"


sock = bind_client_port(TCPSocket(), typeof(IPv6(0)))
addr, port = getsockname(sock)
@test addr == ip"::"
