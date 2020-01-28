using Test

using Distributed: bind_client_port
using Sockets

sock = TCPSocket(delay = false)
sock = bind_client_port(sock)
addr, port = getsockname(sock)
@show addr, port
@test addr == ip"0.0.0.0"
