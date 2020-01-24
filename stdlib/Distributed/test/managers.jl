using Test
using Distributed

using Distributed: parse_machine

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

