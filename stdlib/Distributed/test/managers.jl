using Test
using Distributed
using Base: Semaphore
using Sockets

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "stdlib", "v1.5", "Distributed", "src")
@eval Main include(joinpath($(BASE_TEST_PATH), "managers.jl"))

@test parse_machine("127.0.0.1") == ("127.0.0.1", ``)
@test parse_machine("127.0.0.1:80") == ("127.0.0.1", "80")
@test parse_machine("[2001:db8::1]") == ("2001:db8::1", ``)
@test parse_machine("[2001:db8::1]:443") == ("2001:db8::1", "443")


