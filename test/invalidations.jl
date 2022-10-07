const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :MethodInvalidations) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "MethodInvalidations.jl"))

import Base

using .Main.MethodInvalidations
using REPL: REPL

f(x) = x
g(x) = f(x)
g(2)

invs = @method_invalidations k(x) = x
@test !invalidated(invs, g)
invs = @method_invalidations f(x::Int) = 2x
@test invalidated(invs, g)

struct A end

invs = @method_invalidations Base.convert(::Type{String}, ::A) = A()
@test !invalidated(invs, Base.show_unquoted)
@test !invalidated(invs, REPL.REPLHistoryProvider)
@test !invalidated(invs, Base.CoreLogging.handle_message)
