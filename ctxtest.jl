using Base.Test

include("ctx.jl")

typealias CPU Void
immutable GPU; end

@context GPU foobar() = 123456789
foobar() = 987654321

@test contains(string(code_lowered(foobar, Tuple{})), string(987654321))
@test contains(string(code_lowered(foobar, Tuple{}, ctx=GPU)), string(123456789))
@test contains(string(code_lowered(foobar, Tuple{GPU}, ctx=Base.Bottom)), string(123456789))

@test contains(string(code_typed(foobar, Tuple{})), string(987654321))
@test contains(string(code_typed(foobar, Tuple{}, ctx=GPU)), string(123456789))
@test contains(string(code_typed(foobar, Tuple{GPU}, ctx=Base.Bottom)), string(123456789))

buf = IOBuffer()
code_llvm(buf, foobar, Tuple{})
@test contains(takebuf_string(buf), string(987654321))
code_llvm(buf, foobar, Tuple{}, true, false, GPU)
@test contains(takebuf_string(buf), string(123456789))
code_llvm(buf, foobar, Tuple{GPU}, true, false, Base.Bottom)
@test contains(takebuf_string(buf), string(123456789))

code_native(buf, foobar, Tuple{})
@test contains(takebuf_string(buf), string(987654321))
code_native(buf, foobar, Tuple{}, GPU)
@test contains(takebuf_string(buf), string(123456789))
code_native(buf, foobar, Tuple{GPU}, Base.Bottom)
@test contains(takebuf_string(buf), string(123456789))
