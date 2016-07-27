using Base.Test

include("ctx.jl")

typealias CPU Void
immutable GPU; end

@context GPU foobar() = 123456789
foobar() = 987654321

codestr(linfos) = sprint(io->show(io, MIME"text/plain"(), linfos[1]))

@test contains(codestr(code_lowered(foobar, Tuple{})),
               string(987654321))
@test contains(codestr(code_lowered(foobar, Tuple{}, ctx=GPU)),
               string(123456789))
@test contains(codestr(code_lowered(foobar, Tuple{GPU}, ctx=Base.Bottom)),
               string(123456789))

@test contains(codestr(code_typed(foobar, Tuple{})),
               string(987654321))
@test contains(codestr(code_typed(foobar, Tuple{}, ctx=GPU)),
               string(123456789))
@test contains(codestr(code_typed(foobar, Tuple{GPU}, ctx=Base.Bottom)),
               string(123456789))

@test contains(sprint(io->code_llvm(io, foobar, Tuple{})),
               string(987654321))
@test contains(sprint(io->code_llvm(io, foobar, Tuple{}, true, false, GPU)),
               string(123456789))
@test contains(sprint(io->code_llvm(io, foobar, Tuple{GPU}, true, false, Base.Bottom)),
               string(123456789))

@test contains(sprint(io->code_native(io, foobar, Tuple{})),
               string(987654321))
@test contains(sprint(io->code_native(io, foobar, Tuple{}, GPU)),
               string(123456789))
@test contains(sprint(io->code_native(io, foobar, Tuple{GPU}, Base.Bottom)),
               string(123456789))
