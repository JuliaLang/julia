if ccall(:jl_generating_output, Cint, ()) == 1
    precompile(Tuple{typeof(Profile.tree!), Profile.StackFrameTree{UInt64}, Vector{UInt64}, Dict{UInt64, Vector{Base.StackTraces.StackFrame}}, Bool, Symbol, Int, UInt})
    precompile(Tuple{typeof(Profile.tree!), Profile.StackFrameTree{UInt64}, Vector{UInt64}, Dict{UInt64, Vector{Base.StackTraces.StackFrame}}, Bool, Symbol, Int, UnitRange{UInt}})
    precompile(Tuple{typeof(Profile.tree!), Profile.StackFrameTree{UInt64}, Vector{UInt64}, Dict{UInt64, Vector{Base.StackTraces.StackFrame}}, Bool, Symbol, UnitRange{Int}, UInt})
    precompile(Tuple{typeof(Profile.tree!), Profile.StackFrameTree{UInt64}, Vector{UInt64}, Dict{UInt64, Vector{Base.StackTraces.StackFrame}}, Bool, Symbol, UnitRange{Int}, UnitRange{UInt}})
    precompile(Tuple{typeof(Profile.tree!), Profile.StackFrameTree{UInt64}, Vector{UInt64}, Dict{UInt64, Vector{Base.StackTraces.StackFrame}}, Bool, Symbol, Vector{Int}, Vector{UInt}})
    precompile(Tuple{typeof(Profile._peek_report)})
    precompile(Tuple{typeof(Profile.Allocs.start)})
    precompile(Tuple{typeof(Profile.Allocs.stop)})
    precompile(Tuple{typeof(Profile.Allocs.fetch)})
end
