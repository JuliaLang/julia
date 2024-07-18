const CC = Core.Compiler

m = methods(Base._str_sizehint)[1]
mi = m.specializations[1]

name = typeof(Base._str_sizehint).name.mt.name
mod = typeof(Base._str_sizehint).name.module
old_mt =typeof(Base._str_sizehint).name.mt
mt = ccall(:jl_new_method_table, Any, (Any, Any), typeof(Base._str_sizehint).name.mt.name, mod)

ccall(:jl_method_table_insert, Cvoid, (Any, Any, Ptr{Cvoid}), mt, m, C_NULL)

ccall(:jl_, Cvoid, (Any,), mt)
t = Tuple{typeof(Base._str_sizehint), String}
lim = 3
ambig = 0
world = Base.get_world_counter()
min = Ref{UInt}(m.primary_world)
max = Ref{UInt}(m.deleted_world)

has_ambig = C_NULL

ccall(:jl_matching_methods, Any, (Any, Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}, Ptr{Int32}), t, mt, lim, ambig, world, min, max, has_ambig)::Union{Vector{Any},Nothing}
