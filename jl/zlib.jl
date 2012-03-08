_jl_zlib_wrapper = dlopen("zlib_wrapper")

type CompressedString <: String
	s::Ptr{Uint8}
	sz::Uint
	
	function CompressedString(s::Ptr{Uint8}, sz::Uint)
		cs = new(s, sz)
		finalizer(cs, _jl_zlib_free)
		cs
	end
end

type UncompressedString <: String
	s::Ptr{Uint8}
	
	function UncompressedString(s::Ptr{Uint8})
		us = new(s)
		finalizer(us, _jl_zlib_free)
		us
	end
end

function deflate(s::String)
	so = Array(Ptr{Uint8},1)
	sz = 0
	ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_deflate), Uint8, (Ptr{Uint8}, Ptr{Uint}, Ptr{Ptr{Uint8}}), cstring(s)[1], &sz, so)
	CompressedString(so[1], sz)
end

function inflate(cs::CompressedString)
	so = Array(Ptr{Uint8},1)
	ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_inflate), Int, (Ptr{Uint8}, Uint, Ptr{Ptr{Uint8},1}), cs.s, cs.sz, so)
	so
end

function show(cs::CompressedString) 
	print(cs.s)
end

function show(us::UncompressedString)
	print(us.s)
end

function _jl_zlib_free(cs::CompressedString)
	return ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_free), Void, (Ptr{Uint8},), cs.s)
end

function _jl_zlib_free(cs::UncompressedString)
	return ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_free), Void, (Ptr{Uint8},), cs.s)
end