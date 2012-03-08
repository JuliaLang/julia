_jl_zlib_wrapper = dlopen("zlib_wrapper")

type CompressedString <: String
	zp::Ptr{Void}
	
	function CompressedString(zp::Ptr{Void})
		cs = new(zp)
		finalizer(cs, _jl_zlib_free_pack)
		cs
	end
end

type UncompressedString <: String
	zp::Ptr{Uint8}
	
	function UncompressedString(zp::Ptr{Void})
		us = new(zp)
		finalizer(us, _jl_zlib_free_pack)
		us
	end
end

function deflate(s::String)
	sz = convert(Uint, 0)
	zp = ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_deflate), Ptr{Void}, (Ptr{Uint8},), s)
	CompressedString(zp)
end

function inflate(cs::CompressedString)
	so = ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_inflate), Ptr{Void}, (Ptr{Void},), cs.zp)
	UncompressedString(so)
end

function string(cs::CompressedString)
	c = ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_print_pack), Ptr{Uint8}, (Ptr{Void},), cs.zp)
	s = cstring(c)
	_c_free(c)
	s
end

function string(us::UncompressedString)
	c = ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_print_pack), Ptr{Uint8}, (Ptr{Void},), us.zp)
	s = cstring(c)
	_c_free(c)
	s
end

convert(::Type{String}, x::CompressedString) = string(x)
convert(::Type{String}, x::UncompressedString) = string(x)

promote_rule(::Type{String}, ::Type{CompressedString}) = String
promote_rule(::Type{String}, ::Type{UncompressedString}) = String

function cmp(x::CompressedString, y::CompressedString)
	cmp(string(x), string(y))
end

function cmp(x::UncompressedString, y::UncompressedString)
	cmp(string(x), string(y))
end

function cmp(x::CompressedString, y::UncompressedString)
	cmp(string(x), string(y))
end

function cmp(x::CompressedString, y::String)
	cmp(string(x), y)
end
function cmp(x::UncompressedString, y::String)
	cmp(string(x), y)
end

function _jl_zlib_free_pack(cs::CompressedString)
	return ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_free_pack), Void, (Ptr{Void},), cs.s)
end