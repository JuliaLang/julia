_jl_zlib_wrapper = dlopen("zlib_wrapper")

type CompressedBytes
	zp::Ptr{Void}
	t
	
	function CompressedBytes(zp::Ptr{Void}, t)
		cs = new(zp, t)
		finalizer(cs, _jl_zlib_free_pack)
		cs
	end
end

function deflate(input::Array{Uint8,1})
	p = convert(Ptr{Uint8}, input)
	sz = convert(Uint, 0)
	zp = ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_deflate), Ptr{Void}, (Ptr{Uint8},), p)
	CompressedBytes(zp, typeof(input).name.name)
end

function inflate(cs::CompressedBytes)
	cp = ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_inflate), Ptr{Uint8}, (Ptr{Void},), cs.zp)
	s = convert(cs.t, cp)
	_c_free(cp)
	s
end

function string(cs::CompressedBytes)
	cp = ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_print_pack), Ptr{Uint8}, (Ptr{Void},), cs.zp)
	s = cstring(cp) # copies the string
	_c_free(cp)
	s
end

convert(::Type{String}, x::CompressedBytes) = string(x)

promote_rule(::Type{String}, ::Type{CompressedBytes}) = String

function cmp(x::CompressedBytes, y::CompressedBytes)
	cmp(string(x), string(y))
end

function cmp(x::CompressedBytes, y::String)
	cmp(string(x), y)
end

function _jl_zlib_free_pack(cs::CompressedBytes)
	return ccall(dlsym(_jl_zlib_wrapper, :_jl_zlib_free_pack), Void, (Ptr{Void},), cs.s)
end