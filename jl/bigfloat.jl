_jl_libgmp_wrapper = dlopen("libgmp_wrapper")

load("bigint.jl")

type BigFloat <: Float
	mpf::Ptr{Void}

	function BigFloat(x::String) 
		z = _jl_BigFloat_init()
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_string), Void, (Ptr{Void}, Ptr{Uint8}), z, cstring(x))
		b = new(z)
		finalizer(b, _jl_BigFloat_clear)
		b
	end

	function BigFloat(x::Float) 
		z = _jl_BigFloat_init()
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_d), Void, (Ptr{Void}, Float), z, x)
		b = new(z)
		finalizer(b, _jl_BigFloat_clear)
		b
	end
	
	function BigFloat(x::Uint) 
		z = _jl_BigFloat_init()
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_ui), Void, (Ptr{Void}, Uint), z, x)
		b = new(z)
		finalizer(b, _jl_BigFloat_clear)
		b
	end
	
	function BigFloat(x::Int) 
		z = _jl_BigFloat_init()
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_si), Void, (Ptr{Void}, Int), z, x)
		b = new(z)
		finalizer(b, _jl_BigFloat_clear)
		b
	end
	
	function BigFloat(x::BigInt) 
		z = _jl_BigFloat_init()
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_set_z), Void, (Ptr{Void}, Ptr{Void}), z, x.mpz)
		b = new(z)
		finalizer(b, _jl_BigFloat_clear)
		b
	end

	function BigFloat(z::Ptr{Void}) 
		b = new(z)
		finalizer(b, _jl_BigFloat_clear)
		b
	end
end

convert(::Type{BigFloat}, x::Int8)   = BigFloat(x)
convert(::Type{BigFloat}, x::Int16)  = BigFloat(x)
convert(::Type{BigFloat}, x::Int32)  = BigFloat(x)
convert(::Type{BigFloat}, x::Int64)  = BigFloat(x)
convert(::Type{BigFloat}, x::Uint8)  = BigFloat(x)
convert(::Type{BigFloat}, x::Uint16) = BigFloat(x)
convert(::Type{BigFloat}, x::Uint32) = BigFloat(x)
convert(::Type{BigFloat}, x::Uint64) = BigFloat(x)

convert(::Type{BigFloat}, x::Float) = BigFloat(x)
convert(::Type{BigFloat}, x::Float32) = BigFloat(x)
convert(::Type{BigFloat}, x::Float64) = BigFloat(x)

promote_rule(::Type{BigFloat}, ::Type{Float32}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Float64}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Int8}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Int16}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Int32}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Int64}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Uint8}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Uint16}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Uint32}) = BigFloat
promote_rule(::Type{BigFloat}, ::Type{Uint64}) = BigFloat

function +(x::BigFloat, y::BigFloat) 
	z= _jl_BigFloat_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_add), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
	BigFloat(z)
end

function -(x::BigFloat) 
	z= _jl_BigFloat_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_neg), Void, (Ptr{Void}, Ptr{Void}), z, x.mpf)
	BigFloat(z)
end

function -(x::BigFloat, y::BigFloat)
	z= _jl_BigFloat_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_sub), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
	BigFloat(z)
end

function *(x::BigFloat, y::BigFloat) 
	z= _jl_BigFloat_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_mul), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
	BigFloat(z)
end

function div (x::BigFloat, y::BigFloat)
	z= _jl_BigFloat_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_div), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}), z, x.mpf, y.mpf)
	BigFloat(z)
end

function cmp(x::BigFloat, y::BigFloat) 
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_cmp), Int, (Ptr{Void}, Ptr{Void}), x.mpf, y.mpf)
end

function pow(x::BigFloat, y::Uint) 
	z = _jl_BigFloat_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_pow_ui), Void, (Ptr{Void}, Ptr{Void}, Uint), z, x.mpf, y)
    BigFloat(z)
end

==(x::BigFloat, y::BigFloat) = cmp(x,y) == 0 
<=(x::BigFloat, y::BigFloat) = cmp(x,y) <= 0 
>=(x::BigFloat, y::BigFloat) = cmp(x,y) >= 0 
<(x::BigFloat, y::BigFloat) = cmp(x,y) < 0 
>(x::BigFloat, y::BigFloat) = cmp(x,y) > 0 

function string(x::BigFloat) 
	s=ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_printf), Ptr{Uint8}, (Ptr{Void},), x.mpf)
	ret = cstring(s) #This copies s. 
	_jl_free(convert(Ptr{Void},s))
	ret
end

function show(x::BigFloat) 
	print (string(x))
end	

function _jl_BigFloat_clear(x::BigFloat) 
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_clear), Void, (Ptr{Void},), x.mpf)
end

function _jl_BigFloat_init() 
	return ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpf_init), Ptr{Void}, ())
end