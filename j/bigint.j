_jl_libgmp_wrapper = dlopen("libgmp_wrapper")

## integer conversions ##

type BigInt <: Integer
	mpz::Ptr{Void}

	function BigInt(x::String) 
		z = ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_init), Ptr{Void}, ())
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_set_string), Void, (Ptr{Void}, Ptr{Uint8}),z,cstring(x))
		new(z)
	end

	function BigInt(x::Uint) 
		z = ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_init), Ptr{Void}, ())
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_set_ui), Void, (Ptr{Void}, Uint),z,uint(x))
		new(z)
	end

	function BigInt(z::Ptr{Void}) 
		new(z)
	end
end

BigInt(x::Int64) = BigInt(string(x))

convert(::Type{BigInt}, x::Integer) = BigInt(string(x))
function convert(::Type{BigInt}, x::Int64) 
	if is(Uint, Int64) 
		BigInt(x)
	else
		BigInt(string(x))
	end

end

promote_rule(::Type{BigInt}, ::Type{Int8}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int16}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int32}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int64}) = BigInt

+(x::BigInt, y::BigInt) = _jl_bigint_add(x,y)
-(x::BigInt) = _jl_bigint_neg(x)
-(x::BigInt, y::BigInt) = _jl_bigint_sub(x,y)
*(x::BigInt, y::BigInt) = _jl_bigint_mul(x,y)

function string(x::BigInt) 
	s=ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_printf), Ptr{Uint8}, (Ptr{Void},),x.mpz)
	cstring(s)
	#There is a memory leak here!! s needs to be free'd
end

function show(x::BigInt) 
	print (string(x))
end	

function _jl_bigint_add(x::BigInt, y::BigInt)
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_add), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
	BigInt(z)
end

function _jl_bigint_sub(x::BigInt, y::BigInt)
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_sub), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
	BigInt(z)
end

function _jl_bigint_mul(x::BigInt, y::BigInt)
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_mul), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
	BigInt(z)
end

function _jl_bigint_neg(x::BigInt)
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_neg), Void, (Ptr{Void}, Ptr{Void}),z,x.mpz)
	BigInt(z)
end

function _jl_bigint_init() 
	return ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_init), Ptr{Void}, ())
end