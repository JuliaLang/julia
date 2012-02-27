_jl_libgmp_wrapper = dlopen("libgmp_wrapper")

type BigInt <: Integer
	mpz::Ptr{Void}

	function BigInt(x::String) 
		z = _jl_bigint_init()
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_set_string), Void, (Ptr{Void}, Ptr{Uint8}),z,cstring(x))
		b = new(z)
		finalizer(b, _jl_bigint_clear)
		b
	end

	function BigInt(x::Int) 
		z = _jl_bigint_init()
		ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_set_ui), Void, (Ptr{Void}, Uint),z,uint(x))
		b = new(z)
		finalizer(b, _jl_bigint_clear)
		b
	end

	function BigInt(z::Ptr{Void}) 
		b = new(z)
		finalizer(b, _jl_bigint_clear)
		b
	end
end

convert(::Type{BigInt}, x::Int8) = BigInt(int(x))
convert(::Type{BigInt}, x::Int16) = BigInt(int(x))
convert(::Type{BigInt}, x::Int) = BigInt(x)

macro define_bigint_convert ()
	if WORD_SIZE == 64
		:(convert(::Type{BigInt}, x::Int32) = BigInt(int(x)))

	else
		:(convert(::Type{BigInt}, x::Int64) = BigInt(string(x)))
	end
end

@define_bigint_convert

promote_rule(::Type{BigInt}, ::Type{Int8}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int16}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int32}) = BigInt
promote_rule(::Type{BigInt}, ::Type{Int64}) = BigInt

function +(x::BigInt, y::BigInt) 
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_add), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
	BigInt(z)
end

function -(x::BigInt) 
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_neg), Void, (Ptr{Void}, Ptr{Void}),z,x.mpz)
	BigInt(z)
end

function -(x::BigInt, y::BigInt)
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_sub), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
	BigInt(z)
end

function *(x::BigInt, y::BigInt) 
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_mul), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
	BigInt(z)
end

function div (x::BigInt, y::BigInt)
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_div), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
	BigInt(z)
end

function rem (x::BigInt, y::BigInt)
	z= _jl_bigint_init()
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_rem), Void, (Ptr{Void}, Ptr{Void}, Ptr{Void}),z,x.mpz,y.mpz)
	BigInt(z)
end

function cmp(x::BigInt, y::BigInt) 
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_cmp), Int, (Ptr{Void}, Ptr{Void}),x.mpz, y.mpz)
end

==(x::BigInt, y::BigInt) = cmp(x,y) == 0 
<=(x::BigInt, y::BigInt) = cmp(x,y) <= 0 
>=(x::BigInt, y::BigInt) = cmp(x,y) >= 0 
<(x::BigInt, y::BigInt) = cmp(x,y) < 0 
>(x::BigInt, y::BigInt) = cmp(x,y) > 0 

function string(x::BigInt) 
	s=ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_printf), Ptr{Uint8}, (Ptr{Void},),x.mpz)
	ret = cstring(s) #This copies s. 
	_jl_free(convert(Ptr{Void},s))
	ret
end

function show(x::BigInt) 
	print (string(x))
end	

function _jl_bigint_clear(x::BigInt) 
	ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_clear), Void, (Ptr{Void},),x.mpz)
end

function _jl_bigint_init() 
	return ccall(dlsym(_jl_libgmp_wrapper, :_jl_mpz_init), Ptr{Void}, ())
end