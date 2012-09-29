# source: NotOK.jl
#
# author: Jeffrey A. Sarnoff
# date  : 2012-Sep-28



for T in (Int8, Int16, Int32, Int64, Int128)

	begin
	    NotOK(   ::Type{ (T) } ) = typemin( (T) )
	    NotOK( x ::      (T)   ) = NotOK( typeof(x) )
	end
end


for (T,U) in ((Int8,Uint8), (Int16,Uint16), (Int32,Uint32),
              (Int64,Uint64), (Int128,Uint128))
	begin
	    NotOK(   ::Type{ (U) } ) = reinterpret( (U), NotOK((T)) )
	    NotOK( x ::      (T)   ) = NotOK( typeof(x) )
	end
end


for (T,U) in ((Int32,Float32), (Int64,Float64))
	begin
	    NotOK(   ::Type{ (U) } ) = reinterpret( (U), NotOK((T)) )
	    NotOK( x ::      (T)   ) = NotOK( typeof(x) )
	end
end


# for T in (:Int8, :Int16, :Int32, :Int64, :Int128)

# 	@eval begin
# 	    NotOK(   ::Type{ ($T) } ) = typemin( ($T) )
# 	    NotOK( x ::      ($T)   ) = NotOK( typeof(x) )
# 	end
# end
# for (T,U) in ((:Int8,:Uint8), (:Int16,:Uint16), (:Int32,:Uint32),
#               (:Int64,:Uint64), (:Int128,:Uint128))
# 	@eval begin
# 	    NotOK(   ::Type{ $U } ) = reinterpret( ($U), NotOK($T) )
# 	    NotOK( x ::     ($T)  ) = NotOK( typeof(x) )
# 	end
# end

#NotOK( ::Type{Float64}  ) = reinterpret(Float64, NotOK(Int64))
#NotOK( ::Type{Float32}  ) = reinterpret(Float32, NotOK(Int32))

#NotOK( ::Type{ASCIIString} ) = convert(ASCIIString, "")
#NotOK( ::Type{UTF8String} )  = convert(UTF8String,  "")


isNotOK( x::Union(Unsigned,Signed,FloatingPoint) ) = (x === NotOK(x))

