# Dates and Times for Julia
#
# The types and functions in this class store and manipulate representations of dates and times
# Instances are stored internally as a Julian Date (jd), which is a representation of dates counting linearly 
# in days since midnight localtime on January 1, 4713 BCE. The original Julian Day Number, counting in days
# since noon GMT, is refered to as the astronomical julian date (ajd) within this code base. 

# Only minimal timezone support exists. DateTime objects keep track of timezones supplied, and use timezones 
# in difference calculations. However, no timezone conversion functionality is provided. DST is also not 
# considered by this code. Both these items are cosidered to be the responsibility of the calling code. 

MONTHS = ["January" , "February", "March", "April", "May", "June", "July", "August", "Septempber", "October", "November", "December"]
SHORT_MONTHS = ["Jan" , "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
DAY_OF_WEEK = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
SHORT_DAY_OF_WEEK = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

_UNIXEPOCH = 2440588 # JD of midnight localtime 1/1/70 



type DateTime{T<:Real} 
	jd::T
	off::Float
	zone::ASCIIString

	function DateTime{T<:Real}(jd::T)
		new(jd, 0.0, "UTC")
	end

	function DateTime{T<:Real}(jd::T, off::Float, zone::ASCIIString)
		new(jd, off, zone)
	end	
end

typealias Date DateTime{Int}

#creation functions. 
function date(y::Int,m::Int,d::Int)
	jd::Int = valid_date(y, m, d)
	if jd==-1 
		throw ("Invalid date: $y-$m-$d")
	else 
		DateTime{Int}(jd,0.0, "UTC")
	end
end

function datetime(y::Integer, m::Integer, d::Integer, hh::Integer, mm::Integer, ss::Integer, frac::Float, off::Float, zone::ASCIIString)
	jd = valid_date(int(y), int(m), int(d))
	if jd==-1 
		throw ("Invalid date: $y-$m-$d")
	else 
		jd=jd+_time_to_day_frac(hh,mm,ss) + frac/86400
		DateTime{Float64}(jd, off/24, zone)
	end
end

#Create a datetime upto seconds accuracy with local timezone
function datetime(y::Integer, m::Integer, d::Integer, hh::Integer, mm::Integer, ss::Integer)
	datetime(y,m,d,hh,mm,ss,0.0, TZ_OFFSET, TZ)
end

function string{T<:Float}(dt::DateTime{T}) 
	(y,m,d, hh, mm, ss, fr) = civil(dt)
	"$(d) $(SHORT_MONTHS[m]) $(y) $(hh):$(mm):$(ss).$(string(round(fr*100)/100)[3:end]) ($(dec(int(dt.off * 2400),4))) "
end

function string{T<:Integer}(dt::DateTime{T}) 
	(y,m,d) = civil(dt)
	"$(d) $(SHORT_MONTHS[m]) $(y)"
end

show(d::DateTime) = print(string(d))

(-){T<:Real,S<:Real} (x::DateTime{T}, y::DateTime{S}) = convert(promote_type(T,S), x.jd - y.jd - (x.off - y.off))
(-){T<:Real, S<:Real} (x::DateTime{T}, y::S) = DateTime{promote_type(T,S)}(x.jd - y, x.off, x.zone)
(+){T<:Real,S<:Real} (x::DateTime{T}, y::S) = DateTime{promote_type(T,S)}(x.jd + y, x.off, x.zone)
(+){T<:Real,S<:Real} (x::S, y::DateTime{T}) = DateTime{promote_type(T,S)}(y.jd + x, y.off, x.zone)


<=(x::DateTime, y::DateTime) = x-y <= 0 
>=(x::DateTime, y::DateTime) = x-y >= 0 
<(x::DateTime, y::DateTime) = x-y < 0 
>(x::DateTime, y::DateTime) = x-y > 0 


hash(d::DateTime) = bitmix(hash(d.jd), hash(d.off))
isequal(x::DateTime, y::DateTime) = isequal(x.jd, y.jd) && isequal(x.off, y.off)

function current_time_millis()
    return int(floor(time()*10^3))
end

function current_time_micros()
    return int(floor(time()*10^6))
end

function now()
	t = ccall(:clock_now, Float64, ())  #Seconds since unix epoch
	tm = Array(Uint32, 14)
    ccall(:localtime_r, Ptr{Void}, (Ptr{Int}, Ptr{Uint32}), &t, tm)
	datetime( int(tm[6]) + 1900,  	#int tm_year
			int(tm[5]) + 1 ,		#int tm_mon
			int(tm[4]) ,			#int tm_mday
			int(tm[3]) ,			#int tm_hour
			int(tm[2]) ,			#int tm_min
			int(tm[1]) , 			#int tm_sec
			(t-floor(t)),
			tm[11] / 3600,			#long tm_gmtoff
			cstring(convert(Ptr{Uint8}, ((uint64(0)|tm[14]) << 32 ) | tm[13])) #char *tm_zone
		)
end

#Day of the week for any day, 1=Sunday, 7=Saturday
function wday(d::DateTime)
	((ifloor(d.jd) + 1) % 7) +1
end


common_year_yday_offset = [
    0,
    0 + 31,
    0 + 31 + 28,
    0 + 31 + 28 + 31,
    0 + 31 + 28 + 31 + 30,
    0 + 31 + 28 + 31 + 30 + 31,
    0 + 31 + 28 + 31 + 30 + 31 + 30,
    0 + 31 + 28 + 31 + 30 + 31 + 30 + 31,
    0 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31,
    0 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30,
    0 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31,
    0 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 ]
 
leap_year_yday_offset = [ 
    0,
    0 + 31,
    0 + 31 + 29,
    0 + 31 + 29 + 31,
    0 + 31 + 29 + 31 + 30,
    0 + 31 + 29 + 31 + 30 + 31,
    0 + 31 + 29 + 31 + 30 + 31 + 30,
    0 + 31 + 29 + 31 + 30 + 31 + 30 + 31,
    0 + 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31,
    0 + 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30,
    0 + 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31,
    0 + 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 ]


#Day of the year for any date 1=1JanYY, 365/366=31DecYY
function yday(dt::DateTime)
	y,m,d = _jd_to_date(dt.jd)
	tm_year = y - 1900 ; 
    tm_year_mod400::Integer = tm_year % 400;
    tm_yday = d;

    if (leap_year(tm_year_mod400 + 1900))
		tm_yday = tm_yday + leap_year_yday_offset[m];
    else
		tm_yday = tm_yday + common_year_yday_offset[m];
	end
end


function leap_year(y::Integer)
	((y % 4 == 0) && (y % 100 != 0)) || (y % 400 == 0) 
end

function leap_year(d::Date)
	leap_year(d.year)
end

function is_julian(jd::Integer)
	jd < 2299161 # Date of Gregorian Calendar Reform, ITALY; 1582-10-15 
end

function civil{T<:Float}(dt::DateTime{T})
	return append(_jd_to_date(ifloor(dt.jd)), _day_frac_to_time(dt.jd-ifloor(dt.jd)))
end

function civil{T<:Integer}(dt::DateTime{T})
	return _jd_to_date(dt.jd)
end

#convert a date to a Julian Day Number
function _date_to_jd (y,m,d)
	if m <= 2
      y -= 1
      m += 12
    end
    a = int(floor(y / 100.0))
    b = 2 - a + int(floor(a / 4.0))
    jd = int(floor(365.25 * (y + 4716))) + int(floor(30.6001 * (m + 1))) + d + b - 1524
    if is_julian(jd)
    	jd -= b
    end
    return jd
end

function _jd_to_date (jd::Integer)
	if is_julian(jd)
		a=jd
	else
		x = int(floor((jd - 1867216.25) / 36524.25))
	    a = jd + 1 + x - int(floor(x / 4.0))
	end
    b = a + 1524
    c = ifloor((b - 122.1) / 365.25)
    d = ifloor(365.25 * c)
    e = ifloor((b - d) / 30.6001)
    dom = b - d - ifloor(30.6001 * e)
    if e <= 13
      m = e - 1
      y = c - 4716
    else
      m = e - 13
      y = c - 4715
    end

    return y, m , dom
end

function _day_frac_to_time(fr::Float)
    h,   fr = divmod(fr, 1//24)
    min, fr = divmod(fr, 1//1440)
    s,   fr = divmod(fr, 1//86400)
    return int(h), int(min), int(s), float64(fr*86400)
 end

function _time_to_day_frac(hh::Integer, mm::Integer, ss::Integer )
	 hh/24 + mm/1440 + ss/86400
end

function valid_date(y,m,d)
	jd=_date_to_jd(y,m,d)
	if (y, m, d) == _jd_to_date(jd)
		return jd
	else 
		return -1
	end
end

function _ajd_to_jd(ajd::Real, off::Float)
	ajd + off + 0.5
end

function _jd_to_ajd(jd::Real, off::Float)
	jd -off - 0.5
end

function unixtime(dt::DateTime)
	(dt.jd-_UNIXEPOCH)*86400
end

TZ = 0
TZ_OFFSET = ""
function init()
	d=now();
	global TZ = d.zone
	global TZ_OFFSET = d.off * 24
end	

divmod(x, y) = (div(x,y) , mod(x, y)) 

init()








