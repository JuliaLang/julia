
TIME_SCALE = 1000 #Millisecond precision on DateTime objects

MONTHS = ["January" , "February", "March", "April", "May", "June", "July", "August", "Septempber", "October", "November", "December"]
SHORT_MONTHS = ["Jan" , "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
DAY_OF_WEEK = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
SHORT_DAY_OF_WEEK = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]


type DateTime
	jd::Float
	year::Integer
	mon::Integer
	mday::Integer
	hour::Integer
	min::Integer
	sec::Integer
	subsec::Integer
	isdst::Integer
	utc_offset::Integer
	zone::String

	function DateTime(y::Integer, m::Integer, d::Integer, hh::Integer, mm::Integer, ss::Integer, sss::Integer, dst::Integer, off::Integer, z::String)
		jd = valid_date(y, m, d)
		if jd==-1 
			throw ("Invalid date: $y-$m-$d")
		else 
			new(jd, y, m, d, hh, mm, ss, sss, dst, off, z)
		end
	end
end

type Date
	jd::Int
	year::Integer
	mon::Integer
	mday::Integer

	function Date(y::Integer, m::Integer, d::Integer)
		jd = valid_date(y, m, d)
		if jd==-1 
			throw ("Invalid date: $y-$m-$d")
		else 
			new(jd, y, m, d)
		end
	end
end


convert(::Type{Date}, d::DateTime) = Date(d.year, d.mon, d.mday)
convert(::Type{DateTime}, d::Date) = DateTime(d.year, d.mon, d.mday,0,0,0,0,
						ccall(:default_isdst, Int32, ()) ,
						ccall(:default_gmtoff, Int, ()), 
						cstring(ccall(:default_tzone, Ptr{Uint8}, ())) )


function string(d::DateTime) 
	"$(d.mday) $(SHORT_MONTHS[d.mon]) $(d.year) $(d.hour):$(d.min):$(d.sec).$(d.subsec) $(d.zone)"
end

function string(d::Date) 
	"$(d.mday) $(SHORT_MONTHS[d.mon]) $(d.year)"
end

show(d::DateTime) = print(string(d))
show(d::Date) = print(string(d))

(-) (x::Date, y::Date) = x.jd - y.jd
(-) (x::Date, y::Integer) = Date(_jd_to_date(x.jd-y)...)
(+) (x::Date, y::Integer) = Date(_jd_to_date(x.jd+y)...)
(+) (x::Integer, y::Date) = Date(_jd_to_date(y.jd+x)...)


<=(x::Date, y::Date) = x-y <= 0 
>=(x::Date, y::Date) = x-y >= 0 
<(x::Date, y::Date) = x-y < 0 
>(x::Date, y::Date) = x-y > 0 


hash(d::Date) = hash(d.jd)
isequal(x::Date, y::Date) = isequal(x.jd, y.jd) 

function current_time_millis()
    return int(floor(time()*10^3))
end

function current_time_micros()
    return int(floor(time()*10^6))
end

function now()
	t = ccall(:clock_now, Float64, ())
	tm = ccall(:localtime, Ptr{Void}, (Ptr{Int},), &(int(floor(t))))

	DateTime( ccall(:read_tm_year, Int32, (Ptr{Void},), tm) + 1900,
			ccall(:read_tm_mon, Int32, (Ptr{Void},), tm) + 1 ,
			ccall(:read_tm_mday, Int32, (Ptr{Void},), tm) ,
			ccall(:read_tm_hour, Int32, (Ptr{Void},), tm) ,
			ccall(:read_tm_min, Int32, (Ptr{Void},), tm) ,
			ccall(:read_tm_sec, Int32, (Ptr{Void},), tm) ,
			int((t-floor(t))*TIME_SCALE),
			ccall(:read_tm_isdst, Int32, (Ptr{Void},), tm) ,
			ccall(:read_tm_gmtoff, Int, (Ptr{Void},), tm) ,
			cstring(ccall(:read_tm_zone, Ptr{Uint8}, (Ptr{Void},), tm)) 
		)
end

#Day of the week for any day, 1=Sunday, 7=Saturday
function day_of_week(d::Date)
	year::Integer = d.year; month::Integer = d.mon; day::Integer = d.mday
	a = div((14 - month) , 12);
    y = year + 4800 - a;
    m = month + 12 * a - 3;
    wday = day + div((153*m+2),5) + 365*y + div(y,4) - div(y,100) + div(y,400) + 2;
    wday = wday  % 7;
    return wday + 1;
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
function yday(d::Date)

	tm_year = d.year - 1900 ; 
    tm_year_mod400::Integer = tm_year % 400;
    tm_yday = d.mday;

    if (leap_year(tm_year_mod400 + 1900))
		tm_yday = tm_yday + leap_year_yday_offset[d.mon];
    else
		tm_yday = tm_yday + common_year_yday_offset[d.mon];
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

#The following three functions are used during construction of a Date object, 
#   and thus operate on primitive arguments

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
    c = int(floor((b - 122.1) / 365.25))
    d = int(floor(365.25 * c))
    e = int(floor((b - d) / 30.6001))
    dom = b - d - int(floor(30.6001 * e))
    if e <= 13
      m = e - 1
      y = c - 4716
    else
      m = e - 13
      y = c - 4715
    end

    return y, m , dom
end

function valid_date(y,m,d)
	jd=_date_to_jd(y,m,d)
	if (y, m, d) == _jd_to_date(jd)
		return jd
	else 
		return -1
	end
end






