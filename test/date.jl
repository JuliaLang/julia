
load("base/date.jl")

@assert yday(date(2012, 1,1)) == 1
@assert yday(date(2012, 1,2)) == 2
@assert yday(date(2012, 12,30)) == 365
@assert yday(date(2012, 12,31)) == 366
@assert yday(date(2011, 12,31)) == 365
@assert yday(date(2013, 12,31)) == 365

@assert leap_year(2012)
@assert !leap_year(2011)
@assert !leap_year(2013)
@assert leap_year(2020)
@assert !leap_year(1900)
@assert !leap_year(1901)
@assert leap_year(1904)
@assert leap_year(2000)


#Test internal conversion functions
@assert date(2012, 4,10).jd == 2456028 #Today
@assert date(0,1,1).jd == 1721058  #Start of A.D
@assert date(-4712, 1, 1).jd == 0  #Start of Julian day count
@assert _jd_to_date(_date_to_jd(2018, 9,10)) == (2018,9,10)
@assert _jd_to_date(_date_to_jd(2018, 9,10)+365) == (2019,9,10)
@assert _jd_to_date(_date_to_jd(2018, 9,10)+365+366) == (2020,9,10) #2020 is a leap year


@assert date(1969, 12, 31) + 1 == date(1970, 1,1)
@assert isa(date(1970, 1,1) - date(1969, 12, 31), Int)
@assert date(1970, 1,1) - date(1969, 12, 31) == 1
@assert date(1969, 12, 31) - date(1970, 1,1) == -1

@assert date(1900, 1,1) - date(1899, 12, 31) == 1
@assert date(1900, 1,1) - date(1899, 1, 1) == 365
@assert date(1904, 3,1) - date(1904, 2, 28) == 2

@assert date(2012, 4, 10) - date(0, 1,1)  == 734970  #Century to today

#Invalid dates
@assert_fails date(1900,2,29)
@assert_fails date(2012,2,30)
@assert_fails date(2012,13,1)
@assert_fails date(2012,12,32)

#test correct hashing and identity
s = Set{Date}()
add(s,date(2012, 4,10))
@assert has(s, date(2012, 4, 10))

@assert SHORT_DAY_OF_WEEK[wday(date(2012, 1, 1))] == "Sun"
@assert SHORT_DAY_OF_WEEK[wday(date(2013, 1, 1))] == "Tue"

#Test day of week over many years
function test_week_cycle()
	d=date(2012, 4, 1) #1;Sunday
	for i=1:10000
		@assert wday(d) == ((i-1)%7)+1
		 d = d+1
	end

	d=date(2012, 4, 1)
	for i=1:100000
	    @assert wday(d) == ((-i+1)%7)+1
	    d = d-1
    end
end

test_week_cycle()

#DateTime tests

@assert isa(now(), DateTime)
@assert typeof(now()) == DateTime{Float64}

@assert typeof(datetime(2012, 4, 24, 13,0,0)) == DateTime{Float64}
@assert typeof(datetime(2012, 4,24,14,0,0, 0.0, 1.0, "BST")) == DateTime{Float64}
#Test unix time and julian days are congruous, upto second accuracy
#ifloor(strptime("Tue 24 Apr 13:57:16 2012")) = 1335275836
@assert ifloor((datetime(2012, 4,24, 13,57, 16, 0.0, 1.0, "BST").jd-_UNIXEPOCH)*86400) - 1335275836 == 0

@assert_approx_eq ( datetime(2012, 4, 24, 13,0,0) - datetime(2012, 4,24,14,0,0)  ) * 24  -1
@assert isa(datetime(2012, 4, 24, 13,0,0) - datetime(2012, 4,24,14,0,0), Float64)
@assert_approx_eq ( datetime(2012, 4, 24, 13,0,0, 0.0, 0.0, "GMT") - datetime(2012, 4,24,14,0,0, 0.0, 1.0, "BST")  ) * 24  0


d= datetime(2012, 4, 24, 13,10,5) 
@assert hour(d) == 13
@assert minute(d)==10
@assert second(d) = 5
@assert mday(d) == 24
@assert month(d) == 4
@assert year(d) == 2012

d=date(2012,2,1)
@assert year(d) = 2012
@assert month(d) = 2
@assert mday(d) = 1


