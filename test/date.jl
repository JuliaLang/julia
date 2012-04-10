
@assert typeof(now()) == DateTime

@assert yday(Date(2012, 1,1)) == 1
@assert yday(Date(2012, 1,2)) == 2
@assert yday(Date(2012, 12,30)) == 365
@assert yday(Date(2012, 12,31)) == 366
@assert yday(Date(2011, 12,31)) == 365
@assert yday(Date(2013, 12,31)) == 365

@assert leap_year(2012)
@assert !leap_year(2011)
@assert !leap_year(2013)
@assert leap_year(2020)
@assert !leap_year(1900)
@assert !leap_year(1901)
@assert leap_year(1904)
@assert leap_year(2000)


#Test internal conversion functions
@assert Date(2012, 4,10).jd == 2456028 #Today
@assert Date(0,1,1).jd == 1721058  #Start of A.D
@assert Date(-4712, 1, 1).jd == 0  #Start of Julian day count
@assert _jd_to_date(_date_to_jd(2018, 9,10)) == (2018,9,10)
@assert _jd_to_date(_date_to_jd(2018, 9,10)+365) == (2019,9,10)
@assert _jd_to_date(_date_to_jd(2018, 9,10)+365+366) == (2020,9,10) #2020 is a leap year


@assert Date(1969, 12, 31) + 1 == Date(1970, 1,1)
@assert Date(1970, 1,1) - Date(1969, 12, 31) == 1
@assert Date(1969, 12, 31) - Date(1970, 1,1) == -1

@assert Date(1900, 1,1) - Date(1899, 12, 31) == 1
@assert Date(1900, 1,1) - Date(1899, 1, 1) == 365
@assert Date(1904, 3,1) - Date(1904, 2, 28) == 2

@assert Date(2012, 4, 10) - Date(0, 1,1)  == 734970  #Century to today

#Invalid dates
@assert_fails Date(1900,2,29)
@assert_fails Date(2012,2,30)
@assert_fails Date(2012,13,1)
@assert_fails Date(2012,12,32)

#test correct hashing and identity
s = Set{Date}()
add(s,Date(2012, 4,10))
@assert has(s, Date(2012, 4, 10))

@assert SHORT_DAY_OF_WEEK[day_of_week(Date(2012, 1, 1))] == "Sun"
@assert SHORT_DAY_OF_WEEK[day_of_week(Date(2013, 1, 1))] == "Tue"

#Test day of week over many years
function test_week_cycle()
	d=Date(2012, 4, 1) #1;Sunday
	for i=1:10000
		@assert day_of_week(d) == ((i-1)%7)+1
		 d = d+1
	end

	d=Date(2012, 4, 1)
	for i=1:100000
	    @assert day_of_week(d) == ((-i+1)%7)+1
	    d = d-1
    end
end

test_week_cycle()




