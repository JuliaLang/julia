# Test conversion to and from unix
@test Dates.unix2datetime(Dates.datetime2unix(DateTime(2000,1,1))) == DateTime(2000,1,1)
@test Dates.value(Dates.DateTime(1970)) == Dates.UNIXEPOCH

# Tests from here: http://en.wikipedia.org/wiki/Unix_time
@test string(Dates.unix2datetime(1095379198.75)) == string("2004-09-16T23:59:58.75")
@test string(Dates.unix2datetime(1095379199.00)) == string("2004-09-16T23:59:59")
@test string(Dates.unix2datetime(1095379199.25)) == string("2004-09-16T23:59:59.25")
@test string(Dates.unix2datetime(1095379199.50)) == string("2004-09-16T23:59:59.5")
@test string(Dates.unix2datetime(1095379199.75)) == string("2004-09-16T23:59:59.75")
@test string(Dates.unix2datetime(1095379200.00)) == string("2004-09-17T00:00:00")
@test string(Dates.unix2datetime(1095379200.25)) == string("2004-09-17T00:00:00.25")
@test string(Dates.unix2datetime(1095379200.50)) == string("2004-09-17T00:00:00.5")
@test string(Dates.unix2datetime(1095379200.75)) == string("2004-09-17T00:00:00.75")
@test string(Dates.unix2datetime(1095379201.00)) == string("2004-09-17T00:00:01")
@test string(Dates.unix2datetime(1095379201.25)) == string("2004-09-17T00:00:01.25")
@test string(Dates.unix2datetime(915148798.75)) == string("1998-12-31T23:59:58.75")
@test string(Dates.unix2datetime(915148799.00)) == string("1998-12-31T23:59:59")
@test string(Dates.unix2datetime(915148799.25)) == string("1998-12-31T23:59:59.25")
@test string(Dates.unix2datetime(915148799.50)) == string("1998-12-31T23:59:59.5")
@test string(Dates.unix2datetime(915148799.75)) == string("1998-12-31T23:59:59.75")
@test string(Dates.unix2datetime(915148800.00)) == string("1999-01-01T00:00:00")
@test string(Dates.unix2datetime(915148800.25)) == string("1999-01-01T00:00:00.25")
@test string(Dates.unix2datetime(915148800.50)) == string("1999-01-01T00:00:00.5")
@test string(Dates.unix2datetime(915148800.75)) == string("1999-01-01T00:00:00.75")
@test string(Dates.unix2datetime(915148801.00)) == string("1999-01-01T00:00:01")
@test string(Dates.unix2datetime(915148801.25)) == string("1999-01-01T00:00:01.25")

@test Date(Dates.rata2datetime(734869)) == Dates.Date(2013,1,1)
@test Dates.datetime2rata(Dates.rata2datetime(734869)) == 734869

# Tests from here: http://mysite.verizon.net/aesir_research/date/back.htm#JDN
@test Dates.julian2datetime(1721119.5) == Dates.DateTime(0,3,1)
@test Dates.julian2datetime(1721424.5) == Dates.DateTime(0,12,31)
@test Dates.julian2datetime(1721425.5) == Dates.DateTime(1,1,1)
@test Dates.julian2datetime(2299149.5) == Dates.DateTime(1582,10,4)
@test Dates.julian2datetime(2415020.5) == Dates.DateTime(1900,1,1)
@test Dates.julian2datetime(2415385.5) == Dates.DateTime(1901,1,1)
@test Dates.julian2datetime(2440587.5) == Dates.DateTime(1970,1,1)
@test Dates.julian2datetime(2444239.5) == Dates.DateTime(1980,1,1)
@test Dates.julian2datetime(2452695.625) == Dates.DateTime(2003,2,25,3)
@test Dates.datetime2julian(Dates.DateTime(2013,12,3,21)) == 2456630.375

@test typeof(Dates.now()) <: Dates.DateTime
@test typeof(Dates.today()) <: Dates.Date
@test typeof(Dates.nowtuc()) <: Dates.DateTime