# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test string/show representation of Date
@test string(Dates.Date(1,1,1)) == "0001-01-01" # January 1st, 1 AD/CE
@test sprint(show, Dates.Date(1,1,1)) == "0001-01-01"
@test string(Dates.Date(0,12,31)) == "0000-12-31" # December 31, 1 BC/BCE
@test Dates.Date(1,1,1) - Dates.Date(0,12,31) == Dates.Day(1)
@test Dates.Date(Dates.UTD(-306)) == Dates.Date(0,2,29)
@test string(Dates.Date(0,1,1)) == "0000-01-01" # January 1st, 1 BC/BCE
@test string(Dates.Date(-1,1,1)) == "-0001-01-01" # January 1st, 2 BC/BCE
@test string(Dates.Date(-1000000,1,1)) == "-1000000-01-01"
@test string(Dates.Date(1000000,1,1)) == "1000000-01-01"
@test string(Dates.DateTime(2000,1,1,0,0,0,1)) == "2000-01-01T00:00:00.001"
@test sprint(show,Dates.DateTime(2000,1,1,0,0,0,1)) == "2000-01-01T00:00:00.001"
@test string(Dates.DateTime(2000,1,1,0,0,0,2)) == "2000-01-01T00:00:00.002"
@test string(Dates.DateTime(2000,1,1,0,0,0,500)) == "2000-01-01T00:00:00.5"
@test string(Dates.DateTime(2000,1,1,0,0,0,998)) == "2000-01-01T00:00:00.998"
@test string(Dates.DateTime(2000,1,1,0,0,0,999)) == "2000-01-01T00:00:00.999"

# DateTime parsing
# Useful reference for different locales: http://library.princeton.edu/departments/tsd/katmandu/reference/months.html

# Using parse directly allows for more flexibility.
let str = "1996/02/15 24:00", format = "yyyy/mm/dd HH:MM"
    expected = [Dates.Year(1996), Dates.Month(2), Dates.Day(15), Dates.Hour(24), Dates.Minute(0)]
    @test Dates.parse(str, Dates.DateFormat(format)) == expected
    @test_throws ArgumentError Dates.DateTime(str, Dates.DateFormat(format))
end

# Issue #13644: Ensure that Dates.parse returns a Period array when no custom slots are used.
@test eltype(Dates.parse("1942-12-25T01:23:45", Dates.DateFormat("yyyy-mm-ddTHH:MM:SS", "english"))) == Dates.Period

# Common Parsing Patterns
#'1996-January-15'
dt = Dates.DateTime(1996,1,15)
f = "yy-mm-dd"
a = "96-01-15"
@test DateTime(a,f) + Dates.Year(1900) == dt
@test Dates.format(dt,f) == a
a1 = "96-1-15"
@test Dates.DateTime(a1,f) + Dates.Year(1900) == dt
@test Dates.format(dt,"yy-m-dd") == a1
a2 = "96-1-1"
@test Dates.DateTime(a2,f) + Dates.Year(1900) + Dates.Day(14) == dt
@test Dates.format(dt-Dates.Day(14),"yy-m-d") == a2
a3 = "1996-1-15"
@test Dates.DateTime(a3,f) == dt
@test Dates.format(dt,"yyyy-m-d") == a3
a4 = "1996-Jan-15"
@test_throws ArgumentError Dates.DateTime(a4,f) # Trying to use month name, but specified only "mm"

f = "yy/uuu/dd"
b = "96/Feb/15"
@test Dates.DateTime(b,f) + Dates.Year(1900) == dt + Dates.Month(1)
@test Dates.format(dt+Dates.Month(1),f) == b
b1 = "1996/Feb/15"
@test Dates.DateTime(b1,f) == dt + Dates.Month(1)
@test Dates.format(dt+Dates.Month(1),"yyyy/uuu/dd") == b1
b2 = "96/Feb/1"
@test Dates.DateTime(b2,f) + Dates.Year(1900) + Dates.Day(14) == dt + Dates.Month(1)
@test Dates.format(dt+Dates.Month(1)-Dates.Day(14),"yy/uuu/d") == b2
# Here we've specifed a text month name, but given a number
b3 = "96/2/15"
@test_throws KeyError Dates.DateTime(b3,f)

f = "yy:dd:mm"
c = "96:15:01"
@test Dates.DateTime(c,f) + Dates.Year(1900) == dt
@test Dates.format(dt,f) == c
c1 = "1996:15:01"
@test Dates.DateTime(c1,f) == dt
@test Dates.format(dt,"yyyy:dd:mm") == c1
c2 = "96:15:1"
@test Dates.DateTime(c2,f) + Dates.Year(1900) == dt
@test Dates.format(dt,"yy:dd:m") == c2
c3 = "96:1:01"
@test Dates.DateTime(c3,f) + Dates.Year(1900) + Dates.Day(14) == dt
@test Dates.format(dt-Dates.Day(14),"yy:m:dd") == c3
c4 = "1996:15:01 # random comment"
@test Dates.DateTime(c4,f) == dt

f = "yyyy,uuu,dd"
d = "1996,Jan,15"
@test Dates.DateTime(d,f) == dt
@test Dates.format(dt,f) == d
d1 = "96,Jan,15"
@test Dates.DateTime(d1,f) + Dates.Year(1900) == dt
@test Dates.format(dt,"yy,uuu,dd") == d1
d2 = "1996,Jan,1"
@test Dates.DateTime(d2,f) + Dates.Day(14) == dt
@test Dates.format(dt-Dates.Day(14),"yyyy,uuu,d") == d2
d3 = "1996,2,15"
@test_throws KeyError Dates.DateTime(d3,f)

f = "yyyy.U.dd"
e = "1996.January.15"
@test Dates.DateTime(e,f) == dt
@test Dates.format(dt,f) == e
e1 = "96.January.15"
@test Dates.DateTime(e1,f) + Dates.Year(1900) == dt
@test Dates.format(dt,"yy.U.dd") == e1

fo = "yyyy m dd"
f = "1996 1 15"
@test Dates.DateTime(f,fo) == dt
@test Dates.format(dt,fo) == f
f1 = "1996 01 15"
@test Dates.DateTime(f1,fo) == dt
@test Dates.format(dt,"yyyy mm dd") == f1
f2 = "1996 1 1"
@test Dates.DateTime(f2,fo) + Dates.Day(14) == dt
@test Dates.format(dt-Dates.Day(14),"yyyy m d") == f2

j = "1996-01-15"
f = "yyyy-mm-dd zzz"
@test Dates.DateTime(j,f) == dt
@test Dates.format(dt,f) == j*" zzz"
k = "1996-01-15 10:00:00"
f = "yyyy-mm-dd HH:MM:SS zzz"
@test Dates.DateTime(k,f) == dt + Dates.Hour(10)
@test Dates.format(dt+Dates.Hour(10),f) == k*" zzz"
l = "1996-01-15 10:10:10.25"
f = "yyyy-mm-dd HH:MM:SS.ss zzz"
@test Dates.DateTime(l,f) == dt + Dates.Hour(10) + Dates.Minute(10) + Dates.Second(10) + Dates.Millisecond(250)
@test Dates.format(dt+Dates.Hour(10)+Dates.Minute(10)+Dates.Second(10)+Dates.Millisecond(250),f) == l*" zzz"

r = "1/15/1996" # Excel
f = "m/dd/yyyy"
@test Dates.DateTime(r,f) == dt
@test Dates.format(dt,f) == r
s = "19960115"
f = "yyyymmdd"
@test Dates.DateTime(s,f) == dt
@test Dates.format(dt,f) == s
v = "1996-01-15 10:00:00"
f = "yyyy-mm-dd HH:MM:SS"
@test Dates.DateTime(v,f) == dt + Dates.Hour(10)
@test Dates.format(dt+Dates.Hour(10),f) == v
w = "1996-01-15T10:00:00"
f = "yyyy-mm-ddTHH:MM:SS zzz"
@test Dates.DateTime(w,f) == dt + Dates.Hour(10)
@test Dates.format(dt+Dates.Hour(10),f) == w*" zzz"

f = "yyyy/m"
y = "1996/1"
@test Dates.DateTime(y,f) == dt - Dates.Day(14)
@test Dates.format(dt,f) == y
y1 = "1996/1/15"
@test_throws ArgumentError Dates.DateTime(y1,f)
y2 = "96/1"
@test Dates.DateTime(y2,f) + Dates.Year(1900) == dt - Dates.Day(14)
@test Dates.format(dt,"yy/m") == y2

f = "yyyy"
z = "1996"
@test Dates.DateTime(z,f) == dt - Dates.Day(14)
@test Dates.format(dt,f) == z
z1 = "1996-3"
@test_throws ArgumentError Dates.DateTime(z1,f)
z2 = "1996-3-1"
@test_throws ArgumentError Dates.DateTime(z2,f)

aa = "1/5/1996"
f = "m/d/yyyy"
@test Dates.DateTime(aa,f) == dt - Dates.Day(10)
@test Dates.format(dt-Dates.Day(10),f) == aa
bb = "5/1/1996"
f = "d/m/yyyy"
@test Dates.DateTime(bb,f) == dt - Dates.Day(10)
@test Dates.format(dt-Dates.Day(10),f) == bb
cc = "01151996"
f = "mmddyyyy"
@test Dates.DateTime(cc,f) == dt
@test Dates.format(dt,f) == cc
dd = "15011996"
f = "ddmmyyyy"
@test Dates.DateTime(dd,f) == dt
@test Dates.format(dt,f) == dd
ee = "01199615"
f = "mmyyyydd"
@test Dates.DateTime(ee,f) == dt
@test Dates.format(dt,f) == ee
ff = "1996-15-Jan"
f = "yyyy-dd-uuu"
@test Dates.DateTime(ff,f) == dt
@test Dates.format(dt,f) == ff
gg = "Jan-1996-15"
f = "uuu-yyyy-dd"
@test Dates.DateTime(gg,f) == dt
@test Dates.format(dt,f) == gg
hh = "1996#1#15"
f = "yyyy#m#d"
@test Dates.DateTime(hh,f) == dt
@test Dates.format(dt,f) == hh

# test prefix.
s = "/1996/1/15"
f = "/yyyy/m/d"
@test Dates.DateTime(s,f) == dt
@test Dates.format(dt,f) == s
@test Dates.DateTime("1996/1/15",f) == dt

# from Jiahao
@test Dates.Date("2009年12月01日","yyyy年mm月dd日") == Dates.Date(2009,12,1)
@test Dates.format(Dates.Date(2009,12,1),"yyyy年mm月dd日") == "2009年12月01日"
@test Dates.Date("2009-12-01","yyyy-mm-dd") == Dates.Date(2009,12,1)

# French: from Milan
f = "dd/mm/yyyy"
f2 = "dd/mm/yy"
@test Dates.Date("28/05/2014",f) == Dates.Date(2014,5,28)
@test Dates.Date("28/05/14",f2) + Dates.Year(2000) == Dates.Date(2014,5,28)

const french = Dict("janv"=>1,"févr"=>2,"mars"=>3,"avril"=>4,"mai"=>5,"juin"=>6,"juil"=>7,"août"=>8,"sept"=>9,"oct"=>10,"nov"=>11,"déc"=>12)
Dates.MONTHTOVALUEABBR["french"] = french
Dates.VALUETOMONTHABBR["french"] = Dict(v=>k for (k,v) in french)

f = "dd uuuuu yyyy"
@test Dates.Date("28 mai 2014",f;locale="french") == Dates.Date(2014,5,28)
@test Dates.format(Dates.Date(2014,5,28),f;locale="french") == "28 mai 2014"
@test Dates.Date("28 févr 2014",f;locale="french") == Dates.Date(2014,2,28)
@test Dates.format(Dates.Date(2014,2,28),f;locale="french") == "28 févr 2014"
@test Dates.Date("28 août 2014",f;locale="french") == Dates.Date(2014,8,28)
@test Dates.format(Dates.Date(2014,8,28),f;locale="french") == "28 août 2014"
@test Dates.Date("28 avril 2014",f;locale="french") == Dates.Date(2014,4,28)
@test Dates.format(Dates.Date(2014,4,28),f;locale="french") == "28 avril 2014"

f = "dd u yyyy"
@test Dates.Date("28 avril 2014",f;locale="french") == Dates.Date(2014,4,28)
f = "dduuuuuyyyy"
@test Dates.Date("28avril2014",f;locale="french") == Dates.Date(2014,4,28)
@test_throws KeyError Dates.Date("28mai2014",f;locale="french")

# From Tony Fong
f = "dduuuyy"
@test Dates.Date("01Dec09",f) + Dates.Year(2000) == Dates.Date(2009,12,1)
@test Dates.format(Dates.Date(2009,12,1),f) == "01Dec09"
f = "dduuuyyyy"
@test Dates.Date("01Dec2009",f) == Dates.Date(2009,12,1)
@test Dates.format(Dates.Date(2009,12,1),f) == "01Dec2009"
f = "duy"
const globex = Dict("f"=>Dates.Jan,"g"=>Dates.Feb,"h"=>Dates.Mar,"j"=>Dates.Apr,"k"=>Dates.May,"m"=>Dates.Jun,
                    "n"=>Dates.Jul,"q"=>Dates.Aug,"u"=>Dates.Sep,"v"=>Dates.Oct,"x"=>Dates.Nov,"z"=>Dates.Dec)
Dates.MONTHTOVALUEABBR["globex"] = globex
Dates.VALUETOMONTHABBR["globex"] = Dict(v=>uppercase(k) for (k,v) in globex)
@test Dates.Date("1F4",f;locale="globex") + Dates.Year(2010) == Dates.Date(2014,1,1)
@test Dates.format(Dates.Date(2014,1,1),f;locale="globex") == "1F4"

# From Matt Bauman
f = "yyyy-mm-ddTHH:MM:SS"
@test Dates.DateTime("2014-05-28T16:46:04",f) == Dates.DateTime(2014,5,28,16,46,04)

# Try to break stuff

# Specified mm/dd, but date string has day/mm
@test_throws ArgumentError Dates.DateTime("18/05/2009","mm/dd/yyyy")
@test_throws ArgumentError Dates.DateTime("18/05/2009 16","mm/dd/yyyy hh")
# Used "mm" for months AND minutes
@test_throws ArgumentError Dates.DateTime("18/05/2009 16:12","mm/dd/yyyy hh:mm")
# Date string has different delimiters than format string
@test_throws ArgumentError Dates.DateTime("18:05:2009","mm/dd/yyyy")

f = "y m d"
@test Dates.Date("1 1 1",f) == Dates.Date(1)
@test Dates.Date("10000000000 1 1",f) == Dates.Date(10000000000)
@test_throws ArgumentError Dates.Date("1 13 1",f)
@test_throws ArgumentError Dates.Date("1 1 32",f)
@test_throws ArgumentError Dates.Date(" 1 1 32",f)
@test_throws ArgumentError Dates.Date("# 1 1 32",f)
# can't find 1st space delimiter,s o fails
@test_throws ArgumentError Dates.Date("1",f)
@test Dates.Date("1 2",f) == Dates.Date(1,2)
# can't find space delimiter (finds '/'), so fails
@test_throws ArgumentError Dates.Date("2000/1",f)

@test Dates.DateTime("20140529 120000","yyyymmdd HHMMSS") == Dates.DateTime(2014,5,29,12)

@test Dates.Date(string(Dates.Date(dt))) == Dates.Date(dt)
@test Dates.DateTime(string(dt)) == dt

# Vectorized
dr = ["2000-01-01","2000-01-02","2000-01-03","2000-01-04","2000-01-05"
     ,"2000-01-06","2000-01-07","2000-01-08","2000-01-09","2000-01-10"]
dr2 = [Dates.Date(2000):Dates.Date(2000,1,10);]
@test Dates.Date(dr) == dr2
@test Dates.Date(dr,"yyyy-mm-dd") == dr2
@test Dates.DateTime(dr) == Dates.DateTime(dr2)
@test Dates.DateTime(dr,"yyyy-mm-dd") == Dates.DateTime(dr2)

@test Dates.format(dr2) == dr
@test Dates.format(dr2,"yyyy-mm-dd") == dr

@test typeof(Dates.Date(dr)) == Array{Date,1}

# Issue 13
t = Dates.DateTime(1,1,1,14,51,0,118)
@test Dates.DateTime("[14:51:00.118]","[HH:MM:SS.sss]") == t
@test Dates.DateTime("14:51:00.118", "HH:MM:SS.sss") == t
@test Dates.DateTime("[14:51:00.118?", "[HH:MM:SS.sss?") == t
@test Dates.DateTime("?14:51:00.118?", "?HH:MM:SS.sss?") == t
@test Dates.DateTime("x14:51:00.118", "xHH:MM:SS.sss") == t
@test Dates.DateTime("14:51:00.118]", "HH:MM:SS.sss]") == t

# RFC1123Format
dt = Dates.DateTime(2014,8,23,17,22,15)
@test Dates.format(dt,Dates.RFC1123Format) == "Sat, 23 Aug 2014 17:22:15"
@test Dates.DateTime(Dates.format(dt,Dates.RFC1123Format),Dates.RFC1123Format) == dt
@test Dates.format(dt,"yyyy-mm-ddTHH:MM:SS E") == "2014-08-23T17:22:15 Saturday"
@test Dates.format(dt,"yyyy-mm-ddTHH:MM:SS e") == "2014-08-23T17:22:15 Sat"
@test Dates.format(dt,"yyyy-mm-dd E") == "2014-08-23 Saturday"
@test Dates.format(dt,"yyyy-mm-dd e") == "2014-08-23 Sat"
@test Dates.format(dt,"yyyy-e-mm-dd") == "2014-Sat-08-23"

@test Dates.format(Dates.DateTime(2014,1,2,0,0,0,999),Dates.RFC1123Format) == "Thu, 02 Jan 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,2,18,0,0,0,9),Dates.RFC1123Format) == "Tue, 18 Feb 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,3,8,0,0,0,9),Dates.RFC1123Format) == "Sat, 08 Mar 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,4,28,0,0,0,9),Dates.RFC1123Format) == "Mon, 28 Apr 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,5,10,0,0,0,9),Dates.RFC1123Format) == "Sat, 10 May 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,6,4,0,0,0,9),Dates.RFC1123Format) == "Wed, 04 Jun 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,7,13,0,0,0,9),Dates.RFC1123Format) == "Sun, 13 Jul 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,8,17,0,0,0,9),Dates.RFC1123Format) == "Sun, 17 Aug 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,9,20,0,0,0,9),Dates.RFC1123Format) == "Sat, 20 Sep 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,10,31,0,0,0,9),Dates.RFC1123Format) == "Fri, 31 Oct 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,11,2,0,0,0,9),Dates.RFC1123Format) == "Sun, 02 Nov 2014 00:00:00"
@test Dates.format(Dates.DateTime(2014,12,5,0,0,0,9),Dates.RFC1123Format) == "Fri, 05 Dec 2014 00:00:00"

# Issue 15195
let f = "YY"
    @test Dates.format(Dates.Date(1999), f) == "1999"
    @test Dates.format(Dates.Date(9), f) == "09"
    @test Dates.format(typemax(Dates.Date), f) == "252522163911149"
end

# Issue: https://github.com/quinnj/TimeZones.jl/issues/19
let
    ds = "2015-07-24T05:38:19.591Z"
    dt = Dates.DateTime(2015,7,24,5,38,19,591)

    format = "yyyy-mm-ddTHH:MM:SS.sssZ"
    escaped_format = "yyyy-mm-dd\\THH:MM:SS.sss\\Z"

    # Typically 'Z' isn't treated as a slot so it doesn't have to be escaped
    @test DateTime(ds, format) == dt
    @test DateTime(ds, escaped_format) == dt

    try
        # Make 'Z' into a slot
        Dates.SLOT_RULE['Z'] = Dates.TimeZone
        slotparse(slot::Dates.Slot{Dates.TimeZone},x,locale) = throw(ArgumentError("Invalid slot"))

        @test_throws ArgumentError DateTime(ds, format)
        @test DateTime(ds, escaped_format) == dt
    finally
        # Ideally we would be able to set SLOT_RULE['Z'] back to being undefined
        Dates.SLOT_RULE['Z'] = Void
    end

    # Ensure that the default behaviour has been restored
    @test DateTime(ds, format) == dt
    @test DateTime(ds, escaped_format) == dt
end
