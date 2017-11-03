# This file is a part of Julia. License is MIT: https://julialang.org/license

module QueryTests

using Test
using Dates

Jan = Dates.DateTime(2013, 1, 1) # Tuesday
Feb = Dates.DateTime(2013, 2, 2) # Saturday
Mar = Dates.DateTime(2013, 3, 3) # Sunday
Apr = Dates.DateTime(2013, 4, 4) # Thursday
May = Dates.DateTime(2013, 5, 5) # Sunday
Jun = Dates.DateTime(2013, 6, 7) # Friday
Jul = Dates.DateTime(2013, 7, 7) # Sunday
Aug = Dates.DateTime(2013, 8, 8) # Thursday
Sep = Dates.DateTime(2013, 9, 9) # Monday
Oct = Dates.DateTime(2013, 10, 10) # Thursday
Nov = Dates.DateTime(2013, 11, 11) # Monday
Dec = Dates.DateTime(2013, 12, 11) # Wednesday
monthnames = ["January", "February", "March", "April",
                "May", "June", "July", "August", "September",
                "October", "November", "December"]
daysofweek = [Dates.Tue, Dates.Sat, Dates.Sun, Dates.Thu, Dates.Sun, Dates.Fri,
              Dates.Sun, Dates.Thu, Dates.Mon, Dates.Thu, Dates.Mon, Dates.Wed]
dows = ["Tuesday", "Saturday", "Sunday", "Thursday", "Sunday", "Friday",
        "Sunday", "Thursday", "Monday", "Thursday", "Monday", "Wednesday"]
daysinmonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
@testset "Name functions" begin
    for (i, dt) in enumerate([Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec])
        @test Dates.month(dt) == i
        @test Dates.monthname(dt) == monthnames[i]
        @test Dates.monthname(i) == monthnames[i]
        @test Dates.monthabbr(dt) == monthnames[i][1:3]
        @test Dates.monthabbr(i) == monthnames[i][1:3]
        @test Dates.dayofweek(dt) == daysofweek[i]
        @test Dates.dayname(dt) == dows[i]
        @test Dates.dayname(Dates.dayofweek(dt)) == dows[i]
        @test Dates.dayabbr(dt) == dows[i][1:3]
        @test Dates.dayabbr(Dates.dayofweek(dt)) == dows[i][1:3]
        @test Dates.daysinmonth(dt) == daysinmonth[i]
    end
end
@testset "Customizing locale" begin
    Dates.LOCALES["french"] = Dates.DateLocale(
        ["janvier", "février", "mars", "avril", "mai", "juin",
         "juillet", "août", "septembre", "octobre", "novembre", "décembre"],
        ["janv", "févr", "mars", "avril", "mai", "juin",
         "juil", "août", "sept", "oct", "nov", "déc"],
        ["lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"],
        [""],
    )
    @test Dates.dayname(Nov; locale="french") == "lundi"
    @test Dates.dayname(Jan; locale="french") == "mardi"
    @test Dates.dayname(Dec; locale="french") == "mercredi"
    @test Dates.dayname(Apr; locale="french") == "jeudi"
    @test Dates.dayname(Jun; locale="french") == "vendredi"
    @test Dates.dayname(Feb; locale="french") == "samedi"
    @test Dates.dayname(May; locale="french") == "dimanche"

    @test Dates.monthname(Jan; locale="french") == "janvier"
    @test Dates.monthname(Feb; locale="french") == "février"
    @test Dates.monthname(Mar; locale="french") == "mars"
    @test Dates.monthname(Apr; locale="french") == "avril"
    @test Dates.monthname(May; locale="french") == "mai"
    @test Dates.monthname(Jun; locale="french") == "juin"
    @test Dates.monthname(Jul; locale="french") == "juillet"
    @test Dates.monthname(Aug; locale="french") == "août"
    @test Dates.monthname(Sep; locale="french") == "septembre"
    @test Dates.monthname(Oct; locale="french") == "octobre"
    @test Dates.monthname(Nov; locale="french") == "novembre"
    @test Dates.monthname(Dec; locale="french") == "décembre"
end
@testset "leap years" begin
    @test Dates.isleapyear(Dates.DateTime(1900)) == false
    @test Dates.isleapyear(Dates.DateTime(2000)) == true
    @test Dates.isleapyear(Dates.DateTime(2004)) == true
    @test Dates.isleapyear(Dates.DateTime(2008)) == true
    @test Dates.isleapyear(Dates.DateTime(0)) == true
    @test Dates.isleapyear(Dates.DateTime(1)) == false
    @test Dates.isleapyear(Dates.DateTime(-1)) == false
    @test Dates.isleapyear(Dates.DateTime(4)) == true
    @test Dates.isleapyear(Dates.DateTime(-4)) == true

    @test Dates.daysinyear(2000) == 366
    @test Dates.daysinyear(2001) == 365
    @test Dates.daysinyear(2000) == 366
    @test Dates.daysinyear(2001) == 365

    @test Dates.daysinyear(Dates.Date(2000)) == 366
    @test Dates.daysinyear(Dates.Date(2001)) == 365
    @test Dates.daysinyear(Dates.DateTime(2000)) == 366
    @test Dates.daysinyear(Dates.DateTime(2001)) == 365
end
@testset "dayofweek" begin
    # Days of week from Monday = 1 to Sunday = 7
    @test Dates.dayofweek(Dates.DateTime(2013, 12, 22)) == 7
    @test Dates.dayofweek(Dates.DateTime(2013, 12, 23)) == 1
    @test Dates.dayofweek(Dates.DateTime(2013, 12, 24)) == 2
    @test Dates.dayofweek(Dates.DateTime(2013, 12, 25)) == 3
    @test Dates.dayofweek(Dates.DateTime(2013, 12, 26)) == 4
    @test Dates.dayofweek(Dates.DateTime(2013, 12, 27)) == 5
    @test Dates.dayofweek(Dates.DateTime(2013, 12, 28)) == 6
    @test Dates.dayofweek(Dates.DateTime(2013, 12, 29)) == 7
end
@testset "daysofweekinmonth" begin
    # There are 5 Sundays in December, 2013
    @test Dates.daysofweekinmonth(Dates.DateTime(2013, 12, 1)) == 5
    # There are 4 Sundays in November, 2013
    @test Dates.daysofweekinmonth(Dates.DateTime(2013, 11, 24)) == 4

    @test Dates.dayofweekofmonth(Dates.DateTime(2013, 12, 1)) == 1
    @test Dates.dayofweekofmonth(Dates.DateTime(2013, 12, 8)) == 2
    @test Dates.dayofweekofmonth(Dates.DateTime(2013, 12, 15)) == 3
    @test Dates.dayofweekofmonth(Dates.DateTime(2013, 12, 22)) == 4
    @test Dates.dayofweekofmonth(Dates.DateTime(2013, 12, 29)) == 5
end
@testset "dayofyear" begin
    @test Dates.dayofyear(2000, 1, 1) == 1
    @test Dates.dayofyear(2004, 1, 1) == 1
    @test Dates.dayofyear(20013, 1, 1) == 1
    # Leap year
    @test Dates.dayofyear(2000, 12, 31) == 366
    # Non-leap year
    @test Dates.dayofyear(2001, 12, 31) == 365

    @test Dates.dayofyear(Dates.DateTime(2000, 1, 1)) == 1
    @test Dates.dayofyear(Dates.DateTime(2004, 1, 1)) == 1
    @test Dates.dayofyear(Dates.DateTime(20013, 1, 1)) == 1
    # Leap year
    @test Dates.dayofyear(Dates.DateTime(2000, 12, 31)) == 366
    # Non-leap year
    @test Dates.dayofyear(Dates.DateTime(2001, 12, 31)) == 365
    # Test every day of a year
    dt = Dates.DateTime(2000, 1, 1)
    for i = 1:366
        @test Dates.dayofyear(dt) == i
        dt += Dates.Day(1)
    end
    dt = Dates.DateTime(2001, 1, 1)
    for i = 1:365
        @test Dates.dayofyear(dt) == i
        dt += Dates.Day(1)
    end
end
@testset "quarterofyear" begin
    @test Dates.quarterofyear(Dates.Date(2000, 1, 1))  == 1
    @test Dates.quarterofyear(Dates.Date(2000, 1, 31))  == 1
    @test Dates.quarterofyear(Dates.Date(2000, 2, 1))  == 1
    @test Dates.quarterofyear(Dates.Date(2000, 2, 29))  == 1
    @test Dates.quarterofyear(Dates.Date(2000, 3, 1))  == 1
    @test Dates.quarterofyear(Dates.Date(2000, 3, 31))  == 1
    @test Dates.quarterofyear(Dates.Date(2000, 4, 1)) == 2
    @test Dates.quarterofyear(Dates.Date(2000, 4, 30)) == 2
    @test Dates.quarterofyear(Dates.Date(2000, 5, 1)) == 2
    @test Dates.quarterofyear(Dates.Date(2000, 5, 31)) == 2
    @test Dates.quarterofyear(Dates.Date(2000, 6, 1)) == 2
    @test Dates.quarterofyear(Dates.Date(2000, 6, 30)) == 2
    @test Dates.quarterofyear(Dates.Date(2000, 7, 1)) == 3
    @test Dates.quarterofyear(Dates.Date(2000, 7, 31)) == 3
    @test Dates.quarterofyear(Dates.Date(2000, 8, 1)) == 3
    @test Dates.quarterofyear(Dates.Date(2000, 8, 31)) == 3
    @test Dates.quarterofyear(Dates.Date(2000, 9, 1)) == 3
    @test Dates.quarterofyear(Dates.Date(2000, 9, 30)) == 3
    @test Dates.quarterofyear(Dates.Date(2000, 10, 1)) == 4
    @test Dates.quarterofyear(Dates.Date(2000, 10, 31)) == 4
    @test Dates.quarterofyear(Dates.Date(2000, 11, 1)) == 4
    @test Dates.quarterofyear(Dates.Date(2000, 11, 30)) == 4
    @test Dates.quarterofyear(Dates.Date(2000, 12, 1)) == 4
    @test Dates.quarterofyear(Dates.Date(2000, 12, 31)) == 4

    @test Dates.quarterofyear(Dates.DateTime(2000, 1, 1))  == 1
    @test Dates.quarterofyear(Dates.DateTime(2000, 1, 31))  == 1
    @test Dates.quarterofyear(Dates.DateTime(2000, 2, 1))  == 1
    @test Dates.quarterofyear(Dates.DateTime(2000, 2, 29))  == 1
    @test Dates.quarterofyear(Dates.DateTime(2000, 3, 1))  == 1
    @test Dates.quarterofyear(Dates.DateTime(2000, 3, 31))  == 1
    @test Dates.quarterofyear(Dates.DateTime(2000, 4, 1)) == 2
    @test Dates.quarterofyear(Dates.DateTime(2000, 4, 30)) == 2
    @test Dates.quarterofyear(Dates.DateTime(2000, 5, 1)) == 2
    @test Dates.quarterofyear(Dates.DateTime(2000, 5, 31)) == 2
    @test Dates.quarterofyear(Dates.DateTime(2000, 6, 1)) == 2
    @test Dates.quarterofyear(Dates.DateTime(2000, 6, 30)) == 2
    @test Dates.quarterofyear(Dates.DateTime(2000, 7, 1)) == 3
    @test Dates.quarterofyear(Dates.DateTime(2000, 7, 31)) == 3
    @test Dates.quarterofyear(Dates.DateTime(2000, 8, 1)) == 3
    @test Dates.quarterofyear(Dates.DateTime(2000, 8, 31)) == 3
    @test Dates.quarterofyear(Dates.DateTime(2000, 9, 1)) == 3
    @test Dates.quarterofyear(Dates.DateTime(2000, 9, 30)) == 3
    @test Dates.quarterofyear(Dates.DateTime(2000, 10, 1)) == 4
    @test Dates.quarterofyear(Dates.DateTime(2000, 10, 31)) == 4
    @test Dates.quarterofyear(Dates.DateTime(2000, 11, 1)) == 4
    @test Dates.quarterofyear(Dates.DateTime(2000, 11, 30)) == 4
    @test Dates.quarterofyear(Dates.DateTime(2000, 12, 1)) == 4
    @test Dates.quarterofyear(Dates.DateTime(2000, 12, 31)) == 4
end
@testset "dayofquarter" begin
    @test Dates.dayofquarter(Dates.Date(2014, 1, 1)) == 1
    @test Dates.dayofquarter(Dates.Date(2014, 4, 1)) == 1
    @test Dates.dayofquarter(Dates.Date(2014, 7, 1)) == 1
    @test Dates.dayofquarter(Dates.Date(2014, 10, 1)) == 1
    @test Dates.dayofquarter(Dates.Date(2014, 3, 31)) == 90
    @test Dates.dayofquarter(Dates.Date(2014, 6, 30)) == 91
    @test Dates.dayofquarter(Dates.Date(2014, 9, 30)) == 92
    @test Dates.dayofquarter(Dates.Date(2014, 12, 31)) == 92
    @test Dates.dayofquarter(Dates.DateTime(2014, 1, 1)) == 1
    @test Dates.dayofquarter(Dates.DateTime(2014, 4, 1)) == 1
    @test Dates.dayofquarter(Dates.DateTime(2014, 7, 1)) == 1
    @test Dates.dayofquarter(Dates.DateTime(2014, 10, 1)) == 1
    @test Dates.dayofquarter(Dates.DateTime(2014, 3, 31)) == 90
    @test Dates.dayofquarter(Dates.DateTime(2014, 6, 30)) == 91
    @test Dates.dayofquarter(Dates.DateTime(2014, 9, 30)) == 92
    @test Dates.dayofquarter(Dates.DateTime(2014, 12, 31)) == 92
end

end
