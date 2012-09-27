#include("$(JULIA_JTM_DIR)/bitwise/BitFields.jl")

require("$(JULIA_JTM_DIR)/config/jtm_consts.jl")
require("$(JULIA_JTM_DIR)/config/jtm_iuitypes.jl")
require("$(JULIA_JTM_DIR)/jas/jas_arith.jl")

module ymdhms

export ymd_to_secnum  , ymdsec_to_secnum  , ymdhms_to_secnum  ,
       secnum_to_ymd  , secnum_to_ymdsec  , secnum_to_ymdhms  ,
       ymd_to_secnum!!, ymdsec_to_secnum!!, ymdhms_to_secnum!!,
       secnum_to_ymd!!, secnum_to_ymdsec!!, secnum_to_ymdhms!!


import Base.*

import jas_arith.jas_quorem

import jtm_consts.TAI_SecondsPerDay # int64(86_400)
import jtm_consts.TAI_min_year      # -3999 from notes for ymd_to_daynum!!
import jtm_consts.TAI_top_year      #  4000 from notes for ymd_to_daynum!!

import jtm_iuitypes.IUI3264


# import YMD.ymd_check
# import HMS.hms_check
# import YMDSEC.ymdsec_check
# import YMDHMS.ymdhms_check

# import HMS.hms_to_sec!!
# import HMS.sec_to_hms!!
# import HMS.hms_to_sec
# import HMS.sec_to_hms

#import BitFields.bitmaskof
#import BitFields.bitfilterof


# Dates in a proleptic Gregorian calendar using year zero.
#
#     daynum_to_ymd( ymd_to_daynum(y,m,d) ) == y,m,d
#
#     exaustively verified for all accepted dates
#     in the years: [ -3999.. -1, 0,  +1 ..3999 ]


# convert date into a nonnegative daynum
#   dates change at midnight (a fact not used in this function)
#   months: Jan=1,..,Dec=12, days: 1..31
#   supports years (-3999..,-2,-1,0,1,2,..3999)
#   aligns with the Gregorian 400 year cycle (146907 days)
#      multiples of 400 years have daynums multiples of 146097
#      this is true for positive and for negative years
#   requires year numbered using the non-historical Year 0
#   uses a non-historical proleptic Gregorian calendar
#     historically, year 8 is the first year properly a leap year
#
#   Notes
#   to obtain the corresponding Int64 daynum subtract 1607067
#
#   (y,m,d)       --> daynum, Int64 correspondant
#
#   ( 4400, 1, 1) --> 3214134  [and that is the soft clopen upper bound]
#   ( 4399,12,31) --> 3214133  [and that is the soft realized upper bound]
#   ( 1970, 1, 1) --> 2326595  [UNIX date zero]
#   (    1, 1, 1) --> 1607433
#   (    0, 1, 1) --> 1607067  [0 + 4400 Gregorian years]
#   (   -1, 1, 1) --> 1606702
#   (-4399, 1, 1) -->     366  [and that is the soft lower bound]
#   all intervals are clopen so do not use this as a spec:
#         (-4400, 3, 1) -->      60  [and that is the hard lower bound]
#
#   daynum    requires 22 bits (for this date range)
#   hour,min,sec requires 17 bits (for one day)
#                         39 bits (for date and time-of-day)
#
#   timezone num requires  9 bits
#                         48 bits (for date, time-of-day, timezone)
#
#   millisecs    requires 10 bits
#   or year      requires 13 bits


# _ymd_to_daynum original form
# function daynum_unsafe(y,m,d)
#    # this is div(((152*m)-2),5) with entries 13,14 folded into 1,2
#    monthdays = [ 397,428,91,122,152,183,213,244,275,305,336,366 ]
#    if (m<3)
#        y += 4399 # y-= 1; y+=4400
#    else
#        y += 4400
#    end
#    int64(d + monthdays[m]+(365*y)+udiv4(y)-udiv100(y)+udiv400(y)-32)
# end


function ymd_to_daynum!!(y::Int, m::Int, d::Int)
    # if (m<3)  y += 4399  else  y += 4400  end;
    y += 4400
    y -= (m >= 3) ? 0 : 1
    d += (365*y)
    # assert(y >= 0), tally leap years through year y
    # (+udiv4(y)-udiv100(y)+udiv400(y))
    y100  = (y * 42949673) >> 32
    d    +=  (y>>2) - y100 + (y100 >> 2)
    #  d +=  ( div(((152*m)-2),5) with entries 13,14 folded into 1,2) - 32
    #  monthdays = [ 365, 396, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ]
    d += [ 365, 396, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ][m]
    int64(d)
end

ymd_to_secnum!!(yr::Int, mo::Int, dy::Int) =
    ymd_to_daynum!!(yr,mo,dy) * TAI_SecondsPerDay

ymdsec_to_secnum!!(yr::Int, mo::Int, dy::Int, sc::Int) =
    ymd_to_secnum!!(yr,mo,dy) + sc

ymdhms_to_secnum!!(yr::Int, mo::Int, dy::Int, hr::Int, mi::Int, sc::Int) =
    ymdsec_to_secnum!!(yr,mo,dy) + hms_to_sec!!(hr,mi,sc)




const min_daynum = ymd_to_daynum!!( TAI_min_year, 1, 1)
const top_daynum = ymd_to_daynum!!( TAI_top_year, 1, 1)


isLeapYearGregorian(y::Integer) =
    ((y % 4) == 0) ?
    (((y % 100) != 0) | ((y % 400) == 0)) :
    false

# >> CAUTION <<
#
# ymd_check is correct for use with ymd_to_daynum!!
# with all of the calendric specifics it realizes
# (e.g. proleptic  Gregorian, with a year 0).
# Most other daynumber functions will require
# a different ymd_check function.
#
ymd_check_tf(yr::IUI3264, mo::IUI3264, dy::IUI3264) =
    (
        (TAI_min_year <= yr < TAI_top_year) &
        (1 <= mo < 13) &
      (
           (1 <= dy <= [31,28,31,30,31,30,31,31,30,31,30,31][mo])
        ||
           ((dy == 29) & (mo == 2) & isLeapYearGregorian(yr))
      )
    )

TAI_hms_check_tf(hr::IUI3264, mi::IUI3264, sc::IUI3264) =
    (
        (0 <= hr < 24) &
        (0 <= mi < 60) &
        (0 <= sc < 60)
    )

TAI_sec_check_tf(sc::IUI3264) = (0 <= sc <= TAI_SecondsPerDay)

TAI_ymdsec_check_tf(yr::IUI3264, mo::IUI3264, dy::IUI3264, sc::IUI3264) =
    ymd_check_tf(yr,mo,dy) & TAI_sec_check_tf(sc)

TAI_ymdhms_check_tf(yr::IUI3264, mo::IUI3264, dy::IUI3264, hr::IUI3264, mi::IUI3264, sc::IUI3264) =
    ymd_check_tf(yr,mo,dy) & TAI_hms_check_tf(hr,mi,sc)

function ymd_check(yr::IUI3264, mo::IUI3264, dy::IUI3264)
  if ( ymd_check_tf(yr,mo,dy) )
     (yr,mo,dy)
  else
     error("Invalid (year,month,day): ($(yr)-$(mo)-$(dy)).")
  end
end

function TAI_sec_check(sc::IUI3264)
  if ( TAI_sec_check_tf(sc) )
     sc
  else
     error("Invalid (seconds): ($(sc)).")
  end
end

function TAI_ymdsec_check(yr::IUI3264, mo::IUI3264, dy::IUI3264, sc::IUI3264)
  if ( TAI_ymdsec_check_tf(yr,mo,dy,sc) )
     (yr,mo,dy,sc)
  else
     error("Invalid (year,month,day,seconds): ($(yr)-$(mo)-$(dy)   gd$(sc)).")
  end
end

function TAI_ymdhms_check(yr::IUI3264, mo::IUI3264, dy::IUI3264, sc::IUI3264)
  if ( TAI_ymdhms_check_tf(yr,mo,dy,sc) )
     (yr,mo,dy,hr,mi,sc)
  else
     error("Invalid (year,month,day,hour,minute,second): ($(yr)-$(mo)-$(dy) $(hr):$(mi):$(sc)).")
  end
end


ymd_to_secnum(yr::IUI3264, mo::IUI3264, dy::IUI3264) =
    ymd_to_secnum!!( ymd_check(yr,mo,dy)... )

ymdsec_to_secnum(yr::IUI3264, mo::IUI3264, dy::IUI3264, sc::IUI3264) =
    ymdsec_to_secnum!!( TAI_ymdsec_check(yr,mo,dy,sc)... )

ymdhms_to_secnum(yr::IUI3264, mo::IUI3264, dy::IUI3264, hr::IUI3264, mi::IUI3264, sc::IUI3264) =
    ymdhms_to_secnum!!( TAI_ymdhms_check(yr,mo,dy,hr,mi,sc)... )



# this is a date-shifted version of the reference code
# the only change is the addtion of the first line
#
# ref: # http://www.merlyn.demon.co.uk/daycount.htm
# credit appearing on reference page:
# Adapted from Henry Fliegel and Thomas van Flandern, ACM Vol 11, 1968 #
# (by mail from HJW 20051228)


function daynum_to_ymd!!(J::Int64)
    # J -= 2286008  # untranslate Julian Day Number (1607067 -32 +678973)
    # J = J + 2400001 + 68569
    J += 182562
    # K = div(4*J, 146097)
    K = div( (J<<2), 146097 )
    #J = J - div((146097*K + 3), 4)
    J -= ((146097*K + 3) >> 2)
    Y = div(4000*(J+1), 1461001)
    #J = J - div(1461*Y, 4) + 31
    J -= (((1461*Y) >> 2) - 31)
    M = div(80*J, 2447)
    D = J - div(2447*M,80)
    J = div(M,11)
    #M = M + 2 - 12*J
    M += (2 - 12*J)
    #Y = 100*(K-49) + Y + J
    Y += 100*(K-49) + J
    (Y, M, D)
end


secnum_to_ymd!!(secnum::Int64) = daynum_to_ymd!!( div(secnum, TAI_SecondsPerDay) )

function secnum_to_ymdsec!!(secnum::Int64)
  daynum, secnum = jas_quorem(secnum, TAI_SecondsPerDay)
  yr,mo,dy = daynum_to_ymd!!(daynum)
  (yr, mo, dy, secnum)
end

function secnum_to_ymdhms!!(secnum::Int64)
  daynum, secnum = jas_quorem(secnum, TAI_SecondsPerDay)
  yr,mo,dy = daynum_to_ymd!!(daynum)
  hr,mi,sc = sec_to_hms!!(secnum)
  (yr, mo, dy, hr, mi, sc)
end


const min_secnum = ymd_to_secnum!!( TAI_min_year, 1, 1)
const top_secnum = ymd_to_secnum!!( TAI_top_year, 1, 1)

secnum_check(secnum::Int64) = (min_secnum <= secnum < top_secnum) ?
                              secnum                              :
                              error("SecNumber domain exceeded ($secnum).")


secnum_to_ymd   (secnum::Int64) = secnum_to_ymd!!   ( secnum_check(secnum) )
secnum_to_ymdsec(secnum::Int64) = secnum_to_ymdsec!!( secnum_check(secnum) )
secnum_to_ymdhms(secnum::Int64) = secnum_to_ymdhms!!( secnum_check(secnum) )


end # module

