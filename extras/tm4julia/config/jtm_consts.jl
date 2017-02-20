

module jtm_consts

export TAI_min_year, TAI_top_year,
       TAI_SecondsPerDay, TAI_SecondsPerHour, TAI_SecondsPerMinute

import Base.*

# no leap seconds in TAI, SI seconds
const TAI_SecondsPerDay    = int64(86_400)
const TAI_SecondsPerHour   = int64(3_600)
const TAI_SecondsPerMinute = int64(60)

const TAI_min_year  = -3999         # from notes for ymd_to_daynum!!
const TAI_top_year  =  4000         # from notes for ymd_to_daynum!!

end # module
