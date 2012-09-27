

module jtm_consts

export TAI_min_year, TAI_top_year,
       TAI_SecondsPerDay

import Base.*

const TAI_SecondsPerDay = int64(86_400) # no leap seconds in TAI, SI seconds
const TAI_min_year      = -3999         # from notes for ymd_to_daynum!!
const TAI_top_year      =  4000         # from notes for ymd_to_daynum!!

end # module
