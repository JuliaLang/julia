
#require("$(JULIA_JTM_DIR)/tm_types/tm_abstract_types.jl")
require("$(JULIA_JTM_DIR)/jas/NotOK.jl")
export NotOK


module tz_composite_types

export TimezoneBasic     , TimezoneVects,
       TimezoneBasicEmpty, TimezoneVectsEmpty

import Base.*

import Main.NotOK


type TimezoneBasic

  num     ::Int64

  TimezoneBasic() = new(NotOK(Int64))
end

#  utc2std ::Int64
#  utc2dst ::Int64
#  name    ::ASCIIString
#  stdabbr ::ASCIIString
#  dstabbr ::ASCIIString

#  TimezoneBasic() = new(NotOK(Int64), NotOK(Int64), NotOK(Int64), "", "", "");

  # TimezoneBasic(num::Int64, utc2std::Int64, utc2dst::Int64,
  #               name::ASCIIString, stdabbr::ASCIIString, dstabbr::ASCIIString) =
  #     new(num, utc2std, utc2dst, name, stdabbr, dstabbr);

  # TimezoneBasic(num::Int32, utc2std::Int32, utc2dst::Int32,
  #               name::ASCIIString, stdabbr::ASCIIString, dstabbr::ASCIIString) =
  #     new(int64(num), int64(utc2std), int64(utc2dst), name, stdabbr, dstabbr);

#end

const TimezoneBasicEmpty = TimezoneBasic()

end # module
