

require("$(JULIA_JTM_DIR)/jas/NotOK.jl")
require("$(JULIA_JTM_DIR)/tm_types/tm_abstract_types.jl")


module tz_composite_types

export TimezoneBasic     , TimezoneVects,
       TimezoneBasicEmpty, TimezoneVectsEmpty

import Base.*
import NotOK.NotOK
import tm_abstract_types.ZonedTime


type TimezoneBasic

  num     ::Int64
  utc2std ::Int64
  utc2dst ::Int64
  name    ::ASCIIString
  stdabbr ::ASCIIString
  dstabbr ::ASCIIString

  TimezoneBasic() = new(NotOK(Int64), NotOK(Int64), NotOK(Int64), "", "", "");

  TimezoneBasic(num::Int64, utc2std::Int64, utc2dst::Int64,
                name::ASCIIString, stdabbr::ASCIIString, dstabbr::ASCIIString) =
      new(num, utc2std, utc2dst, name, stdabbr, dstabbr);

  TimezoneBasic(num::Int32, utc2std::Int32, utc2dst::Int32,
                name::ASCIIString, stdabbr::ASCIIString, dstabbr::ASCIIString) =
      new(int64(num), int64(utc2std), int64(utc2dst), name, stdabbr, dstabbr);

end

const TimezoneBasicEmpty = tz_composite_types.TimezoneBasic()


type TimezoneVects

  utcsecs ::Vector{Int64}
  utc2lcl ::Vector{Int64}
  isitdst ::Vector{Int64}

  TimezoneVects() = new([NotOK(Int64)], [NotOK(Int64)], [NotOK(Int64)])

  TimezoneVects(utcsecs::Vector{Int64}, utc2lcl::Vector{Int64}, isitdst::Vector{Int64}) =
      new(utcsecs, utc2lcl, isitdst)

  TimezoneVects(utcsecs::Vector{Int32}, utc2lcl::Vector{Int32}, isitdst::Vector{Int32}) =
      new(int64(utcsecs), int64(utc2lcl), int64(isitdst))
end

const TimezoneVectsEmpty = TimezoneVects()


end # module
