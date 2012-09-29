

module tm_abstract_types

export Temporal, TemporalMetric, MeasuredTime, ZonedTime

import Base.*

  abstract    Temporal          <: Signed
  abstract    TemporalMetric    <: Temporal
  abstract    MeasuredTime      <: TemporalMetric
  abstract    ZonedTime{E}      <: MeasuredTime

  #                     ^
  #  For each IANA primary timezone, there is a
  #    unique positive integer assigned to the
  #    E parameter of bitstype ZonedTime. These
  #    parameter values derive from the timezone
  #    enum values given in a relevant standards
  #    track document.  The mapping used here:
  #
  #          E = (iana_tz_enum + 16)
  #          iana_tz_enum = (E - 16)
  #

end # module
