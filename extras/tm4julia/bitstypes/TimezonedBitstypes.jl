# Jeffrey A. Sarnoff 2012-Sep-18 at 10:29 EDT in New York City



  IUI8     = Union(Uint8  , Int8  )
  IUI16    = Union(Uint16 , Int16 )
  IUI32    = Union(Uint32 , Int32 )
  IUI64    = Union(Uint64 , Int64 )
  IUI128   = Union(Uint128, Int128)
  IUI3264  = Union(Uint64, Int64, Uint32 , Int32 )
  IUI64128 = Union(Uint64, Int64, Uint128, Int128)

  typealias IUI
      if (WORD_SIZE ==   8) IUI8
  elseif (WORD_SIZE ==  16) IUI16
  elseif (WORD_SIZE ==  32) IUI32
  elseif (WORD_SIZE ==  64) IUI64
  elseif (WORD_SIZE == 128) IUI128
  else   error("Unexpected WORD_SIZE ($WORD_SIZE).")
  end



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


  #    workhorse extroversive and introversive types
  #  -------------------------------------------------
  #
  #
  #    UTC (Universal Coordinated Time)
  #    This is wall clock time made more precise and
  #    timezone invariant.  It is how user datetimes
  #    are understood, after unshifting the timezone.
  #
  #    While visiting the Royal Greenwich Observatory,
  #    when asked "Do you know the time " a very good
  #    answer would be "Yes. Here, the time is UTC."
  #
  #
  #    TAI (International Atomic Time)
  #
  #    This is time as an accumulation of seconds,
  #    not as a succession of sunrises.  It is the
  #    time of record of choice when records matter.
  #    It is how Julia understands datetimes before
  #    reshifting the timezone.
  #
  #    Would you rather step than leap seconds
  #    Are you feeling slightly ahead of your time
  #    Relax, it's just the Earth being wobbly, and
  #    Professor Einstein's imagination being right.
  #    Have a nice cup of tea, wear a watch with no
  #    second hand, curl up with TAI ... and relax.
  #
  #
  #    Tic-Toc
  #
  #    Everything that happens happens in the Tocs.
  #    Tics separate tocs, and take no time at all.
  #
  # --------------------------------------------------


  #   <T>_tocks and <T>_zoned carry datetime information the same way,
  #   giving the moment of an occurance as a subsecond accumulation.
  #   <T>_zoned  adds a type parameter giving the locale of occurance
  #   as a geopolitical region; the param value indicates the timezone.
  #
  #   UTC_tocks is UTC_zoned without timezone parameterized typing
  #   UTC_zoned  is UTC_tocks with   timezone parameterized typing
  #   TAI_tocks is TAI_zoned without timezone parameterized typing
  #   TAI_zoned  is TAI_tocks with   timezone parameterized typing
  #
  #   <T>_tocks and <T>_zoned carry both <T>datetime information
  #   and for observed occurances, the shift from UTC to LocalTime.
  #
  #       They are paired because one cannot add timezones
  #       (what is Chicago + London), while it is easy to
  #       add offsets from UTC to LocalTime.  Arithmetic
  #       is limited to <T>_tocks types.  This increases
  #       reliability and eliminates potential problems.
  #
  #   <T>_zoned values may persist and are useful to transfer.
  #   <T>_tocks values are ephemeral, coming into existance
  #   in support of calculating a temporal relativity and then
  #   providing the information for <T>_zoned (or timespan)
  #   object instantiation.



  bitstype 64 TAI64_tocks     <: MeasuredTime      # by the Greenwich
  bitstype 64 UTC64_tocks     <: MeasuredTime      # Royal Observatory

  bitstype 64 TAI64_zoned{E}  <: ZonedTime{E}      # E is value mapped
  bitstype 64 UTC64_zoned{E}  <: ZonedTime{E}      # enum_IANA_TZ + 16


  const _TZ_index_bits       = 10                    # max index <= 511 (+1bit)
  const _TZ64_index_mask_    = uint64(0x03FF)        # (1 << 10) - 1
  const _TZ64_index_shift_   = (64 - _TZ_index_bits) # for use with 64bit types
  const _TZ64_index_filter_  = ~(_TZ64_index_mask_ << _TZ64_index_shift_)

  TZ64_index_shiftup!(tz_index::IUI64)  = (tz_index << _TZ64_index_shift_)
  TZ64_index_shiftup (tz_index::IUI3264)= TZ_index_shiftup!(_TZ64_index_mask_ & tz_index)
  TZ64_index_shiftdn (zonedtime::IUI64) = ((zonedtime >>> _TZ64_index_shift_) & _TZ64_index_mask_)


  convert(::Type{Uint64}, x::TAI64_tocks) = reinterpret(Uint64, x)
  convert(::Type{ Int64}, x::TAI64_tocks) = reinterpret( Int64, x)
  convert(::Type{Uint64}, x::UTC64_tocks) = reinterpret(Uint64, x)
  convert(::Type{ Int64}, x::UTC64_tocks) = reinterpret( Int64, x)

  convert(::Type{TAI64_tocks}, x::Uint64) = reinterpret(TAI64_tocks, x)
  convert(::Type{TAI64_tocks}, x::Int64 ) = reinterpret(TAI64_tocks, x)
  convert(::Type{UTC64_tocks}, x::Uint64) = reinterpret(UTC64_tocks, x)
  convert(::Type{UTC64_tocks}, x::Int64 ) = reinterpret(UTC64_tocks, x)

  convert(::Type{Uint64}, x::TAI64_zoned) = reinterpret(Uint64, x) | (TZ64_index_shiftup!(type_param1(x)))
  convert(::Type{ Int64}, x::TAI64_zoned) = reinterpret( Int64, x) | (TZ64_index_shiftup!(type_param1(x)))
  convert(::Type{Uint64}, x::UTC64_zoned) = reinterpret(Uint64, x) | (TZ64_index_shiftup!(type_param1(x)))
  convert(::Type{ Int64}, x::UTC64_zoned) = reinterpret( Int64, x) | (TZ64_index_shiftup!(type_param1(x)))

  convert{E}(::Type{TAI64_zoned{E}}, x::Uint64) = reinterpret(TAI64_zoned{E}, (x | TZ64_index_shiftup!(E)))
  convert{E}(::Type{TAI64_zoned{E}}, x::Int64 ) = reinterpret(TAI64_zoned{E}, (x | TZ64_index_shiftup!(E)))
  convert{E}(::Type{UTC64_zoned{E}}, x::Uint64) = reinterpret(UTC64_zoned{E}, (x | TZ64_index_shiftup!(E)))
  convert{E}(::Type{UTC64_zoned{E}}, x::Int64)  = reinterpret(UTC64_zoned{E}, (x | TZ64_index_shiftup!(E)))


  # instatiators

  TAI_tocks(x::IUI64)             = convert(TAI64_tocks, x)
  UTC_tocks(x::IUI64)             = convert(UTC64_tocks, x)

  TAI_zoned(E::IUI,     x::IUI64) = convert(TAI64_zoned{E}, x)
  TAI_zoned(E::Integer, x::IUI64) = TAI_zoned(x, convert(Int,E))
  UTC_zoned(E::IUI,     x::IUI64) = convert(UTC64_zoned{E}, x)
  UTC_zoned(E::Integer, x::IUI64) = UTC_zoned(x, convert(Int,E))


  # intra-instantiators

  zonedtime_from_tocks(E::Int, t::TAI64_tocks) = TAI_zoned(E,reinterpret(Int64,t))
  zonedtime_from_tocks(E::Int, t::UTC64_tocks) = UTC_zoned(E,reinterpret(Int64,t))

  tocks_from_zonedtime{E}(d::TAI64_zoned{E}) =
     reinterpret(Int64,(reinterpret(Uint64,d) & _TZ64_index_filter_))
  tocks_from_zonedtime{E}(d::UTC64_zoned{E}) =
     reinterpret(Int64,(reinterpret(Uint64,d) & _TZ64_index_filter_))

  zone_from_zonedtime{E}(d::TAI64_zoned{E}) = (E)
  zone_from_zonedtime{E}(d::UTC64_zoned{E}) = (E)


  # show, print for MeasuredTime with specializations for ZonedTime

  function print(io::IO, d::TAI64_zoned)
      tocks = tocks_from_zonedtime(d)
      tzone = zone_from_zonedtime(d)
      print(io, "$(tocks)@$(tzone)")
  end

  function show(io::IO,d::TAI64_zoned)
      tocks = tocks_from_zonedtime(d)
      typ   = typeof(d)
      show(io, "$(typ)($(tocks))")
  end

  function print(io::IO, d::UTC64_zoned)
      tocks = tocks_from_zonedtime(d)
      tzone = zone_from_zonedtime(d)
      print(io, "$(tocks)@$(tzone)")
  end

  function show(io::IO,d::UTC64_zoned)
      tocks = tocks_from_zonedtime(d)
      typ   = typeof(d)
      show(io, "$(typ)($(tocks))")
  end


  print(io::IO, d::MeasuredTime) =
      print(io, "$(convert(Int64,d))")
  show (io::IO, d::MeasuredTime) =
      show (io, "$(typeof(d))($(convert(Int64,d)))")




  # # conversion an promotion for <T>_tocks with Uint64, Int64

  # convert(::Int64 , tai::Type{TAI_tocks}) = int64(tai)
  # convert(::Int64 , utc::Type{UTC_tocks}) = int64(utc)
  # convert(::Uint64, tai::Type{TAI_tocks}) = uint(tai)
  # convert(::Uint64, utc::Type{UTC_tocks}) = uint(utc)
  # #
  # convert(::Type{TAI_tocks}, si::Int64 )  = TAItictoc(si)
  # convert(::Type{UTC_tocks}, si::Int64 )  = UTCtictoc(si)
  # convert(::Type{TAI_tocks}, ui::Uint64)  = TAItictoc(ui)
  # convert(::Type{UTC_tocks}, ui::Uint64)  = UTCtictoc(ui)
  # #
  # convert(::Type{TAI_tocks}, utc::Type{UTC_tocks}) = TAItictoc(int64(utc))
  # convert(::Type{UTC_tocks}, tai::Type{TAI_tocks}) = UTCtictoc(int64(tai))
  # #
  # #
  # promote_rule(::Type{TAI_tocks}, ::Type{Uint64}) = TAI_tocks
  # promote_rule(::Type{TAI_tocks}, ::Type{Int64} ) = TAI_tocks
  # promote_rule(::Type{TAI_tocks}, ::Type{Int64} ) = TAI_tocks
  # promote_rule(::Type{UTC_tocks}, ::Type{Uint64}) = UTC_tocks
  # promote_rule(::Type{UTC_tocks}, ::Type{Int64} ) = UTC_tocks
  # promote_rule(::Type{UTC_tocks}, ::Type{Uint64}) = UTC_tocks
  # #
  # promote_rule(::Type{TAI_tocks}, ::Type{UTC_tocks}) = TAI_tocks


  # #
  # promote_rule(::Type{TAI_zoned}, ::Type{Uint64}) = TAI_zoned
  # promote_rule(::Type{TAI_zoned}, ::Type{Int64} ) = TAI_zoned
  # promote_rule(::Type{TAI_zoned}, ::Type{Int64} ) = TAI_zoned
  # promote_rule(::Type{UTC_zoned}, ::Type{Uint64}) = UTC_zoned
  # promote_rule(::Type{UTC_zoned}, ::Type{Int64} ) = UTC_zoned
  # promote_rule(::Type{UTC_zoned}, ::Type{Uint64}) = UTC_zoned
  # #
  # promote_rule(::Type{TAI_zoned}, ::Type{UTC_zoned}) = TAI_zoned







   #  UTC_tocks bits:
   #         b63_(UTCshift@1mi)_b53 b52_(SubSeconds@10_000)_b00
   #  TAI_tocks bits:
   #         b63_(UTCshift@1mi)_b53 b52_(SubSeconds@10_000)_b00
   #
   #  UTCshift@1m is *minutes* added to UTC to obtain LocalTime
   #  SubSeconds@10_000 is datetime at 100 microsecond resolution
   #
