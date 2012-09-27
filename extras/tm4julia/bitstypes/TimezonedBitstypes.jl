# Jeffrey A. Sarnoff 2012-Sep-18 at 10:29 EDT in New York City



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


  #   <T>_tictoc and <T>_zoned carry datetime information the same way,
  #   giving the moment of an occurance as a subsecond accumulation.
  #   <T>_zoned  adds a type parameter giving the locale of occurance
  #   as a geopolitical region; the param value indicates the timezone.
  #
  #   UTC_tictoc is UTC_zoned without timezone parameterized typing
  #   UTC_zoned  is UTC_tictoc with   timezone parameterized typing
  #   TAI_tictoc is TAI_zoned without timezone parameterized typing
  #   TAI_zoned  is TAI_tictoc with   timezone parameterized typing
  #
  #   <T>_tictoc and <T>_zoned carry both <T>datetime information
  #   and for observed occurances, the shift from UTC to LocalTime.
  #
  #       They are paired because one cannot add timezones
  #       (what is Chicago + London), while it is easy to
  #       add offsets from UTC to LocalTime.  Arithmetic
  #       is limited to <T>_tictoc types.  This increases
  #       reliability and eliminates potential problems.
  #
  #   <T>_zoned values may persist and are useful to transfer.
  #   <T>_tictoc values are ephemeral, coming into existance
  #   in support of calculating a temporal relativity and then
  #   providing the information for <T>_zoned (or timespan)
  #   object instantiation.



  bitstype 64 UTC_tictoc    <: MeasuredTime      # by the Greenwich
  bitstype 64 TAI_tictoc    <: MeasuredTime      # Royal Observatory

  bitstype 64 UTC_zoned{E}  <: ZonedTime{E}      # E is value mapped
  bitstype 64 TAI_zoned{E}  <: ZonedTime{E}      # enum_IANA_TZ + 16




  # show, print for MeasuredTime with specializations for ZonedTime

  print(io::IO, d::MeasuredTime) = print(io, "$(box(Int64,unbox(typeof(d),d)))")
  show (io::IO, d::MeasuredTime) = show (io, "$(typeof(d))($(box(Int64,unbox(typeof(d),d))))")

  print(io::IO, d::ZonedTime) = print(io, "$(box(Int64,unbox(typeof(d),d)))")
  show (io::IO, d::ZonedTime) =
  show (io, "$(typeof(d))($( box(Int64,unbox(typeof(d),d)) ))")


  #
  #  interconversion with the 64 bit Unsigned and Signed types
  #

  for T in (:Uint64, :Int64)
      @eval begin
           TAItictoc(x::($T)) = box(TAI_tictoc, unbox(($T),x))
           UTCtictoc(x::($T)) = box(UTC_tictoc, unbox(($T),x))
           TAIzoned( x::($T)) = box(TAI_zoned , unbox(($T),x))
           UTCzoned( x::($T)) = box(UTC_zoned , unbox(($T),x))
           TAIzoned( x::($T), tz_enum::Int ) = box(TAI_zoned{tz_enum} , unbox(($T),x))
           UTCzoned( x::($T), tz_enum::Int ) = box(UTC_zoned{tz_enum} , unbox(($T),x))
      end
  end
  for T in (:TAI_tictoc, :UTC_tictoc, :TAI_zoned, :UTC_zoned)
      @eval begin
           uint64( x :: ($T) ) = box(Uint64, unbox(($T),x))
            int64( x :: ($T) ) = box( Int64, unbox(($T),x))
      end
  end


  # conversion an promotion for <T>_tictoc with Uint64, Int64

  convert(::Int64 , tai::Type{TAI_tictoc}) = int64(tai)
  convert(::Int64 , utc::Type{UTC_tictoc}) = int64(utc)
  convert(::Uint64, tai::Type{TAI_tictoc}) = uint(tai)
  convert(::Uint64, utc::Type{UTC_tictoc}) = uint(utc)
  #
  convert(::Type{TAI_tictoc}, si::Int64 )  = TAItictoc(si)
  convert(::Type{UTC_tictoc}, si::Int64 )  = UTCtictoc(si)
  convert(::Type{TAI_tictoc}, ui::Uint64)  = TAItictoc(ui)
  convert(::Type{UTC_tictoc}, ui::Uint64)  = UTCtictoc(ui)
  #
  convert(::Type{TAI_tictoc}, utc::Type{UTC_tictoc}) = TAItictoc(int64(utc))
  convert(::Type{UTC_tictoc}, tai::Type{TAI_tictoc}) = UTCtictoc(int64(tai))
  #
  #
  promote_rule(::Type{TAI_tictoc}, ::Type{Uint64}) = TAI_tictoc
  promote_rule(::Type{TAI_tictoc}, ::Type{Int64} ) = TAI_tictoc
  promote_rule(::Type{TAI_tictoc}, ::Type{Int64} ) = TAI_tictoc
  promote_rule(::Type{UTC_tictoc}, ::Type{Uint64}) = UTC_tictoc
  promote_rule(::Type{UTC_tictoc}, ::Type{Int64} ) = UTC_tictoc
  promote_rule(::Type{UTC_tictoc}, ::Type{Uint64}) = UTC_tictoc
  #
  promote_rule(::Type{TAI_tictoc}, ::Type{UTC_tictoc}) = TAI_tictoc


  # conversion an promotion for <T>_zoned with Uint64, Int64

  convert(::Int64 , tai::Type{TAI_zoned}) = int64(tai)
  convert(::Int64 , utc::Type{UTC_zoned}) = int64(utc)
  convert(::Uint64, tai::Type{TAI_zoned}) = uint(tai)
  convert(::Uint64, utc::Type{UTC_zoned}) = uint(utc)
  #
  convert(::Type{TAI_zoned}, si::Int64 )  = TAIzoned(si)
  convert(::Type{UTC_zoned}, si::Int64 )  = UTCzoned(si)
  convert(::Type{TAI_zoned}, ui::Uint64)  = TAIzoned(ui)
  convert(::Type{UTC_zoned}, ui::Uint64)  = UTCzoned(ui)
  #
  convert(::Type{TAI_zoned}, utc::Type{UTC_zoned}) = TAIzoned(int64(utc))
  convert(::Type{UTC_zoned}, tai::Type{TAI_zoned}) = UTCzoned(int64(tai))
  #
  #
  promote_rule(::Type{TAI_zoned}, ::Type{Uint64}) = TAI_zoned
  promote_rule(::Type{TAI_zoned}, ::Type{Int64} ) = TAI_zoned
  promote_rule(::Type{TAI_zoned}, ::Type{Int64} ) = TAI_zoned
  promote_rule(::Type{UTC_zoned}, ::Type{Uint64}) = UTC_zoned
  promote_rule(::Type{UTC_zoned}, ::Type{Int64} ) = UTC_zoned
  promote_rule(::Type{UTC_zoned}, ::Type{Uint64}) = UTC_zoned
  #
  promote_rule(::Type{TAI_zoned}, ::Type{UTC_zoned}) = TAI_zoned







   #  UTC_tictoc bits:
   #         b63_(UTCshift@1mi)_b53 b52_(SubSeconds@10_000)_b00
   #  TAI_tictoc bits:
   #         b63_(UTCshift@1mi)_b53 b52_(SubSeconds@10_000)_b00
   #
   #  UTCshift@1m is *minutes* added to UTC to obtain LocalTime
   #  SubSeconds@10_000 is datetime at 100 microsecond resolution
   #
