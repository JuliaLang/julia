abstract SIPrefix
type Yocto <: SIPrefix end
type Zepto <: SIPrefix end
type Atto <: SIPrefix end
type Femto <: SIPrefix end
type Pico <: SIPrefix end
type Nano <: SIPrefix end
type Micro <: SIPrefix end
type Milli <: SIPrefix end
type Centi <: SIPrefix end
type Deci <: SIPrefix end
type SINone <: SIPrefix end
type Deca <: SIPrefix end
type Hecto <: SIPrefix end
type Kilo <: SIPrefix end
type Mega <: SIPrefix end
type Giga <: SIPrefix end
type Tera <: SIPrefix end
type Peta <: SIPrefix end
type Exa <: SIPrefix end
type Zetta <: SIPrefix end
type Yotta <: SIPrefix end

# PrettyShow
pshow(x) = pshow(OUTPUT_STREAM::IOStream, x)
# LatexShow
lshow(x) = lshow(OUTPUT_STREAM::IOStream, x)
# FullShow
fshow(x) = fshow(OUTPUT_STREAM::IOStream, x)

let
#  Prefix        ToSINone      Show          PrettyShow   LatexShow  Full
const prefix_table = {
  (Yocto,        1e-24,        "y",          "y",         "y",       "yocto")
  (Zepto,        1e-21,        "z",          "z",         "z",       "zepto")
  (Atto,         1e-18,        "a",          "a",         "a",       "atto")
  (Femto,        1e-15,        "f",          "f",         "f",       "femto")
  (Pico,         1e-12,        "p",          "p",         "p",       "pico")
  (Nano,         1e-9,         "n",          "n",         "n",       "nano")
  (Micro,        1e-6,         "u",          "\u03bc",   L"$\mu$",   "micro")
  (Milli,        1e-3,         "m",          "m",         "m",       "milli")
  (Centi,        1e-2,         "c",          "c",         "c",       "centi")
  (Deci,         1e-1,         "d",          "d",         "d",       "deci")
  (Deca,         1e1,          "da",         "da",        "da",      "deca")
  (Hecto,        1e2,          "h",          "h",         "h",       "hecto")
  (Kilo,         1e3,          "k",          "k",         "k",       "kilo")
  (Mega,         1e6,          "M",          "M",         "M",       "mega")
  (Giga,         1e9,          "G",          "G",         "G",       "giga")
  (Tera,         1e12,         "T",          "T",         "T",       "tera")
  (Peta,         1e15,         "P",          "P",         "P",       "peta")
  (Exa,          1e18,         "E",          "E",         "E",       "exa")
  (Zetta,        1e21,         "Z",          "Z",         "Z",       "zetta")
  (Yotta,        1e24,         "Y",          "Y",         "Y",       "yotta")
}
global to_reference
global show
global pshow
global lshow
global _unit_si_prefixes
for (t, f, s, ps, ls, fs) in prefix_table
    @eval to_reference(::Type{$t}) = $f
    @eval show(io, ::Type{$t}) = print(io, $s)
    @eval pshow(io, ::Type{$t}) = print(io, $ps)
    @eval lshow(io, ::Type{$t}) = print(io, $ls)
    @eval fshow(io, ::Type{$t}) = print(io, $fs)
end
function return_prefix(i::Int)
    if i <= length(prefix_table)
        return prefix_table[i][1]
    elseif i == length(prefix_table)+1
        return SINone
    else
        error("Something is wrong")
    end
end
_unit_si_prefixes = ntuple(length(prefix_table)+1, i->return_prefix(i))
end  # let
to_reference(::Type{SINone}) = 1
show(io, ::Type{SINone}) = nothing
pshow(io, ::Type{SINone}) = nothing
lshow(io, ::Type{SINone}) = nothing
fshow(io, ::Type{SINone}) = nothing

# Units
abstract Unit
type SIUnit{TP<:SIPrefix, TU<:Unit} end
SIUnit{TP<:SIPrefix, TU<:Unit}(tp::Type{TP}, tu::Type{TU}) = SIUnit{tp, tu}
SIUnit{TU<:Unit}(tu::Type{TU}) = SIUnit{SINone, tu}
function show{TP<:SIPrefix, TU<:Unit}(io, tu::SIUnit{TP, TU})
    print(io, TP)
    print(io, TU)
end

# Values with units
type SIValue{TP<:SIPrefix, TU<:Unit, Tdata}
    value::Tdata
end
SIValue{TP<:SIPrefix, TU<:Unit, Tdata}(tp::Type{TP}, tu::Type{TU}, val::Tdata) = SIValue{tp, tu, Tdata}(val)
SIValue{TU<:Unit, Tdata}(tu::Type{TU}, val::Tdata) = SIValue{SINone, tu, Tdata}(val)
prefix{TP<:SIPrefix, TU<:Unit, Tdata}(v::SIValue{TP, TU, Tdata}) = TP
unit{TP<:SIPrefix, TU<:Unit, Tdata}(v::SIValue{TP, TU, Tdata}) = TU

function show{TP<:SIPrefix, TU<:Unit}(io, v::SIValue{TP, TU})
    print(io, v.value, " ")
    show(io, TP)
    show(io, TU)
end
function pshow{TP<:SIPrefix, TU<:Unit}(io, v::SIValue{TP, TU})
    print(io, v.value, " ")
    pshow(io, TP)
    pshow(io, TU)
end
function lshow{TP<:SIPrefix, TU<:Unit}(io, v::SIValue{TP, TU})
    print(io, v.value, " ")
    lshow(io, TP)
    lshow(io, TU)
end
function fshow{TP<:SIPrefix, TU<:Unit}(io, v::SIValue{TP, TU})
    print(io, v.value, " ")
    fshow(io, TP)
    fshow(io, TU)
end

# Functions and parsing dictionaries
_unit_string_dict = Dict{ASCIIString, Tuple}()

function _unit_gen_func(table)
    for (t, r, to_r, s, ps, ls, fs) in table
        @eval reference(::Type{$t}) = $r
        @eval to_reference(::Type{$t}) = $to_r
        @eval show(io, ::Type{$t}) = print(io, $s)
        @eval pshow(io, ::Type{$t}) = print(io, $ps)
        @eval lshow(io, ::Type{$t}) = print(io, $ls)
        @eval fshow(io, ::Type{$t}) = print(io, $fs)
    end
end

function _unit_gen_product_dict(table)
    for (u, rest) in table
        for p in _unit_si_prefixes
            key = string(p)*string(u)
#            println(key, ": ", p, ", ", u) 
            _unit_string_dict[key] = (p, u)
        end
    end
end

function _unit_gen_noprefix_dict(table)
    for (u, rest) in table
        key = string(u)
#        println(key, ": ", u) 
        _unit_string_dict[key] = (SINone, u)
    end
end


# Length units
type Meter <: Unit end
type Inch <: Unit end
let
  # Unit    RefUnit     ToRef            show      pshow     lshow   fshow
const utable = {
  (Meter,   Meter,      1,               "m",      "m",      "m",    "meter")
  (Inch,    Meter,      25.4/1000,       "in",     "in",     "in",   "inch")
}
_unit_gen_func(utable)
_unit_gen_product_dict({utable[1]})  # parse prefixes only for Meter
_unit_gen_noprefix_dict(utable[2:end])  # parse prefixes only for Meter
end

# Time units
type Second <: Unit end
type Minute <: Unit end
type Hour <: Unit end
type Day <: Unit end
type Week <: Unit end
type JulianYear <: Unit end
let
  # Unit       RefUnit     ToRef            show      pshow     lshow  fshow
const utable = {
  (Second,     Second,      1,             "s",      "s",      "s",    "second")
  (Minute,     Second,      60,            "min",    "min",    "min",  "minute")
  (Hour,       Second,      3600,          "hr",     "hr",     "hr",   "hour")
  (Day,        Second,      86400,         "d",      "d",      "d",    "day")
  (JulianYear, Second,      365.25*86400,  "yr",     "yr",     "yr",   "year")
}
_unit_gen_func(utable)
_unit_gen_product_dict({utable[1]})   # prefixes are only common for seconds
_unit_gen_noprefix_dict(utable[2:end])
end

# Rate units
type Herz <: Unit end
let
  # Unit       RefUnit     ToRef        show      pshow     lshow   fshow
const utable = {
  (Herz,       Herz,        1,          "Hz",     "Hz",     "Hz",   "Herz")
}
_unit_gen_func(utable)
_unit_gen_product_dict(utable)
end

# Parsing strings to extract SIValues
function parse_sivalue(s::String)
    m = match(r"[a-zA-Z]", s)
    if m.offset < 1
        error("String does not have a 'value unit' structure")
    end
    val = parse_float(Float64, strip(s[1:m.offset-1]))
    (prefix, unit) = _unit_string_dict[strip(s[m.offset:end])]
    return SIValue(prefix, unit, val)
end

# Conversion between units
function convert{Uin<:Unit, Uout<:Unit, Pin<:SIPrefix, Pout<:SIPrefix, T}(::Type{SIUnit{Pout, Uout}}, u::SIValue{Pin, Uin, T})
    if reference(Uin) != reference(Uout)
        error("Not convertable")
    end
    return SIValue(Pout, Uout, u.value * to_reference(Uin) * to_reference(Pin) / to_reference(Uout) / to_reference(Pout))
end
