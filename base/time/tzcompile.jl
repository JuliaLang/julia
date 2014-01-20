module TZCompile

using Base.Time

# Convenience type for working with HH:MM
immutable HourMin
	hour::Int
	min::Int
end

const ZERO = HourMin(0,0)

function HourMin(s::String)
	# "-" represents 0:00 for some DST rules
	ismatch(r"\d",s) || return ZERO
	# handle single number as # of hours
	length(s) == 1 && return HourMin(int(s),0)
	ss = split(s,':')
	return HourMin(int(ss[1]),int(ss[2]))
end

millis(hm::HourMin) = hm.min*60000 + 3600000*hm.hour
(+)(x::HourMin,y::HourMin) = HourMin(x.hour+y.hour,x.min+y.min)
(-)(x::HourMin,y::HourMin) = HourMin(x.hour-y.hour,x.min-y.min)
(+)(x::Datetime,y::HourMin) = x + Hour(y.hour) + Minute(y.min)
(-)(x::Datetime,y::HourMin) = x - Hour(y.hour) - Minute(y.min)

# Zone type maps to an Olsen Timezone database entity
type Zone
	name::String 		# of the form "America/Chicago", etc.
	gmtoffset::HourMin  # Default offset: most recent is used as default
	abbr::String 		# Default abbreviation: Most recent "Standard" abbr is used
	dst::Array{Any,2} 	# nx3 matrix [DST_Millisecond_Instant offset abbr]
end
# Rules govern how Daylight Savings transitions happen for a given timezone
type Rule
	from::Int 		# First year rule applies
	to::Int 		# Rule applies up until, but not including this year
	month::Int 		# Month in which DST transition happens
	on::Function 	# Anonymous boolean function to determine day
	at::HourMin 	# Hour and minute at which the transition happens
	at_flag::Int 	# 0, 1, 2 = Local wall time, UTC time, Local Standard time
	save::HourMin 	# How much time is "saved" in daylight savings transition
	letter::String  # Timezone abbreviation letter change; i.e CST => CDT
end
# Rules are collected in RuleSets
type RuleSet
	name::String
	rules::Vector{Rule}
end
RuleSet(n::String) = RuleSet(n,Rule[])

# Max date that we create DST transition instants for
const MINDATE = Datetime(1917,1,1)
const MAXDATE = Datetime(2038,12,31)

# Helper functions/data
const MONTHS = ["Jan"=>1,"Feb"=>2,"Mar"=>3,"Apr"=>4,"May"=>5,"Jun"=>6,
				"Jul"=>7,"Aug"=>8,"Sep"=>9,"Oct"=>10,"Nov"=>11,"Dec"=>12]
const DAYS = ["Mon"=>1,"Tue"=>2,"Wed"=>3,"Thu"=>4,"Fri"=>5,"Sat"=>6,"Sun"=>0]
for d in collect(keys(DAYS))
	sym = symbol("last" * d)
	dayofweek = get(DAYS,d,0)
	@eval (function $sym(dt)
			return dayofweek(dt) == $dayofweek &&
			dayofweekofmonth(dt) == daysofweekinmonth(dt)
		end)
end

# Olsen timezone dates can be a single year (1900), yyyy-mm-dd (1900-Jan-01),
# or minute-precision (1900-Jan-01 2:00).
# They can also be given in Local Wall Time, UTC time (u), or Local Standard time (s)
function parsedate(x::String,offset,save)
	if length(x) == 4
		dt = Datetime(x,"yyyy")
	elseif length(x) < 9
		dt = Datetime(replace(x,' ','-',1),"yyyy-mmm")
	elseif length(x) < 12
		dt = Datetime(replace(replace(x,"  ",' '),' ','-',2),"yyyy-mmm-dd")
	elseif contains(x,"lastSun")
		dt = Datetime(replace(x," lastSun","",1),"yyyy mmm")
		while !lastSun(dt)
			dt += Day(1)
		end
	else
		# Sometimes there are double spaces... :(
		xx = replace(replace(x,"  ",' '),' ','-',2)
		# And this is so the Datetime parser doesn't barf w/ a letter at the end...
		xx = xx[1:(isalpha(x[end]) ? end-1 : end)]
		dt = Datetime(xx,"yyyy-mmm-dd HH:MM")
	end
	# If the time is UTC, we add back the offset and any saved amount
	# If it's local standard time, we just need to add any saved amount
	dt = x[end] == 's' ? (dt - save) : (dt - offset - save)
	return dt
end

function lineclean(line::String)
	# All lines are *supposed* to be tab-delimited
	spl = split(line,'\t')
	# We also need to strip off any comments at the end of the line
	spl[end] = replace(spl[end],r"\s*#.+$","")
	# Unfortunately, sometimes there's just a space...
	splice!(spl,1,split(strip(spl[1]),' '))
	splice!(spl,2,split(strip(spl[2]),' '))
	splice!(spl,4,split(strip(spl[4]),' '))
	length(spl) > 6 && !ismatch(r"\d{4} \w{3}",spl[7]) && splice!(spl,7,split(strip(spl[7]),' '))
	return spl
end

# Takes a string array of Rule lines and parses a RuleSet
function rulesetparse(lines,i,rulesets)
	typeof(lines) <: AbstractArray || (lines = split(lines,'\n'))
	line = lines[i]
	rulename = match(r"(?<=^Rule\s)[\w-]+?(?=\s)",line).match
	ruleset = haskey(rulesets,rulename) ? rulesets[rulename] : RuleSet(rulename)
	# And away we go...
	while true
		ismatch(r"^#",line) && (i += 1; line = lines[i])
		# First, clean and split the line
		spl = lineclean(line)
		# Then get the month. Easy.
		month = get(MONTHS,spl[6],0)
		# Now we need to get the right anonymous function
		# for determining the right day for transitioning
		if ismatch(r"last\w\w\w",spl[7])
			# We pre-built these functions above
			# They follow the format "lastSun","lastMon"
			on = eval(symbol(spl[7]))
		elseif ismatch(r"\w\w\w[<>]=\d\d?",spl[7])
			# For the first day of the week after or before a given day
			# i.e. Sun>=8 refers to the 1st Sunday after the 8th of the month
			# or in other words, the 2nd Sunday
			zday = int(match(r"\d\d?",spl[7]).match)
			dow = get(DAYS,match(r"\w\w\w",spl[7]).match,0)
			if ismatch(r"<=",spl[7])
				on = @eval (x->day(x) <= $zday && dayofweek(x) == $dow)
			else
				on = @eval (x->day(x) >= $zday && dayofweek(x) == $dow)
			end
		elseif ismatch(r"\d\d?",spl[7])
			# Matches just a plain old day of the month
			zday = int(spl[7])
			on = @eval (x->day(x) == $zday)
		else
			error("Can't parse day of month for DST change")
		end
		# Now we get the time of the transition
		c = spl[8][end]
		at = isalpha(c) ? HourMin(spl[8][1:end-1]) : HourMin(strip(spl[8]))
		# 0 for Local Wall time, 1 for UTC, 2 for Local Standard time
		at_flag = c == 'u' ? 1 : c == 's' ? 2 : 0

		save = HourMin(spl[9])
		letter = string(strip(spl[10])[1])

		from = spl[3] == "min" ? year(MINDATE) : int(spl[3])
		to = spl[4] == "only" ? from : spl[4] == "max" ? year(MAXDATE) : int(spl[4])
		# Now we've finally parsed everything we need
		push!(ruleset.rules,Rule(from,to,month,on,at,at_flag,save,letter))

		# And start again with the next line
		i += 1
		length(lines) < i && break
		line = lines[i]
		# There might be a better way to detect that we've finished
		# this RuleSet, but this seems to work fine
		nm = match(r"(?<=^Rule\t)[\w-]+?(?=\t)",line)
		(nm == nothing || nm.match != rulename) && break
	end
	return ruleset, i
end

# Takes string array, and Dict{RuleSet.name=>RuleSet} for Zone parsing
function zoneparse(lines,i,rulesets)
	typeof(lines) <: Array || (lines = split(lines,'\n'))
	line = lines[i]
	zonename = match(r"(?<=^Zone[\s\t]).+?(?=[\s\t])",line).match

	# These 3 arrays will hold our DST_matrics
	dst = Int64[]	 # DST transition instants in milliseconds
	offs = Int64[] 	 # Offset from UTC for the time leading up to dst_instant
	abbrs = String[] # Abbreviation for period up to dst_instant
	
	# Set some default values and starting Datetime increment and away we go...
	y = MINDATE
	default_letter = "S"
	save = ZERO
	offset = ZERO
	abbr = ""

	while true
		i += 1
		line = lines[i]
		# Sometimes there are "LMT" lines which we don't care about
		ismatch(r"\b[^ATGM\s0-9]M\w\b",line) && (i += 1; line = lines[i])
		ismatch(r"^\s+#.*$",line) && (i += 1; line = lines[i])
		# First clean and split the line
		spl = lineclean(line)
		# Get our offset and abbreviation string for this period
		offset = HourMin(spl[4])
		abbr = spl[6] == "zzz" ? "" : spl[6]
		# Parse the date the line rule applies up to
		# If it's blank, then we're at the last line, so go to MAXDATE
		until = (length(spl) < 7 || spl[7] == "") ? MAXDATE : parsedate(spl[7],offset,save)

		if spl[5] == "-" || ismatch(r"\d",spl[5])
			save = HourMin(spl[5])
			y = y - offset - save
			push!(dst,y.instant.ms - Time.setleaps(y.instant.ms))
			push!(offs,millis(offset+save))
			push!(abbrs,abbr)
			y = until
		else
			# Get the Rule that applies for this period
			ruleset = rulesets[spl[5]]
			# Now we iterate thru the years until we reach UNTIL
			while y < until
				# We need to check all Rules to see if they apply
				# for the given year
				for n = 1:length(ruleset.rules)
					r = ruleset.rules[n]
					# If the Rule is out of range, skip it
					r.from > year(y) && continue
					r.to   < year(y) && continue
					# Now we need to deterimine the transition day
					# We start at the Rule month, hour, minute
					# And apply our boolean "on" function until we
					# arrive at the right transition instant
					dt = Datetime(year(y),r.month,1,r.at.hour,r.at.min)
					while true
						r.on(dt) && break
						dt += Day(1)
					end
					# If our time was given in UTC, add back offset and save
					# if local standard time, add back any saved amount
					dt = r.at_flag == 1 ? dt :
						 r.at_flag == 0 ? dt - offset - save : dt - r.save

					push!(dst,dt.instant.ms - Time.setleaps(dt.instant.ms))
					push!(offs,millis(HourMin(spl[4])+r.save))
					push!(abbrs,replace(abbr,"%s",r.letter == "-" ? "" : r.letter,1))
					save = r.save != ZERO ? r.save : ZERO
					r.save == ZERO && (default_letter = r.letter)
				end
				y += Year(1)
			end
		end
		until == MAXDATE && break
	end
	abbr = replace(abbr,r"%\w",default_letter,1)
	dst_matrix = [dst offs abbrs]
	sortinds = sortperm(dst_matrix[:,1])
	return (Zone(zonename,offset,abbr,dst_matrix[sortinds,:]),i)
end
function tzparse(tzfile::String)
	zones = Zone[]
	rulesets = (String=>RuleSet)[] # RuleSet.name=>RuleSet for easy lookup
	lines = String[]
	open(tzfile) do x
		for line in eachline(x)
			l = replace(replace(line,"\r\n",""),"\n","")
			(l == "" || l[1] == '#') && continue
			push!(lines,l)
		end
	end
	i = 1
	# Rule pass
	while i <= length(lines)
		line = lines[i]
		if ismatch(r"^Rule",line)
			ruleset, i = rulesetparse(lines,i,rulesets)
			rulesets[ruleset.name] = ruleset
		else
			i += 1
		end
	end
	i = 1
	# Zone pass
	while i <= length(lines)
		line = lines[i]
		# These are some legacy definitions we don't care about
		if ismatch(r"^Zone\t[EST|MST|HST|EST5EDT|CST6CDT|MST7MDT|PST8PDT]",line)
			i += 1
		elseif ismatch(r"^Zone",line)
			zone, i = zoneparse(lines,i,rulesets)
			push!(zones,zone)
		else
			i += 1
		end
	end
	return zones, rulesets
end

function zone_symbol(z::Zone)
	n = z.name
	n = ismatch(r"(?<=/).+?$",n) ? match(r"(?<=/).+?$",n).match : n
	n = ismatch(r"(?<=/).+?$",n) ? match(r"(?<=/).+?$",n).match : n
	return replace(n,"-","_")
end

function generate_tzinfo(olsen_path::String,dest_path::String)
	files = [:africa,:antarctica,:asia,:australasia,
			 :europe,:northamerica,:southamerica]
	zones = Zone[]
	for f in files
		append!(zones,tzparse(olsen_path*string(f))[1])
	end
	z_syms = [symbol(zone_symbol(x)) for x in zones]
	z_s = {a.name=>b for (a,b) in zip(zones,z_syms)}
	s_z = {a=>b.name for (a,b) in zip(z_syms,zones)}
	open(dest_path * "tzinfo.jl","w") do f
		write(f,"### AUTO-GENERATED FILE ###\n\n")
		# Zone Definitions
		write(f,"#Define zone immutable for each timezone in Olson tz database\n")
		write(f,"for tz in $(repr(tuple(z_syms...)))\n")
		write(f,"\t@eval immutable \$tz <: PoliticalTimezone end\n")
		write(f,"end\n\n")
		# String=>Zone, Zone=>String mapping
		write(f,"const TIMEZONES = $(repr(s_z))\n")
		write(f,"const TIMEZONES1 = $(repr(z_s))\n")
		# Abbreviations

	end
end

function generate_tzdata(olsen_path::String,dest_path::String)
	files = [:africa,:antarctica,:asia,:australasia,
			 :europe,:northamerica,:southamerica]
	for f in files
		for zone in tzparse(olsen_path*string(f))[1]
			open(dest_path*zone_symbol(zone),"w") do x
				serialize(x,(zone.name,millis(zone.gmtoffset),zone.abbr,zone.dst))
			end
		end
		end
	end
end

# For generating the SETLEAPS/GETLEAPS const arrays in leapsinfo.jl
function leapsparse(leapfile::String)
	SETLEAPS = Int64[]
	GETLEAPS = Int64[]
	lines = String[]
	open(leapfile) do x
		for line in eachline(x)
			l = replace(replace(line,"\r\n",""),"\n","")
			(l == "" || l[1] == '#') && continue
			push!(lines,l)
		end
	end
	i = 1
	while i <= length(lines)
		line = lines[i]
		spl = split(line,'\t')
		secs = 1000*int(spl[5][7:8]) 
		mins = 60000*int(spl[5][4:5])
		hrs =  3600000*int(spl[5][1:2])
		days = 86400000*Time.totaldays(int(spl[2]),get(MONTHS,spl[3],0),int(spl[4]))
		inst = secs + mins + hrs + days
		push!(SETLEAPS,inst)
		push!(GETLEAPS,inst + (1000*(length(SETLEAPS)-1)))
		i += 1
	end
	return SETLEAPS,GETLEAPS
end
# Generate the leapsinfo.jl file
function generate_leapsinfo(path::String,setleaps,getleaps)
	open(path * "leapsinfo.jl","w") do f
		write(f,"### AUTO-GENERATED FILE ###\n\n")
		write(f,"# Two leap-second arrays\n# SETLEAPS is used when calculating the # of leap seconds from inputs\n# GETLEAPS represents the leap second instant in a timeline that includes leap seconds\n")
		write(f,"const SETLEAPS = $(repr(setleaps))\n")
		write(f,"const GETLEAPS = $(repr(getleaps))\n")
	end
end

#TODO
 #spot check times/offsets/abbrs
 #handle timezone link names 
 #generate common abbreviation typealiases
 #fix abbreviation for kiev? antarctica
 #use etcetera file for generic offsets?

end