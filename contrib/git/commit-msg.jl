#!/usr/bin/env julia
# Replace @macros with ＠macros to avoid triggering GitHub notifications

msg = readall(ARGS[1])
newmsg = msg

for binding in [names(Base); names(Base.Test)]
	str = string(binding)
	if startswith(str, '@')
		newmsg = replace(newmsg, str, string('＠', str[2:end]))
	end
end

if newmsg != msg
	f = open(ARGS[1], "w")
	write(f, newmsg)
	close(f)
end
