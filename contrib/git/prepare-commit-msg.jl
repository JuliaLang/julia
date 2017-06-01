#!/usr/bin/env julia
# Checks for documentation or contrib-only commits and adds [ci skip]
# to the commit message

if length(ARGS) == 3 && ARGS[2] == "commit"
	cmd = `git diff --cached --name-only $(ARGS[3])`
elseif length(ARGS) == 1
	cmd = `git diff --cached --name-only`
else
	exit()
end

io, proc = open(cmd)
for path in readlines(io)
	(startswith(path, "contrib") || startswith(path, "doc") || endswith(path, ".md")) && continue
	exit()
end
close(io)

msg = readall(ARGS[1])
contains(msg, "[ci skip]") && exit()
newmsg = string("\n\n[ci skip]", msg)
f = open(ARGS[1], "w")
write(f, newmsg)
close(f)
