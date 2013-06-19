module Cache

using Base.Git

path(pkg::String) = abspath(".cache", pkg)
origin(pkg::String) = Git.readchomp(`config remote.origin.url`, dir=path(pkg))

function prefetch(pkg::String, url::String, ver::VersionNumber, sha1::String)
	cache = path(pkg)
	isdir(".cache") || mkpath(".cache")
	if !isdir(cache)
		from = ispath(pkg,".git") ? pkg : url
		Git.run(`clone -q --bare $from $cache`)
	elseif ispath(pkg,".git")
	    Git.run(`fetch -q $pkg`, dir=cache)
	end
	Git.run(`config remote.origin.url $url`, dir=cache)
	Git.iscommit(sha1, dir=cache) && return
    Git.run(`fetch -q $url`, dir=cache)
	Git.iscommit(sha1, dir=cache) ||
		error("$pkg version $ver [$(sha1[1:8])] not found, possible metadata misconfiguration")
	return
end

end # module
