module Cache

using ..Dir, Base.Git

const cache = ".cache"

function prefetch(pkg::String, url::String, ver::VersionNumber, sha1::String)
	dir = Dir.path(pkg)
	isdir(cache) || mkpath(cache)
	cd(cache) do
		if !isdir(pkg)
			from = ispath(dir, ".git") ? dir : url
			run(`git clone -q --bare $from $pkg`)
		end
		cd(pkg) do
			run(`git config remote.origin.url $url`)
	        ispath(dir, ".git") && run(`git fetch -q --tags $dir`)
			Git.iscommit(sha1) && return
			run(`git fetch -q --tags $url`)
			Git.iscommit(sha1) ||
				error("$pkg version $ver [$(sha1[1:8])] not found, possible metadata misconfiguration")
		end
	end
	nothing
end

path(pkg::String) = joinpath(cache, pkg)
origin(pkg::String) = cd(path(pkg)) do
	readchomp(`git config remote.origin.url`)
end

end # module
