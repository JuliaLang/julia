module Cache

using Base.Git

const cache = ".cache"

function fetch(pkg::String, url::String, ver::VersionNumber, sha1::String)
	isdir(cache) || mkpath(cache)
	cd(cache) do
		if !isdir(pkg)
			run(`git clone -q --bare $url $pkg`)
			cd(pkg) do
				run(`git config --remove-section remote.origin`)
			end
		end
		cd(pkg) do
			run(`git fetch -q --tags $url`)
			Git.iscommit(sha1) || error("$pkg version $ver [$(sha1[1:8])] not found")
		end
	end
	nothing
end

end # module
