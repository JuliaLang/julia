require("pkg")
try 
require("JSON")
catch 
Pkg.add("JSON")
require("JSON")
end

function gen_listpkg()
	local gh_auth
	try 
		gh_auth = ENV["GH_AUTH"]
	catch e
		error ("Please provide a Github OAuth Token as environment variable GH_AUTH")
	end
	Pkg.update()
	io=open("packages/packagelist.rst","w+");
	print(io, "********************\n Available Packages  \n********************\n\n")
	cd(julia_pkgdir()) do 
	for pkg in Metadata.each_package()
		print(" Processing $(pkg)\n")
		url = (Metadata.pkg_url(pkg))
		maxv = Metadata.versions([pkg])[end]
		url_reg = r"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?"
		gh_path_reg_git=r"^/(.*)?/(.*)?.git$"
		gh_path_reg_http=r"^/(.*)?/(.*)?$"
		m=match(url_reg, url)
		host=m.captures[4]
		path=m.captures[5]
		scheme=m.captures[2]
		fullname = "Unkown"
		avatar = "Not on Github"
		desc = "Not provided"
		homepage = nothing
		html_url = url
		user_url = url

		if ismatch(r"github\.com", host)
			m2 = match(gh_path_reg_git, path)
			user=m2.captures[1]
			repo=m2.captures[2]
			gh_repo_url = "https://api.github.com/repos/$(user)/$(repo)?access_token=$(gh_auth)"
			gh_user_url = "https://api.github.com/users/$(user)?access_token=$(gh_auth)"
			#print("processing $gh_repo_url")
			gh_repo=JSON.parse(readall(download_file(gh_repo_url)))
			#print("processing $gh_user_url")
			gh_user=JSON.parse(readall(download_file(gh_user_url)))
			#Sometimes name is missing, sometimes it is null in the JSON
			fullname = get(gh_user, "name", user)
			if fullname == nothing; fullname = user; end
			avatar = gh_user["avatar_url"]
			user_url = gh_user["html_url"]
			desc = get(gh_repo, "description", "No description provided")
			homepage = get(gh_repo, "homepage", nothing)
			html_url = gh_repo["html_url"]
		end
		print(io, "`$(pkg) <$(html_url)>`_\n"); 
		print(io, "_"^(length("`$(pkg) <$(html_url)>`_")) * "\n\n")
		print(io, "  .. image:: $(avatar)\n     :height: 80px\n     :width: 80px\n     :align: right\n     :alt: $(fullname)\n\n")
		print(io, "  Current Version: ``$(maxv.version)``\n\n"); 
		print(io, "  $(desc) \n\n")
		print(io, "  Maintainer: `$(fullname) <$user_url>`_\n\n") 
		
		if homepage != nothing && length(chomp(homepage)) > 0
			print(io, "  More Info: `<$(homepage)>`_ \n\n")
		end
		print(io, "  Dependencies::\n\n" )
		ver_dir = "METADATA/$pkg/versions/$(maxv.version)/requires"
		any_ver = "Any Version"
		if isfile(ver_dir)
			vset = Metadata.parse_requires(ver_dir)
			if length(vset) > 0
				for deps in vset
					print(io, "      $(deps.package)"); print(io, " "^(15-length(deps.package))); print(io, "$(length(deps.versions)>0 ? deps.versions : any_ver)\n")
				end
			else 
				print(io, "      None\n")
			end
		else 
			print(io, "      None\n")
		end
		print(io, "\n")
	end  #for
	end  #cd
	close(io)
end #function

gen_listpkg()
