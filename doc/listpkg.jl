
require("JSON")
require("Calendar")
using Calendar

try 
	  global gh_auth
		gh_auth = ENV["GH_AUTH"]
catch e
		error ("Please provide a Github OAuth Token as environment variable GH_AUTH")
end

function gen_listpkg()

	Pkg.update()
	io=open("packages/packagelist.rst","w+");
	print(io, "********************\n Available Packages  \n********************\n\n")
	cd(Pkg.dir()) do
	for pkg in Pkg.Metadata.each_package()
		print(" Processing $(pkg)\n")
		url = (Pkg.Metadata.pkg_url(pkg))
		maxv = Pkg.Metadata.versions([pkg])[end]
		url_reg = r"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?"
		gh_path_reg_git=r"^/(.*)?/(.*)?.git$"
		gh_path_reg_http=r"^/(.*)?/(.*)?$"
		m=match(url_reg, url)
		host=m.captures[4]
		path=m.captures[5]
		scheme=m.captures[2]
		u=get_user_details_default()

		if ismatch(r"github\.com", host)
			m2 = match(gh_path_reg_git, path)
			user=m2.captures[1]
			repo=m2.captures[2]
			u=get_user_details_gh(user)
			gh_repo_url = "https://api.github.com/repos/$(user)/$(repo)?access_token=$(gh_auth)"
			gh_contrib_url = "https://api.github.com/repos/$(user)/$(repo)/contributors?access_token=$(gh_auth)"
			#print("processing $gh_repo_url")
			gh_repo=JSON.parse(readall(download_file(gh_repo_url)))
			#print("processing $gh_user_url")
			gh_contrib=JSON.parse(readall(download_file(gh_contrib_url)))
			

			desc = get(gh_repo, "description", "No description provided")
			homepage = get(gh_repo, "homepage", nothing)
			html_url = gh_repo["html_url"]
		end
		print(io, "`$(pkg) <$(html_url)>`_\n"); 
		print(io, "_"^(length("`$(pkg) <$(html_url)>`_")) * "\n\n")
		print(io, "  .. image:: $(u[:avatar])\n     :height: 80px\n     :width: 80px\n     :align: right\n     :alt: $(u[:fullname])\n     :target: $(u[:url])\n\n")
		print(io, "  Current Version: ``$(maxv.version)``\n\n"); 
		print(io, "  $(desc) \n\n")
		print(io, "  Maintainer: `$(u[:fullname]) <$(u[:url])>`_\n\n") 
		
		if homepage != nothing && length(chomp(homepage)) > 0
			print(io, "  Documentation: `<$(homepage)>`_ \n\n")
		end
		print(io, "  Dependencies::\n\n" )
		ver_dir = "METADATA/$pkg/versions/$(maxv.version)/requires"
		any_ver = "Any Version"
		if isfile(ver_dir)
			vset = Pkg.Metadata.parse_requires(ver_dir)
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

		if ismatch(r"github\.com", host)
			print(io, "  Contributors:\n\n")
			for contributor in gh_contrib 
				c_user = get(contributor, "login", "")
				u=get_user_details_gh(c_user)
				print(io, "    .. image:: $(u[:avatar])\n        :height: 40px\n        :width: 40px\n")
				print(io, "        :alt: $(u[:fullname])\n        :target: $(u[:url])\n\n")
			end  #for contributor
		end

		print(io, "----\n\n")
	end  #for pkg
	print(io, ".. footer: $(length(Pkg.Metadata.packages())) packages, generated $(now()) \n\n")
	end  #cd
	
	close(io)
end #function

global user_cache = Dict{String, Dict}()

function get_user_details_default()
	u=Dict{Symbol, String}()
	u[:login] = "Unknown"
	u[:fullname] = "Unknown"
	u[:avatar] = "Not provided"
	u[:url] = "Not provided"

end

function get_user_details_gh(user)

	if !has(user_cache, user)
		gh_user_url = "https://api.github.com/users/$(user)?access_token=$(gh_auth)"
		gh_user=JSON.parse(readall(download_file(gh_user_url)))
		fullname = get(gh_user, "name", user)
		if fullname == nothing; fullname = user; end
		avatar = 
		user_url = gh_user["html_url"]

	  u=Dict{Symbol, String}()

	  u[:login] = user
	  u[:avatar] = gh_user["avatar_url"]
	  #Sometimes name is missing, sometimes it is null in the JSON
	  if get(gh_user, "name", user) == nothing
	  	u[:fullname] = user
	  else
	  	u[:fullname] = get(gh_user, "name", user)
	  end
	  u[:url] = gh_user["html_url"]

	  user_cache[user] = u
	end

	return user_cache[user]

end #function 

gen_listpkg()
