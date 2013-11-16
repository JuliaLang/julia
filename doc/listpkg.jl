###################################################
#
# A script to generate a list of all packages in reST format
#
# The JSON and Calendar packages are prerequisites for this script, and should be installed
#
# This script also needs a Github OAuth token, since un-authenticated requests are limited to 60 per hour 
# Therefore, you need to specify the token as an environment variable GH_AUTH before running this program 
#
# Once the auth token is specified, run `make listpkg` in this directory to create the latest listing. 
# Note that this will update your packages. 
#
# Checkin the resultant `packagelist.rst`, to allow the docs to be built locally without running this script. 
#
# **Note**:  
#
# To create a github oauth token, you will need to create a registered github application. Once you have an application, you can then call 
#   
#     curl -i -u <gh-user> -d @gh_auth https://api.github.com/authorizations
#
# This will prompt for your gh password using basic authentication. `gh_auth` is a local file containing: 
#
# ```JSON
# {
#   "scopes": ["public_repo"],
#   "client_id":"<application client id>",
#   "client_secret":"<application client secret>",
#   "note": "For Julia Metadata Listings"
# }
# ```
# Registered applications and authorised token are visible within your github profile page. 
###################################################

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
	print(io, ".. _available-packages:\n\n")
	print(io, "********************\n Available Packages  \n********************\n\n")
	cd(Pkg.dir()) do
	for pkg in Pkg.available()
		print(" Processing $(pkg)\n")
		url = (Pkg.Read.url(pkg))
		maxv = Pkg.available(pkg)[end]
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
			travis_url= "https://api.travis-ci.org/repositories/$(user)/$(repo).json"
			#print("processing $gh_repo_url")
			gh_repo=JSON.parse(readall(download(gh_repo_url)))
			#print("processing $gh_user_url")
			gh_contrib=JSON.parse(readall(download(gh_contrib_url)))
			travis = Dict()
			try
				travis = JSON.parse(readall(download(travis_url)))
			catch
				print("Error reading travis url for $(repo)")
			end
			
			desc = get(gh_repo, "description", "No description provided")
			homepage = get(gh_repo, "homepage", nothing)
			html_url = gh_repo["html_url"]

			sha1_file = "METADATA/$pkg/versions/$(maxv)/sha1"
			if isfile(sha1_file)
				sha1 = readchomp(sha1_file)
				gh_update_url = "https://api.github.com/repos/$(user)/$(repo)/git/commits/$(sha1)?access_token=$(gh_auth)"
				gh_update = JSON.parse(readall(download(gh_update_url)))
				author = get(gh_update, "author", nothing)
				date = author != nothing ? get(author, "date", nothing) : nothing
			end
		end
		print(io, "`$(pkg) <$(html_url)>`_"); 
		if (get(travis, "file", nothing) != "not found" && get(travis, "last_build_status", nothing) != nothing)
			print(io, " |$(pkg)_build|\n")
			print(io, "_"^(length("`$(pkg) <$(html_url)>`_")) * "\n\n")
			print(io, "  .. |$(pkg)_build| image:: ../images/travis-icon.png\n")
			print(io, "     :height: 19\n")
			print(io, "     :width: 19\n")
			print(io, "     :target: https://travis-ci.org/$(user)/$(repo)\n\n")
		else 
			print(io, "\n")
			print(io, "_"^(length("`$(pkg) <$(html_url)>`_")) * "\n\n")
		end

		
		print(io, "  .. image:: $(u[:avatar])\n     :height: 80px\n     :width: 80px\n     :align: right\n     :alt: $(u[:fullname])\n     :target: $(u[:url])\n\n")
		print(io, "  $(desc) \n\n")
		print(io, "  Current Version: ``$(maxv)``"); 
		if date != nothing
			print(io, "  (updated: $(date[1:10])) \n\n")
		end

		print(io, "  Maintainer: `$(u[:fullname]) <$(u[:url])>`_\n\n") 
		
		if homepage != nothing && length(chomp(homepage)) > 0
			print(io, "  Documentation: `<$(homepage)>`_ \n\n")
		end
		print(io, "  Dependencies::\n\n" )
		ver_dir = "METADATA/$pkg/versions/$(maxv)/requires"
		if isfile(ver_dir)
			vset = Pkg.Reqs.parse(ver_dir)
			if length(vset) > 0
				for (dpkg, dver) in vset
					print(io, "      $(dpkg)"); print(io, " "^(25-length(dpkg))); print(io, "$(dver)\n")
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
	print(io, ".. footer: $(length(Pkg.available())) packages, generated $(now()) \n\n")
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
		gh_user=JSON.parse(readall(download(gh_user_url)))
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
