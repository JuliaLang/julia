module GitHub

import Main, Base.Git, ..Dir

const AUTH_DATA = {
    "scopes" => ["repo"],
    "note" => "Julia Package Manager",
    "note_url" => "http://docs.julialang.org/en/latest/manual/packages/",
}

user() = Git.readchomp(`config --global github.user`)

function json()
    isdefined(:JSON) || try require("JSON")
    catch err
        warn(err)
        error("using the GitHub API requires having the JSON package installed ")
    end
    Main.JSON
end

function curl(url::String, opts::Cmd=``)
    success(`which -s curl`) || error("using the GitHub API requires having `curl` installed")
    out, proc = readsfrom(`curl -i -s -S $opts $url`)
    head = readline(out)
    status = int(split(head,r"\s+",3)[2])
    for line in eachline(out)
        ismatch(r"^\s*$",line) || continue
        wait(proc); return status, readall(out)
    end
    error("strangely formatted HTTP response")
end
curl(url::String, data::Nothing, opts::Cmd=``) = curl(url,opts)
curl(url::String, data, opts::Cmd=``) =
    curl(url,`--data $(sprint(io->json().print(io,data))) $opts`)

function token(user::String=user())
    tokfile = Dir.path(".github","token")
    isfile(tokfile) && return strip(readchomp(tokfile))
    status, content = curl("https://api.github.com/authorizations",AUTH_DATA,`-u $user`)
    status == 200 || error("$status: $(r["message"])")
    tok = json().parse(content)["token"]
    mkpath(dirname(tokfile))
    open(io->println(io,tok),tokfile,"w")
    return tok
end

function req(resource::String, data, opts::Cmd=``)
    url = "https://api.github.com/$resource"
    status, content = curl(url,data,`-u $(token()):x-oauth-basic $opts`)
    response = json().parse(content)
    status, response
end

for m in (:GET,:HEAD,:PUT,:POST,:PATCH,:DELETE)
    @eval begin
        m = $(string(m))
        $m(resource::String, data, opts::Cmd=``) = req(resource,data,`-X $m $opts`)
        $m(resource::String, opts::Cmd=``) = $m(resource,nothing,opts)
    end
end
GET(resource::String, data, opts::Cmd=``) = req(resource,data,opts)

function pushable(owner::String, repo::String, user::String=user())
    status, response = HEAD("repos/$owner/$repo")
    status == 404 && error("repo $owner/$repo does not exist")
    status, response = GET("repos/$owner/$repo/collaborators/$user")
    status == 204 && return true
    status == 404 && return false
    error("unexpected API status code: $status – $(response["message"])")
end

function fork(owner::String, repo::String)
    status, response = POST("repos/$owner/$repo/forks")
    status == 202 || error("forking $owner/$repo failed: $(response["message"])")
    return response["url"]
end

end # module
