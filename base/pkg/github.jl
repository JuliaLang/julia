module GitHub

import Main, Base.Git, ..Dir

const AUTH_DATA = {
    "scopes" => ["repo"],
    "note" => "Julia Package Manager",
    "note_url" => "http://docs.julialang.org/en/latest/manual/packages/",
}

function json()
    isdefined(:JSON) || try require("JSON")
    catch err
        warn(err)
        error("using the GitHub API requires having the JSON package installed ")
    end
    Main.JSON
end

function curl(opts::Cmd, url::String, data=nothing)
    success(`which -s curl`) || error("using the GitHub API requires having `curl` installed")
    data == nothing || (opts = `$opts --data $(sprint(io->JSON.print(io,data)))`)
    readall(`curl -s -S $opts $url`)
end
curl(url::String, data=nothing) = curl(``,url,data)

function token(user::String=Git.readchomp(`config --global github.user`))
    tokfile = Dir.path(".github","token")
    isfile(tokfile) && return strip(readchomp(tokfile))
    r = curl(`-u $user`,"https://api.github.com/authorizations",AUTH_DATA)
    r = json().parse(r)
    haskey(r,"token") || error("curl: $(r["message"])")
    tok = r["token"]
    mkpath(dirname(tokfile))
    open(io->println(io,tok),tokfile,"w")
    return tok
end

function req(opts::Cmd, resource::String, data=nothing)
    r = curl(`$opts -u $(token()):x-oauth-basic`,"https://api.github.com/$resource",data)
    json().parse(r)
end
req(resource::String, data=nothing) = req(``,resource,data)

end # module
