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
    out, proc = readsfrom(`curl -i -s -S $opts $url`)
    head = readline(out)
    status = int(split(head,r"\s+",3)[2])
    for line in eachline(out)
        ismatch(r"^\s*$",line) || continue
        wait(proc); return status, readall(out)
    end
    error("strangely formatted HTTP response")
end
curl(url::String, data=nothing) = curl(``,url,data)

function token(user::String=Git.readchomp(`config --global github.user`))
    tokfile = Dir.path(".github","token")
    isfile(tokfile) && return strip(readchomp(tokfile))
    status, content = curl(`-u $user`,"https://api.github.com/authorizations",AUTH_DATA)
    status == 200 || error("$status: $(r["message"])")
    tok = json().parse(content)["token"]
    mkpath(dirname(tokfile))
    open(io->println(io,tok),tokfile,"w")
    return tok
end

function req(opts::Cmd, resource::String, data=nothing)
    url = "https://api.github.com/$resource"
    status, content = curl(`$opts -u $(token()):x-oauth-basic`,url,data)
    response = json().parse(content)
    status, response
end
req(resource::String, data=nothing) = req(``,resource,data)

end # module
