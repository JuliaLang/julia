module GitHub

import Main, ..Git, ..Dir

const AUTH_NOTE = "Julia Package Manager"
const AUTH_DATA = Dict{Any,Any}(
    "scopes" => ["repo"],
    "note" => AUTH_NOTE,
    "note_url" => "http://docs.julialang.org/en/latest/manual/packages/",
)

function user()
    if !success(`git config --global github.user`)
        error("""
        no GitHub user name configured; please configure it with:

            git config --global github.user USERNAME

        where USERNAME is replaced with your GitHub user name.
        """)
    end
    readchomp(`git config --global github.user`)
end

function json()
    isdefined(:JSON) || try require("JSON")
    catch err
        warn(err)
        error("using the GitHub API requires having the JSON package installed ")
    end
    Main.JSON
end

function curl(url::AbstractString, opts::Cmd=``)
    success(`curl --version`) || error("using the GitHub API requires having `curl` installed")
    out, proc = open(`curl -i -s -S $opts $url`,"r")
    head = readline(out)
    status = int(split(head,r"\s+";limit=3)[2])
    header = Dict{AbstractString,AbstractString}()
    for line in eachline(out)
        if !ismatch(r"^\s*$",line)
            (k,v) = split(line, r":\s*"; limit=2)
            header[k] = v
            continue
        end
        wait(proc); return status, header, readall(out)
    end
    error("strangely formatted HTTP response")
end
curl(url::AbstractString, data::Void, opts::Cmd=``) = curl(url,opts)
curl(url::AbstractString, data, opts::Cmd=``) =
    curl(url,`--data $(sprint(io->json().print(io,data))) $opts`)

function delete_token()
    tokfile = Dir.path(".github","token")
    Base.rm(tokfile)
    info("Could not authenticate with existing token. Deleting token and trying again.")
end

function token(user::AbstractString=user())
    tokfile = Dir.path(".github","token")
    isfile(tokfile) && return strip(readchomp(tokfile))
    status, header, content = curl("https://api.github.com/authorizations",AUTH_DATA,`-u $user`)
    tfa = false

    # Check for two-factor authentication
    if status == 401 && get(header, "X-GitHub-OTP", "") |> x->beginswith(x, "required") && isinteractive()
        tfa = true
        info("Two-factor authentication in use.  Enter auth code.  (You may have to re-enter your password.)")
        print(STDERR, "Authentication code: ")
        code = readline(STDIN) |> chomp
        status, header, content = curl("https://api.github.com/authorizations",AUTH_DATA,`-H "X-GitHub-OTP: $code" -u $user`)
    end

    if status == 422
        error_code = json().parse(content)["errors"][1]["code"]
        if error_code == "already_exists"
            if tfa
                info("Retrieving existing GitHub token. (You may have to re-enter your password twice more.)")
                status, header, content = curl("https://api.github.com/authorizations",AUTH_DATA,`-u $user`)
                status != 401 && error("$status: $(json().parse(content)["message"])")
                print(STDERR, "New authentication code: ")
                code = readline(STDIN) |> chomp
                status, header, content = curl("https://api.github.com/authorizations",`-H "X-GitHub-OTP: $code" -u $user`)
            else
                info("Retrieving existing GitHub token. (You may have to re-enter your password.)")
                status, header, content = curl("https://api.github.com/authorizations", `-u $user`)
            end
            (status >= 400) && error("$status: $(json().parse(content)["message"])")
            for entry in json().parse(content)
                if entry["note"] == AUTH_NOTE
                    tok = entry["token"]
                    break
                end
            end
        else
            error("GitHub returned validation error (422): $error_code: $(json().parse(content)["message"])")
        end
    else
        (status != 401 && status != 403) || error("$status: $(json().parse(content)["message"])")
        tok = json().parse(content)["token"]
    end

    mkpath(dirname(tokfile))
    open(io->println(io,tok),tokfile,"w")
    return tok
end

function req(resource::AbstractString, data, opts::Cmd=``)
    url = "https://api.github.com/$resource"
    status, header, content = curl(url,data,`-u $(token()):x-oauth-basic $opts`)
    response = json().parse(content)
    status, response
end

GET(resource::AbstractString, data, opts::Cmd=``) = req(resource,data,opts)
HEAD(resource::AbstractString, data, opts::Cmd=``) = req(resource,data,`-I $opts`)
PUT(resource::AbstractString, data, opts::Cmd=``) = req(resource,data,`-X PUT $opts`)
POST(resource::AbstractString, data, opts::Cmd=``) = req(resource,data,`-X POST $opts`)
PATCH(resource::AbstractString, data, opts::Cmd=``) = req(resource,data,`-X PATCH $opts`)
DELETE(resource::AbstractString, data, opts::Cmd=``) = req(resource,data,`-X DELETE $opts`)

for m in (:GET,:HEAD,:PUT,:POST,:PATCH,:DELETE)
    @eval $m(resource::AbstractString, opts::Cmd=``) = $m(resource,nothing,opts)
end

function pushable(owner::AbstractString, repo::AbstractString, user::AbstractString=user())
    status, response = HEAD("repos/$owner/$repo")
    status == 404 && error("repo $owner/$repo does not exist")
    status, response = GET("repos/$owner/$repo/collaborators/$user")
    status == 204 && return true
    status == 404 && return false
    error("unexpected API status code: $status – $(response["message"])")
end

function fork(owner::AbstractString, repo::AbstractString)
    status, response = POST("repos/$owner/$repo/forks")
    if status == 401
        delete_token()
        status, response = POST("repos/$owner/$repo/forks")
    end
    status == 202 || error("forking $owner/$repo failed: $(response["message"])")
    return response
end

end # module
