import GitHub
import Dates
import JSON
import HTTP

# Script settings
############################################

# The repository for which to backport
REPO = "JuliaLang/julia";
BACKPORT = "1.6"
LIMIT_DATE = Dates.Date("2021-02-08")

# where the release branch started
START_COMMIT =
    BACKPORT == "1.6" ? "599d329" :
    BACKPORT == "1.0" ? "5b7e8d9" :
    error()
#############################################


# stop looking after encountering PRs opened before this date
const BACKPORT_LABEL = "backport " * BACKPORT

# Authentication
const __myauth = Ref{Union{Nothing,GitHub.Authorization}}(nothing)
function getauth()
    if __myauth[] === nothing
        __myauth[] = GitHub.authenticate(ENV["GITHUB_AUTH"])
    end
    return __myauth[]
end


########################################
# Git executable convenience functions #
########################################
function cherry_picked_commits(start_commit::AbstractString)
    commits = Set{String}()
    logg = read(`git log $start_commit..HEAD`, String)
    for match in eachmatch(r"\(cherry picked from commit (.*?)\)", logg)
        push!(commits, match.captures[1])
    end
    return commits
end

get_parents(hash::AbstractString) =
    return split(chomp(read(`git rev-list --parents -n 1 $hash`, String)))[2:end]

function get_real_hash(hash::AbstractString)
    # check if it is a merge commit
    parents = get_parents(hash)
    if length(parents) == 2 # it is a merge commit, use the parent as the commit
        hash = parents[2]
    end
    return hash
end

function try_cherry_pick(hash::AbstractString)
    if !success(`git cherry-pick -x $hash`)
        read(`git cherry-pick --abort`)
        return false
    end
    return true
end

branch() = chomp(String(read(`git rev-parse --abbrev-ref HEAD`)))

function find_pr_associated_with_commit(hash::AbstractString)
    headers = Dict()
    GitHub.authenticate_headers!(headers, getauth())
    headers["User-Agent"] = "GitHub-jl"
    req = HTTP.request("GET", "https://api.github.com/search/issues?q=$hash+type:pr+repo:$REPO";
                 headers = headers)
    json = JSON.parse(String(req.body))
    if json["total_count"] !== 1
        return nothing
    end
    item = json["items"][1]
    if !haskey(item, "pull_request")
        return nothing
    end

    pr = parse(Int, basename(item["pull_request"]["url"]))
    return pr
end

function was_squashed_pr(pr)
    parents = get_parents(pr.merge_commit_sha)
    if length(parents) != 1
        return false
    end
    return pr.number != find_pr_associated_with_commit(parents[1])
end


##################
# Main functions #
##################

function collect_label_prs(backport_label::AbstractString)
    myparams = Dict("state" => "all", "per_page" => 20, "page" => 1);
    label_prs = []
    i = 1
    print("Collecting PRs...")
    first = true
    local page_data
    while true
        print(".")
        prs, page_data = GitHub.pull_requests(REPO;
            page_limit = 1, auth=getauth(),
            (first ? (params = myparams,) : (start_page = page_data["next"],))...)
        first = false
        for pr in prs
            do_backport = false
            for label in pr.labels
                if label["name"] == backport_label
                    do_backport = true
                end
            end
            if do_backport
                push!(label_prs, pr)
            end
            if pr.created_at < LIMIT_DATE
                return label_prs
            end
        end
        haskey(page_data, "next") || break
    end
    println()
    return label_prs
end


function do_backporting(refresh_prs = false)
    label_prs = collect_label_prs(BACKPORT_LABEL)
    already_backported_commits = cherry_picked_commits(START_COMMIT)
    release_branch = branch()
    open_prs = []
    multi_commits = []
    closed_prs = []
    already_backported = []
    backport_candidates = []
    for pr in label_prs
        if pr.state != "closed"
            push!(open_prs, pr)
        else
            if pr.merged_at === nothing
                push!(closed_prs, pr)
            elseif get_real_hash(pr.merge_commit_sha) in already_backported_commits
                push!(already_backported, pr)
            else
                push!(backport_candidates, pr)
            end
        end
    end

    sort!(closed_prs; by = x -> x.number)
    sort!(already_backported; by = x -> x.merged_at)
    sort!(backport_candidates; by = x -> x.merged_at)

    failed_backports = []
    successful_backports = []
    multi_commit_prs = []
    for pr in backport_candidates
        if pr.commits === nothing
            # When does this happen...
            i = findfirst(x -> x.number == pr.number, label_prs)
            pr = GitHub.pull_request(REPO, pr.number; auth=getauth())
            @assert pr.commits !== nothing
            label_prs[i] = pr
        end
        if pr.commits != 1
            # Check if this was squashed, in that case we can still backport
            if was_squashed_pr(pr) && try_cherry_pick(get_real_hash(pr.merge_commit_sha))
                push!(successful_backports, pr)
            else
                push!(multi_commit_prs, pr)
            end
        elseif try_cherry_pick(get_real_hash(pr.merge_commit_sha))
            push!(successful_backports, pr)
        else
            push!(failed_backports, pr)
        end
    end

    # Actions to take:
    remove_label_prs = [closed_prs; already_backported]
    if !isempty(remove_label_prs)
    for ppr in remove_label_prs
           if ppr.merged_at == nothing
               @show ppr
           end
        end
        sort!(remove_label_prs; by = x -> x.merged_at)
        println("The following PRs are closed or already backported but still has a backport label, remove the label:")
        for pr in remove_label_prs
            println("    #$(pr.number) - $(pr.html_url)")
        end
        println()
    end

    if !isempty(open_prs) println("The following PRs are open but have a backport label, merge first?")
        for pr in open_prs
            println("    #$(pr.number) - $(pr.html_url)")
        end
        println()
    end

    if !isempty(failed_backports)
        println("The following PRs failed to backport cleanly, manually backport:")
        for pr in failed_backports
            println("    #$(pr.number) - $(pr.html_url) - $(pr.merge_commit_sha)")
        end
        println()
    end

    if !isempty(multi_commit_prs)
        println("The following PRs had multiple commits, manually backport")
        for pr in multi_commit_prs
            println("    #$(pr.number) - $(pr.html_url)")
        end
        println()
    end

    if !isempty(successful_backports)
        println("The following PRs where backported to this branch:")
        for pr in successful_backports
            println("    #$(pr.number) - $(pr.html_url)")
        end
        printstyled("Push the updated branch"; bold=true)
        println()
    end

    println("Update the first post with:")

    function summarize_pr(pr; checked=true)
        println("- [$(checked ? "x" : " ")] #$(pr.number) - $(pr.title)")
    end

    backported_prs = [successful_backports; already_backported]
    if !isempty(backported_prs)
        sort!(backported_prs; by = x -> x.merged_at)
        println("Backported PRs:")
        for pr in backported_prs
            summarize_pr(pr)
        end
    end

    if !isempty(failed_backports)
        println()
        println("Need manual backport:")
        for pr in failed_backports
            summarize_pr(pr; checked=false)
        end
    end

    if !isempty(multi_commit_prs)
        println()
        println("Contains multiple commits, manual intervention needed:")
        for pr in multi_commit_prs
            summarize_pr(pr; checked=false)
        end
    end

    if !isempty(open_prs)
        println()
        println("Non-merged PRs with backport label:")
        for pr in open_prs
            summarize_pr(pr; checked=false)
        end
    end
end

do_backporting()
