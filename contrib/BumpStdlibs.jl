module BumpStdlibs

import GitCommand: git
import GitHub

# Some of the code in this file is taken from:
# 1. CompatHelper.jl (https://github.com/bcbi/CompatHelper.jl)
# 2. https://github.com/JuliaRegistries/General/blob/master/.ci/remember_to_update_registryci.jl

function get_all_pull_requests(repo::GitHub.Repo,
                               state::String;
                               auth::GitHub.Authorization,
                               per_page::Integer = 100,
                               page_limit::Integer = 100)
    all_pull_requests = Vector{GitHub.PullRequest}(undef, 0)
    myparams = Dict("state" => state,
                    "per_page" => per_page,
                    "page" => 1)
    prs, page_data = GitHub.pull_requests(repo;
                                          auth=auth,
                                          params = myparams,
                                          page_limit = page_limit)
    append!(all_pull_requests, prs)
    while haskey(page_data, "next")
        prs, page_data = GitHub.pull_requests(repo;
                                              auth=auth,
                                              page_limit = page_limit,
                                              start_page = page_data["next"])
        append!(all_pull_requests, prs)
    end
    unique!(all_pull_requests)
    return all_pull_requests
end

_repos_are_the_same(::GitHub.Repo, ::Nothing) = false
_repos_are_the_same(::Nothing, ::GitHub.Repo) = false
_repos_are_the_same(::Nothing, ::Nothing) = false
function _repos_are_the_same(x::GitHub.Repo, y::GitHub.Repo)
    if x.name == y.name && x.full_name == y.full_name &&
                           x.owner == y.owner &&
                           x.id == y.id &&
                           x.url == y.url &&
                           x.html_url == y.html_url &&
                           x.fork == y.fork
       return true
    else
        return false
    end
end

function exclude_pull_requests_from_forks(repo::GitHub.Repo, pr_list::Vector{GitHub.PullRequest})
    non_forked_pull_requests = Vector{GitHub.PullRequest}(undef, 0)
    for pr in pr_list
        if !_repos_are_the_same(repo, pr.base.repo)
            error("repo is not the same as pr.base.repo")
        end
        if _repos_are_the_same(repo, pr.head.repo)
            push!(non_forked_pull_requests, pr)
        end
    end
    return non_forked_pull_requests
end

function only_my_pull_requests(pr_list::Vector{GitHub.PullRequest}; my_username::String)
    _my_username_lowercase = lowercase(strip(my_username))
    n = length(pr_list)
    pr_is_mine = BitVector(undef, n)
    for i = 1:n
        pr_user_login = pr_list[i].user.login
        if lowercase(strip(pr_user_login)) == _my_username_lowercase
            pr_is_mine[i] = true
        else
            pr_is_mine[i] = false
        end
    end
    my_pr_list = pr_list[pr_is_mine]
    return my_pr_list
end

function git_commit(message)::Bool
    return try
        git() do git
            p = run(`$git commit -m "$(message)"`)
            wait(p)
            success(p)
        end
    catch
        false
    end
end

function generate_username_mentions(usernames::AbstractVector)::String
    intermediate_result = ""
    for username in usernames
        _username = filter(x -> x != '@', strip(username))
        if length(_username) > 0
            intermediate_result = intermediate_result * "\ncc: @$(_username)"
        end
    end
    final_result = convert(String, strip(intermediate_result))
    return final_result
end

function set_git_identity(username, email)
    git() do git
        run(`$git config user.name "$(username)"`)
        run(`$git config user.email "$(email)"`)
    end
    return nothing
end

function with_temp_dir(f::Function)
    original_directory = pwd()
    tmp_dir = mktempdir()
    atexit(() -> rm(tmp_dir; force = true, recursive = true))
    cd(tmp_dir)
    result = f(tmp_dir)
    cd(original_directory)
    rm(tmp_dir; force = true, recursive = true)
    return result
end

function main(main_repo::AbstractString,
              stdlibs_to_bump::AbstractVector{<:Pair{<:AbstractString, <:AbstractString}};
              github_token::AbstractString = ENV["GITHUB_TOKEN"],
              cc_usernames::AbstractVector{<:AbstractString} = String[],
              my_username::AbstractString = "github-actions[bot]",
              my_email::AbstractString = "41898282+github-actions[bot]@users.noreply.github.com")
    for stdlib_to_bump in stdlibs_to_bump
        stdlib_name = stdlib_to_bump[1]
        @info("Starting to work on $(stdlib_name)")
        _bump_stdlib(main_repo,
                     stdlib_to_bump;
                     github_token = github_token,
                     cc_usernames = cc_usernames,
                     my_username = my_username,
                     my_email = my_email)
        @info("Finished working on $(stdlib_name)")
    end
    return nothing
end

function _bump_stdlib(main_repo::AbstractString,
                      stdlib_to_bump::Pair{<:AbstractString, <:AbstractString};
                      github_token::AbstractString,
                      cc_usernames::AbstractVector{<:AbstractString},
                      my_username::AbstractString,
                      my_email::AbstractString)
    stdlib_name = stdlib_to_bump[1]
    stdlib_name_allcaps = uppercase(stdlib_name)
    stdlib_url = stdlib_to_bump[2]

    main_repo_url = "https://github.com/$(main_repo)"
    main_repo_url_with_auth = "https://x-access-token:$(github_token)@github.com/$(main_repo)"

    with_temp_dir() do tmp_dir_main
        with_temp_dir() do tmp_dir_stdlib
            git() do git
                cd(tmp_dir_main)
                run(`$(git) clone $(main_repo_url) MAINJULIAREPO`)
            end
            main_repo_dir = joinpath(tmp_dir_main, "MAINJULIAREPO")

            git() do git
                cd(tmp_dir_stdlib)
                run(`$(git) clone $(stdlib_url) STDLIBREPO`)
            end
            stdlib_dir = joinpath(tmp_dir_stdlib, "STDLIBREPO")

            stdlib_latest_commit = git() do git
                cd(stdlib_dir)
                return chomp(read(`$(git) rev-parse HEAD`, String))
            end
            @info("$(stdlib_name) latest: $(stdlib_latest_commit)")

            stdlib_latest_commit_short = git() do git
                cd(stdlib_dir)
                return chomp(read(`$(git) rev-parse --short HEAD`, String))
            end
            @info("$(stdlib_name) latest (short): $(stdlib_latest_commit_short)")

            cd(joinpath(main_repo_dir, "stdlib"))
            stdlib_version_file = joinpath(main_repo_dir, "stdlib", "$(stdlib_name).version")
            stdlib_current_commit = match(Regex("$(stdlib_name_allcaps)_SHA1[ \\t]?=[ \\t]?([A-Za-z0-9]*)[ \\t]?\\n"), read(stdlib_version_file, String))[1]
            @info("$(stdlib_name) current: $(stdlib_current_commit)")

            stdlib_current_commit_short = git() do git
                cd(stdlib_dir)
                return chomp(read(`$(git) rev-parse --short $(stdlib_current_commit)`, String))
            end
            @info("$(stdlib_name) current (short): $(stdlib_current_commit_short)")

            username_mentions_text = generate_username_mentions(cc_usernames)
            my_pr_branch_name = "bumpstdlibs/bot/bump-$(stdlib_name)-to-$(stdlib_latest_commit)"
            my_pr_title = "Bump $(stdlib_name) to $(stdlib_latest_commit_short)"
            my_pr_commit_message = my_pr_title

            cd(stdlib_dir)
            changelog = try
                read(`$(git) log --oneline $(stdlib_current_commit)^...$(stdlib_latest_commit)`, String)
            catch
                "Unable to automatically generate changelog."
            end

            my_pr_description_line_one = string("This pull request bumps ",
                                                "`$(stdlib_name)` to ",
                                                "`$(stdlib_latest_commit_short)` ",
                                                "(`$(stdlib_latest_commit)`), ",
                                                "which is the most recent ",
                                                "commit on the ",
                                                "`$(stdlib_name)` ",
                                                "master branch.")

            my_pr_description_line_two = string("Previously, ",
                                                "`$(stdlib_name)` ",
                                                "was at ",
                                                "`$(stdlib_current_commit_short)` ",
                                                "(`$(stdlib_current_commit)`).")

            my_pr_body = """
                $(my_pr_description_line_one)

                $(my_pr_description_line_two)

                $(username_mentions_text)

                [$(stdlib_url)@$(stdlib_current_commit_short)...$(stdlib_latest_commit_short)]($(stdlib_url)/compare/$(stdlib_current_commit)...$(stdlib_latest_commit))
                ```
                $(changelog)
                ```
                """

            cd(joinpath(main_repo_dir, "stdlib"))
            git() do git
                try
                    run(`$(git) checkout $(my_pr_branch_name)`)
                    @info("The branch already exists on the origin, so I will use the existing version of the branch", my_pr_branch_name)
                catch
                    @info("Created new branch $(my_pr_branch_name)")
                    run(`$(git) checkout -B $(my_pr_branch_name)`)
                end
            end

            cd(main_repo_dir)
            deps_checksums_dir = joinpath(main_repo_dir, "deps", "checksums")
            for entry in readdir(deps_checksums_dir)
                fullpath = joinpath(deps_checksums_dir, entry)
                if startswith(entry, "$(stdlib_name)-")
                    @info("Deleting: $(fullpath)")
                    rm(fullpath; force = true, recursive = true)
                end
            end

            rm(stdlib_version_file; force = true, recursive = true)
            open(stdlib_version_file, "w") do io
                println(io, "$(stdlib_name_allcaps)_BRANCH = master")
                println(io, "$(stdlib_name_allcaps)_SHA1 = $(stdlib_latest_commit)")
            end

            cd(main_repo_dir)
            set_git_identity(my_username, my_email)

            auth = try
                GitHub.authenticate(github_token)
            catch
                nothing
            end

            if lowercase(strip(stdlib_current_commit)) != lowercase(strip(stdlib_latest_commit))
                @info("Running `make` inside the `stdlib` subdirectory")
                cd(joinpath(main_repo_dir, "stdlib"))
                run(`make`)

                # @info("Running `make` inside the `deps` subdirectory")
                # cd(joinpath(main_repo_dir, "deps"))
                # run(`make`)

                cd(main_repo_dir)
                git() do git
                    run(`$(git) add -A`)
                end
                commit_was_success = git_commit(my_pr_commit_message)
                @info("commit_was_success: $(commit_was_success)")

                git() do git
                    cd(main_repo_dir)
                    run(`$(git) remote rm origin`)
                    run(`$(git) remote add origin $(main_repo_url_with_auth)`)
                end

                git() do git
                    cd(main_repo_dir)
                    run(`$(git) push --force origin $(my_pr_branch_name)`)
                end

                params = Dict{String, String}()
                params["title"] = my_pr_title
                params["head"] = my_pr_branch_name
                params["base"] = "master"
                params["body"] = my_pr_body
                main_repo_github = GitHub.repo(main_repo; auth = auth)
                try
                    GitHub.create_pull_request(main_repo_github;
                                               params = params,
                                               auth = auth)
                catch
                end

                # This last part just makes sure that the PR body is up-to-date.
                _update_pr_body(main_repo_github;
                                auth = auth,
                                my_username = my_username,
                                params_original = params)
            end
        end
    end
    return nothing
end

function _update_pr_body(main_repo_github::GitHub.Repo;
                         auth,
                         my_username,
                         params_original::AbstractDict)
    params = deepcopy(params_original)
    _this_job_pr = _get_matching_prs(main_repo_github;
                                     auth = auth,
                                     my_username = my_username,
                                     params_original = params_original)
    for pr in _this_job_pr
        GitHub.update_pull_request(main_repo_github,
                                   pr;
                                   params = params,
                                   auth = auth)
    end
    return nothing
end

function _get_matching_prs(main_repo_github::GitHub.Repo;
                           auth,
                           my_username,
                           params_original::AbstractDict)
    params = deepcopy(params_original)
    _all_prs = get_all_pull_requests(main_repo_github, "open";
                                     auth = auth)
    _all_nonforked_prs = exclude_pull_requests_from_forks(main_repo_github,
                                                          _all_prs)
    _all_my_prs = only_my_pull_requests(_all_nonforked_prs;
                                        my_username = my_username)
    _this_job_pr = Vector{GitHub.PullRequest}(undef, 0)
    for candidate_pr in _all_my_prs
        if candidate_pr.base.ref == params["base"] && candidate_pr.head.ref == params["head"]
            push!(_this_job_pr, candidate_pr)
        end
    end
    return _this_job_pr
end

end # end module BumpStdlibs
