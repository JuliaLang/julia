module ChallengePrompts

include("FakePTYs.jl")
using .FakePTYs: with_fake_pty
using Serialization: serialize, deserialize

const timeout = 60

"""
    challenge_prompt(code::Expr, challenges; pkgs=[])

Execute the passed code in a separate process, looking for
the passed prompts and responding as defined in the pairs of
(prompt, response) in the collection of challenges.

Optionally `import` the given `pkgs`.

Returns the value of the last expression.
"""
function challenge_prompt(code::Expr, challenges; pkgs=[])
    input_code = tempname()
    open(input_code, "w") do fp
        serialize(fp, code)
    end
    output_file = tempname()
    torun = """
        $(isempty(pkgs) ? "" : string("import ", join(pkgs, ", ")))
        using Serialization
        result = open($(repr(input_code))) do fp
            eval(deserialize(fp))
        end
        open($(repr(output_file)), "w") do fp
            serialize(fp, result)
        end"""
    cmd = `$(Base.julia_cmd()) --startup-file=no -e $torun`
    try
        challenge_prompt(cmd, challenges)
        return open(output_file, "r") do fp
            deserialize(fp)
        end
    finally
        isfile(output_file) && rm(output_file)
        isfile(input_code) && rm(input_code)
    end
    return nothing
end

function challenge_prompt(cmd::Cmd, challenges)
    function format_output(output)
        str = read(seekstart(output), String)
        isempty(str) && return ""
        return "Process output found:\n\"\"\"\n$str\n\"\"\""
    end
    out = IOBuffer()
    with_fake_pty() do pts, ptm
        p = run(detach(cmd), pts, pts, pts, wait=false) # getpass uses stderr by default
        Base.close_stdio(pts)

        # Kill the process if it takes too long. Typically occurs when process is waiting
        # for input.
        timer = Channel{Symbol}(1)
        watcher = @async begin
            waited = 0
            while waited < timeout && process_running(p)
                sleep(1)
                waited += 1
            end

            if process_running(p)
                kill(p)
                put!(timer, :timeout)
            elseif success(p)
                put!(timer, :success)
            else
                put!(timer, :failure)
            end

            # SIGKILL stubborn processes
            if process_running(p)
                sleep(3)
                process_running(p) && kill(p, Base.SIGKILL)
            end
            wait(p)
        end

        wroteall = false
        try
            for (challenge, response) in challenges
                write(out, readuntil(ptm, challenge, keep=true))
                if !isopen(ptm)
                    error("Could not locate challenge: \"$challenge\". ",
                          format_output(out))
                end
                write(ptm, response)
            end
            wroteall = true

            # Capture output from process until `pts` is closed
            write(out, ptm)
        catch ex
            if !(wroteall && ex isa Base.IOError && ex.code == Base.UV_EIO)
                # ignore EIO from `ptm` after `pts` dies
                error("Process failed possibly waiting for a response. ",
                      format_output(out))
            end
        end

        status = fetch(timer)
        close(ptm)
        if status !== :success
            if status === :timeout
                error("Process timed out possibly waiting for a response. ",
                      format_output(out))
            else
                error("Failed process. ", format_output(out), "\n", p)
            end
        end
        wait(watcher)
    end
    nothing
end

end
