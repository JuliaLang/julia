#
# Load this file in the web repl to display winston plots inline.
#

load("winston")
using Winston

function web_show(user_id, p::PlotContainer)
    g = nothing
    try
        g = svg(p)
    catch err
        return __Message(__MSG_OUTPUT_EVAL_ERROR, {user_id, sprint(show,err)})
    end
    return __Message(__MSG_OUTPUT_HTML, {user_id, g})
end
