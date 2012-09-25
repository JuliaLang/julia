#
# Load this file in the web repl to display winston plots inline.
#

load("winston.jl")
import Winston.*

web_show(user_id, p::PlotContainer) = __Message(__MSG_OUTPUT_HTML, {user_id,svg(p)})

