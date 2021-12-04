# Prototype ParseStream interface
#
# Here we test the ParseStream interface, by taking input code and checking
# that the correct sequence of begin_node, end_node and bump!() produces a
# valid parse tree.

code = """
for i = 1:10
    xx[i] + 2
    # hi
    yy
end
"""

st = ParseStream(code)

# Here we manually issue parse events in the order a Julia parser would issue
# them (if such a parser existed... which it doesn't yet!)
begin_node(st)
    bump!(st, TRIVIA_FLAG)  # for
    begin_node(st)
        bump!(st) # 'i'
        bump!(st, TRIVIA_FLAG) # =
        begin_node(st)
            bump!(st) # 1
            bump!(st) # :
            bump!(st) # 10
        end_node(st, K"call", INFIX_FLAG)
    end_node(st, K"=")
    begin_node(st)
        begin_node(st) # [call]
            begin_node(st) # [ref]
                bump!(st) # xx
                bump!(st, TRIVIA_FLAG) # [
                bump!(st) # i
                bump!(st, TRIVIA_FLAG) # ]
            end_node(st, K"ref")
            bump!(st) # +
            bump!(st) # 2
        end_node(st, K"call", INFIX_FLAG)
        bump!(st) # yy
    end_node(st, K"block")
    bump!(st, TRIVIA_FLAG) # end
t = end_node(st, K"for")

# ## Input code
println("-----------------------")
print(code)
println()

# ## Output tree
show(stdout, MIME"text/plain"(), t, code, show_trivia=true)
