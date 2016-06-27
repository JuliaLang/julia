macro context(ctxT, def)
    insert!(def.args[1].args, 2, Expr(:(::),Symbol("#context"), ctxT))
    def.args[1] = Expr(:escape, def.args[1])
    def
end
