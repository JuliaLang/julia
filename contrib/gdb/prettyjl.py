class Julia:
    def __init__(self) -> None:
        self.jl_value_t = gdb.lookup_type("jl_value_t").pointer()
        self.jl_datatype_t = gdb.lookup_type("jl_datatype_t").pointer()
        jl_sym = gdb.lookup_type("jl_sym_t")
        self.jl_sym_t = jl_sym.pointer()
        self.jl_taggedvalue_t = gdb.lookup_type("jl_taggedvalue_t").pointer()
        self.uintptr_t = gdb.lookup_type("uintptr_t")
        self.charp = gdb.lookup_type("char").pointer()
        self.tagsize = gdb.parse_and_eval("sizeof(jl_taggedvalue_t)")
        self.tagmask = gdb.parse_and_eval("~(uintptr_t)15")
        self.sym_offset = self.LLT_ALIGN(jl_sym.sizeof, self.charp.sizeof)

    def astaggedvalue(self, val):
        v = val.cast(self.uintptr_t) - self.tagsize
        return v.cast(self.jl_taggedvalue_t)

    def typeof(self, val):
        tagged_val = self.astaggedvalue(val)
        type = tagged_val['header'] & self.tagmask
        return type.cast(self.jl_value_t)

    def symbol_name(self, val):
        return val.cast(self.charp) + self.sym_offset

    def LLT_ALIGN(self, x, sz):
        return (x + sz -1) & ~(sz-1)


class ValuePrinter(Julia):
    def __init__(self, val):
        super().__init__()
        self.val = val

    def to_string(self):
        type = self.typeof(self.val).cast(self.jl_datatype_t)
        typename = self.symbol_name(type['name']['name']).string()

        return "{}::{}".format(hex(self.val), typename)

    def children(self):
        return []

def julia_pp(val):
    if str(val.type) == 'jl_value_t *': return ValuePrinter(val)

def build_pretty_printer():
    pp = gdb.printing.RegexpCollectionPrettyPrinter("julia")
    pp.add_printer('Value', '^jl_value_t *^', ValuePrinter)
    return pp

def load():
    gdb.printing.register_pretty_printer(gdb, build_pretty_printer())
# This one evens supports tasks! https://github.com/facebook/folly/blob/main/folly/fibers/scripts/gdb.py
