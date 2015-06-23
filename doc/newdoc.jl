using Base.Meta

exceptions = ["ans", "CPU_CORES", "JULIA_HOME", "STDOUT", "STDERR", "STDIN"]

qualify = ["ccall", "in", "<:", "|>", "*", "\\", "*", "/", "^", ".+", ".-", ".*",
           "./", ".\\", ".^", "//", "<<", ">>", ">>>", "==", "!=", "===", "!==",
           "<", "<=", ">", ">=", ".==", ".!=", ".<", ".<=", ".>", ".>=", "|", "*",
           "^"]

cd(joinpath(dirname(@__FILE__), "..", "base", "docs")) do
  open("helpdb.jl", "w") do io
    for (mod, func, desc) in evalfile(Base.Help.helpdb_filename())
      (func in exceptions || ismatch(r"[\{\} ]", func)) && continue
      isop = ismatch(r"[^\w@!]|^!$", func)
      isbase = mod == "Base" && !(func in qualify)

      isop && !isbase && (func = "(:($func))")
      desc = replace(rstrip(desc), "\$", "\\\$")
      desc = replace(desc, "\"\"\"", "\\\"\"\"")

      println(io, """
        @doc doc\"""
        ```rst
        $desc
        ```
        \""" $(isbase ? func : "$mod.$func")
      """)
    end
  end
end
