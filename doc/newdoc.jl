using Base.Meta

exceptions = ["ans", "CPU_CORES", "JULIA_HOME", "STDOUT", "STDERR", "STDIN", "help", "apropos", "Help"]

qualify = ["ccall", "in", "<:", "|>", "*", "\\", "*", "/", "^", ".+", ".-", ".*",
           "./", ".\\", ".^", "//", "<<", ">>", ">>>", "==", "!=", "===", "!==",
           "<", "<=", ">", ">=", ".==", ".!=", ".<", ".<=", ".>", ".>=", "|", "*",
           "^", ":"]

cd(joinpath(dirname(@__FILE__), "..", "base", "docs")) do
  open("helpdb.jl", "w") do io
    for (mod, func, desc) in evalfile("helpdb.jl")
      (func in exceptions || ismatch(r"[\{\} ]", func)) && continue
      isop = ismatch(r"[^\w@!]|^!$", func)
      isbase = mod == "Base" && !(func in qualify)

      isop && !isbase && (func = "(:($func))")
      desc = replace(rstrip(desc), "\$", "\\\$")
      desc = replace(desc, "\"\"\"", "\\\"\"\"")

      println(io, """
        doc\"""
        ```rst
        $desc
        ```
        \"""
        $(isbase ? func : "$mod.$func")
        """)
    end
  end
end
