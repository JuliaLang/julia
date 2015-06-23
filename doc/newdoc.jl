exceptions = ["ans", "CPU_CORES", "JULIA_HOME", "STDOUT", "STDERR", "STDIN"]

cd(joinpath(dirname(@__FILE__), "..", "base", "docs")) do
  open("helpdb.jl", "w") do io
    for (mod, func, desc) in evalfile(Base.Help.helpdb_filename())
      (func in exceptions || ismatch(r"[\{\} ]", func)) && continue

      ismatch(r"[^\w@!]|^!$", func) && (func = "(:($func))")
      desc = replace(rstrip(desc), "\$", "\\\$")
      desc = replace(desc, "\"\"\"", "\\\"\"\"")

      println(io, """
        @doc doc\"""
        ```rst
        $desc
        ```
        \""" $mod.$func
      """)
    end
  end
end
