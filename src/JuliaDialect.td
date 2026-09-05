include "llvm-dialects/Dialect/Dialect.td"

def JuliaDialect : Dialect {
  let name = "julia";
  let cppNamespace = "julia";
}
