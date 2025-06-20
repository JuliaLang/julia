function fib(n::Int)
                  n <= 1 && return n
                  t = Threads.@spawn fib(n - 2)
                  return fib(n - 1) + fetch(t)::Int
              end

fib(34)
