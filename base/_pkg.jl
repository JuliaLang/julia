# Temporary module whose functions will be filled in
# by the real Pkg in stdlib
module _Pkg
    const status = Ref{Any}()
    const dir    = Ref{Any}()
end
