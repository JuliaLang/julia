
(and
  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-350497472
  (parse-expect-underscroe "map(_[3], arrays)" parse-stmts
    '(call map (-> |#1#_| (ref |#1#_| 3)) arrays))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-350505877
  (parse-expect-underscroe "data |> map(f, _) |> filter(g, _) |> innerjoin(_, some_other_data) |> reduce(h, v0, _)" parse-stmts
    ; TODO: treat special case of <|
    '(call |\|>| (call |\|>| (call |\|>| (call |\|>| data (-> |#1#_| (call map f |#1#_|))) (-> |#2#_| (call filter g |#2#_|))) (-> |#3#_| (call innerjoin |#3#_| some_other_data))) (-> |#4#_| (call reduce h v0 |#4#_|))))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-350562318
  (parse-expect-underscroe "2_+3" parse-stmts
    '(-> |#1#_| (call + (call * 2 |#1#_|) 3)))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-1193095687
  (parse-expect-underscroe "filter(f(_) > a, xs)" parse-stmts
    '(call filter (-> |#1#_| (call > (call f |#1#_|) a)) xs))
  (parse-expect-underscroe "filter(f(_) in a, xs)" parse-stmts
    '(call filter (-> |#1#_| (call in (call f |#1#_|) a)) xs))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-1205853878
  (parse-expect-underscroe "1 + _ + 1" parse-stmts
    '(-> |#1#_| (call + 1 |#1#_| 1)))
  (parse-expect-underscroe "1 - _ - 1" parse-stmts
    '(-> |#1#_| (call - (call - 1 |#1#_|) 1)))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-1206123426
  ; don't do this use f(_) in a instead
  (parse-expect-underscroe "in(f(_), a)" parse-stmts
    '(call in (-> |#1#_| (call f |#1#_|)) a))
  (parse-expect-underscroe "f(_, c(b(_ + _) + 1))" parse-stmts
    '(-> |#1#_| (call f |#1#_| (call c (call + (call b (-> (tuple |#2#_| |#3#_|) (call + |#2#_| |#3#_|))) 1)))))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-1206335408
  (parse-expect-underscroe "WeakKeyDict{_,_}" parse-stmts
    '(-> (tuple |#1#_| |#2#_|) (curly WeakKeyDict |#1#_| |#2#_|)))
  (parse-expect-underscroe "_ isa Integer" parse-stmts
    '(-> |#1#_| (call isa |#1#_| Integer)))
  (parse-expect-underscroe "_[i]" parse-stmts
    '(-> |#1#_| (ref |#1#_| i)))
  (parse-expect-underscroe "unalias(A, _)" parse-stmts
    '(-> |#1#_| (call unalias A |#1#_|)))
  (parse-expect-underscroe "!from(_,m)" parse-stmts
    '(-> |#1#_| (call ! (call from |#1#_| m))))
  (parse-expect-underscroe "!_.form_c" parse-stmts
    '(-> |#1#_| (call ! (|.| |#1#_| 'form_c))))
  (parse-expect-underscroe "is_expr(_, :kw)", parse-stmts
    '(-> |#1#_| (call is_expr |#1#_| 'kw)))
  (parse-expect-underscroe "isa(_, Symbol)" parse-stmts
    '(-> |#1#_| (call isa |#1#_| Symbol)))
  (parse-expect-underscroe "2*_, _%3, _^2" parse-stmts
    '(tuple (-> |#1#_| (call * 2 |#1#_|)) (-> |#2#_| (call % |#2#_| 3)) (-> |#3#_| (call ^ |#3#_| 2))))
  (parse-expect-underscroe "_bool(f) = f(_)::Bool" parse-stmts
    '(= (call _bool f) (block (line 1 none) (-> |#1#_| (|::| (call f |#1#_|) Bool)))))
  (parse-expect-underscroe "4 <= _ <= 6" parse-stmts
    '(-> |#1#_| (comparison 4 <= |#1#_| <= 6)))
  (parse-expect-underscroe "!(_ isa LineNumberNode)" parse-stmts
    ;; BROKEN: don't do this, since the () is seen, work as intended
    '(call ! (-> |#1#_| (call isa |#1#_| LineNumberNode))))
  (parse-expect-underscroe "view(A, _...)" parse-stmts
    ;; BROKEN: shall we treat _... as single token? work as intended
    '(call view A (-> |#1#_| (... |#1#_|))))
  (parse-expect-underscroe "_ ∉ dims" parse-stmts
    '(-> |#1#_| (call ∉ |#1#_| dims)))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-353730741
  (parse-expect-underscroe "sin.(cos.(_))" parse-stmts
    '(|.| sin (tuple (-> |#1#_| (|.| cos (tuple |#1#_|))))))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-353741737
  (parse-expect-underscroe "[_]" parse-stmts
    '(-> |#1#_| (vect |#1#_|)))
  (parse-expect-underscroe "Int[_]" parse-stmts
    '(-> |#1#_| (ref Int |#1#_|)))
  (parse-expect-underscroe "(_,)" parse-stmts
    '(-> |#1#_| (tuple |#1#_|)))
  (parse-expect-underscroe "(i for i in _)" parse-stmts
    '(-> |#1#_| (generator i (= i |#1#_|))))
  (parse-expect-underscroe "sum(i for i in _)" parse-stmts
    ; this is special case in julia
    '(-> |#1#_| (call sum (generator i (= i |#1#_|)))))
  (parse-expect-underscroe "\"$(_)\"" parse-stmts
    '(-> |#1#_| (string |#1#_|)))
  (parse-expect-underscroe "(_...,)" parse-stmts
    '(tuple (-> |#1#_| (... |#1#_|))))
  (parse-expect-underscroe "Array{_}(uninitialized, 0)" parse-stmts
    '(-> |#1#_| (call (curly Array |#1#_|) uninitialized 0)))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-354413704
  (parse-expect-underscroe "_'" parse-stmts
    '(-> |#1#_| (|'| |#1#_|)))
  (parse-expect-underscroe "_ ? true : false" parse-stmts
    '(-> |#1#_| (if |#1#_| (true) (false))))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-356279351
  (parse-expect-underscroe "_ -> 1" parse-stmts
    ; not know how this mean
    '(-> |#1#_| (-> |#1#_| (block (line 1 none) 1))))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-414069218
  ; df = CSV.read(file) |> @map({_.col1, _.col2}) |> DataFrame
  ; CSV.read(file2) |> @map({_.col1, _.col2}) |> append!(df, _)

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-431413599
  (parse-expect-underscroe "map(_ + 1 + _, v)" parse-stmts
    '(call map (-> (tuple |#1#_| |#2#_|) (call + |#1#_| 1 |#2#_|)) v))
  (parse-expect-underscroe "map(_ - 1 - _, v)" parse-stmts
    '(call map (-> (tuple |#1#_| |#2#_|) (call - (call - |#1#_| 1) |#2#_|)) v))
  (parse-expect-underscroe "map(_.field + 1, v)" parse-stmts
    '(call map (-> |#1#_| (call + (|.| |#1#_| 'field) 1)) v))
  (parse-expect-underscroe "map(2_ + 1, v)" parse-stmts
    '(call map (-> |#1#_| (call + (call * 2 |#1#_|) 1)) v))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-431633548
  (parse-expect-underscroe "y + 3_" parse-stmts
    '(-> |#1#_| (call + y (call * 3 |#1#_|))))
  (parse-expect-underscroe "y + sqrt(_)" parse-stmts
    '(-> |#1#_| (call + y (call sqrt |#1#_|))))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-439246986
  (parse-expect-underscroe "map(y + sqrt(abs(_)), A)" parse-stmts
    ; BROKEN: work as indented
    '(call map (call + y (call sqrt (-> |#1#_| (call abs |#1#_|)))) A))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-439428090
  (parse-expect-underscroe "h(g(f(2_, a), b), c)" parse-stmts
    '(call h (call g (call f (-> |#1#_| (call * 2 |#1#_|)) a) b) c))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-439548889
  (parse-expect-underscroe "map(_.a, A)" parse-stmts
    '(call map (-> |#1#_| (|.| |#1#_| 'a)) A))
  (parse-expect-underscroe "filter(_ > y, A)" parse-stmts
    '(call filter (-> |#1#_| (call > |#1#_| y)) A))
  (parse-expect-underscroe "findall(_ in (2,3,4), A)" parse-stmts
    '(call findall (-> |#1#_| (call in |#1#_| (tuple 2 3 4))) A))
  (parse-expect-underscroe "map(_.a == 0, A)" parse-stmts
    '(call map (-> |#1#_| (call == (|.| |#1#_| 'a) 0)) A))
  (parse-expect-underscroe "filter(_+2 > y, A)" parse-stmts
    '(call filter (-> |#1#_| (call > (call + |#1#_| 2) y)) A))
  (parse-expect-underscroe "filter(_ > y-2, A)" parse-stmts
    '(call filter (-> |#1#_| (call > |#1#_| (call - y 2))) A))
  (parse-expect-underscroe "findall(2*_ in (4,6,8), A)" parse-stmts
    '(call findall (-> |#1#_| (call in (call * 2 |#1#_|) (tuple 4 6 8))) A))
  (parse-expect-underscroe "_ >_< _" parse-stmts
    '(-> (tuple |#1#_| |#2#_| |#3#_|) (comparison |#1#_| > |#2#_| < |#3#_|)))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-442977882
  ; do I really know AND/OR infix/non-infix call?
  (parse-expect-underscroe "2_ + 1" parse-stmts
    '(-> |#1#_| (call + (call * 2 |#1#_|) 1)))
  (parse-expect-underscroe "f(3, _)" parse-stmts
    '(-> |#1#_| (call f 3 |#1#_|)))
  (parse-expect-underscroe "f(3, 2_ + 1)" parse-stmts
    '(call f 3 (-> |#1#_| (call + (call * 2 |#1#_|) 1))))
  (parse-expect-underscroe "4f(3, 2_ + 1) - 1" parse-stmts
    '(call - (call * 4 (call f 3 (-> |#1#_| (call + (call * 2 |#1#_|) 1)))) 1))
  (parse-expect-underscroe "4f(3, _) - 1" parse-stmts
    '(-> |#1#_| (call - (call * 4 (call f 3 |#1#_|)) 1)))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-478137038
  (parse-expect-underscroe "@m g(_)" parse-stmts
    ; BROKEN: macro never see _ then
    '(macrocall @m (line 1 none) (-> |#1#_| (call g |#1#_|))))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-478157855
  (parse-expect-underscroe "map(_[end], A)" parse-stmts
    '(call map (-> |#1#_| (ref |#1#_| end)) A))
  (parse-expect-underscroe "map(f(_[1]), A)" parse-stmts ; BROKEN as intended
    '(call map (call f (-> |#1#_| (ref |#1#_| 1))) A))
  (parse-expect-underscroe "map(f(_)[1], A)" parse-stmts
    '(call map (-> |#1#_| (ref (call f |#1#_|) 1)) A))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-600239227
  (parse-expect-underscroe "df |> filter(_.age > 50, _)" parse-stmts
    '(call |\|>| df (-> |#2#_| (call filter (-> |#1#_| (call > (|.| |#1#_| 'age) 50)) |#2#_|))))

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-605531476
  ; (parse-expect-underscroe "" parse-stmts
  ;   '())
  ; (parse-expect-underscroe "" parse-stmts
  ;   '())
  ; (parse-expect-underscroe "" parse-stmts
  ;   '())
  ; (parse-expect-underscroe "" parse-stmts
  ;   '())
  ; (parse-expect-underscroe "" parse-stmts
  ;   '())

  ; https://github.com/JuliaLang/julia/pull/24990#issuecomment-1206335408
  (parse-expect-underscroe "I[_]" parse-stmts
    '(-> |#1#_| (ref I |#1#_|)))
  (parse-expect-underscroe "_ == dims" parse-stmts
    '(-> |#1#_| (call == |#1#_| dims)))
  (parse-expect-underscroe "_:_" parse-stmts
    '(-> (tuple |#1#_| |#2#_|) (call : |#1#_| |#2#_|)))
  (parse-expect-underscroe "_:_:_" parse-stmts
    '(-> (tuple |#1#_| |#2#_| |#3#_|) (call : |#1#_| |#2#_| |#3#_|)))
  (parse-expect-underscroe "startswith(_, entryfile * \"_\")" parse-stmts
    '(-> |#1#_| (call startswith |#1#_| (call * entryfile "_"))))
  (parse-expect-underscroe "_ .+ _" parse-stmts
    '(-> (tuple |#1#_| |#2#_|) (call .+ |#1#_| |#2#_|)))
  #t)
