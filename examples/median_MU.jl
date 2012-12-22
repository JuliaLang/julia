function median_MU(S)
  n = length(S)

  if n < 1500
      C = sort(S)
      half = iround(n/2)
      if mod(n,2) == 1
        m = C[half]
      else
        m = (C[half]+C[half+1])/2
      end
      return m
  end

  # finds median of set S, a la Mitzenmacher & Upfal, Alg. 3.1
  
  n75 = int64(ceil(n^.75))
  R = S[int64(ceil(n*rand(n75)))]
  sort!(R)
  d = R[max(1.0, floor((n75/2)-sqrt(n)))]
  u = R[ceil((n75/2)+sqrt(n))]

  C = similar(S, 0)
  Ld = 0
  Lu = 0
  for i = 1:n
    if S[i] < d
      Ld = Ld + 1
    else
      if S[i] > u
        Lu = Lu + 1
      else
        C = push(C,S[i])
      end
    end
  end

  if (Ld > n/2) || (Lu > n/2)
    m = median(S)
  else
    if length(C) > 4*n75
      m = median(S)
    else
      sort!(C)
      if mod(n,2) == 1
        m = C[floor(n/2)-Ld+1]
      else
        m = (C[floor(n/2)-Ld]+C[floor(n/2)-Ld+1])/2
      end
    end
  end

  return m
end
