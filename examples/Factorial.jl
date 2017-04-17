function fatorial(n)
	if n==1
		n
	else
		n*fatorial(n-1)
	end
end
print(fatorial(5))
