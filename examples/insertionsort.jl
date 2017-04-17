size = 50000
vetor = round(rand(Float32,size)*1000)

function InsertionSorte(vetor, tam)
	for i in 1:tam	
		aux = vetor[i]
		j = i
		while((aux < vetor[j - 1]) && (j > 0)) 
            vetor[j] = vetor[j - 1]
            j--
		end
        vetor[j] = aux
    end
end

InsertionSorte(vetor,size)
println("Vetor ordenado: $vetor")
