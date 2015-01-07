#Tail Recursion Factorial using Julia

function fact(acc::BigInt,n)                                                                                                                                      
    if n == 0                                                                                                                                         
        acc                                                                                                                                             
    else                                                                                                                                              
        fact(acc* n,n-1)                                                                                                                                 
    end                                                                                                                                               
end

function factorial(n)
    fact(BigInt(1),n)                                                                                                                                                   
end

