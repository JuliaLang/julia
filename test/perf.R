## recursive fibonacci 

len = 10
fibvals = numeric(len)
fibvals[1] = 1
fibvals[2] = 1
for (i in 3:len) { 
   fibvals[i] = fibvals[i-1] + fibvals[i-2]
} 


## I don't think the above can be vectorized.

## Binary to Decimal

bin2dec <- function(x)
{
 x <- as.character(x)
 b <- as.numeric(unlist(strsplit(x, "")))
 pow <- 2 ^ ((length(b) - 1):0)
 sum(pow[b == 1])
} 

bin2dec(10)

## Matrix of all ones

ones=rep(1,200*200)
dim(ones)=c(200,200)

## A*A'
ans=ones %*% t(ones)

test=rep(200,200*200)
dim(test)=c(200,200)
all.equal(ans,test)





