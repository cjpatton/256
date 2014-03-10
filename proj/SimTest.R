source("Parsimony.R")


# Creates a corresponding sample Y from an n by 10 X for this problem.
genY <- function(X)
{
	
	mean <- X[,1] + X[,2] + X[,3] + 0.1*X[,4] + 0.01*X[,5]

	Y <- sapply(mean, function(x) (runif(1, x-1, x+1)))	

	return(Y)
}



SimTest <- function(n, k)
{

	X <- matrix(runif(n * 10), n)
	
	Y <- genY(X)

	xFrame <- as.data.frame(X)

	pred <- prsm(Y, xFrame, k=k, crit="max")

	return(pred)
}

threeRuns <- function(n, k)
{

	print(SimTest(n, k))
	print(SimTest(n, k))
	print(SimTest(n, k))

}


sigTest <- function(n)
{

	X <- matrix(runif(n * 10), n)
	
	Y <- genY(X)

	summary(lm(Y ~ X))
}
