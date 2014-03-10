#source("Parsimony.R")

# A 'leave one out' pac for a categorizing system with Y = 0 or 1
leave1out01 <- function(Y, X)
{

	n <- length(Y)

	res <- sapply(1:n, function(x) (leave1outHelper(Y, X, x)))

	return(sum(res) / n)

}


leave1outHelper <- function(Y, X, i)
{

	y <- NULL
	x <- NULL

	n <- length(Y)

	if(i > 1){
		y <- Y[1:(i-1)]
		x <- X[1:(i-1),]
	}
	if(i < n){
		#print(i)
		#print(n)
		#print(dim(X))
		y <- c(y, Y[(i+1):n])
		x <- rbind(x, X[(i+1):n,])
	}

	
	fit <<- glm(y ~ x, family = binomial)
	
	left <- c(1, X[i,]) # Add a value for the intercept to multiply against
	gt <- Y[i]

	predict <- sum(left * fit["coefficients"][[1]])

	if(predict > 0.5){
		return(1)
	}

	return(0)
}
