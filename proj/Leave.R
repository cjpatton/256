#source("Parsimony.R")

# A 'leave one out' pac for a categorizing system with Y = 0 or 1
leave1out01 <- function(Y, X)
{

	n <- length(Y)

	# perform the prediction! res[i] = 1 --> correct prediction, 0 otherwise
	res <- sapply(1:n, function(x) (leave1outHelper(Y, X, x)))

	# sum(res) --> # of correct predictions
	return(sum(res) / n)

}


leave1outHelper <- function(Y, X, i)
{

	X1 <- as.matrix(X)

	y <- NULL
	x <- NULL

	n <- length(Y)


	# Remove i for the predictor
	if(i > 1){
		y <- Y[1:(i-1)]
		x <- X1[1:(i-1),]
	}
	if(i < n){
		#print(i)
		#print(n)
		#print(dim(X))
		y <- c(y, Y[(i+1):n])
		x <- rbind(x, X1[(i+1):n,])
	}

	# Fit Y & X without i
	fit <- glm(y ~ x, family = binomial)
	
	# Calculate the prediction
	left <- c(1, X1[i,]) # Add a value for the intercept to multiply against
	gt <- Y[i]

	predict <- sum(left * fit["coefficients"][[1]])

	# Return 1 if correct, 0 otherwise
	if(predict > 0.5){
		if(gt == 1){
			return(1)
		}
		return(0)
	}
	
	if(gt == 0)
	{
		return(1)
	}
	return(0)
}


leaveTest <- function(){

	df <- read.csv("pima.csv", header=T)
	#parsimony <- prsm(df$insulin, subset(df, select=-c(insulin)), k=0.01, crit="max", printdel=T)
	#parsimony <- prsm(df$class, subset(df, select=-c(class)), predacc=aiclogit, k=0.01, printdel=T)
	guy <- as.matrix(df)
	parsimony <- prsm(guy[,9], guy[,1:8], predacc=leave1out01, crit="max", k=0.01, printdel=T)
	print(parsimony)

}
