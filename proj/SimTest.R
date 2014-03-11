source("Parsimony.R")


# Creates a corresponding sample Y from an n by 10 X for this problem.
genY <- function(X)
{
	
	mean <- X[,1] + X[,2] + X[,3] + 0.1*X[,4] + 0.01*X[,5]

	Y <- sapply(mean, function(x) (runif(1, x-1, x+1)))	

	return(Y)
}



SimTest <- function(n)
{

	X <- matrix(runif(n * 10), n)
	
	Y <- genY(X)

	xFrame <- as.data.frame(X)

	pred1 <- prsm(Y, xFrame, k=0.01, crit="max")

	pred5 <- prsm(Y, xFrame, k=0.05, crit="max")


	#print(summary(lm(Y ~ X)))

	ret <- list(X, Y, pred1, pred5)

	return(ret)
}

SimTestK <- function(n, k)
{

	X <- matrix(runif(n * 10), n)
	
	Y <- genY(X)

	xFrame <- as.data.frame(X)

	pred <- prsm(Y, xFrame, k=k, crit="max")


	#print(summary(lm(Y ~ X)))

	ret <- list(X, Y, pred)

	return(ret)
}


threeRuns <- function(n)
{
	
	print("Run 1")
	oneRun(n)

	print("Run 2")
	oneRun(n)

	print("Run 3")
	oneRun(n)

}

oneRun <- function(n)
{

	res <- SimTest(n)
	print(res[[3]])
	print(res[[4]])

	Y <- res[[2]]
	X <- res[[1]]

	prRes <- summary(lm(Y~X))["coefficients"][[1]][,4]
	sigRes <- prRes[prRes < 0.05]

	print(names(sigRes))

}


multiTest <- function(m, n)
{

	count1 <- rep(0, 10)
	count5 <- rep(0, 10)
	countSig <- rep(0, 10)

	names(countSig) <- paste("X", 1:10, sep="")
	
	names(count1) <- paste("V", 1:10, sep="")
	names(count5) <- paste("V", 1:10, sep="")

	#print(count)

	for(i in 1:m){

		result <- SimTest(n)

		for(i in result[[3]]){
			count1[i] <- count1[i] + 1
			#print(count[i])
		}
		for(i in result[[4]]){
			count5[i] <- count5[i] + 1
			#print(count[i])
		}

		Y <- result[[2]]
		X <- result[[1]]

		prRes <- summary(lm(Y~X))["coefficients"][[1]][,4]
		sigRes <- prRes[prRes < 0.05]

		for(i in names(sigRes))
		{
			if(i != "(Intercept)"){
				countSig[i] <- countSig[i] + 1
				#print(countSig[i])
			}
		}

	}

	propSig <- countSig / m
	prop1 <- count1 / m
	prop5 <- count5 / m

	ret <- list(prop1, prop5, propSig)

	names(ret) <- c("prsm(k=0.01)", "prsm(k=0.05)", "signif")

	return(ret)
}

multiTestK <- function(m, n, k)
{

	count <- rep(0, 10)
	
	countSig <- rep(0, 10)

	names(countSig) <- paste("X", 1:10, sep="")
	names(count) <- paste("V", 1:10, sep="")

	#print(count)

	for(i in 1:m){

		result <- SimTestK(n, k)

		for(i in result[[3]]){
			count[i] <- count[i] + 1
			#print(count[i])
		}
		

		Y <- result[[2]]
		X <- result[[1]]

		prRes <- summary(lm(Y~X))["coefficients"][[1]][,4]
		sigRes <- prRes[prRes < 0.05]

		for(i in names(sigRes))
		{
			if(i != "(Intercept)"){
				countSig[i] <- countSig[i] + 1
				#print(countSig[i])
			}
		}

	}

	propSig <- countSig / m
	prop <- count / m

	ret <- list(prop, propSig)

	names(ret) <- c("prsm(k)", "signif")

	return(ret)
}

sigTest <- function(n)
{

	X <- matrix(runif(n * 10), n)
	
	Y <- genY(X)

	summary(lm(Y ~ X))
}
