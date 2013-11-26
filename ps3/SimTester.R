





simrenewal <- function(w, nevents, perwidth){

	# Get nevents samples from w, with replacement
	x <- sample(w, nevents, replace=TRUE)

	#print("X")
	#print(x)
	
	# Get the running total of x, or the times at which a renewal occurs
	times <- cumsum(x)

	#print("Times")
	#print(times)

	nsamp <- floor(times[length(times)] / perwidth)

	resid <- numeric(nsamp)

	# Okay... lets see.
	
	# For a for loop, I would do something like this
	# t <- 1
	# for(i <- 0; i < nsamp ; i++)

	# 	get the next value in times larger than i*perwidth
	# 	Okay, that's what I would do if I was vectorizing. If I was doing sequential...
	
	#	while(times[t] < i*perwidth)
	#		t++

	#	Alright, can I vectorize that while loop?

	#	Yes I can. This statement gives me the next largest value of times, because times is ordered.

	#	times[times>i*perwidth][1]
	
	#	This gets me the residual of some index t...
	#	resid[i+1] <- times[t] - i*perwidth

	#	and I can combine these two, giving me only one statement in the for loop

	#	resid[i+1] <- times[times>i*perwidth][1] - i*perwidth

	#	Alright... now how do I force that to vectorize

	#	Ah, we can do this in one statement. I think.

	#mVal <- sapply(0:(nsamp-1), function(x) times[times>=x*perwidth][1])

	#print("mVal")
	#print(mVal)

	resid <- sapply(0:(nsamp-1), function(x) (times[times>x*perwidth][1] - (x*perwidth)))


	return(resid)

}


residUnif <- function(x){

	# 11.21 calculation

	temp <- 1 - punif(x)
	return(temp / 0.5)

}


plotUnifResid <- function(n, nevents, perwidth){

	library(ggplot2)
	
	title <- sprintf("Density of Residual Lifetimes for Uniform Distribution\nn = %d nevents = %d perwidth = %d", n, nevents, perwidth)


	sam <- simrenewal(runif(n), nevents, perwidth)

	data <- rbind(data.frame(type="range", dat=c(0,1)), data.frame(type="simulation", dat=sam))

	base <- ggplot(data, aes(x = dat))

	#base <- ggplot(data.frame(x = c(0,1)), aes(x))

	base <- base + geom_density(colour = "green") + stat_function(fun = residUnif, colour = "red")	

	plot <- base + ggtitle(title) + xlab("D") + ylab("DENSITY")

	return(plot)

	#ggsave(filename = "test.png", plot= plot)

}


residFaithful <- function(x){
	# Estimate by 11.21

	# fd <- (1 - Fx) / EX
	# Fx estimate <- ecdf(faithful$waiting)
	# EX estimate <- mean(faithful$waiting)

	Fx <- ecdf(faithful$waiting)

	
	temp <- 1 - Fx(x)
	return(temp / mean(faithful$waiting))



}


plotFaithfulResid <- function(nevents, perwidth){

	library(ggplot2)

	title <- sprintf("Density of Residual Lifetimes for Old Faithful\nnevents = %d perwidth = %d",nevents, perwidth)

	sam <- simrenewal(faithful$waiting, nevents, perwidth)



	data <- rbind(data.frame(type="simulation", dat=sam))

	base <- ggplot(data, aes(x = dat))

	#base <- ggplot(data.frame(x = c(0,1)), aes(x))

	base <- base + geom_density(colour = "green") + stat_function(fun = residFaithful, colour = "red")	

	plot <- base + ggtitle(title) + xlab("D") + ylab("DENSITY")

	return(plot)

}

#data <- rbind( data.frame(type="simulation", dat=sam))


#data <- rbind( data.frame(type="non-parametrical", lr=faithful$waiting), data.frame(type="parametrical", lr=sim_waiting))
#m <- ggplot(data, aes(x=lr)) 
#m <- m + geom_density(aes(fill=factor(type)), size=2, alpha=.4) 


calcED <- function(nevents, perwidth){

	
	sam <- simrenewal(faithful$waiting, nevents, perwidth)

	res1 <- mean(sam)
	sprintf("From SimRenewal() :: ED = %f", res1)




}


