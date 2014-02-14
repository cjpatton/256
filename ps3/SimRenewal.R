





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
