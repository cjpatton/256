





simrenewal <- function(w, nevents, perwidth){

	# Get nevents samples from w, with replacement
	x <- sample(w, nevents, replace=TRUE)
	
	# Get the running total of x, or the times at which a renewal occurs
	times <- cumsum(x)

	# Total number of samples
	nsamp <- floor(times[length(times)] / perwidth)

	# For each i from 0:nsamp-1, get the next time that occurace after i*perwidth,
	# then subtract i*perwidth to get the residual for that sample.
	resid <- sapply(0:(nsamp-1), function(x) (times[times>x*perwidth][1] - (x*perwidth)))

	return(resid)
}


