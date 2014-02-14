erlangmix <- function(r,nmix=NULL,qftn=NULL,const=NULL) {
	returnVector <- c(r)
	if (!is.null(const) && (!is.null(nmix) || !is.null(qftn))){  
		stop("either const or both nmix and qftn have to be provided as arguments")
	} else if (!is.null(const)){ #constant simulation case
		lambda<-r/const
		returnVector<-c(returnVector,lambda)
	} else if (is.null(nmix) || nmix<1 || is.null(qftn)){ #const is null for sure
        stop("either const or both nmix and qftn have to be provided as arguments")
	} else { #function simulation case
		delta <- 1/(nmix+1) #nmix or nmix+1 here?
		print(delta)
		x <- delta
		while (x<1){
			point <- qftn(x) #point to find lambda for
			x <- x + delta 
			lambda<-r/point
			returnVector<-c(returnVector,lambda)
		}
	}
	returnVector
}

test <- function() {
   # consider the density having the value 2t on (0,1); its quantile
   # function is the sqrt function
   qf <- function(q) sqrt(q)
   print(erlangmix(500,250,qf))
}