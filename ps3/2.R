erlangmix <- function(r,nmix=NULL,qftn=NULL,const=NULL) {
	returnVector <- r
	if (!is.null(const) && (!is.null(nmix) || !is.null(qftn))){  
		stop("either const or both nmix and qftn have to be provided as arguments")
	} else if (!is.null(const)){ #constant simulation case
		lambda<-const/r
		returnVector<-c(returnVector,lambda)
	} else if (is.null(nmix) || is.null(qftn)){ #const is null for sure
        stop("either const or both nmix and qftn have to be provided as arguments")
	} else { #function simulation case

	}
}
