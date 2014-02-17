#ECS 256 PS 3
# Written by Olga Prilepova, minor modifications to fit to hw specs by John Chen

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
    #print(delta)
    x <- delta
    for(i in 1:nmix) {
      point <- qftn(x) #point to find lambda for
      x <- x + delta 
      lambda<-r/point
      returnVector<-c(returnVector,lambda)
    }
  }
  #The lambdas are the SECOND to SECOND LAST elements of returnVector.
  lamb <-returnVector[-1];
  
  #R is the first element of the returnVector
  
  retList <- list(r=returnVector[1], lambda =lamb)
  #retList$lambda <-retList$lambda[-length(retList$lambda)]
  return(retList)
}