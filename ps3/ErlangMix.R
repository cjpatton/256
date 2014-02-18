#ECS 256 PS 3 Problem 2
# Written by Olga Prilepova and John Chen

# Function: erlangmix
# Goal: Approximate the distribution of a given nonnegative random variable by a mixture of Erlang distributions.
# Parameters:
#   r is the desired value for the Erlang parameter r, also known as the shape parameter.
#   nmix is the desired number of components (rate parameters lambda) in the mixture; for use only if const is NULL 
#   qftn is the quantile function for the given distribution
#   const is the value of a constant that we wish to approximate; for use only if nmix and qftn are NULL
# Returns:
#   The object ermixobj, which has the attributes:
#     r is the shape parameter for a gamma distribution.
#     lamb is a vector of nmix lambdas (rate parameters) to be used to approximate the quantile function or constant.
#   The ermixobj will be fed into plottermix(), which will generate a plot of the approximation.

erlangmix <- function(r,nmix=NULL,qftn=NULL,const=NULL) {
  # Initialize return variable(s)
  returnVector <- c(r)
  
  # Check parameter integrity
  if (!is.null(const) && (!is.null(nmix) || !is.null(qftn))){  
    stop("either const or both nmix and qftn have to be provided as arguments")
  } else if (!is.null(const)){ #constant simulation case
    lambda<-r/const
    returnVector<-c(returnVector,lambda)
  } else if (is.null(nmix) || nmix<1 || is.null(qftn)){ #const is null for sure
    stop("either const or both nmix and qftn have to be provided as arguments")
  } else { 
    #function simulation case
    delta <- 1/(nmix+1)
    x <- delta
    for(i in 1:nmix) {
      #Generate nmix lambdas
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
  return(retList)
}

plottermix <- function(ermixobj,plotint){
  library(ggplot2);
  #Goal: Assess how well the approximation is working. 
  
  #Here ermixobj is an object returned from erlangmix() and plotint is a vector of length 2, 
  # giving the interval ("x axis") over which the function is to be plotted.
  
  #Function will display a graph that plots the given density and its method-of-stages approximation,
  # for two values of nmix and two values of r.
  
  title <- sprintf("Erlang Method of Stages Estimation vs Actual : r = %d, nmix = %d", ermixobj$r, length(ermixobj$lambda))
  
  #Create base plot
  base <- ggplot(data.frame(x = plotint), aes(x));
  
  #Insert each component erlang distribution
  for(i in 1:length(ermixobj$lambda)){
    newPlot <- addplot(ermixobj, i);
    base <- base + newPlot;
  }
  
  #Overlay the approximation distribution.
  base <- base + sumplot(ermixobj) + labs(title);
  return(base);
}

#Generate erlang distribution (gamma distribution).
addplot <- function(ermixobj, n)
{
  return(stat_function(fun = derlang_jc_n, args=list(ermixobj=ermixobj, n=n), colour="green"))
}

sumplot <- function(ermixobj)
{
  return(stat_function(fun = derlang_jc_sum, args=list(ermixobj=ermixobj), colour="blue"))
}

# Add up all previous lambdas to get the nice stacking effect
derlang_jc_n <- function(x, ermixobj, n){
    sum <- 0;
    for(i in 1:n){
      sum <- sum + dgamma(x=x, shape=ermixobj$r, rate=ermixobj$lambda[i]);
    }
    return(sum/length(ermixobj$lambda));
}

derlang_jc_sum <- function(x, ermixobj){
  sum <- 0;
  for(i in 1:length(ermixobj$lambda)){
    sum <- sum + dgamma(x=x, shape=ermixobj$r, rate=ermixobj$lambda[i]);
  }
  return(sum/length(ermixobj$lambda))
}

#------------------------------------------------------------------------------------------#
# This function generates a uniform distribution, approximates it, and plots the two against each other on on plot.
unifplot <- function(min, max, r, nmix, plotint) {
  #Define Quantile Function
  qf <- function(q) qunif(p=q,min=min,max=max);
  #Calculate erlang estimation
  ermixobj <- erlangmix(r, nmix, qf);
  #Generate plot
  plot <- plottermix(ermixobj, plotint);
  #Superimpose actual distribution in red
  plot <- plot + stat_function(fun = dunif, args = list(min=min, max=max), colour = "red");
  #Finish up labeling touches.
  title <- sprintf("Uniform Distribution: Min = %d, Max = %d, r = %d, nmix = %d", min, max, r, nmix);
  xlab <- "X (Random Variable Drawn from Distribution)";
  ylab <- "DENSITY";
  plot <- plot + ggtitle(title) + xlab(xlab) + ylab(ylab);
  
  filename <- sprintf("unifdist_%d_%d_%d_%d.png", min, max, r, nmix);
  ggsave(filename=filename, plot=plot)
  # Display plot
  plot
}
#------------------------------------------------------------------------------------------#


# This function generates a normal distribution, approximates it, and plots the two against each other on on plot.
normplot <- function(mean=10, sd=4, r=10, nmix=10, plotint=c(0,20)) {
  #Define Quantile Function
  qf <- function(q) qnorm(p=q,mean=mean,sd=sd);
  #Calculate erlang estimation
  ermixobj <- erlangmix(r, nmix, qf);
  #Generate plot
  plot <- plottermix(ermixobj, plotint);
  #Superimpose actual distribution in red
  plot <- plot + stat_function(fun = dnorm, args = list(mean=10, sd=4), colour = "red");
  #Finish up labeling touches.
  title <- sprintf("Normal Distribution: Mean = %d, SD = %d, r = %d, nmix = %d", mean, sd, r, nmix);
  xlab <- "X (Random Variable Drawn from Distribution)";
  ylab <- "DENSITY";
  plot <- plot + ggtitle(title) + xlab(xlab) + ylab(ylab);
  
  # Display plot
  filename <- sprintf("normdist_%d_%d_%d_%d.png", mean, sd, r, nmix);
  ggsave(filename=filename, plot=plot)
  plot
}
#------------------------------------------------------------------------------------------#
# The following code generates the plots used in the TeX file with various parameters.
# UNIFORM DISTRIBUTION(10,15)
testUnif <- function(){
	plotint = c(5,20);
	unifplot(min=10, max=15, r=8, nmix=5, plotint)
	unifplot(min=10, max=15, r=50, nmix=10, plotint)
	unifplot(min=10, max=15, r=50, nmix=40, plotint)
	unifplot(min=10, max=15, r=200, nmix=10, plotint)
	unifplot(min=10, max=15, r=200, nmix=40, plotint)
	unifplot(min=10, max=15, r=800, nmix=60, plotint)
}

#------------------------------------------------------------------------------------------#
# NORMAL DISTRIBUTION(10,4)
testNorm <- function(){
	plotint = c(0,20);
	normplot(mean=10, sd=4, r=10, nmix=4, plotint)
	normplot(mean=10, sd=4, r=10, nmix=10, plotint)
	normplot(mean=10, sd=4, r=10, nmix=30, plotint)
	normplot(mean=10, sd=4, r=100, nmix=10, plotint)
	normplot(mean=10, sd=4, r=100, nmix=30, plotint)
	normplot(mean=10, sd=4, r=300, nmix=90, plotint)
}