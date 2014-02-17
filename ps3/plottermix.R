plottermix <- function(ermixobj,plotint){
  #load ggplot2
  library(ggplot2);
  #Goal: Assess how well the approximation is working. 
  
  #Here ermixobj is an object returned from erlangmix() and plotint is a vector of length 2, 
  # giving the interval ("x axis") over which the function is to be plotted.
  
  #Function will display a graph that plots the given density and its method-of-stages approximation,
  # for two values of nmix and two values of r.
  
  title <- sprintf("Erlang Method of Stages Estimation vs Actual : r = %d, nmix = %d", ermixobj$r, length(ermixobj$lambda))
  
  #x <- rerlang_jc(n=10000, ermixobj=ermixobj);
  base <- ggplot(data.frame(x = plotint), aes(x))
  #base <- qplot(x, geom = "density", xlab ="X", main=title, xlim = plotint)
  for(i in 1:length(ermixobj$lambda)){
    newPlot <- addplot(ermixobj, i);
    base <- base + newPlot;
  }
  base <- base + sumplot(ermixobj) + labs(title);
  return(base);
}

addplot <- function(ermixobj, n)
{
  return(stat_function(fun = derlang_jc_n, args=list(ermixobj=ermixobj, n=n), colour="green"))
}

sumplot <- function(ermixobj)
{
  return(stat_function(fun = derlang_jc_sum, args=list(ermixobj=ermixobj), colour="blue"))
}

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

# UNIFORM DISTRIBUTION(10,15)
plotint = c(5,20);
unifplot(min=10, max=15, r=8, nmix=5, plotint)
unifplot(min=10, max=15, r=50, nmix=10, plotint)
unifplot(min=10, max=15, r=50, nmix=40, plotint)
unifplot(min=10, max=15, r=200, nmix=10, plotint)
unifplot(min=10, max=15, r=200, nmix=40, plotint)
unifplot(min=10, max=15, r=800, nmix=60, plotint)


#------------------------------------------------------------------------------------------#
# NORMAL DISTRIBUTION(10,4)
plotint = c(0,20);
normplot(mean=10, sd=4, r=10, nmix=4, plotint)
normplot(mean=10, sd=4, r=10, nmix=10, plotint)
normplot(mean=10, sd=4, r=10, nmix=30, plotint)
normplot(mean=10, sd=4, r=100, nmix=10, plotint)
normplot(mean=10, sd=4, r=100, nmix=30, plotint)
normplot(mean=10, sd=4, r=300, nmix=90, plotint)