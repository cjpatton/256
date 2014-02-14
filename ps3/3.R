# density = hazard * exp(-1*integrate(hazard from 0 to t))
#htfo: Given a hazard function, hftn, returns a vector of density values


htof = function(hftn,t,lower){
  density_val = c()
  for( val in t)
  {
    density_val = c(density_val, hftn(val) * exp(-1*integrate(hftn,lower,val)$value))
    
  }
  
  
  return (density_val)
}

plothtof = function(hftn, t, lower,title){
	title = paste("Density Function for Hazard Function h(x)",title,sep='=')
  library(ggplot2)
	#get the density values (y values)
	densities = htof(hftn, t, lower)
	#x values are stored in the t vector
	df = data.frame(Likelihood = densities, xval = t)
	plot = ggplot(df) + geom_line(aes(x=xval, y = Likelihood)) + ggtitle(title)
	
	return (plot)
	}

func1 = function(x) {2*x} # increasing hazard function
t = (1:100)/20
plot1 =plothtof(func1,t,0,"2x")

func2 = function(x) {-2*x} # decreasing hazard function
t = (1:100)/50
plot2 =plothtof(func2,t,0,"-2x")

func3 = function(x) rep(5,length(x))
t = (1:100)/100
plot3 = plothtof(func3,t,0,"5")

func4 = function(x) {(x-1)*(x-2)*(x-3)*(x-4)}
t = (1:100)/20
plot4 = plothtof(func3,t,0,"(x-1)(x-2)(x-3)(x-4)")


