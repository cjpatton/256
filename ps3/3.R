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
	title = paste("Density Function for Hazard Function h(t)",title,sep='=')
  library(ggplot2)
	#get the density values (y values)
	densities = htof(hftn, t, lower)
	#x values are stored in the t vector
	df = data.frame(Likelihood = densities, t = t)
	plot = ggplot(df) + geom_line(aes(x=t, y = Likelihood)) + ggtitle(title)
	
	return (plot)
	}

func1 = function(x) {2*x} # increasing hazard function
t = (1:100)/20
plot1 =plothtof(func1,t,0,"2t")
ggsave(filename="3_increasing.png", plot=plot1)

func2 = function(x) {4 - (2*x)} # decreasing hazard function
t = (1:100)/50
plot2 =plothtof(func2,t,0,"4-2t")
ggsave(filename="3_decreasing.png", plot=plot2)

func3 = function(x) rep(5,length(x))
t = (1:100)/20
plot3 = plothtof(func3,t,0,"5")
ggsave(filename="3_constant.png", plot=plot3)

func4 = function(x) {(x-1)*(x-.5)*(x-1.5)*(x-2) + 0.1}
t = (0:100)/20
plot4 = plothtof(func4,t,0,"(t-0.5)(t-1)(t-1.5)(t-2) + 0.1")
ggsave(filename="3_wshape.png", plot=plot4)



