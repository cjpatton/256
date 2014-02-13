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

#example usage:
func1 = function(x) {2*x} # our function
t = (1:100)/100 #generate vector of 100 values evenly spaced between 0 and 1
htof(func1,t,0)