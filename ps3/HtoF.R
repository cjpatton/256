htof = function(hftn,t,lower){
  density_val = c()
  for(val in t)
  {
    density_val = c(density_val, hftn(val) * 
          exp(-1*integrate(hftn,lower,val)$value))
  }
  return (density_val)
}
