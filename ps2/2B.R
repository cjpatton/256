bonusflipcorr = function(nrep, p, k){
  x_vec=c()
  y_vec=c()
  for(i in 1:nrep){
    numheads = rbinom(1,k,prob=p)
    y_vec = c(y_vec,numheads) 
    bonusheads = rbinom(1,numheads,prob=p)
    totalheads = numheads+bonusheads
    x_vec = c(x_vec,totalheads) 
  }
  cat("rho: ", cor(x_vec,y_vec))

}
