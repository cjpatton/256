bonusflip = function(nrep, p, k){
  headsvec=c()
  for(i in 1:nrep){
    numheads = rbinom(1,k,prob=p)
    bonusheads = rbinom(1,numheads,prob=p)
    totalheads = numheads+bonusheads
    headsvec = c(headsvec,totalheads) 
  }
  cat("Var(X): ", var(headsvec))
}