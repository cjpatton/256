minersim = function(nrep){
  sumvec = c()
  choices = c(2,3,5)
  for (i in 1:nrep){
    lastattempt = sample(choices, 1)
    sum = lastattempt
    while(lastattempt != 2){
      lastattempt = sample(choices,1)
      sum = sum + lastattempt
    }
    sumvec = c(sumvec,sum)
  }
  cat ("E(Y): ", mean(sumvec),'\n')
  cat ("Var(Y): ", var(sumvec), '\n')
  
  
}