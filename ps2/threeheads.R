threeheads = function(nreps, k){
  statetab = matrix(ncol=k+1)
  states = c(1,2,3)
  corvec1 = c()
  corvec2 = c()
  pivec = c(0.5714286,0.2857143,0.1428571)
  for (i in 1:nreps){ 
    statev = c(sample(states,1,prob=pivec))
    for(j in 2:(k+1)){
      coinv = sample(1:2,1)
      if(coinv == 2)#heads
        nextstate = statev[j-1] + 1
      else { nextstate = 1 }
      if(nextstate == 4){ nextstate = 1 }
      statev = c(statev, nextstate)
    }
    statetab = rbind(statetab,statev)
  }
  print(nrow(statetab))
  statetab = rbind(statetab[1:nreps+1,])
  for(i in 2:(k+1))
  {
    corvec1=c(corvec1,cor(statetab[,1],statetab[,i]))
  }
  return (corvec1)
}
