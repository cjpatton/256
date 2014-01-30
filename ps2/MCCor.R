# Chris, Alex, and John

# Calculate Pi distribution from a transition matrix. 
findpis <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p) 
  imp[n,] <- rep(1,n)
  rhs <- c(rep(0,n-1),1)
  solve(imp, rhs)
}

# Calculate correlations between the current state and
# the next K states, given transition matrix tm. 
mccor <- function(tm, K) {

  Pi <- findpis(tm)
  n <- nrow(p)

  # mu = E[Xi] for all i. 
  mu <- 0 
  for (i in 1 : n) {
    mu <- mu + (i * Pi[i])
  }

  # denom = Var(Xi). 
  denom <- 0
  for (i in 1 : n) {
    denom <- denom + (i^2 * Pi[i])
  }
  denom <- denom - mu^2

  # corr = Correlations. 
  corr <- c()

  # Mj = transition matrix at time i+j. 
  Mj <- tm 

  i <- 1
  for(j in 1 : K) {

    outer_sum <- 0
    for (l in 1 : n) {
      inner_sum <- 0
      for (k in 1 : n) {
        inner_sum <- inner_sum + (k * Mj[l,k])
      }
      outer_sum <- outer_sum + (l * Pi[l] * inner_sum)
    }

    # numer = Cov(Xi, Xi+j).
    numer <- outer_sum - mu^2

    corr <- c(corr, c(numer / denom))
    Mj <- Mj %*% tm # Next Mj. 
  }

  # Result is c(rho(Xi, Xi+1), rho(Xi, Xi+2) ... rho(Xi, Xi+k)). 
  return (corr)
}

# A test case (three heads game from book). 
p <- matrix(rep(0,9), nrow=3)
p[1,1] <- 0.5
p[1,2] <- 0.5
p[2,3] <- 0.5
p[2,1] <- 0.5
p[3,1] <- 1

# Simulate the test case. 
threeheads = function(nreps, k) {
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
