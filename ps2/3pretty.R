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
