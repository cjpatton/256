# Perform linear regression on the response variable 'y' and a subset of the 
# predictor variables. 'y' is a vector and 'x' is a data.frame. The columns 
# Return the predictor accuracy criterion (PAC) value. (Calls lm().) 
ar2 <- function(y, x)
{
  a <- summary(lm(y ~ ., data=x))
  return (a$adj.r.squared)
}

# Same signature as ar2(), but logistic linear regression. 'y' is an indicator 
# random variable. (Calls glm().)
aiclogit <- function(y, x)
{
  a = glm(formula = y ~ ., data=x, family = binomial)
  return (a$aic)
}

# Reduce the parsimony of a data set for predicting the response variable 'y'.
# 'x' is either a data.frame or matrix with N samples and R attributes. 
# 'predacc' is a function with inputs 'y' and a subset of 'x' which returns
# a predictor error criterion value. if 'crit' is "min", then we minimize 
# the PAC; if 'crit' is "max", then we maximize the PAC. Return a vector of
# column names corresponding to the new parsimony for 'y'. 
prsm <- function(y, x, k=0.01, predacc=ar2, crit="min", printdel=F) 
{
  if (is.matrix(x))
  {
    x <- data.frame(x)
  }
  orig_cols <- colnames(x)
  cols <- orig_cols 
  pac <- predacc(y, x)
  if (printdel) 
  {
    cat("full outcome = ", pac, "\n")
  }

  for (col in orig_cols)
  {
    new_cols <- setdiff(cols, col)
    new_pac <- predacc(y, subset(x, select=new_cols))
    if (crit == "max" & (new_pac >= pac | new_pac >= (1-k)*pac)) # ar2() case 
    {
      cols <- new_cols
      pac <- new_pac
      if (printdel)
      {
        cat("deleted        ", col, "\n")
        cat("new outcome  = ", pac, "\n")
      }
    }
    else if ( crit == "min" & (new_pac <= pac | new_pac <= (1+k)*pac )) # aiclogit() case
    {
      cols <- new_cols
      pac <- new_pac
      if (printdel)
      {
        cat("deleted        ", col, "\n")
        cat("new outcome  = ", pac, "\n")
      }
    }
  }
  return (cols)
}
