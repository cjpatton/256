# Parsimony.R 
# ECS256, Norm Matloff
# Winter 2014, UC Davis

# Perform linear regression on the response variable y and a subset of the 
# predictor variables x. 'x' is an NxR matrix, where N is the number of samples
# and R is the number of attributes per sample. The columns to use for the 
# calculation is given by the vector 'cols'. 'y' is the response variable. 
# return the some error metric. (Calls lm().) 
ar2 <- function(y, x)
{
  a <- summary(lm(y ~ ., data=x))
  return(a$adj.r.squared)
}

# Same as ar2(), but logistic linear regression. 'y' is an indicator 
# random variable (in {0,1}). (Calls glm().)
aiclogit <- function(y, x)
{
  a = glm(formula = y ~ ., data=x, family = binomial)
  return (a$aic)
}

# Reduce the parsimony of a data set for predicting the response variable 'y'. 
# 'x' is an NxR matrix, where N is the number of samples and R the number of 
# attributes per sample. Return a vector of column names. 
#  TODO How to prevent over fitting when then the data set is small? 
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

# Reduce parsimony, exhaustively trying all combinations of attributes.
prsmpwr <- function(y, x, k=0.01, predacc=ar2, crit="min", printdel=F) 
{
  if (is.matrix(x))
  {
    x <- data.frame(x)
  }

  cols <- colnames(x)
  pac <- predacc(y, x)
  
  if (printdel) 
  {
    cat("full outcome = ", pac, "\n")
  }

  for (new_cols in powerset(cols))
  {
    new_pac <- predacc(y, subset(x, select=new_cols))
    if (crit == "max" & (new_pac >= pac | new_pac >= (1-k)*pac)) # ar2() case 
    {
      cols <- new_cols
      pac <- new_pac
      if (printdel)
      {
        cat("new outcome  = ", pac, "\n")
      }
    }
    else if ( crit == "min" & (new_pac <= pac | new_pac <= (1+k)*pac )) # aiclogit() case
    {
      cols <- new_cols
      pac <- new_pac
      if (printdel)
      {
        cat("new outcome  = ", pac, "\n")
      }
    }
  }
  return (cols)
}

# Enumerate all k-length subsets of S. Called by powerset().   
kset <- function(S, k, i, current, e)
{
  if (length(current) == k)
  {
    e$sets <- append(e$sets, list(c(current)))
    return()
  }

  if (i == length(S) + 1)
  {
    return()
  }
  
  current <- append(current, S[i])
  kset(S, k, i+1, current, e)

  current <- current[!current==S[i]]
  kset(S, k, i+1, current, e)
}

# Generate the power set of S. 
powerset <- function(S) 
{
  e <- new.env()
  e$sets <- list(c(S))
  for (k in (length(S)-1) : 2)
  {
    kset(S, k, 1, c(), e)
  }
  for (s in S)
  {
    e$sets <- append(e$sets, c(s))
  }
  return (e$sets)
}

# Testing, testing ... 
#parsimony <- prsm(df$insulin, subset(df, select=-c(insulin)), k=0.01, crit="max", printdel=T)
#parsimony <- prsmpwr(df$class, subset(df, select=-c(class)), predacc=aiclogit, k=0.01, printdel=T)
df <- read.csv("cadata.csv", header=T)
parsimony <- prsmpwr(df$median_house_value, subset(df, select=-c(median_house_value)), k=0.01, predacc=ar2, crit="max", printdel=T)
print(parsimony)
