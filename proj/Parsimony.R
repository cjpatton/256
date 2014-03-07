# Parsimony.R 
# ECS256, Norm Matloff
# Winter 2014, UC Davis

# Perform linear regression on the response variable y and a subset of the 
# predictor variables x. 'x' is an NxR matrix, where N is the number of samples
# and R is the number of attributes per sample. The columns to use for the 
# calculation is given by the vector 'cols'. 'y' is the response variable. 
# return the some error metric. (Calls lm().) 
ar2 <- function(y, x, cols)
{
  a <- summary(lm(y ~ ., data=subset(x, select=cols)))
  return(a$adj.r.squared)
}

# Same as ar2(), but logistic linear regression. 'y' is an indicator 
# random variable (in {0,1}). (Calls glm().)
aiclogit <- function(y, x, cols)
{
  # TODO 
}

# Reduce the parsimony of a data set for predicting the response variable 'y'. 
# 'x' is an NxR matrix, where N is the number of samples and R the number of 
# attributes per sample. Return a vector of column names. 
#  TODO How to prevent over fitting when then the data set is small? 
#  TODO prediction accuracy criterion for aiclogit().
prsm <- function(y, x, k=0.1, predacc=ar2, crit="max", printdel=F) 
{
  orig_cols <- colnames(x)
  cols <- orig_cols 
  pac <- predacc(y, x, cols) 

  for (col in orig_cols) 
  {
    new_cols <- setdiff(cols, col)
    new_pac <- predacc(y, x, new_cols)        
    if (crit == "max" & new_pac > pac | pac - new_pac < k) # ar2() case 
    {
      cols <- new_cols
      pac <- new_pac
    }
  }
  return (cols)
}

# Testing, testing ... 
df <- read.csv("pima.csv", header=T)
parsimony <- prsm(df$age, subset(df, select=-c(age)), k=0.01)
print(parsimony)
