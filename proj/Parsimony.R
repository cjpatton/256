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
}

# Same as ar2(), but logistic linear regression. 'y' is an indicator 
# random variable (in {0,1}). (Calls glm().)
aiclogit <- function(y, x, cols)
{
}

# Reduce the parsimony of a data set for predicting the response variable 'y'. 
# 'x' is an NxR matrix, where N is the number of samples and R the number of 
# attributes per sample. Return a vector of column names. 
prsm <- function(y, x, k=0.1, predacc=ar2, crit=NULL, printdel=F) 
{
  cols <- c()

  # So I'm thinking we proceed in this way. We're gonna try various subsets
  # of the column names until we fine one which has the lowest regression 
  # error. Matloff tells us exactly how to proceed. 
  # cols <- c(1 .. R) // all of the columns initially. 
  # pac <- predacc(y, x, cols)
  # for i <- 1 to R do
  #   cols_new <- c(1 .. R) - c(i) // This psuedocode is supposed to mean "all 
  #                                // columns except i"
  #   pac_new <- predacc(y, x, cols_temp) 
  #   if (pac - pac_new < 1 - k) 
  #     then cols <- cols_new 
  #          pac <- pac_new
  #   fi
  # done 
  # I think if we're doing logit regression, we need to maximize pac instead of 
  # minimize it. (Does anyone get this part.) 

  return (cols)
}
