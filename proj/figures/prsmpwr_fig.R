# Reduce parsimony, exhaustively trying all combinations of attributes. To do this,
# we maximize/minimize the PAC over all subsets of the columns, in order of the 
# size of the subsets. 
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
    else if (crit == "min" & (new_pac <= pac | new_pac <= (1+k)*pac)) # aiclogit() case
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

# Generate the power set of S, ordered by decreasing 
# subset size. 
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
