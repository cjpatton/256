# Function calcpi calculates the pi vector from the transition matrix
# The pi vector is one of the rows (they're identical) of the Nth-step-ahead transition matrix as N approaches infinity.
# To calculate the pi vector, we will square the transition matrix repeatedly, until there is no more improvement in accuracy.

calcpi <- function(tm) {
  #create a copy so we don't mess up what comes in
  pi_matrix <- tm;
  #Repeatedly square pi_matrix until there are no more differences after a squaring.
  repeat{
    
    pi_matrix <- pi_matrix %*% pi_matrix;
    
    #Calculate PI by squaring the transition matrix over and over again.
    
    offsets <- abs(sweep(x=pi_matrix,MARGIN=2,pi_matrix[1,],FUN="-"));
    #print(offsets);
    # Writer's Notes: Change to < arbritrarily small number if there are issues
    if(mean(offsets) == 0){break}
  }
  #If results of sweet were small, we've found an accurate solution
  return(pi_matrix[1,])
}