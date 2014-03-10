#Load Matloff's Algorithms

smoothz <- function(z,sf,k,checkna=T,cls=NULL,
                    nchunks=length(cls)) {
  require(parallel)
  if (is.vector(z)) z <- matrix(z,ncol=1)
  if (is.data.frame(z)) z <- as.matrix(z)
  if (checkna) z <- z[complete.cases(z),]
  if (is.null(cls)) {
    return(sf(z,k))
  } else {
    # determine which observations each node will process
    n <- nrow(z)
    idxchunks <- splitIndices(n,nchunks)
    zchunks <- Map(function(ichunk) z[ichunk,],idxchunks)
    tmp <- clusterApply(cls,zchunks,sf,k)
    return(Reduce(c,tmp))
  }
}

# kNN regression; predict the points in data from those points
knnreg <- function(data,k) {
  require(FNN)
  ycol <- ncol(data)
  x <- data[,-ycol,drop=F]
  y <- data[,ycol]
  idx <- get.knn(data=x,k=k)$nn.index
  # i-th row of idx contains the indices of the k nearest neighbors to
  # that row of x (not including that row)
  apply(idx,1,function(idxrow) mean(y[idxrow]))
}

# kNN density estimation
knndens <- function(data,k) {
  # finds kNN-based density estimates at the rows of data
  require(FNN)
  dsts <- get.knn(data,k=k)$nn.dist
  hvec <- dsts[,k]
  # (k/nrow(data)) / (pi * hvec^2)
  (k/nrow(data)) / (hvec^ncol(data))
}

# predicts Y values for the rows in newx, based on the X data oldx from
# our training set and the corresponding estimated regression values
# oldxregest; since the latter are already the result of smoothing, we
# predict via 1-NN 
smoothzpred <- function(newx,oldx,oldxregest,
                        checkna=T,cls=NULL,nchunks=length(cls)) {
  require(parallel)
  if (is.vector(newx)) newx <- matrix(newx,nrow=1)
  if (is.vector(oldx)) oldx <- matrix(oldx,nrow=1)
  if (is.data.frame(newx)) newx <- as.matrix(newx)
  if (is.data.frame(oldx)) oldx <- as.matrix(oldx)
  if (checkna) newx <- newx[complete.cases(newx),]
  if (is.null(cls)) {
    return(onennreg(newx,oldx,oldxregest))
  } else {
    n <- nrow(newx)
    # determine which observations each node will process
    idxchunks <- splitIndices(n,nchunks)
    newxchunks <- Map(function(ichunk) newx[ichunk,],idxchunks)
    tmp <- clusterApply(cls,newxchunks,onennreg,oldx,oldxregest)
    return(Reduce(c,tmp))
  }
}

# 
onennreg <- function(nx,ox,oxrgest) {
  require(FNN)
  if (is.vector(nx)) nx <- matrix(nx,nrow=1)
  if (is.vector(ox)) ox <- matrix(ox,nrow=1)
  pred1row <- function(nxrow) {
    nxrow <- matrix(nxrow,nrow=1)
    idx <- get.knnx(data=ox,query=nxrow,k=1)$nn.index
    oxrgest[idx]
  }
  apply(nx,1,pred1row)
}

#Done loading Algorithm

#-------------------------------------------------------------------------

#Read Bank Data from CSV File
data.bank <- read.csv(file="bank-full.csv",sep=";");

#Generate Training and Validation Sets, 80% and 20% of the data each

# This data set is arranged in such a way that the higher indices are more likely to have a Y response
# Thus, we should shuffle the data.
maxIndex <- dim(data.bank)[1];
indices <- 1:maxIndex;
shuffledIndices <- sample(indices, size=maxIndex, replace=FALSE);
# Calculate the 80% index
index_80 <- round(0.8 * maxIndex);

#Separate data into training and validation sets

#Generate unique ID's for each title, as KNN relies upon a numerical distance, and cannot process character parameters.
l=unique(c(as.character(data.bank$job), as.character(data.bank$marital), as.character(data.bank$education)
           , as.character(data.bank$default), as.character(data.bank$housing), as.character(data.bank$loan)
           , as.character(data.bank$contact), as.character(data.bank$month), as.character(data.bank$poutcome)
           , as.character(data.bank$y)))

data.bank = data.frame(#age = as.numeric(data.bank$age) 
                 job=as.numeric(factor(data.bank$job, levels=l))
                , marital = as.numeric(factor(data.bank$marital, level = l))
                , education = as.numeric(factor(data.bank$education, level = l))
                , default = as.numeric(factor(data.bank$default, level = l))
                , housing = as.numeric(factor(data.bank$housing, level = l))
                , loan = as.numeric(factor(data.bank$loan, level = l))
                , contact = as.numeric(factor(data.bank$contact, level = l))
                , balance = as.numeric(data.bank$balance)
                #, day = as.numeric(data.bank$day)
                , month = as.numeric(factor(data.bank$month, level = l))
                , duration = as.numeric(data.bank$duration)
                , campaign = as.numeric(data.bank$campaign)
                , pdays = as.numeric(data.bank$pdays)
                , previous = as.numeric(data.bank$previous)
                #, poutcome = as.numeric(factor(data.bank$poutcome, level = l))
                , y = as.numeric(factor(data.bank$y, level = l)));

#Convert Values such that they are equidistant within a category.
#Normalize Parameters, so that all are considered somewhat equally.
data.bank = data.frame(#age = data.bank$age / (1*max(data.bank$age)+0)
                        job=data.bank$job / (0*max(data.bank$job)+1) 
                       , marital = (data.bank$marital - 13) / (0*max(data.bank$marital)+1) 
                       , education = 0.3*(data.bank$education - 16) / (0*max(data.bank$education)+1) 
                       , default = (data.bank$default - 19) / (0*max(data.bank$default)+1) 
                       , housing = 0.3*(data.bank$housing - 19) / (0*max(data.bank$housing)+1) 
                       , loan = (data.bank$loan - 19) / (0*max(data.bank$loan)+1) 
                       , contact = data.bank$contact / (0*max(data.bank$contact)+1) 
                       , balance = data.bank$balance / (0.5*max(data.bank$balance)+0) 
                       #, day = data.bank$day / (0*max(data.bank$day)+1) 
                       , month = 0.3*(data.bank$month-22) / (0*max(data.bank$month)+1) 
                       , duration = 0.3*data.bank$duration / (0*max(data.bank$duration)+1) 
                       , campaign = data.bank$campaign / (0*max(data.bank$campaign)+1) 
                       , pdays =  data.bank$pdays / (0*max(data.bank$pdays)+1) 
                       , previous =  data.bank$previous / (0*max(data.bank$previous)+1) 
                       #, poutcome =  data.bank$poutcome / (0*max(data.bank$poutcome)+1) 
                       , y = (data.bank$y - 19));
training_set <- data.bank[shuffledIndices[1:index_80],];
validation_set <- data.bank[shuffledIndices[(index_80+1):maxIndex],];

#Generate estimations from training set

#Set number of neighbors
# Generate estimations for validation set.
num_Neighbors <- 100;
startIndex = 1;
numPredictions = 1000;
endIndex = startIndex + numPredictions - 1;
ans_col = ncol(data.bank);

estm <- smoothz(training_set,knnreg,num_Neighbors);
actual <- validation_set[startIndex:endIndex,ans_col];

predictions <- smoothzpred(validation_set[startIndex:endIndex,1:(ans_col-1)], training_set[,1:(ans_col-1)], estm);

#Round the predictions, we don't want continuous output.
predictions_rounded <- round(predictions)
# Since the output is an indicator variable, we calculate P(Successful Guess) as just the mean

pct_Success = mean(actual == predictions_rounded);
print(sprintf("Success Rate (%d Neighbors, %d Predictions): %f", num_Neighbors, numPredictions, pct_Success))

print(sprintf("Blind Guessing: %f", mean(actual==0)));
print(sprintf("Improvement: %f", (pct_Success-mean(actual==0))*100));
#Some Additional calculations to debug
numFalsePositives = sum( predictions_rounded[actual==0] == 1 );
numFalseNegatives = sum( predictions_rounded[actual==1] == 0 );
print(sprintf("False Positives: %f", numFalsePositives/length(predictions_rounded[actual==0])*100) );
print(sprintf("False Negatives: %f", numFalseNegatives/length(predictions_rounded[actual==1])*100) );
