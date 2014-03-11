
# Census dataset - make sure all headers are present. otherwise complains about row.names.. 
df <- read.csv("census.csv", header=T)

#need to switch string values to numbers. Use following website suggestions
#http://www.ats.ucla.edu/stat/stata/faq/destring.htm

### the code below create a new data frame new_df that has only numberical values and 110 total columns

###init new data matrix for new data frame
new_mat <- df[[1]] #will be concatenating with cbind
cur_headers <- names(df)
new_heads <- cur_headers[1] #will be concatenating as new_heads <- c(new_heads, new_header)

for (i in (2 : length(cur_headers))){
	cur_head <- cur_headers[i]
	### if the header is in the factors - get header index
	### else save the header and move whole column
	createNewCols = TRUE
	if (cur_head == "workclass") {
		labels <- levels(factor(df$workclass))
	} else if (cur_head == "education") {
		labels <- levels(factor(df$education))
	} else if (cur_head == "marital_status") {
		labels <- levels(factor(df$marital_status))
	} else if (cur_head == "occupation") {
		labels <- levels(factor(df$occupation))
	} else if (cur_head == "relationship") {
		labels <- levels(factor(df$relationship))
	} else if (cur_head == "race") {
		labels <- levels(factor(df$race))
	} else if (cur_head == "sex") {
		#0 for male, 1 for female
		createNewCols=FALSE
		new_heads <- c(new_heads, cur_head)
		newCol <- rep(0,nrow(df))
		ones<-(df[i]=="Female")
		newCol[ones]=1	
		###place into new matrix
		new_mat<-cbind(new_mat,newCol)
	} else if (cur_head == "native_country") {
		labels <- levels(factor(df$native_country))
	} else if (cur_head == "salary") {
		#0 for <=50k, 1 for >50k
		createNewCols=FALSE
		new_heads <- c(new_heads, cur_head)
		newCol <- rep(0,nrow(df))
		ones<-(df[i]==">50K")
		newCol[ones]=1	
		###place into new matrix
		new_mat<-cbind(new_mat,newCol)
	} else { #integer column
		createNewCols=FALSE
		new_heads <- c(new_heads, cur_head)
		new_mat<-cbind(new_mat,df[i])
	}

	if (createNewCols){
		for (label in labels){

			### save header into new header vector
			new_heads <- c(new_heads, label)


			newCol <- rep(0,nrow(df))
			ones<-(df[i]==label)
			newCol[ones]=1	
			###place into new matrix
			new_mat<-cbind(new_mat,newCol)
			
		}
	}

}#end of loop over old headers

new_df<-data.frame(new_mat)
names(new_df)<-new_heads

x<- names(new_df)
new.headers <- gsub("-",".",x)
names(new_df) <- new.headers

x<- names(new_df)
new.headers <- gsub("_",".",x)
names(new_df) <- new.headers

####### Use with various K for analysis ##########

## logistic
use.k<-0.01
parsimony <- prsm(new_df$sex, subset(new_df, select=-c(sex)), predacc=aiclogit, k=use.k, printdel=T)
print(parsimony)
use.k<-0.05
parsimony <- prsm(new_df$sex, subset(new_df, select=-c(sex)), predacc=aiclogit, k=use.k, printdel=T)
print(parsimony)
use.k<-0.01
parsimony <- prsm(new_df$salary, subset(new_df, select=-c(salary)), predacc=aiclogit, k=use.k, printdel=T)
print(parsimony)
use.k<-0.05
parsimony <- prsm(new_df$salary, subset(new_df, select=-c(salary)), predacc=aiclogit, k=use.k, printdel=T)
print(parsimony)

## continuous
use.k<-0.01
parsimony <- prsm(new_df$age, subset(new_df, select=-c(age)), crit="max", k=use.k, printdel=T)
print(parsimony)
use.k<-0.05
parsimony <- prsm(new_df$age, subset(new_df, select=-c(age)), crit="max", k=use.k, printdel=T)
print(parsimony)


## logisic
fitSex <- glm(sex ~ Widowed + Craft.repair + Farming.fishing + Handlers.cleaners + Transport.moving + Not.in.family + Other.relative + Own.child + Unmarried + Wife, family=binomial, data=new_df)
summary(fitSex)

fitSalary <- glm(salary ~ capital.gain, family=binomial, data=new_df)
summary(fitSalary)

## continuous
fitSalaryMore <- glm(salary ~ age + education.num + Exec.managerial + Not.in.family + Own.child + Unmarried + Wife + capital.gain + capital.loss + hours.per.week, family=binomial, data = new_df)
summary(fitSalaryMore)


fitSexAll <- glm(sex ~ ., family=binomial, data=new_df)
summary(fitSex)

fitSalaryAll <- glm(salary ~ ., family=binomial, data=new_df)
summary(fitSalary)


fitAge <- lm(age ~ Self.emp.not.inc + Assoc.acdm + Never.married + Widowed + Own.child + hours.per.week + salary, data = new_df)
summary(fitAge)
plot(fitAge)

fitAgeAll <- lm(age ~ ., data = new_df)
summary(fitAge)
plot(fitAge)

test.set.index = sample(c(1:dim(new_df)[1]),dim(new_df)[1]*.1)

data.test = new_df[test.set.index,]
data.train = new_df[-test.set.index,]


SS.fit.all = lm(age ~ Self.emp.not.inc + Assoc.acdm + Never.married + Widowed + Own.child + hours.per.week + salary, data=data.train)
predicted.values.SS.fit.all = predict(SS.fit.all,newdata=data.test)
mean((predicted.values.SS.fit.all-data.test$age)^2)

plot(data.test$age ~ predicted.values.SS.fit.all, xlab="predicted age", ylab="actual age", main="predicted vs actual age of 15% test")
abline(0,1)

### plot of variables maximally contributing to variation of y-value

contribution <- function(y, x, listOfCols, predacc=ar2, printdel=F) 
{
  if (is.matrix(x))
  {
    x <- data.frame(x)
  }
  orig_cols <- listOfCols
  cols <- orig_cols 
  pac <- predacc(y, x)
  if (printdel) 
  {
    cat("full outcome = ", pac, "\n")
  }


  prev_outcome = pac
  results = c()
  headers = c()
  for (col in listOfCols)
  {
  	if (col!=listOfCols[length(listOfCols)]){
	    new_cols <- setdiff(cols, col)
	    new_pac <- predacc(y, subset(x, select=new_cols))

	      cols <- new_cols
	      pac <- new_pac
	      if (printdel)
	      {
	        cat("deleted        ", col, "\n")
	        cat("new outcome  = ", pac, "\n")
	        contrib = prev_outcome-pac
	        results = c(results, contrib)
	        headers = c(headers,col)
	        cat("contribution = ", contrib, "\n")
	        prev_outcome=pac
	      }
	  }
  }

  par(mar=c(7.1,4.1,4.1,2.1))
  barplot(results, ylab = "contribution to age", space=0, main="PAC delta contributions")
  axis(1, las=2, at=(1:length(results)-0.5), labels=headers)
}

#order picked on the basis of correlation between each explanatory variable and Y (sorted by magnitude of correlation)
listOfCols <- c( "Never.married",  "Own.child", "Widowed",  "salary", "Self.emp.not.inc", "hours.per.week",  "Assoc.acdm", "Thailand")

contribution(new_df$age, subset(new_df, select=-c(age)), listOfCols, printdel=T)


### Splitting into training and test sets ###

## printing prediction plots, or residuals plots and the error

### various k ###

### train, validation, test

### eliminating by column and seeing how much they contribute.

