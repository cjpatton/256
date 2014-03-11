new_df <- read.csv("abalone.csv", header=T)

x<- names(new_df)
new.headers <- gsub("-",".",x)
names(new_df) <- new.headers

x<- names(new_df)
new.headers <- gsub("_",".",x)
names(new_df) <- new.headers

k<-0.01
parsimony <- prsm(new_df$is.infant, subset(new_df, select=-c(is.infant)), predacc=aiclogit, crit="min", k, printdel=T)
print(parsimony)

k<-0.05
parsimony <- prsm(new_df$is.infant, subset(new_df, select=-c(is.infant)), predacc=aiclogit, crit="min", k, printdel=T)
print(parsimony)

fitInfant <- glm(is.infant ~ ., family=binomial, data=new_df)
summary(fitInfant)

fitInfantFew <- glm(is.infant ~ length + diameter + whole.weight + viscerra.weight + rings, family=binomial, data=new_df)
summary(fitInfantFew)


k<-0.01
parsimony <- prsm(new_df$shell.weight, subset(new_df, select=-c(shell.weight)), predacc=ar2, crit="max", k, printdel=T)
print(parsimony)

k<-0.05
parsimony <- prsm(new_df$shell.weight, subset(new_df, select=-c(shell.weight)), predacc=ar2, crit="max", k, printdel=T)
print(parsimony)

fitShellWeight <- lm(shell.weight ~ ., data=new_df)
summary(fitShellWeight)

fitShellWeightFew <- lm(shell.weight ~ diameter + height + whole.weight + shucked.weight + viscerra.weight + rings, data=new_df)
summary(fitShellWeightFew)

