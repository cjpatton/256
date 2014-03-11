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

fitInfant <- lm(is.infant ~ ., data=new_df)
summary(fitInfant)

fitInfantFew <- lm(is.infant ~ is.male + length + viscerra.weight + rings , data=new_df)
summary(fitInfantFew)
