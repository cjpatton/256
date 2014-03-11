df = read.csv('cdata.csv')
col1 = prsm(df[,1],df[,2:9],k=0.01, predacc=ar2, crit='max', printdel=T)
col2 = prsm(df[,1],df[,2:9],k=0.05, predacc=ar2, crit='max', printdel=T)

#regression with all the predictors
logr = lm(df[,1]~.,data = df[,2:9])
summary(logr)

#regression with predictors selected by parsimony
logr = lm(df[,1]~., data= df[,col1])
summary(logr)

#leave out lat & lon
col = ('Median.Income','Median.Age','Population','Households')
logr = lm(df[,1]~., data= df[,col])
summary(logr)