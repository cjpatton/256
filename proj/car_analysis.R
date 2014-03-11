# Load Raw Automobile Data Set
df <- read.csv("imports-85.csv", header=T)
# Some housecleaning on data set
new_mat <- df[[1]] #will be concatenating with cbind
cur_headers <- names(df)
new_heads <- cur_headers[1] #will be concatenating as new_heads <- c(new_heads, new_header)

for (i in (2 : length(cur_headers))){
  cur_head <- cur_headers[i]
  ### if the header is in the factors - get header index
  ### else save the header and move whole column
  createNewCols = TRUE
  if (cur_head == "make") {
    labels <- levels(factor(df$make))
  } else if (cur_head == "fuel.type") {
    labels <- levels(factor(df$fuel.type))
  } else if (cur_head == "aspiration") {
    labels <- levels(factor(df$aspiration))
  } else if (cur_head == "num.doors") {
    labels <- levels(factor(df$num.doors))
  } else if (cur_head == "body.style") {
    labels <- levels(factor(df$body.style))
  } else if (cur_head == "drive.wheels") {
    labels <- levels(factor(df$drive.wheels))
  } else if (cur_head == "engine.location") {
    labels <- levels(factor(df$engine.location))
  } else if (cur_head == "engine.type") {
    labels <- levels(factor(df$engine.type))
  } else if (cur_head == "num.cylinders") {
    labels <- levels(factor(df$num.cylinders))
  } else if (cur_head == "fuel.system") {
    labels <- levels(factor(df$fuel.system))
  } else if (cur_head == "safety.rating") {  
    #0 for unsafe, 1 for safe
    createNewCols=FALSE
    new_heads <- c(new_heads, cur_head)
    newCol <- rep(0,nrow(df))
    ones<-(df[i]>0)
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

#################

# Parsimony: Car Price

parsimony <- prsm(y=new_df$price, x=subset(new_df, select=-c(price)), k=0.01, predacc=ar2, crit="max", printdel=T)
print(parsimony)

parsimony <- prsm(new_df$price, subset(new_df, select=-c(price)), k = 0.01, crit="max", printdel=T)
print(parsimony)

parsimony <- prsm(new_df$price, subset(new_df, select=-c(price)), k = 0.05, crit="max", printdel=T)
print(parsimony)

#Find significant predictors
fitPrice <- lm(price ~. , data=new_df)
summary(fitPrice)

# Re-do, with only significant predictors.
fitPrice <- lm(price ~ bmw + dodge + `mercedes-benz` + mitsubishi + plymouth + porsche + saab + std + front + wheel.base + length + width + height + curb.weight + dohc + ohc + engine.size + peak.rpm, data=new_df)
summary(fitPrice)


# Logit case

parsimony <- prsm(new_df$safety.rating, subset(new_df, select=-c(safety.rating)), predacc=aiclogit, crit="min", k=0.01, printdel=T)
print(parsimony)

parsimony <- prsm(new_df$safety.rating, subset(new_df, select=-c(safety.rating)), predacc=aiclogit, crit="min", k=0.05, printdel=T)
print(parsimony)

fitSafety <- lm(safety.rating ~. , data = new_df)
summary(fitSafety)

fitSafety <- lm(safety.rating ~ audi + saab + volkswagen + diesel + std + `four-doors` + `4wd` + fwd + `1bbl`, data=new_df)
summary(fitSafety)
