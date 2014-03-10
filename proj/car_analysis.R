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
    labels <- levels(factor(df$num.doors))
  } else if (cur_head == "drive.wheels") {
    labels <- levels(factor(df$num.doors))
  } else if (cur_head == "engine.location") {
    labels <- levels(factor(df$engine.location))
  } else if (cur_head == "engine.type") {
    labels <- levels(factor(df$engine.type))
  } else if (cur_head == "num.cylinders") {
    labels <- levels(factor(df$num.cylinders))
  } else if (cur_head == "fuel.system") {
    labels <- levels(factor(df$fuel.system))
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
k<-0.005
parsimony <- prsm(new_df$price, subset(new_df, select=-c(price)), k, crit="max", printdel=T)
print(parsimony)

fitPrice <- lm(new_df$price ~. , data=new_df[,2:66])
summary(fitPrice)
#fitPrice2 <- lm(price ~ audi + bmw + dodge + mitsubishi + plymouth + porsche + saab + std + front + wheel.base + length + width + height + curb.weight + dohc + ohc + engine.size + peak.rpm, data=new_df)

k<-0.01
parsimony <- prsm(new_df$price, subset(new_df, select=-c(price)), k, crit="max", printdel=T)
print(parsimony)

k<-0.05
parsimony <- prsm(new_df$price, subset(new_df, select=-c(price)), k, crit="max", printdel=T)
print(parsimony)