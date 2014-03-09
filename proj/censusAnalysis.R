
# Census dataset - make sure all headers are present. otherwise complains about row.names.. 
df <- read.csv("census.csv", header=T)

#need to switch string values to numbers. Use following website suggestions
#http://www.ats.ucla.edu/stat/stata/faq/destring.htm
# bash commands:
# wc -l census.csv returns 32562 lines including header
# tail -32561 census.csv | cut -f2 -d"," | sort | uniq
# gives the following categories:
# ?
# Federal-gov
# Local-gov
# Never-worked
# Private
# Self-emp-inc
# Self-emp-not-inc
# State-gov
# Without-pay
#mat <- matrix(0, nrow=2, ncol=5)
#vec <- c("x","y")
#data.frame(vec, mat)


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
		ones<-(df[index]=="Female")
		newCol[ones]=1	
		###place into new matrix
		new_mat<-cbind(new_mat,newCol)
	} else if (cur_head == "native_country") {
		labels <- levels(factor(df$native_country))
	} else if (cur_head == "salary") {
		labels <- levels(factor(df$salary))
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
			ones<-(df[index]==label)
			newCol[ones]=1	
			###place into new matrix
			new_mat<-cbind(new_mat,newCol)
			
		}
	}

}#end of loop over old headers

new_df<-data.frame(new_mat)
names(new_df)<-new_heads

#################
k<-0.1
parsimony <- prsm(new_df$sex, subset(new_df, select=-c(sex)), predacc=aiclogit, k)
print(parsimony)