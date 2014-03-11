test_shuttle = function(x, reg){
  
  coeffs = as.numeric((reg$coeff))
  namelist = as.character(names(reg$coeff))
  inboundt = 0
  inboundf = 0
  outt = 0
  outf = 0
  class1vals=c()
  falseposvals=c()
  negativevals=c()
  for (i in 1:length(x[,1])){
    num = coeffs[1];
    val = 0
    for (j in 2:length(coeffs)){
      num = num + coeffs[j]*x[i,namelist[j]]
    }
    val = 1/(1+exp(-1*num))
    
    if(val >0.5 ){
      if(x[i,'Class'] == '1') { inboundt = inboundt +1; class1vals=c(class1vals,val) }
      else { inboundf = inboundf + 1; falseposvals = c(falseposvals,val) }
    }
    else if(x[i,'Class'] == '1'){ outt = outt+ 1;class1vals=c(class1vals,val) }
    else { outf = outf + 1 ; negativevals=c(negativevals,val) }
    

  }
  totalin = inboundt + inboundf
  totalout = outt+ outf
  total = totalin+totalout
  totalt = inboundt+outt
  cat("%InBound:",inboundt/totalin,'\n')
  cat("%Out Correct:",outf/totalout,'\n')
  cat("%Class Correct: ",inboundt/totalt,'\n')
  cat("%Not Class Correct: ", outf/(outf+inboundf))  
}


#example use with parsimony:
df = read.csv('shuttle.trn',header = FALSE, sep=' ')
df['class'] = ifelse(df[,10]==1,1,0)
col1 = prsm(df[,'class'],df[,1:9],predacc = aiclogit, k = 0.01, crit ='min', printdel='T')
col2 = prsm(df[,'class'],df[,1:9],predacc = aiclogit, k = 0.05, crit ='min', printdel='T')

logr = glm(df['class'] ~ ., data = df[,col1], family=binomial)
logr2 = glm(df['class'] ~. , data = df[,col2], family = binomial)
colsig = c('V1','V2','V3','V5','V6','V7','V8','V9')
sigr = glm(df['class'] ~. , data = df[,sigr], family = binomial)

val = read.csv('shuttle.tst',header = FALSE, sep=' ')
test_shuttle(val, logr)
test_shuttle(val, logr2)
test_shuttle(val, sigr)
