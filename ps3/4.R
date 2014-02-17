#install.packages("ggplot2")
#install.packages("mixtools")
library(mixtools)
library(ggplot2)

#4.a ##############################
p<-ggplot(data.frame(faithful))
p+geom_density(aes(x=faithful$waiting))

#4.b ##############################
simulateFromDist <- function(n,p1,m1,s1,m2,s2){
	k1 <- p1*n #proportion of type 1
	k2 <- n-k1+1 #proportion of type 2
	x1 <- rnorm(k1, mean=m1, sd=s1)
	x2 <- rnorm(k2, mean=m2, sd=s2)
	c(x1,x2) #order of events doesn't matter for histogram, so simply concatenate
}

####from mixtools simulation
mixout<-normalmixEM(faithful$waiting,lambda=0.5,mu=c(55,80),sigma=10,k=2)
str(mixout)
# $ lambda    : num [1:2] 0.361 0.639
# $ mu        : num [1:2] 54.6 80.1
# $ sigma     : num [1:2] 5.87 5.87


# Is it necessary to simulate this? Can we plot the function directly?
sim_waiting<-simulateFromDist(length(faithful$waiting),0.361,54.6,5.87,80.1,5.87)

data <- rbind( data.frame(type="non-parametrical", lr=faithful$waiting), data.frame(type="parametrical", lr=sim_waiting))
m <- ggplot(data, aes(x=lr)) 
m <- m + geom_density(aes(fill=factor(type)), size=2, alpha=.4) 

#save m
pdf('plot4b.pdf')
m
dev.off()

#4.f  #####################

EL2 <- ( mixout$sigma[1]^2 * mixout$lambda[1] + mixout$sigma[2]^2 * mixout$lambda[2] ) + (mixout$mu[1]^2 * mixout$lambda[1] + mixout$mu[2]^2 * mixout$lambda[2])
EL <- mixout$mu[1] * mixout$lambda[1] + mixout$mu[2] * mixout$lambda[2]
