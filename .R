library(pscl)
library (ggplot2)

##### Load the votes matrix (change the path accordingly in line 6) ##############

store<- read.csv("/Users/ah35/Google Drive/El Salvador roll calls/Data/Legislature 2012-2015/rollcalls_legislature2.csv")
store<- store[,-1]
store[ store == 9 ] <- NA

#### Dropping
#### Uncontested
#### Votes >2.5%
rollcall1<-data.frame(t(store[c(-1, -2)]))

yea<-nay<-c()

for(i in 1:length(rollcall1[,1])){
  
  yea[i]<-sum(as.numeric(rollcall1[i,1:length(rollcall1[1,])]), na.rm=T)
  nay[i]<-sum(as.numeric(rollcall1[i,1:length(rollcall1[1,])]==0), na.rm=T)
  
}

uncontested<-ifelse(yea<(yea+nay)*.025 | nay<(yea+nay)*.025, yes=0, no=1)
uncontested<-cbind(uncontested, rollcall1)
uncontested<-subset(uncontested, uncontested==1)
uncontested<- uncontested[!names(uncontested) %in% c("uncontested")]

uncontested<-data.frame(t(uncontested))

rollcall<-data.frame(cbind(store[c(1,2)], uncontested), row.names=NULL); rm(rollcall1)

######
#### Ideal-Points Estimation
######

###### Legislature 2012-2015 ####
store<- store [,-1]

store[ store == 9 ] <- NA
house.roll<-rollcall(data=store[,3:826], legis.names=store$legislator)

#### Exclude legislators with less than 25% votes ###

yea<-nay<-c()

for(i in 1:length(rollcall[,1])){
  
  yea[i]<-sum(as.numeric(rollcall[i,3:length(rollcall[1,])])==1, na.rm=T)
  nay[i]<-sum(as.numeric(rollcall[i,3:length(rollcall[1,])]==0), na.rm=T)
  
}

votes<-cbind(subset(store, select=c(legislator, party)), vote.count= yea+nay)

### Legislature 2012-2015
votes <- cbind(subset(votes, vote.count >= 13, 
                  select=c(legislator, party)))

##### Getting eigenvalues and variance of each dimension for the legislature ###
library("wnominate")
ideal<- wnominate(house.roll, trials=3, polarity=c(1,1), dims=2)
plot.scree(ideal)
veigen<- cbind(1:length(ideal$eigenvalues), ideal$eigenvalues)
veigen<-data.frame(veigen)
ggplot(veigen, aes(X1, X2))+geom_point() +geom_line()+
  geom_hline(yintercept=1)+ ylab("Eigenvalue")+ xlab("Number of Dimensions")+ ggtitle("12-15 Legislative Period")

ggsave("./images/eigenvalues_0813.pdf")

#### SHORT RUN Legislature 2012-2015
ideal.house<-ideal(house.roll, d=1, maxiter = 100, thin = 10, burnin = 10, verbose = TRUE)
plot(ideal.house)

#### FULL Run
ideal.house <- ideal(house.roll, d=1, maxiter = 1000000, thin = 1000, burnin = 100000, verbose = TRUE)

#### Saving Data in R format
setwd("/Users/ah35/Google Drive/El Salvador roll calls/Data/Legislature 2012-2015/ID Point Estimation")

######
#### Ideal Points: Store 2012-2015
######

rollcall.col<- votes[,c(1,2)]

id.points<-cbind(rollcall.col,
                 as.matrix(summary(ideal.house)[[c("xm")]]),
                 as.matrix(summary(ideal.house)[[c("xsd")]]),
                 matrix(summary(ideal.house)[[c("xHDR")]],ncol=2))

colnames(id.points)<-c("legislator", "party", "IP.mean","SD","CI.lower","CI.upper")


write.csv(id.points, file="/Users/ah35/Google Drive/El Salvador roll calls/Data/Legislature 2012-2015/ID Point Estimation/id.points.csv", na="", row.names=FALSE) 
write.csv(rollcall.votes, file="/Users/ah35/Google Drive/El Salvador roll calls/Data/Legislature 2012-2015/ID Point Estimation/votesestimation.csv")

