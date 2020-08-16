subs<-subset(X50KJan20NYCCitybike,X50KJan20NYCCitybike$usertype=="Subscriber")
View(subs)
customers<-subset(X50KJan20NYCCitybike,X50KJan20NYCCitybike$usertype=="Customer")
View(customers)
subsmean<-mean(subs$tripduration)
customersmean<-mean(customers$tripduration)
sdsubs<-sd(subs$tripduration)
View (subs$tripduration)
sdcustomer<-sd(customers$tripduration)

subsnum<-length(subs$tripduration)
customersnum<-length(customers$tripduration)
sdivisor<-sqrt(sdsubs^2/subsnum+sdcustomer^2/customersnum)
zscore<-(customersmean-subsmean)/sdivisor
pval<-1-pnorm(zscore)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=zscore,col='red')


One<-subset(X50KJan20NYCCitybike,X50KJan20NYCCitybike$gender==1)
View(One)
Two<-subset(X50KJan20NYCCitybike,X50KJan20NYCCitybike$gender==2)
View(Two)
Onemean<-mean(One$tripduration)
Twomean<-mean(Two$tripduration)
sdOne<-sd(One$tripduration)
sdTwo<-sd(Two$tripduration)
numOne<-length(One$tripduration)
numTwo<-length(Two$tripduration)
divisor<-sqrt(sdOne^2/numOne+sdTwo^2/numTwo)
zscore2<-(Twomean-Onemean)/divisor
pval2<-1-pnorm(zscore2)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=zscore2,col='red')




t <- data.frame(x=c(1,2,3),y=c(3,2,1),z=c("a","b","c"))
t["x",]
t[1,]
t[,2:1]






tapply(survey$ID,survey$CellPhoneType,length)
prior<-121/(52+121)
tapply(survey$ID,survey$ChocolateOrVanilla,length)
PE<-91/(91+82)
iphone<-subset(survey,survey$CellPhoneType=="iPhone")
tapply(iphone$ID,iphone$ChocolateOrVanilla,length)
PEH<-68/(53+68)

post<-PEH*prior/PE




critical<-qt(1-0.05/2,df=27)






