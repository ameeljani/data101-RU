

#TEST 1
recent<-FinalProjectData[142:151,]

notrecent<-FinalProjectData[1:142,]

year10m<-mean(recent$Total)

year10sd<-sd(recent$Total)

year10length<-length(recent$Total)

allm<-mean(FinalProjectData$Total)

allsd<-sd(FinalProjectData$Total)


recent$Year2<-as.character(recent$Year)

tscore1<-(year10m-allm)/(year10sd/sqrt(year10length))

criticalt1<-qt(1-0.05,10-1)

pt1<-1-pt(tscore1,9)

#NOT SIGNIFICANT-BOTH TESTS

#TEST 2
pre45<-FinalProjectData[1:76,]
  
post45<-FinalProjectData[77:151,]

pre45mean<-mean(pre45$Total)
  
post45mean<-mean(post45$Total)


pre45num<-length(pre45$Total)

post45num<-length(post45$Total)


pre45sd<-sd(pre45$Total)

post45sd<-sd(post45$Total)

sdivisor1<-sqrt(pre45sd^2/pre45num+post45sd^2/post45num)

zscore2<-(pre45mean-post45mean)/sdivisor1

p2<-1-pnorm(zscore2)

#NOT SIGNIFICANT

# TEST 3
total90to20<-FinalProjectData[121:151,]

total59to89<-FinalProjectData[90:120,]

numlatter<-length(total90to20$Total)

numformer<-length(total59to89$Total)

meanlatter<-mean(total90to20$Total)

meanformer<-mean(total59to89$Total)

sdlatter<-sd(total90to20$Total)
  
sdformer<-sd(total59to89$Total)

divisor2<-sqrt(sdformer^2/numformer+sdlatter^2/numlatter)

zscore3<-(meanlatter-meanformer)/divisor2

p3<-1-pnorm(zscore3)

#NOT SIGNIFICANT 

#TEST 4
recent10<-FinalProjectData[143:151,]

dec10m<-mean(recent10$December)

dec10sd<-sd(recent10$December)

dec10length<-length(recent10$December)

alldecm<-mean(FinalProjectData$December)

alldecsd<-sd(FinalProjectData$December) 

tscore2<-(alldecm-dec10m)/(dec10sd/sqrt(dec10length))

criticalt1<-qt(1-0.05,9-1)

pt2<-1-pt(tscore2,8)

#SIGNIFICANT-2011-2019 values significantly less (slight example of p hacking too)

#VISUALIZATION


install.packages('ggplot2')
library(ggplot2)


ggplot(FinalProjectData, aes(x=Year, y=Total)) + geom_line() + geom_point()

ggplot(FinalProjectData, aes(x=Year, y=December)) + geom_line() + geom_point()

ggplot(FinalProjectData, aes(x=December, y=Total)) + geom_line() + geom_point()

#Muddled effect for all 3 plots, though the December and Total shows a slight upward trend 











