convertDecDate <- function(decdate){
  year <- floor(decdate)
  decday <- decdate-year
  if(((year%%4)==0) & (year!=1900)){
    numdays=366
    months = c(31,60,91,121,152,182,213,244,274,305,335,366)
  } 
  else{
    numdays=365
    months = c(31,59,90,120,151,181,212,243,272,304,334,365)
  }
  day = decday*numdays + 0.5
  month=1
  while((month < 12) & (day>months[month])){
    month <- month+1
  }
  return(c(round(year,0),round(month,0),round(day,0)))
  #code used with data frames
  #> historic_temp$Year <- sapply(historic_temp$Date,FUN=convertDecDate)[1,]
  #> historic_temp$Month <- sapply(historic_temp$Date,FUN=convertDecDate)[2,]
}



 historic_temp$Year <- sapply(historic_temp$Date,FUN=convertDecDate)[1,]
 historic_temp$Month <- sapply(historic_temp$Date,FUN=convertDecDate)[2,]
 
 as.numeric(historic_temp$AverageCelsiusTemperature)

 Two01<-historic_temp[historic_temp$Year==2001 ,]
 Two002<-Two01[Two01$Month==7,]
 Two002final<-Two002[Two002$Country=='United States',]
 Nineteen50<-historic_temp[historic_temp$Year==1950,]
 Nineeteen502<-Nineteen40[Nineteen50$Month==7,]

 rest<-historic_temp[historic_temp$Year!=2001,]
 restfinal<-rest[rest$Month==7,]
 restfinal2<-restfinal[restfinal$Country=='United States',]
 
 all7<-historic_temp[historic_temp$Year,]
 all72<-all7[all7$Month==7,]
 all7final<-all72[all72$Country=='United States',]
 all7final2<-all72[all72$Country=='United States',]
 
 all7final2$MaxCelsiusTemp<-as.numeric(as.character(all7final$MaxCelsiusTemp))
 
 x<-as.numeric(Two002final$MaxCelsiusTemp)
 y<-as.numeric(restfinal2$MaxCelsiusTemp)
 
 x<-as.numeric(Two002$MaxCelsiusTemp)
 y<-as.numeric(Nineeteen502$MaxCelsiusTemp)
 
 
 Mean2001<-mean(x,na.rm=TRUE)
 Meanrest<-mean(y,na.rm=TRUE)
 
 HypothesisDiff<-Mean2001-Meanrest
 

 
 
 num2001<-length(x)
 numrest<-length(y)
 totalitems<-length(all7final2$MaxCelsiusTemp)
 
 permutationsample<-rep(0,totalitems)
 permutationsample[0:num2001]<-1
 permutationitself<-sample(permutationsample,length(permutationsample),FALSE)
 View(permutationitself)
 
 insamplemean<-mean(all7final2[permutationitself==1,]$MaxCelsiusTemp)
 outsamplemean<-mean(all7final2[permutationitself==0,]$MaxCelsiusTemp)
 sampledifference<-insamplemean-outsamplemean
 
 p<-Permute_samples <- function(p,df,X){
  
   sample(p,length(p),FALSE)
   in_sample_mean <- mean(df[p==1,X])
   out_sample_mean <- mean(df[p==0,X])
   
   
   return(insamplemean - outsamplemean)
 }
 
 samples<-sampledifference
 
 
 Permute_samples(permutationsample,all7final2,"MaxCelsiusTemp")
 
 
 
 samples<- replicate(2000,Permute_samples(permutationsample))
 
 sd(samples)
 
 
 #p-value test only code below 
 
 sd2001<-sd(x,na.rm=TRUE)
 sd1950<-sd(y,na.rm=TRUE)
 
 
 num2001<-length(x)
 num1950<-length(y)
 
 
 sdivisor<-sqrt(sd2001^2/num2001+sd1950^2/num1950)
 zscore<-(Mean2001-Mean1950)/sdivisor
 pvalfinal<-1-pnorm(zscore)
 plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
 abline(v=zscore,col='red')
 
 
 