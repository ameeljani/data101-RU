citybike_train$starttime2<-as.character(citybike_train$starttime)

citybike_train$Day<-substring(citybike_train$starttime2,9,10)

citybike_train$Day2<-as.numeric(citybike_train$Day)

train<-merge(citybike_train,temp,by=c("Day2"),all=TRUE)

#Quadlm<-lm(tripduration~Temp,data=train)

#summary(Quadlm)

install.packages("caret")
library(caret)
caret.control <- trainControl(method="repeatedcv", number=10,repeated=3) 

model1<-train(tripduration~Temp,data=train,method="lm",trControl=caret.control)

citybike_test10000A$starttime2<-as.character(citybike_test10000A$starttime)

citybike_test10000A$Day<-substring(citybike_test10000A$starttime2,9,10)

citybike_test10000A$Day2<-as.numeric(citybike_test10000A$Day)

test1<-merge(citybike_test10000A,temp,by=c("Day2"),all=TRUE)


predictions1<-predict(model1,test1)

citybike_test10000Awithduration$starttime2<-as.character(citybike_test10000Awithduration$starttime)

citybike_test10000Awithduration$Day<-substring(citybike_test10000Awithduration$starttime2,9,10)

citybike_test10000Awithduration$Day2<-as.numeric(citybike_test10000Awithduration$Day)

error1A<-merge(citybike_test10000Awithduration,temp,by=c("Day2"),all=TRUE)

DMwR::regr.eval(predictions1,error1A$tripduration)

write.table(predictions1,file="prediction1_10000A.csv",sep=",")


citybike_test10000B$starttime2<-as.character(citybike_test10000B$starttime)

citybike_test10000B$Day<-substring(citybike_test10000B$starttime2,9,10)

citybike_test10000B$Day2<-as.numeric(citybike_test10000B$Day)

test2<-merge(citybike_test10000B,temp,by=c("Day2"),all=TRUE)


predictions1B<-predict(model1,test2)
TL
write.table(predictions1,file="prediction1_10000B.csv",sep=",")

# NOTE: MY PREDICTIONS ARE OUT OF ORDER; THEY ARE ORDERED BY DAY; PLEASE USE THE ABOVE CODE TO CORRECTLY COMPARE MY RESULTS WITH THE ACTUAL RESULTS OF SET 10000B, THANKS!
#10000B ERROR CODE: 

#citybike_test10000Bwithduration$starttime2<-as.character(citybike_test10000Bwithduration$starttime)

#citybike_test10000Bwithduration$Day<-substring(citybike_test10000Bwithduration$starttime2,9,10)

#citybike_test10000Bwithduration$Day2<-as.numeric(citybike_test10000Bwithduration$Day)

#error1A<-merge(citybike_test10000Bwithduration,temp,by=c("Day2"),all=TRUE)

#DMwR::regr.eval(predictions1,error1B$tripduration)






caret.control <- trainControl(method="repeatedcv", number=10,repeated=3) 

model2<-train(tripduration~I(start.station.id^2)+start.station.id+I(end.station.id^2)+end.station.id,data=train,method="lm",trControl=caret.control)



predictions2<-predict(model2,citybike_test10000A)


DMwR::regr.eval(predictions2,citybike_test10000Awithduration$tripduration)

write.table(predictions2,file="prediction2_10000A.csv",sep=",")


predictions2B<-predict(model2,citybike_test10000B)

write.table(predictions2B,file="prediction2_10000Bfinal.csv",sep=",")























