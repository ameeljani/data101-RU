

citybike_train$Decade<-citybike_train$birth.year%/%10


#actual prediction model 

Quadlm<-lm(tripduration~I(Decade^2)+Decade+I(start.station.latitude^2)+start.station.latitude,data=citybike_train)


summary(Quadlm)

citybike_test10000A$Decade<-citybike_test10000A$birth.year%/%10

#error to compare to 
bikeid.lm.model <- lm(tripduration~bikeid, data=citybike_train)

bikeid.lm.predictions <- predict(bikeid.lm.model,citybike_test10000A)

DMwR::regr.eval(bikeid.lm.predictions,citybike_test10000Awithduration$tripduration)




#predictions10000Aset

decadeandlatitudepredictions<-predict(Quadlm,citybike_test10000A)

View(decadeandlatitudepredictions)

#error_on_10000A_set

install.packages('DMwR')

DMwR::regr.eval(decadeandlatitudepredictions,citybike_test10000Awithduration$tripduration)

#error: RMSE is 1.329384e+04

#predictions10000Bset
citybike_test10000B$Decade<-citybike_test10000B$birth.year%/%10
predictionBset<-predict(Quadlm,citybike_test10000B)

View(predictionBset)

write.table(predictionBset,file="predictionsB.csv",sep=",")

write.table(decadeandlatitudepredictions,file="predictionsA.csv",sep=",")

