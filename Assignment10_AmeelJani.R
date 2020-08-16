


citybike_train$diff=abs(citybike_train$end.station.id-citybike_train$start.station.id)

install.packages('caret')
library(caret)

set.seed(1234)

caret.control <- trainControl(method="repeatedcv", number=10, repeats=3) 

Quadlm<-lm(tripduration~I(diff^2)+diff,data=citybike_train)


summary(Quadlm)


citybike_test10000A$diff=citybike_test10000A$end.station.id-citybike_test10000A$start.station.id

predictions<-predict(Quadlm,citybike_test10000A)

DMwR::regr.eval(predictions,citybike_test10000Awithduration$tripduration)