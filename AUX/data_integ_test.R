


library(funModeling)
data=heart_disease
data$has_heart_disease2=ifelse(data$has_heart_disease==1,T, F)
data$fecha=Sys.Date()
data$fecha2=as.POSIXct(Sys.Date())
data$gender=as.character(data$gender)
data$fecha[1]=Sys.Date()+1
data$fecha2[2]=as.POSIXct(Sys.Date())+1
data$max_heart_rate=as.character(data$max_heart_rate)


status(data)
data_integrity(data)

o1=data_integrity_model(data, 'xgboost')
print(o1)

o1$model_selected

o1=data_integrity_model(data, 'xgboost', MAX_UNIQUE = 100);print(o1)
o2=data_integrity_model(data, 'randomForest');print(o2)
o3=data_integrity_model(data, 'no_na');print(o3)


o4=data_integrity(data);summary.integrity(o4)
print(o4)

o5=data_integrity(select(data_country, -country));summary(o5)
print(o5)

library(tidyverse)

