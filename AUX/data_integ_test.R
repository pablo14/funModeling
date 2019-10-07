#library(funModeling)
data=heart_disease
data$has_heart_disease2=ifelse(data$has_heart_disease==1,T, F)
data$fecha=Sys.Date()
data$fecha2=as.POSIXct(Sys.Date())
data$gender=as.character(data$gender)
data$gender[1]=NA
data$fecha[1]=Sys.Date()+1
data$fecha2[2]=as.POSIXct(Sys.Date())+1
data$max_heart_rate=as.character(data$max_heart_rate)
data$constant=999
data$id=as.character(seq(1:nrow(data)))

write_delim(data, "messy_data.txt", delim = ';')

#
#library(stringr)

status(data)
di=data_integrity(data)
summary(di)

di$results$vars_other


## all ok:
library(tidyverse)
o5=data_integrity(dplyr::select(data_country, -country));summary(o5)
print(o5)


##############


o4=data_integrity(data);summary(o4)
print(o4)
reprex()


o1=data_integrity_model(data, 'xgboost')
print(o1)

o1$model_selected

o1=data_integrity_model(data, 'xgboost', MAX_UNIQUE = 100);print(o1)
o2=data_integrity_model(data, 'randomForest');print(o2)
o3=data_integrity_model(data, 'no_na');print(o3)
ibrary(tidyverse)

