## normal ##########################################
cross_plot(data=heart_disease, str_input="chest_pain", str_target="has_heart_disease")

## forcing NA in target ##########################################
heart_disease$has_heart_disease[1]=NA
cross_plot(data=heart_disease, str_input="chest_pain", str_target="has_heart_disease")

## forcing 3 values in target ##########################################
heart_disease$has_heart_disease=as.character(heart_disease$has_heart_disease)
heart_disease$has_heart_disease2=heart_disease$has_heart_disease
heart_disease$has_heart_disease2[1]="hello_world"
cross_plot(data=heart_disease, str_input="chest_pain", str_target="has_heart_disease2")

## target as numeric ##########################################
heart_disease$has_heart_disease_num=ifelse(heart_disease$has_heart_disease=="yes", 1, 0)
library(Hmisc)
describe(heart_disease$has_heart_disease_num)
cross_plot(data=heart_disease, str_input="chest_pain", str_target="has_heart_disease_num")


## str_input missing ##########################################
cross_plot(data=heart_disease, str_target="has_heart_disease")
