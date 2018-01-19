library(funModeling)

## normal ##########################################
cross_plot(data=heart_disease, input="chest_pain", target="has_heart_disease")

## normal + auto_binning= T ##########################################
# forcing hp to have less than 20 uniq values
mtcars=prep_outliers(mtcars, input = "hp",prob = 0.80)
# binning 1
cross_plot(data=mtcars, input="hp", target="vs", auto_binning = T)
mtcars$hp_2=equal_freq(mtcars$hp, 5)
cross_plot(data=mtcars, input="hp_2", target="vs", auto_binning = T)
# not binning
cross_plot(data=mtcars, input="hp", target="vs", auto_binning = F)

## uniq>20 - automatic auto binning
cross_plot(data=mtcars, input="drat", target="vs")
## uniq>20 - forcing not binning
cross_plot(data=mtcars, input="drat", target="vs", auto_binning = F)


## forcing NA in target ##########################################
heart_disease$has_heart_disease[1]=NA
cross_plot(data=heart_disease, input="chest_pain", target="has_heart_disease")

## forcing 3 values in target ##########################################
heart_disease$has_heart_disease=as.character(heart_disease$has_heart_disease)
heart_disease$has_heart_disease2=heart_disease$has_heart_disease
heart_disease$has_heart_disease2[1]="hello_world"
cross_plot(data=heart_disease, input="chest_pain", target="has_heart_disease2")

## target as numeric ##########################################
heart_disease$has_heart_disease_num=ifelse(heart_disease$has_heart_disease=="yes", 1, 0)
library(Hmisc)
describe(heart_disease$has_heart_disease_num)
cross_plot(data=heart_disease, input="chest_pain", target="has_heart_disease_num")


## input missing, run for every variable ##########################################
cross_plot(data=heart_disease, target="has_heart_disease")
