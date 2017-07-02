library(funModeling);library(dplyr);library(Hmisc)

############################################################################################
## method="bottom_top"
############################################################################################
## 1 var, stop ##############################################
data_prep_a=funModeling::prep_outliers(data = heart_disease, str_input = c('age','resting_blood_pressure'), top_percent  = 0.05, method = "bottom_top",  type='stop')


max(heart_disease$age);max(data_prep_a$age) # have modfied, from 77 to 68
max(heart_disease$resting_blood_pressure);max(data_prep_a$resting_blood_pressure) # have been modfied, from 200 to 160
max(heart_disease$max_heart_rate);max(data_prep_a$max_heart_rate) # remains the same,202, not in the str_input

## passing a vector returning a vector
tail(heart_disease$age[order(heart_disease$age)])
b=prep_outliers(data=heart_disease$age, top_percent  = 0.01,  type='stop', method = "bottom_top")
tail(b[order(b)], 10) # now the max is 71 (not 77), and the result is a vector



## 2 var, set na ##############################################
df_res=prep_outliers(data = heart_disease, str_input = c('age', 'max_heart_rate'), top_percent  = 0.01,  type='set_na', method = "bottom_top")

describe(select(heart_disease, age, max_heart_rate));describe(select(df_res, age, max_heart_rate)) ## returns a data frame

## testing with an skewed variable ############################
set.seed(10)
df=data.frame(var=rchisq(1000,df = 1))
df=rbind(df, c(-1000,1135, 2432)) # forcing outliers

options(scipen = 999)
df_2=prep_outliers(data = df, str_input = c('var'), bottom_percent = 0.01, top_percent  = 0.01,  type='set_na', method = "bottom_top")
profiling_num(df)
profiling_num(df_2) # the mean and variation coef changed a lot

## testing warning message  ############################
df_3=data.frame(var=c(1,1,1,1,1,1,1,1,1,1,1,1,1,4))

df_3_b=prep_outliers(data = df_3, str_input = c('var'), bottom_percent = 0.01, top_percent  = 0.01,  type='set_na', method = "bottom_top")
profiling_num(df_3)
profiling_num(df_3_b) # remains the same

### only bott
df_3_c=prep_outliers(data = df, str_input = c('var'), bottom_percent = 0.01,  type='set_na', method = "bottom_top")
tail(df$var);tail(df_3_c$var) # the value "-1000" is NA

# fail because it doesn have bot/top and method is bottom_top
df_3_c=prep_outliers(data = df, str_input = c('var'),type='set_na', method = "bottom_top")


############################################################################################
## method="tukey"
############################################################################################
data_prep_a=funModeling::prep_outliers(data = heart_disease, str_input = c('age','resting_blood_pressure'), method = "tukey",  type='stop')

max(heart_disease$age);max(data_prep_a$age) # remains the same (77) because the max thers for age is 100
tukey_outlier(heart_disease$age)
# forcing two outliers
data_prep_a$age[1]=1
data_prep_a$age[2]=110

v_age=funModeling::prep_outliers(data = data_prep_a$age, method = "tukey",  type='stop')
summary(v_age) # now the min is 7 and max is 101.5, also i tested the single vector treatment
tukey_outlier(data_prep_a$age) # it matches with the threshold, gr8!

############################################################################################
## method="hampel"
############################################################################################
data_prep_h=funModeling::prep_outliers(data = heart_disease, str_input = c('age','resting_blood_pressure'), method = "hampel",  type='stop')

summary(heart_disease$age);summary(data_prep_h$age) # it changed from 29 to 29.31, and the max remains the same at 77
hampel_outlier(heart_disease$age)

# bottom remains the same at 94, and the top is adjusted at 174.47 (before it was 200)
summary(heart_disease$resting_blood_pressure);summary(data_prep_h$resting_blood_pressure)
hampel_outlier(heart_disease$resting_blood_pressure)

## testing set_na
v_age=funModeling::prep_outliers(data = heart_disease$age, method = "hampel",  type='set_na')
head(heart_disease$age[order(heart_disease$age)])
head(v_age[order(v_age)])

summary(heart_disease$age);summary(v_age)
hampel_outlier(heart_disease$age)
## the 29 is now NA


