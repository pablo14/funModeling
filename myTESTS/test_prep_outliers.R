## 1 var, stop ##############################################
data_prep_a=prep_outliers(data = heart_disease, str_input = c('age','resting_blood_pressure'), top_percent  = 0.05,  type='stop')

max(data_prep_a$age);max(heart_disease$age) # have modfied
max(data_prep_a$resting_blood_pressure);max(heart_disease$resting_blood_pressure) # have modfied
max(data_prep_a$max_heart_rate);max(heart_disease$max_heart_rate) # remains the same

## passing a vector returning a vector
tail(heart_disease$age[order(heart_disease$age)])
b=prep_outliers(data=heart_disease$age, top_percent  = 0.01,  type='stop')
tail(b[order(b)], 10) # now the max is 71, and the result a vector



## 2 var, set na ##############################################
df_res=prep_outliers(data = heart_disease, str_input = c('age', 'max_heart_rate'), top_percent  = 0.01,  type='set_na')

describe(select(heart_disease, age, max_heart_rate)) ;describe(select(df_res, age, max_heart_rate)) ## returns a data frame

## testing with an skewed variable ############################
set.seed(10)
df=data.frame(var=rchisq(1000,df = 1))
df=rbind(df, c(-1000,1135, 2432)) # forcing outliers

options(scipen = 999)
df_2=prep_outliers(data = df, str_input = c('var'), bottom_percent = 0.01, top_percent  = 0.01,  type='set_na')
describe(df$var)
describe(df_2$var)

