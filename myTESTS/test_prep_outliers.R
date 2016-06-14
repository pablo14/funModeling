## 1 var, stop ##############################################
a=prep_outliers(data = heart_disease, str_input = c('age'), top_percent  = 0.01,  type='stop')

describe(heart_disease$age==71)
describe(a==71) ## it has 3 more values with 71

## 2 var, remove ##############################################
df_res=prep_outliers(data = heart_disease, str_input = c('age', 'max_heart_rate'), top_percent  = 0.01,  type='stop')

describe(df_res) ## returns a data frame

## testing with an skewed variable ############################
set.seed(10)
df=data.frame(var=rchisq(1000,df = 1))
df=rbind(df, c(1135, 2432)) # forcing outliers
summary(df$var)

res=prep_outliers(data = df, str_input = c('var'), button_percent = 0.01, top_percent  = 0.01,  type='remove')
summary(res)

