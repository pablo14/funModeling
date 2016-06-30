## ----lib, results="hide"-------------------------------------------------
## Loading fubModeling !
suppressMessages(library(funModeling))
data(heart_disease)

## ----df_status-----------------------------------------------------------
my_data_status=df_status(heart_disease)

## ----df_status3----------------------------------------------------------
# Removing variables with 60% of zero values
vars_to_remove=subset(my_data_status, my_data_status$p_zeros > 60)
vars_to_remove["variable"]

## Keeping all except vars_to_remove 
heart_disease_2=heart_disease[, !(names(heart_disease) %in% vars_to_remove[,"variable"])]


## ----df_status4----------------------------------------------------------
my_data_status[order(-my_data_status$p_zeros), c('variable', 'p_zeros')] 

## ----outliers_treatment1,  fig.height=3, fig.width=4---------------------
########################################
# Creating data frame with outliers
########################################
set.seed(10)
df=data.frame(var1=rchisq(1000,df = 1), var2=rnorm(1000))
df=rbind(df, 1135, 2432) # forcing outliers
df$id=as.character(seq(1:1002))

# for var1: mean is ~ 4.56, and max 2432
summary(df)



## ----outliers_treatment2,  fig.height=3, fig.width=4---------------------
########################################################
### CASE 1: Treatment outliers for data profiling
########################################################

#### EXAMPLE 1: Removing top 1% for a single variable

# checking the value for the top 1% of highest values (percentile 0.99), which is ~ 7.05
quantile(df$var1, 0.99)

# Setting type='set_na' sets NA to the highest value)
var1_treated=prep_outliers(data = df,  str_input = 'var1',  type='set_na', top_percent  = 0.01)

# now the mean (~ 0.94) is less biased, and note that: 1st, median and 3rd quartiles remaining very similar to the original variable.
summary(var1_treated)

#### EXAMPLE  2: if 'str_input' is missing, then it runs for all numeric variables (which have 3 or more distinct values).
df_treated2=prep_outliers(data = df, type='set_na', top_percent  = 0.01)
summary(df_treated2)

#### EXAMPLE  3: Removing top 1% (and bottom 1%) for 'N' specific variables.
vars_to_process=c('var1', 'var2')
df_treated3=prep_outliers(data = df, str_input = vars_to_process, type='set_na', bottom_percent = 0.01, top_percent  = 0.01)
summary(df_treated3)


## ----outliers_treatment3,  fig.height=3, fig.width=4---------------------
########################################################
### CASE 2: Treatment outliers for predictive modeling
########################################################
#### EXAMPLE 4: Stopping outliers at the top 1% value for all variables. For example if the top 1% has a value of 7, then all values above will be set to 7. Useful when modeling because outlier cases can be used.
df_treated4=prep_outliers(data = df, type='stop', top_percent = 0.01)

# before
summary(df$var1)

# after, the max value is 7
summary(df_treated4$var1)

 	

## ----outliers_treatment4,  fig.height=3, fig.width=4---------------------
ggplot(df_treated3, aes(x=var1)) + geom_histogram(binwidth=.5) + ggtitle("Setting type='set_na' (var1)")
ggplot(df_treated4, aes(x=var1)) + geom_histogram(binwidth=.5) + ggtitle("Setting type='stop' (var1)")

