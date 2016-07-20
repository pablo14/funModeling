## ----lib, results="hide"-------------------------------------------------
## Loading funModeling !
suppressMessages(library(funModeling))
data(heart_disease)

## ------------------------------------------------------------------------
v1=c("height","weight","age")
v2=c("height","weight","location","q_visits")

res=v_compare(vector_x=v1, vector_y=v2)

# Print the keys (or values) that didn't match
res

## ------------------------------------------------------------------------
# Training and test data. Percentage of training cases default value=80%.
index_sample=get_sample(data=heart_disease, percentage_tr_rows=0.8)

# It returns a TRUE/FALSE vector same length as 'data' param. TRUE represents that that particular will be hold for training data

## Generating the samples
data_tr=heart_disease[index_sample,]
data_ts=heart_disease[-index_sample,] # excluding all rows that belong to training

# percentage_tr_rows: range value from 0.1 to 0.99, default value=0.8 (80 percent of training data)


## ------------------------------------------------------------------------
# Selecting variables
my_data_1=filter_vars(mtcars, str_input=c('mpg', 'cyl'))
colnames(my_data_1)

# Deleting all except desiered variables
my_data_2=filter_vars(mtcars, str_input=c('mpg', 'cyl', 'qsec', 'vs'), keep=FALSE)
colnames(my_data_2)

