## ----lib, results="hide"-------------------------------------------------
## Loading funModeling !
suppressMessages(library(funModeling))
data(heart_disease)

## ----model_perfomance1---------------------------------------------------
## Training and test data. Percentage of training cases default value=80%.
index_sample=get_sample(data=heart_disease, percentage_tr_rows=0.8)

## Generating the samples
data_tr=heart_disease[index_sample,] 
data_ts=heart_disease[-index_sample,]


## Creating the model only with training data
fit_glm=glm(has_heart_disease ~ age + oldpeak, data=data_tr, family = binomial)


## ----model_perfomance2,  fig.height=3, fig.width=4-----------------------
## Performance metrics for Training Data
model_performance(fit=fit_glm, data = data_tr, target_var = "has_heart_disease")

## Performance metrics for Test Data
model_performance(fit=fit_glm, data = data_ts, target_var = "has_heart_disease")

