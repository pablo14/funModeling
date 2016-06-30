## ----lib, results="hide"-------------------------------------------------
## Loading fubModeling !
suppressMessages(library(funModeling))
data(heart_disease)

## ------------------------------------------------------------------------
v1=c("height","weight","age")
v2=c("height","weight","location","q_visits")

res=compare_v(vector_x=v1, vector_y=v2)

# Printint the keys that didn't match
res

