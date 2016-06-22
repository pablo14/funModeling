---
title: "funModeling Index"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Using funModeling in predictive modeling}
  %\VignetteEngine{knitr::knitr}
---

Data Preparation
====

## Part 1: Profiling Data

**Overview**: Quantity of zeros, NA, Inf, unique values; as well as the data type may lead to a good or bad model. Here an approach to cover the very first step in data modeling. 


```r
## Loading needed libraries
library(funModeling)
data(heart_disease)
```

### Checking NA, zeros, data type and unique values

```r
my_data_status=df_status(heart_disease)
```

```
##                  variable q_zeros p_zeros q_na p_na q_inf p_inf    type
## 1                     age       0    0.00    0 0.00     0     0 integer
## 2                  gender       0    0.00    0 0.00     0     0  factor
## 3              chest_pain       0    0.00    0 0.00     0     0  factor
## 4  resting_blood_pressure       0    0.00    0 0.00     0     0 integer
## 5       serum_cholestoral       0    0.00    0 0.00     0     0 integer
## 6     fasting_blood_sugar     258   85.15    0 0.00     0     0  factor
## 7         resting_electro     151   49.83    0 0.00     0     0  factor
## 8          max_heart_rate       0    0.00    0 0.00     0     0 integer
## 9             exer_angina     204   67.33    0 0.00     0     0 integer
## 10                oldpeak      99   32.67    0 0.00     0     0 numeric
## 11                  slope       0    0.00    0 0.00     0     0 integer
## 12      num_vessels_flour     176   58.09    4 1.32     0     0 integer
## 13                   thal       0    0.00    2 0.66     0     0  factor
## 14 heart_disease_severity     164   54.13    0 0.00     0     0 integer
## 15           exter_angina     204   67.33    0 0.00     0     0  factor
## 16      has_heart_disease       0    0.00    0 0.00     0     0  factor
##    unique
## 1      41
## 2       2
## 3       4
## 4      50
## 5     152
## 6       2
## 7       3
## 8      91
## 9       2
## 10     40
## 11      3
## 12      4
## 13      3
## 14      5
## 15      2
## 16      2
```
* `q_zeros`: quantity of zeros (`p_zeros`: in percentage)
* `q_na`:  quantity of NA (`p_na`: in percentage)
* `type`: factor or numeric
* `unique`: quantity of unique values

#### Why are these metrics important?
* **Zeros**: Variables with **lots of zeros** may be not useful for modeling, and in some cases it may dramatically bias the model.
* **NA**: Several models automatically exclude rows with NA (**random forest**, for example). As a result, the final model can be biased due to several missing rows because of only one variable. For example, if the data contains only one out of 100 variables with 90% of NAs, the model will be training with only 10% of original rows.
* **Inf**: Infinite values may lead to an unexpected behavior in some functions in R.
* **Type**: Some variables are encoded as numbers, but they are codes or categories, and the models **don't handle them** in the same way.
* **Unique**: Factor/categorical variables with a high number of different values (~30), tend to do overfitting if categories have low representative, (**decision tree**, for example).

#### Filtering unwanted cases
Function `df_status` takes a data frame and returns a the status table to quickly remove unwanted cases.


**Removing variables with high number of NA/zeros**

```r
# Removing variables with 60% of zero values
vars_to_remove=subset(my_data_status, my_data_status$p_zeros > 60)
vars_to_remove["variable"]
```

```
##               variable
## 6  fasting_blood_sugar
## 9          exer_angina
## 15        exter_angina
```

```r
## Keeping all except vars_to_remove 
heart_disease_2=heart_disease[, !(names(heart_disease) %in% vars_to_remove[,"variable"])]
```

**Ordering data by percentage of zeros**

```r
my_data_status[order(-my_data_status$p_zeros), c('variable', 'p_zeros')] 
```

```
##                  variable p_zeros
## 6     fasting_blood_sugar   85.15
## 9             exer_angina   67.33
## 15           exter_angina   67.33
## 12      num_vessels_flour   58.09
## 14 heart_disease_severity   54.13
## 7         resting_electro   49.83
## 10                oldpeak   32.67
## 1                     age    0.00
## 2                  gender    0.00
## 3              chest_pain    0.00
## 4  resting_blood_pressure    0.00
## 5       serum_cholestoral    0.00
## 8          max_heart_rate    0.00
## 11                  slope    0.00
## 13                   thal    0.00
## 16      has_heart_disease    0.00
```

## Part 2: Treatment of Outliers

**Overview**: `prep_outliers` function tries to automatize as much as it can be outliers preparation. It focus on the values that influence heavly the mean.
It sets an `NA` or stop at a certaing value all outliers for the desiered variables.


**Outlier threshold**: The method to detect them is based on percentile, flagging as outlier if the value is on the top X % (commonly 0.5%, 1%, 2%). Setting parameter `top_percent` in `0.01` will flag all values on the top 1%.

Same logic goes for the lowest values, setting parameter `bottom_percent` in 0.01 will flag as an outlier the lowest 1% of all values.

**Models highly affected by a biased mean**: linear regression, logistic regression, kmeans, decision trees. Random forest deals better with outliers. 
 
This function covers two typical escenarios (paramater `type`):

* Case A: Descriptive statistics / data profiling
* Case B: Data for predictive model


### Case A: `type='set_na'`

In this case all outliers are converted into `NA`, thus appling most of the descriptive functions (max, min, mean) will return a **less-biased mean** value - with the proper `na.rm=TRUE` parameter.


### Case B: `type='stop'`

Last case will cause that all rows with `NA` values will lost when a machine learning model is created. To avoid this, but keep controled the outliers, all values flagged as outlier will be converted to the threshold value.

**Key notes**: 

* Try to think variables treatment (and creation) as if you're explaining to the model. Stopping variables at a certaing value, 1% for example, you are telling to the model: _consider all extremes values as if they are on the 99% percentile, this value is already high enough_
* Models try to be noise tolereant, but you can help them by treat some common issues.


## Examples


```r
########################################
# Creating data frame with outliers
########################################
set.seed(10)
df=data.frame(var1=rchisq(1000,df = 1), var2=rnorm(1000))
df=rbind(df, 1135, 2432) # forcing outliers
df$id=as.character(seq(1:1002))

# for var1: mean is ~ 4.56, and max 2432
summary(df)
```

```
##       var1                var2                id           
##  Min.   :   0.0000   Min.   :  -3.2282   Length:1002       
##  1st Qu.:   0.0989   1st Qu.:  -0.6304   Class :character  
##  Median :   0.4455   Median :  -0.0352   Mode  :character  
##  Mean   :   4.5666   Mean   :   3.5512                     
##  3rd Qu.:   1.3853   3rd Qu.:   0.6242                     
##  Max.   :2432.0000   Max.   :2432.0000
```

### Case A: `type='set_na'`


```r
########################################################
### CASE A: Treatment outliers for data profiling
########################################################

#### EXAMPLE 1: Removing top 1% for a single variable

# checking the value for the top 1% of highest values (percentile 0.99), which is ~ 7.05
quantile(df$var1, 0.99)
```

```
##      99% 
## 7.052883
```

```r
# Setting type='set_na' sets NA to the highest value)
var1_treated=prep_outliers(data = df,  str_input = 'var1',  type='set_na', top_percent  = 0.01)

# now the mean (~ 0.94) is less biased, and note that: 1st, median and 3rd quartiles remaining very similar to the original variable.
summary(var1_treated)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
## 0.000003 0.095680 0.438800 0.940900 1.326000 6.795000       11
```

```r
#### EXAMPLE  2: if 'str_input' is missing, then it runs for all numeric variables (which have 3 or more distinct values).
df_treated2=prep_outliers(data = df, type='set_na', top_percent  = 0.01)
summary(df_treated2)
```

```
##       var1               var2               id           
##  Min.   :0.000003   Min.   :-3.22817   Length:1002       
##  1st Qu.:0.095676   1st Qu.:-0.64758   Class :character  
##  Median :0.438830   Median :-0.05779   Mode  :character  
##  Mean   :0.940909   Mean   :-0.05862                     
##  3rd Qu.:1.326450   3rd Qu.: 0.57706                     
##  Max.   :6.794558   Max.   : 1.99101                     
##  NA's   :11         NA's   :23
```

```r
#### EXAMPLE  3: Removing top 1% (and bottom 1%) for 'N' specific variables.
vars_to_process=c('var1', 'var2')
df_treated3=prep_outliers(data = df, str_input = vars_to_process, type='set_na', bottom_percent = 0.01, top_percent  = 0.01)
summary(df_treated3)
```

```
##       var1               var2               id           
##  Min.   :0.000003   Min.   :-1.98803   Length:1002       
##  1st Qu.:0.095676   1st Qu.:-0.60871   Class :character  
##  Median :0.438830   Median :-0.03522   Mode  :character  
##  Mean   :0.940909   Mean   :-0.00420                     
##  3rd Qu.:1.326450   3rd Qu.: 0.58415                     
##  Max.   :6.794558   Max.   : 1.99101                     
##  NA's   :11         NA's   :45
```

### Case B: `type='stop'`


```r
########################################################
### CASE B: Treatment outliers for predictive modeling
########################################################
#### EXAMPLE 4: Stopping outliers at the top 1% value for all variables. For example if the top 1% has a value of 7, then all values above will be set to 7. Useful when modeling because outlier cases can be used.
df_treated4=prep_outliers(data = df, type='stop', top_percent = 0.01)

# before
summary(df$var1)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##    0.0000    0.0989    0.4455    4.5670    1.3850 2432.0000
```

```r
# after, the max value is 7
summary(df_treated4$var1)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000003 0.098870 0.445500 1.007000 1.385000 7.000000
```

### Plots
Note when `type='set_na'` 

```r
ggplot(df_treated3, aes(x=var1)) + geom_histogram(binwidth=.5) + ggtitle("Setting type='set_na' (var1)")
```

```
## Warning: Removed 11 rows containing non-finite values (stat_bin).
```

![plot of chunk outliers_treatment4](figure/outliers_treatment4-1.png) 

```r
ggplot(df_treated4, aes(x=var1)) + geom_histogram(binwidth=.5) + ggtitle("Setting type='stop' (var1)")
```

![plot of chunk outliers_treatment4](figure/outliers_treatment4-2.png) 
