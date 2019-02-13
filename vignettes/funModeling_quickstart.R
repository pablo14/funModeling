## ---- message=FALSE, warning=FALSE---------------------------------------
library(funModeling)

df_status(heart_disease)

## ---- fig.height=3, fig.width=5------------------------------------------
plot_num(heart_disease)

## ------------------------------------------------------------------------
profiling_num(heart_disease)

## ----distribution1, message=FALSE, fig.height=3, fig.width=5, warning=FALSE----
library(dplyr)

# Select only two variables for this example
heart_disease_2=heart_disease %>% select(chest_pain, thal)

# Frequency distribution
freq(heart_disease_2)

## ------------------------------------------------------------------------
correlation_table(heart_disease, "has_heart_disease")

## ------------------------------------------------------------------------
var_rank_info(heart_disease, "has_heart_disease")

## ----profiling1, fig.height=4, fig.width=8-------------------------------
cross_plot(data=heart_disease, input=c("age", "oldpeak"), target="has_heart_disease")

## ----boxplot_analysis, fig.height=2, fig.width=4-------------------------
plotar(data=heart_disease, input = c("age", "oldpeak"), target="has_heart_disease", plot_type="boxplot")

## ----density_histogram, fig.height=2, fig.width=4------------------------
plotar(data=mtcars, input = "gear", target="cyl", plot_type="histdens")

## ------------------------------------------------------------------------
df_ca=categ_analysis(data = data_country, input = "country", target = "has_flu")

head(df_ca)

## ------------------------------------------------------------------------
# Step 1: Getting the thresholds for the desired variables: "max_heart_rate" and "oldpeak"
d_bins=discretize_get_bins(data=heart_disease, input=c("max_heart_rate", "oldpeak"), n_bins=5)

# Step 2: Applying the threshold to get the final processed data frame
heart_disease_discretized=discretize_df(data=heart_disease, data_bins=d_bins, stringsAsFactors=T)

## ------------------------------------------------------------------------
iris_char=convert_df_to_categoric(data = iris, n_bins = 5)

# checking first rows
head(iris_char)

## ------------------------------------------------------------------------
new_age=equal_freq(heart_disease$age, n_bins = 5)

# checking results
Hmisc::describe(new_age)

## ------------------------------------------------------------------------
age_scaled=range01(heart_disease$oldpeak)

# checking results
summary(age_scaled)

## ------------------------------------------------------------------------
tukey_outlier(heart_disease$resting_blood_pressure)

## ------------------------------------------------------------------------
hampel_outlier(heart_disease$resting_blood_pressure)

## ------------------------------------------------------------------------
# Get threshold according to Hampel's method
hampel_outlier(heart_disease$max_heart_rate)

# Apply function to stop outliers at the threshold values
data_prep=prep_outliers(data = heart_disease, input = c('max_heart_rate','resting_blood_pressure'), method = "hampel", type='stop')


## ---- echo=FALSE---------------------------------------------------------
# Checking max and min value for 'max_heart_rate' before the transformation
sprintf("Before transformation -> Min: %s; Max: %s", min(heart_disease$max_heart_rate), max(heart_disease$max_heart_rate))

# Apply function to stop outliers at the threshold values
data_prep=prep_outliers(data = heart_disease, input = c('max_heart_rate','resting_blood_pressure'), method = "hampel", type='stop')

# Checking the results, the maximum value is now 174.5 (the minimum remains the same)
# Checking max and min value for 'max_heart_rate' before the transformation
sprintf("After transformation -> Min: %s; Max: %s", min(data_prep$max_heart_rate), max(data_prep$max_heart_rate))


## ----performance, fig.height=3, fig.width=7------------------------------
# Create machine learning model and get its scores for positive case 
fit_glm=glm(has_heart_disease ~ age + oldpeak, data=heart_disease, family = binomial)
heart_disease$score=predict(fit_glm, newdata=heart_disease, type='response')

# Calculate performance metrics
gain_lift(data=heart_disease, score='score', target='has_heart_disease')


