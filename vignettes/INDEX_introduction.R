## ---- results="hide"-----------------------------------------------------
## Loading funModeling !
suppressMessages(library(funModeling))

# loading data frame
data(heart_disease)

## ------------------------------------------------------------------------
df_status(heart_disease)

## ---- results="hide"-----------------------------------------------------
data_2=prep_outliers(data = heart_disease,  type='set_na', top_percent  = 0.01)

## ----  fig.height=3, fig.width=7-----------------------------------------
cross_plot(data=heart_disease, str_input="age", str_target="has_heart_disease")

## ----  fig.height=3, fig.width=5-----------------------------------------
plotar(data=heart_disease, str_input="age", str_target="has_heart_disease", plot_type="histdens")
plotar(data=heart_disease, str_input="age", str_target="has_heart_disease", plot_type="boxplot", path_out = 'd')

