## ----variable_importance1, results="hide", fig.height=4, fig.width=8-----
library(funModeling)
cross_gender=cross_plot(heart_disease, str_input="gender", str_target="has_heart_disease")

## ----variable_importance2, results="hide", fig.height=4, fig.width=12----
cross_plot(heart_disease, str_input="max_heart_rate", str_target="has_heart_disease")

## ----variable_importance3------------------------------------------------
heart_disease$oldpeak_2=equal_freq(var=heart_disease$oldpeak, n_bins = 3)
summary(heart_disease$oldpeak_2)

## ----variable_importance4, results="hide", fig.height=4, fig.width=8-----
cross_oldpeak_2=cross_plot(heart_disease, str_input="oldpeak_2", str_target="has_heart_disease", auto_binning = F)

## ----variable_importance5, results="hide", fig.height=4, fig.width=12----
heart_disease$max_heart_rate_2=equal_freq(var=heart_disease$max_heart_rate, n_bins = 10)
cross_plot(heart_disease, str_input="max_heart_rate_2", str_target="has_heart_disease")

## ----variable_importance6, results="hide", fig.height=4, fig.width=10----
heart_disease$max_heart_rate_3=equal_freq(var=heart_disease$max_heart_rate, n_bins = 5)
cross_plot(heart_disease, str_input="max_heart_rate_3", str_target="has_heart_disease")

## ----several_cross_plot1, eval=FALSE-------------------------------------
#  cross_plot(heart_disease, str_input="max_heart_rate_3", str_target="has_heart_disease", path_out="my_plots")

## ----several_cross_plot2, eval=FALSE-------------------------------------
#  vars_to_analyze=c("age", "oldpeak", "max_heart_rate")

## ----several_cross_plot3, eval=FALSE-------------------------------------
#  cross_plot(data=heart_disease, str_target="has_heart_disease", str_input=vars_to_analyze)

