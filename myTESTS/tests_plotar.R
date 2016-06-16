## 1 var, sin output, hisdens ####################################
plotar(data=heart_disease, str_input="age", str_target="has_heart_disease", plot_type = "histdens")

## 1 var, sin output, boxplot ####################################
plotar(data=heart_disease, str_input="age", str_target="has_heart_disease", plot_type = "boxplot")

## 1 var, hisdens, no output ####################################
plotar(data=sub, str_input="num_vessels_flour", str_target="has_heart_disease", plot_type = "histdens")

## 1 var, boxplot, no output ####################################
# boxplot is not a good plot for this var due to the presence of lots of zeros
plotar(data=heart_disease, str_input="num_vessels_flour", str_target="has_heart_disease", plot_type = "boxplot")

## Filtering zeros ####################################
sub=subset(heart_disease, num_vessels_flour!=0)
plotar(data=sub, str_input="num_vessels_flour", str_target="has_heart_disease", plot_type = "boxplot")


####################################
# ALL vars, sin output, boxplot
plotar(data=heart_disease, str_target="has_heart_disease", plot_type = "boxplot")

# ALL vars, sin output, histdens
plotar(data=heart_disease, str_target="has_heart_disease", plot_type = "histdens")

## target as numeric
heart_disease$has_heart_disease_num=ifelse(heart_disease$has_heart_disease=="yes", 1, 0)
library(Hmisc)
describe(heart_disease$has_heart_disease_num)
plotar(data=heart_disease, str_target="has_heart_disease_num", str_input="age", plot_type = "histdens")


#############################################
