library(funModeling)

### 1 var
freq(heart_disease$gender)

### 2 var
freq(heart_disease, c("gender","thal"))

### high card, see layout
freq(data_country$country)

### warn message
a=as.factor(1:300)
b=freq(a)

### no vars -> all
freq(heart_disease)


