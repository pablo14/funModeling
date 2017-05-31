library(funModeling)

### 1 var
freq(heart_disease$gender)

### 2 var
freq(heart_disease, c("gender","thal"))

### no vars -> all
freq(heart_disease)

