---
title: "funModeling Index"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Using funModeling in predictive modeling}
  %\VignetteEngine{knitr::knitr}
---

Miscellaneous
====

Here are some functions that don't fit in the data preparation, selecting best variables nor model accuracy testing


```r
## Loading fubModeling !
suppressMessages(library(funModeling))
data(heart_disease)
```

Part A) Comparing vectors

**What it does?**: Get the elements present (and not) between 2 vectors.

**Machine Learning purpose:** It's a common practice to run several times a variable selecting algorithm, getting in every run different variables. _So what are the new variables?_ and, _what are the ones that are not present anymore?_


```r
v1=c("height","weight","age")
v2=c("height","weight","location","q_visits")

res=compare_v(vector_x=v1, vector_y=v2)
```

```
## [1] "Coincident in both: 2"
## [1] "Rows not present in X: 2"
## [1] "Rows not present in Y: 1"
```

```r
# Printint the keys that didn't match
res
```

```
## $present_in_both
## [1] "height" "weight"
## 
## $rows_not_in_X
## [1] "location" "q_visits"
## 
## $rows_not_in_Y
## [1] "age"
```
