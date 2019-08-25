#library(datapasta)
# tribble_paste
metadata_models=tibble::tribble(
~name, ~allow_NA, ~max_unique, ~allow_factor, ~allow_character, ~only_numeric,
"randomForest",     FALSE,          53,          TRUE,            FALSE,         FALSE,
"xgboost",      TRUE,         Inf,         FALSE,            FALSE,          TRUE,
"num_no_na",     FALSE,         Inf,         FALSE,            FALSE,          TRUE,
"no_na",     FALSE,         Inf,          TRUE,             TRUE,          TRUE,
"kmeans",     FALSE,         Inf,          TRUE,             TRUE,          TRUE,
"hclust",     FALSE,         Inf,          TRUE,             TRUE,          TRUE,
"hdbscan",     FALSE,         Inf,          TRUE,             TRUE,          TRUE,
"dbscan",     FALSE,         Inf,          TRUE,             TRUE,          TRUE,
"umap",     FALSE,         Inf,          TRUE,             TRUE,          TRUE,
"pca",     FALSE,         Inf,          TRUE,             TRUE,          TRUE,
"rpart",     TRUE,         Inf,          TRUE,             TRUE,          FALSE

  )



