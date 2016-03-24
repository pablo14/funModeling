#' @title Get a summary for the given data frame.
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA and p_na). Last two columns indicates data type and quantity of unique values.
#' This function print and return the results.
#' @param data data frame
#' @param print_results if FALSE then there is not a print in the console, TRUE by default.
#' @examples
#' df_status(heart_disease)
#' @return Metrics data frame
#' @export
df_status <- function(data, print_results)
{
	if(missing(print_results))
		print_results=T

  df_status=data.frame(
    q_zeros=sapply(data, function(x) sum(x==0,na.rm=T)),
    p_zeros=round(100*sapply(data, function(x) sum(x==0,na.rm=T))/nrow(data),2),
    q_na=sapply(data, function(x) sum(is.na(x))),
    p_na=round(100*sapply(data, function(x) sum(is.na(x)))/nrow(data),2),
    type=sapply(data, class),
    unique=sapply(data, function(x) sum(!is.na(unique(x))))
  )

  ## Create new variable for column name
  df_status$variable=rownames(df_status)
  rownames(df_status)=NULL

  ## Reordering columns
  df_status=df_status[,c(7,1,2,3,4,5,6)]

  ## Print or return results
  ifelse(print_results, print(df_status), return(df_status))
}


#' @title Get model perfomance metrics (KS, AUC and ROC)
#' @description Get model performance for tree models (rpart library), or glm. It returns quality metrics: AUC (Area Under ROC Curve) and KS (Kolmogorov-Smirnov), and plots the ROC curve.
#' @param fit model, it could be any of the following: decision tree from rpart package, glm model or randomForest.
#' @param data data frame used to build the model. Also it supports data for testing, (it has to contain same columns as training data.
#' @param target_var It's the name of the column used as target/outcome. It's an string value, write it between apostrohpe.
#' @examples
#' fit_glm=glm(has_heart_disease ~ age + oldpeak, data=heart_disease, family = binomial)
#' model_performance(fit=fit_glm, data = heart_disease, target_var = "has_heart_disease")
#' @return None.
#' @export
model_performance <- function (fit, data, target_var)
{
  if(grepl("glm", as.character(fit$call))[1]) {
    scr = predict(fit, data)
     } else if(grepl("rpart", as.character(fit$call))[1]) {
    scr = predict(fit, data)[,2] } else if(grepl("randomForest", as.character(fit$call))[1]) {
    scr = predict(fit, data, type = "prob")[, 2]
     } else {
    print("Model not supported")
  }


  ## Get prediction ROCR object
  pred = prediction(scr, data[,target_var])

  ## Plot ROC
  perfROC = performance(pred, "tpr", "fpr")
  plot(perfROC, col=rainbow(10))
  abline(a=0,b=1)
  grid()

  ## AUC
  auc=performance(pred,"auc")@y.values[[1]]
  sprintf("AUC: %0.3f", round(auc,3))

  ## KS
  ks=max(attr(perfROC, "y.values")[[1]] - (attr(perfROC, "x.values")[[1]]))
  sprintf("KS: %0.3f", round(ks,3))

  ## Accuracy
  perfAcc = performance(pred, "acc")
  max(perfAcc@y.values[[1]])

  # Table creation
  dfAcc=data.frame(acc=perfAcc@y.values[[1]], cutoff=perfAcc@x.values[[1]])

  ## Metrics table
  dataMetrics=data.frame(
    AUC=round(auc,3),
    KS=round(ks,3)
  )

  pandoc.table(dataMetrics)
}


#' @title Sampling training and test data
#' @description Split input data into training and test set, retrieving always same sample by setting the seed.
#' @param data input data source
#' @param percentage_tr_rows percentage of training rows, range value from 0.1 to 0.99, default value=0.8 (80 percent of training data)
#' @param seed to generate the sample randomly, default value=987
#' @examples
#' ## Training and test data. Percentage of training cases default value=80%.
#' index_sample=get_sample(data=heart_disease, percentage_tr_rows=0.8)
#' ## Generating the samples
#' data_tr=heart_disease[index_sample,]
#' data_ts=heart_disease[-index_sample,]
#' @return TRUE/FALSE vector same length as 'data' param. TRUE represents that row position is for training data
#' @export
get_sample <- function(data, percentage_tr_rows=0.8, seed=987)
{
  set.seed(seed)
  tr=sample(nrow(data), round(percentage_tr_rows*nrow(data)))
  return(tr)
}


