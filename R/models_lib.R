#' @title Get a summary for the given data frame.
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA/p_na), and infinite values (q_inf/p_inf). Last two columns indicates data type and quantity of unique values.
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

  df_status_res=data.frame(
    q_zeros=sapply(data, function(x) sum(x==0,na.rm=T)),
    p_zeros=round(100*sapply(data, function(x) sum(x==0,na.rm=T))/nrow(data),2),
    q_na=sapply(data, function(x) sum(is.na(x))),
  	p_na=round(100*sapply(data, function(x) sum(is.na(x)))/nrow(data),2),
    q_inf=sapply(data, function(x) sum(is.infinite(x))),
  	p_inf=round(100*sapply(data, function(x) sum(is.infinite(x)))/nrow(data),2),
  	type=sapply(data, class),
    unique=sapply(data, function(x) sum(!is.na(unique(x))))
  )

  ## Create new variable for column name
  df_status_res$variable=rownames(df_status_res)
  rownames(df_status_res)=NULL

  ## Reordering columns
  df_status_res=df_status_res[, c(9,1,2,3,4,5,6,7,8)]

  ## Print or return results
  if(print_results) print(df_status_res) else return(df_status_res)
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


#' @title Generates lift (gain) performance table
#' @description It retrieves the cumulative positive rate when score is divided in (i.e.) 10 segments.
#' @param data input data source
#' @param str_score the variable which contains the score number
#' @param str_target target variable
#' @param q_segments quantity of segments to split str_score_var, valid values: 5, 10 or 20
#' @examples
#' fit_glm=glm(has_heart_disease ~ age + oldpeak, data=heart_disease, family = binomial)
#' heart_disease$score=predict(fit_glm, newdata=heart_disease, type='response')
#' lift_table(data=heart_disease,str_score='score',str_target='has_heart_disease')
#'
#' @return lift/gain table, column: gain implies how much positive cases are catched if the cut point to define the positive class is set to the column "Score Point"
#' @export
lift_table <- function(data, str_score, str_target, q_segments)
{
  # The negative score produces that the highest score are at the top
	# data=heart_disease; str_score='score'; str_target='has_heart_disease'; q_segments='10'
  data$neg_score=-data[, str_score]

  # Valid values for q_segments
  if(missing(q_segments))
    q_segments=10

  if(q_segments==20)
    seq_v=seq(from=0.05, to=0.95, by=0.05)

  if(q_segments==10 | !(q_segments %in% c(5,10,20)))
    seq_v=seq(from=0.1, to=0.9, by=0.1)

  if(q_segments==5)
    seq_v=seq(from=0.2, to=0.8, by=0.2)

  quantile_cuts=quantile(data$neg_score, probs=seq_v)

  data[,str_target]=as.character(data[,str_target])

  grp=group_by(data, data[,str_target]) %>% summarise(q=n()) %>% arrange(q)

  less_representative_class=as.character(grp[1,1])


  lift_table=round(100*sapply(quantile_cuts, function(x) sum(data[data$neg_score<=x, str_target]==less_representative_class))/sum(data[, str_target]==less_representative_class),2)


  lift_res=rbind(lift_table,-quantile_cuts)
  rownames(lift_res)=c("Gain", "Score Point")

  # likelihood of being less representative class (lrc)
  likelihood_lrc=grp[1,2]/(grp[2,2]+grp[1,2])

  lift_res_t=data.frame(t(lift_res))
  lift_res_t$Population=as.character(row.names(lift_res_t))
  row.names(lift_res_t)=NULL
  lift_res_t=select(lift_res_t, Population, Gain, Score.Point)

	lift_res_t$Lift=round(lift_res_t$Gain/100/seq_v,2)
  lift_res_t$Gain=paste(lift_res_t$Gain, "%", sep='')

  lift_res_t=select(lift_res_t, Population, Gain, Lift, Score.Point)

  print(lift_res_t)



}


