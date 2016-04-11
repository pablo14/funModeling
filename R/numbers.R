#' @title Correlation index against target variable
#' @description Obtain correlation table of all variables that belongs to data against target variable
#' @param data data frame
#' @param str_target string variable to predict
#' @examples
#' correlation_table(data=heart_disease, str_target="has_heart_disease")
#' @return Correlation index for all data input variable
#' @export
correlation_table <- function(data, str_target)
{
	data[, str_target]=as.numeric(data[, str_target])

	data=data[, c(give_me_num_vars(data, str_target), str_target)]

  df_cor=as.data.frame(round(cor(data, use="complete.obs"	),2))
  df_cor$Variable = rownames(df_cor)
  df_cor=df_cor[, names(df_cor) %in% c(str_target, "Variable")]

  row.names(df_cor) = NULL
  df_cor=df_cor[, c(2,1)]

  df_cor
  return(df_cor)
}


#' @title Remove or treat outliers
#' @description
#' Treat outliers by removing them or to stopping values at a certain percentile
#' @param data data frame
#' @param str_input string input variable (if empty, it runs for all numeric variable), it can take a single character value or a character vector.
#' @param prob value from 0 to 1, represents the prob to
#' @examples
#' prep_outliers()
#' @return
#' ddd
#' @export
prep_outliers <- function(data, str_input, prob)
{
  for(i in 1:length(str_input))
  {
   	top_value=round(quantile(data[,str_input[i]], probs=prob, names=F))
   	data[, str_input[i]][data[, str_input[i]]>top_value]=top_value
  }


 return(data)
}

#' @title Compare two vectors of keys
#' @description Obtain correlation table of all variables that belongs to data against target variable
#' @param data data frame
#' @param str_target string variable to predict
#' @examples
#' v1=c(1,2,4)
#' v2=c(1,2,5,6)
#' res=compare_df(key_x=v1, key_y=v2)
#' # Print the keys that didn't match
#' res
#' # Accessing the keys not present in
#' @return Correlation index for all data input variable
#' @export
compare_df <- function(key_x, key_y)
{
	# key_x=v1;key_y=v2
  df_x=data.frame(key_x=key_x, flag_x=1)
  df_y=data.frame(key_y=key_y, flag_y=1)

  df_x$key_x=as.character(df_x$key_x)
	df_y$key_y=as.character(df_y$key_y)

  merge_all=merge(df_x, df_y, by.x='key_x', by.y='key_y', all=T)

  names(merge_all)[1]="key"

  merge_all_nona=merge_all[!is.na(merge_all$flag_x) & !is.na(merge_all$flag_y),]

  not_in_x=merge_all[is.na(merge_all$flag_x),]
  not_in_y=merge_all[is.na(merge_all$flag_y),]

  print(sprintf("Coincident in both: %s", nrow(merge_all_nona)))
  print(sprintf("Rows not present in X: %s", nrow(not_in_x)))
  print(sprintf("Rows not present in Y: %s", nrow(not_in_y)))


  list_diff=list()

  list_diff[[1]]=list(
    present_in_both=merge_all_nona$key,
    rows_not_in_X=not_in_x$key,
    rows_not_in_Y=not_in_y$key
  	)

  return(list_diff)
}

