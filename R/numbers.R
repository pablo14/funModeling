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


#' @title Outliers Data Preparation
#' @description
#' Deal with outliers by removing or stopping them. Removes put an NA in the highest/lowers values at a certain percentile (top_percent/button_percent respectively).
#' Removing is recommended when describing the general population metrics, parameter: type='remove'.
#' Stopping is recommended when creating a predictive model without bias the result due to outliers, parameter: type='stop'.
#' @param data data frame
#' @param str_input string input variable (if empty, it runs for all numeric variable).
#' @param top_percent value from 0 to 1, represents the highest X% of values to treat
#' @param button_percent value from 0 to 1, represents the lowest X% of values to treat
#' @param type can be 'stop' or 'remove', in the first case the original variable is stopped at the desiered percentile, 'remove'  sets NA to the same values.
#' @examples
#' set.seed(10)
#'
# Creating variable with outliers
#' df=data.frame(var=rchisq(1000,df = 1))
#' df=rbind(df, 1135, 2432) # forcing outliers
#' summary(df$var) ## mean is 4.64, and max 2432

##
# Removing highest 1% of data. Type='remove' sets NA to highest value)
#' res=prep_outliers(data = df, str_input = 'var', top_percent  = 0.01,  type='remove')
#' summary(res) ## now the mean and the other metrics represent better the population

# Removing highest 1% of data. Type='stop' sets the value of top_percent (7 in this case) to the highest values
#' res=prep_outliers(data = df, str_input = 'var', top_percent  = 0.01,  type='stop')
#' summary(res) ## metrics represents better the population, this escenario is good when creating a predictive model, since not every alghorimg handles well the outliers
#' @return
#' A vector if str_input contains only one variable, or the input data frame with the transformed variables if str_input has 2 or more
#' @export
prep_outliers <- function(data, str_input, top_percent, button_percent, type=c('stop', 'remove'))
{

	if(!(type %in% c('stop', 'remove')))
		stop("Parameter 'type' must be one 'stop' or 'remove'")

	## Logic for top value
	if(!missing(top_percent))
	{
	  for(i in 1:length(str_input))
	  {
	   	top_value=round(quantile(data[,str_input[i]], probs=(1-top_percent), names=F, na.rm=T))
	   	data[, str_input[i]][data[, str_input[i]]>top_value]=ifelse(type=='stop', top_value, NA)
	  }
	}

	## Logic for button value
	if(!missing(button_percent))
	{
	  for(i in 1:length(str_input))
	  {
	   	button_value=round(quantile(data[,str_input[i]], probs=button_percent, names=F, na.rm=T))
	   	data[, str_input[i]][data[, str_input[i]]<button_value]=ifelse(type=='stop', button_value, NA)
	  }
	}

	## Return the input vector if only 1 var was desired, otherwise it returns all the data frame transformed
	if(length(str_input)==1) {
		return(data[, str_input[i]])
	} else {
 		return(data)
	}

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

  res=list(
    present_in_both=merge_all_nona$key,
    rows_not_in_X=not_in_x$key,
    rows_not_in_Y=not_in_y$key
  	)

  return(res)
}

