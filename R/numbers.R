#' @title Outliers Data Preparation
#' @description
#' Deal with outliers by setting an 'NA value' or by 'stopping' them at a certain. The parameters: 'top_percent'/'bottom_percent' are used to consider a value as outlier.
#' Setting NA is recommended when doing statistical analysis, parameter: type='set_na'.
#' Stopping is recommended when creating a predictive model without biasing the result due to outliers, parameter: type='stop'.
#' @param data data frame
#' @param str_input string input variable (if empty, it runs for all numeric variable).
#' @param top_percent value from 0 to 1, represents the highest X percentage of values to treat
#' @param bottom_percent value from 0 to 1, represents the lowest X percentage of values to treat
#' @param type can be 'stop' or 'set_na', in the first case the original variable is stopped at the desiered percentile, 'set_na'  sets NA to the same values.
#' @examples
#' # Creating data frame with outliers
#' set.seed(10)
#' df=data.frame(var1=rchisq(1000,df = 1), var2=rnorm(1000))
#' df=rbind(df, 1135, 2432) # forcing outliers
#' df$id=as.character(seq(1:1002))
#'
#' # for var1: mean is ~ 4.56, and max 2432
#' summary(df)
#'
#' ########################################################
#' ### PREPARING OUTLIERS FOR DESCRIPTIVE STATISTICS
#' ########################################################
#'
#' #### EXAMPLE 1: Removing top 1% for a single variable
#' # checking the value for the top 1% of highest values (percentile 0.99), which is ~ 7.05
#' quantile(df$var1, 0.99)
#'
#' # Setting type='set_na' sets NA to the highest value)
#' var1_treated=prep_outliers(data = df,  str_input = 'var1', type='set_na', top_percent  = 0.01)
#'
#' # now the mean (~ 0.94) is more accurate, and note that: 1st, median and 3rd quartiles remaining very similar to the original variable.
#' summary(var1_treated)
#'
#' #### EXAMPLE 2: if 'str_input' is missing, then it runs for all numeric variables (which have 3 or more distinct values).
#' df_treated2=prep_outliers(data = df, type='set_na', top_percent  = 0.01)
#' summary(df_treated2)
#'
#' #### EXAMPLE 3: Removing top 1% (and bottom 1%) for 'N' specific variables.
#' vars_to_process=c('var1', 'var2')
#' df_treated3=prep_outliers(data = df, str_input = vars_to_process, type='set_na', bottom_percent = 0.01, top_percent  = 0.01)
#' summary(df_treated3)
#'
#' ########################################################
#' ### PREPARING OUTLIERS FOR PREDICTIVE MODELING
#' ########################################################
#'
#' #### EXAMPLE 4: Stopping outliers at the top 1% value for all variables. For example if the top 1% has a value of 7, then all values above will be set to 7. Useful when modeling because outlier cases can be used.
#' df_treated4=prep_outliers(data = df, top_percent  = 0.01, type='stop')

#' @return A vector or data frame with the desired outlier transformation
#' @export
prep_outliers <- function(data, str_input, type=c('stop', 'set_na'), top_percent, bottom_percent)
{
	if(!(type %in% c('stop', 'set_na', 'sigmoid')))
		stop("Parameter 'type' must be one 'stop' or 'set_na'")


	if(missing(str_input))
		str_input=give_me_num_vars(data)


	# #########################################################
	# ### Sigmoid procesing
	# #########################################################
	# if(type == 'sigmoid')
	# {
	# 	for(i in 1:length(str_input))
	#   {
	#    	data[, str_input[i]]=sigmoid(as.numeric(scale(data[, str_input[i]])))
	# 	}
	# 	return(data)
	# }

	#########################################################
	### Stopping and Setting NA processing
	#########################################################
	## If not sigmoid, then it's stop or set_na, thus it has to have top or bottom param.
	if(missing(top_percent) & missing(bottom_percent))
		stop("Parameters 'top_percent' and 'bottom_percent' cannot be missing at the same time")

	## Logic for top value
	if(!missing(top_percent))
	{
	  for(i in 1:length(str_input))
	  {
	   	top_value=round(quantile(data[,str_input[i]], probs=(1-top_percent), names=F, na.rm=T))
	   	data[, str_input[i]][data[, str_input[i]]>top_value]=ifelse(type=='stop', top_value, NA)
	  }
	}

	## Logic for bottom value
	if(!missing(bottom_percent))
	{
	  for(i in 1:length(str_input))
	  {
	   	bottom_value=round(quantile(data[,str_input[i]], probs=bottom_percent, names=F, na.rm=T))
	   	data[, str_input[i]][data[, str_input[i]]<bottom_value]=ifelse(type=='stop', bottom_value, NA)
	  }
	}

	## Return the input vector if only 1 var was desired, otherwise it returns all the data frame transformed
	if(length(str_input)==1) {
		return(data[, str_input[i]])
	} else {
 		return(data)
	}

}

#' @title Compare two vectors
#' @description Obtaing coincident and not coincident elements between two vectors.
#' @param vector_x 1st vector to compare
#' @param vector_y 2nd vector to compare
#' @examples
#' v1=c("height","weight","age")
#' v2=c("height","weight","location","q_visits")
#' res=compare_v(key_x=v1, key_y=v2)
#' # Print the keys that didn't match
#' res
#' # Accessing the keys not present in
#' @return Correlation index for all data input variable
#' @export
compare_v <- function(vector_x, vector_y)
{
	# vector_x=v1;vector_y=v2
  df_x=data.frame(vector_x=vector_x, flag_x=1)
  df_y=data.frame(vector_y=vector_y, flag_y=1)

  df_x$vector_x=as.character(df_x$vector_x)
	df_y$vector_y=as.character(df_y$vector_y)

  merge_all=merge(df_x, df_y, by.x='vector_x', by.y='vector_y', all=T)

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

#' @title Correlation analyisis against target variable
#' @description Obtain correlation table of all variables that belongs to data against target variable. Only numeric variables are analyzed.
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

  df_cor=df_cor[interp(~order(df_cor, -v) , v=as.name(str_target)),  ]

  row.names(df_cor) = NULL
  df_cor=df_cor[, c(2,1)]

  df_cor[order(-df_cor[,2]) , ]

  return(df_cor)
}

#' @title Sigmoid function
#' @description Sigmoid function, also known as logistic or s-shaped
#' @param x numeric input vector
#' @param a constant to multiply 'x', default=1
#' @examples
#' sigmoid()
#' @return vector transformed with sigmoid
#' @export
sigmoid<-function(x, a=1)
{
	if(missing(a))
		a=1

	y = 1/(1 + exp(-a*x))

	return(y)
}

#' @title Transform a variable into the 0 to 1 range
#' @description Range a variable into [0-1], assigning 0 to the min and 1 to the max of the input variable.
#' @param x numeric input vector
#' @examples
#' range01(mtcars$cyl)
#' @return vector ranged into 0-1
#' @export
range01 <- function(x)
{
	return((x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T)))
}
