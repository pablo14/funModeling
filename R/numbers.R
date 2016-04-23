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

#' @title Compare two data frames by keys
#' @description Obtain differences between two data frames
#' @param dfcomp_x first data frame to compare
#' @param dfcomp_y second data frame to compare
#' @param keys_x keys of the first dataframe
#' @param keys_y (optional) keys of the second dataframe, if missing both data frames will be compared with the keys_x
#' @param compare_values (optional) if TRUE it will not only compare keys, but also will check if the values of non-key matching columns have the same values
#' @examples
#' data(heart_disease)
#' a=heart_disease
#' b=heart_disease
#' a=subset(a, age >45)
#' b=subset(b, age <50)
#' b$gender='male'
#' b$chest_pain=ifelse(b$chest_pain ==3, 4, b$chest_pain)
#' res=compare_df(a, b, c('age', 'gender'))
#' # Print the keys that didn't match
#' res
#' # Accessing the keys not present in the first data frame
#' res[[1]]$rows_not_in_X
#' # Accessing the keys not present in the second data frame
#' res[[1]]$rows_not_in_Y
#' # Accessing the keys which coincide completely
#' res[[1]]$coincident
#' # Accessing the rows whose values did not coincide
#' res[[1]]$different_values
#' @return Differences and coincident values
#' @export
compare_df <- function(dfcomp_x, dfcomp_y, keys_x, keys_y=NA, compare_values=FALSE)
{
	#Setup internal flags for merging data
	internal_flags = c('comparedf_flag_x', 'comparedf_flag_y', 'comparedf_flag_equal')
  dfcomp_x$comparedf_flag_x=1
  dfcomp_y$comparedf_flag_y=1

  #If keys_y is missing, it is equal to keys_x
  if (missing(keys_y)){
    keys_y = keys_x
    all_keys = keys_x
  }else{
    all_keys = unique(c(keys_x, keys_y))
  }

  #Merge the input data frames
  merge_all=merge(dfcomp_x, dfcomp_y, by.x=keys_x, by.y=keys_y, all=T)

  #These are only the coincident keys
  merge_all_nona=subset(merge_all, !is.na(merge_all$comparedf_flag_x) & !is.na(merge_all$comparedf_flag_y))

  #Get non intersecting keys
  not_in_x=merge_all[is.na(merge_all$comparedf_flag_x), keys_y]
  not_in_y=merge_all[is.na(merge_all$comparedf_flag_y), keys_x]

  #If there are coincident keys
  if(nrow(merge_all_nona) > 0){

  	#Search for different values in non-key columns
    merge_all_nona$comparedf_flag_equal = TRUE
    if(compare_values){
	    for(varx in names(merge_all_nona)){
	      if(grepl(".*\\.x", varx)){
	        vary = gsub("\\.x", ".y", varx)
	        merge_all_nona$comparedf_flag_equal = ifelse(as.character(merge_all_nona[[varx]]) != as.character(merge_all_nona[[vary]]), FALSE, merge_all_nona$comparedf_flag_equal)
	      }
	    }
    }

    #Print results
    print(sprintf("Coincident keys: %s", nrow(merge_all_nona)))
    if(compare_values){
    	print(sprintf("Coincident entire rows: %s", nrow(merge_all_nona[merge_all_nona$comparedf_flag_equal == TRUE,])))
   		print(sprintf("Coincident keys with different values: %s", nrow(merge_all_nona[merge_all_nona$comparedf_flag_equal == FALSE,])))
    }

    #Save results into output list
    if(compare_values){
    	list_diff=list(
	      coincident=subset(merge_all_nona[,all_keys], merge_all_nona$comparedf_flag_equal == TRUE),
	      different_values=subset(merge_all_nona[,!names(merge_all_nona) %in% internal_flags], merge_all_nona$comparedf_flag_equal == FALSE),
	      rows_not_in_X=not_in_x,
	      rows_not_in_Y=not_in_y
	    )
    }else{
    	list_diff=list(
	      coincident=merge_all_nona[,all_keys],
	      rows_not_in_X=not_in_x,
	      rows_not_in_Y=not_in_y
	    )
    }


  #If no key coincides
  }else{
    print("No coincident keys")

    list_diff=list(
      rows_not_in_X=not_in_x,
      rows_not_in_Y=not_in_y
    )

  }

  #Print non-coincident keys
  print(sprintf("Keys not present in X: %s", nrow(not_in_x)))
  print(sprintf("Keys not present in Y: %s", nrow(not_in_y)))

  return(list_diff)
}

