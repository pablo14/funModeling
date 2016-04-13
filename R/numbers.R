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

#' @title Compare two data frames by keys
#' @description Obtain differences between two data frames
#' @param dfcomp_x first data frame to compare
#' @param dfcomp_y second data frame to compare
#' @param keys_x keys of the first dataframe
#' @param keys_y keys of the second dataframe
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
compare_df <- function(dfcomp_x, dfcomp_y, keys_x, keys_y=NA)
{
  dfcomp_x$flag_x=1
  dfcomp_y$flag_y=1
  if (any(is.na(keys_y))){
    keys_y = keys_x
    all_keys = keys_x
  }else{
    all_keys = unique(c(keys_x, keys_y))
  }
  
  merge_all=merge(dfcomp_x, dfcomp_y, by.x=keys_x, by.y=keys_y, all=T)
  
  merge_all_nona=subset(merge_all, !is.na(merge_all$flag_x) & !is.na(merge_all$flag_y))
  
  not_in_x=merge_all[is.na(merge_all$flag_x), keys_y]
  not_in_y=merge_all[is.na(merge_all$flag_y), keys_x]
  
  if(nrow(merge_all_nona) > 0){
    merge_all_nona$flag_equal = TRUE
    for(varx in names(merge_all_nona)){
      if(grepl(".*\\.x", varx)){
        vary = gsub("\\.x", ".y", varx)
        merge_all_nona$flag_equal = ifelse(as.character(merge_all_nona[[varx]]) != as.character(merge_all_nona[[vary]]), FALSE, merge_all_nona$flag_equal)
      }
    }
    print(sprintf("Coincident keys: %s", nrow(merge_all_nona)))
    print(sprintf("Coincident entire rows: %s", nrow(merge_all_nona[merge_all_nona$flag_equal == TRUE,])))
    print(sprintf("Coincident keys with different values: %s", nrow(merge_all_nona[merge_all_nona$flag_equal == FALSE,])))
    
    list_diff=list(
      coincident=subset(merge_all_nona[,all_keys], merge_all_nona$flag_equal == TRUE),
      different_values=subset(merge_all_nona[,], merge_all_nona$flag_equal == FALSE),
      rows_not_in_X=not_in_x[,keys_y],
      rows_not_in_Y=not_in_y[,keys_x]
    )
    
  }else{
    print("No coincident rows")
    
    list_diff=list(
      rows_not_in_X=not_in_x[,keys_y],
      rows_not_in_Y=not_in_y[,keys_x]
    )
    
  }
  
  print(sprintf("Keys not present in X: %s", nrow(not_in_x)))
  print(sprintf("Keys not present in Y: %s", nrow(not_in_y)))
  
  return(list_diff)
}

