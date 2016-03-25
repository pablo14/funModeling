#' @title Plot
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA and p_na). Last two columns indicates data type and quantity of unique values.
#' This function print and return the results.
#' @param data data frame
#' @examples
#' df_status(heart_disease)
#' @return Metrics data frame
#' @export
get_cor_matrix <- function(data, str_target)
{
	str_target="has_heart_disease"
	data=heart_disease

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
