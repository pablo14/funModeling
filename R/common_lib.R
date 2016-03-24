remove_na_target <- function(data, str_target)
{
	## Removing NA from target variable #########
	data_tmp=subset(data, !is.na(data[, str_target]))
	if(nrow(data) > nrow(data_tmp))
	{
	  warning(sprintf("There were removed %d rows with NA values in target variable '%s'.", nrow(data)-nrow(data_tmp), str_target))

	  ## Keeping with cleaned data
	  data=data_tmp
	}

	return(data)
}


check_target_2_values <- function(data, str_target)
{
	## Stop if target is not binary
  if(length(unique(data[,str_target]))>2)
  {
    stop(sprintf("Target variable '%s' does not have 2 unique values.", str_target))
  }
}


check_target_existance <- function(data, str_target)
{
	## Checking for variable existance.
	if(!(str_target %in% colnames(data))) stop(sprintf("Target variable '%s' does not exists in the data", str_target))
}


