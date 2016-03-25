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

give_me_num_vars <-function(data, str_target)
{
	## If missing => Runs automatically for all numeric variables (valid only for numeric)
	status=df_status(data, print_results = F)
	## select all columns that not are factor nor character
	str_input=status[!(status$type %in% "factor" | status$type %in% "character"), 'variable']
	## Excluding target variable (in the case that it's detected as numeric)
	str_input=str_input[str_input!=str_target]

	return(str_input)
}
