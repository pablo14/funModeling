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

give_me_num_vars <- function(data, str_target=NULL)
{
	##
	status=df_status(data, print_results = F)

	## Excluding not numeric variables
	str_input=status[!(status$type %in% "factor" | status$type %in% "character"), 'variable']

	## Excluding target variable (in the case that it's detected as numeric)
	#if(!missing(str_target))
	#	str_input=str_input[str_input!=str_target]


	## Excluding variables with less than two unique value
	ex_variables=status[status$unique<=2, 'variable']
	str_input=str_input[!(str_input %in% ex_variables)]

	if(length(ex_variables)>0)
		sprintf('Excluding variables with 1 or 2 unique values: %s', paste(ex_variables, collapse = ', '))


	return(str_input)
}
