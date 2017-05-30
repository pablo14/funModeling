#' @title Fibonacci series
#' @description
#' It retrieves a vector containing the first N numbers specified in 'length' parameter of the Fibonacci series.
#' @param length data frame
#' @param remove_first removes the first value of the series, because first 2 elements are the same (number=1). False by default.
#' @examples
#' # Get the first 4 elements of Fibonacci series
#' fibonacci(4)
#' @return vector
#' @export
fibonacci <- function(length, remove_first=F)
{
	fibvals = numeric(length)
	fibvals[1] = 1
	fibvals[2] = 1
	for (i in 3:length) {
		fibvals[i] = fibvals[i-1] + fibvals[i-2]
	}

	if(remove_first)
		fibvals=fibvals[-1]

	return(fibvals)
}

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


check_target_existence <- function(data, str_target)
{
	## Checking for variable existence.
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

give_me_character_vars <- function(data, str_target=NULL)
{
	##
	status=df_status(data, print_results = F)

	## Excluding not numeric variables
	str_input=status[status$type %in% "factor" | status$type %in% "character", 'variable']

	return(str_input)
}

#' @title Filtering variables by string name
#' @description Based on the variables name present in 'str_input', it returns the original data frame (keep=T), or it deletes all except the desired ones.
#' @param data data frame
#' @param str_input string vector containing variables to delete or to keep
#' @param keep boolean indicating if the variables names in 'str_input' must be kept or
#' @examples
#' # Selecting variables
#' my_data_1=filter_vars(mtcars, str_input=c('mpg', 'cyl'))
#' colnames(my_data_1)
#'
#' # Deleting all except desired variables
#' my_data_2=filter_vars(mtcars, str_input=c('mpg', 'cyl', 'qsec', 'vs'), keep=FALSE)
#' colnames(my_data_2)
#' @return Filtered data frame
#' @export
filter_vars <- function(data, str_input, keep=TRUE)
{
	#str_input=as.vector(str_input)
	if(keep)
	{
		# keeping variables
		return(data[, names(data) %in% str_input])
	} else {
		# delete variables all except...
		return(data[, !(names(data) %in% str_input)])
	}
}
