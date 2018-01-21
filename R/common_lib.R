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

remove_na_target <- function(data, target)
{
	## Removing NA from target variable #########
	data_tmp=subset(data, !is.na(data[[target]]))
	if(nrow(data) > nrow(data_tmp))
	{
	  warning(sprintf("There were removed %d rows with NA values in target variable '%s'.", nrow(data)-nrow(data_tmp), target))

	  ## Keeping with cleaned data
	  data=data_tmp
	}

	return(data)
}


check_target_2_values <- function(data, target)
{
	## Stop if target is not binary
  if(length(unique(data[[target]]))>2)
  {
    stop(sprintf("Target variable '%s' does not have 2 unique values.", target))
  }
}


check_target_existence <- function(data, target)
{
	## Checking for variable existence.
	if(!(target %in% colnames(data))) stop(sprintf("Target variable '%s' does not exists in the data", target))
}

give_me_num_vars <- function(data, target=NULL)
{
	##
	status=df_status(data, print_results = F)

	## Excluding not numeric variables
	input=status[!(status$type %in% "factor" | status$type %in% "character"), 'variable']

	## Excluding variables with less than two unique value
	ex_variables=status[status$unique<=2, 'variable']
	input=input[!(input %in% ex_variables)]

	if(length(ex_variables)>0)
		sprintf('Excluding variables with 1 or 2 unique values: %s', paste(ex_variables, collapse = ', '))


	return(input)
}

give_me_character_vars <- function(data, target=NULL)
{
	##
	status=df_status(data, print_results = F)

	## Excluding not numeric variables
	input=status[status$type %in% "factor" | status$type %in% "character", 'variable']

	return(input)
}

#' @title Export plot to jpeg file
#' @description
#' Export 'object_plot' to jpeg file under the name 'file_name' in the directory 'path_out'
#' @param object_plot Object plot to export (like ggplot2)
#' @param path_out path directory to export the output, if it has a value the plot is saved,
#' if the directory doesn't existis it will try to create it. To save in current directory path must be dot: "."
#' @param file_name output file name
#' @return none
#' @export
export_plot <- function(object_plot, path_out, file_name)
{
	## Save plot into a jpeg file
	dir.create(path_out, showWarnings = F)

	if(dir.exists(path_out))
	{
		file_name_png=sprintf("%s/%s.png", path_out, file_name)
		suppressMessages(jpeg(file_name_png, width= 12.25, height= 6.25, units="in",res=200, quality = 90))
		plot(object_plot)
		suppressMessages(dev.off())
	} else {
		warning(sprintf("Directory '%s' doesn't exist or it couldn't be created", path_out))
	}
}
