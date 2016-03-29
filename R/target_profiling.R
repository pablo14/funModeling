#' @title Correlation plots
#' @description Visual correlation analysis. Plot different graphs in order to expose the inner information of any numeric variable against the target variable
#' @param data data frame source
#' @param str_input string input variable (if empty, it runs for all numeric variable), it can take a single character value or a character vector.
#' @param str_target string of the variable to predict
#' @param plot_type Indicates the type of plot to retrieve, available values: "boxplot" or "histdens".
#' @param path_out path directory, if it has a value the plot is saved
#' @examples
#' ## Run for all numeric variables
#' plotar(data=heart_disease, str_target="has_heart_disease",
#' 	plot_type="histdens")
#' @return Single or multiple plots specified by 'plot_type' parameter
#' @export
plotar <- function(data, str_input, str_target, plot_type, path_out)
{

	## Parameters & Error handlers
	if(missing(plot_type))
		stop("Parameter 'plot_type' cannot be missing, available values: 'histdens' or 'boxplot'.")

	if(!(plot_type %in% c('histdens','boxplot')))
		stop("Value for 'plot_type' is not valid: available values: 'histdens' or 'boxplot'.")

  check_target_existance(data, str_target=str_target)

	data=remove_na_target(data, str_target=str_target)

	check_target_2_values(data, str_target=str_target)

	## Convert to factor target variable
	data[,str_target]=as.factor(data[,str_target])

	if(missing(path_out)) path_out=NA

	if(missing(str_input))
		str_input=give_me_num_vars(data, str_target)

	## Begin iterator logic
  for(i in 1:length(str_input))
  {
    sprintf("Plotting '%s' (%s)", str_input[i], plot_type)

    ## Get the desiered plot
		target_plot=get_target_plot(data, str_input[i], str_target, plot_type)

		plot(target_plot)

		## Save plot into a jpeg file
	  if(!is.na(path_out))
	  {
	  	dir.create(path_out, showWarnings = F)

	    if(dir.exists(path_out))
	    {
		     jpeg(sprintf("%s/%s_%s.png", path_out, str_input[i], plot_type), width= 12.25, height= 6.25, units="in",res=200, quality = 90)
				 plot(target_plot)
	    dev.off()
	    } else {
	      warning(sprintf("The directory '%s' doesn't exists.", path_out))
	    }
	  }

  }

}

get_target_plot <- function(data, str_input, str_target, plot_type)
{
	## Retrieve the desiered plot
	if(plot_type=="histdens")
     plot_target=histdens_target(data, str_input, str_target)

  if(plot_type=="boxplot")
    plot_target=boxplot_target(data, str_input, str_target)

	return(plot_target)
}


#' @title Get a summary for the given data frame.
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA and p_na). Last two columns indicates data type and quantity of unique values.
#' This function print and return the results.
#' @param data data frame
#' @examples
#' df_status(heart_disease)
#' @return Metrics data frame
#' @export
histdens_target <- function(data, str_input, str_target)
{
  cdf=ddply(data, str_target, .fun = function(d)
    c(
      "var.mean" = round(mean(d[,str_input], na.rm=TRUE),2)
    ))

  plot_histdens=ggplot(data, aes_string(x=str_input, colour=str_target)) + geom_density() + geom_vline(data=cdf, aes_string(xintercept="var.mean",  colour=str_target), linetype="dashed", size=0.5)

  return(plot_histdens)

}

#' @title Plot
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA and p_na). Last two columns indicates data type and quantity of unique values.
#' This function print and return the results.
#' @param data data frame
#' @examples
#' df_status(heart_disease)
#' @return Metrics data frame
#' @export
boxplot_target <- function(data, str_input, str_target)
{
	plot_box=ggplot(data, aes_string(x=str_target, y=str_input, fill=str_target)) + geom_boxplot() +
         guides(fill=FALSE)+stat_summary(fun.y=mean, geom="point", shape=5, size=4)

	return(plot_box)
}

