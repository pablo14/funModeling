#' @title Correlation plots
#' @description Visual correlation analysis. Plot different graphs in order to expose the inner information of any numeric variable against the target variable
#' @param data data frame source
#' @param str_input string input variable (if empty, it runs for all numeric variable), it can take a single character value or a character vector.
#' @param str_target string of the variable to predict, it supports binary or multinominal values.
#' @param plot_type Indicates the type of plot to retrieve, available values: "boxplot" or "histdens".
#' @param path_out path directory, if it has a value the plot is saved
#' @examples
#' \dontrun{
#' ## Run for all numeric variables
#' plotar(data=heart_disease, str_target="has_heart_disease",
#' 	plot_type="histdens")
#' plotar(heart_disease, str_input = 'age', str_target = 'chest_pain', plot_type = "boxplot")
#' }
#' @return Single or multiple plots specified by 'plot_type' parameter
#' @export
plotar <- function(data, str_input, str_target, plot_type, path_out)
{
	data=as.data.frame(data)

	## Parameters & Error handlers
	if(missing(plot_type))
		stop("Parameter 'plot_type' cannot be missing, available values: 'histdens' or 'boxplot'.")

	if(!(plot_type %in% c('histdens','boxplot')))
		stop("Value for 'plot_type' is not valid: available values: 'histdens' or 'boxplot'.")

  check_target_existence(data, str_target=str_target)

	data=remove_na_target(data, str_target=str_target)

	#check_target_2_values(data, str_target=str_target)

	## Convert to factor target variable
	data[,str_target]=as.factor(data[,str_target])

	if(missing(path_out)) path_out=NA

	if(missing(str_input))
	{
		data_2=data[, !(names(data) %in% str_target)]
		str_input=give_me_num_vars(data_2)
	}

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


histdens_target <- function(data, str_input, str_target)
{
	cdf=group_by_(data, str_target) %>% summarise_(var.mean=interp(~mean(v, na.rm=T), v=as.name(str_input)))

	cdf$var.mean=round(cdf$var.mean, 2)

  plot_histdens=ggplot(data, aes_string(x=str_input, colour=str_target)) + geom_density() + geom_vline(data=cdf, aes_string(xintercept="var.mean",  colour=str_target), linetype="dashed", size=0.5) +

	theme_bw() +

  theme(
		plot.background = element_blank(),
		panel.grid.minor = element_blank(),
		panel.border = element_blank(),
		axis.title.x=element_text(margin=margin(15,0,0,0)),
		axis.title.y=element_text(margin=margin(0,33,0,0))
  )

  return(plot_histdens)

}

boxplot_target <- function(data, str_input, str_target)
{
	plot_box=ggplot(data, aes_string(x=str_target, y=str_input, fill=str_target)) + geom_boxplot() +
         guides(fill=FALSE)+stat_summary(fun.y=mean, geom="point", shape=5, size=4) +

	theme_bw() +

  theme(
    	plot.background = element_blank(),
			panel.grid.minor = element_blank(),
			panel.border = element_blank(),
			axis.title.x=element_text(margin=margin(15,0,0,0)),
			axis.title.y=element_text(margin=margin(0,15,0,0))

  )

	return(plot_box)
}

#' @title Profiling analysis of categorical vs. target variable
#' @description Retrieves a complete summary of the grouped input variable against the target variable. Type of target variable must be binary for now. A positive case will be the less representative one. It returns the total positive cases (sum_target)); pecentage of total positive cases (perc_target) that fell in that category (this column sums 1); likelihood or mean of positive cases (mean_target) measured by the total positive cases over total cases in that category; quantity of rows of that category (q_rows) and in percentage (perc_rows) -this column sums 1. Full documentation can be found at: <http://http://livebook.datascienceheroes.com/data_preparation/high_cardinality_predictive_modeling.html/>.
#' @param data input data containing the variable to describe
#' @param input string input variable (if empty, it runs for all categorical variable), it can take a single character value or a character vector.
#' @param target string target variable. Binary or two class is only supported by now.
#' @examples
#' categ_analysis(data_country, "country", "has_flu")
#' @return if str_input has 1 variable, it retrurns a data frame indicating all the metrics, otherwise prints in console all variable results.
#' @export
categ_analysis<-function(data, input, target)
{
	data=as.data.frame(data)

	## Parameters & Error handlers #####################
	check_target_existence(data, str_target=target)

	data=remove_na_target(data, str_target=target)

	check_target_2_values(data, str_target=target)
	#####################################################

	## If missing it runs for all categorical variables
	if(missing(input))
	{
		data_2=data[, !(names(data) %in% target)]
		input=give_me_character_vars(data_2)
	}

	## Iterator
	q_vars=length(input)
	if(q_vars==1)
	{
		res=categ_analysis_logic(data = data, input=input, target=target)
		return(res)
	} else {
		for(i in 1:q_vars)
		{
			res=categ_analysis_logic(data = data, input=input[i], target=target)
			print(res)
			cat("", sep="\n")
		}

		cat("", sep="\n")
		return(sprintf("Variables processed: %s", paste(input, collapse = ", ")))
	}

}

categ_analysis_logic <- function(data, input, target)
{
	## Infering positive class as the less representative class.
	data[,target]=as.character(data[,target])
	grp_class=group_by(data, data[,target]) %>% summarise(q=n()) %>% arrange(q)
	pred_class=as.character(grp_class[1,1])
	tot_pos=sum(data[,target]==pred_class)

	## profiling
	grp=group_by_(data, input) %>% summarise_(
					mean_target=interp(~round(mean(var==pred_class, na.rm = TRUE), 3), var = as.name(target)),
					sum_target=interp(~sum(var==pred_class, na.rm = TRUE), var = as.name(target)),
					perc_target=interp(~round(sum(var==pred_class, na.rm = TRUE)/tot_pos,3), var = as.name(target)),
					q_rows=~n(),
					perc_rows=~round(n()/nrow(data), 3)
	) %>% arrange(-mean_target)

	#colnames(grp)[colnames(grp)=='sum_target']=paste("sum", target, sep="_")
	#colnames(grp)[colnames(grp)=='perc_target']=paste("perc", target, sep="_")
	#colnames(grp)[colnames(grp)=='mean_target']=paste("mean", target , sep="_")

	grp=data.frame(grp, stringsAsFactors = F)

	#print(sprintf("Variable: '%s'", input))

#	colnames(grp)[1]="category"

	return(grp)
}


