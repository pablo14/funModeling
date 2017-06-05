#' @title Plotting numerical data
#' @description
#' One plot containing all the histograms for numerical variables. NA values will not be displayed.
#' @param data data frame
#' @param bins number of bars (bins) to plot each histogram, 10 by default
#' @examples
#' plot_num(mtcars)
#' @return plot containing all numerical variables
#' @export
plot_num <- function(data, bins=10)
{
	## The concept of 'wide' and 'long' is crucial to understand how to pass the correct data to ggplot.
	# The official documentation is quite clear about it: http://seananderson.ca/2013/10/19/reshape.html
	wide_data=suppressMessages(melt(data))
	p=ggplot(data = wide_data, mapping = aes(x = value)) +
		geom_histogram(bins = bins, na.rm=T) + facet_wrap(~variable, scales = 'free_x') +  aes(fill = variable) + guides(fill=FALSE)

	p

}


#' @title Profiling numerical data
#' @description
#' Get a metric table with many indicators for all numerical variables, automatically skipping the non-numerical variables. Current metrics are:
#' mean, std_dev: standard deviation, all the p_XX: percentile at XX number, skewness, kurtosis, iqr: inter quartile range, variation_coef: the ratio of sd/mean, range_98 is the limit for which the 98% of fall, range_80 similar to range_98 but with 80%. All NA values will be skipped from calculations.
#' @param data data frame
#' @param print_results prints the result, TRUE by default.
#' @param digits the number of digits to show the result, 2 by default.
#' @examples
#' profiling_num(mtcars)
#' @return metrics table
#' @export
profiling_num <- function(data, print_results=T, digits=2)
{
	options(digits=digits)

	## Keeping all non factor nor char variables
	status=df_status(data, print_results = F)
	vars_num=filter(status,!(type %in% c("character", "factor"))) %>% .$variable
	data_num=select(data, one_of(vars_num))

	if(missing(print_results))
		print_results=T


	df_res=data.frame(
		mean=sapply(data_num, function(x) mean(x, na.rm=T)),
		std_dev=sapply(data_num, function(x) sd(x, na.rm=T)),
		p_01=sapply(data_num, function(x) quantile(x, probs = 0.01, na.rm=T)),
		p_05=sapply(data_num, function(x) quantile(x, probs = 0.05, na.rm=T)),
		p_10=sapply(data_num, function(x) quantile(x, probs = 0.10, na.rm=T)),
		p_25=sapply(data_num, function(x) quantile(x, probs = 0.25, na.rm=T)),
		p_50=sapply(data_num, function(x) quantile(x, probs = 0.50, na.rm=T)),
		p_75=sapply(data_num, function(x) quantile(x, probs = 0.75, na.rm=T)),
		p_90=sapply(data_num, function(x) quantile(x, probs = 0.90, na.rm=T)),
		p_95=sapply(data_num, function(x) quantile(x, probs = 0.95, na.rm=T)),
		p_99=sapply(data_num, function(x) quantile(x, probs = 0.99, na.rm=T)),

		skewness=sapply(data_num, function(x) skewness(x, na.rm=T)),
		kurtosis=sapply(data_num, function(x) kurtosis(x, na.rm=T)),
		iqr=sapply(data_num, function(x) IQR(x, na.rm=T))
	)

	df_res$variation_coef=df_res$std_dev/df_res$mean
	df_res$range_98=sprintf("[%s, %s]", round(df_res$p_01, digits), round(df_res$p_99, digits))
	df_res$range_80=sprintf("[%s, %s]", round(df_res$p_10, digits), round(df_res$p_90, digits))

	df_res=select(df_res, -p_10, -p_90)

	## Create new variable for column name
	df_res$variable=rownames(df_res)
	rownames(df_res)=NULL

	# reordering columns
	df_res=select(df_res, variable, mean, std_dev, variation_coef, everything(), iqr, skewness, kurtosis)

	## Print or return results
	if(print_results) print(df_res) else return(df_res)
}



#' @title Outliers Data Preparation
#' @description
#' Deal with outliers by setting an 'NA value' or by 'stopping' them at a certain. The parameters: 'top_percent'/'bottom_percent' are used to consider a value as outlier.
#'
#' Setting NA is recommended when doing statistical analysis, parameter: type='set_na'.
#' Stopping is recommended when creating a predictive model without biasing the result due to outliers, parameter: type='stop'.
#'
#' The function can take a data frame, and returns the same data plus the transformations specified in the str_input parameter. Or it can take a single vector (in the same 'data' parameter), and it returns a vector.
#'
#' @param data a data frame or a single vector. If it's a data frame, the function returns a data frame, otherwise it returns a vector.
#' @param str_input string input variable (if empty, it runs for all numeric variable).
#' @param top_percent value from 0 to 1, represents the highest X percentage of values to treat
#' @param bottom_percent value from 0 to 1, represents the lowest X percentage of values to treat
#' @param type can be 'stop' or 'set_na', in the first case the original variable is stopped at the desiered percentile, 'set_na'  sets NA to the same values.
#' @examples
#' \dontrun{
#' # Creating data frame with outliers
#' set.seed(10)
#' df=data.frame(var1=rchisq(1000,df = 1), var2=rnorm(1000))
#' df=rbind(df, 1135, 2432) # forcing outliers
#' df$id=as.character(seq(1:1002))
#'
#' # for var1: mean is ~ 4.56, and max 2432
#' summary(df)
#'
#' ########################################################
#' ### PREPARING OUTLIERS FOR DESCRIPTIVE STATISTICS
#' ########################################################
#'
#' #### EXAMPLE 1: Removing top 1%% for a single variable
#' # checking the value for the top 1% of highest values (percentile 0.99), which is ~ 7.05
#' quantile(df$var1, 0.99)
#'
#' # Setting type='set_na' sets NA to the highest value specified by top_percent.
#' # In this case 'data' parameter is single vector, thus it returns a single vector as well.
#' var1_treated=prep_outliers(data = df$var1, type='set_na', top_percent  = 0.01)
#'
#' # now the mean (~ 1) is more accurate, and note that: 1st, median and 3rd
#' #  quartiles remaining very similar to the original variable.
#' summary(var1_treated)
#'
#' #### EXAMPLE 2: Removing top and bottom 1% for the specified input variables.
#' vars_to_process=c('var1', 'var2')
#' df_treated3=prep_outliers(data = df, str_input = vars_to_process, type='set_na',
#'  bottom_percent = 0.01, top_percent  = 0.01)
#' summary(df_treated3)
#'
#' ########################################################
#' ### PREPARING OUTLIERS FOR PREDICTIVE MODELING
#' ########################################################
#'
#' #### EXAMPLE 3: Stopping outliers at the top 1%% value for all variables. For example
#' #   if the top 1%% has a value of 7, then all values above will be set to 7. Useful
#' #   when modeling because outlier cases can be used.
#' df_treated4=prep_outliers(data = df, top_percent  = 0.01, type='stop')
#' }
#' @return A data frame with the desired outlier transformation
#' @export
prep_outliers <- function(data, str_input=NA, type=c('stop', 'set_na'), top_percent, bottom_percent)
{
	if(!(type %in% c('stop', 'set_na')))
		stop("Parameter 'type' must be one 'stop' or 'set_na'")

	## If str_input is NA then ask for a single vector. True if it is a single vector
	if(sum(is.na(str_input)>0) & mode(data) %in% c("logical","numeric","complex","character"))
	{
		# creates a ficticious variable called 'var'
		data=data.frame(var=data)
		str_input="var"
		input_one_var=TRUE
	} else {
		input_one_var=FALSE
	}


	#########################################################
	### Stopping and Setting NA processing
	#########################################################
	if(missing(top_percent) & missing(bottom_percent))
		stop("Parameters 'top_percent' and 'bottom_percent' cannot be missing at the same time")

	## Logic for top value
	if(!missing(top_percent))
	{
	  for(i in 1:length(str_input))
	  {
	   	top_value=quantile(data[,str_input[i]], probs=(1-top_percent), names=F, na.rm=T)
	   	data[, str_input[i]][data[, str_input[i]]>=top_value]=ifelse(type=='stop', top_value, NA)

	   	if(length(na.omit(unique(data[, str_input[i]])))==1)
	   	{
	   		warning(sprintf("The transformation left the variable '%s' with 1 unique value. Please consider setting the threshold less restrictive.", str_input[i]))
	   	}
	  }
	}

	## Logic for bottom value
	if(!missing(bottom_percent))
	{
	  for(i in 1:length(str_input))
	  {
	   	bottom_value=quantile(data[,str_input[i]], probs=bottom_percent, names=F, na.rm=T)
	   	data[, str_input[i]][data[, str_input[i]]<=bottom_value]=ifelse(type=='stop', bottom_value, NA)
	  }

		if(length(na.omit(unique(data[, str_input[i]])))==1)
		{
			warning(sprintf("The transformation left the variable '%s' with 1 unique value. Please consider setting the threshold less restrictive.", str_input[i]))
		}
	}

	ifelse(input_one_var,  return(data$var), return(data))

}

#' @title Compare two vectors
#' @description Obtaing coincident and not coincident elements between two vectors.
#' @param vector_x 1st vector to compare
#' @param vector_y 2nd vector to compare
#' @examples
#' v1=c("height","weight","age")
#' v2=c("height","weight","location","q_visits")
#' res=v_compare(vector_x=v1, vector_y=v2)
#' # Print the keys that didn't match
#' res
#' # Accessing the keys not present in
#' @return Correlation index for all data input variable
#' @export
v_compare <- function(vector_x, vector_y)
{
	# vector_x=v1;vector_y=v2
  df_x=data.frame(vector_x=vector_x, flag_x=1)
  df_y=data.frame(vector_y=vector_y, flag_y=1)

  df_x$vector_x=as.character(df_x$vector_x)
	df_y$vector_y=as.character(df_y$vector_y)

  merge_all=merge(df_x, df_y, by.x='vector_x', by.y='vector_y', all=T)

  names(merge_all)[1]="key"

  merge_all_nona=merge_all[!is.na(merge_all$flag_x) & !is.na(merge_all$flag_y),]

  not_in_x=merge_all[is.na(merge_all$flag_x),]
  not_in_y=merge_all[is.na(merge_all$flag_y),]

  print(sprintf("Coincident in both: %s", nrow(merge_all_nona)))
  print(sprintf("Rows not present in X: %s", nrow(not_in_x)))
  print(sprintf("Rows not present in Y: %s", nrow(not_in_y)))


  list_diff=list()

  res=list(
    present_in_both=merge_all_nona$key,
    rows_not_in_X=not_in_x$key,
    rows_not_in_Y=not_in_y$key
  	)

  return(res)
}

#' @title Get correlation against target variable
#' @description Obtain correlation table for all variables against target variable. Only numeric variables are analyzed (factor/character are skippted automatically).
#' @param data data frame
#' @param str_target string variable to predict
#' @examples
#' correlation_table(data=heart_disease, str_target="has_heart_disease")
#' @return Correlation index for all data input variable
#' @export
correlation_table <- function(data, str_target)
{
	data=as.data.frame(data)

	data[, str_target]=as.numeric(data[, str_target])

	data=data[, c(give_me_num_vars(data, str_target), str_target)]

  df_cor=as.data.frame(round(cor(data, use="complete.obs"	),2))
  df_cor$Variable = rownames(df_cor)
  df_cor=df_cor[, names(df_cor) %in% c(str_target, "Variable")]

  df_cor=df_cor[, c(2,1)]

  df_cor_final=df_cor[order(-df_cor[,2]) , ]
	row.names(df_cor_final) = NULL

	return(df_cor_final)
}



#' @title Transform a variable into the [0-1] range
#' @description Range a variable into [0-1], assigning 0 to the min and 1 to the max of the input variable. All NA values will be removed.
#' @param var numeric input vector
#' @examples
#' range01(mtcars$cyl)
#' @return vector with the values scaled into the 0 to 1 range
#' @export
range01 <- function(var)
{
	return((var-min(var, na.rm=T))/(max(var, na.rm=T)-min(var, na.rm=T)))
}

#' @title Frequency table for categorical variables
#' @description Retrieves the frequency and percentage for str_input
#' @param data input data containing the variable to describe
#' @param str_input string input variable (if empty, it runs for all numeric variable), it can take a single character value or a character vector.
#' @param plot flag indicating if the plot is desired, TRUE by default
#' @param na.rm flag indicating if NA values must be included in the analysis, TRUE by default
#' @param path_out path directory, if it has a value the plot is saved
#' @examples
#' freq(data=heart_disease$thal)
#' freq(data=heart_disease, str_input = c('thal','chest_pain'))
#' @return vector with the values scaled into the 0 to 1 range
#' @export
freq <- function(data, str_input=NA, plot=T, na.rm=T, path_out)
{
	if(missing(path_out)) path_out=NA

  ## If str_input is NA then it runs for all variables in case it is not a single vector
	if(sum(is.na(str_input)>0))
	{
  	# True if it is a single vector
  	if(mode(data) %in% c("logical","numeric","complex","character"))
  	{
  		data=data.frame(var=data)
  		str_input="var"
  	} else {
			## Keeping all categorical variables
  		data=data.frame(data)
			status=df_status(data, print_results = F)
			str_input=status[status$type %in% c("factor", "character"), 'variable']
			if(length(str_input)==0)
				stop("None of the input variables are factor nor character")

  	}
	}

	## Iterator
	tot_vars=length(str_input)
	if(tot_vars==1)
	{
		res=freq_logic(data = data, str_input=str_input, plot, na.rm, path_out = path_out)
		return(res)
	} else {
		for(i in 1:tot_vars)
		{
			res=freq_logic(data = data, str_input=str_input[i], plot, na.rm, path_out = path_out)
			print(res)
			cat("", sep="\n")
		}

		return(sprintf("Variables processed: %s", paste(str_input, collapse = ", ")))

	}

}

freq_logic <- function(data, str_input, plot, na.rm, path_out)
{
	if(na.rm) {
		tbl=data.frame(table(factor(data[,str_input], exclude = NULL)))
	} else {
		tbl=data.frame(table(data[,str_input]))
	}

	tbl=rename(tbl, category=Var1, frequency=Freq) %>% arrange(-frequency)
	tbl$percentage=round(100*tbl$frequency/sum(tbl$frequency),2)
	tbl$cumulative_perc=cumsum(tbl$percentage)
	tbl$cumulative_perc[length(tbl$cumulative_perc)]=100.00

	## calculating best font size
	uq=nrow(tbl)
	if(uq<=10)
	{
		letter_size=3
		axis_size=12
	} else if(uq<=20){
		letter_size=2.5
		axis_size=10
	} else {
		letter_size=2
		axis_size=8
	}

	if(plot)
	{
		# Plot
		tbl_plot=tbl
		tbl_plot$label=sprintf('%s (%s%%)', tbl_plot$frequency, tbl_plot$percentage)

		tbl_plot$category=factor(tbl_plot$category, levels =  tbl_plot$category[order(tbl_plot$percentage)])


		if(nrow(tbl_plot)<200)
		{
			p=ggplot(tbl_plot,aes(x=tbl_plot$category,y=tbl_plot$frequency,fill=tbl_plot$category, label=label)) +
				geom_bar(stat='identity') + coord_flip() +	theme_bw() +
				theme(
					panel.grid.minor=element_blank(),
						panel.grid.major =element_blank(),
					legend.title=element_blank(),
					plot.title = element_text(vjust=2),
					axis.ticks.y=element_blank(),
					axis.ticks.x=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_text(size=axis_size),
					axis.title.x=element_text(size=12, margin=margin(10,0,0,0)),
					axis.title.y=element_text(size=14, margin=margin(0,10,0,0))
				) + ylab("Frequency / (Percentage %)") + xlab(str_input) +
				geom_text( color="#151515", size=letter_size, hjust=-.06) +
				guides(fill=F) +
				scale_y_continuous(expand = c(0,0),limits = c(0, max(tbl_plot$frequency)*1.2))

	## Save plot
			if(!is.na(path_out))
			{
				dir.create(path_out, showWarnings = F)

				if(dir.exists(path_out))
				{
					jpeg(sprintf("%s/%s.jpeg", path_out, str_input), width= 12.25, height= 6.25, units="in",res=200, quality = 90)

					plot(p)
					dev.off()
				} else {
					warning(sprintf("The directory '%s' doesn't exists.", path_out))
				}
			} else {
				plot(p)
			}

		} else {
			message_high_card=sprintf("Skipping plot for variable '%s' (more than 200 categories)", str_input)
		}

	}

	colnames(tbl)[1]=str_input
	tbl[,str_input]=as.character(tbl[,str_input])

	if(exists("message_high_card")) {warning(message_high_card)}

	return(tbl)
}
